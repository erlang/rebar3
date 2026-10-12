%% Vendored from hex_core v0.20.0, do not edit manually

%% @doc
%% Pure-Erlang raw DEFLATE (RFC 1951) encoder. The output only depends on the
%% input bytes and the options, not on the zlib library the VM links or the
%% number of schedulers.
%%
%% The input is split into chunks of `chunk' bytes (default 112 KiB, at most
%% 448 KiB) that are compressed independently, each with the preceding 32 KiB
%% as history, and joined in order. Chunks run in up to `workers' processes
%% in parallel (default: the number of online schedulers); each needs a heap
%% of about 4 MiB with the default chunk size.
%%
%% Matching is zlib's deflate_slow (or deflate_fast when `lazy' is false)
%% with level 6's parameters `chain', `nice', `good', `max_lazy' and
%% `too_far', and `hash_bits' bits of hash (default 16). Searches spend at
%% most `budget' chain steps per input byte on average (default 12), which
%% bounds the time spent on inputs with long, useless hash chains. Lazy
%% matching searches only positions at multiples of `stride' (a power of
%% two, default 4; 1 searches every position) from `skip' bytes (default
%% 256) after the end of the last match until it finds one, which saves time
%% on incompressible input. Blocks hold at most `block_syms' symbols.
-module(r3_hex_deflate).
-export([compress/1, compress/2]).
-ifdef(TEST).
-export([huff_lengths/3, parallel/4]).
-endif.
-compile({inline, [dist_code/2, find/14, walk/18]}).

-define(MAX_DIST, 32767).
-define(EOB, 256).
-define(DIST_BASE, 286).
%% Indexes into a chunk and its history, plus the window, stay below ?NONE.
-define(MAX_CHUNK, 7 bsl 16).

-record(c, {
    d,
    n,
    hash_bits,
    lazy,
    chain,
    nice,
    good,
    max_lazy,
    too_far,
    block_syms,
    budget,
    max_credit,
    lsym,
    dcode,
    fixed,
    skip,
    stride_mask,
    %% Per chunk: end of the chunk, whether it is the last one, the last
    %% position a match can start at plus one, and the hash chains and input
    %% (see links/4).
    last,
    final,
    top,
    links
}).

compress(Data) ->
    compress(Data, #{}).

compress(Data, Opts) when is_list(Data) ->
    compress(iolist_to_binary(Data), Opts);
compress(Data, Opts) when is_binary(Data), is_map(Opts) ->
    Size = byte_size(Data),
    Chunk = maps:get(chunk, Opts, 7 bsl 14),
    (is_integer(Chunk) andalso Chunk >= 1 andalso Chunk =< ?MAX_CHUNK) orelse
        error({badarg, {chunk, Chunk}}),
    Stride = maps:get(stride, Opts, 4),
    (is_integer(Stride) andalso Stride >= 1 andalso Stride band (Stride - 1) =:= 0) orelse
        error({badarg, {stride, Stride}}),
    Chain = maps:get(chain, Opts, 128),
    Cx = #c{
        d = Data,
        n = Size,
        hash_bits = maps:get(hash_bits, Opts, 16),
        lazy = maps:get(lazy, Opts, true),
        chain = Chain,
        nice = maps:get(nice, Opts, 128),
        good = maps:get(good, Opts, 8),
        max_lazy = maps:get(max_lazy, Opts, 16),
        too_far = maps:get(too_far, Opts, 4096),
        block_syms = maps:get(block_syms, Opts, 16384),
        budget = maps:get(budget, Opts, 12),
        max_credit = 64 * Chain,
        lsym = len_sym_table(),
        dcode = dist_code_table(),
        fixed = fixed_tables(),
        skip = maps:get(skip, Opts, 256),
        stride_mask = Stride - 1
    },
    NChunks = max(1, (Size + Chunk - 1) div Chunk),
    Workers = max(1, min(NChunks, maps:get(workers, Opts, erlang:system_info(schedulers_online)))),
    %% A heap with room for the links list and tuple (3 words per position,
    %% history included) and what compressing a chunk allocates, mostly
    %% Huffman tables (up to about half a word per byte measured on the
    %% corpus; the heap size is rounded up to the next one in the VM's
    %% sequence, 514,229 words with the default chunk size, which leaves 71,864
    %% words after the links), and a binary virtual heap with room for the
    %% input, which every chunk references, the hash head table and the
    %% chunk's own binaries, so that chunks finish without a garbage
    %% collection.
    Span = min(Size, Chunk + ?MAX_DIST),
    SpawnOpts = [
        {min_heap_size, 3 * Span + min(Size, Chunk) * 9 div 20 + 16384},
        {min_bin_vheap_size, Size div 8 + Span + (1 bsl Cx#c.hash_bits) + 16384}
    ],
    {Out, Acc, NB} = parallel(fun(I) -> chunk(I, Chunk, Cx) end, NChunks, Workers, SpawnOpts),
    <<Out/binary, Acc:(((NB + 7) div 8) * 8)/little>>.

%% Compresses chunk I into a list of segments: {bits, Bytes, Acc, NB} (whole
%% bytes followed by NB < 8 bits, placed at any bit offset) and
%% {stored, FinalBit, Data} (stored blocks, which start at a byte boundary).
chunk(I, Chunk, #c{d = D, n = N, lazy = Lazy} = Cx0) ->
    Start = I * Chunk,
    Last = min(N, Start + Chunk),
    %% Chunks shorter than 4 bytes have no matches, only literals.
    Links =
        if
            Last - Start >= 4 -> links(D, max(0, Start - ?MAX_DIST), Last, Cx0#c.hash_bits);
            Last > Start -> links(D, Start, Last, Cx0#c.hash_bits);
            true -> {}
        end,
    Cx = Cx0#c{
        last = Last,
        final = Last =:= N,
        top = Last - 3,
        links = Links
    },
    W0 = {<<>>, 0, 0, []},
    Credit = Cx#c.max_credit - Cx#c.budget * Start,
    {Out, Acc, NB, Segs} =
        case Lazy of
            true -> lazy(Start, 0, false, Start + Cx#c.skip, <<>>, 0, Start, W0, Credit, Cx);
            false -> greedy(Start, <<>>, 0, Start, W0, Credit, Cx)
        end,
    lists:reverse([bits(Out, Acc, NB) | Segs]).

bits(Out, Acc, NB) ->
    K = NB div 8,
    {bits, <<Out/binary, Acc:(K * 8)/little>>, Acc bsr (K * 8), NB - K * 8}.

%% Appends a segment to the output {Bytes, Acc, NB} with NB < 8.
join({bits, BOut, BAcc, BNB}, {AOut, AAcc, 0}) ->
    {<<AOut/binary, BOut/binary>>, AAcc bor BAcc, BNB};
join({bits, BOut, BAcc, BNB}, {AOut, AAcc, ANB}) ->
    %% The segment is shifted into a new binary, which shift/4 appends to in
    %% place.
    {Shifted, Carry} = shift(BOut, ANB, AAcc, <<>>),
    Acc = Carry bor (BAcc bsl ANB),
    NB = ANB + BNB,
    if
        NB >= 8 -> {<<AOut/binary, Shifted/binary, Acc:8>>, Acc bsr 8, NB - 8};
        true -> {<<AOut/binary, Shifted/binary>>, Acc, NB}
    end;
join({stored, FinalBit, <<Chunk:65535/binary, Rest/binary>>}, W) when byte_size(Rest) > 0 ->
    join({stored, FinalBit, Rest}, join({stored, 0, Chunk}, W));
join({stored, FinalBit, Data}, {Out, Acc, NB}) ->
    %% The 3 header bits (BTYPE 00), then padding to a byte boundary.
    Pad = ((NB + 10) div 8) * 8,
    Len = byte_size(Data),
    {
        <<Out/binary, (Acc bor (FinalBit bsl NB)):Pad/little, Len:16/little,
            (Len bxor 16#FFFF):16/little, Data/binary>>,
        0,
        0
    }.

%% Appends Bin shifted left by K < 8 bits, below which Carry is placed.
shift(<<X:48/little, Rest/binary>>, K, Carry, Out) ->
    V = (X bsl K) bor Carry,
    shift(Rest, K, V bsr 48, <<Out/binary, V:48/little>>);
shift(<<X, Rest/binary>>, K, Carry, Out) ->
    V = (X bsl K) bor Carry,
    shift(Rest, K, V bsr 8, <<Out/binary, V:8>>);
shift(<<>>, _K, Carry, Out) ->
    {Out, Carry}.

%% Compresses the chunks 0..N-1 with F in up to Workers processes spawned
%% with SpawnOpts, and joins their segments in order as they become
%% available. Each worker takes the next chunk when it is done with the
%% previous one. An exception in F is raised in the caller.
parallel(F, N, Workers, SpawnOpts) ->
    Parent = self(),
    Tag = make_ref(),
    Pool = [
        spawn_opt(fun() -> worker(Parent, Tag, F) end, [monitor | SpawnOpts])
     || _ <- lists:seq(1, Workers)
    ],
    _ = [Pid ! {Tag, I} || {I, {Pid, _}} <- lists:zip(lists:seq(0, Workers - 1), Pool)],
    collect(Tag, Workers, N, maps:from_list(Pool), 0, #{}, {<<>>, 0, 0}).

worker(Parent, Tag, F) ->
    MRef = monitor(process, Parent),
    worker(Parent, Tag, F, MRef).

worker(Parent, Tag, F, MRef) ->
    receive
        {Tag, I} when is_integer(I) ->
            Result =
                try
                    {ok, F(I)}
                catch
                    Class:Reason:Stack -> {Class, Reason, Stack}
                end,
            Parent ! {Tag, self(), I, Result},
            erlang:garbage_collect(),
            worker(Parent, Tag, F, MRef);
        {Tag, done} ->
            ok;
        {'DOWN', MRef, process, _, _} ->
            ok
    end.

%% Joined holds the output of the chunks before Join; Done the segments of
%% finished chunks from Join on.
collect(Tag, Next, N, Pool, Join, Done, Joined) when Join < N ->
    case Done of
        #{Join := Segs} ->
            Joined1 = lists:foldl(fun join/2, Joined, Segs),
            collect(Tag, Next, N, Pool, Join + 1, maps:remove(Join, Done), Joined1);
        #{} ->
            receive
                {Tag, Pid, I, {ok, Segs}} ->
                    if
                        Next < N -> Pid ! {Tag, Next};
                        true -> Pid ! {Tag, done}
                    end,
                    collect(Tag, Next + 1, N, Pool, Join, Done#{I => Segs}, Joined);
                {Tag, _Pid, _I, {Class, Reason, Stack}} ->
                    stop(Tag, Pool),
                    erlang:raise(Class, Reason, Stack);
                {'DOWN', MRef, process, Pid, Reason} when
                    map_get(Pid, Pool) =:= MRef, Reason =/= normal
                ->
                    stop(Tag, maps:remove(Pid, Pool)),
                    exit(Reason)
            end
    end;
collect(_Tag, _Next, _N, Pool, _Join, _Done, Joined) ->
    _ = [erlang:demonitor(MRef, [flush]) || MRef <- maps:values(Pool)],
    Joined.

%% Kills the workers, waits until they are gone and discards their messages.
stop(Tag, Pool) ->
    _ = [exit(Pid, kill) || Pid <- maps:keys(Pool)],
    _ = [
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        end
     || {Pid, MRef} <- maps:to_list(Pool)
    ],
    discard(Tag).

discard(Tag) ->
    receive
        {Tag, _, _, _} -> discard(Tag)
    after 0 -> ok
    end.

%%====================================================================
%% LZ77
%%
%% Chains are hashed on 3 bytes. Each chunk first inserts every position
%% from 32 KiB before the chunk up to three bytes before its end into a
%% hash head table, recording per position the position the head held
%% before: the previous one with the same hash. Positions are identified by
%% their index I = Last - Pos into the chunk's tuple, so older positions
%% have larger indexes. Each element packs the previous index and the 5
%% bytes at the position (zero past the end of the input) as
%% (Prev bsl 40) bor Quint, so a chain step reads one element, and the match
%% finder compares 5 bytes at a time instead of reading the input.
%%====================================================================

-define(HASH(Q, Shift), (((((Q) bsr 16) * 16#9E3779B1) band 16#FFFFFFFF) bsr (Shift))).
-define(M5, 16#FFFFFFFFFF).
%% Previous index of positions without one: outside every window. Elements
%% are small integers (below 2^59) with indexes below 2^19.
-define(NONE, ((1 bsl 19) - 1)).

%% Inserts the position at index I, whose 5 bytes Q are the 4 last bytes of
%% Q0 followed by B, into the head table Head (hashed with shift S, both
%% variables of links/6), and prepends its element to Acc0.
-define(LINK(Q0, B, I, Q, Acc0, Acc),
    Q = (((Q0) band 16#FFFFFFFF) bsl 8) bor (B),
    Acc = [
        ((atomics:exchange(Head, ?HASH(Q, S) + 1, (I) bxor ?NONE) bxor ?NONE) bsl 40) bor Q
        | Acc0
    ]
).

%% Tuple where element(Last - Pos, Links) is (Prev bsl 40) bor Quint for
%% From =< Pos < Last: Prev is the index of the previous position with Pos's
%% hash (?NONE for the last three positions) and Quint the 5 bytes at Pos.
%% The head table holds Index bxor ?NONE, so that its initial 0 reads as
%% ?NONE.
links(D, From, Last, HashBits) ->
    Head = atomics:new(1 bsl HashBits, []),
    Avail = min(byte_size(D), Last + 4) - From,
    <<Q:32, R/binary>> = <<(binary_part(D, From, Avail))/binary, 0:32>>,
    %% Bit 40 keeps the first position off the run path below: the position
    %% before it isn't inserted.
    list_to_tuple(links(R, Q bor (1 bsl 40), Last - From, [], Head, 32 - HashBits)).

links(<<B1, B2, B3, B4, B5, B6, B7, B8, R/binary>>, Q0, I, Acc0, Head, S) when I > 10 ->
    B = Q0 band 255,
    if
        Q0 =:= B * 16#0101010101,
        B1 =:= B,
        B2 =:= B,
        B3 =:= B,
        B4 =:= B,
        B5 =:= B,
        B6 =:= B,
        B7 =:= B,
        B8 =:= B ->
            %% The 8 positions and the one before are in a run of one byte:
            %% each one's previous position with its hash is the one before
            %% it.
            ok = atomics:put(Head, ?HASH(Q0, S) + 1, (I - 7) bxor ?NONE),
            E = ((I + 1) bsl 40) bor Q0,
            Acc = [
                E - (7 bsl 40),
                E - (6 bsl 40),
                E - (5 bsl 40),
                E - (4 bsl 40),
                E - (3 bsl 40),
                E - (2 bsl 40),
                E - (1 bsl 40),
                E
                | Acc0
            ],
            links(R, Q0, I - 8, Acc, Head, S);
        true ->
            ?LINK(Q0, B1, I, Q1, Acc0, Acc1),
            ?LINK(Q1, B2, I - 1, Q2, Acc1, Acc2),
            if
                Q1 bsr 16 =:= Q1 band 16#FFFFFF,
                Q2 bsr 16 =:= Q2 band 16#FFFFFF,
                B3 =:= B1,
                B4 =:= B2,
                B5 =:= B1,
                B6 =:= B2,
                B7 =:= B1,
                B8 =:= B2,
                ?HASH(Q1, S) =/= ?HASH(Q2, S) ->
                    %% The 8 positions repeat 2 bytes and their two prefixes
                    %% hash differently: from the third one on, each one's
                    %% previous position with its hash is 2 before it.
                    ok = atomics:put(Head, ?HASH(Q1, S) + 1, (I - 6) bxor ?NONE),
                    ok = atomics:put(Head, ?HASH(Q2, S) + 1, (I - 7) bxor ?NONE),
                    E1 = (I bsl 40) bor Q1,
                    E2 = ((I - 1) bsl 40) bor Q2,
                    Acc = [
                        E2 - (4 bsl 40),
                        E1 - (4 bsl 40),
                        E2 - (2 bsl 40),
                        E1 - (2 bsl 40),
                        E2,
                        E1
                        | Acc2
                    ],
                    links(R, Q2, I - 8, Acc, Head, S);
                true ->
                    ?LINK(Q2, B3, I - 2, Q3, Acc2, Acc3),
                    ?LINK(Q3, B4, I - 3, Q4, Acc3, Acc4),
                    ?LINK(Q4, B5, I - 4, Q5, Acc4, Acc5),
                    ?LINK(Q5, B6, I - 5, Q6, Acc5, Acc6),
                    ?LINK(Q6, B7, I - 6, Q7, Acc6, Acc7),
                    ?LINK(Q7, B8, I - 7, Q8, Acc7, Acc8),
                    links(R, Q8, I - 8, Acc8, Head, S)
            end
    end;
links(<<B, R/binary>>, Q0, I, Acc0, Head, S) when I > 3 ->
    ?LINK(Q0, B, I, Q, Acc0, Acc),
    links(R, Q, I - 1, Acc, Head, S);
links(<<B, R/binary>>, Q0, I, Acc, Head, S) when I > 0 ->
    Q = ((Q0 band 16#FFFFFFFF) bsl 8) bor B,
    links(R, Q, I - 1, [(?NONE bsl 40) bor Q | Acc], Head, S);
links(_, _, _, Acc, _, _) ->
    Acc.

%% The match loop state that a search passes through to found/12: the match
%% pending from the previous position and whether its byte is pending, the
%% position from which searches are sparse (skip bytes after the end of the
%% last match; lazy matching only), the matches of the current block (see flush/6), the number of symbols in the
%% block, the block's first position, the bit writer, the chain step credit
%% (see lazy/10) and the chunk.
-define(STATE, PM, Pending, Sparse, Ms, NS, BStart, W, Credit, Cx).

%% Searches for the longest match at index I longer than BL, following the
%% chain from index C for at most Chain candidates, and continues matching
%% with the result, (Dist bsl 9) bor Len or 0. Q holds the 5 bytes at I.
find(C, Q, I, BL, Chain, ?STATE) ->
    if
        BL >= I; BL >= 258 ->
            found(0, I, Chain, ?STATE);
        true ->
            L = Cx#c.links,
            SQ =
                if
                    BL >= 5 -> element(I - BL + 4, L) band ?M5;
                    true -> Q
                end,
            walk(C, Q, L, I, I + ?MAX_DIST + 1, Chain, BL, 0, SQ, ?STATE)
    end.

%% Walks the chain with short/18 (BL = 2 or 3) or longest/18 by BL.
walk(C, Q, L, I, MaxC, Chain, 2, Best, _SQ, ?STATE) ->
    short(C, Q, L, I, MaxC, Chain, 2, Best, 16#FFFFFF0000, ?STATE);
walk(C, Q, L, I, MaxC, Chain, 3, Best, _SQ, ?STATE) ->
    short(C, Q, L, I, MaxC, Chain, 3, Best, 16#FFFFFFFF00, ?STATE);
walk(C, Q, L, I, MaxC, Chain, BL, Best, SQ, ?STATE) ->
    longest(C, Q, L, I, MaxC, Chain, BL, Best, SQ, ?STATE).

%% longest/18 for BL = 2 or 3: candidates whose first BL + 1 bytes match Q,
%% the bytes that Mask (in SQ's place) selects.
short(C, Q, L, I, MaxC, Chain, BL, Best, Mask, ?STATE) when C < MaxC, Chain > 0 ->
    X = element(C, L),
    if
        (X bxor Q) band Mask =/= 0 ->
            short(X bsr 40, Q, L, I, MaxC, Chain - 1, BL, Best, Mask, ?STATE);
        X band ?M5 =:= Q ->
            ext(C, Q, L, I, MaxC, Chain, BL, Best, Q, 5, ?STATE);
        (X bxor Q) band 16#FF00 =:= 0 ->
            cand(C, Q, L, I, MaxC, Chain, BL, Best, Q, 4, ?STATE);
        true ->
            cand(C, Q, L, I, MaxC, Chain, BL, Best, Q, 3, ?STATE)
    end;
short(_C, _Q, _L, I, _MaxC, Chain, _BL, Best, _Mask, ?STATE) ->
    found(Best, I, Chain, ?STATE).

%% Walks the chain while indexes are inside the window (below MaxC), for
%% BL >= 4. Only candidates whose first 5 bytes match Q are compared. SQ is
%% Q when BL < 5, otherwise the 5 bytes ending BL bytes after I, which a
%% candidate must match to be longer than BL.
longest(C, Q, L, I, MaxC, Chain, BL, Best, SQ, ?STATE) when C < MaxC, Chain > 0 ->
    X = element(C, L),
    if
        X band ?M5 =:= Q, BL < 5 ->
            ext(C, Q, L, I, MaxC, Chain, BL, Best, SQ, 5, ?STATE);
        X band ?M5 =:= Q, element(C - BL + 4, L) band ?M5 =:= SQ, BL =< 9 ->
            ext(C, Q, L, I, MaxC, Chain, BL, Best, SQ, BL + 1, ?STATE);
        X band ?M5 =:= Q, element(C - BL + 4, L) band ?M5 =:= SQ ->
            ext(C, Q, L, I, MaxC, Chain, BL, Best, SQ, 5, ?STATE);
        true ->
            longest(X bsr 40, Q, L, I, MaxC, Chain - 1, BL, Best, SQ, ?STATE)
    end;
longest(_C, _Q, _L, I, _MaxC, Chain, _BL, Best, _SQ, ?STATE) ->
    found(Best, I, Chain, ?STATE).

%% Extends the match with the candidate at index C, whose first N bytes
%% match.
ext(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N, ?STATE) when N < I, N < 258 ->
    case (element(C - N, L) bxor element(I - N, L)) band ?M5 of
        0 ->
            ext(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N + 5, ?STATE);
        X when X >= 1 bsl 32 ->
            cand(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N, ?STATE);
        X when X >= 1 bsl 24 ->
            cand(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N + 1, ?STATE);
        X when X >= 1 bsl 16 ->
            cand(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N + 2, ?STATE);
        X when X >= 1 bsl 8 ->
            cand(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N + 3, ?STATE);
        _ ->
            cand(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N + 4, ?STATE)
    end;
ext(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N, ?STATE) ->
    cand(C, Q, L, I, MaxC, Chain, BL, Best, SQ, N, ?STATE).

%% Continues the chain walk after the candidate at index C matched Len0
%% bytes, which may exceed the maximum match length at I.
cand(C, Q, L, I, MaxC, Chain, BL, Best, SQ, Len0, ?STATE) ->
    Max = min(258, I),
    Len = min(Len0, Max),
    Match = ((C - I) bsl 9) bor Len,
    Next = element(C, L) bsr 40,
    if
        Len > BL, Len >= Cx#c.nice; Len >= Max ->
            found(Match, I, Chain - 1, ?STATE);
        Len > BL, Len >= 5 ->
            SQ1 = element(I - Len + 4, L) band ?M5,
            longest(Next, Q, L, I, MaxC, Chain - 1, Len, Match, SQ1, ?STATE);
        Len > BL ->
            walk(Next, Q, L, I, MaxC, Chain - 1, Len, Match, SQ, ?STATE);
        true ->
            walk(Next, Q, L, I, MaxC, Chain - 1, BL, Best, SQ, ?STATE)
    end.

%% Continues matching at Pos = Last - I with the search result M and the
%% search's chain steps left.
found(M, I, Chain, PM, Pending, Sparse, Ms, NS, BStart, W, Credit, #c{lazy = true} = Cx) ->
    lazy_step(Cx#c.last - I, PM, Pending, Sparse, Ms, NS, BStart, W, Credit + Chain, Cx, M);
found(M, I, Chain, _PM, _Pending, _Sparse, Ms, NS, BStart, W, Credit, Cx) ->
    greedy_step(M, Cx#c.last - I, Ms, NS, BStart, W, Credit + Chain, Cx).

%% Greedy matching (zlib's deflate_fast, but every position is inserted).
greedy(Pos, Ms, NS, BStart, W, Credit, #c{top = Top} = Cx) when Pos < Top ->
    #c{last = Last, links = Links, chain = Chain, budget = B} = Cx,
    I = Last - Pos,
    X = element(I, Links),
    Avail = min(Cx#c.max_credit, Credit + B * Pos),
    Limit = min(Avail, Chain),
    Credit1 = Avail - Limit - B * Pos,
    find(X bsr 40, X band ?M5, I, 2, Limit, 0, false, 0, Ms, NS, BStart, W, Credit1, Cx);
greedy(_Pos, Ms, _NS, BStart, W, _Credit, #c{last = Last} = Cx) ->
    flush(Ms, BStart, Last, Cx#c.final, W, Cx).

greedy_step(0, Pos, Ms, NS, BStart, W, Credit, Cx) ->
    greedy_next(Pos + 1, Ms, NS, BStart, W, Credit, Cx);
greedy_step(M, Pos, Ms, NS, BStart, W, Credit, Cx) ->
    Ms1 = <<Ms/binary, (Pos - BStart):32, M:32>>,
    greedy_next(Pos + (M band 511), Ms1, NS, BStart, W, Credit, Cx).

greedy_next(End, Ms, NS, BStart, W, Credit, #c{block_syms = BS} = Cx) when NS + 1 >= BS ->
    W1 = flush(Ms, BStart, End, false, W, Cx),
    greedy(End, <<>>, 0, End, W1, Credit, Cx);
greedy_next(End, Ms, NS, BStart, W, Credit, Cx) ->
    greedy(End, Ms, NS + 1, BStart, W, Credit, Cx).

%% Lazy matching as in zlib's deflate_slow: the match PM found at Pos - 1
%% (0 when none) is emitted only when Pos has no longer match. Pending is
%% true when the byte at Pos - 1 is still to be emitted, as a literal or as
%% the start of PM. From Sparse on, positions without a pending match are
%% only searched at multiples of stride. Searches spend chain steps from a
%% credit that accrues budget steps per byte, up to max_credit; Credit is
%% the credit minus budget * Pos, so that it only changes at searches.
lazy(Pos, PM, Pending, Sparse, Ms, NS, BStart, W, Credit, #c{top = Top} = Cx) when
    PM =:= 0, Pos >= Sparse, Pos band Cx#c.stride_mask =/= 0, Pos < Top
->
    %% The positions up to the next multiple of stride aren't searched: each
    %% emits the byte before it as a literal.
    Next = min((Pos bor Cx#c.stride_mask) + 1, Top),
    NS1 =
        if
            Pending -> NS + Next - Pos;
            true -> NS + Next - Pos - 1
        end,
    if
        NS1 < Cx#c.block_syms ->
            lazy(Next, 0, true, Sparse, Ms, NS1, BStart, W, Credit, Cx);
        true ->
            lazy_step(Pos, ?STATE, 0)
    end;
lazy(Pos, PM, Pending, Sparse, Ms, NS, BStart, W, Credit, #c{top = Top} = Cx) when Pos < Top ->
    #c{last = Last, links = Links, max_lazy = ML} = Cx,
    I = Last - Pos,
    X = element(I, Links),
    PL = PM band 511,
    if
        PL < ML,
        X bsr 40 =< I + ?MAX_DIST,
        Pos < Sparse orelse PM > 0 orelse Pos band Cx#c.stride_mask =:= 0 ->
            #c{good = Good, chain = Chain, budget = B} = Cx,
            Avail = min(Cx#c.max_credit, Credit + B * Pos),
            Limit =
                if
                    PL >= Good -> min(Avail, Chain bsr 2);
                    true -> min(Avail, Chain)
                end,
            Credit1 = Avail - Limit - B * Pos,
            Q = X band ?M5,
            find(
                X bsr 40,
                Q,
                I,
                max(PL, 2),
                Limit,
                PM,
                Pending,
                Sparse,
                Ms,
                NS,
                BStart,
                W,
                Credit1,
                Cx
            );
        PM =:= 0, Pending, NS + 1 < Cx#c.block_syms ->
            %% Emit the byte at Pos - 1 as a literal (lazy_step/11 without a
            %% match on either side).
            lazy(Pos + 1, 0, true, Sparse, Ms, NS + 1, BStart, W, Credit, Cx);
        true ->
            lazy_step(Pos, ?STATE, 0)
    end;
lazy(Pos, PM, _Pending, _Sparse, Ms, _NS, BStart, W, _Credit, #c{last = Last} = Cx) ->
    %% At most three bytes left: no new match starts here, the rest are
    %% literals.
    Ms1 =
        if
            PM band 511 >= 3 -> <<Ms/binary, (Pos - 1 - BStart):32, PM:32>>;
            true -> Ms
        end,
    flush(Ms1, BStart, Last, Cx#c.final, W, Cx).

%% Continues lazy matching at Pos with M, the match found there.
lazy_step(Pos, ?STATE, M0) ->
    M =
        if
            M0 band 511 =:= 3, M0 bsr 9 > Cx#c.too_far -> 0;
            true -> M0
        end,
    PL = PM band 511,
    BS = Cx#c.block_syms,
    if
        PL >= 3, M band 511 =< PL, NS + 1 >= BS ->
            %% Emit the previous match, which covers Pos - 1 .. Pos + PL - 2.
            End = Pos + PL - 1,
            W1 = flush(<<Ms/binary, (Pos - 1 - BStart):32, PM:32>>, BStart, End, false, W, Cx),
            lazy(End, 0, false, End + Cx#c.skip, <<>>, 0, End, W1, Credit, Cx);
        PL >= 3, M band 511 =< PL ->
            Ms1 = <<Ms/binary, (Pos - 1 - BStart):32, PM:32>>,
            End = Pos + PL - 1,
            lazy(End, 0, false, End + Cx#c.skip, Ms1, NS + 1, BStart, W, Credit, Cx);
        Pending, NS + 1 >= BS ->
            %% Emit the byte at Pos - 1 as a literal.
            W1 = flush(Ms, BStart, Pos, false, W, Cx),
            lazy(Pos + 1, M, true, Sparse, <<>>, 0, Pos, W1, Credit, Cx);
        Pending ->
            lazy(Pos + 1, M, true, Sparse, Ms, NS + 1, BStart, W, Credit, Cx);
        true ->
            lazy(Pos + 1, M, true, Sparse, Ms, NS, BStart, W, Credit, Cx)
    end.

%%====================================================================
%% Blocks
%%====================================================================

%% Ms holds the block's matches as <<Offset:32, Match:32>>: the offset from
%% BStart and (Dist bsl 9) bor Len. The bytes not covered by matches are
%% literals.
flush(Ms, BStart, BEnd, Final, W, #c{d = D, lsym = LS, dcode = DC} = Cx) ->
    F = atomics:new(?DIST_BASE + 30, []),
    atomics:put(F, ?EOB + 1, 1),
    count(Ms, BStart, BStart, -1, Cx#c.last, Cx#c.links, F, {LS, DC, BStart, BEnd}),
    LF = get_list(F, 1, 286, []),
    DF = get_list(F, ?DIST_BASE + 1, ?DIST_BASE + 30, []),
    {LLens, LCounts} = huff_lengths(LF, 286, 15),
    {DLens, DCounts} = huff_lengths(DF, 30, 15),
    Extra = extra_bits(LF, DF),
    %% The size of a dynamic block without its header, which is only built
    %% when it can change the choice.
    DynData = 3 + weighted_a(LF, LLens) + weighted_a(DF, DLens) + Extra,
    {FLLens, FDLens, FL, FD} = Cx#c.fixed,
    FixBits = 3 + weighted(LF, FLLens) + weighted(DF, FDLens) + Extra,
    Bytes = BEnd - BStart,
    Chunks = max(1, (Bytes + 65534) div 65535),
    StoredBits = 8 * Bytes + 40 * Chunks + 7,
    FinalBit =
        if
            Final -> 1;
            true -> 0
        end,
    if
        StoredBits < FixBits, StoredBits < DynData ->
            stored(binary_part(D, BStart, Bytes), FinalBit, W);
        FixBits =< StoredBits, FixBits =< DynData ->
            encode(Ms, BStart, BEnd, FL, FD, put(FinalBit bor (1 bsl 1), 3, W), Cx);
        true ->
            Header = dyn_header(LLens, DLens),
            DynBits = DynData + element(1, Header),
            if
                StoredBits < DynBits, StoredBits < FixBits ->
                    stored(binary_part(D, BStart, Bytes), FinalBit, W);
                FixBits =< DynBits ->
                    encode(Ms, BStart, BEnd, FL, FD, put(FinalBit bor (1 bsl 1), 3, W), Cx);
                true ->
                    W1 = put(FinalBit bor (2 bsl 1), 3, W),
                    W2 = put_header(Header, W1),
                    LC = codes(LLens, 286, LCounts),
                    encode(Ms, BStart, BEnd, LC, codes(DLens, 30, DCounts), W2, Cx)
            end
    end.

%% [atomics:get(A, I) || I <- lists:seq(From, To)] ++ Acc.
get_list(A, From, To, Acc) when To >= From ->
    get_list(A, From, To - 1, [atomics:get(A, To) | Acc]);
get_list(_A, _From, _To, Acc) ->
    Acc.

%% Counts in F the codes of the literals from Pos up to MPos, of the match
%% M at MPos (-1: read the next match from Ms, 0: none, the block ends at
%% MPos) and of the rest of the block. Literals are read from the chunk's
%% links (see links/4).
count(<<Ms/binary>>, Pos, MPos, M, Last, Ws, F, Tables) when Pos < MPos ->
    atomics:add(F, ((element(Last - Pos, Ws) bsr 32) band 255) + 1, 1),
    count(Ms, Pos + 1, MPos, M, Last, Ws, F, Tables);
count(<<Off:32, M:32, Ms/binary>>, Pos, _MPos, -1, Last, Ws, F, Tables) ->
    count(Ms, Pos, element(3, Tables) + Off, M, Last, Ws, F, Tables);
count(<<>>, Pos, _MPos, -1, Last, Ws, F, Tables) ->
    count(<<>>, Pos, element(4, Tables), 0, Last, Ws, F, Tables);
count(<<Ms/binary>>, Pos, _MPos, M, Last, Ws, F, {LS, DC, _, _} = Tables) when M > 0 ->
    atomics:add(F, element((M band 511) - 2, LS) + 258, 1),
    atomics:add(F, dist_code((M bsr 9) - 1, DC) + ?DIST_BASE + 1, 1),
    End = Pos + (M band 511),
    count(Ms, End, End, -1, Last, Ws, F, Tables);
count(<<>>, _Pos, _MPos, 0, _Last, _Ws, _F, _Tables) ->
    ok.

weighted(Freqs, Lens) ->
    weighted(Freqs, Lens, 1, 0).

weighted([F | T], Lens, I, Acc) -> weighted(T, Lens, I + 1, Acc + F * element(I, Lens));
weighted([], _, _, Acc) -> Acc.

%% weighted/2 with the lengths in atomics.
weighted_a(Freqs, Lens) ->
    weighted_a(Freqs, Lens, 1, 0).

weighted_a([0 | T], Lens, I, Acc) -> weighted_a(T, Lens, I + 1, Acc);
weighted_a([F | T], Lens, I, Acc) -> weighted_a(T, Lens, I + 1, Acc + F * atomics:get(Lens, I));
weighted_a([], _, _, Acc) -> Acc.

extra_bits(LF, DF) ->
    extra_bits(lists:nthtail(265, LF), 265, 0) + extra_bits(DF, 0, 0).

%% Lengths codes 265..284 have (Sym - 261) div 4 extra bits, distance codes
%% D >= 2 have D div 2 - 1.
extra_bits([F | T], S, Acc) when S >= 265, S < 285 ->
    extra_bits(T, S + 1, Acc + F * ((S - 261) div 4));
extra_bits([_ | T], S, Acc) when S >= 265 -> extra_bits(T, S + 1, Acc);
extra_bits([F | T], S, Acc) when S >= 2 -> extra_bits(T, S + 1, Acc + F * (S div 2 - 1));
extra_bits([_ | T], S, Acc) ->
    extra_bits(T, S + 1, Acc);
extra_bits([], _, Acc) ->
    Acc.

stored(Data, FinalBit, {Out, Acc, NB, Segs}) ->
    {<<>>, 0, 0, [{stored, FinalBit, Data}, bits(Out, Acc, NB) | Segs]}.

put(V, N, {Out, Acc, NB, Segs}) ->
    Acc1 = Acc bor (V bsl NB),
    NB1 = NB + N,
    if
        NB1 >= 32 -> {<<Out/binary, Acc1:32/little>>, Acc1 bsr 32, NB1 - 32, Segs};
        true -> {Out, Acc1, NB1, Segs}
    end.

%% LitCodes: tuple of 286 packed (RevCode bsl 4) bor Len; DistCodes: 30.
encode(Ms, BStart, BEnd, LitCodes, DistCodes, {Out, Acc, NB, Segs}, Cx) ->
    #c{last = Last, links = Ws, lsym = LS, dcode = DC} = Cx,
    LenT = list_to_tuple(len_entries(258, LS, LitCodes, [])),
    DistT = list_to_tuple(dist_entries(29, DistCodes, [])),
    K = {LenT, DistT, DC, BStart, BEnd},
    %% The block's bytes start from an empty binary, which enc/11 appends to
    %% in place.
    {Bytes, Acc1, NB1} = enc(Ms, BStart, BStart, -1, Last, Ws, LitCodes, K, <<>>, Acc, NB),
    E = element(?EOB + 1, LitCodes),
    put(E bsr 4, E band 15, {<<Out/binary, Bytes/binary>>, Acc1, NB1, Segs}).

len_entries(2, _LS, _LitCodes, Acc) ->
    Acc;
len_entries(L, LS, LitCodes, Acc) ->
    len_entries(L - 1, LS, LitCodes, [len_entry(L, LS, LitCodes) | Acc]).

dist_entries(-1, _DistCodes, Acc) -> Acc;
dist_entries(D, DistCodes, Acc) -> dist_entries(D - 1, DistCodes, [dist_entry(D, DistCodes) | Acc]).

len_entry(L, LS, LitCodes) ->
    I = element(L - 2, LS),
    P = element(I + 258, LitCodes),
    CL = P band 15,
    {Base, EB} = len_base(I),
    ((P bsr 4) bor ((L - Base) bsl CL)) bsl 5 bor (CL + EB).

dist_entry(D, DistCodes) ->
    P = element(D + 1, DistCodes),
    {Base, EB} = dist_base(D),
    ((P bsl 5 bor EB) bsl 15) bor (Base - 1).

%% Encodes the literals from Pos up to MPos, the match M at MPos (-1: read
%% the next match from Ms, 0: none, the block ends at MPos) and the rest of
%% the block, like count/8.
enc(<<Ms/binary>>, Pos, MPos, M, Last, Ws, LitT, K, Out, Acc, NB) when Pos < MPos ->
    P = element(((element(Last - Pos, Ws) bsr 32) band 255) + 1, LitT),
    Acc1 = Acc bor ((P bsr 4) bsl NB),
    NB1 = NB + (P band 15),
    if
        NB1 >= 32 ->
            Out1 = <<Out/binary, Acc1:32/little>>,
            enc(Ms, Pos + 1, MPos, M, Last, Ws, LitT, K, Out1, Acc1 bsr 32, NB1 - 32);
        true ->
            enc(Ms, Pos + 1, MPos, M, Last, Ws, LitT, K, Out, Acc1, NB1)
    end;
enc(<<Off:32, M:32, Ms/binary>>, Pos, _MPos, -1, Last, Ws, LitT, K, Out, Acc, NB) ->
    enc(Ms, Pos, element(4, K) + Off, M, Last, Ws, LitT, K, Out, Acc, NB);
enc(<<>>, Pos, _MPos, -1, Last, Ws, LitT, K, Out, Acc, NB) ->
    enc(<<>>, Pos, element(5, K), 0, Last, Ws, LitT, K, Out, Acc, NB);
enc(<<Ms/binary>>, Pos, _MPos, M, Last, Ws, LitT, K, Out, Acc, NB) when M > 0 ->
    {LenT, DistT, DC, _, _} = K,
    P = element((M band 511) - 2, LenT),
    D0 = (M bsr 9) - 1,
    %% DistT entry: (((RevCode bsl 4 bor CodeLen) bsl 5 bor ExtraBits) bsl 15)
    %% bor Base0
    E = element(dist_code(D0, DC) + 1, DistT),
    CL = (E bsr 20) band 15,
    Dist = (E bsr 24) bor ((D0 - (E band 16#7FFF)) bsl CL),
    DistN = CL + ((E bsr 15) band 31),
    Acc1 = Acc bor ((P bsr 5) bsl NB),
    NB1 = NB + (P band 31),
    End = Pos + (M band 511),
    if
        NB1 >= 32 ->
            Out1 = <<Out/binary, Acc1:32/little>>,
            Acc2 = (Acc1 bsr 32) bor (Dist bsl (NB1 - 32)),
            NB2 = NB1 - 32 + DistN,
            if
                NB2 >= 32 ->
                    Out2 = <<Out1/binary, Acc2:32/little>>,
                    enc(Ms, End, End, -1, Last, Ws, LitT, K, Out2, Acc2 bsr 32, NB2 - 32);
                true ->
                    enc(Ms, End, End, -1, Last, Ws, LitT, K, Out1, Acc2, NB2)
            end;
        true ->
            Acc2 = Acc1 bor (Dist bsl NB1),
            NB2 = NB1 + DistN,
            if
                NB2 >= 32 ->
                    Out1 = <<Out/binary, Acc2:32/little>>,
                    enc(Ms, End, End, -1, Last, Ws, LitT, K, Out1, Acc2 bsr 32, NB2 - 32);
                true ->
                    enc(Ms, End, End, -1, Last, Ws, LitT, K, Out, Acc2, NB2)
            end
    end;
enc(<<>>, _Pos, _MPos, 0, _Last, _Ws, _LitT, _K, Out, Acc, NB) ->
    {Out, Acc, NB}.

%%====================================================================
%% Huffman codes
%%====================================================================

%% Code lengths limited to MaxBits, in atomics indexed by symbol + 1, and
%% the number of codes of each length (tuple indexed by length). At least
%% two symbols always get a code so that every code is complete.
huff_lengths(Freqs, NSyms, MaxBits) ->
    %% Symbols sorted by frequency, then symbol, packed as (F bsl 9) bor S.
    Sorted = lists:sort(pad_used(used(Freqs, 0))),
    Counts = limit_counts(depth_counts(Sorted, MaxBits), MaxBits),
    Lens = atomics:new(NSyms, []),
    assign(Sorted, MaxBits, element(MaxBits, Counts), Counts, Lens),
    {Lens, Counts}.

used([F | T], S) when F > 0 -> [(F bsl 9) bor S | used(T, S + 1)];
used([_ | T], S) -> used(T, S + 1);
used([], _) -> [].

pad_used([]) -> [1 bsl 9, (1 bsl 9) bor 1];
pad_used([X]) when X band 511 =:= 0 -> [(1 bsl 9) bor 1, X];
pad_used([X]) -> [1 bsl 9, X];
pad_used(U) -> U.

%% Number of leaves at each depth 1..MaxBits (deeper leaves counted at
%% MaxBits) of the Huffman tree over the sorted leaves, built with the
%% two-queue method (ties prefer leaves). Internal nodes are merged in the
%% order they are created, so the internal children of the nodes, from the
%% root down, are the nodes in the order that follows; merge/5 and merge2/7
%% return the number of internal children of each node, root first, and
%% levels/5 counts the leaves at each depth from them.
depth_counts([X0, X1 | Xs], MaxBits) ->
    Cs = merge(Xs, [], [(X0 bsr 9) + (X1 bsr 9)], [0], length(Xs)),
    clamp(levels(Cs, 1, 0, 0, []), 1, MaxBits).

%% Creates K more internal nodes from the leaves Ls and the internal nodes
%% not merged yet, Is followed by the reverse of Back (weights only):
%% merge/5 takes the first child, merge2/7 the second.
merge(_Ls, _Is, _Back, Cs, 0) ->
    Cs;
merge(Ls, [], [_ | _] = Back, Cs, K) ->
    merge(Ls, lists:reverse(Back), [], Cs, K);
merge([L | _] = Ls, [I | Is], Back, Cs, K) when I < L bsr 9 ->
    merge2(Ls, Is, Back, Cs, K, I, 1);
merge([L | Ls], Is, Back, Cs, K) ->
    merge2(Ls, Is, Back, Cs, K, L bsr 9, 0);
merge([], [I | Is], Back, Cs, K) ->
    merge2([], Is, Back, Cs, K, I, 1).

merge2(Ls, [], [_ | _] = Back, Cs, K, W, C) ->
    merge2(Ls, lists:reverse(Back), [], Cs, K, W, C);
merge2([L | _] = Ls, [I | Is], Back, Cs, K, W, C) when I < L bsr 9 ->
    merge(Ls, Is, [W + I | Back], [C + 1 | Cs], K - 1);
merge2([L | Ls], Is, Back, Cs, K, W, C) ->
    merge(Ls, Is, [W + (L bsr 9) | Back], [C | Cs], K - 1);
merge2([], [I | Is], Back, Cs, K, W, C) ->
    merge([], Is, [W + I | Back], [C + 1 | Cs], K - 1).

%% Leaves at each depth from D + 1 on, from the internal nodes at depth D
%% (N left) and their internal children (N1) and leaves (Leaves1) at depth
%% D + 1.
levels([C | Cs], N, N1, Leaves1, Acc) when N > 0 ->
    levels(Cs, N - 1, N1 + C, Leaves1 + 2 - C, Acc);
levels(Cs, 0, N1, Leaves1, Acc) when N1 > 0 ->
    levels(Cs, N1, 0, 0, [Leaves1 | Acc]);
levels([], 0, 0, Leaves1, Acc) ->
    lists:reverse([Leaves1 | Acc]).

clamp([C | Cs], I, MaxBits) when I < MaxBits -> [C | clamp(Cs, I + 1, MaxBits)];
clamp(Cs, MaxBits, MaxBits) -> [lists:sum(Cs)];
clamp([], I, MaxBits) -> [0 | clamp([], I + 1, MaxBits)].

%% Restores the Kraft equality after deeper leaves were counted at MaxBits
%% (miniz's tdefl_huffman_enforce_max_code_size). Returns a tuple of counts
%% for lengths 1..MaxBits.
limit_counts(Counts, MaxBits) ->
    C = list_to_tuple(Counts),
    fix_kraft(C, kraft(C, MaxBits, 1, 0), MaxBits).

kraft(C, MaxBits, I, Acc) when I =< MaxBits ->
    kraft(C, MaxBits, I + 1, Acc + (element(I, C) bsl (MaxBits - I)));
kraft(_C, _MaxBits, _I, Acc) ->
    Acc.

fix_kraft(Counts, Total, MaxBits) when Total =:= 1 bsl MaxBits ->
    Counts;
fix_kraft(Counts, Total, MaxBits) ->
    C1 = setelement(MaxBits, Counts, element(MaxBits, Counts) - 1),
    I = find_nonzero(C1, MaxBits - 1),
    C2 = setelement(I, C1, element(I, C1) - 1),
    C3 = setelement(I + 1, C2, element(I + 1, C2) + 2),
    fix_kraft(C3, Total - 1, MaxBits).

find_nonzero(C, I) when element(I, C) > 0 -> I;
find_nonzero(C, I) -> find_nonzero(C, I - 1).

%% Highest frequency symbols get the shortest codes: from the lowest, the
%% symbols get length L while K of that length are left.
assign([X | T], L, K, Counts, Lens) when K > 0 ->
    atomics:put(Lens, (X band 511) + 1, L),
    assign(T, L, K - 1, Counts, Lens);
assign([_ | _] = Syms, L, 0, Counts, Lens) ->
    assign(Syms, L - 1, element(L - 1, Counts), Counts, Lens);
assign([], _L, _K, _Counts, _Lens) ->
    ok.

%% Canonical codes: tuple of (BitReversedCode bsl 4) bor Len, 0 if unused.
%% Symbols are visited from the highest, which gets the last code of its
%% length.
codes(Lens, NSyms, Counts) ->
    Next = atomics:new(tuple_size(Counts), []),
    end_codes(Next, Counts, 1, 0),
    list_to_tuple(code_list(Lens, Next, NSyms, [])).

end_codes(Next, Counts, L, Code) when L =< tuple_size(Counts) ->
    End = Code + element(L, Counts),
    atomics:put(Next, L, End),
    end_codes(Next, Counts, L + 1, End bsl 1);
end_codes(_Next, _Counts, _L, _Code) ->
    ok.

code_list(_Lens, _Next, 0, Acc) ->
    Acc;
code_list(Lens, Next, S, Acc) ->
    code_list(Lens, Next, S - 1, [code(atomics:get(Lens, S), Next) | Acc]).

code(0, _Next) ->
    0;
code(L, Next) ->
    C = atomics:sub_get(Next, L, 1),
    C1 = ((C band 16#5555) bsl 1) bor ((C bsr 1) band 16#5555),
    C2 = ((C1 band 16#3333) bsl 2) bor ((C1 bsr 2) band 16#3333),
    C3 = ((C2 band 16#0F0F) bsl 4) bor ((C2 bsr 4) band 16#0F0F),
    ((((C3 band 16#FF) bsl 8) bor (C3 bsr 8)) bsr (16 - L)) bsl 4 bor L.

%% Dynamic block header after BTYPE: {Bits, ...} with its size in bits and
%% what put_header/2 writes.
dyn_header(LLens, DLens) ->
    HLit = last_nonzero(LLens, 286, 257),
    HDist = last_nonzero(DLens, 30, 1),
    Rle = rle({LLens, HLit, DLens}, 1, HLit + HDist),
    CF = atomics:new(19, []),
    _ = [atomics:add(CF, (X band 31) + 1, 1) || X <- Rle],
    {CLens, CCounts} = huff_lengths(get_list(CF, 1, 19, []), 19, 7),
    CCodes = codes(CLens, 19, CCounts),
    Order = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15],
    OrderedLens = [atomics:get(CLens, S + 1) || S <- Order],
    HCLen = max(4, 19 - length(lists:takewhile(fun(L) -> L =:= 0 end, lists:reverse(OrderedLens)))),
    Bits = 14 + 3 * HCLen + rle_bits(Rle, CCodes, 0),
    {Bits, (HLit - 257) bor ((HDist - 1) bsl 5) bor ((HCLen - 4) bsl 10), HCLen, OrderedLens, Rle,
        CCodes}.

last_nonzero(_Lens, I, Min) when I =< Min -> Min;
last_nonzero(Lens, I, Min) ->
    case atomics:get(Lens, I) of
        0 -> last_nonzero(Lens, I - 1, Min);
        _ -> I
    end.

%% Code length run-length encoding of the literal/length code lengths
%% 1..HLit followed by the distance code lengths: list of
%% Sym bor (ExtraValue bsl 5).
rle(Src, I, End) when I =< End ->
    run(Src, I + 1, End, len_at(Src, I), 1);
rle(_Src, _I, _End) ->
    [].

run(Src, I, End, L, N) when I =< End ->
    case len_at(Src, I) of
        L -> run(Src, I + 1, End, L, N + 1);
        _ -> runs(L, N, rle(Src, I, End))
    end;
run(_Src, _I, _End, L, N) ->
    runs(L, N, []).

len_at({LLens, HLit, _DLens}, I) when I =< HLit -> atomics:get(LLens, I);
len_at({_LLens, HLit, DLens}, I) -> atomics:get(DLens, I - HLit).

runs(0, N, Rest) -> zeros(N, Rest);
runs(L, N, Rest) when N >= 4 -> [L | repeat(N - 1, L, Rest)];
runs(L, N, Rest) -> dup(N, L, Rest).

zeros(N, Rest) when N >= 11 ->
    K = min(N, 138),
    [18 bor ((K - 11) bsl 5) | zeros(N - K, Rest)];
zeros(N, Rest) when N >= 3 -> [17 bor ((N - 3) bsl 5) | Rest];
zeros(N, Rest) ->
    dup(N, 0, Rest).

repeat(N, L, Rest) when N >= 3 ->
    K = min(N, 6),
    [16 bor ((K - 3) bsl 5) | repeat(N - K, L, Rest)];
repeat(N, L, Rest) ->
    dup(N, L, Rest).

dup(0, _L, Rest) -> Rest;
dup(N, L, Rest) -> [L | dup(N - 1, L, Rest)].

rle_bits([X | T], CCodes, Acc) ->
    rle_bits(T, CCodes, Acc + (element((X band 31) + 1, CCodes) band 15) + rle_extra(X band 31));
rle_bits([], _CCodes, Acc) ->
    Acc.

rle_extra(16) -> 2;
rle_extra(17) -> 3;
rle_extra(18) -> 7;
rle_extra(_) -> 0.

put_header({_Bits, V, HCLen, OrderedLens, Rle, CCodes}, {Out, Acc, NB, Segs}) ->
    put_rle(Rle, CCodes, put_clens(OrderedLens, HCLen, Out, Acc, NB, V, 14), Segs).

%% Writes V (N bits), then the first K code length code lengths.
put_clens(Lens, K, Out, Acc, NB, V, N) ->
    Acc1 = Acc bor (V bsl NB),
    NB1 = NB + N,
    if
        NB1 >= 32 -> put_clens(Lens, K, <<Out/binary, Acc1:32/little>>, Acc1 bsr 32, NB1 - 32);
        true -> put_clens(Lens, K, Out, Acc1, NB1)
    end.

put_clens([L | T], K, Out, Acc, NB) when K > 0 ->
    put_clens(T, K - 1, Out, Acc, NB, L, 3);
put_clens(_Lens, _K, Out, Acc, NB) ->
    {Out, Acc, NB}.

put_rle(Rle, CCodes, {Out, Acc, NB}, Segs) ->
    put_rle(Rle, CCodes, Out, Acc, NB, Segs).

put_rle([X | T], CCodes, Out, Acc, NB, Segs) ->
    S = X band 31,
    P = element(S + 1, CCodes),
    CL = P band 15,
    Acc1 = Acc bor (((P bsr 4) bor ((X bsr 5) bsl CL)) bsl NB),
    NB1 = NB + CL + rle_extra(S),
    if
        NB1 >= 32 ->
            put_rle(T, CCodes, <<Out/binary, Acc1:32/little>>, Acc1 bsr 32, NB1 - 32, Segs);
        true ->
            put_rle(T, CCodes, Out, Acc1, NB1, Segs)
    end;
put_rle([], _CCodes, Out, Acc, NB, Segs) ->
    {Out, Acc, NB, Segs}.

%%====================================================================
%% Static tables
%%====================================================================

%% Length 3..258 (index Length - 2) -> length code index 0..28.
len_sym_table() ->
    Runs = [lists:duplicate(1 bsl element(2, len_base(I)), I) || I <- lists:seq(0, 27)],
    list_to_tuple(lists:droplast(lists:append(Runs)) ++ [28]).

len_base(I) ->
    element(I + 1, {
        {3, 0},
        {4, 0},
        {5, 0},
        {6, 0},
        {7, 0},
        {8, 0},
        {9, 0},
        {10, 0},
        {11, 1},
        {13, 1},
        {15, 1},
        {17, 1},
        {19, 2},
        {23, 2},
        {27, 2},
        {31, 2},
        {35, 3},
        {43, 3},
        {51, 3},
        {59, 3},
        {67, 4},
        {83, 4},
        {99, 4},
        {115, 4},
        {131, 5},
        {163, 5},
        {195, 5},
        {227, 5},
        {258, 0}
    }).

dist_base(I) ->
    element(I + 1, {
        {1, 0},
        {2, 0},
        {3, 0},
        {4, 0},
        {5, 1},
        {7, 1},
        {9, 2},
        {13, 2},
        {17, 3},
        {25, 3},
        {33, 4},
        {49, 4},
        {65, 5},
        {97, 5},
        {129, 6},
        {193, 6},
        {257, 7},
        {385, 7},
        {513, 8},
        {769, 8},
        {1025, 9},
        {1537, 9},
        {2049, 10},
        {3073, 10},
        {4097, 11},
        {6145, 11},
        {8193, 12},
        {12289, 12},
        {16385, 13},
        {24577, 13}
    }).

%% zlib's _dist_code: D0 < 256 -> T[D0], else T[256 + (D0 bsr 7)].
dist_code_table() ->
    Low = [lists:duplicate(1 bsl element(2, dist_base(I)), I) || I <- lists:seq(0, 15)],
    High = [lists:duplicate(1 bsl (element(2, dist_base(I)) - 7), I) || I <- lists:seq(16, 29)],
    list_to_tuple(lists:append(Low) ++ [0, 14] ++ lists:append(High)).

dist_code(D0, T) when D0 < 256 -> element(D0 + 1, T);
dist_code(D0, T) -> element(257 + (D0 bsr 7), T).

fixed_tables() ->
    LLens =
        lists:duplicate(144, 8) ++ lists:duplicate(112, 9) ++ lists:duplicate(24, 7) ++
            [8, 8, 8, 8, 8, 8],
    %% Canonical codes for 288/32 symbols; 286..287 and 30..31 unused.
    LCodes = codes(to_atomics(LLens ++ [8, 8]), 288, {0, 0, 0, 0, 0, 0, 24, 152, 112}),
    DCodes = codes(to_atomics(lists:duplicate(32, 5)), 32, {0, 0, 0, 0, 32}),
    {
        list_to_tuple(LLens),
        erlang:make_tuple(30, 5),
        list_to_tuple(lists:sublist(tuple_to_list(LCodes), 286)),
        list_to_tuple(lists:sublist(tuple_to_list(DCodes), 30))
    }.

to_atomics(L) ->
    A = atomics:new(length(L), []),
    _ = [atomics:put(A, I, X) || {I, X} <- lists:zip(lists:seq(1, length(L)), L)],
    A.
