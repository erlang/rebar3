Definitions.

D = [0-9]

Rules.

{D}+ :
  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.

{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
  {token,{float,TokenLine,list_to_float(TokenChars)}}.

Erlang code.
