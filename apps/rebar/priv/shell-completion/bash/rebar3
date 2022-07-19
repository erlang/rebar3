# bash completion for rebar3

_rebar3()
{
    local cur prev sopts lopts cmdsnvars
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    if [[ ${prev} == rebar3 ]] ; then
        sopts="-h -v"
        lopts="--help --version"
        cmdsnvars=" \
            as \
            clean \
            compile \
            cover \
            ct \
            deps \
            dialyzer \
            do \
            edoc \
            escriptize \
            eunit \
            help \
            new \
            path \
            pkgs \
            plugins \
            release \
            relup \
            report \
            shell \
            tar \
            tree \
            unlock \
            unstable \
            update \
            upgrade \
            version \
            xref \
            "
    elif [[ ${prev} == as ]] ; then
        :
    elif [[ ${prev} == clean ]] ; then
        sopts="-a"
        lopts="--all"
    elif [[ ${prev} == compile ]] ; then
        :
    elif [[ ${prev} == cover ]] ; then
        sopts="-r -v"
        lopts="--reset --verbose"
    elif [[ ${prev} == ct ]] ; then
        sopts="-c -v"
        lopts=" \
            --dir \
            --suite \
            --group \
            --case \
            --config \
            --allow_user_terms \
            --logdir \
            --logopts \
            --verbosity \
            --silent_connections \
            --stylesheet \
            --cover \
            --repeat \
            --duration \
            --until \
            --force_stop \
            --basic_html \
            --stylesheet \
            --decrypt_key \
            --decrypt_file \
            --abort_if_missing_suites \
            --multiply_timetraps \
            --scale_timetraps \
            --create_priv_dir \
            --include \
            --verbose \
            --auto_compile \
            "
    elif [[ ${prev} == deps ]] ; then
        :
    elif [[ ${prev} == dialyzer ]] ; then
        sopts="-u -s"
        lopts="--update-plt --succ-typings"
    elif [[ ${prev} == do ]] ; then
        :
    elif [[ ${prev} == edoc ]] ; then
        :
    elif [[ ${prev} == escriptize ]] ; then
        :
    elif [[ ${prev} == eunit ]] ; then
        sopts="-c -e -v -d -f -m -s -g"
        lopts="--app --application --cover --dir --error_on_warning --file --module --suite --generator --verbose"
    elif [[ ${prev} == help ]] ; then
        :
    elif [[ ${prev} == new ]] ; then
        sopts="-f"
        lopts="--force"
    elif [[ ${prev} == path ]] ; then
        sopts="-s"
        lopts=" \
            --app \
            --base \
            --bin \
            --ebin \
            --lib \
            --priv \
            --separator \
            --src \
            --rel \
            "
    elif [[ ${prev} == pkgs ]] ; then
        :
    elif [[ ${prev} == plugins ]] ; then
        :
    elif [[ ${prev} == release ]] ; then
        sopts="-n -v -g -u -o -h -l -p -V -d -i -a -c -r"
        lopts=" \
            --relname \
            --relvsn \
            --goal \
            --upfrom \
            --output-dir \
            --help \
            --lib-dir \
            --path \
            --default-libs \
            --verbose \
            --dev-mode \
            --include-erts \
            --override \
            --config \
            --overlay_vars \
            --vm_args \
            --sys_config \
            --system_libs \
            --version \
            --root \
            "
    elif [[ ${prev} == relup ]] ; then
        sopts="-n -v -g -u -o -h -l -p -V -d -i -a -c -r"
        lopts=" \
            --relname \
            --relvsn \
            --goal \
            --upfrom \
            --output-dir \
            --help \
            --lib-dir \
            --path \
            --default-libs \
            --verbose \
            --dev-mode \
            --include-erts \
            --override \
            --config \
            --overlay_vars \
            --vm_args \
            --sys_config \
            --system_libs \
            --version \
            --root \
            "
    elif [[ ${prev} == report ]] ; then
        :
    elif [[ ${prev} == shell ]] ; then
        :
    elif [[ ${prev} == tar ]] ; then
        sopts="-n -v -g -u -o -h -l -p -V -d -i -a -c -r"
        lopts=" \
            --relname \
            --relvsn \
            --goal \
            --upfrom \
            --output-dir \
            --help \
            --lib-dir \
            --path \
            --default-libs \
            --verbose \
            --dev-mode \
            --include-erts \
            --override \
            --config \
            --overlay_vars \
            --vm_args \
            --sys_config \
            --system_libs \
            --version \
            --root \
            "
    elif [[ ${prev} == tree ]] ; then
        sopts="-v"
        lopts="--verbose"
    elif [[ ${prev} == unstable ]] ; then
        :
    elif [[ ${prev} == update ]] ; then
        :
    elif [[ ${prev} == upgrade ]] ; then
        :
    elif [[ ${prev} == version ]] ; then
        :
    elif [[ ${prev} == xref ]] ; then
        :
    fi

    COMPREPLY=( $(compgen -W "${sopts} ${lopts} ${cmdsnvars}" -- ${cur}) )

    if [ -n "$COMPREPLY" ] ; then
        # append space if matched
        COMPREPLY="${COMPREPLY} "
        # remove trailing space after equal sign
        COMPREPLY=${COMPREPLY/%= /=}
    fi
    return 0
}
complete -o nospace -F _rebar3 rebar3

# Local variables:
# mode: shell-script
# sh-basic-offset: 4
# sh-indent-comment: t
# indent-tabs-mode: nil
# End:
# ex: ts=4 sw=4 et filetype=sh
