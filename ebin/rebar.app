{application, rebar,
 [{description, "Rebar: Erlang Build Tool"},
  {vsn, "2"},
  {modules, [ rebar,
              rebar_abnfc_compiler,
              rebar_appups,
              rebar_app_utils,
              rebar_base_compiler,
              rebar_config,
              rebar_core,
              rebar_cleaner,
              rebar_ct,
              rebar_deps,
              rebar_dialyzer,
              rebar_asn1_compiler,
              rebar_edoc,
              rebar_erlc_compiler,
              rebar_escripter,
              rebar_eunit,
              rebar_file_utils,
              rebar_lfe_compiler,
              rebar_erlydtl_compiler,
              rebar_log,
              rebar_otp_app,
              rebar_port_compiler,
              rebar_protobuffs_compiler,
              rebar_neotoma_compiler,
              rebar_port_compiler,
              rebar_post_script,
              rebar_pre_script,
              rebar_rel_utils,
              rebar_reltool,
              rebar_require_vsn,
              rebar_subdirs,
              rebar_templater,
              rebar_upgrade,
              rebar_utils,
              rebar_xref,
              getopt,
              mustache ]},
  {registered, []},
  {applications, [kernel,
                  stdlib,
                  sasl]},
  {env, [
         %% Default log level
         {log_level, error},

         %% Default parallel jobs
         {jobs, 3},

         %% any_dir processing modules
         {any_dir_modules, [
                            rebar_require_vsn,
                            rebar_deps,
                            rebar_subdirs,
                            rebar_templater,
                            rebar_cleaner
                           ]},

         %% Dir specific processing modules
         {modules, [
                    {app_dir, [
                               rebar_pre_script,
                               rebar_abnfc_compiler,
                               rebar_protobuffs_compiler,
                               rebar_neotoma_compiler,
                               rebar_asn1_compiler,
                               rebar_erlc_compiler,
                               rebar_lfe_compiler,
                               rebar_erlydtl_compiler,
                               rebar_port_compiler,
                               rebar_otp_app,
                               rebar_ct,
                               rebar_eunit,
                               rebar_dialyzer,
                               rebar_escripter,
                               rebar_edoc,
                               rebar_xref,
                               rebar_post_script
                              ]},

                    {rel_dir, [
                               rebar_appups,
                               rebar_reltool,
                               rebar_upgrade
                              ]}
                   ]}
        ]}
]}.
