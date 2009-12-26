{application, rebar,
 [{description, "Rebar: Erlang Build Tool"},
  {vsn, "2"},
  {modules, [ rebar,
              rebar_app_utils,
              rebar_config,              
              rebar_core,
              rebar_ct,
              rebar_deps,
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
              rebar_port_compiler,
              rebar_rel_utils,
              rebar_reltool,
              rebar_subdirs,
              rebar_utils ]},
  {registered, []},
  {applications, [kernel, 
                  stdlib, 
                  sasl]},
  {env, [
         %% Default log level
         {log_level, error},

         %% any_dir processing modules
         {any_dir_modules, [
                            rebar_subdirs,
                            rebar_deps
                           ]},

         %% Dir specific processing modules
         {modules, [
                    {app_dir, [
                               rebar_protobuffs_compiler,
                               rebar_erlc_compiler,
                               rebar_lfe_compiler,
                               rebar_erlydtl_compiler,
                               rebar_port_compiler,
                               rebar_otp_app,
                               rebar_ct,
                               rebar_eunit,
                               rebar_escripter
                              ]},
                    
                    {rel_dir, [
                               rebar_reltool
                              ]}
                   ]}
        ]}
]}.
