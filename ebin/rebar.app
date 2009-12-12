{application, rebar,
 [{description, "Rebar: Erlang Build Tool"},
  {vsn, "1"},
  {modules, [ rebar,
              rebar_app_utils,
              rebar_config,
              rebar_core,
              rebar_ct,
              rebar_erlc_compiler,
              rebar_eunit,
              rebar_file_utils,
              rebar_log,
              rebar_otp_app,
              rebar_port_compiler,
              rebar_protobuffs_compiler,
              rebar_port_compiler,
              rebar_rel_utils,
              rebar_reltool,
              rebar_utils ]},
  {registered, []},
  {applications, [kernel, 
                  stdlib, 
                  sasl]},
  {env, [
         %% Default log level
         {log_level, error},

         %% Processing modules
         {modules, [
                    {app_dir, [ rebar_protobuffs_compiler,
                                rebar_erlc_compiler,
                                rebar_port_compiler,
                                rebar_otp_app,
                                rebar_ct,
                                rebar_eunit]},
                    {rel_dir, [ rebar_reltool ]}
                   ]}
        ]}
]}.
