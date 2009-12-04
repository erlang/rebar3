{application, rebar,
 [{description, "Rebar: Erlang Build Tool"},
  {vsn, "1"},
  {modules, [ rebar,
              rebar_app_utils,
              rebar_config,
              rebar_core,
	      rebar_ct,
              rebar_erlc_compiler,
              rebar_file_utils,
              rebar_log,
              rebar_otp_app,
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
         
         %% Key/value list of base/default configuration used by
         %% rebar_config during initialization
         {default_config, [
                           {app_modules, [ rebar_protobuffs_compiler,
                                           rebar_erlc_compiler,
                                           rebar_port_compiler,
                                           rebar_otp_app,
					   rebar_ct ]},

                           {rel_modules, [ rebar_reltool ]}
                           ]}
        ]}
]}.
