{application, rebar,
 [{description, "Rebar: Erlang Build Tool"},
  {vsn, "1"},
  {modules, [ rebar_config,
              rebar_utils,
              rebar_app_utils,
              rebar_rel_utils,
              rebar_erlc_compiler]},
  {registered, []},
  {applications, [kernel, 
                  stdlib, 
                  sasl]},
  {env, [
         %% Key/value list of base/default configuration used by
         %% rebar_config during initialization
         {default_config, [
                           {app_modules, [ rebar_erlc_compiler ]}
                           ]}
        ]}
]}.
