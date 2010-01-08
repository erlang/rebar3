{application, {{appid}},
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             {{appid}}_app,
             {{appid}}_sup
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]}.
