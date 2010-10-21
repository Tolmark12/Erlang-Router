{application, baker,
 [{description, "HTTP traffic router"},
  {vsn, "1"},
  {modules, [baker, router, listener, http_handler, util, web_service]},
  {registered, [router, listener, http_handler, util, web_service]},
  {applications, [kernel, stdlib]},
  {mod, {baker,[]}}
 ]}.