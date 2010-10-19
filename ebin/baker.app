{application, baker,
 [{description, "HTTP traffic router"},
  {vsn, "1"},
  {modules, [baker, router, listener]},
  {registered, [router, listener]},
  {applications, [kernel, stdlib]},
  {mod, {baker,[]}}
 ]}.