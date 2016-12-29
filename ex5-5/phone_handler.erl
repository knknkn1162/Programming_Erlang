-module(phone_handler).
-compile(export_all).

init(_) ->
  phone_handler:start(),
  phone_handler:gen().

terminate(Pid) ->
  phone_handler:terminate(),
  exit(Pid).
  
  
  
