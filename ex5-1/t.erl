-module(t).
-compile(export_all).

gen() -> 
  my_db:write("apple", 150),
  my_db:write("orange", 80),
  my_db:write("cherry", 150).

u() -> unregister(db).
