defmodule FooRecord do
  require Record
  Record.defrecord(:user, name: "meg", age: "25")
end
