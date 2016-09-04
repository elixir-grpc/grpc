# NOTE: @integration needs greet_server, e.g. `ruby examples/ruby/greeter_server.rb`
ExUnit.configure exclude: [integration: true]
ExUnit.start()
