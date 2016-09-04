# gRPC Elixir

A Elixir implementation of [gRPC](https://github.com/grpc/grpc). It's implemented
by [Erlang NIF](http://erlang.org/doc/tutorial/nif.html) written on top of [gRPC c code](https://github.com/tony612/grpc-core).

**WARNING: This is unstable. Don't use in production!**

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed as:

  1. Add `grpc` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:grpc, "~> 0.1.0"}]
    end
    ```

  2. Ensure `grpc` is started before your application:

    ```elixir
    def application do
      [applications: [:grpc]]
    end
    ```
