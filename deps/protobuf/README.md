# protobuf-elixir

[![CI](https://github.com/elixir-protobuf/protobuf/actions/workflows/main.yml/badge.svg)](https://github.com/elixir-protobuf/protobuf/actions/workflows/main.yml)
[![Coverage Status](https://coveralls.io/repos/github/elixir-protobuf/protobuf/badge.svg?branch=main)](https://coveralls.io/github/elixir-protobuf/protobuf?branch=main)

A pure Elixir implementation of [Google Protobuf](https://developers.google.com/protocol-buffers/).

## Why this instead of exprotobuf(gpb)?

It has some must-have and other cool features like:

1. A protoc [plugin](https://developers.google.com/protocol-buffers/docs/cpptutorial#compiling-your-protocol-buffers) to generate Elixir code just like what other official libs do, which is powerful and reliable.
2. Generate **simple and explicit** code with the power of Macro. See [test/support/test_msg.ex](https://github.com/tony612/protobuf-elixir/blob/master/test/support/test_msg.ex).
3. Plugins support. Only [grpc](https://github.com/tony612/grpc-elixir) is supported now.
4. Use **structs** for messages instead of Erlang records.
5. Support Typespec in generated code.

## Installation

The package can be installed by adding `:protobuf` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:protobuf, "~> 0.14.1"}
  ]
end
```

### Google Protos

Since `:protobuf` version `0.14.0` we include all of the well known Google Protobuf modules. This conflicts with the deprecated `:google_protos` package. Please remove the `:google_protos` package from your dependencies and run `mix deps.unlock --unused`.

## Features

* Define messages with DSL
* Decode basic messages
* Skip unknown fields
* Decode embedded messages
* Decode packed and repeated fields
* Encode messages
* protoc plugin
* map
* Support default values
* Validate values
* Generate typespecs
* oneof
* (proto2) Extension (Experiment, see `Protobuf.Extension`)

## Usage

### Generate Elixir code

1. [Download](https://github.com/protocolbuffers/protobuf#protocol-compiler-installation) and
   install the protocol buffer compiler (`protoc`). MacOS users can also install it through
   Homebrew with `brew install protobuf`.

2. Install `protoc` plugin `protoc-gen-elixir` for Elixir using the command below. Make sure the
   `protoc-gen-elixir` binary is in your `PATH`. Either add `PATH=~/.mix/escripts:$PATH` to your
   bash or zsh profile or, if you used asdf to install elixir, run `asdf reshim` and then verify
   that `protoc-gen-elixir` works:

    ```bash
    $ mix escript.install hex protobuf 0.14.0
    ```
    Note: make sure to use the same version of the escript and dependency in your application.

3. Generate Elixir code for [helloworld.proto](https://raw.githubusercontent.com/grpc/grpc/master/examples/protos/helloworld.proto) using `protoc`:

    ```bash
    $ protoc --elixir_out=./lib helloworld.proto
    ```

4. A `lib/helloworld.pb.ex` file will be generated, like:

    ```elixir
    defmodule Helloworld.HelloRequest do
      @moduledoc false
      use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

      field :name, 1, type: :string
    end

    defmodule Helloworld.HelloReply do
      @moduledoc false
      use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

      field :message, 1, type: :string
    end
    ```

### Encode and decode in your code

```elixir
struct = %Foo{a: 3.2, c: %Foo.Bar{}}
encoded = Foo.encode(struct)
struct = Foo.decode(encoded)
```

Validation is done during encoding. An error will be raised if the struct is invalid: when it
misses a required field or has a mistyped value.

### Descriptor support

If you use any custom options in your protobufs then to gain access to them you'll need to include
the raw descriptors in the generated modules. You can generate the descriptors by passing
`gen_descriptors=true` in `--elixir_out`.

The descriptors will be available on each module from the `descriptor/0` function.

```
$ protoc --elixir_out=gen_descriptors=true:./lib/ *.proto
$ protoc --elixir_out=gen_descriptors=true,plugins=grpc:./lib/ *.proto
```

### Package prefix

You can use the `package_prefix` option to prefix generated Elixir code.

For example to prefix generated Elixir modules with `MyApp.Protos` use `my_app.protos` as package
prefix:

```
$ protoc --elixir_out=./lib --elixir_opt=package_prefix=my_app.protos *.proto
```

### Transformer module

By defining a callback `transform_module/0` function on your protobuf message module
you can add custom encoding and decoding logic for your message. See the documentation
for `Protobuf.TransformModule` for more details.

If your protobufs are generated from a `.proto` files you can add the callback function
by passing `transform_module=...` in `--elixir_out`.

```
$ protoc --elixir_out=transform_module=MyTransformModule:./lib/ *.proto
```

### One file per module

You can use the `one_file_per_module=true` option to change the way that files
are generated into directories. By default, one `.pb.ex` file is generated for
each `.proto` file you compile and each of those `.pb.ex` files can have
multiple Elixir module definitions in it.

With `one_file_per_module=true`, one `.pb.ex` file will be generated for each
generated Elixir module and the directory structure will respect Elixir
conventions. For example, a `MyPackage.MyMessage` message will end up in the
`my_package/my_message.pb.ex` file.

```
$ protoc --elixir_out=one_file_per_module=true:./lib *.proto
```

### Include documentation

You can use the `include_docs=true` option to set the visibility of the
generated modules documentation.

With `include_docs=true`, the generated modules will not have the
`@module false` attribute.

```
$ protoc --elixir_out=./lib --elixir_opt=include_docs=true *.proto
```

### gRPC Support

If you write [services](https://developers.google.com/protocol-buffers/docs/proto#services) in
protobuf, you can generate [gRPC](https://github.com/elixir-grpc/grpc) code by passing
`plugins=grpc` in `--elixir_out`:

```
$ protoc --elixir_out=plugins=grpc:./lib/ *.proto
```

### Tips for protoc

Custom protoc-gen-elixir name or path using `--plugin`:

```bash
$ protoc --elixir_out=./lib --plugin=./protoc-gen-elixir *.proto
```

Pass `-I` argument if you import other protobuf files:

```bash
$ protoc -I protos --elixir_out=./lib protos/hello.proto
```

### Custom options

Since extensions(`Protobuf.Extension`) is supported now, some options are defined, like custom
`module_prefix`.

1. Copy `src/elixirpb.proto` to your protos path.

2. Import `elixirpb.proto` and use the options.

    ```proto
    syntax = "proto2";

    package your.pkg;

    import "elixirpb.proto";

    option (elixirpb.file).module_prefix = "Foo.Bar";
    ```

3. Generate code as before.

More options will be added in the future, see `elixirpb.proto` comments for details.

## Tests

```bash
mix test
```

## Sponsors

* [Tubi](https://tubitv.com/)

<img src="https://user-images.githubusercontent.com/1253659/37473536-4db44048-28a9-11e8-90d5-f8a2f5a8d53c.jpg" height="80">

* [Community](https://www.community.com)

<img src="https://user-images.githubusercontent.com/1253659/84641850-3f163d80-af2e-11ea-98a2-cfb854180222.png" height="80">

## Acknowledgements

Many thanks to [gpb](https://github.com/tomas-abrahamsson/gpb) and
[golang/protobuf](https://github.com/golang/protobuf) as good examples of
writing Protobuf decoder/encoder.
