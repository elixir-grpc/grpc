defmodule GRPC.Channel do
  @moduledoc """
  A struct to store the connection data, which should be passed to
  RPC functions as the first argument:

      Greeter.Stub.say_hello(channel, request)

  ## Fields

    * `:host` - server's host to connect
    * `:port` - server's port to connect
    * `:scheme` - scheme of connection, like `http`
    * `:cred` - credentials used for authentication
    * `:adapter` - a client adapter module, like `GRPC.Adapter.Gun`
    * `:codec` - a default codec for this channel
    * `:adapter_payload` - payload the adapter uses
  """

  @type t :: %__MODULE__{
          host: String.t(),
          port: non_neg_integer,
          scheme: String.t(),
          cred: GRPC.Credential.t(),
          adapter: atom,
          adapter_payload: any,
          codec: module,
          interceptors: [],
          compressor: module,
          accepted_compressors: [module],
          headers: list
        }
  defstruct host: nil,
            port: nil,
            scheme: nil,
            cred: nil,
            adapter: nil,
            adapter_payload: nil,
            codec: GRPC.Codec.Proto,
            interceptors: [],
            compressor: nil,
            accepted_compressors: [],
            headers: []
end
