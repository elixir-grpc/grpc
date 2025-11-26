# HPAX

![CI](https://github.com/elixir-mint/hpax/actions/workflows/main.yml/badge.svg)
[![Docs](https://img.shields.io/badge/api-docs-green.svg?style=flat)](https://hexdocs.pm/hpax)
[![Hex.pm Version](http://img.shields.io/hexpm/v/hpax.svg?style=flat)](https://hex.pm/packages/hpax)
[![Coverage Status](https://coveralls.io/repos/github/elixir-mint/hpax/badge.svg?branch=main)](https://coveralls.io/github/elixir-mint/hpax?branch=main)

HPAX is an Elixir implementation of the HPACK header compression algorithm as used in HTTP/2 and
defined in RFC 7541. HPAX is used by several Elixir projects, including the
[Mint](https://github.com/elixir-mint/mint) HTTP client and
[bandit](https://github.com/mtrudel/bandit) HTTP server projects.

## Installation

To install HPAX, add it to your `mix.exs` file.

```elixir
defp deps do
  [
    {:hpax, "~> 0.1.0"}
  ]
end
```

Then, run `$ mix deps.get`.

## Usage

HPAX is designed to be used in both encoding and decoding scenarios. In both cases, a context is
used to maintain state internal to the HPACK algorithm. In the common use case of using HPAX
within HTTP/2, this context is called a **table** and must be shared between any
subsequent encoding/decoding calls within
an endpoint. Note that the contexts used for encoding and decoding within HTTP/2 are completely
distinct from one another, even though they are structurally identical.

To encode a set of headers into a binary with HPAX:

```elixir
context = HPAX.new(4096)
headers = [{:store, ":status", "201"}, {:store, "location", "http://example.com"}]
{encoded_headers, context} = HPAX.encode(headers, context)
#=> {iodata, updated_context}
```

To decode a binary into a set of headers with HPAX:

```elixir
context = HPAX.new(4096)
encoded_headers = <<...>>
{:ok, headers, context} = HPAX.decode(encoded_headers, context)
#=> {:ok, [{:store, ":status", "201"}, {:store, "location", "http://example.com"}], updated_context}
```

For complete usage information, please see the HPAX [documentation](https://hex.pm/packages/hpax).

## Contributing

If you wish to contribute check out the [issue list](https://github.com/elixir-mint/hpax/issues) and let us know what you want to work on so we can discuss it and reduce duplicate work.

## License

Copyright 2021 Eric Meadows-Jönsson and Andrea Leopardi

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
