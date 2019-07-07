# Benchmark

Benchmark implementation followed by [official spec](https://grpc.io/docs/guides/benchmarking/)

## Usage

```
$ git clone https://github.com/tony612/grpc.git
$ cd grpc && git checkout -t origin/elixir-bench
$ export ELIXIR_GRPC_PATH=$(dirname $(pwd))
$ python tools/run_tests/run_performance_tests.py -l elixir
```
