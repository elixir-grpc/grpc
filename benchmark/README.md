# Benchmark

Benchmark implementation followed by [official spec](https://grpc.io/docs/guides/benchmarking/)

## Usage

```
$ git clone https://github.com/elixir-grpc/grpc.git
$ cd grpc && git checkout -t origin/elixir-bench
$ git submodule update --init
$ export ELIXIR_GRPC_PATH= # elixir-grpc path
$ python tools/run_tests/run_performance_tests.py -l elixir
```

## Result

```
# Elixir
# Branch improve-perf
I0707 13:04:20.222163000 4618786240 report.cc:82]                      QPS: 3070.4
I0707 13:04:20.224064000 4618786240 report.cc:122]                     QPS: 3070.4 (255.9/server core)
I0707 13:04:20.224079000 4618786240 report.cc:127]                     Latencies (50/90/95/99/99.9%-ile): 305.0/406.1/469.6/733.8/1730.8 us
I0707 13:04:20.224088000 4618786240 report.cc:137]                     Server system time: 4.72%
I0707 13:04:20.224097000 4618786240 report.cc:139]                     Server user time:   47.32%
I0707 13:04:20.224104000 4618786240 report.cc:141]                     Client system time: 7.93%
I0707 13:04:20.224112000 4618786240 report.cc:143]                     Client user time:   94.19%
I0707 13:04:20.224119000 4618786240 report.cc:148]                     Server CPU usage: 0.00%
I0707 13:04:20.224126000 4618786240 report.cc:153]                     Client Polls per Request: 0.00
I0707 13:04:20.224134000 4618786240 report.cc:155]                     Server Polls per Request: 0.00
I0707 13:04:20.224142000 4618786240 report.cc:160]                     Server Queries/CPU-sec: 5899.79
I0707 13:04:20.224149000 4618786240 report.cc:162]                     Client Queries/CPU-sec: 3006.81

# Go
# It seems there are some problems with Go's benchmark because there's no CPU related metrics
I0707 12:36:06.782124000 4488484288 report.cc:82]                      QPS: 9525.8
I0707 12:36:06.783670000 4488484288 report.cc:122]                     QPS: 9525.8 (793.8/server core)
I0707 12:36:06.783688000 4488484288 report.cc:127]                     Latencies (50/90/95/99/99.9%-ile): 93.8/118.7/139.0/247.9/526.0 us
I0707 12:36:06.783698000 4488484288 report.cc:137]                     Server system time: 0.00%
I0707 12:36:06.783707000 4488484288 report.cc:139]                     Server user time:   0.00%
I0707 12:36:06.783750000 4488484288 report.cc:141]                     Client system time: 0.00%
I0707 12:36:06.783758000 4488484288 report.cc:143]                     Client user time:   0.00%
I0707 12:36:06.783764000 4488484288 report.cc:148]                     Server CPU usage: 0.00%
I0707 12:36:06.783771000 4488484288 report.cc:153]                     Client Polls per Request: 0.00
I0707 12:36:06.783777000 4488484288 report.cc:155]                     Server Polls per Request: 0.00
I0707 12:36:06.783783000 4488484288 report.cc:160]                     Server Queries/CPU-sec: inf
I0707 12:36:06.783790000 4488484288 report.cc:162]                     Client Queries/CPU-sec: inf
```
