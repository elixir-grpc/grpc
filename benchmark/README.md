# gRPC Benchmark

Benchmark tools for testing gRPC performance.

## Usage

This project provides Mix tasks for running benchmarks:

### Starting a Worker Server

```bash
mix benchmark.worker --port=10000
```

Options:
- `--port` - Port to listen on (required)

The worker will run until you send it a quit signal.

### Running Benchmark Tests

```bash
# Use defaults (port 10000, 1000 requests)
mix benchmark.test

# Custom port
mix benchmark.test --port=9999

# Custom number of requests
mix benchmark.test --requests=5000

# Both options
mix benchmark.test --port=9999 --requests=5000
```

Options:
- `--port` - Server port (default: 10000)
- `--requests` - Number of requests to send (default: 1000)

### Legacy Scripts

The original `bin/` scripts are still available for backward compatibility:

```bash
# Start worker
elixir bin/worker.exs --port=10000

# Run test
elixir bin/test.exs
```

## Project Structure

This project follows standard Elixir/OTP conventions:

```
benchmarkg/
├── lib/
│   ├── benchmark/           # Core benchmark modules
│   │   ├── application.ex   # OTP Application supervisor
│   │   ├── client_manager.ex
│   │   ├── server_manager.ex
│   │   └── ...
│   └── mix/tasks/           # Mix tasks
│       ├── benchmark.worker.ex
│       └── benchmark.test.ex
├── bin/                     # Legacy scripts (preserved)
├── proto/                   # Protocol buffer definitions
└── mix.exs
```

See [REFACTORING.md](REFACTORING.md) for details on the migration from scripts to Mix tasks.

