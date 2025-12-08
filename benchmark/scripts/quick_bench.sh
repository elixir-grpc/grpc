#!/usr/bin/env bash
set -e

echo "========================================"
echo "Quick Performance Benchmark"
echo "========================================"
echo ""

cd "$(dirname "$0")/.."

echo "Running optimized benchmark..."
echo ""

MIX_ENV=prod mix benchmark.test --codec=proto --requests=30000 2>&1 | tee /tmp/bench_output.txt

echo ""
echo "========================================"
echo "Results Summary:"
echo "========================================"
grep -E "req/s|Requests|requests/sec" /tmp/bench_output.txt || echo "Check full output above"
