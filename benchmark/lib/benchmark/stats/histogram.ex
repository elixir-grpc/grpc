# HistogramOptions contains the parameters that define the histogram's buckets.
# The first bucket of the created histogram (with index 0) contains [min, min+n)
# where n = base_bucket_size, min = min_value.
# Bucket i (i>=1) contains [min + n * m^(i-1), min + n * m^i), where m = 1+GrowthFactor.
defmodule Benchmark.Stats.HistogramOpts do
  defstruct num_buckets: 32, growth_factor: 0.0, base_bucket_size: 1.0, min_value: 0
end

defmodule Benchmark.Stats.HistogramBucket do
  defstruct low_bound: 0, count: 0
end

defmodule Benchmark.Stats.Histogram do
  defstruct buckets: [],
            min: nil,
            max: 0,
            opts: nil,
            count: 0,
            sum: 0,
            sum_of_squares: 0,
            cached_base_bucket_size: nil,
            cached_growth_factor: nil

  alias Benchmark.Stats.HistogramBucket

  def new(opts) do
    m = 1.0 + opts.growth_factor
    buckets = [%HistogramBucket{low_bound: opts.min_value}]

    {buckets, _} =
      Enum.reduce(1..(opts.num_buckets - 1), {buckets, opts.base_bucket_size}, fn _,
                                                                                  {bs, delta} ->
        b = %HistogramBucket{low_bound: opts.min_value + delta}
        {[b | bs], delta * m}
      end)

    buckets = Enum.reverse(buckets)

    %__MODULE__{
      buckets: buckets,
      opts: opts,
      cached_base_bucket_size: :math.log(opts.base_bucket_size),
      cached_growth_factor: 1 / :math.log(1 + opts.growth_factor)
    }
  end

  def add(h, val) do
    bucket_idx = find_bucket(h, val)

    if bucket_idx >= h.opts.num_buckets do
      raise ArgumentError, "found bucket for #{val} is out of range: #{bucket_idx}"
    end

    buckets = List.update_at(h.buckets, bucket_idx, fn b -> Map.update!(b, :count, &(&1 + 1)) end)

    h
    |> Map.put(:buckets, buckets)
    |> Map.update!(:count, &(&1 + 1))
    |> Map.update!(:sum, &(&1 + val))
    |> Map.update!(:sum_of_squares, &(&1 + val * val))
    |> Map.update(:min, val, &min(&1, val))
    |> Map.update(:max, val, &max(&1, val))
  end

  def find_bucket(h, val) do
    delta = val - h.opts.min_value

    if delta >= h.opts.base_bucket_size do
      trunc((:math.log(delta) - h.cached_base_bucket_size) * h.cached_growth_factor + 1)
    else
      0
    end
  end

  def merge(h1, h2) do
    if h1.opts != h2.opts do
      raise ArgumentError, "failed to merge histograms, created by inequivalent options"
    end

    min = min(h1.min, h2.min)
    max = max(h1.max, h2.max)

    buckets =
      Enum.zip(h1.buckets, h2.buckets)
      |> Enum.map(fn {b1, b2} -> %{b1 | count: b1.count + b2.count} end)

    Map.merge(h1, %{
      count: h1.count + h2.count,
      sum: h1.sum + h2.sum,
      sum_of_squares: h1.sum_of_squares + h2.sum_of_squares,
      buckets: buckets,
      min: min,
      max: max
    })
  end
end
