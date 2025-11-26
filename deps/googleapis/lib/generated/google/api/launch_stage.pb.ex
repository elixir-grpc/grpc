defmodule Google.Api.LaunchStage do
  @moduledoc """
  The launch stage as defined by [Google Cloud Platform
  Launch Stages](https://cloud.google.com/terms/launch-stages).
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :LAUNCH_STAGE_UNSPECIFIED, 0
  field :UNIMPLEMENTED, 6
  field :PRELAUNCH, 7
  field :EARLY_ACCESS, 1
  field :ALPHA, 2
  field :BETA, 3
  field :GA, 4
  field :DEPRECATED, 5
end
