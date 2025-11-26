defmodule Google.Api.Visibility do
  @moduledoc """
  `Visibility` restricts service consumer's access to service elements,
  such as whether an application can call a visibility-restricted method.
  The restriction is expressed by applying visibility labels on service
  elements. The visibility labels are elsewhere linked to service consumers.

  A service can define multiple visibility labels, but a service consumer
  should be granted at most one visibility label. Multiple visibility
  labels for a single service consumer are not supported.

  If an element and all its parents have no visibility label, its visibility
  is unconditionally granted.

  Example:

      visibility:
        rules:
        - selector: google.calendar.Calendar.EnhancedSearch
          restriction: PREVIEW
        - selector: google.calendar.Calendar.Delegate
          restriction: INTERNAL

  Here, all methods are publicly visible except for the restricted methods
  EnhancedSearch and Delegate.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :rules, 1, repeated: true, type: Google.Api.VisibilityRule
end

defmodule Google.Api.VisibilityRule do
  @moduledoc """
  A visibility rule provides visibility configuration for an individual API
  element.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :selector, 1, type: :string
  field :restriction, 2, type: :string
end
