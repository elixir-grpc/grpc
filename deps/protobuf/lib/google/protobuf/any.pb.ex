defmodule Google.Protobuf.Any do
  @moduledoc """
  `Any` contains an arbitrary serialized protocol buffer message along with a
  URL that describes the type of the serialized message.

  Protobuf library provides support to pack/unpack Any values in the form
  of utility functions or additional generated methods of the Any type.

  Example 1: Pack and unpack a message in C++.

      Foo foo = ...;
      Any any;
      any.PackFrom(foo);
      ...
      if (any.UnpackTo(&foo)) {
        ...
      }

  Example 2: Pack and unpack a message in Java.

      Foo foo = ...;
      Any any = Any.pack(foo);
      ...
      if (any.is(Foo.class)) {
        foo = any.unpack(Foo.class);
      }
      // or ...
      if (any.isSameTypeAs(Foo.getDefaultInstance())) {
        foo = any.unpack(Foo.getDefaultInstance());
      }

   Example 3: Pack and unpack a message in Python.

      foo = Foo(...)
      any = Any()
      any.Pack(foo)
      ...
      if any.Is(Foo.DESCRIPTOR):
        any.Unpack(foo)
        ...

   Example 4: Pack and unpack a message in Go

       foo := &pb.Foo{...}
       any, err := anypb.New(foo)
       if err != nil {
         ...
       }
       ...
       foo := &pb.Foo{}
       if err := any.UnmarshalTo(foo); err != nil {
         ...
       }

  The pack methods provided by protobuf library will by default use
  'type.googleapis.com/full.type.name' as the type URL and the unpack
  methods only use the fully qualified type name after the last '/'
  in the type URL, for example "foo.bar.com/x/y.z" will yield type
  name "y.z".

  JSON
  ====
  The JSON representation of an `Any` value uses the regular
  representation of the deserialized, embedded message, with an
  additional field `@type` which contains the type URL. Example:

      package google.profile;
      message Person {
        string first_name = 1;
        string last_name = 2;
      }

      {
        "@type": "type.googleapis.com/google.profile.Person",
        "firstName": <string>,
        "lastName": <string>
      }

  If the embedded message type is well-known and has a custom JSON
  representation, that representation will be embedded adding a field
  `value` which holds the custom JSON in addition to the `@type`
  field. Example (for message [google.protobuf.Duration][]):

      {
        "@type": "type.googleapis.com/google.protobuf.Duration",
        "value": "1.212s"
      }
  """

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "Any",
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          name: "type_url",
          extendee: nil,
          number: 1,
          label: :LABEL_OPTIONAL,
          type: :TYPE_STRING,
          type_name: nil,
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "typeUrl",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "value",
          extendee: nil,
          number: 2,
          label: :LABEL_OPTIONAL,
          type: :TYPE_BYTES,
          type_name: nil,
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "value",
          proto3_optional: nil,
          __unknown_fields__: []
        }
      ],
      nested_type: [],
      enum_type: [],
      extension_range: [],
      extension: [],
      options: nil,
      oneof_decl: [],
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end

  field :type_url, 1, type: :string, json_name: "typeUrl"
  field :value, 2, type: :bytes
end
