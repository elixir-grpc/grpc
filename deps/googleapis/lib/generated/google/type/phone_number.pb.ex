defmodule Google.Type.PhoneNumber.ShortCode do
  @moduledoc """
  An object representing a short code, which is a phone number that is
  typically much shorter than regular phone numbers and can be used to
  address messages in MMS and SMS systems, as well as for abbreviated dialing
  (e.g. "Text 611 to see how many minutes you have remaining on your plan.").

  Short codes are restricted to a region and are not internationally
  dialable, which means the same short code can exist in different regions,
  with different usage and pricing, even if those regions share the same
  country calling code (e.g. US and CA).
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :region_code, 1, type: :string, json_name: "regionCode"
  field :number, 2, type: :string
end

defmodule Google.Type.PhoneNumber do
  @moduledoc """
  An object representing a phone number, suitable as an API wire format.

  This representation:

   - should not be used for locale-specific formatting of a phone number, such
     as "+1 (650) 253-0000 ext. 123"

   - is not designed for efficient storage
   - may not be suitable for dialing - specialized libraries (see references)
     should be used to parse the number for that purpose

  To do something meaningful with this number, such as format it for various
  use-cases, convert it to an `i18n.phonenumbers.PhoneNumber` object first.

  For instance, in Java this would be:

     com.google.type.PhoneNumber wireProto =
         com.google.type.PhoneNumber.newBuilder().build();
     com.google.i18n.phonenumbers.Phonenumber.PhoneNumber phoneNumber =
         PhoneNumberUtil.getInstance().parse(wireProto.getE164Number(), "ZZ");
     if (!wireProto.getExtension().isEmpty()) {
       phoneNumber.setExtension(wireProto.getExtension());
     }

   Reference(s):
    - https://github.com/google/libphonenumber
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :kind, 0

  field :e164_number, 1, type: :string, json_name: "e164Number", oneof: 0
  field :short_code, 2, type: Google.Type.PhoneNumber.ShortCode, json_name: "shortCode", oneof: 0
  field :extension, 3, type: :string
end
