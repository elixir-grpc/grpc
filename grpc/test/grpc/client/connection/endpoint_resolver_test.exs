defmodule GRPC.Client.Connection.EndpointResolverTest do
  use ExUnit.Case, async: true

  alias GRPC.Client.Connection.EndpointResolver

  doctest GRPC.Client.Connection.EndpointResolver

  defp cred(opts \\ [verify: :verify_none]), do: %GRPC.Credential{ssl: opts}

  describe "normalize/2 — https://" do
    test "normalises host and port to ipv4 prefix" do
      {norm_target, scheme, _cred} = EndpointResolver.normalize("https://example.com:50051", nil)

      assert norm_target == "ipv4:example.com:50051"
      assert scheme == "https"
    end

    test "injects a GRPC.Credential when none supplied" do
      {_target, _scheme, cred} = EndpointResolver.normalize("https://example.com:50051", nil)

      assert %GRPC.Credential{} = cred
    end

    test "preserves caller-supplied cred unchanged" do
      supplied = cred()

      {_target, _scheme, returned} =
        EndpointResolver.normalize("https://example.com:50051", supplied)

      assert returned == supplied
    end

    test "uses port from URL" do
      {norm_target, _scheme, _cred} = EndpointResolver.normalize("https://example.com:8443", nil)

      assert norm_target == "ipv4:example.com:8443"
    end

    test "falls back to URI default port 443 when no port in URL" do
      {norm_target, _scheme, _cred} = EndpointResolver.normalize("https://example.com", nil)

      assert norm_target == "ipv4:example.com:443"
    end
  end

  describe "normalize/2 — http://" do
    test "normalises host and port to ipv4 prefix with http scheme" do
      {norm_target, scheme, cred} = EndpointResolver.normalize("http://example.com:50051", nil)

      assert norm_target == "ipv4:example.com:50051"
      assert scheme == "http"
      assert cred == nil
    end

    test "uses URI default port 80 when no port in URL" do
      {norm_target, _scheme, _cred} = EndpointResolver.normalize("http://example.com", nil)

      assert norm_target == "ipv4:example.com:80"
    end

    test "raises ArgumentError when cred is supplied for an http:// target" do
      assert_raise ArgumentError, ~r/invalid option for insecure/, fn ->
        EndpointResolver.normalize("http://example.com:50051", cred())
      end
    end
  end

  describe "normalize/2 — schemeless host:port shorthand" do
    test "IPv4 address:port becomes ipv4 prefix with http scheme" do
      {norm_target, scheme, cred} = EndpointResolver.normalize("127.0.0.1:50051", nil)

      assert norm_target == "ipv4:127.0.0.1:50051"
      assert scheme == "http"
      assert cred == nil
    end

    test "hostname:port is treated as schemeless shorthand" do
      {norm_target, scheme, _cred} = EndpointResolver.normalize("localhost:50051", nil)

      assert norm_target == "ipv4:localhost:50051"
      assert scheme == "http"
    end

    test "supplying cred flips scheme to https" do
      {_target, scheme, returned_cred} = EndpointResolver.normalize("127.0.0.1:50051", cred())

      assert scheme == "https"
      assert returned_cred == cred()
    end

    test "bare path (no colon) becomes a unix socket and drops cred" do
      {norm_target, scheme, cred} = EndpointResolver.normalize("/tmp/my.sock", cred())

      assert norm_target == "unix:///tmp/my.sock"
      assert scheme == "unix"
      assert cred == nil
    end
  end

  describe "normalize/2 — schemeless IPv6" do
    test "bracketed IPv6 loopback [::1]:50051" do
      {norm_target, scheme, _cred} = EndpointResolver.normalize("[::1]:50051", nil)

      assert norm_target == "ipv4:::1:50051"
      assert scheme == "http"
    end

    test "bracketed full address [2001:db8::1]:8080" do
      {norm_target, _scheme, _cred} = EndpointResolver.normalize("[2001:db8::1]:8080", nil)

      assert norm_target == "ipv4:2001:db8::1:8080"
    end

    test "bracketed IPv6 without port — strips brackets, uses default port" do
      {norm_target, _scheme, _cred} = EndpointResolver.normalize("[::1]", nil)

      assert norm_target == "ipv4:::1"
    end

    test "bare IPv6 loopback ::1:50051 (last segment is port)" do
      {norm_target, scheme, _cred} = EndpointResolver.normalize("::1:50051", nil)

      assert norm_target == "ipv4:::1:50051"
      assert scheme == "http"
    end

    test "bare full address 2001:db8::1:8080" do
      {norm_target, _scheme, _cred} = EndpointResolver.normalize("2001:db8::1:8080", nil)

      assert norm_target == "ipv4:2001:db8::1:8080"
    end

    test "supplying cred flips scheme to https for bracketed IPv6" do
      {_target, scheme, _cred} = EndpointResolver.normalize("[::1]:50051", cred())

      assert scheme == "https"
    end
  end

  describe "normalize/2 — passthrough resolver schemes" do
    test "dns:// target is passed through unchanged" do
      {norm_target, scheme, cred} =
        EndpointResolver.normalize("dns://my-service.local:50051", nil)

      assert norm_target == "dns://my-service.local:50051"
      assert scheme == "http"
      assert cred == nil
    end

    test "ipv4: target is passed through unchanged" do
      {norm_target, scheme, cred} = EndpointResolver.normalize("ipv4:10.0.0.1:50051", nil)

      assert norm_target == "ipv4:10.0.0.1:50051"
      assert scheme == "http"
      assert cred == nil
    end

    test "ipv6: target is passed through unchanged" do
      {norm_target, scheme, cred} = EndpointResolver.normalize("ipv6:[::1]:50051", nil)

      assert norm_target == "ipv6:[::1]:50051"
      assert scheme == "http"
      assert cred == nil
    end

    test "unix:// target is passed through unchanged" do
      {norm_target, scheme, cred} = EndpointResolver.normalize("unix:///tmp/my.sock", nil)

      assert norm_target == "unix:///tmp/my.sock"
      assert scheme == "http"
      assert cred == nil
    end

    test "xds:// target is passed through unchanged" do
      {norm_target, scheme, cred} = EndpointResolver.normalize("xds:///my-service", nil)

      assert norm_target == "xds:///my-service"
      assert scheme == "http"
      assert cred == nil
    end

    test "supplying cred to a passthrough target returns https and the cred" do
      supplied = cred()

      {norm_target, scheme, returned_cred} =
        EndpointResolver.normalize("ipv4:10.0.0.1:50051", supplied)

      assert norm_target == "ipv4:10.0.0.1:50051"
      assert scheme == "https"
      assert returned_cred == supplied
    end
  end

  describe "split_host_port/1 — standard host:port" do
    test "IPv4 address:port" do
      assert EndpointResolver.split_host_port("127.0.0.1:50051") == {"127.0.0.1", 50051}
    end

    test "hostname:port" do
      assert EndpointResolver.split_host_port("localhost:8080") == {"localhost", 8080}
    end

    test "bare host with no port uses default 50051" do
      assert EndpointResolver.split_host_port("myhost") == {"myhost", 50051}
    end
  end

  describe "split_host_port/1 — scheme-prefixed targets" do
    test "ipv4:host:port strips the scheme prefix" do
      assert EndpointResolver.split_host_port("ipv4:10.0.0.1:50051") == {"10.0.0.1", 50051}
    end

    test "ipv4:host with no port uses default 50051" do
      assert EndpointResolver.split_host_port("ipv4:10.0.0.1") == {"10.0.0.1", 50051}
    end
  end

  describe "split_host_port/1 — bracketed IPv6" do
    test "[::1]:50051" do
      assert EndpointResolver.split_host_port("[::1]:50051") == {"::1", 50051}
    end

    test "[2001:db8::1]:8080" do
      assert EndpointResolver.split_host_port("[2001:db8::1]:8080") == {"2001:db8::1", 8080}
    end

    test "ipv6:[::1]:50051 — scheme with bracketed IPv6" do
      assert EndpointResolver.split_host_port("ipv6:[::1]:50051") == {"::1", 50051}
    end

    test "[::1] with no port uses default 50051" do
      assert EndpointResolver.split_host_port("[::1]") == {"::1", 50051}
    end
  end

  describe "split_host_port/1 — bare IPv6" do
    test "::1:50051 — loopback with port" do
      assert EndpointResolver.split_host_port("::1:50051") == {"::1", 50051}
    end

    test "2001:db8::1:8080 — full address with port" do
      assert EndpointResolver.split_host_port("2001:db8::1:8080") == {"2001:db8::1", 8080}
    end

    test "::ffff:192.0.2.1:50051 — IPv4-mapped IPv6 with port" do
      assert EndpointResolver.split_host_port("::ffff:192.0.2.1:50051") ==
               {"::ffff:192.0.2.1", 50051}
    end
  end
end
