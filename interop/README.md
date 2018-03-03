# Interoperability Test

doc: https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md

## Run

```
# server side
$ mix grpc.server

# client side. "Succeed!" is printed at last when succeed.
$ mix run script/client.exs
```
