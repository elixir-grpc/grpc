{:ok, _pid} = RouteGuide.Data.start_link
GRPC.Server.start(Routeguide.RouteGuide.Server, 50051, insecure: true)

RouteGuide.Client.main()

:ok = GRPC.Server.stop(Routeguide.RouteGuide.Server)
