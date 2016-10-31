features = Routeguide.RouteGuide.Server.load_features
GRPC.Server.start(Routeguide.RouteGuide.Server, "localhost:50051", insecure: true, state: features)

RouteGuide.Client.main()

:ok = GRPC.Server.stop(Routeguide.RouteGuide.Server)
