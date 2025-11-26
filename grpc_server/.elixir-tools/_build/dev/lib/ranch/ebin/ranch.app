{application, 'ranch', [
	{description, "Socket acceptor pool for TCP protocols."},
	{vsn, "2.2.0"},
	{modules, ['ranch','ranch_acceptor','ranch_acceptors_sup','ranch_app','ranch_conns_sup','ranch_conns_sup_sup','ranch_crc32c','ranch_embedded_sup','ranch_listener_sup','ranch_protocol','ranch_proxy_header','ranch_server','ranch_server_proxy','ranch_ssl','ranch_sup','ranch_tcp','ranch_transport']},
	{registered, [ranch_sup,ranch_server]},
	{applications, [kernel,stdlib,ssl]},
	{optional_applications, []},
	{mod, {ranch_app, []}},
	{env, []}
]}.