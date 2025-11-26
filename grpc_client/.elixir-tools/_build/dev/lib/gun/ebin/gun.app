{application, 'gun', [
	{description, "HTTP/1.1, HTTP/2 and Websocket client for Erlang/OTP."},
	{vsn, "2.2.0"},
	{modules, ['gun','gun_app','gun_conns_sup','gun_content_handler','gun_cookies','gun_cookies_list','gun_data_h','gun_default_event_h','gun_event','gun_http','gun_http2','gun_http3','gun_pool','gun_pool_events_h','gun_pools_sup','gun_protocols','gun_public_suffix','gun_quicer','gun_raw','gun_socks','gun_sse_h','gun_sup','gun_tcp','gun_tcp_proxy','gun_tls','gun_tls_proxy','gun_tls_proxy_cb','gun_tls_proxy_http2_connect','gun_tunnel','gun_ws','gun_ws_h','gun_ws_protocol']},
	{registered, [gun_sup]},
	{applications, [kernel,stdlib,public_key,ssl,cowlib]},
	{optional_applications, []},
	{mod, {'gun_app', []}},
	{env, []}
]}.