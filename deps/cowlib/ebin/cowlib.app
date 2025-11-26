{application, 'cowlib', [
	{description, "Support library for manipulating Web protocols."},
	{vsn, "2.16.0"},
	{modules, ['cow_base64url','cow_capsule','cow_cookie','cow_date','cow_deflate','cow_hpack','cow_http','cow_http1','cow_http2','cow_http2_machine','cow_http3','cow_http3_machine','cow_http_hd','cow_http_struct_hd','cow_http_te','cow_iolists','cow_link','cow_mimetypes','cow_multipart','cow_qpack','cow_qs','cow_spdy','cow_sse','cow_uri','cow_uri_template','cow_ws']},
	{registered, []},
	{applications, [kernel,stdlib,crypto]},
	{optional_applications, []},
	{env, []}
]}.