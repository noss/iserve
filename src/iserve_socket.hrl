-ifndef(_ISERVE_SOCKET_).
-define(_ISERVE_SOCKET_, true).

-record(c,  {sock,
             port,
             peer_addr,
             peer_port,
             cb_mod,      % callback module M:iserve_request(#req{})
             cb_data      % callback module M:iserve_init_data()
	     }).

-endif.
