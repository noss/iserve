{application, iserve,
        [{description, "Web Server"},
         {vsn, "%ISERVE_VSN%"},
         {modules, [    iserve_sup,
			iserve_app,
			iserve_server,
                        iserve_socket
			]},

         {registered, [	iserve_sup]},
         {applications, [kernel, stdlib, sasl]},
	 {mod, {iserve_app, []}}]}.
