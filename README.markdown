
Steps to get a simple test up:

1. cd iserve
1. make test
1. erl -pa ./test
		application:start(sasl).
		application:start(iserve).
		iserve:add_server(6464, iserve_test, x).
1. Point your browser to http://localhost:6464/


1. You can more http listeners on other ports, served by other callback
   modules.
		iserve:add_server(6465, iserve_test2, x).
1. Point your browser to http://localhost:6465/


