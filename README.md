timeseries
==========

`timeseries` is a timeseries service.

Build
-----

    $ rebar3 compile
    
How to use the server (for dummies)
-----------------------------------

Current directory should be `timeseries`.

1. Start server.

    $ rebar3 shell
    
2. Add timeseries examples (dummy, dummy2). In the rebar3 shell:
    $ timeseries_server:save(timeseries_fixtures:timeseries(dummy)).
    $ timeseries_server:save(timeseries_fixtures:timeseries(dummy2)).
  The output sholud be `ok`.
  
3. Open the index.html. In a browser (preferably Firefox): http://localhost:8080/index.html

+1. Other urls:
  * http://localhost:8080/info - information about timeseries in the server
  * http://localhost:8080/download/xxx - timeseries xxx. E.g http://localhost:8080/download/dummy
  * http://localhost:8080/motion-collector
