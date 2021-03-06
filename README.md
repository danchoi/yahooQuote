# yahooQuote

A command line tool to fetch Yahoo finance data for stock tickers.

```
Usage: yahooq SYMBOL [-t|--timeout MSEC] 
```



```
yahooq-cache

Usage: yahooq-cache [-s|--symbol SYMBOL] [-f|--freshness SEC] [-d|--dbconf PATH]
  Caching service helper for yahooq

Available options:
  -h,--help                Show this help text
  -s,--symbol SYMBOL       Fetch mode; provide ticker symbol
  -f,--freshness SEC       [fetch mode] minimum cached age in seconds
  -d,--dbconf PATH         path to MySQL db configuration. Default is yahooq.cfg
```

To use yahooq-cache you must create a MySQL database and put the
database configuration in `yahooq.cfg`. Example configuration:

```
mysqlHost = "localhost"
mysqlUser = "root"
mysqlDatabase = "tickers"
mysqlPassword = ""
mysqlPort = 3306
mysqlUnixSocket = "/tmp/mysql.sock"
```

Then create the database with the `create.sql` script.


`yahooq-cache` can be put in the pipeline in two ways. 

In pipeline downstream, `yahooq-cache` acts like `tee` and caches any output
from yahooq while copying the text stream through to STDOUT. Example:

```
yahooq YHOO | yahooq-cache
```

But `yahooq-cache` can also substitute relatively fresh cached data for live
data from `yahooq` when used in a short-cutting bash expression like this:

```
yahooq-cache -s YHOO -f 60 || (yahoo YHOO | yahooq-cache)
```


If data is returned from the MySQL cache, the JSON is augmented by extra
values for CACHED and CACHED-AGE-SECONDS; e.g., 

    {..."CACHED":"2014-03-07 17:21:42","CACHED-AGE-SECONDS":"2111",...}



### yahooq-web

The auxilliary command `yahooq-web` starts up a web server that serves JSON to
API queries to `/yahooQuote/:symbol`. The URL pattern is.

    yahooQuote/[symbol]?[timeout=MILLISECONDS]&[freshness=SECS]"

The default timeout is 2000 milliseconds. Default freshness is 1 hour (3600 s).

Make sure `yahooq`, `yahooq-cache`, and `bin/yahooq-wrappers` are in PATH
environment variable for the webserver process.




### Building

The code requires the HEAD of HBDC-mysql from the repo on github, not the
version in Hackage. Clone the repo from `git@github.com:bos/hdbc-mysql.git` and
then `cabal sandbox add-source [path]`. 
