# yahooQuote

A command line tool to fetch Yahoo finance data for stock tickers.

```
Usage: yahooq SYMBOL [-t|--timeout MSEC] 
```


`yahooq-cache` can be put in the pipeline in two ways. 


```
yahooq-cache

Usage: yahooq-cache [-s|--symbol SYMBOL] [-f|--freshness SEC]
  Caching service helper for yahooq

Available options:
  -h,--help                Show this help text
  -s,--symbol SYMBOL       Fetch mode; provide ticker symbol
  -f,--freshness SEC       [fetch mode] minimum cached age in seconds
```

In pipeline downstream, `yahooq-cache` acts like `tee` and caches any output
from yahooq while copying the text stream through to STDOUT. Example:

```
yahooq YHOO | yahooq-cache
```

But `yahooq-cache` can also substitute relatively fresh cached data for live
data from `yahooq` when used in a short-cutting bash expression like this:

```
yahooq-cache -f YHOO -f 60 || (yahoo YHOO | yahooq-cache)
```


If data is returned from the sqlite3 cache, the JSON is augmented by extra
values for CACHED and CACHED-AGE-SECONDS; e.g., 

{"Error Indication (returned for symbol changed / invalid)":"N/A","Previous Close":"39.66","Price/EPS Estimate Current Year":"25.10","52-week Range":"21.87 - 41.72","P/E Ratio":"31.48","CACHED":"2014-03-07 17:21:42","Volume":"6752283","EPS Estimate Next Year":"1.80","Market Capitalization":"39.008B","Name":"Yahoo Inc.","Bid":"38.64","Open":"39.87","Last Trade (Price Only)":"38.645","EPS Estimate Current Year":"1.58","Symbol":"YHOO","CACHED-AGE-SECONDS":"2111","Price/EPS Estimate Next Year":"22.03"}




TODO

The auxilliary command `yahooq-web` starts up a web server that serves JSON to
API queries to `/yahooQuote/:symbol`. 

The default timeout is 2000 milliseconds. To customize the timeout, use
`/yahooQuote/:symbol?timeout=ms`




