# yahooQuote

A command line tool to fetch Yahoo finance data for stock tickers.

```
Usage: yahooq SYMBOL [-t|--timeout MSEC] 
```


In pipeline downstream, `yahooq-cache` acts like `tee` and caches any
output from yahooq.


```
Usage: yahooq-cache [-s|--symbol SYMBOL] [-f|--freshness MIN]
```

Example:

```
yahooq YHOO | yahooq-cache
```

Cached values can be conditionally retrieved with a bash expression like 


```
yahooq-cache -f YHOO || (yahoo YHOO | yahooq-cache)
```



TODO

The auxilliary command `yahooq-web` starts up a web server that serves JSON to
API queries to `/yahooQuote/:symbol`. 

The default timeout is 2000 milliseconds. To customize the timeout, use
`/yahooQuote/:symbol?timeout=ms`





