# yahooQuote

A command line tool to fetch Yahoo finance data for stock tickers.

```
Usage: yahooq SYMBOL [-t|--timeout MSEC] [-u|--use-cache]
```

`--use-cache` assumes the presence of sqlite3 libraries.

The auxilliary command `yahooq-web` starts up a web server that serves JSON to
API queries to `/yahooQuote/:symbol`. 

The default timeout is 2000 milliseconds. To customize the timeout, use
`/yahooQuote/:symbol?timeout=ms`



