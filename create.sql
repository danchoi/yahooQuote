CREATE table errors (
  ticker VARCHAR(255) NOT NULL,
  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  error TEXT NOT NULL
);
create table tickers (
  ticker VARCHAR(255) NOT NULL UNIQUE,
  jsonData TEXT,
  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

