# http-tower-hs

Composable HTTP client middleware for Haskell, inspired by Rust's [Tower](https://docs.rs/tower/latest/tower/).

The Haskell ecosystem has solid HTTP clients (`http-client`, `http-client-tls`) but no middleware composition story. Every project ends up hand-rolling retry logic, timeout handling, and logging around raw HTTP calls. `http-tower-hs` fixes this with a simple `Service`/`Middleware` abstraction.

## Quick start

```haskell
import Network.HTTP.Tower
import qualified Network.HTTP.Client as HTTP
import Data.Text.IO (putStrLn)

main :: IO ()
main = do
  client <- newClient
  let configured = client
        |> withRetry (constantBackoff 3 1.0)
        |> withTimeout 5000
        |> withLogging Data.Text.IO.putStrLn

  req <- HTTP.parseRequest "https://httpbin.org/get"
  result <- runRequest configured req
  case result of
    Left err   -> print err
    Right resp -> print (HTTP.responseStatus resp)
```

## Core concepts

### Service

The fundamental building block — a function from request to `IO (Either ServiceError response)`:

```haskell
newtype Service req res = Service { runService :: req -> IO (Either ServiceError res) }
```

### Middleware

A function that wraps a service to add behavior:

```haskell
type Middleware req res = Service req res -> Service req res
```

### Client

An HTTP client with a middleware stack, built using the `(|>)` operator:

```haskell
client <- newClient
let configured = client
      |> withRetry (exponentialBackoff 5 0.5 2.0)
      |> withTimeout 3000
      |> withLogging myLogger
```

## Middleware

### Retry

Retries failed requests with configurable backoff:

```haskell
-- Constant: 3 retries, 1 second between each
withRetry (constantBackoff 3 1.0)

-- Exponential: 5 retries, starting at 500ms, doubling each time
withRetry (exponentialBackoff 5 0.5 2.0)
```

### Timeout

Fails with `TimeoutError` if the request exceeds the given milliseconds:

```haskell
withTimeout 5000  -- 5 second timeout
```

### Logging

Logs method, host, status, and duration via a pluggable logger:

```haskell
withLogging (\msg -> Data.Text.IO.putStrLn msg)
```

## Error handling

All errors are returned as `Either ServiceError Response` — no exceptions escape the middleware stack:

```haskell
data ServiceError
  = HttpError SomeException
  | TimeoutError
  | RetryExhausted Int ServiceError
  | CustomError Text
```

## Building

```bash
stack build
stack test
```

## License

MIT
