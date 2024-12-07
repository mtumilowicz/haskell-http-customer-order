[![Build Status](https://app.travis-ci.com/mtumilowicz/haskell-http-customer-order.svg?token=PwyvjePQ7aiAX51hSYLE&branch=main)](https://app.travis-ci.com/mtumilowicz/haskell-http-customer-order)

# haskell-http-customer-order
basic CRUD service with two concepts: customers and orders (to show how apis and handlers compose)

goal was to check how to implement server e2e in haskell

# design
hexagonal architecture (port & adapters) with concepts separation
* Gateway (http layer, separation of payloads from domain objects)
* Domain (services, ports for data access, domain objects)
    * in that case services are very thin, but usually they have a lot of logic
    * switching to sql repositories: implementation in corresponding repositories and plugging in `Main.hs`
* Infrastructure
    * in that case - only config

# test
* Gateway (per handler)
    * in future can be run on sql repos
* Domain (per service)
    * mocked repositories

# manual testing
* `stack run`
    ```
    curl -X POST \
      "http://localhost:8080/customers" \
      -H "Content-Type: application/json" \
      -d '{"name": "Alice"}'
    ```
    ```
    curl -X POST \
      "http://localhost:8080/orders" \
      -H "Content-Type: application/json" \
      -d '{"name": "Macbook"}'
    ```
