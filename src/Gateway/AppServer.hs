{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Gateway.AppServer
  ( runServer,
  )
where

import Domain.Customer.CustomerRepository
import Domain.Customer.CustomerService
import Domain.Order.OrderRepository
import Domain.Order.OrderService
import Gateway.Customer.Handler as CustomerHandler
import Gateway.Order.Handler as OrderHandler
import Infrastructure.Config
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type AppAPI = CustomerAPI :<|> OrderAPI

appServer :: CustomerService IO -> OrderService IO -> Server AppAPI
appServer customerService orderService =
  CustomerHandler.handler customerService
    :<|> OrderHandler.handler orderService

app :: CustomerService IO -> OrderService IO -> Application
app customerService orderService = serve (Proxy @AppAPI) (appServer customerService orderService)

runServer ::
  ( CustomerRepository customerRepo,
    OrderRepository orderRepo
  ) =>
  customerRepo ->
  orderRepo ->
  AppConfig ->
  IO ()
runServer customerRepo orderRepo config = do
  let serverPort = port (server config)

  let customerService = mkCustomerService customerRepo
  let orderService = mkOrderService orderRepo

  run serverPort (app customerService orderService)
