{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Gateway.AppServer
  ( runServer,
  )
where

import Domain.Services
import Gateway.Customer.Handler as CustomerHandler
import Gateway.Order.Handler as OrderHandler
import Infrastructure.Config
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type AppAPI = CustomerAPI :<|> OrderAPI

appServer :: Services IO -> Server AppAPI
appServer Services {..} =
  CustomerHandler.handler customerService
    :<|> OrderHandler.handler orderService

app :: Services IO -> Application
app s = serve (Proxy :: Proxy AppAPI) (appServer s)

runServer ::
  Services IO ->
  AppConfig ->
  IO ()
runServer services config = do
  let serverPort = port (server config)
  run serverPort (app services)
