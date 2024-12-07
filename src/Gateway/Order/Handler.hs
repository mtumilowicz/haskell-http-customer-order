{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Gateway.Order.Handler
  ( handler,
    OrderApiOutput,
    CreateOrderApiInput,
    OrderAPI,
  )
where

import Control.Lens
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Generics.Labels ()
import Domain.Order.Errors
import Domain.Order.OrderService
import Domain.Order.Types
import Gateway.Order.In.CreateOrderApiInput as CreateOrderApiInput
import Gateway.Order.Out.OrderApiOutput as OrderApiOutput
import Servant

type OrderAPI =
  "orders" :> Capture "orderId" String :> Get '[JSON] OrderApiOutput
    :<|> "orders" :> ReqBody '[JSON] CreateOrderApiInput :> Post '[JSON] OrderApiOutput

handler :: OrderService IO -> Server OrderAPI
handler service = getOrderHandler :<|> createOrderHandler
  where
    getOrderHandler :: String -> Handler OrderApiOutput
    getOrderHandler oidStr =
      case parseOrderId oidStr of
        Left err -> throwError err400 {errBody = BL.pack $ "Invalid order ID: " <> err}
        Right oid -> do
          order <- (service ^. #findOrder) oid & runExceptT & liftIO
          case order of
            Left (OrderNotFound _) ->
              throwError err404 {errBody = "Order not found: " <> BL.pack oidStr}
            Right o -> return $ OrderApiOutput.fromOrder o

    createOrderHandler :: CreateOrderApiInput -> Handler OrderApiOutput
    createOrderHandler input = do
      let command = CreateOrderApiInput.toDomain input
      result <- (service ^. #createOrder) command & runExceptT & liftIO
      case result of
        Left (OrderAlreadyExists cid) ->
          throwError err409 {errBody = "Order already exists with ID: " <> BL.pack (show $ unOrderId cid)}
        Right order -> return $ OrderApiOutput.fromOrder order
