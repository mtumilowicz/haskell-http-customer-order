{-# LANGUAGE DeriveGeneric #-}

module Domain.Order.OrderService
  ( mkOrderService,
    OrderService (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.UUID.V4 as UUIDv4
import Domain.Order.Errors
import Domain.Order.OrderRepository (OrderRepository (..))
import Domain.Order.Types
import GHC.Generics (Generic)

data OrderService m = OrderService
  { findOrder :: OrderId -> ExceptT OrderNotFound m Order,
    createOrder :: CreateOrderCommand -> ExceptT OrderAlreadyExists m Order
  }
  deriving (Generic)

mkOrderService :: OrderRepository IO -> OrderService IO
mkOrderService repository =
  OrderService
    { findOrder = findOrder' repository,
      createOrder = createOrder' repository
    }

findOrder' :: OrderRepository IO -> OrderId -> ExceptT OrderNotFound IO Order
findOrder' = getOrder

createOrder' :: OrderRepository IO -> CreateOrderCommand -> ExceptT OrderAlreadyExists IO Order
createOrder' repository command = do
  uuid <- liftIO UUIDv4.nextRandom
  let order =
        Order
          { orderId = OrderId uuid,
            orderName = newOrderName command
          }
  saveOrder repository order
