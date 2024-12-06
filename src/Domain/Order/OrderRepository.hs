module Domain.Order.OrderRepository
  ( OrderRepository (..),
  )
where

import Control.Monad.Except (ExceptT)
import Domain.Order.Errors
import Domain.Order.Types

class OrderRepository repo where
  getOrder :: repo -> OrderId -> ExceptT OrderNotFound IO Order
  saveOrder :: repo -> Order -> ExceptT OrderAlreadyExists IO Order
