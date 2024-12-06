module Domain.Order.Errors
  ( OrderNotFound (..),
    OrderAlreadyExists (..),
  )
where

import Domain.Order.Types

newtype OrderNotFound = OrderNotFound OrderId
  deriving (Show, Eq)

newtype OrderAlreadyExists = OrderAlreadyExists OrderId
  deriving (Show, Eq)
