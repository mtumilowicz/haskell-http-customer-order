{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Order.Types
  ( Order (..),
    OrderId (..),
    parseOrderId,
    OrderName (..),
    CreateOrderCommand (..),
  )
where

import Data.Hashable (Hashable)
import Data.UUID as UUID

newtype OrderId = OrderId {unOrderId :: UUID}
  deriving (Show, Eq, Hashable)

newtype OrderName = OrderName {unOrderName :: String}
  deriving (Show, Eq)

parseOrderId :: String -> Either String OrderId
parseOrderId str =
  case UUID.fromString str of
    Nothing -> Left "Invalid UUID format."
    Just uuid -> Right $ OrderId uuid

data Order = Order
  { orderId :: OrderId,
    orderName :: OrderName
  }
  deriving (Show, Eq)

data CreateOrderCommand = CreateOrderCommand
  { newOrderName :: OrderName
  }
