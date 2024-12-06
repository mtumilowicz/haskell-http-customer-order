{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Customer.Types
  ( Customer (..),
    CustomerId (..),
    parseCustomerId,
    CustomerName (..),
    CreateCustomerCommand (..),
  )
where

import Data.Hashable (Hashable)
import Data.UUID as UUID

newtype CustomerId = CustomerId {unCustomerId :: UUID}
  deriving (Show, Eq, Hashable)

newtype CustomerName = CustomerName {unCustomerName :: String}
  deriving (Show, Eq)

parseCustomerId :: String -> Either String CustomerId
parseCustomerId str =
  case UUID.fromString str of
    Nothing -> Left "Invalid UUID format."
    Just uuid -> Right $ CustomerId uuid

data Customer = Customer
  { customerId :: CustomerId,
    customerName :: CustomerName
  }
  deriving (Show, Eq)

data CreateCustomerCommand = CreateCustomerCommand
  { newCustomerName :: CustomerName
  }
