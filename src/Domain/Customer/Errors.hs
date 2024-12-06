module Domain.Customer.Errors
  ( CustomerNotFound (..),
    CustomerAlreadyExists (..),
  )
where

import Domain.Customer.Types

newtype CustomerNotFound = CustomerNotFound CustomerId
  deriving (Show, Eq)

newtype CustomerAlreadyExists = CustomerAlreadyExists CustomerId
  deriving (Show, Eq)
