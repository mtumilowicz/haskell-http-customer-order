{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Gateway.Customer.In.CreateCustomerApiInput
  ( CreateCustomerApiInput (..),
    toDomain,
  )
where

import Data.Aeson
import Domain.Customer.Types
import GHC.Generics

data CreateCustomerApiInput = CreateCustomerApiInput
  { name :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

toDomain :: CreateCustomerApiInput -> CreateCustomerCommand
toDomain input =
  CreateCustomerCommand
    { newCustomerName = CustomerName $ name input
    }
