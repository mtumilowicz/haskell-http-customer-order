{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Gateway.Order.In.CreateOrderApiInput
  ( CreateOrderApiInput (..),
    toDomain,
  )
where

import Data.Aeson
import Domain.Order.Types
import GHC.Generics

data CreateOrderApiInput = CreateOrderApiInput
  { name :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

toDomain :: CreateOrderApiInput -> CreateOrderCommand
toDomain input =
  CreateOrderCommand
    { newOrderName = OrderName $ name input
    }
