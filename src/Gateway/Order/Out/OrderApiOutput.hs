{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gateway.Order.Out.OrderApiOutput
  ( OrderApiOutput (..),
    fromOrder,
  )
where

import Data.Aeson
import Data.UUID as UUID
import Domain.Order.Types
import GHC.Generics
import Prelude hiding (id)

data OrderApiOutput = OrderApiOutput
  { id :: String,
    name :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

fromOrder :: Order -> OrderApiOutput
fromOrder (Order (OrderId oid) (OrderName oname)) =
  OrderApiOutput
    { id = UUID.toString oid,
      name = oname
    }
