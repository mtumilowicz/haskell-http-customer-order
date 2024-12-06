{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gateway.Customer.Out.CustomerApiOutput
  ( CustomerApiOutput (..),
    fromCustomer,
  )
where

import Data.Aeson
import qualified Data.UUID as UUID
import Domain.Customer.Types
import GHC.Generics
import Prelude hiding (id)

data CustomerApiOutput = CustomerApiOutput
  { id :: String,
    name :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

fromCustomer :: Customer -> CustomerApiOutput
fromCustomer (Customer (CustomerId cid) (CustomerName cname)) =
  CustomerApiOutput
    { id = UUID.toString cid,
      name = cname
    }
