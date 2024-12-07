{-# LANGUAGE DeriveGeneric #-}

module Domain.Customer.CustomerService
  ( mkCustomerService,
    CustomerService (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.UUID.V4 as UUIDv4
import Domain.Customer.CustomerRepository (CustomerRepository (..))
import Domain.Customer.Errors
import Domain.Customer.Types
import GHC.Generics (Generic)

data CustomerService m = CustomerService
  { findCustomer :: CustomerId -> ExceptT CustomerNotFound m Customer,
    createCustomer :: CreateCustomerCommand -> ExceptT CustomerAlreadyExists m Customer
  }
  deriving (Generic)

mkCustomerService :: CustomerRepository IO -> CustomerService IO
mkCustomerService repository =
  CustomerService
    { findCustomer = findCustomer' repository,
      createCustomer = createCustomer' repository
    }

findCustomer' :: CustomerRepository IO -> CustomerId -> ExceptT CustomerNotFound IO Customer
findCustomer' = getCustomer

createCustomer' :: CustomerRepository IO -> CreateCustomerCommand -> ExceptT CustomerAlreadyExists IO Customer
createCustomer' repository command = do
  uuid <- liftIO UUIDv4.nextRandom
  let customer =
        Customer
          { customerId = CustomerId uuid,
            customerName = newCustomerName command
          }
  saveCustomer repository customer
