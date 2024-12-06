module Domain.Customer.CustomerRepository
  ( CustomerRepository (..),
  )
where

import Control.Monad.Except (ExceptT)
import Domain.Customer.Errors
import Domain.Customer.Types

class CustomerRepository repo where
  getCustomer :: repo -> CustomerId -> ExceptT CustomerNotFound IO Customer
  saveCustomer :: repo -> Customer -> ExceptT CustomerAlreadyExists IO Customer
