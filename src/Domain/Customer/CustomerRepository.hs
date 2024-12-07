module Domain.Customer.CustomerRepository
  ( CustomerRepository (..),
    newInMemoryRepository,
  )
where

import Control.Concurrent.MVar
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Domain.Customer.Errors
import Domain.Customer.Types

data CustomerRepository m = CustomerRepository
  { getCustomer :: CustomerId -> ExceptT CustomerNotFound m Customer,
    saveCustomer :: Customer -> ExceptT CustomerAlreadyExists m Customer
  }

type CustomerStore = MVar (HM.HashMap CustomerId Customer)

newInMemoryRepository :: IO (CustomerRepository IO)
newInMemoryRepository = do
  customerStore <- newMVar HM.empty
  return $
    CustomerRepository
      { getCustomer = getCustomer' customerStore,
        saveCustomer = saveCustomer' customerStore
      }
  where
    getCustomer' :: CustomerStore -> CustomerId -> ExceptT CustomerNotFound IO Customer
    getCustomer' customerStore cid = do
      customers <- liftIO $ readMVar customerStore
      case HM.lookup cid customers of
        Just customer -> return customer
        Nothing -> throwError $ CustomerNotFound cid

    saveCustomer' :: CustomerStore -> Customer -> ExceptT CustomerAlreadyExists IO Customer
    saveCustomer' customerStore customer =
      do
        let cid = customerId customer
        liftIO $
          modifyMVar customerStore $ \customers ->
            if HM.member cid customers
              then return (customers, Left $ CustomerAlreadyExists cid)
              else
                let newCustomers = HM.insert cid customer customers
                 in return (newCustomers, Right customer)
        >>= either throwError return
