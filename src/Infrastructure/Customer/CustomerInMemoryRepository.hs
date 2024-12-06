{-# LANGUAGE InstanceSigs #-}

module Infrastructure.Customer.CustomerInMemoryRepository (new) where

import Control.Concurrent.MVar
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Domain.Customer.CustomerRepository
import Domain.Customer.Errors
import Domain.Customer.Types

newtype CustomerInMemoryRepository = CustomerInMemoryRepository
  { repoData :: MVar (HM.HashMap CustomerId Customer)
  }

new :: IO CustomerInMemoryRepository
new = do
  customers <- newMVar HM.empty
  return $ CustomerInMemoryRepository customers

instance CustomerRepository CustomerInMemoryRepository where
  getCustomer :: CustomerInMemoryRepository -> CustomerId -> ExceptT CustomerNotFound IO Customer
  getCustomer repo cid = do
    customers <- liftIO $ readMVar $ repoData repo
    case HM.lookup cid customers of
      Just customer -> return customer
      Nothing -> throwError $ CustomerNotFound cid

  saveCustomer :: CustomerInMemoryRepository -> Customer -> ExceptT CustomerAlreadyExists IO Customer
  saveCustomer repo customer = do
    let customersRef = repoData repo
    let cid = customerId customer
    customers <- liftIO $ takeMVar customersRef
    if HM.member cid customers
      then do
        liftIO $ putMVar customersRef customers
        throwError $ CustomerAlreadyExists cid
      else do
        let newCustomers = HM.insert cid customer customers
        liftIO $ putMVar customersRef newCustomers
        return customer
