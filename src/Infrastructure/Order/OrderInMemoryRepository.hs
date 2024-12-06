{-# LANGUAGE InstanceSigs #-}

module Infrastructure.Order.OrderInMemoryRepository (new) where

import Control.Concurrent.MVar
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Domain.Order.Errors
import Domain.Order.OrderRepository
import Domain.Order.Types

newtype OrderInMemoryRepository = OrderInMemoryRepository
  { repoData :: MVar (HM.HashMap OrderId Order)
  }

new :: IO OrderInMemoryRepository
new = do
  orders <- newMVar HM.empty
  return $ OrderInMemoryRepository orders

instance OrderRepository OrderInMemoryRepository where
  getOrder :: OrderInMemoryRepository -> OrderId -> ExceptT OrderNotFound IO Order
  getOrder repo oid = do
    orders <- liftIO $ readMVar $ repoData repo
    case HM.lookup oid orders of
      Just order -> return order
      Nothing -> throwError $ OrderNotFound oid

  saveOrder :: OrderInMemoryRepository -> Order -> ExceptT OrderAlreadyExists IO Order
  saveOrder repo order =
    do
      let ordersRef = repoData repo
      let oid = orderId order
      liftIO $ modifyMVar ordersRef $ \orders ->
        if HM.member oid orders
          then return (orders, Left $ OrderAlreadyExists oid)
          else
            let newOrders = HM.insert oid order orders
             in return (newOrders, Right order)
      >>= either throwError return
