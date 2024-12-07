module Domain.Order.OrderRepository
  ( OrderRepository (..),
    newInMemoryRepository,
  )
where

import Control.Concurrent.MVar
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Domain.Order.Errors
import Domain.Order.Types

data OrderRepository m = OrderRepository
  { getOrder :: OrderId -> ExceptT OrderNotFound m Order,
    saveOrder :: Order -> ExceptT OrderAlreadyExists m Order
  }

type OrderStore = MVar (HM.HashMap OrderId Order)

newInMemoryRepository :: IO (OrderRepository IO)
newInMemoryRepository = do
  orderStore <- newMVar HM.empty
  return $
    OrderRepository
      { getOrder = getOrder' orderStore,
        saveOrder = saveOrder' orderStore
      }
  where
    getOrder' :: OrderStore -> OrderId -> ExceptT OrderNotFound IO Order
    getOrder' orderStore oid = do
      orders <- liftIO $ readMVar orderStore
      case HM.lookup oid orders of
        Just order -> return order
        Nothing -> throwError $ OrderNotFound oid

    saveOrder' :: OrderStore -> Order -> ExceptT OrderAlreadyExists IO Order
    saveOrder' orderStore order =
      do
        let oid = orderId order
        liftIO $
          modifyMVar orderStore $ \orders ->
            if HM.member oid orders
              then return (orders, Left $ OrderAlreadyExists oid)
              else
                let newOrders = HM.insert oid order orders
                 in return (newOrders, Right order)
        >>= either throwError return
