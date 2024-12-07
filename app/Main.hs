{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Domain.Customer.CustomerRepository as CustomerRepository
import Gateway.AppServer
import Infrastructure.Config (loadConfig)
import Infrastructure.Order.OrderInMemoryRepository as OrderInMemoryRepository

main :: IO ()
main = do
  customerRepo <- CustomerRepository.newInMemoryRepository
  orderRepo <- OrderInMemoryRepository.new

  config <-
    runExceptT (loadConfig "config.yaml") >>= \case
      Left err -> fail (show err)
      Right cfg -> return cfg

  runServer customerRepo orderRepo config
