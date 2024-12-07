module Domain.Services
  ( Services (..),
  )
where

import Domain.Customer.CustomerService
import Domain.Order.OrderService

data Services m = Services
  { customerService :: CustomerService m,
    orderService :: OrderService m
  }
