module Conduit.Features.Account (handlers) where

import Conduit.App.Monad (AppM)
import Web.Scotty.Internal.Types (ScottyT)

import Conduit.Features.Account.Actions.GetUser      (handleGetUser)
import Conduit.Features.Account.Actions.LoginUser    (handleUserLogin)
import Conduit.Features.Account.Actions.RegisterUser (handleUserRegistration)

handlers :: ScottyT AppM ()
handlers = fold
  [ handleGetUser
  , handleUserLogin
  , handleUserRegistration
  ]
