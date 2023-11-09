module Conduit.Features.Account.Handlers where

import Conduit.App.Monad (AppM)
import Web.Scotty.Internal.Types (ScottyT)

import Conduit.Features.Account.Actions.GetUser      (handleGetUser)
import Conduit.Features.Account.Actions.LoginUser    (handleUserLogin)
import Conduit.Features.Account.Actions.RegisterUser (handleUserRegistration)
import Conduit.Features.Account.Actions.GetProfile   (handleGetProfile)
import Conduit.Features.Account.Actions.UpdateUser   (handleUpdateUser)
import Conduit.Features.Account.Actions.FollowUser   (handleUserFollow)
import Conduit.Features.Account.Actions.UnfollowUser (handleUserUnfollow)

handlers :: ScottyT AppM ()
handlers = fold
  [ handleGetUser
  , handleUserLogin
  , handleUserRegistration
  , handleUpdateUser
  , handleGetProfile
  , handleUserFollow
  , handleUserUnfollow
  ]
