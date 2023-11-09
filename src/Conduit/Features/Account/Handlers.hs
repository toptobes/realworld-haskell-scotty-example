module Conduit.Features.Account.Handlers where

import Conduit.App.Monad (AppM)
import Web.Scotty.Internal.Types (ScottyT)

import Conduit.Features.Account.User.GetUser         (handleGetUser)
import Conduit.Features.Account.User.LoginUser       (handleUserLogin)
import Conduit.Features.Account.User.RegisterUser    (handleUserRegistration)
import Conduit.Features.Account.User.GetProfile      (handleGetProfile)
import Conduit.Features.Account.User.UpdateUser      (handleUpdateUser)
import Conduit.Features.Account.Follows.FollowUser   (handleUserFollow)
import Conduit.Features.Account.Follows.UnfollowUser (handleUserUnfollow)

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
