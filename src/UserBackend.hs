module UserBackend where

import User

class UserBackend b where
  authUser :: b -> UserName -> UserPassword -> Bool

