module NioFormErrors where

import Data.Text
import Data.String.Conversions

data MyNioFieldError =
    MyNioFieldErrorEmty Text
  | MyNioIncorrectValue Text
  | MyNioFileFuckup Text
  | MyNioFieldNotPresent
  | MyNioFieldInternalFailure Text deriving Eq

instance Show (MyNioFieldError) where
  show (MyNioFieldInternalFailure e) = "Internal error: " <> cs e
  show (MyNioFieldNotPresent) = "Internal error field not present"
  show (MyNioFileFuckup t ) = "File is incorrect: " <> cs t
  show (MyNioFieldErrorEmty t) = "Field must not be empty: " <> cs t
  show (MyNioIncorrectValue t ) = "Field is incorrect: " <> cs t
