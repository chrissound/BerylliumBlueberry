{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Forms.Forms2 where

import Blog
import NioForm
import NioFormTypes
import Data.Text (Text, length)
import Data.Foldable
import AppTypes
import NioFormExtra
import qualified MyNioFieldError as MNE

type Form' a = FormInput -> Either ([FieldEr MNE.MyNioFieldError]) a
type FieldMaybe a = (Maybe a -> String -> Maybe (FieldEr MNE.MyNioFieldError))
type AppActionFieldT a =
     NioValidateField a MNE.MyNioFieldError
  -> NioFormKey
  -> FormInput
  -> AppActionT (Either (FieldEr MNE.MyNioFieldError) a)

myGetFile :: AppActionFieldT FileUpload
myGetFile = myGetFieldAppAction

myGetFileImageResize :: AppActionFieldT ImageResizedFileUpload
myGetFileImageResize = myGetFieldAppAction

myGetFieldAppAction :: (Show a, FieldGetter (AppActionT) a MNE.MyNioFieldError NioFormKey, FieldGetterErrorKey (AppActionT) a NioFormKey) => AppActionFieldT a
myGetFieldAppAction = fieldValue'

always :: Maybe b -> String -> Maybe (FieldEr MNE.MyNioFieldError)
always _ _ = Nothing

always' :: Maybe a -> Either MNE.MyNioFieldError a
always' v = case v of
               Just x -> Right x
               Nothing -> Left (MNE.MyNioFailedValidation "Internal field error")


isPresent :: NioValidateField a MNE.MyNioFieldError
isPresent v = case v of
  Just x  -> Right x
  Nothing -> Left $ MNE.MyNioFieldErrorNotPresent

isEq :: (b -> Bool) -> Text -> Maybe (Either String b) -> a -> Maybe (a, NioFieldError MNE.MyNioFieldError)
isEq f t x k = case x of
  Just (Right x') -> if f x' then Nothing
                     else Just (k, NioFieldErrorV $ MNE.MyNioIncorrectValue (cs t) (cs t))
  _       -> Just (k, NioFieldErrorV $ MNE.MyNioIncorrectValue "Internal error" "Internal error")

-- isAny :: ([b] -> Bool) -> Text -> Maybe [b] -> a -> Maybe (a, NioFieldError)
-- isAny f t x k = case x of
  -- Just x' -> if f x' then Nothing else Just (k, NioFieldErrorV $ MyNioIncorrectValue $  t)
  -- _ -> Just (k, NioFieldErrorV $ MyNioIncorrectValue "Not true")

allRules :: [NioValidateField a MNE.MyNioFieldError] -> NioValidateField a MNE.MyNioFieldError
allRules [] _ = Left $ MNE.MyNioFieldErrorNotPresent
allRules r v = case (sequence $ fmap (\r' -> r' v) r) of
                 Right [] -> error "this should never be possible..."
                 Right (x:_) -> Right x
                 Left e -> Left e

liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6
       ; return (f x1 x2 x3 x4 x5 x6)
       }

liftM7  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m r
liftM7 f m1 m2 m3 m4 m5 m6 m7
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7
       ; return (f x1 x2 x3 x4 x5 x6 x7)
       }


emptyError :: [MNE.MyNioFieldError]
emptyError = []

minLength :: Int -> NioValidateField Text MNE.MyNioFieldError
minLength _ Nothing = Left $ MNE.MyNioFieldErrorNotPresent
minLength i (Just a) =
      if ((Data.Text.length a) > i) then Right a
      else Left $ MNE.MyNioIncorrectValue (cs $ "Must be longer than " <> show i <> " characters") (cs $ "Must be longer than " <> show i <> " characters")

-- mydbg :: Show a => String -> a -> a
-- mydbg s = traceShow
--   <$> ((++) s . show)
--   <*> id
