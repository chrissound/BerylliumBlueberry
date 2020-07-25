{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Forms.Forms2 where

import Blog
import NioForm
import NioFormM
import NioFormTypes
import Data.Text (Text, length)
import Data.Foldable
import AppTypes

type Form' a = FormInput -> Either ([FieldEr]) a
type FieldMaybe a = (Maybe a -> String -> Maybe (FieldEr))
type AppActionFieldT a = 
     NioValidateField a
  -> NioFormKey
  -> FormInput
  -> AppActionT (Either (FieldEr) a)

myGetField :: (Show a, FieldGetter a) =>
     NioValidateField a
  -> NioFormKey
  -> FormInput
  -> Either FieldEr a
myGetField = fieldValue (undefined)

myGetFieldM :: (Show a, FieldGetterM m a, Monad m) =>
     NioValidateField a
  -> NioFormKey
  -> FormInput
  -> m (Either FieldEr a)
myGetFieldM = fieldValueM (error "error: myGetFieldM")

myGetFile :: AppActionFieldT FileUpload
myGetFile = myGetFieldAppAction

myGetFileName :: AppActionFieldT FileUploadName
myGetFileName = myGetFieldAppAction

myGetFileImageResize :: AppActionFieldT ImageResizedFileUpload
myGetFileImageResize = myGetFieldAppAction

myGetFieldAppAction :: (Show a, FieldGetterM (AppActionT) a) =>
  AppActionFieldT a
myGetFieldAppAction = myGetFieldM

always :: Maybe b -> String -> Maybe FieldEr
always _ _ = Nothing

isPresent :: Maybe b -> String -> Maybe (String, NioFieldError)
isPresent x k = case x of
  Just _ -> Nothing
  Nothing -> Just (k, NioFieldErrorV $ MyNioFieldErrorEmty $ cs k)

isEq :: (b -> Bool) -> Text -> Maybe (Either String b) -> a -> Maybe (a, NioFieldError)
isEq f t x k = case x of
  Just (Right x') -> if f x' then Nothing
                     else Just (k, NioFieldErrorV $ MyNioIncorrectValue $  t)
  _       -> Just (k, NioFieldErrorV $ MyNioIncorrectValue "Internal error")

-- isAny :: ([b] -> Bool) -> Text -> Maybe [b] -> a -> Maybe (a, NioFieldError)
-- isAny f t x k = case x of
  -- Just x' -> if f x' then Nothing else Just (k, NioFieldErrorV $ MyNioIncorrectValue $  t)
  -- _ -> Just (k, NioFieldErrorV $ MyNioIncorrectValue "Not true")

allRules ::  [NioValidateField c] -> NioValidateField c
allRules r v k = asum $ fmap (\r' -> r' v k) r

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


emptyError :: [NioFieldError]
emptyError = []

minLength :: Int -> NioValidateField Text
minLength _ Nothing x = Just (x, NioFieldErrorV MyNioFieldNotPresent)
minLength i (Just a) k = do
  case a of
    Right a' ->
      if (Data.Text.length a') > i then
        Nothing
      else
        Just (k, NioFieldErrorV $  MyNioIncorrectValue $ cs $ "Must be longer than " <> show i <> " characters")
    Left e -> Just (k, NioFieldErrorV $ MyNioFieldInternalFailure $ cs e)

-- mydbg :: Show a => String -> a -> a
-- mydbg s = traceShow
--   <$> ((++) s . show)
--   <*> id
