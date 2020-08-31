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

type Form' a = FormInput -> Either ([FieldEr]) a
type FieldMaybe a = (Maybe a -> String -> Maybe (FieldEr))
type AppActionFieldT a = 
     NioValidateField a
  -> NioFormKey
  -> FormInput
  -> AppActionT (Either FieldEr a)

myGetFile :: AppActionFieldT FileUpload
myGetFile = myGetFieldAppAction

myGetFileImageResize :: AppActionFieldT ImageResizedFileUpload
myGetFileImageResize = myGetFieldAppAction

myGetFieldAppAction :: (Show a, FieldGetter (AppActionT) a String) => AppActionFieldT a
myGetFieldAppAction = fieldValue'

always :: Maybe b -> String -> Maybe FieldEr
always _ _ = Nothing

always' :: Maybe a -> Either NioFieldError a
always' v = case v of
               Just x -> Right x
               Nothing -> Left (NioFieldErrorV "Internal field error")


isPresent :: NioValidateField a
isPresent v = case v of
  Just x  -> Right x
  Nothing -> Left (NioFieldErrorV $ MyNioFieldErrorEmty "???")

isEq :: (b -> Bool) -> Text -> Maybe (Either String b) -> a -> Maybe (a, NioFieldError)
isEq f t x k = case x of
  Just (Right x') -> if f x' then Nothing
                     else Just (k, NioFieldErrorV $ MyNioIncorrectValue $  t)
  _       -> Just (k, NioFieldErrorV $ MyNioIncorrectValue "Internal error")

-- isAny :: ([b] -> Bool) -> Text -> Maybe [b] -> a -> Maybe (a, NioFieldError)
-- isAny f t x k = case x of
  -- Just x' -> if f x' then Nothing else Just (k, NioFieldErrorV $ MyNioIncorrectValue $  t)
  -- _ -> Just (k, NioFieldErrorV $ MyNioIncorrectValue "Not true")

allRules :: [NioValidateField a] -> NioValidateField a
allRules [] _ = Left (NioFieldErrorV $ MyNioFieldErrorEmty "???")
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


emptyError :: [NioFieldError]
emptyError = []

minLength :: Int -> NioValidateField Text
minLength _ Nothing = Left (NioFieldErrorV MyNioFieldNotPresent)
minLength i (Just a) = 
      if ((Data.Text.length a) > i) then Right a
      else Left (NioFieldErrorV $  MyNioIncorrectValue $ cs $ "Must be longer than " <> show i <> " characters")

-- mydbg :: Show a => String -> a -> a
-- mydbg s = traceShow
--   <$> ((++) s . show)
--   <*> id
