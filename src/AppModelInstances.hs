module AppModelInstances where

import AppTypes
-- import Models.Image

class AppModel a where
  dataPath :: a -> AppAction String

-- instance AppModel FileUploadImage where
--   dataPath = const imageDataPath
