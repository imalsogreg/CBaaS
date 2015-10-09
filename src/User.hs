{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module User where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API

-- data User = User
--   { _uLogin :: Int
--   , _uEmail :: Text
--   } deriving (Show, Eq, Generic)

data LoginInfo = LoginInfo
  { _liUsername :: Text
  , _liPassword :: Text
  , _liRemember :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON   LoginInfo where
instance FromJSON LoginInfo where

instance FromFormUrlEncoded LoginInfo where
  fromFormUrlEncoded xs = case res of
    Just li -> Right li
    Nothing -> Left  "Failed to parse LoginInfo"
    where res = do
            u <- lookup "username" xs
            p <- lookup "password" xs
            l <- case lookup "remember" xs of
                   Just "true" -> return True  -- TODO: is this how true
                                               -- query param comes in?
                   _           -> return False
            return (LoginInfo u p l)


data RegisterInfo = RegisterInfo
  { _riUsername :: Text
  , _riPassword :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON   RegisterInfo where
instance FromJSON RegisterInfo where

instance FromFormUrlEncoded RegisterInfo where
  fromFormUrlEncoded xs = case res of
    Just ri -> Right ri
    Nothing -> Left "Failed to parse RegisterInfo"
    where res = RegisterInfo
                <$> lookup "username" xs
                <*> lookup "password" xs

