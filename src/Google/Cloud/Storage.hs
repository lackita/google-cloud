{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.Storage where


import Control.Monad

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid

import Google.Cloud.Internal.Types
import Google.Cloud.Internal.HTTP
import Google.Cloud.Internal.Token



newtype Bucket = Bucket { unBucket :: String }

newtype Name = Name { unName :: String }


-- | Upload a 'ByteString' to a 'Bucket'. This is the simplest function
-- to upload something into a 'Bucket'.
uploadMedia :: Bucket -> Name -> ByteString -> ByteString -> Cloud ()
uploadMedia bucket name body contentType = do
    authH <- authorizationHeader
    void $ post url [("Content-Type", contentType), authH] body
  where
    url = "https://www.googleapis.com/upload/storage/v1/b/" <> unBucket bucket <> "/o?uploadType=media&name=" <> unName name

downloadMedia :: (FromJSON a) => Bucket -> Name -> Cloud a
downloadMedia bucket name = do
    authH <- authorizationHeader
    getJSON url [("Content-Type", "application/json"), authH]
  where
    url = "https://www.googleapis.com/upload/storage/v1/b/" <> unBucket bucket <> "/o/" <> unName name
