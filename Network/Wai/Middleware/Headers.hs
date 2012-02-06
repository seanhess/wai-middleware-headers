{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Headers
    ( cors
    , addHeaders
    ) where

import Network.Wai
import Data.ByteString
import Network.HTTP.Types (Header)

cors :: Middleware
cors = addHeaders [("Access-Control-Allow-Origin", "*")]

addHeaders :: [Header] -> Middleware
addHeaders hs app env = do
    res <- app env
    return $ case res of
        ResponseFile s rhs f mfp -> ResponseFile s (fix rhs) f mfp
        ResponseBuilder s rhs b -> ResponseBuilder s (fix rhs) b
        ResponseSource s rhs src -> ResponseSource s (fix rhs) src
  where fix rhs = rhs ++ hs
