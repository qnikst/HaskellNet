----------------------------------------------------------------------
-- |
-- Module      :  HaskellNet.Auth
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  stable
-- Portability :  portable
-- 
-- Authentication related APIs
-- 

module HaskellNet.Auth
where

import Data.Digest.MD5
import Codec.Utils
import qualified Codec.Binary.Base64 as B64 (encode)

import Data.List
import Data.Bits
import Data.Array

type UserName = String
type Password = String

data AuthType = PLAIN
              | LOGIN
              | CRAM_MD5 String

b64Encode :: String -> String
b64Encode = map (toEnum.fromEnum) . B64.encode . map (toEnum.fromEnum)

showOctet :: [Octet] -> String
showOctet = concat . map hexChars
    where hexChars c = [arr ! (c `div` 16), arr ! (c `mod` 16)]
          arr = listArray (0, 15) "0123456789abcdef"

hmacMD5 :: String -> String -> [Octet]
hmacMD5 text key = hash $ okey ++ hash (ikey ++ map (toEnum.fromEnum) text)
    where koc = map (toEnum.fromEnum) key
          key' = if length koc > 64
                 then hash koc ++ replicate 48 0
                 else koc ++ replicate (64-length koc) 0
          ipad = replicate 64 0x36
          opad = replicate 64 0x5c
          ikey = zipWith xor key' ipad
          okey = zipWith xor key' opad

plain :: UserName -> Password -> String
plain user pass = b64Encode $ concat $ intersperse "\0" [user, user, pass]

login :: UserName -> Password -> String
login = plain

cramMD5 :: String -> UserName -> Password -> String
cramMD5 challenge user pass =
    b64Encode (user ++ " " ++ showOctet (hmacMD5 challenge pass))

auth :: AuthType -> UserName -> Password -> String
auth PLAIN user pass = plain user pass
auth LOGIN user pass = login user pass
auth (CRAM_MD5 c) user pass = cramMD5 c user pass
