{-# language CPP #-}

module Network.HaskellNet.Auth
where

import Crypto.Hash.MD5
import Data.Text.Encoding.Base64 as B64

#if MIN_VERSION_base64(0,5,0)
import Data.Base64.Types as B64
#endif

import Data.Word
import Data.List
import Data.Bits
import Data.Array
import qualified Data.ByteString as B
import qualified Data.Text as T

type UserName = String
type Password = String

-- | Authorization types supported by the <https://www.ietf.org/rfc/rfc4954.txt RFC5954>
data AuthType = PLAIN
              | LOGIN
              | CRAM_MD5
              | XOAUTH2
                deriving Eq

instance Show AuthType where
    showsPrec d at = showParen (d>app_prec) $ showString $ showMain at
        where app_prec = 10
              showMain PLAIN    = "PLAIN"
              showMain LOGIN    = "LOGIN"
              showMain CRAM_MD5 = "CRAM-MD5"
              showMain XOAUTH2  = "XOAUTH2"

b64Encode :: String -> String
b64Encode = T.unpack . encode . T.pack
    where encode =
#if MIN_VERSION_base64(0,5,0)
              B64.extractBase64 . B64.encodeBase64
#else
              B64.encodeBase64
#endif



b64Decode :: String -> String
b64Decode = T.unpack . decode . T.pack
    where decode =
#if MIN_VERSION_base64(0,5,0)
              B64.decodeBase64Lenient . B64.assertBase64
#else
              B64.decodeBase64Lenient
#endif

showOctet :: [Word8] -> String
showOctet = concatMap hexChars
    where hexChars c = [arr ! (c `div` 16), arr ! (c `mod` 16)]
          arr = listArray (0, 15) "0123456789abcdef"

hashMD5 :: [Word8] -> [Word8]
hashMD5 = B.unpack . hash . B.pack

hmacMD5 :: String -> String -> [Word8]
hmacMD5 text key = hashMD5 $ okey ++ hashMD5 (ikey ++ map (toEnum.fromEnum) text)
    where koc = map (toEnum.fromEnum) key
          key' = if length koc > 64
                 then hashMD5 $ koc ++ replicate 48 0
                 else koc ++ replicate (64-length koc) 0
          ipad = replicate 64 0x36
          opad = replicate 64 0x5c
          ikey = zipWith xor key' ipad
          okey = zipWith xor key' opad

plain :: UserName -> Password -> String
plain user pass = b64Encode $ intercalate "\0" ["", user, pass]

login :: UserName -> Password -> (String, String)
login user pass = (b64Encode user, b64Encode pass)

cramMD5 :: String -> UserName -> Password -> String
cramMD5 challenge user pass =
    b64Encode (user ++ " " ++ showOctet (hmacMD5 challenge pass))

auth :: AuthType -> String -> UserName -> Password -> String
auth PLAIN    _ u p = plain u p
auth LOGIN    _ u p = let (u', p') = login u p in unwords [u', p']
auth CRAM_MD5 c u p = cramMD5 c u p
auth XOAUTH2  _ u p = b64Encode $ "user=" ++ u ++ "\001auth=" ++ p ++ "\001\001"
