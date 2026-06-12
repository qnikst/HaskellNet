module Network.HaskellNet.IMAP.UTF7
    ( encodeMailboxName
    , decodeMailboxName
    )
where

import Data.Bits
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Word (Word8)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError

encodeMailboxName :: String -> String
encodeMailboxName [] = []
encodeMailboxName ('&':cs) = "&-" ++ encodeMailboxName cs
encodeMailboxName cs@(c:rest')
    | isDirect c = c : encodeMailboxName rest'
    | otherwise =
        let (encoded, rest) = span isShifted cs
        in '&' : encodeBase64 (B.unpack (TextEncoding.encodeUtf16BE (Text.pack encoded)))
           ++ "-" ++ encodeMailboxName rest
  where
    isShifted ch = ch /= '&' && not (isDirect ch)

decodeMailboxName :: String -> String
decodeMailboxName [] = []
decodeMailboxName ('&':'-':cs) = '&' : decodeMailboxName cs
decodeMailboxName ('&':cs) =
    let (encoded, rest) = break (== '-') cs
        rest' = dropDash rest
        shifted = '&' : encoded ++ dash rest
    in case rest of
         '-' : _ ->
             case decodeBase64 encoded of
               Just decoded
                   | even (length decoded) ->
                       Text.unpack (TextEncoding.decodeUtf16BEWith
                                        TextEncodingError.lenientDecode
                                        (B.pack decoded))
                       ++ decodeMailboxName rest'
               _ -> shifted ++ decodeMailboxName rest'
         _ -> shifted ++ decodeMailboxName rest'
  where
    dropDash ('-':rest) = rest
    dropDash rest = rest
    dash ('-':_) = "-"
    dash _ = ""
decodeMailboxName (c:cs) = c : decodeMailboxName cs

isDirect :: Char -> Bool
isDirect c = ord c >= 0x20 && ord c <= 0x7e && c /= '&'

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,"

encodeBase64 :: [Word8] -> String
encodeBase64 [] = []
encodeBase64 [a] =
    [ alphabetAt (fromIntegral a `shiftR` 2)
    , alphabetAt ((fromIntegral a .&. 0x03) `shiftL` 4)
    ]
encodeBase64 [a, b] =
    [ alphabetAt (fromIntegral a `shiftR` 2)
    , alphabetAt (((fromIntegral a .&. 0x03) `shiftL` 4) .|. (fromIntegral b `shiftR` 4))
    , alphabetAt ((fromIntegral b .&. 0x0f) `shiftL` 2)
    ]
encodeBase64 (a:b:c:rest) =
    [ alphabetAt (fromIntegral a `shiftR` 2)
    , alphabetAt (((fromIntegral a .&. 0x03) `shiftL` 4) .|. (fromIntegral b `shiftR` 4))
    , alphabetAt (((fromIntegral b .&. 0x0f) `shiftL` 2) .|. (fromIntegral c `shiftR` 6))
    , alphabetAt (fromIntegral c .&. 0x3f)
    ] ++ encodeBase64 rest

alphabetAt :: Int -> Char
alphabetAt n = alphabet !! n

decodeBase64 :: String -> Maybe [Word8]
decodeBase64 [] = Just []
decodeBase64 chars =
    let (chunk, rest) = splitAt 4 chars
    in do decodedChunk <- traverse base64Value chunk >>= decodeChunk
          decodedRest <- decodeBase64 rest
          return (decodedChunk ++ decodedRest)

decodeChunk :: [Int] -> Maybe [Word8]
decodeChunk [a, b] =
    Just [fromIntegral $ (a `shiftL` 2) .|. (b `shiftR` 4)]
decodeChunk [a, b, c] =
    Just [ fromIntegral $ (a `shiftL` 2) .|. (b `shiftR` 4)
         , fromIntegral $ ((b .&. 0x0f) `shiftL` 4) .|. (c `shiftR` 2)
         ]
decodeChunk [a, b, c, d] =
    Just [ fromIntegral $ (a `shiftL` 2) .|. (b `shiftR` 4)
         , fromIntegral $ ((b .&. 0x0f) `shiftL` 4) .|. (c `shiftR` 2)
         , fromIntegral $ ((c .&. 0x03) `shiftL` 6) .|. d
         ]
decodeChunk _ = Nothing

base64Value :: Char -> Maybe Int
base64Value c =
    case lookup c (zip alphabet [0..]) of
      Just n -> Just n
      Nothing -> Nothing
