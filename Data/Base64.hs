module Base64
    ( b64Encode
    , b64Decode
    , splits
    )
    where

import Control.Monad.State
import Data.Char
import Data.Ix
import Data.Bits
import Data.Array


splits _ [] = []
splits n l  = lh : splits n lt
    where (lh, lt) = splitAt n l

encChars c1 c2 c3 = [ lookupTable (shiftR c1 2)
                    , lookupTable (shiftL (c1 .&. 3) 4 .|. shiftR c2 4)
                    , lookupTable (shiftL (c2 .&. 15) 2 .|. shiftR c3 6)
                    , lookupTable (c3 .&. 63)]

b64Encode l = f $ map ord l
    where f [fst]              = encChars fst 0 0 ++ "=="
          f [fst, snd]         = encChars fst snd 0 ++ "="
          f [fst, snd, thd]    = encChars fst snd thd
          f (fst:snd:thd:rest) = encChars fst snd thd ++ f rest

decChars c1 c2 c3 c4 = map chr [ shiftL ci1 2 .|. shiftR ci2 4
                               , shiftL (ci2 .&. 15) 4 .|. shiftR ci3 2
                               , shiftL (ci3 .&. 3) 6 .|. ci4]
    where (ci1, ci2, ci3, ci4) = ( revLookup c1, revLookup c2
                                 , revLookup c3, revLookup c4)

b64Decode l = f $ filter invalidChars l
    where invalidChars c = any (flip inRange c)
                           [('A', 'Z'), ('a', 'z'), ('0', '9')]
                           || c == '+' || c == '/' || c == '='
          f (c1:c2:c3:c4:"==") = take 1 $ decChars c1 c2 c3 c4
          f (c1:c2:c3:c4:"=")  = take 2 $ decChars c1 c2 c3 c4
          f (c1:c2:c3:c4:rest) = decChars c1 c2 c3 c4 ++ f rest
          f []                 = []

revLookup c = tbl ! c
    where tbl = array ('+', 'z') $ [('+', 62), ('/', 63)] ++ zip ['0'..'9'] [52..] ++ zip ['A'..'Z'] [0..] ++ zip ['a'..'z'] [26..]
lookupTable n = tbl ! n
    where tbl = listArray (0,63) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
