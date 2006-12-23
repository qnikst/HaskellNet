{-# OPTIONS_GHC -fglasgow-exts #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Record
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  stable
-- Portability :  GHC only
-- 
-- Polymorphic record definitions
-- 
-- To see the usage of this module, you refer Text.Atom
-- 



module Data.Record
    (Attr(..), get, set, update, has, add, howMany, set', get', delete)
where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Prelude hiding (foldr1)

data Attr r v = Attr { getter :: r -> v
                     , setter :: r -> v -> r
                     }

get :: Attr r v -> r -> v
get = getter
set :: Attr r v -> v -> r -> r
set a v r = setter a r v
update :: Attr r v -> (v -> v) -> r -> r
update a f r = set a (f $ get a r) r

has :: (Foldable t) => Attr r (t v) -> r -> Bool
has a r = howMany a r > 0

add :: (Alternative t) => Attr r (t v) -> v -> r -> r
add a v r = update a (pure v <|>) r

howMany :: (Foldable t, Integral n) => Attr r (t v) -> r -> n
howMany a r = getSum $ foldMap (const (Sum 1)) $ get a r

set' :: (Applicative t, Foldable t) => Attr r (t v) -> v -> r -> r
set' a v r | has a r   = r
           | otherwise = set a (pure v) r
get' :: (Foldable t) => Attr r (t v) -> r -> v
get' a r = foldr1 const $ get a r

delete :: (Alternative t) => Attr r (t v) -> r -> r
delete a r = set a empty r

