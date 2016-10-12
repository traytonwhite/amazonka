{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- Module      : Gen.Types.Id
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Id
    (
    -- * Class
      HasId (..)

    -- * Identifier
    , Id
    , mkId
    , variables

    -- * Lenses
    , memberId
    , typeId
    , typeVarId
    , ctorId
    , branchId
    , smartCtorId
    , accessorId
    , lensId

    -- * Modify representation
    , prependId
    , appendId
    , replaceId
    ) where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Aeson
import           Data.Hashable
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Manipulate
import           Gen.Text

-- | A class to extract identifiers from arbitrary products.
class HasId a where
    identifier :: a -> Id

instance HasId Id where
    identifier = id

instance (Functor f, HasId a) => HasId (Cofree f a) where
    identifier = identifier . extract

-- | A type where the actual identifier is immutable,
-- but the usable representation can be appended/modified.
data Id = Id Text Text [Char]
    deriving (Show)

instance Eq Id where
    Id x _ _ == Id y _ _ = x == y

instance Hashable Id where
    hashWithSalt n (Id x _ _) = hashWithSalt n x

instance FromJSON Id where
    parseJSON = withText "id" (pure . mkId)

instance ToJSON Id where
    toJSON = toJSON . view representation

mkId :: Text -> Id
mkId t = Id t (format t) []

format :: Text -> Text
format = upperHead . upperAcronym

variables :: Lens' Id [Char]
variables =
    lens (\(Id _ _ xs)   -> xs)
         (\(Id x t _) xs -> Id x t xs)

representation :: Lens' Id Text
representation =
    lens (\(Id _ t _)    -> t)
         (\(Id x _ xs) t -> Id x (format t) xs)

memberId :: Id -> Text
memberId (Id x _ _) = x

typeId :: Id -> Text
typeId = view representation

typeVarId :: Id -> (Text, [Char])
typeVarId n = (view representation n, view variables n)

ctorId :: Id -> Text
ctorId = (`Text.snoc` '\'') . view representation

branchId :: Maybe Text -> Id -> Text
branchId p = f . view representation
  where
    f :: Text -> Text
    f | Just x <- p = mappend (upperHead x)
      | otherwise   = id

smartCtorId :: Id -> Text
smartCtorId = renameReserved . lowerHead . lowerFirstAcronym . view representation

accessorId :: Maybe Text -> Id -> Text
accessorId p = Text.cons '_' . accessor p

lensId :: Maybe Text -> Id -> Text
lensId p = renameReserved . accessor p

accessor :: Maybe Text -> Id -> Text
accessor Nothing  = lowerHead . view representation
accessor (Just p) = f . view representation
  where
    f | Text.null p = lowerHead
      | otherwise   = mappend (Text.toLower p) . upperHead

prependId :: Text -> Id -> Id
prependId t i = i & representation %~ mappend t

appendId :: Id -> Text -> Id
appendId i t = i & representation <>~ t

replaceId :: Id -> Id -> Id
replaceId x y = x & representation .~ y ^. representation
