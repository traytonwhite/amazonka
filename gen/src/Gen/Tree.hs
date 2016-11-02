{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Gen.Tree
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Tree
    ( root
    , fold
    , populate
    ) where

import           Control.Error
import           Control.Lens              (each, (%~), (&), (^.), (^..))
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson                hiding (json)
import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as Map
import qualified Data.HashSet              as Set
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Filesystem.Path.CurrentOS hiding (FilePath, root)
import           Gen.Formatting            (failure, shown)
import           Gen.Import
import qualified Gen.JSON                  as JS
import           Gen.Types
import           Prelude                   hiding (mod, product, sum)
import           System.Directory.Tree     hiding (file)
import           Text.EDE                  hiding (failure, render)

root :: AnchoredDirTree a -> Path
root (p :/ d) = decodeString p </> decodeString (name d)

fold :: MonadError Error m
     => (Path -> m ())     -- ^ Directories
     -> (Path -> a -> m b) -- ^ Files
     -> AnchoredDirTree a
     -> m (AnchoredDirTree b)
fold g f (p :/ t) = (p :/) <$> go (decodeString p) t
  where
    go x = \case
        Failed n e  -> failure shown e >> return (Failed n e)
        File   n a  -> File n <$> f (x </> decodeString n) a
        Dir    n cs -> g d >> Dir n <$> mapM (go d) cs
          where
            d = x </> decodeString n

-- If Nothing, then touch the file, otherwise write the Just contents.
type Touch = Maybe Rendered

populate :: Path
         -> Templates
         -> Library
         -> Either Error (AnchoredDirTree Touch)
populate d Templates{..} l = (encodeString d :/) . dir lib <$> layout
  where
    layout :: Either Error [DirTree Touch]
    layout = traverse sequenceA
        [ dir "src"
              -- Supress cabal warnings about directories listed that don't exist.
            [ touch ".gitkeep"
            ]

        , dir "gen"
            [ dir "Network"
                [ dir "AWS"
                    [ dir svc $
                        [ dir "Types" $
                               sums     (Map.toList (_sums'     l))
                            ++ products (Map.toList (_products' l))
                        , mod (typesNS   l) (typeImports   l) typesTemplate
                        , mod (waitersNS l) (waiterImports l) waitersTemplate
                        ] ++ map op (l ^.. operations . each)
                    , mod (libraryNS l) mempty tocTemplate
                    ]
                ]
            ]

        , dir "test"
            [ mod "Main" (testImports l) testsTemplate
            , dir "Test"
                [ dir "AWS"
                    [ touch (l ^. serviceAbbrev <> ".hs")
                    , dir svc
                        [ touch "Internal.hs"
                        ]
                    , dir "Gen"
                        [ mod (fixturesNS l) (fixtureImports l) fixturesTemplate
                        ]
                    ]
                ]
            ]

        , dir "fixture" $
            concatMap fixture (l ^.. operations . each)

        , file (lib <.> "cabal") cabalTemplate
        , file "README.md" readmeTemplate
        ]

    svc, lib :: Path
    svc = fromText (l ^. serviceAbbrev)
    lib = fromText (l ^. libraryName)

    sums = \case
        []  -> []
        [x] -> [go x]
        xs  -> [dir "Sum" (map go xs)]
      where
        go (n, ks) =
            write $ module' n (sumImports l) sumTemplate $
                pure (toJSON (filteredShapes ks))

    products = \case
        []  -> []
        [x] -> [go x]
        xs  -> [dir "Product" (map go xs)]
      where
        go (n, ks) =
            write $ module' n (productImports l) productTemplate $
                pure (toJSON (filteredShapes ks))

    filteredShapes ks =
        l & service' . shapes %~ Map.filterWithKey (const . flip Set.member ks)

    op :: Operation Identity SData a -> DirTree (Either Error Touch)
    op = write . operation' l operationTemplate

    fixture :: Operation Identity SData a -> [DirTree (Either Error Touch)]
    fixture o =
        [ touch (n <> ".yaml")
        , touch (n <> "Response.proto")
        ]
      where
       n = typeId (_opName o)

    mod :: NS -> [NS] -> Template -> DirTree (Either Error Touch)
    mod n is t = write $ module' n is t (pure env)

    file :: Path -> Template -> DirTree (Either Error Touch)
    file p t = write $ file' p t (pure env)

    env :: Value
    env = toJSON l

operation' :: Library
           -> Template
           -> Operation Identity SData a
           -> DirTree (Either Error Rendered)
operation' l t o = module' n is t $ do
    x <- JS.objectErr (show n) o
    y <- JS.objectErr "metadata" (toJSON m)
    return $! y <> x
  where
    n  = operationNS (libraryNS l) (o ^. opName)
    m  = l ^. metadata

    is = operationImports l o

module' :: ToJSON a
        => NS
        -> [NS]
        -> Template
        -> Either Error a
        -> DirTree (Either Error Rendered)
module' ns is t f = file' (filename $ nsToPath ns) t $ do
    x <- f >>= JS.objectErr (show ns)
    return $! x <> fromPairs
        [ "moduleName"    .= ns
        , "moduleImports" .= is
        ]

file' :: ToJSON a
      => Path
      -> Template
      -> Either Error a
      -> DirTree (Either Error Rendered)
file' (encodeString -> p) t f = File p $
    f >>= JS.objectErr p
      >>= fmapL LText.pack . eitherRender t

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

write :: DirTree (Either e a) -> DirTree (Either e (Maybe a))
write = fmap (second Just)

touch :: Text -> DirTree (Either e (Maybe a))
touch f = File (Text.unpack f) (Right Nothing)
