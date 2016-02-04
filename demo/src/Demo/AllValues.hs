{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Demo.AllValues (
  AllValues(..),
  AllValuesApi,
  allValuesApi,
  allValuesApp,
  combine,
) where

import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Sequence as Seq hiding (take)
import           Data.String
import           Generics.Eot
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           Demo.Adt

class AllValues a where
  allValues :: [a]
  default allValues :: (HasEot a, GAllValues (Eot a)) => [a]
  allValues = map fromEot gAllValues

instance (AllValues a, AllValues b) => AllValues (Either a b)

instance AllValues () where
  allValues = [()]

instance AllValues Bool

instance AllValues Int where
  allValues = 0 : alternate [1 ..] [-1, -2 ..]

instance AllValues Char where
  allValues =
    ['a' .. 'z'] ++
    [chr 0 .. pred 'a'] ++
    [succ 'z' ..]

instance AllValues a => AllValues [a] where
  allValues =
    [] :
    map (uncurry (:)) (combine allValues allValues)

instance (AllValues a, AllValues b) => AllValues (a, b) where
  allValues = combine allValues allValues

alternate :: [a] -> [a] -> [a]
alternate (x : xs) (y : ys) = x : y : alternate xs ys
alternate [] l = l
alternate l [] = l

combine :: forall a b . [a] -> [b] -> [(a, b)]
combine xs ys = evalState (concat <$> mapM go (padZip xs ys)) (mempty, mempty)
  where
    go :: (Maybe a, Maybe b) -> State (Seq a, Seq b) [(a, b)]
    go = \ case
      (Just x, Just y) -> do
        (xs, ys) <- get
        put (xs |> x, ys |> y)
        let xLen = Seq.length xs
            yLen = Seq.length ys
            positions =
              (map (\ x -> (x, yLen)) [0 .. xLen]) ++
              (map (\ y -> (xLen, y)) [0 .. pred yLen])
        mapM positionLookup positions
      (Just x, Nothing) -> do
        (xs, ys) <- get
        put (xs |> x, ys)
        let xLen = Seq.length xs
            yLen = Seq.length ys
            positions =
              (map (\ y -> (xLen, y)) [0 .. pred yLen])
        mapM positionLookup positions
      (Nothing, Just y) -> do
        (xs, ys) <- get
        put (xs, ys |> y)
        let xLen = Seq.length xs
            yLen = Seq.length ys
            positions =
              (map (\ x -> (x, yLen)) [0 .. pred xLen])
        mapM positionLookup positions
      (Nothing, Nothing) -> return []

    positionLookup :: (Int, Int) -> State (Seq x, Seq y) (x, y)
    positionLookup (x, y) = do
      (xs, ys) <- get
      return (index xs x, index ys y)

padZip :: [a] -> [b] -> [(Maybe a, Maybe b)]
padZip (x : xs) (y : ys) =
  (Just x, Just y) : padZip xs ys
padZip (x : xs) [] =
  (Just x, Nothing) : padZip xs []
padZip [] (y : ys) =
  (Nothing, Just y) : padZip [] ys
padZip [] [] = []

-- * generics

class GAllValues eot where
  gAllValues :: [eot]

instance (GAllValues x, GAllValues xs) =>
  GAllValues (Either x xs) where

  gAllValues = alternate
    (map Left gAllValues)
    (map Right gAllValues)

instance (AllValues x, GAllValues xs) => GAllValues (x, xs) where
  gAllValues = do
    combine allValues gAllValues

instance GAllValues Void where
  gAllValues = []

instance GAllValues () where
  gAllValues = [()]

-- * api

type AllValuesApi =
  QueryParam "limit" Int :> Get '[HTML] [DemoADT]

allValuesApi :: Proxy AllValuesApi
allValuesApi = Proxy

allValuesApp :: Server AllValuesApi
allValuesApp (fromMaybe 10 -> limit) = return $ take limit allValues

instance AllValues DemoADT

instance ToMarkup [DemoADT] where
  toMarkup list = do
    H.head $ do
      H.title "All Values"
    H.body $ do
      H.ul $ forM_ list $ \ value -> do
        H.li $ fromString $ show value
