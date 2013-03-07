{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.SQL.SqlWord
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Language.SQL.SqlWord (
  SqlWord (..),

  wordShow, wordsConcat,

  commaed', commaed,

  parened', parened
  ) where

import Data.String (IsString(fromString))
import Data.List (find, intercalate, intersperse)

data SqlWord = SELECT | ALL | DISTINCT | ON
             | GROUP | COUNT | SUM | AVG | MAX | MIN
             | ORDER | BY | ASC | DESC
             | FETCH | FIRST | NEXT | ROW | ROWS | ONLY

             | DELETE | USING | RETURNING

             | FROM | AS | WITH
             | JOIN | LEFT | RIGHT | NATURAL | OUTER

             | UPDATE | SET | DEFAULT

             | WHERE

             | INSERT | INTO | VALUES

             | CASE | END | WHEN | ELSE | THEN

             | LIKE
             -- | (:?)
             -- | (:=) | (:<) | (:<=)| (:>) | (:>=) | (:<>)
             -- | (:+) | (:-) | (:*) | (:/) | (:||)
             | AND | OR | NOT

             | IS | NULL

             -- | OPEN | CLOSE

             | Sequence String
             deriving (Read, Show)


instance IsString SqlWord where
  fromString s' = found (find ((== "") . snd) (reads s')) s'  where
   found  Nothing      s = Sequence s
   found (Just (w, _)) _ = w

wordShow :: SqlWord -> String
wordShow =  d  where
  d (Sequence s)   = s
  d w              = show w


wordsConcat :: [SqlWord] -> String
wordsConcat =  intercalate " " . map wordShow

commaed' :: [SqlWord] -> [SqlWord] -> [SqlWord]
commaed' ws = (intersperse "," ws ++)

commaed :: [SqlWord] -> [SqlWord]
commaed =  (`commaed'` [])

parened' :: [SqlWord] -> [SqlWord] -> [SqlWord]
parened' ws = ("(" :) . (ws ++) . ([")"] ++)

parened :: [SqlWord] -> [SqlWord]
parened =  (`parened'` [])
