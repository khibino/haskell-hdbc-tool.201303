{-# LANGUAGE FlexibleContexts #-}

module Database.HDBC.Tuple (
  RecordFromSql, takeRecord, width, toRecord',

  singleton, (<&>),

  outer
  ) where

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue(SqlNull), fromSql)

data RecordFromSql a =
  RecordFromSql
  { toRecord' :: [SqlValue] -> a
  , width     :: !Int
  }

takeRecord :: RecordFromSql a -> [SqlValue] -> (a, [SqlValue])
takeRecord rec vals = (toRecord' rec va, vr) where
  (va, vr) = splitAt (width rec) vals

singleton :: Convertible SqlValue a => RecordFromSql a
singleton =  RecordFromSql { toRecord' = fromSql . head, width = 1 }

(<&>) :: RecordFromSql a -> RecordFromSql b -> RecordFromSql (a, b)
a <&> b = RecordFromSql { toRecord' = toR, width = width a + width b } where
  toR vals = let (ra, vals') = takeRecord a vals
             in  (ra, fst $ takeRecord b vals')

infixl 4 <&>

outer :: RecordFromSql a -> Int -> RecordFromSql (Maybe a)
outer rec pkeyIdx = RecordFromSql { toRecord' = mayToRec, width = width rec } where
  mayToRec vals | vals !! pkeyIdx /= SqlNull = Just . fst $ takeRecord rec vals
                | otherwise                  = Nothing
