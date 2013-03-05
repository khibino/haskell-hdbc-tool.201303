{-# LANGUAGE FlexibleContexts #-}

module Database.HDBC.Tuple (
  RecordFromSql, createRecordFromSql,
  takeRecord', width, runToRecord,

  singleton, (<&>),

  outer,

  PrimaryKey, createPrimaryKey, index,

  FromSql (recordFromSql),
  takeRecord,
) where

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue(SqlNull), fromSql)

data RecordFromSql a =
  RecordFromSql
  { runToRecord :: [SqlValue] -> a
  , width     :: !Int
  }

createRecordFromSql :: ([SqlValue] -> a) -> Int -> RecordFromSql a
createRecordFromSql =  RecordFromSql

takeRecord' :: RecordFromSql a -> [SqlValue] -> (a, [SqlValue])
takeRecord' rec vals = (runToRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals

singleton :: Convertible SqlValue a => RecordFromSql a
singleton =  RecordFromSql { runToRecord = fromSql . head, width = 1 }

(<&>) :: RecordFromSql a -> RecordFromSql b -> RecordFromSql (a, b)
a <&> b = RecordFromSql { runToRecord = toR, width = width a + width b } where
  toR vals = let (ra, vals') = takeRecord' a vals
             in  (ra, fst $ takeRecord' b vals')

infixl 4 <&>

newtype PrimaryKey a = PrimaryKey { index :: Int }

createPrimaryKey :: Int -> PrimaryKey a
createPrimaryKey = PrimaryKey

outer :: RecordFromSql a -> PrimaryKey a -> RecordFromSql (Maybe a)
outer rec pkey = RecordFromSql { runToRecord = mayToRec, width = width rec } where
  mayToRec vals | vals !! index pkey /= SqlNull = Just . fst $ takeRecord' rec vals
                | otherwise                  = Nothing


class FromSql a where
  recordFromSql :: RecordFromSql a

instance (FromSql a, FromSql b) => FromSql (a, b) where
  recordFromSql = recordFromSql <&> recordFromSql


-- newtype PrimaryKey a = PrimaryKey { index :: Int }

class HasPrimaryKey a where
  primaryKey :: PrimaryKey a

instance (HasPrimaryKey a, FromSql a) => FromSql (Maybe a) where
  recordFromSql = outer recordFromSql $ primaryKey


takeRecord :: FromSql a => [SqlValue] -> (a, [SqlValue])
takeRecord =  takeRecord' recordFromSql
