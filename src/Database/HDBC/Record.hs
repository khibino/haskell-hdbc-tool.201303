{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.HDBC.Record (
  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width,

  singleton,

  Persistable (..),

  takeRecord
  ) where

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue, fromSql, toSql)

data PersistableRecord a =
  PersistableRecord
  { toRecord   :: [SqlValue] -> a
  , fromRecord :: a -> [SqlValue]
  , width      :: !Int
  }

persistableRecord :: ([SqlValue] -> a) -> (a -> [SqlValue]) -> Int -> PersistableRecord a
persistableRecord = PersistableRecord

singleton :: (Convertible SqlValue a, Convertible a SqlValue) => PersistableRecord a
singleton =  persistableRecord (fromSql . head) ((:[]) . toSql) 1

class Persistable a where
  persistable :: PersistableRecord a

instance (Convertible SqlValue a, Convertible a SqlValue) => Persistable a  where
  persistable = singleton

takeRecord :: PersistableRecord a -> [SqlValue] -> (a, [SqlValue])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals
