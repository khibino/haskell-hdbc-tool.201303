{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.RecordJoin
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.RecordJoin (
  Record, unRecord, record,

  RecordFromSql, runTakeRecord,
  createRecordFromSql,

  recordDeSerializer,

  (<&>),

  PrimaryKey, definePrimaryKey, leftPrimaryKey, index,

  HasPrimaryKey (primaryKey),

  outer,

  FromSql (recordFromSql),
  takeRecord, takeSimpleRecord,
  toRecord, toSimpleRecord,

  RecordToSql, fromRecord,
  createRecordToSql,

  recordSerializer,

  ToSql (recordToSql),

  updateValues',
  updateValues
  ) where

import Database.HDBC.Persistable
  (PersistableRecord, Persistable, persistable)
import qualified Database.HDBC.Persistable as Persistable

import Database.HDBC (SqlValue(SqlNull))
import Control.Monad (liftM, ap)
import Control.Applicative ((<$>), Applicative(pure, (<*>)))


newtype Record a = Record { unRecord :: a } deriving (Show, Read, Eq)

record :: a -> Record a
record = Record


newtype RecordFromSql a =
  RecordFromSql
  { runTakeRecord :: [SqlValue] -> (a, [SqlValue]) }

createRecordFromSql' :: ([SqlValue] -> (a, [SqlValue])) -> RecordFromSql a
createRecordFromSql' =  RecordFromSql

createRecordFromSql :: ([SqlValue] -> (a, [SqlValue])) -> RecordFromSql (Record a)
createRecordFromSql takeF = createRecordFromSql' takeF'  where
  takeF' vals = let (a, vals') = takeF vals
               in  (record a, vals')

recordDeSerializer :: PersistableRecord a -> RecordFromSql (Record a)
recordDeSerializer =  createRecordFromSql . Persistable.takeRecord


instance Monad RecordFromSql where
  return a = createRecordFromSql' ((,) a)
  ma >>= fmb =
    createRecordFromSql' (\vals ->
                           let (a, vals') = runTakeRecord ma vals
                           in  runTakeRecord (fmb a) vals')

instance Functor RecordFromSql where
  fmap = liftM

instance Applicative RecordFromSql where
  pure  = return
  (<*>) = ap

(<&>) :: RecordFromSql a -> RecordFromSql b -> RecordFromSql (a, b)
a <&> b = (,) <$> a <*> b

infixl 4 <&>


newtype PrimaryKey a = PrimaryKey { index :: Int }

definePrimaryKey :: Int -> PrimaryKey a
definePrimaryKey = PrimaryKey

leftPrimaryKey :: PrimaryKey a -> PrimaryKey (a, b)
leftPrimaryKey pa = definePrimaryKey (index pa)

outer :: RecordFromSql a -> PrimaryKey a -> RecordFromSql (Maybe a)
outer rec pkey = createRecordFromSql' mayToRec where
  mayToRec vals
    | vals !! index pkey /= SqlNull = (Just a,  vals')
    | otherwise                     = (Nothing, vals')  where
      (a, vals') = runTakeRecord rec vals


class FromSql a where
  recordFromSql :: RecordFromSql a

instance Persistable a => FromSql (Record a)  where
  recordFromSql = recordDeSerializer persistable

instance (FromSql a, FromSql b) => FromSql (a, b)  where
  recordFromSql = recordFromSql <&> recordFromSql


class HasPrimaryKey a where
  primaryKey :: PrimaryKey a
  
instance HasPrimaryKey a => HasPrimaryKey (a, b) where
  primaryKey = leftPrimaryKey primaryKey

instance (HasPrimaryKey a, FromSql a) => FromSql (Maybe a) where
  recordFromSql = outer recordFromSql $ primaryKey

takeRecord :: FromSql a => [SqlValue] -> (a, [SqlValue])
takeRecord =  runTakeRecord recordFromSql

takeSimpleRecord :: FromSql (Record a) => [SqlValue] -> (a, [SqlValue])
takeSimpleRecord vals = (unRecord ra, vals')  where
  (ra, vals') = takeRecord vals

toRecord :: FromSql a => [SqlValue] -> a
toRecord = fst . takeRecord

toSimpleRecord :: FromSql (Record a) => [SqlValue] -> a
toSimpleRecord =  unRecord . toRecord


data RecordToSql a =
  RecordToSql
  { fromRecord :: a -> [SqlValue] }

createRecordToSql' :: (a -> [SqlValue]) -> RecordToSql a
createRecordToSql' =  RecordToSql

createRecordToSql :: (a -> [SqlValue]) -> RecordToSql (Record a)
createRecordToSql packF = createRecordToSql' (packF . unRecord)

recordSerializer :: PersistableRecord a -> RecordToSql (Record a)
recordSerializer =  createRecordToSql . Persistable.fromRecord


class ToSql a where
  recordToSql :: RecordToSql a

instance Persistable a => ToSql (Record a) where
  recordToSql = recordSerializer persistable


updateValues' :: RecordToSql ra
              -> PrimaryKey ra
              -> ra
              -> [SqlValue]
updateValues' pr pk a = hd ++ tl  where
  (hd, _pk:tl) = splitAt (index pk) (fromRecord pr a)

updateValues :: (HasPrimaryKey (Record a), ToSql (Record a)) =>
                a -> [SqlValue]
updateValues = updateValues' recordToSql primaryKey . record
