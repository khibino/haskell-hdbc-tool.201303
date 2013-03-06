
module Database.HDBC.RecordJoin (
  Record, unRecord, record,

  RecordFromSql, runTakeRecord,
  createRecordFromSql,

  recordGetter,

  (<&>),

  PrimaryKey, definePrimaryKey, leftPrimaryKey, index,

  HasPrimaryKey (primaryKey),

  outer,

  FromSql (recordFromSql),
  takeRecord,

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

recordGetter :: PersistableRecord a -> RecordFromSql (Record a)
recordGetter =  createRecordFromSql . Persistable.takeRecord


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
  recordFromSql = recordGetter persistable

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


updateValues :: PrimaryKey a -> [SqlValue] -> [SqlValue]
updateValues pk vals = hd ++ tl where
  (hd, _pk:tl) = splitAt (index pk) vals
