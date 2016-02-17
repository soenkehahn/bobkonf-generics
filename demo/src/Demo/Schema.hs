{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Demo.Schema where

import           Data.Foldable
import           Data.List
import           Data.Proxy
import           Data.String
import           Data.String.Interpolate
import           Data.Typeable
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics
import           Generics.Eot

createSchema :: (Generic a, HasEot a) =>
  Connection -> Proxy a -> String -> IO ()
createSchema connection proxy table = do
  let fields = map (\ (field, typ) -> field ++ " " ++ toSqlType typ)
        (getFields proxy (datatype proxy))
  execute_ connection $ fromString [i|
    CREATE TABLE #{table} (#{intercalate ", " fields})
  |]
  return ()

insert :: Connection -> String -> [a] -> IO ()
insert connection table list = forM_ list $ \ e -> do
  let values = sqlValues e
  execute_ connection $ fromString [i|
    INSERT INTO #{table} VALUES #{intercalate ", " values}
  |]

query :: FromRow a => Connection -> String -> IO [a]
query connection table = do
  query_ connection $ fromString [i|SELECT * FROM #{table}|]

-- * sqlValues

getFields :: forall a . (EotFieldList (Eot a)) =>
  Proxy a -> Generics.Eot.Datatype -> [(String, TypeRep)]
getFields proxy dt =
  case dt of
    (Datatype _ [Constructor _ (Selectors fieldNames)]) ->
      zip fieldNames (eotGetFields (Proxy :: Proxy (Eot a)))

toSqlType :: TypeRep -> String
toSqlType = _

sqlValues :: a -> [String]
sqlValues = _

class EotFieldList eot where
  eotGetFields :: Proxy eot -> [TypeRep]
  eotFieldList :: eot -> [String]
  eotFromRow :: RowParser eot

instance EotFieldList fields => EotFieldList (Either fields Void) where
  eotGetFields _ = eotGetFields (Proxy :: Proxy fields)

instance EotFieldList (f, fs) where
  eotGetFields _ = _

instance (HasEot a, EotFieldList (Eot a)) => FromRow a where
  fromRow = fromEot <$> eotFromRow
