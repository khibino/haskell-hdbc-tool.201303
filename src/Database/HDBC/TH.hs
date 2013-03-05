{-# LANGUAGE TemplateHaskell #-}

module Database.HDBC.TH (
  ConName(conName), toConName,
  VarName(varName), toVarName,

  conCamelcaseName,
  varCamelcaseName,

  pprQ,

  fieldInfo', fieldInfo,
  defineRecordType,
  defineRecordConstructFunction,
  defineRecordDecomposeFunction,

  defineRecord,
  defineRecordDefault,

  defineConstantSql,
  defineSqlPrimarySelect,
  defineSqlPrimaryUpdate,
  defineSqlInsert
  ) where

import Data.Char (toUpper, toLower)
import Data.List (intercalate)

import Database.HDBC (SqlValue, fromSql, toSql)

import Language.Haskell.TH
  (Q, Name, mkName, runQ, Ppr, ppr,
   TypeQ, DecQ, Dec,
   Lit(StringL),
   appsE, conE, varE, listE, litE,
   listP, varP, wildP,
   conT,
   dataD, sigD, funD, valD,
   clause, normalB,
   recC, cxt, varStrictType, strictType, isStrict)
import qualified Language.Haskell.TH.PprLib as TH
import qualified Language.Haskell.TH.Syntax as TH


capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""

unCapitalize :: String -> String
unCapitalize (c:cs) = toLower c : cs
unCapitalize ""     = ""

newtype ConName = ConName { conName :: Name }
newtype VarName = VarName { varName :: Name }

toConName :: String -> ConName
toConName =  ConName . mkName . capitalize

toVarName :: String -> VarName
toVarName =  VarName . mkName . unCapitalize

nameChars :: String
nameChars =  ['0' .. '9'] ++ ['A' .. 'Z'] ++  ['a' .. 'z']

splitForName :: String -> [String]
splitForName str
  | rest /= [] = tk : splitForName (tail rest)
  | otherwise  = [tk]
  where
    (tk, rest) = span (`elem` nameChars) str

camelcaseUpper :: String -> String
camelcaseUpper =  concat . map capitalize . splitForName

-- camelcaseLower :: String -> String
-- camelcaseLower =  unCapitalize . camelcaseUpper

conCamelcaseName :: String -> ConName
conCamelcaseName =  toConName . camelcaseUpper

varCamelcaseName :: String -> VarName
varCamelcaseName =  toVarName . camelcaseUpper

pprQ :: (Functor m, TH.Quasi m, Ppr a) => Q a -> m TH.Doc
pprQ =  fmap ppr . runQ

fieldInfo' :: String
           -> TypeQ
           -> (VarName, (String, TypeQ)) -- ^ (fieldVarName, (fieldInSQL, fieldTypeInTable))
fieldInfo' n t = (varCamelcaseName n, (n, t))

fieldInfo :: String
          -> TypeQ
          -> (VarName, TypeQ) -- ^ (fieldInSQL, fieldTypeInTable)
fieldInfo n' t' = (v, t) where
  (v, (_n, t)) = fieldInfo' n' t'


defineRecordType :: ConName            -- ^ Name of the data type of table record type.
                 -> [(VarName, TypeQ)] -- ^ List of fields in the table. Must be legal, properly cased record fields.
                 -> DecQ               -- ^ The data type record declaration.
defineRecordType typeName' fields = do
  let typeName = conName typeName'
  dataD (cxt []) typeName [] [recC typeName (map fld fields)] []
  where
    fld (n, tq) = varStrictType (varName n) (strictType isStrict tq)

defineRecordConstructFunction :: VarName   -- ^ Name of record construct function.
                              -> ConName   -- ^ Name of record type.
                              -> [VarName] -- ^ List of field names of record.
                              -> Q [Dec]   -- ^ Declaration of record construct function from SqlValues.
defineRecordConstructFunction funName' typeName' fields = do
  let funName = varName funName'
      typeName = conName typeName'
      width = length fields
      names = map (mkName . ('f':) . show) [1 .. width]
      fromSqlE n = [| fromSql $(varE n) |]
  sig <- sigD funName [t| [SqlValue] -> $(conT typeName) |]
  var <- funD funName [ clause
                        [listP (map varP names)]
                        (normalB . appsE $ conE typeName : map fromSqlE names)
                        [],
                        clause [wildP]
                        (normalB [| error "Generated code of 'defineRecordConstructFunction': Something wrong!" |])
                        [] ]
  return [sig, var]

defineRecordDecomposeFunction :: VarName   -- ^ Name of record decompose function.
                              -> ConName   -- ^ Name of record type.
                              -> [VarName] -- ^ List of field names of record.
                              -> Q [Dec]   -- ^ Declaration of record construct function from SqlValues.
defineRecordDecomposeFunction funName' typeName' fields = do
  let funName = varName funName'
      typeName = conName typeName'
      accessors = map (varE . varName) fields
      recVar = mkName "rec"
  sig <- sigD funName [t| $(conT typeName) -> [SqlValue] |]
  var <- funD funName [ clause [varP recVar]
                        (normalB . listE $ map (\a -> [| toSql ($a $(varE recVar)) |]) accessors)
                        [] ]
  return [sig, var]

defineRecord :: VarName
             -> VarName
             -> ConName
             -> [(VarName, TypeQ)]
             -> Q [Dec]
defineRecord cF dF tyC fields = do
  typ  <- defineRecordType tyC fields
  let names = map fst fields
  fromSQL <- defineRecordConstructFunction cF tyC names
  toSQL   <- defineRecordDecomposeFunction dF tyC names
  return $ typ : fromSQL ++ toSQL

defineRecordDefault :: String
                    -> [(String, TypeQ)]
                    -> Q [Dec]
defineRecordDefault table fields =
  defineRecord
  (varCamelcaseName $ "fromSql" ++ tn)
  (varCamelcaseName $ "toSql" ++ tn)
  (conCamelcaseName tn) fields'
  where
    tn = camelcaseUpper table
    fields' = map (\(s, t) -> (varCamelcaseName s, t)) fields

commaed :: [String] -> String
commaed = intercalate ", "

sqlCat :: [String] -> String
sqlCat = intercalate " "

sqlEq :: String -> String -> String
sqlEq a b = sqlCat [a, "=", b]

pfEq :: String -> String
pfEq = (`sqlEq` "?")

defineConstantSql :: VarName -> String -> Q [Dec]
defineConstantSql name' sqlStr = do
  let name = varName name'
  sig <- sigD name [t| String |]
  var <- valD (varP name)
         (normalB . litE . StringL $ sqlStr)
         []
  return [sig, var]


defineSqlPrimarySelect :: VarName -> String -> String -> [String] -> Q [Dec]
defineSqlPrimarySelect name' table pkey fields =
  defineConstantSql name'
  $ sqlCat ["SELECT", commaed fields, "FROM", table, "WHERE", pfEq pkey]

defineSqlPrimaryUpdate :: VarName -> String -> String -> [String] -> Q [Dec]
defineSqlPrimaryUpdate name' table pkey fields =
  defineConstantSql name'
  $ sqlCat ["UPDATE", table,
            "SET", commaed . map pfEq . filter (/= pkey) $ fields,
            "WHERE", pfEq pkey]

defineSqlInsert :: VarName -> String -> [String] -> Q [Dec]
defineSqlInsert name' table fields = do
  defineConstantSql name'
  $ sqlCat ["INSERT", "INTO", table, "(", commaed fields, ")",
            "VALUES", "(", commaed (replicate (length fields) "?") , ")"]
