{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Mergeable.TH (mkMergeable) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype

mkMergeable :: Name -> Q [Dec]
mkMergeable name = do
  reify name
  info <- reifyDatatype name
  verifyKind info
  p1 <- newName "param1"
  p2 <- newName "param2"
  names <- getMutableFields name
  let updates = map (\n -> (n, AppE (AppE (VarE (mkName "merge1")) (AppE (VarE n) (VarE p1))) (AppE (VarE n) (VarE p2)))) names
      fun = [FunD (mkName "merge") [Clause [VarP p1, VarP p2] (NormalB (RecUpdE (VarE p1) updates))[]]]
      defaultInst  = InstanceD Nothing [] (AppT (ConT (mkName "Data.Default.Default")) (AppT (ConT name) (ConT (mkName "Maybe")))) []
  mergableInst <- instanceD (pure []) (appT (conT (mkName "Data.Mergeable.Mergeable")) (conT name)) (pure <$> fun)
  return [mergableInst, defaultInst]

getMutableFields :: Name -> Q [Name]
getMutableFields name = do
  dt <- reifyDatatype name
  let names = getFieldNames $ map constructorVariant $ datatypeCons dt
      fieldTypes = fmap appTFst $ concatMap constructorFields $ datatypeCons dt
      mName = Just $ getMName dt
      mutable = map fst $ filter ((==mName) . snd) $ zip names fieldTypes
  return mutable

verifyKind :: DatatypeInfo -> Q Name
verifyKind info
  | [SigT (VarT m) (AppT (AppT ArrowT StarT) StarT)] <- datatypeInstTypes info
  = return m
verifyKind info = error $ "'" ++ show (datatypeName info) ++ "' must be of kind '(* -> *) -> *'"

getMName :: DatatypeInfo -> Type
getMName info
  | ((KindedTV n _):_) <- datatypeVars info
  = VarT n
getMName _ = error "Provided data type do not contain m wrapper"

getFieldNames :: [ConstructorVariant] -> [Name]
getFieldNames cons
  | [RecordConstructor names] <- cons
  = names
getFieldNames _ = error "Passed type is not a record"

appTFst :: Type -> Maybe Type
appTFst (AppT x _) = Just x
appTFst _          = Nothing

