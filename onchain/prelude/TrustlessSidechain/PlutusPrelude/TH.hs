{-# LANGUAGE NoRebindableSyntax #-}

module TrustlessSidechain.PlutusPrelude.TH (
  mkHasField,
) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Prelude

{- | Derive a HasField instance for a given data type declaration.  Works only
 for single-constructor data types and newtypes.
-}
mkHasField :: Name -> Q [Dec]
mkHasField name = do
  dataTypeInfo <- reifyDatatype name

  -- Ensure that data type contains only a single data constructor
  constructorInfo <- case datatypeCons dataTypeInfo of
    [info] -> pure info
    _ -> fail "Cannot derive HasField for data types with more than one constructor"

  -- Ensure we are working on a data type or a newtype declaration
  when (datatypeVariant dataTypeInfo `notElem` [Datatype, Newtype]) $
    fail "HasField instance can only be derived for data and newtype delcarations"

  -- Ensure record fields are explicitly named.  The field names are stripped of
  -- module prefixes so that we can use them as pattern and variable names.
  fieldNames <- case constructorVariant constructorInfo of
    RecordConstructor names -> pure (map (mkName . nameBase) names)
    _ -> fail "HasField instance can only be derived for constructors with explicit field names"

  -- Unique name of a function that is applied by modify
  fName <- newName "f"

  -- Generate fresh pattern names based on record field names
  fieldPatNames <- mapM (newName . nameBase) fieldNames

  -- A bunch of helper definitions common for all instances for a given data
  -- type
  let -- get and modify function names
      getName = mkName "get"
      modifyName = mkName "modify"

      -- parent data type Name
      conName = constructorName constructorInfo

      -- type variables passed to a type
      tyVars = map tyVarBndrName (datatypeVars dataTypeInfo)

      -- Parent data type TyCon.  Make sure to apply type variables for
      -- paremeterized data types.
      parentTyCon =
        if null tyVars
          then ConT (datatypeName dataTypeInfo)
          else
            foldl
              AppT
              (ConT (datatypeName dataTypeInfo))
              (map VarT tyVars)

      -- Takes two names.  If ther are equal constructs an application of f to
      -- that field, otherwise leaves it unchanged.  This function is intended
      -- to be partially applied and used to construct body of "modify"
      -- function.
      applyToField field field' =
        if field' == field
          then AppE (VarE fName) (VarE field)
          else VarE field'

      -- Takes two names.  If ther are equal constructs a pattern with variable
      -- name, otherwise replaces the pattern with a wildcard.  This function is
      -- intended to be partially applied and used to construct pattern in "get"
      -- function.
      mkWildCards field field' =
        if field' == field
          then VarP field
          else WildP

      -- A pattern builder function.  For "get" we need to replace all but one
      -- field with a wildcard.  For "modify" we just turn field names into
      -- patterns.  This function provides a uniform way of doing it.
      mkPat f = ParensP $ ConP conName (map f fieldPatNames)

      -- pair record fields with their pattern variables and types
      fieldsWithTypes =
        zip3 fieldNames fieldPatNames (constructorFields constructorInfo)

      -- Worker that constructs a HasField instance definition for each field.
      decls = flip map fieldsWithTypes $ \(fieldName, fieldPatName, ty) ->
        let -- field name as type literal.
            fieldLit = LitT $ StrTyLit $ nameBase fieldName
            -- type of instance: HasField "someField" FooDataType Integer
            instanceT =
              foldl1
                AppT
                [ ConT (mkName "HasField")
                , fieldLit
                , parentTyCon
                , ty
                ]
            -- body of modify function
            modifyB field =
              foldl
                AppE
                (ConE conName)
                (map (applyToField field) fieldPatNames)
         in -- Build instance declaration that contains definition of "get" and
            -- "modify", together with their corresponding INLINE pragmas.
            InstanceD
              Nothing
              []
              instanceT
              [ PragmaD (InlineP getName Inline FunLike AllPhases)
              , FunD
                  getName
                  [ Clause
                      [mkPat (mkWildCards fieldPatName)]
                      (NormalB (VarE fieldPatName))
                      []
                  ]
              , PragmaD (InlineP modifyName Inline FunLike AllPhases)
              , FunD
                  modifyName
                  [ Clause
                      [VarP fName, mkPat VarP]
                      (NormalB (modifyB fieldPatName))
                      []
                  ]
              ]

  pure decls

-- | Get name of a type variable binder
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name
