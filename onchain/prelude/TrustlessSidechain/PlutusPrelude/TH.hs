{-# LANGUAGE NoRebindableSyntax #-}

module TrustlessSidechain.PlutusPrelude.TH (
  makeUnsafeNewtypes,
  makeUnsafeGetters,
) where

import Control.Monad
import Data.List (isSuffixOf)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Prelude

-- `makeUnsafeNewtypes` generates a wrapper for a type's
-- `BuiltinData` representation.
-- It is intended to be used inside `TrustlessSidechain.Types.Unsafe` only.
--
-- Example:
--   module Safe where
--   data Foo = Bar | Fizz Integer
--   module Unsafe where
--   makeUnsafeNewtypes ''Safe.Foo
-- Generates:
--   newtype Foo = Foo {unFoo :: BuiltinData}
--     deriving Eq
--   instance Packable Foo where
--     {-# INLINE wrap #-}
--     wrap = Foo
--     {-# INLINE unwrap #-}
--     unwrap = unFoo
--   instance Codable Safe.Foo Foo

makeUnsafeNewtypes :: Name -> Q [Dec]
makeUnsafeNewtypes name = do
  let rawTypeName = getNameUnqualified name

  let builtinDataName = mkName "BuiltinData"
  let newtypeName = mkName rawTypeName
  let unNewtypeName = mkName $ "un" <> rawTypeName

  let decNewtype =
        NewtypeD
          []
          newtypeName
          []
          Nothing
          ( RecC
              newtypeName
              [
                ( unNewtypeName
                , Bang NoSourceUnpackedness NoSourceStrictness
                , ConT builtinDataName
                )
              ]
          )
          [DerivClause Nothing [ConT $ mkName "Eq"]]
  let packableInstanceT =
        foldl1
          AppT
          [ ConT $ mkName "Packable"
          , ConT newtypeName
          ]
  let packableInstance =
        InstanceD
          Nothing
          []
          packableInstanceT
          [ PragmaD (InlineP (mkName "wrap") Inline FunLike AllPhases)
          , FunD
              (mkName "wrap")
              [Clause [] (NormalB (ConE newtypeName)) []]
          , PragmaD (InlineP (mkName "unwrap") Inline FunLike AllPhases)
          , FunD
              (mkName "unwrap")
              [Clause [] (NormalB (VarE unNewtypeName)) []]
          ]
  let codableInstance =
        InstanceD
          Nothing
          []
          ( foldl1
              AppT
              [ ConT $ mkName "Codable"
              , ConT name
              , ConT newtypeName
              ]
          )
          []
  return [decNewtype, packableInstance, codableInstance]

-- `makeUnsafeGetters` generates getter functions for wrapper types generated
-- by `makeUnsafeNewtypes`.
-- It is intended to be used inside `TrustlessSidechain.Types.Unsafe` only.
--
-- Example:
--   data Foo = Bar | Fizz Integer
--   makeUnsafeGetters ''Foo
-- Generates:
--   {-# INLINE isBar #-}
--   isBar :: Foo -> Bool
--   isBar x = (isNthCtorOf 0) (unwrap x)
--   {-# INLINE getFizz #-}
--   getFizz :: Foo -> Maybe Integer
--   getFizz x = (fmap wrap) ((nthCtorOf 1) (unwrap x))

-- Example:
--   data Record = Record
--     { recordFoo :: Integer
--     , recordBar :: [Integer]
--     }
--   makeUnsafeGetters ''Record
-- Generates:
--   {-# INLINE recordFoo #-}
--   recordFoo :: Record -> Integer
--   recordFoo x = wrap ((nthFieldOf 0) (unwrap x))
--   {-# INLINE recordBar #-}
--   recordBar :: Record -> [Integer]
--   recordBar x
--     = (fmap wrap)
--         (Builtins.unsafeDataAsList ((nthFieldOf 1) (unwrap x)))

makeUnsafeGetters :: Name -> Q [Dec]
makeUnsafeGetters name = do
  DatatypeInfo {datatypeName, datatypeCons} <- reifyDatatype name
  let newtypeName = makeNameUnqualified datatypeName
  case datatypeCons of
    [ConstructorInfo {constructorVariant, constructorFields}] ->
      return case constructorVariant of
        RecordConstructor fieldNames ->
          zipWithIndex (zip (map makeNameUnqualified fieldNames) constructorFields)
            >>= mkGetter newtypeName
        _ -> []
    sumCtors -> do
      join <$> forM (zipWithIndex sumCtors) \(ix, ConstructorInfo {constructorFields, constructorName}) -> do
        case constructorFields of
          [] -> return $ mkVariantQuery newtypeName ix constructorName
          [ctorField] -> return $ mkVariantGetter newtypeName ix constructorName ctorField
          -- TODO implement this as necessary
          _ -> fail "Sum type constructors are only supported with less than 2 arguments"
  where
    zipWithIndex = zip [0 ..]

    mkGetter :: Name -> (Int, (Name, Type)) -> [Dec]
    mkGetter newtypeName (ix, (fieldName, fieldType)) = getterDec =<< targetTypeL
      where
        targetTypeL = case fieldType of
          ConT n -> [(ConT (makeNameUnqualified n), getterBodySimple)]
          AppT ListT (ConT n) -> [(AppT ListT (ConT (makeNameUnqualified n)), getterBodyList)]
          AppT (ConT m) (ConT n) | show m == "GHC.Maybe.Maybe" -> [(AppT (ConT m) (ConT (makeNameUnqualified n)), getterBodyMaybe)]
          AppT (AppT (ConT m) (ConT k)) (ConT v) | isSuffixOf "Map" (show m) -> [(AppT ListT (AppT (AppT (TupleT 2) (ConT (makeNameUnqualified k))) (ConT (makeNameUnqualified v))), getterBodyMap)]
          _ -> [] -- TODO support for fields with more complex types should be implemented as needed

        -- (nthFieldOf ix) (unwrap x)
        nthFieldOf_ix_unwrap_x =
          (fun "nthFieldOf" $$ integerLit ix) $$ fun "unwrap" $$ var "x"

        -- wrap ((nthFieldOf ix) (unwrap x))
        getterBodySimple =
          fun "wrap" $$ nthFieldOf_ix_unwrap_x

        -- (fmap wrap) (Builtins.unsafeDataAsList ((nthFieldOf ix) (unwrap x)))
        getterBodyList =
          (fun "fmap" $$ fun "wrap") $$ fun "Builtins.unsafeDataAsList" $$ nthFieldOf_ix_unwrap_x

        -- (fmap wrap) (unsafeDataAsMaybe (unwrap x))
        getterBodyMaybe =
          (fun "fmap" $$ fun "wrap") $$ fun "unsafeDataAsMaybe" $$ fun "unwrap" $$ var "x"

        getterBodyMap =
          (fun "fmap" $$ (LamE [TupP [VarP (mkName "a"), VarP (mkName "b")]] (TupE [Just (fun "wrap" $$ (VarE (mkName "a"))), Just (fun "wrap" $$ (VarE (mkName "b")))]))) $$ fun "Builtins.unsafeDataAsMap" $$ nthFieldOf_ix_unwrap_x

        getterDec :: (Type, Exp) -> [Dec]
        getterDec (targetType, getterBody) =
          [ PragmaD (InlineP fieldName Inline FunLike AllPhases)
          , SigD fieldName (AppT (AppT ArrowT (ConT newtypeName)) targetType)
          , FunD
              fieldName
              [ Clause
                  [VarP (mkName "x")]
                  (NormalB getterBody)
                  []
              ]
          ]

    mkVariantQuery :: Name -> Int -> Name -> [Dec]
    mkVariantQuery newtypeName ix ctorName =
      [ PragmaD (InlineP queryName Inline FunLike AllPhases)
      , SigD queryName (AppT (AppT ArrowT (ConT newtypeName)) (ConT (mkName "Bool")))
      , FunD
          queryName
          [ Clause
              [VarP (mkName "x")]
              ( NormalB $
                  -- (isNthCtorOf 0) (unwrap x)
                  (fun "isNthCtorOf" $$ integerLit ix) $$ fun "unwrap" $$ var "x"
              )
              []
          ]
      ]
      where
        queryName = mkName $ "is" <> getNameUnqualified ctorName

    mkVariantGetter :: Name -> Int -> Name -> Type -> [Dec]
    mkVariantGetter newtypeName ix ctorName fieldType =
      variantGetterDec =<< targetTypeL
      where
        targetTypeL = case fieldType of
          ConT n -> [(ConT (makeNameUnqualified n), variantGetterBodySimple)]
          _ -> [] -- TODO support for ctors with more complex types should be implemented as needed

        -- (fmap wrap) ((nthCtorOf ix) (unwrap x))
        variantGetterBodySimple =
          (fun "fmap" $$ fun "wrap") $$ (fun "nthCtorOf" $$ integerLit ix) $$ fun "unwrap" $$ var "x"

        variantGetterDec :: (Type, Exp) -> [Dec]
        variantGetterDec (targetType, getterBody) =
          [ PragmaD (InlineP getterName Inline FunLike AllPhases)
          , SigD getterName (AppT (AppT ArrowT (ConT newtypeName)) (AppT (ConT (mkName "Maybe")) targetType))
          , FunD
              getterName
              [ Clause
                  [VarP (mkName "x")]
                  (NormalB getterBody)
                  []
              ]
          ]
          where
            getterName = mkName $ "get" <> getNameUnqualified ctorName

-- TH helpers

fun :: String -> Exp
fun = VarE . mkName

var :: String -> Exp
var = VarE . mkName

integerLit :: (Integral a) => a -> Exp
integerLit = LitE . IntegerL . fromIntegral

infixr 0 $$
($$) :: Exp -> Exp -> Exp
($$) = AppE

makeNameUnqualified :: Name -> Name
makeNameUnqualified = mkName . getNameUnqualified

getNameUnqualified :: Name -> String
getNameUnqualified (Name (OccName n) _) = n
