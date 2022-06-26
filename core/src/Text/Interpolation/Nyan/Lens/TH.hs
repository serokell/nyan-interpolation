-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Text.Interpolation.Nyan.Lens.TH
  ( makeLenses
  )
  where

import Control.Monad ((<=<))
import Language.Haskell.TH

-- | Information about the record field the lenses will operate on.
type RecordFieldInfo = (Name, Strict, Type)

-- | Given a record datatype, derives lenses for all of its fields.
makeLenses :: Name -> Q [Dec]
makeLenses = mapM deriveLens <=< extractRecordFields

extractRecordFields :: Name -> Q [RecordFieldInfo]
extractRecordFields datatype = do
  let datatypeStr = nameBase datatype
  info <- reify datatype
  return $ case info of
    TyConI (DataD    _ _ _ _ [RecC _ fs] _) -> fs
    TyConI (NewtypeD _ _ _ _ (RecC _ fs) _) -> fs
    TyConI (DataD _ _ _ _ [_] _) ->
      error $ "Can't derive lenses without record selectors: " ++ datatypeStr
    TyConI NewtypeD{} ->
      error $ "Can't derive lenses without record selectors: " ++ datatypeStr
    TyConI TySynD{} ->
      error $ "Can't derive lenses for type synonym: " ++ datatypeStr
    TyConI DataD{} ->
      error $ "Can't derive lenses for a sum type: " ++ datatypeStr
    _ ->
      error $ "Can't derive lenses for: "  ++ datatypeStr
           ++ ", type name required."

-- | Given a record field name,
-- produces a single function declaration:
-- @lensName f a = (\x -> a { field = x }) `fmap` f (field a)@
deriveLens :: RecordFieldInfo -> Q Dec
deriveLens (fieldName, _, _) = funD lensName [defLine]
  where
    lensName = mkName $ (nameBase fieldName) <> "L"
    a = mkName "a"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP a]
    body = [| (\x -> $(record a fieldName [|x|]))
              `fmap` $(appE (varE f) (appE (varE fieldName) (varE a)))
           |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]
