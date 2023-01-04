-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Text.Interpolation.Nyan.Lens.TH
  ( makeLenses
  )
  where

import Control.Monad (forM)
import Language.Haskell.TH

-- | Information about the record field the lenses will operate on.
type RecordFieldInfo = (Name, Strict, Type)

-- | Given a record datatype, derives lenses for all of its fields.
makeLenses :: Name -> Q [Dec]
makeLenses datatype = do
  fields <- extractRecordFields datatype
  fmap concat $ forM fields \field -> do
    sig <- deriveLensSignature datatype field
    body <- deriveLensBody field
    return [sig, body]

extractRecordFields :: Name -> Q [RecordFieldInfo]
extractRecordFields datatype = do
  let datatypeStr = nameBase datatype
  info <- reify datatype
  return $ case info of
    TyConI (DataD    _ _ _ _ [RecC _ fs] _) -> fs
    TyConI (NewtypeD _ _ _ _ (RecC _ fs) _) -> fs
    TyConI (DataD _ _ _ _ [_] _) ->
      fail $ "Can't derive lenses without record selectors: " ++ datatypeStr
    TyConI NewtypeD{} ->
      fail $ "Can't derive lenses without record selectors: " ++ datatypeStr
    TyConI TySynD{} ->
      fail $ "Can't derive lenses for type synonym: " ++ datatypeStr
    TyConI DataD{} ->
      fail $ "Can't derive lenses for a sum type: " ++ datatypeStr
    _ ->
      fail $ "Can't derive lenses for: "  ++ datatypeStr
           ++ ", type name required."

mkLensName :: Name -> Name
mkLensName = mkName . (<> "L") . nameBase

deriveLensSignature :: Name -> RecordFieldInfo -> Q Dec
deriveLensSignature datatype (fieldName, _, fieldType) =
  sigD (mkLensName fieldName)
    [t|forall f. Functor f => ($field -> f $field)
        -> $record -> f $record
    |]
  where
    field = return fieldType
    record = conT datatype

-- | Given a record field name,
-- produces a single function declaration:
-- @lensName f a = (\x -> a { field = x }) `fmap` f (field a)@
deriveLensBody :: RecordFieldInfo -> Q Dec
deriveLensBody (fieldName, _, _) = funD (mkLensName fieldName) [defLine]
  where
    a = mkName "a"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP a]
    body = [| (\x -> $(record a fieldName [|x|]))
              `fmap` $(varE f `appE` (varE fieldName `appE` varE a))
           |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]
