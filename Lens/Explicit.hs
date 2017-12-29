-- |
-- Module      : Lens.Explicit
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE UnicodeSyntax   #-}


module Lens.Explicit (
                     -- * Getters
                       to, Getter, AGetter, (^.)
                     -- * Setters
                     , sets, Setter, ASetter, (%~), (.~), Setter'
                     -- * Lenses
                     , lens, Lens, ALens, (%%~), Lens'
                     -- * Prisms
                     , prism, Prism, APrism, matching, Prism'
                     -- * Isomorphisms
                     , iso, Iso, AnIso, from, under, Iso'
                     -- * Folds
                     , folded, Fold, AFold, foldMapOf
                     -- * Traversals
                     , traversed, Traversal, ATraversal, traverseOf, Traversal'
                     ) where

import qualified Lens.Explicit.Core as Ж



infixl 8 ^.

(^.) :: 𝑠 -> AGetter 𝑠 𝑎 -> 𝑎
s ^. Ж.Getter f = f s

to :: (𝑠 -> 𝑎) -> Getter 𝑠 𝑎
to = Ж.to

-- | Getters are basically just functions: accessors which can read a field (type @𝑎@)
--   of some data structure (type @𝑠@), but not write back anything to the structure.
type Getter 𝑠 𝑎 = Ж.Getter 𝑠 𝑠 𝑎 𝑠

-- | A getter that may also have additional capabilities, e.g. a 'Lens'.
type AGetter 𝑠 𝑎 = Ж.AGetter 𝑠 𝑎


infixr 4 %~, .~

(%~) :: ASetter 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑏) -> 𝑠 -> 𝑡
Ж.Setter f %~ m = f m

(.~) :: ASetter 𝑠 𝑡 𝑎 𝑏 -> 𝑏 -> 𝑠 -> 𝑡
a .~ b = a %~ const b

sets :: ((𝑎 -> 𝑏) -> 𝑠 -> 𝑡) -> Setter 𝑠 𝑡 𝑎 𝑏
sets = Ж.sets

-- | Setters are accessors that can write/manipulate a field (type @𝑎@)
--   of a data structure (type @𝑠@), but not retrieve any results.
--
--   The manipulation might result in a type @𝑏@ for the field different from
--   the original @𝑎@, in that case, the data structure will likewise change
--   change its type from @𝑠@ to @𝑡@.
type Setter 𝑠 𝑡 𝑎 𝑏 = Ж.Setter 𝑠 𝑡 𝑎 𝑏

-- | A setter that may also have additional capabilities, e.g. a 'Lens'.
type ASetter 𝑠 𝑡 𝑎 𝑏 = Ж.ASetter 𝑠 𝑡 𝑎 𝑏

type Setter' 𝑠 𝑎 = Setter 𝑠 𝑠 𝑎 𝑎


infixr 4 %%~

(%%~) :: Functor 𝑓 => ALens 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡
(%%~) (Ж.Lens f φ) τ s = fmap (φ s) . τ $ f s

lens :: (𝑠 -> 𝑎) -> (𝑠 -> 𝑏 -> 𝑡) -> Lens 𝑠 𝑡 𝑎 𝑏
lens = Ж.lens

-- | Lenses combine the capabilities of 'Getter' and 'Setter' – they have “read and
--   write permission”, i.e. you can use them with the '^.' as well as '.~' and '%~'
--   operators.
--
--   This is the standard type of record-field accessor.
type Lens 𝑠 𝑡 𝑎 𝑏 = Ж.Lens 𝑠 𝑡 𝑎 𝑏

-- | A lens that may also have additional capabilities, e.g. an 'Iso'.
type ALens 𝑠 𝑡 𝑎 𝑏 = Ж.ALens 𝑠 𝑡 𝑎 𝑏

type Lens' 𝑠 𝑎 = Lens 𝑠 𝑠 𝑎 𝑎


prism :: (𝑏 -> 𝑡) -> (𝑠 -> Either 𝑡 𝑎) -> Prism 𝑠 𝑡 𝑎 𝑏
prism = Ж.prism

matching :: APrism 𝑠 𝑡 𝑎 𝑏 -> 𝑠 -> Either 𝑡 𝑎
matching (Ж.Prism _ f) = f

-- | Prisms are the categorical dual of lenses: whilst a lens /focuses/ in on a field
--   of a record structure (i.e. of a product type), a prism /distinguishes/ constructors
--   of an alternative (i.e. of a sum type).
type Prism 𝑠 𝑡 𝑎 𝑏 = Ж.Prism 𝑠 𝑡 𝑎 𝑏

-- | A prism that may also have additional capabilities, e.g. an 'Iso'.
type APrism 𝑠 𝑡 𝑎 𝑏 = Ж.APrism 𝑠 𝑡 𝑎 𝑏

type Prism' 𝑠 𝑎 = Prism 𝑠 𝑠 𝑎 𝑎


under :: AnIso 𝑠 𝑡 𝑎 𝑏 -> (𝑡 -> 𝑠) -> 𝑏 -> 𝑎
under (Ж.Iso f φ) g = f . g . φ

from :: AnIso 𝑠 𝑡 𝑎 𝑏 -> Iso 𝑏 𝑎 𝑡 𝑠
from (Ж.Iso f φ) = iso φ f

iso :: (𝑠 -> 𝑎) -> (𝑏 -> 𝑡) -> Iso 𝑠 𝑡 𝑎 𝑏
iso = Ж.iso

-- | Isomorphisms are 1-1 mappings. This can be seen as a 'Lens' which focuses on
--   a field that contains the entire information of the data structure, or as a
--   prism that distinguishes the only constructor available.
type Iso 𝑠 𝑡 𝑎 𝑏 = Ж.Iso 𝑠 𝑡 𝑎 𝑏

-- | An isomorphism that could also have additional capabilities. (This is somewhat
--   theoretical, since isomorphism is already the most powerful relation we describe.)
type AnIso 𝑠 𝑡 𝑎 𝑏 = Ж.AnIso 𝑠 𝑡 𝑎 𝑏

type Iso' 𝑠 𝑎 = Iso 𝑠 𝑠 𝑎 𝑎


traverseOf :: Applicative 𝑓 => ATraversal 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡
traverseOf (Ж.Traversal y) = y

traversed :: (∀ 𝑓 . Applicative 𝑓 => (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡) -> Traversal 𝑠 𝑡 𝑎 𝑏
traversed = Ж.traversed

-- | Traversals can 'Fold' over the fields of a data structure, and additionally
--   reconstruct the structure with modified fields.
type Traversal 𝑠 𝑡 𝑎 𝑏 = Ж.Traversal 𝑠 𝑡 𝑎 𝑏

-- | A traversal that may also have additional capabilities, e.g. a 'Lens' or 'Prism'.
type ATraversal 𝑠 𝑡 𝑎 𝑏 = Ж.ATraversal 𝑠 𝑡 𝑎 𝑏

type Traversal' 𝑠 𝑎 = Traversal 𝑠 𝑠 𝑎 𝑎


foldMapOf :: Monoid 𝑟 => AFold 𝑠 𝑎 -> (𝑎 -> 𝑟) -> 𝑠 -> 𝑟
foldMapOf (Ж.Fold y) = y

folded :: Foldable 𝑓 => Fold (𝑓 𝑎) 𝑎
folded = Ж.folded

-- | Folds access fields that may occur multiple times in the data structure,
--   or not at all, such as the elements of a list. Like 'Getter', they don't
--   have “write permission”.
type Fold 𝑠 𝑎 = Ж.Fold 𝑠 𝑠 𝑎 𝑠

-- | A fold that may also have additional capabilities, e.g. a 'Getter' or 'Traversal'.
type AFold 𝑠 𝑎 = Ж.AFold 𝑠 𝑠 𝑎 𝑠

