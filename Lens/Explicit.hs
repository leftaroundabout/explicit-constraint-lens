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
                       (^.), to, Getter, AGetter
                     -- * Setters
                     , (%~), (.~), sets, Setter, ASetter
                     -- * Lenses
                     , (%%~), lens, Lens, ALens
                     -- * Prisms
                     , matching, prism, Prism, APrism
                     -- * Isomorphisms
                     , from, iso, under, Iso, AnIso
                     -- * Folds
                     , foldMapOf, folded, Fold, AFold
                     -- * Traversals
                     , traverseOf, traversed, Traversal, ATraversal
                     ) where

import qualified Lens.Explicit.Core as Ж



infixl 8 ^.

(^.) :: s -> AGetter s a -> a
s ^. Ж.Getter f = f s

to :: (s -> a) -> Getter s a
to = Ж.to

-- | Getters are basically just functions: accessors which can read a field (type @a@)
--   of some data structure (type @s@), but not write back anything to the structure.
type Getter s a = Ж.Getter s s a s

-- | A getter that may also have additional capabilities, e.g. a 'Lens'.
type AGetter s a = Ж.AGetter s a


infixr 4 %~, .~

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
Ж.Setter f %~ m = f m

(.~) :: ASetter s t a b -> b -> s -> t
a .~ b = a %~ const b

sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets = Ж.sets

-- | Setters are accessors that can write/manipulate a field (type @a@)
--   of a data structure (type @s@), but not retrieve any results.
--
--   The manipulation might result in a type @b@ for the field different from
--   the original @a@, in that case, the data structure will likewise change
--   change its type from @s@ to @t@.
type Setter s t a b = Ж.Setter s t a b

-- | A setter that may also have additional capabilities, e.g. a 'Lens'.
type ASetter s t a b = Ж.ASetter s t a b


(%%~) :: Functor f => ALens s t a b -> (a -> f b) -> s -> f t
(%%~) (Ж.Lens f φ) τ s = fmap (φ s) . τ $ f s

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens = Ж.lens

-- | Lenses combine the capabilities of 'Getter' and 'Setter' – they have “read and
--   write permission”, i.e. you can use them with the '^.' as well as '.~' and '%~'
--   operators.
--
--   This is the standard type of record-field accessor.
type Lens s t a b = Ж.Lens s t a b

-- | A lens that may also have additional capabilities, e.g. an 'Iso'.
type ALens s t a b = Ж.ALens s t a b


prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism = Ж.prism

matching :: APrism s t a b -> s -> Either t a
matching (Ж.Prism _ f) = f

-- | Prisms are the categorical dual of lenses: whilst a lens /focuses/ in on a field
--   of a record structure (i.e. of a product type), a prism /distinguishes/ constructors
--   of an alternative (i.e. of a sum type).
type Prism s t a b = Ж.Prism s t a b

-- | A prism that may also have additional capabilities, e.g. an 'Iso'.
type APrism s t a b = Ж.APrism s t a b


under :: AnIso s t a b -> (t -> s) -> b -> a
under (Ж.Iso f φ) g = f . g . φ

from :: AnIso s t a b -> Iso b a t s
from (Ж.Iso f φ) = iso φ f

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = Ж.iso

-- | Isomorphisms are 1-1 mappings. This can be seen as a 'Lens' which focuses on
--   a field that contains the entire information of the data structure, or as a
--   prism that distinguishes the only constructor available.
type Iso s t a b = Ж.Iso s t a b

-- | An isomorphism that could also have additional capabilities. (This is somewhat
--   theoretical, since isomorphism is already the most powerful relation we describe.)
type AnIso s t a b = Ж.AnIso s t a b


traverseOf :: Applicative f => ATraversal s t a b -> (a -> f b) -> s -> f t
traverseOf (Ж.Traversal y) = y

traversed :: (∀ f . Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b
traversed = Ж.traversed

-- | Traversals can 'Fold' over the fields of a data structure, and additionally
--   reconstruct the structure with modified fields.
type Traversal s t a b = Ж.Traversal s t a b

-- | A traversal that may also have additional capabilities, e.g. a 'Lens' or 'Prism'.
type ATraversal s t a b = Ж.ATraversal s t a b


foldMapOf :: Monoid r => AFold s a -> (a -> r) -> s -> r
foldMapOf (Ж.Fold y) = y

folded :: Foldable f => Fold (f a) a
folded = Ж.folded

-- | Folds access fields that may occur multiple times in the data structure,
--   or not at all, such as the elements of a list. Like 'Getter', they don't
--   have “write permission”.
type Fold s a = Ж.Fold s s a s

-- | A fold that may also have additional capabilities, e.g. a 'Getter' or 'Traversal'.
type AFold s a = Ж.AFold s s a s

