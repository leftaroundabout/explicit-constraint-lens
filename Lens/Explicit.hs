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

{-# LANGUAGE Safe #-}


module Lens.Explicit (
                     -- * Lenses and other optics
                     -- ** Getters
                       to, Getter, AGetter, (^.)
                     -- ** Setters
                     , sets, Setter, ASetter, (%~), (.~), Setter'
                     -- ** Lenses
                     , lens, Lens, ALens, (%%~), Lens'
                     -- ** Prisms
                     , prism, Prism, APrism, matching, Prism'
                     -- ** Reviews
                     , unto, Review, AReview, re
                     -- ** Isomorphisms
                     , iso, Iso, AnIso, from, under, Iso'
                     -- ** Equalities
                     , Cat.id, Equality, AnEquality, Equality', simple
                     -- ** Folds
                     , folded, Fold, AFold, foldMapOf
                     -- *** Minimum-length 1
                     , Fold1, AFold1, foldMap1Of
                     -- ** Traversals
                     , traversal, traversed, Traversal, ATraversal, traverseOf, Traversal'
                     -- *** Minimum-length 1 
                     , traversal1By, Traversal1, ATraversal1, traverse1ByOf, Traversal1'
                     -- * Composition
                     -- $composInfo
                     , (Cat..), (&)
                     -- * Weakening hierarchy
                     , weaken
                     , Ж.FromIso, Ж.FromLens, Ж.FromPrism, Ж.FromTraversal1, Ж.FromTraversal
                     , Ж.FromGetter, Ж.FromReview, Ж.FromFold1, Ж.FromFold
                     , Ж.IsoTrait, Ж.LensTrait, Ж.PrismTrait, Ж.Traversal1Trait, Ж.TraversalTrait
                     , Ж.GetterTrait, Ж.ReviewTrait, Ж.Fold1Trait, Ж.FoldTrait
                     ) where

import qualified Lens.Explicit.Core as Ж
import Lens.Explicit.Core (OpticC(..))
import Prelude hiding (id, (.))
import Control.Category as Cat
import Data.Function hiding (id, (.))

import Data.Semigroup


infixl 8 ^.

(^.) :: 𝑠 -> AGetter 𝑠 𝑡 𝑎 𝑏 -> 𝑎
s ^. Ж.Equality = s
s ^. OpticC (Ж.Getter f) = f s

to :: (𝑠 -> 𝑎) -> Getter 𝑠 𝑎
to = OpticC . Ж.to

-- | Getters are basically just functions: accessors which can read a field (type @𝑎@)
--   of some data structure (type @𝑠@), but not write back anything to the structure.
type Getter 𝑠 𝑎 = ∀ c . Ж.FromGetter c => Ж.Optic c 𝑠 𝑠 𝑎 𝑎

-- | A getter that may also have additional capabilities, e.g. a 'Lens'.
type AGetter 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.GetterTrait 𝑠 𝑡 𝑎 𝑏


infixr 4 %~, .~

(%~) :: ASetter 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑏) -> 𝑠 -> 𝑡
Ж.Equality %~ m = m
OpticC (Ж.Setter f) %~ m = f m

(.~) :: ASetter 𝑠 𝑡 𝑎 𝑏 -> 𝑏 -> 𝑠 -> 𝑡
a .~ b = a %~ const b

sets :: ((𝑎 -> 𝑏) -> 𝑠 -> 𝑡) -> Setter 𝑠 𝑡 𝑎 𝑏
sets = OpticC . Ж.sets

-- | Setters are accessors that can write/manipulate a field (type @𝑎@)
--   of a data structure (type @𝑠@), but not retrieve any results. The
--   field may turn up multiple times in the structure, in which case all
--   of them are manipulated independently.
--
--   The manipulation can result in a type @𝑏@ for the field different from
--   the original @𝑎@; in that case, the data structure will likewise change
--   change its type from @𝑠@ to @𝑡@.
type Setter 𝑠 𝑡 𝑎 𝑏 = ∀ c . Ж.FromSetter c => Ж.Optic c 𝑠 𝑡 𝑎 𝑏

-- | A setter that may also have additional capabilities, e.g. a 'Lens'.
type ASetter 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.SetterTrait 𝑠 𝑡 𝑎 𝑏

type Setter' 𝑠 𝑎 = Setter 𝑠 𝑠 𝑎 𝑎


infixr 4 %%~

(%%~) :: Functor 𝑓 => ALens 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡
(%%~) Ж.Equality τ s = τ s
(%%~) (OpticC (Ж.Lens f φ)) τ s = fmap (φ s) . τ $ f s

lens :: (𝑠 -> 𝑎) -> (𝑠 -> 𝑏 -> 𝑡) -> Lens 𝑠 𝑡 𝑎 𝑏
lens f g = OpticC $ Ж.lens f g

-- | Lenses combine the capabilities of 'Getter' and 'Setter' – they have “read and
--   write permission”, i.e. you can use them with the '^.' as well as '.~' and '%~'
--   operators.
--
--   This is the standard type of record-field accessor.
type Lens 𝑠 𝑡 𝑎 𝑏 = ∀ c . Ж.FromLens c => Ж.Optic c 𝑠 𝑡 𝑎 𝑏

-- | A lens that may also have additional capabilities, e.g. an 'Iso'.
type ALens 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.LensTrait 𝑠 𝑡 𝑎 𝑏

type Lens' 𝑠 𝑎 = Lens 𝑠 𝑠 𝑎 𝑎


prism :: (𝑏 -> 𝑡) -> (𝑠 -> Either 𝑡 𝑎) -> Prism 𝑠 𝑡 𝑎 𝑏
prism f g = OpticC $ Ж.prism f g

matching :: APrism 𝑠 𝑡 𝑎 𝑏 -> 𝑠 -> Either 𝑡 𝑎
matching Ж.Equality = Right
matching (OpticC (Ж.Prism _ f)) = f

-- | Prisms are the categorical dual of lenses: whilst a lens /focuses/ in on a field
--   of a record structure (i.e. of a product type), a prism /distinguishes/ constructors
--   of an alternative (i.e. of a sum type).
type Prism 𝑠 𝑡 𝑎 𝑏 = ∀ c . Ж.FromPrism c => Ж.Optic c 𝑠 𝑡 𝑎 𝑏

-- | A prism that may also have additional capabilities, e.g. an 'Iso'.
type APrism 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.PrismTrait 𝑠 𝑡 𝑎 𝑏

type Prism' 𝑠 𝑎 = Prism 𝑠 𝑠 𝑎 𝑎


unto :: (𝑏 -> 𝑡) -> Review 𝑡 𝑏
unto = OpticC . Ж.unto

re :: AReview 𝑠 𝑡 𝑎 𝑏 -> Getter 𝑏 𝑡
re Ж.Equality = Ж.Equality
re (OpticC (Ж.Review f)) = OpticC $ Ж.to f

-- | Reviews are basically like constructors in languages without pattern matching:
--   /prisms without read permission/. Because such a constructor is just a function,
--   and getters are functions too, you can also consider a review as a “reverse 'Getter'”.
type Review 𝑡 𝑏 = ∀ c . Ж.FromReview c => Ж.Optic c 𝑡 𝑡 𝑏 𝑏

-- | A review that may also have additional capabilities, e.g. a 'Prism'.
type AReview 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.ReviewTrait 𝑠 𝑡 𝑎 𝑏


under :: AnIso 𝑠 𝑡 𝑎 𝑏 -> (𝑡 -> 𝑠) -> 𝑏 -> 𝑎
under Ж.Equality g = g
under (OpticC (Ж.Iso f φ)) g = f . g . φ

from :: AnIso 𝑠 𝑡 𝑎 𝑏 -> Iso 𝑏 𝑎 𝑡 𝑠
from Ж.Equality = Ж.Equality
from (OpticC (Ж.Iso f φ)) = iso φ f

iso :: (𝑠 -> 𝑎) -> (𝑏 -> 𝑡) -> Iso 𝑠 𝑡 𝑎 𝑏
iso f g = OpticC $ Ж.iso f g

-- | Isomorphisms are 1-1 mappings. This can be seen as a 'Lens' which focuses on
--   a field that contains the entire information of the data structure, or as a
--   prism that distinguishes the only constructor available. Typically, this is
--   used for @newtype@s, which have exactly one constructor containing one field.
type Iso 𝑠 𝑡 𝑎 𝑏 = ∀ c . Ж.FromIso c => Ж.Optic c 𝑠 𝑡 𝑎 𝑏

-- | An isomorphism that could also have additional capabilities, i.e. either
 --  an 'Iso' or 'Equality'.
type AnIso 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.IsoTrait 𝑠 𝑡 𝑎 𝑏

type Iso' 𝑠 𝑎 = Iso 𝑠 𝑠 𝑎 𝑎


-- | Equalities are simply witnesses that nothing nontrivial happens. I.e. they are
--   always identity isomorphisms.
type Equality 𝑠 𝑡 𝑎 𝑏 = ∀ c . Ж.Optical c => Ж.Optic c 𝑠 𝑡 𝑎 𝑏

-- | An equality that could also have additional capabilities. This is only theoretical,
--   because equalities do by design nothing at all.
type AnEquality 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.EqualityTrait 𝑠 𝑡 𝑎 𝑏

type Equality' 𝑠 𝑎 = Equality 𝑠 𝑠 𝑎 𝑎

simple :: Equality' 𝑎 𝑎
simple = id


traverseOf :: Applicative 𝑓 => ATraversal 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡
traverseOf Ж.Equality = id
traverseOf (OpticC (Ж.Traversal y)) = y

traversal :: (∀ 𝑓 . Applicative 𝑓 => (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡) -> Traversal 𝑠 𝑡 𝑎 𝑏
traversal f = OpticC (Ж.traversal f)

traversed :: Traversable 𝑡 => Traversal (𝑡 𝑎) (𝑡 𝑏) 𝑎 𝑏
traversed = traversal traverse

-- | Traversals can 'Fold' over the fields of a data structure, and additionally
--   reconstruct the structure with modified fields.
type Traversal 𝑠 𝑡 𝑎 𝑏 = ∀ c . Ж.FromTraversal c => Ж.Optic c 𝑠 𝑡 𝑎 𝑏

-- | A traversal that may also have additional capabilities, e.g. a 'Lens' or 'Prism'.
type ATraversal 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.TraversalTrait 𝑠 𝑡 𝑎 𝑏

type Traversal' 𝑠 𝑎 = Traversal 𝑠 𝑠 𝑎 𝑎


traverse1ByOf :: Functor 𝑓 => ATraversal1 𝑠 𝑡 𝑎 𝑏 -> (∀ 𝑥 𝑦 . 𝑓 (𝑥->𝑦) -> 𝑓 𝑥 -> 𝑓 𝑦)
                                              -> (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡
traverse1ByOf Ж.Equality = const id
traverse1ByOf (OpticC (Ж.Traversal1 y)) = y

traversal1By :: (∀ 𝑓 . Functor 𝑓 => (∀ 𝑥 𝑦 . 𝑓 (𝑥->𝑦) -> 𝑓 𝑥 -> 𝑓 𝑦)
                                    -> (𝑎 -> 𝑓 𝑏) -> 𝑠 -> 𝑓 𝑡) -> Traversal1 𝑠 𝑡 𝑎 𝑏
traversal1By f = OpticC (Ж.traversal1By f)

-- | Like 'Traversal', but always hits at least one element in the structure.
type Traversal1 𝑠 𝑡 𝑎 𝑏 = ∀ c . Ж.FromTraversal1 c => Ж.Optic c 𝑠 𝑡 𝑎 𝑏

-- | A 'Traversal1' that may also have additional capabilities, e.g. a 'Lens'.
type ATraversal1 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.Traversal1Trait 𝑠 𝑡 𝑎 𝑏

type Traversal1' 𝑠 𝑎 = Traversal1 𝑠 𝑠 𝑎 𝑎


foldMapOf :: (Monoid 𝑟, Semigroup 𝑟) => AFold 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑟) -> 𝑠 -> 𝑟
foldMapOf Ж.Equality = id
foldMapOf (OpticC (Ж.Fold y)) = y

folded :: Foldable 𝑓 => Fold (𝑓 𝑎) 𝑎
folded = OpticC $ Ж.folded

-- | Folds access fields that may occur multiple times in the data structure,
--   or not at all, such as the elements of a list. Like 'Getter', they don't
--   have “write permission”.
type Fold 𝑠 𝑎 = ∀ c . Ж.FromFold c => Ж.Optic c 𝑠 𝑠 𝑎 𝑎

-- | A fold that may also have additional capabilities, e.g. a 'Getter', 'Traversal'
--   or 'Fold1'.
type AFold 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.FoldTrait 𝑠 𝑡 𝑎 𝑏

foldMap1Of :: (Semigroup 𝑟) => AFold1 𝑠 𝑡 𝑎 𝑏 -> (𝑎 -> 𝑟) -> 𝑠 -> 𝑟
foldMap1Of Ж.Equality = id
foldMap1Of (OpticC (Ж.Fold1 y)) = y

-- | 'Fold1' is slightly stronger than 'Fold': it requires that there is at least
--   one targeted value in the structure.
type Fold1 𝑠 𝑎 = ∀ c . Ж.FromFold1 c => Ж.Optic c 𝑠 𝑠 𝑎 𝑎

-- | A 'Fold1' that may also have additional capabilities, e.g. a 'Getter' or 'Lens'.
type AFold1 𝑠 𝑡 𝑎 𝑏 = Ж.Optic Ж.Fold1Trait 𝑠 𝑡 𝑎 𝑏

-- $composInfo
-- Optics compose “OO style”, from left to right. For example, given
-- 
-- @
-- data Foo = Foo Int String
-- foostr :: 'Lens'' Foo String
-- data Bar = Bar Foo Bool
-- barfoo :: 'Lens'' Bar Foo
-- 
-- hideout :: bar
-- hideout = Bar (Foo 7 "I'm here!") True
-- @
--
-- you can use
--
-- @
--    hideout '^.' barfoo'.'foostr
-- @
--
-- to look up the @"I'm here!"@ string.
-- 
-- Optics of different power can directly be composed with each other, for instance,
-- in the example above it would have also been sufficient if
--
-- @
-- barfoo :: 'Getter' Bar Foo
-- @
--
-- This is not possible for the 'AGetter', 'ALens' etc. varieties; use 'weaken' in
-- this case.


-- | Turn 'ALens' into 'Lens', or 'ATraversal' into 'Traversal', and so on.
--   This may be necessary if you want to re-use e.g. an 'AnIso' as a 'Lens',
--   or an 'AGetter' as a 'Fold', etc.. See the 'FromIso' hierarchy for which
--   general optics can be used as which concrete (i.e. @A@-prefixed) ones.
--
--   'weaken' is elsewhere known as @cloneLens@, @cloneIso@ etc..
weaken :: (Ж.Optical c, Ж.Optical ζ, Ж.OptDens c ζ)
               => Ж.Optic c 𝑠 𝑡 𝑎 𝑏 -> Ж.Optic ζ 𝑠 𝑡 𝑎 𝑏
weaken Ж.Equality = id
weaken (Ж.OpticC c) = Ж.OpticC $ Ж.cloneOptic c
