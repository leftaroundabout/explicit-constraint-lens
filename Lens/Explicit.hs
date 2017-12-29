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
                     -- * Traversals
                     , traverseOf, traversed, Traversal, ATraversal
                     -- * Folds
                     , foldMapOf, folded, Fold, AFold
                     ) where

import qualified Lens.Explicit.Core as Ж



infixl 8 ^.

(^.) :: s -> AGetter s a -> a
s ^. Ж.Getter f = f s

to :: (s -> a) -> Getter s a
to = Ж.to

type Getter s a = Ж.Getter s s a s
type AGetter s a = Ж.AGetter s a


infixr 4 %~, .~

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
Ж.Setter f %~ m = f m

(.~) :: ASetter s t a b -> b -> s -> t
a .~ b = a %~ const b

sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets = Ж.sets

type Setter s t a b = Ж.Setter s t a b
type ASetter s t a b = Ж.ASetter s t a b


(%%~) :: Functor f => ALens s t a b -> (a -> f b) -> s -> f t
(%%~) (Ж.Lens f φ) τ s = fmap (φ s) . τ $ f s

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens = Ж.lens

type Lens s t a b = Ж.Lens s t a b
type ALens s t a b = Ж.ALens s t a b


prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism = Ж.prism

matching :: APrism s t a b -> s -> Either t a
matching (Ж.Prism _ f) = f

type Prism s t a b = Ж.Prism s t a b
type APrism s t a b = Ж.APrism s t a b


under :: AnIso s t a b -> (t -> s) -> b -> a
under (Ж.Iso f φ) g = f . g . φ

from :: AnIso s t a b -> Iso b a t s
from (Ж.Iso f φ) = iso φ f

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = Ж.iso

type Iso s t a b = Ж.Iso s t a b
type AnIso s t a b = Ж.AnIso s t a b


traverseOf :: Applicative f => ATraversal s t a b -> (a -> f b) -> s -> f t
traverseOf (Ж.Traversal y) = y

traversed :: (∀ f . Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b
traversed = Ж.traversed

type Traversal s t a b = Ж.Traversal s t a b
type ATraversal s t a b = Ж.ATraversal s t a b


foldMapOf :: Monoid r => AFold s a -> (a -> r) -> s -> r
foldMapOf (Ж.Fold y) = y

folded :: Foldable f => Fold (f a) a
folded = Ж.folded

type Fold s a = Ж.Fold s s a s
type AFold s a = Ж.AFold s s a s

