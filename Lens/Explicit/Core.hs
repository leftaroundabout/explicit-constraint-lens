-- |
-- Module      : Lens.Explicit.Core
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE UnicodeSyntax                #-}
{-# LANGUAGE Rank2Types                   #-}
{-# LANGUAGE KindSignatures               #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE EmptyCase                    #-}

module Lens.Explicit.Core where
    
import Prelude hiding (id, (.))
import Control.Category
import Control.Monad ((>=>))
import Control.Applicative (Const(..))
import Control.Arrow ((+++))
import Data.Functor.Identity

import GHC.Exts (Constraint)

import Data.Semigroup


type Optic c s t a b = OpticC c (a,b) (s,t)
data OpticC c x y where
  Equality :: OpticC c q q
  OpticC :: c s t a b -> Optic c s t a b

class (Category (OpticC c)) => Optical c where
  type OptDens c (ζ :: * -> * -> * -> * -> *) :: Constraint
  cloneOptic :: OptDens c ζ => c s t a b -> ζ s t a b
  (∘) :: c x y s t -> c s t a b -> c x y a b

instance (Optical c) => Category (OpticC c) where
  id = Equality
  Equality . f = f
  f . Equality = f
  OpticC f . OpticC g = OpticC $ f ∘ g



-- ⣿⣉⡉⣠⠤⣤⢠⡄⢠⡄⢠⢤⡀⢸⡇⣭⠠⣿⠄⣭⢀⡤⢤⡀⣤⠤⠄
-- ⣿⣀⣀⢿⣀⣿⠸⣇⣸⡇⢿⡹⣇⢸⡇⣿⠀⣿⡀⣿⠸⣗⣚⡃⣙⣻⠆
-- ⠀⠀⠀⠀⠀⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
data EqualityTrait s t a b

instance Optical EqualityTrait where
  type OptDens EqualityTrait ζ = ()
  cloneOptic e = case e of {}
  e∘s = case (e,s) of {}

  

-- ⣿⢠⡤⠤⢀⣤⢤⣄⢠⡤⠤
-- ⣿⢈⣛⡷⠸⣧⣠⡿⢈⣛⡷

data IsoTrait s t a b = Iso (s -> a) (b -> t)

instance Optical IsoTrait where
  type OptDens IsoTrait ζ = FromIso ζ
  cloneOptic (Iso f φ) = iso f φ
  Iso g γ ∘ Iso f φ = Iso (f . g) (γ . φ)


class Optical c => FromIso c where
  iso :: (s -> a) -> (b -> t) -> c s t a b
instance FromIso IsoTrait where
  iso = Iso
instance FromIso GetterTrait where
  iso f _ = Getter f
instance FromIso ReviewTrait where
  iso _ φ = Review φ
instance FromIso LensTrait where
  iso f φ = Lens f (\_ b -> φ b)
instance FromIso PrismTrait where
  iso f φ = Prism φ (Right . f)
instance FromIso SetterTrait where
  iso f φ = Setter $ \u -> φ . u . f
instance FromIso FoldTrait where
  iso f _ = Fold (\t -> t . f)
instance FromIso Fold1Trait where
  iso f _ = Fold1 (\t -> t . f)
instance FromIso TraversalTrait where
  iso f φ = Traversal (\t -> fmap φ . t . f)


-- ⣿⠀⠀⣠⠤⣄⢠⡤⢤⡀⣤⠤⠄⣠⠤⣄⢠⡤⠤
-- ⣿⣀⣀⢿⣒⣛⢸⡇⢸⡇⣙⣻⠆⢿⣒⣛⢈⣛⡷

data LensTrait s t a b = Lens (s -> a) (s -> b -> t)

instance Optical LensTrait where
  type OptDens LensTrait ζ = FromLens ζ
  cloneOptic (Lens f φ) = lens f φ
  Lens g γ ∘ Lens f φ = Lens (f . g) (\s b -> γ s $ φ (g s) b)


class FromIso c => FromLens c where
  lens :: (s -> a) -> (s -> b -> t) -> c s t a b
instance FromLens GetterTrait where
  lens f _ = Getter f
instance FromLens LensTrait where
  lens = Lens
instance FromLens TraversalTrait where
  lens f φ = Traversal (\τ s -> fmap (φ s) . τ $ f s)
instance FromLens FoldTrait where
  lens f _ = Fold (\τ -> τ . f)
instance FromLens Fold1Trait where
  lens f _ = Fold1 (\τ -> τ . f)
instance FromLens SetterTrait where
  lens f φ = Setter (\τ s -> φ s . τ $ f s)


-- ⣿⢉⣿⢠⣄⡄⣭⢠⡤⠤⢠⡤⢤⡤⢤⡄⣤⠤⠄
-- ⣿⠉⠁⢸⡇⠀⣿⢈⣛⡷⢸⡇⢸⡇⢸⡇⣙⣻⠆

data PrismTrait s t a b = Prism (b -> t) (s -> Either t a)

instance Optical PrismTrait where
  type OptDens PrismTrait ζ = FromPrism ζ
  cloneOptic (Prism f φ) = prism f φ
  Prism γ g ∘ Prism φ f = Prism (γ . φ) (g >=> (γ+++id) . f)


class FromIso c => FromPrism c where
  prism :: (b -> t) -> (s -> Either t a) -> c s t a b
instance FromPrism PrismTrait where
  prism = Prism
instance FromPrism TraversalTrait where
  prism φ f = Traversal (\τ -> either pure (fmap φ . τ) . f)
instance FromPrism FoldTrait where
  prism φ f = Fold (\τ -> either (const mempty) τ . f)
instance FromPrism SetterTrait where
  prism φ f = Setter (\τ -> either id (φ . τ) . f)
instance FromPrism ReviewTrait where
  prism φ _ = Review φ


-- ⣴⠋⠉⠁⣠⠤⣄⢼⡧⠠⣿⢄⡤⢤⡀⣤⣠⢠⡤⠤
-- ⠻⣄⣸⡇⢿⣒⣛⢸⣇⠀⣿⡸⣗⣚⡃⣿⠀⢈⣛⡷

data GetterTrait s t a b = Getter (s -> a)

instance Optical GetterTrait where
  type OptDens GetterTrait ζ = FromGetter ζ
  cloneOptic (Getter f) = to f
  Getter g ∘ Getter f = Getter (f . g)


class FromLens c => FromGetter c where
  to :: (s -> a) -> c s t a b
instance FromGetter GetterTrait where
  to = Getter
instance FromGetter FoldTrait where
  to f = Fold (\t -> t . f)


-- ⣿⢉⡷⢀⡤⢤⡠⣄⠀⡤⢨⡅⣠⠤⣄⢠⡄⢠⡄⡠⢠⡤⠤
-- ⣿⠙⣧⡸⣗⣚⡃⢹⣶⠁⢸⡇⢿⣒⣛⠀⣿⠇⣧⠇⢈⣛⡷

data ReviewTrait s t a b = Review (b -> t)

instance Optical ReviewTrait where
  type OptDens ReviewTrait ζ = FromReview ζ
  cloneOptic (Review η) = unto η
  Review η ∘ Review θ = Review (η . θ)


class FromPrism c => FromReview c where
  unto :: (b -> t) -> c s t a b
instance FromReview ReviewTrait where
  unto = Review


-- ⠉⢹⡏⠉⣤⣠⢠⢤⡀⢤⡀⢠⢄⡤⢤⡀⣤⣠⢠⡤⠤⠀⡤⣄⠀⣿⢠⡤⠤
-- ⠀⢸⡇⠀⣿⠀⢿⡹⣇⠈⣷⡎⠸⣗⣚⡃⣿⠀⢈⣛⡷⠸⣏⢿⡀⣿⢈⣛⡷

data TraversalTrait s t a b = Traversal (∀ f . Applicative f => (a -> f b) -> s -> f t)

instance Optical TraversalTrait where
  type OptDens TraversalTrait ζ = FromTraversal ζ
  cloneOptic (Traversal η) = traversed η
  Traversal η ∘ Traversal θ = Traversal (η . θ)


class (FromLens c, FromPrism c) => FromTraversal c where
  traversed :: (∀ f . Applicative f => (a -> f b) -> s -> f t) -> c s t a b
instance FromTraversal TraversalTrait where
  traversed = Traversal
instance FromTraversal SetterTrait where
  traversed θ = Setter (\f -> runIdentity . θ (Identity . f))
instance FromTraversal FoldTrait where
  traversed θ = Fold (\t -> getConst . θ (Const . t))


-- ⣾⣍⠁⢀⡤⢤⡠⣿⠄⢼⡧⣠⠤⣄⢠⣄⡄⣤⠤⠄
-- ⣀⣉⡿⠸⣗⣚⡃⣿⡀⢸⣇⢿⣒⣛⢸⡇⠀⣙⣻⠆

data SetterTrait s t a b = Setter ((a -> b) -> s -> t)

instance Optical SetterTrait where
  type OptDens SetterTrait ζ = FromSetter ζ
  cloneOptic (Setter η) = sets η
  Setter s ∘ Setter σ = Setter $ s . σ


class FromTraversal c => FromSetter c where
  sets :: ((a -> b) -> s -> t) -> c s t a b
instance FromSetter SetterTrait where
  sets = Setter


-- ⣿⣉⢁⣤⢤⣄⢸⡇⣠⠤⣿⢠⡤⠤
-- ⣿⠀⠸⣧⣠⡿⢸⡇⢿⣀⣿⢈⣛⡷

data FoldTrait s t a b = Fold (∀ r . (Monoid r, Semigroup r) => (a -> r) -> s -> r)

instance Optical FoldTrait where
  type OptDens FoldTrait ζ = FromFold ζ
  cloneOptic (Fold η) = folds η
  Fold η ∘ Fold θ = Fold (η . θ)


class (FromTraversal c, FromFold1 c) => FromFold c where
  folds :: (∀ r . (Monoid r, Semigroup r) => (a -> r) -> s -> r) -> c s t a b
  folded :: Foldable f => c (f a) t a b
  folded = folds foldMap
instance FromFold FoldTrait where
  folds = Fold


data Fold1Trait s t a b = Fold1 (∀ r . Semigroup r => (a -> r) -> s -> r)

instance Optical Fold1Trait where
  type OptDens Fold1Trait ζ = FromFold1 ζ
  cloneOptic (Fold1 η) = folds1 η
  Fold1 η ∘ Fold1 θ = Fold1 (η . θ)


class FromLens c => FromFold1 c where
  folds1 :: (∀ r . Semigroup r => (a -> r) -> s -> r) -> c s t a b
instance FromFold1 FoldTrait where
  folds1 = Fold
instance FromFold1 Fold1Trait where
  folds1 = Fold1
