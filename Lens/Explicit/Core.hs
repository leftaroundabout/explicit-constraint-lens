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

type AnEquality s t a b = Optic EqualityTrait s t a b
type Equality s t a b = ∀ c . Optical c => Optic c s t a b
  

-- ⣿⢠⡤⠤⢀⣤⢤⣄⢠⡤⠤
-- ⣿⢈⣛⡷⠸⣧⣠⡿⢈⣛⡷

data IsoTrait s t a b = Iso (s -> a) (b -> t)

instance Optical IsoTrait where
  type OptDens IsoTrait ζ = FromIso ζ
  cloneOptic (Iso f φ) = iso f φ
  Iso g γ ∘ Iso f φ = Iso (f . g) (γ . φ)

type AnIso s t a b = Optic IsoTrait s t a b
type Iso s t a b = ∀ c . FromIso c => Optic c s t a b

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
instance FromIso TraversalTrait where
  iso f φ = Traversal (\t -> fmap φ . t . f)


-- ⣿⠀⠀⣠⠤⣄⢠⡤⢤⡀⣤⠤⠄⣠⠤⣄⢠⡤⠤
-- ⣿⣀⣀⢿⣒⣛⢸⡇⢸⡇⣙⣻⠆⢿⣒⣛⢈⣛⡷

data LensTrait s t a b = Lens (s -> a) (s -> b -> t)

instance Optical LensTrait where
  type OptDens LensTrait ζ = FromLens ζ
  cloneOptic (Lens f φ) = lens f φ
  Lens g γ ∘ Lens f φ = Lens (f . g) (\s b -> γ s $ φ (g s) b)

type ALens s t a b = Optic LensTrait s t a b
type Lens s t a b = ∀ c . FromLens c => Optic c s t a b

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
instance FromLens SetterTrait where
  lens f φ = Setter (\τ s -> φ s . τ $ f s)


-- ⣿⢉⣿⢠⣄⡄⣭⢠⡤⠤⢠⡤⢤⡤⢤⡄⣤⠤⠄
-- ⣿⠉⠁⢸⡇⠀⣿⢈⣛⡷⢸⡇⢸⡇⢸⡇⣙⣻⠆

data PrismTrait s t a b = Prism (b -> t) (s -> Either t a)

instance Optical PrismTrait where
  type OptDens PrismTrait ζ = FromPrism ζ
  cloneOptic (Prism f φ) = prism f φ
  Prism γ g ∘ Prism φ f = Prism (γ . φ) (g >=> (γ+++id) . f)

type APrism s t a b = Optic PrismTrait s t a b
type Prism s t a b = ∀ c . FromPrism c => Optic c s t a b

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

type AGetter s a = Optic GetterTrait s s a a
type Getter s t a b = ∀ c . FromGetter c => Optic c s t a b

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

type AReview b t = Optic ReviewTrait t t b b
type Review s t a b = ∀ c . FromReview c => Optic c s t a b

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

type ATraversal s t a b = Optic TraversalTrait s t a b
type Traversal s t a b = ∀ c . FromTraversal c => Optic c s t a b

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

type ASetter s t a b = Optic SetterTrait s t a b
type Setter s t a b = ∀ c . FromSetter c => Optic c s t a b

class FromTraversal c => FromSetter c where
  sets :: ((a -> b) -> s -> t) -> c s t a b
instance FromSetter SetterTrait where
  sets = Setter


-- ⣿⣉⢁⣤⢤⣄⢸⡇⣠⠤⣿⢠⡤⠤
-- ⣿⠀⠸⣧⣠⡿⢸⡇⢿⣀⣿⢈⣛⡷

data FoldTrait s t a b = Fold (∀ r . Monoid r => (a -> r) -> s -> r)

instance Optical FoldTrait where
  type OptDens FoldTrait ζ = FromFold ζ
  cloneOptic (Fold η) = folds η
  Fold η ∘ Fold θ = Fold (η . θ)

type AFold s t a b = Optic FoldTrait s t a b
type Fold s t a b = ∀ c . FromFold c => Optic c s t a b

class FromTraversal c => FromFold c where
  folds :: (∀ r . Monoid r => (a -> r) -> s -> r) -> c s t a b
  folded :: Foldable f => c (f a) t a b
  folded = folds foldMap
instance FromFold FoldTrait where
  folds = Fold
