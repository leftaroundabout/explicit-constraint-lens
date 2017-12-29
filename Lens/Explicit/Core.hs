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

module Lens.Explicit.Core where
    
import Prelude hiding (id, (.))
import Control.Category
import Control.Monad ((>=>))
import Control.Arrow ((+++))
import Data.Functor.Identity


data IsoTrait
data GetterTrait
data SetterTrait
data LensTrait
data PrismTrait
data FoldTrait
data TraversalTrait

type Optic c s t a b = OpticC c (s,t) (a,b)
data OpticC c x y where
  Equality :: OpticC c q q
  Iso :: (s -> a) -> (b -> t) -> AnIso s t a b
  Lens :: (s -> a) -> (s -> b -> t) -> ALens s t a b
  Prism :: (b -> t) -> (s -> Either t a) -> APrism s t a b
  Fold :: (∀ f . Applicative f => (a -> f r) -> s -> f r) -> Optic FoldTrait s b a r
  Traversal :: (∀ f . Applicative f => (a -> f b) -> s -> f t)
                             -> ATraversal s t a b
  Getter :: (s -> a) -> Optic GetterTrait s t a b
  Setter :: ((a -> b) -> s -> t) -> ASetter s t a b

instance Category (OpticC c) where
  id = Equality
  Equality . f = f
  f . Equality = f
  Iso f φ . Iso g γ = Iso (f . g) (γ . φ)
  Lens f φ . Lens g γ = Lens (f . g) (\s b -> γ s $ φ (g s) b)
  Prism φ f . Prism γ g = Prism (γ . φ) (g >=> (γ+++id) . f)
                                                -- Left ξ -> Left $ γ ξ
                                                -- Right υ -> Right υ)
  Getter f . Getter g = Getter (f . g)
  Setter σ . Setter s = Setter $ s . σ
  

-- ⣿⢠⡤⠤⢀⣤⢤⣄⢠⡤⠤
-- ⣿⢈⣛⡷⠸⣧⣠⡿⢈⣛⡷

type AnIso s t a b = Optic IsoTrait s t a b
type Iso s t a b = ∀ c . FromIso c => Optic c s t a b

class FromIso c where
  iso :: (s -> a) -> (b -> t) -> Optic c s t a b
instance FromIso IsoTrait where
  iso = Iso
instance FromIso GetterTrait where
  iso f _ = Getter f
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

type ALens s t a b = Optic LensTrait s t a b
type Lens s t a b = ∀ c . FromLens c => Optic c s t a b

class FromIso c => FromLens c where
  lens :: (s -> a) -> (s -> b -> t) -> Optic c s t a b
instance FromLens GetterTrait where
  lens f _ = Getter f
instance FromLens LensTrait where
  lens = Lens
instance FromLens TraversalTrait where
  lens f φ = Traversal (\τ s -> fmap (φ s) . τ $ f s)
instance FromLens SetterTrait where
  lens f φ = Setter (\τ s -> φ s . τ $ f s)


-- ⣿⢉⣿⢠⣄⡄⣭⢠⡤⠤⢠⡤⢤⡤⢤⡄⣤⠤⠄
-- ⣿⠉⠁⢸⡇⠀⣿⢈⣛⡷⢸⡇⢸⡇⢸⡇⣙⣻⠆

type APrism s t a b = Optic PrismTrait s t a b
type Prism s t a b = ∀ c . FromPrism c => Optic c s t a b

class FromIso c => FromPrism c where
  prism :: (b -> t) -> (s -> Either t a) -> Optic c s t a b
instance FromPrism PrismTrait where
  prism = Prism
instance FromPrism TraversalTrait where
  prism φ f = Traversal (\τ -> either pure (fmap φ . τ) . f)
instance FromPrism SetterTrait where
  prism φ f = Setter (\τ -> either id (φ . τ) . f)


-- ⣴⠋⠉⠁⣠⠤⣄⢼⡧⠠⣿⢄⡤⢤⡀⣤⣠⢠⡤⠤
-- ⠻⣄⣸⡇⢿⣒⣛⢸⣇⠀⣿⡸⣗⣚⡃⣿⠀⢈⣛⡷

type AGetter s a = Optic GetterTrait s s a a
type Getter s t a b = ∀ c . FromGetter c => Optic c s t a b

class FromLens c => FromGetter c where
  to :: (s -> a) -> Optic c s t a t
instance FromGetter GetterTrait where
  to = Getter
instance FromGetter TraversalTrait where
  to f = Traversal (\t -> t . f)



-- ⠉⢹⡏⠉⣤⣠⢠⢤⡀⢤⡀⢠⢄⡤⢤⡀⣤⣠⢠⡤⠤⠀⡤⣄⠀⣿⢠⡤⠤
-- ⠀⢸⡇⠀⣿⠀⢿⡹⣇⠈⣷⡎⠸⣗⣚⡃⣿⠀⢈⣛⡷⠸⣏⢿⡀⣿⢈⣛⡷

type ATraversal s t a b = Optic TraversalTrait s t a b
type Traversal s t a b = ∀ c . FromTraversal c => Optic c s t a b

class (FromLens c, FromPrism c) => FromTraversal c where
  traversed :: (∀ f . Applicative f => (a -> f b) -> s -> f t) -> Optic c s t a b
instance FromTraversal TraversalTrait where
  traversed = Traversal
instance FromTraversal SetterTrait where
  traversed θ = Setter (\f -> runIdentity . θ (Identity . f))


-- ⣾⣍⠁⢀⡤⢤⡠⣿⠄⢼⡧⣠⠤⣄⢠⣄⡄⣤⠤⠄
-- ⣀⣉⡿⠸⣗⣚⡃⣿⡀⢸⣇⢿⣒⣛⢸⡇⠀⣙⣻⠆

type ASetter s t a b = Optic SetterTrait s t a b
type Setter s t a b = ∀ c . FromSetter c => Optic c s t a b

class FromTraversal c => FromSetter c where
  sets :: ((a -> b) -> s -> t) -> Optic c s t a b
instance FromSetter SetterTrait where
  sets = Setter
