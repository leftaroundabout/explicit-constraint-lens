-- |
-- Module      : Lens.Explicit
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE UnicodeSyntax                #-}
{-# LANGUAGE Rank2Types                   #-}

module Lens.Explicit where
    
import Prelude hiding (id, (.))
import Control.Category
import Data.Void

newtype Accessor x y = Accessor { runAccessor :: x -> y }
  deriving (Category)

type Optic c s t a b = Accessor (c s t a b a b s t) (c s t a b s t a b)


--  I S O S

class FromIso c where
  iso :: (s -> a) -> (b -> t) -> Optic c s t a b

newtype IsoTrait s t a b α β σ τ = IsoTrait (τ -> σ)

instance FromIso IsoTrait where
  iso g f = Accessor $ \(IsoTrait q) -> IsoTrait $ g . q . f 

type Iso s t a b = ∀ c . FromIso c => Optic c s t a b

type AnIso s t a b = Optic IsoTrait s t a b

under :: AnIso s t a b -> (t -> s) -> b -> a
under (Accessor f) g = case f $ IsoTrait g of IsoTrait q -> q

--  L E N S E S

class FromIso c => FromLens c where
  lens :: (s -> a) -> (s -> b -> t) -> Optic c s t a b

type Lens s t a b = ∀ c . FromLens c => Optic c s t a b

--  G E T T E R S

class FromLens c => FromGetter c where
  to :: (s -> a) -> Optic c s t a b

newtype GetterTrait s t a b α β σ τ = GetterTrait (α -> b)

instance FromIso GetterTrait where
  iso g _ = to g
instance FromLens GetterTrait where
  lens g _ = to g
instance FromGetter GetterTrait where
  to g = Accessor $ \(GetterTrait q) -> GetterTrait $ q . g

type Getter s a = ∀ c . FromGetter c => Optic c s s a a

type AGetter s a = Optic GetterTrait s s a a

infixl 8 ^.

(^.) :: AGetter s a -> s -> a
(^.) (Accessor f) = case f $ GetterTrait id of GetterTrait q -> q

foldMapOf :: Optic GetterTrait s t a b -> (a -> b) -> s -> b
foldMapOf (Accessor f) g = case f $ GetterTrait g of GetterTrait q -> q

--  S E T T E R S

class FromLens c => FromSetter c where
  sets :: ((a -> b) -> s -> t) -> Optic c s t a b

newtype SetterTrait s t a b α β σ τ = SetterTrait (α -> β)

instance FromIso SetterTrait where
  iso g f = Accessor $ \(SetterTrait q) -> SetterTrait $ f . q . g
instance FromLens SetterTrait where
  lens g f = Accessor $ \(SetterTrait q) -> SetterTrait $ \s -> f s . q $ g s
instance FromSetter SetterTrait where
  sets f = Accessor $ \(SetterTrait q) -> SetterTrait $ f q

type Setter s t a b = ∀ c . FromSetter c => Optic c s t a b

type ASetter s t a b = Optic SetterTrait s t a b

infixr 4 %~, .~

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
Accessor f %~ m = case f $ SetterTrait m of SetterTrait q -> q

(.~) :: ASetter s t a b -> b -> s -> t
a .~ b = a %~ const b

