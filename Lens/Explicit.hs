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

type Iso s t a b = ∀ c . FromIso c => Optic c s t a b

newtype IsoTrait s t a b α β σ τ = IsoTrait (τ -> σ)

instance FromIso IsoTrait where
  iso g f = Accessor $ \(IsoTrait q) -> IsoTrait $ g . q . f 

type AnIso s t a b = Optic IsoTrait s t a b

under :: AnIso s t a b -> (t -> s) -> b -> a
under (Accessor f) g = case f $ IsoTrait g of IsoTrait q -> q

--  L E N S E S

class FromIso c => FromLens c where
  lens :: (s -> a) -> (s -> b -> t) -> Optic c s t a b

type Lens s t a b = ∀ c . FromLens c => Optic c s t a b

data LensTrait s t a b α β σ τ = LensTrait σ (b->β)

instance FromIso LensTrait where
  iso g f = Accessor $ \(LensTrait s i) -> LensTrait (g s) (f . i)
instance FromLens LensTrait where
  lens g f = Accessor $ \(LensTrait s i) -> LensTrait (g s) (f s . i)

type ALens s t a b = Optic LensTrait s t a b

getset :: ALens s t a b -> s -> (a, b->t)
getset (Accessor f) s = case f $ LensTrait s id of LensTrait a m -> (a, m)

--  P R I S M S

class FromIso c => FromPrism c where
  prism :: (b -> t) -> (s -> Either t a) -> Optic c s t a b

type Prism s t a b = ∀ c . FromPrism c => Optic c s t a b

data PrismTrait s t a b α β σ τ
     = PrismWas β
     | PrismBecomes σ

instance FromIso PrismTrait where
  iso g f = Accessor $ \p -> case p of
                PrismWas b -> PrismWas $ f b
                PrismBecomes s -> PrismBecomes $ g s
instance FromPrism PrismTrait where
  prism f g = Accessor $ \p -> case p of
                PrismWas b -> PrismWas $ f b
                PrismBecomes s -> case g s of
                    Left t -> PrismWas t
                    Right a -> PrismBecomes a

type APrism s t a b = Optic PrismTrait s t a b

matching :: APrism s t a b -> s -> Either t a
matching (Accessor f) s = case f $ PrismBecomes s of
                           PrismBecomes a -> Right a
                           PrismWas t -> Left t

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism p cont = uncurry cont $ disassemblePrism p

disassemblePrism :: APrism s t a b -> (b -> t, s -> Either t a)
disassemblePrism (Accessor f)
     = ( \b -> case f $ PrismWas b of
                   PrismWas t -> t
                   PrismBecomes _ -> error
                      "Black magic Prism, extracts value where none can be."
       , \s -> case f $ PrismBecomes s of
                   PrismWas t -> Left t
                   PrismBecomes a -> Right a )

--  T R A V E R S A L S


--  G E T T E R S

class FromLens c => FromGetter c where
  to :: (s -> a) -> Optic c s t a b

type Getter s a = ∀ c . FromGetter c => Optic c s s a a

newtype GetterTrait s t a b α β σ τ = GetterTrait σ

instance FromIso GetterTrait where
  iso g _ = to g
instance FromLens GetterTrait where
  lens g _ = to g
instance FromGetter GetterTrait where
  to g = Accessor $ \(GetterTrait q) -> GetterTrait $ g q

type AGetter s a = Optic GetterTrait s s a a

infixl 8 ^.

(^.) :: s -> AGetter s a -> a
s ^. Accessor f = case f $ GetterTrait s of GetterTrait a -> a

foldMapOf :: Optic GetterTrait s s a a -> (a -> b) -> s -> b
foldMapOf a g s = g $ s^.a

--  S E T T E R S

class (FromLens c, FromPrism c) => FromSetter c where
  sets :: ((a -> b) -> s -> t) -> Optic c s t a b

type Setter s t a b = ∀ c . FromSetter c => Optic c s t a b

newtype SetterTrait s t a b α β σ τ = SetterTrait (α -> β)

instance FromIso SetterTrait where
  iso g f = Accessor $ \(SetterTrait q) -> SetterTrait $ f . q . g
instance FromLens SetterTrait where
  lens g f = Accessor $ \(SetterTrait q) -> SetterTrait $ \s -> f s . q $ g s
instance FromPrism SetterTrait where
  prism g f = Accessor $ \(SetterTrait q) -> SetterTrait $ \s ->
                 case f s of Left t -> t
                             Right a -> g $ q a
instance FromSetter SetterTrait where
  sets f = Accessor $ \(SetterTrait q) -> SetterTrait $ f q

type ASetter s t a b = Optic SetterTrait s t a b

infixr 4 %~, .~

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
Accessor f %~ m = case f $ SetterTrait m of SetterTrait q -> q

(.~) :: ASetter s t a b -> b -> s -> t
a .~ b = a %~ const b

