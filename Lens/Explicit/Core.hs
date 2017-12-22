-- |
-- Module      : Lens.Explicit.Core
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
{-# LANGUAGE DeriveFunctor                #-}

module Lens.Explicit.Core where
    
import Prelude hiding (id, (.))
import Control.Category
import qualified Data.Foldable as Foldable

newtype Accessor x y = Accessor { runAccessor :: x -> y }
  deriving (Category)

type Optic (c :: * -> * -> * -> * -> *) s t a b
        = ∀ x y . Accessor (Focus c s t a b x y) (c s t a b)

type AnOptic c s t a b = Accessor (Focus c s t a b a b) (c s t a b)

class Æther (c :: * -> * -> * -> * -> *) where
  data Focus c s t a b :: * -> * -> *
  liftOptic :: AnOptic c s t a b -> Optic (Focus c x y) s t a b

--  I S O S

class Æther c => FromIso c where
  iso :: (s -> a) -> (b -> t) -> Optic c s t a b

type Iso s t a b = ∀ c . FromIso c => Optic c s t a b

data IsoTrait s t a b = IsoTrait (s -> a) (b -> t)
instance Æther IsoTrait where
  data Focus IsoTrait s t a b x y = IsoFocus (a -> a, t -> t)
  liftOptic (Accessor α) = Accessor $ \(IsoFocus φ) -> case α _ of
                             IsoFocus (f,g) -> _

instance FromIso IsoTrait where
  iso g f = Accessor $ \(IsoFocus (p,q)) -> IsoTrait (p . g) (q . f)

type AnIso s t a b = AnOptic IsoTrait s t a b

under :: AnIso s t a b -> (t -> s) -> b -> a
under (Accessor f) g = case f $ IsoFocus (id,id) of IsoTrait p q -> p . g . q

from :: AnIso s t a b -> Iso b a t s
from (Accessor i) = case i $ IsoFocus (id,id) of IsoTrait d b -> iso b d

--  L E N S E S

class FromIso c => FromLens c where
  lens :: (s -> a) -> (s -> b -> t) -> Optic c s t a b

type Lens s t a b = ∀ c . FromLens c => Optic c s t a b

data LensTrait s t a b = LensTrait a (b->t)
instance Æther LensTrait where
  data Focus LensTrait s t a b x y = LensFocus (s, b->b)

instance FromIso LensTrait where
  iso g f = Accessor $ \(LensFocus (s,i)) -> LensTrait (g s) (f . i)
instance FromLens LensTrait where
  lens g f = Accessor $ \(LensFocus (s,i)) -> LensTrait (g s) (f s . i)

type ALens s t a b = AnOptic LensTrait s t a b

getset :: ALens s t a b -> s -> (a, b->t)
getset (Accessor f) s = case f $ LensFocus (s,id) of LensTrait a m -> (a, m)

--  P R I S M S

class FromIso c => FromPrism c where
  prism :: (b -> t) -> (s -> Either t a) -> Optic c s t a b

type Prism s t a b = ∀ c . FromPrism c => Optic c s t a b

data PrismTrait s t a b = PrismRemixed t
                        | PrismFiltered a
instance Æther PrismTrait where
  data Focus PrismTrait s t a b x y = PrismFocus (Either b s)

instance FromIso PrismTrait where
  iso g f = Accessor $ \p -> case p of
                PrismFocus (Left b) -> PrismRemixed $ f b
                PrismFocus (Right s) -> PrismFiltered $ g s
instance FromPrism PrismTrait where
  prism f g = Accessor $ \p -> case p of
                PrismFocus (Left b) -> PrismRemixed $ f b
                PrismFocus (Right s) -> case g s of
                    Left t -> PrismRemixed t
                    Right a -> PrismFiltered a

type APrism s t a b = AnOptic PrismTrait s t a b

matching :: APrism s t a b -> s -> Either t a
matching (Accessor f) s = case f . PrismFocus $ Right s of
                           PrismFiltered a -> Right a
                           PrismRemixed t -> Left t

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism p cont = uncurry cont $ disassemblePrism p

disassemblePrism :: APrism s t a b -> (b -> t, s -> Either t a)
disassemblePrism (Accessor f)
     = ( \b -> case f . PrismFocus $ Left b of
                   PrismRemixed t -> t
                   PrismFiltered _ -> prismError "disassemblePrism"
       , \s -> case f . PrismFocus $ Right s of
                   PrismRemixed t -> Left t
                   PrismFiltered a -> Right a )

prismError :: String -> a
prismError f = error $ f ++ ": Prism extracts value where none can be."

--  T R A V E R S A L S

class (FromLens c, FromPrism c) => FromTraversal c where
  traversed :: Traversable f => Optic c (f a) (f b) a b

type Traversal s t a b = ∀ c . FromTraversal c => Optic c s t a b

newtype TraversalTrait m s t a b = TraversalTrait (s -> m t)
instance Æther (TraversalTrait m) where
  data Focus (TraversalTrait m) s t a b x y = TraversalFocus (a -> m b)

instance Functor m => FromIso (TraversalTrait m) where
  iso g f = Accessor $ \(TraversalFocus q) -> TraversalTrait $ fmap f . q . g
instance Functor m => FromLens (TraversalTrait m) where
  lens g f = Accessor $ \(TraversalFocus q) -> TraversalTrait $ \s -> fmap (f s) . q $ g s
instance Applicative m => FromPrism (TraversalTrait m) where
  prism f g = Accessor $ \(TraversalFocus q) -> TraversalTrait $ \s -> case g s of
                                                     Left t -> pure t
                                                     Right a -> fmap f $ q a
instance Applicative m => FromTraversal (TraversalTrait m) where
  traversed = Accessor $ \(TraversalFocus q)
                -> TraversalTrait $ traverse q

type ATraversal m s t a b = AnOptic (TraversalTrait m) s t a b

data StateL s x = StateL (s -> (s,x)) deriving (Functor)
instance Applicative (StateL s) where
  pure = StateL . flip (,)
  StateL f<*>StateL g = StateL $ \s -> let (s',μ) = f s in fmap μ $ g s'

mapAccumLOf :: ATraversal (StateL acc) s t a b
                   -> (acc -> a -> (acc,b)) -> acc -> s -> (acc,t)
mapAccumLOf (Accessor y) f = case y . TraversalFocus $ StateL . flip f of
                               TraversalTrait w -> \acc s -> case w s of
                                        StateL p -> p acc

traverseOf :: ATraversal f s t a b -> (a -> f b) -> s -> f t
traverseOf (Accessor y) f = case y $ TraversalFocus f of
                               TraversalTrait w -> w

--  G E T T E R S

class FromLens c => FromGetter c where
  to :: (s -> a) -> Optic c s t a b

type Getter s a = ∀ c . FromGetter c => Optic c s s a a

newtype GetterTrait s t a b = GetterTrait a
instance Æther GetterTrait where
  data Focus GetterTrait s t a b x y = GetterFocus s

instance FromIso GetterTrait where
  iso g _ = Accessor $ \(GetterFocus q) -> GetterTrait $ g q
instance FromLens GetterTrait where
  lens g _ = Accessor $ \(GetterFocus q) -> GetterTrait $ g q
instance FromGetter GetterTrait where
  to g = Accessor $ \(GetterFocus q) -> GetterTrait $ g q

type AGetter s a = AnOptic GetterTrait s s a a

infixl 8 ^.

(^.) :: s -> AGetter s a -> a
s ^. Accessor f = case f $ GetterFocus s of GetterTrait a -> a

--  F O L D S

class FromGetter c => FromFold c where
  folded :: Foldable f => Optic c (f a) t a b

type Fold s t a b = ∀ c . FromFold c => Optic c s t a b

newtype FoldlTrait r s t a b = FoldlTrait (r -> s -> r)
instance Æther (FoldlTrait r) where
  data Focus (FoldlTrait r) s t a b x y = FoldlFocus (r -> a -> r)

instance FromIso (FoldlTrait r) where
  iso g _ = to g
instance FromLens (FoldlTrait r) where
  lens g _ = to g
instance FromGetter (FoldlTrait r) where
  to g = Accessor $ \(FoldlFocus q) -> FoldlTrait $ \b -> q b . g
instance FromFold (FoldlTrait r) where
  folded = Accessor $ \(FoldlFocus q) -> FoldlTrait $ Foldable.foldl' q

type AFoldl r s t a b = AnOptic (FoldlTrait r) s t a b

foldl'Of :: AFoldl r s t a b -> (r -> a -> r) -> r -> s -> r
foldl'Of (Accessor f) y = case f $ FoldlFocus y of FoldlTrait w -> w

newtype FoldrTrait r s t a b = FoldrTrait (s -> r -> r)
instance Æther (FoldrTrait r) where
  data Focus (FoldrTrait r) s t a b x y = FoldrFocus (a -> r -> r)

instance FromIso (FoldrTrait r) where
  iso g _ = to g
instance FromLens (FoldrTrait r) where
  lens g _ = to g
instance FromGetter (FoldrTrait r) where
  to g = Accessor $ \(FoldrFocus q) -> FoldrTrait $ q . g
instance FromFold (FoldrTrait r) where
  folded = Accessor $ \(FoldrFocus q) -> FoldrTrait . flip $ Foldable.foldr q

type AFoldr r s t a b = AnOptic (FoldrTrait r) s t a b

foldrOf :: AFoldr r s t a b -> (a -> r -> r) -> r -> s -> r
foldrOf (Accessor f) y = case f $ FoldrFocus y of FoldrTrait w -> flip w

foldMapOf :: Monoid r => AFoldr r s t a b -> (a -> r) -> s -> r
foldMapOf f g = foldrOf f (mappend . g) mempty

--  S E T T E R S

class FromTraversal c => FromSetter c where
  sets :: ((a -> b) -> s -> t) -> Optic c s t a b

type Setter s t a b = ∀ c . FromSetter c => Optic c s t a b

newtype SetterTrait s t a b = SetterTrait (s -> t)
instance Æther SetterTrait where
  data Focus SetterTrait s t a b x y = SetterFocus (a -> b)

instance FromIso SetterTrait where
  iso g f = Accessor $ \(SetterFocus q) -> SetterTrait $ f . q . g
instance FromLens SetterTrait where
  lens g f = Accessor $ \(SetterFocus q) -> SetterTrait $ \s -> f s . q $ g s
instance FromPrism SetterTrait where
  prism g f = Accessor $ \(SetterFocus q) -> SetterTrait $ \s ->
                 case f s of Left t -> t
                             Right a -> g $ q a
instance FromTraversal SetterTrait where
  traversed = Accessor $ \(SetterFocus q) -> SetterTrait $ fmap q
instance FromSetter SetterTrait where
  sets f = Accessor $ \(SetterFocus q) -> SetterTrait $ f q

type ASetter s t a b = AnOptic SetterTrait s t a b

infixr 4 %~, .~

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
Accessor f %~ m = case f $ SetterFocus m of SetterTrait q -> q

(.~) :: ASetter s t a b -> b -> s -> t
a .~ b = a %~ const b




instance Æther c => Æther (Focus c x y) where
  data Focus (Focus c x y) s t a b m n = Focus² (Focus 
