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
import qualified Data.Foldable as Foldable

import Control.Applicative

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

class (FromLens c, FromPrism c) => FromTraversal c where
  traversed :: Optic c [a] (m [b]) (Maybe (b, [b]) -> [b]) (Maybe (a, m [b]) -> m [b])

type Traversal s t a b = ∀ c . FromTraversal c => Optic c s t a b

newtype TraversalTrait s t a b α β σ τ = TraversalTrait ((b -> s) -> α -> t)
        -- ATraversal s t a b ~ ((b -> s) -> a -> t) -> ((t -> s) -> s -> t)
        -- ATraversal s (m t) (Maybe (a, m t) -> m t) (Maybe (b, t) -> t)
        --                    ~ (((b,t) -> t) -> (a, m t) -> s -> t)
        --                       -> ((b,t) -> t) -> t -> s -> m t

instance FromIso TraversalTrait where
  iso g _ = Accessor $ \(TraversalTrait q) -> TraversalTrait $ \h -> q h . g
instance FromLens TraversalTrait where
  lens g _ = Accessor $ \(TraversalTrait q) -> TraversalTrait $ \h -> q h . g
instance FromPrism TraversalTrait where
  prism f g = Accessor $ \(TraversalTrait q) -> TraversalTrait $ \h s -> case g s of
                                                     Left t -> t
                                                     Right a -> q h a


instance FromTraversal TraversalTrait where
--  traversed = Accessor $ \(TraversalTrait q) -> TraversalTrait . undefined $ curry . q

type ATraversal s t a b = Optic TraversalTrait s t a b


-- mapAccumLOf :: ATraversal s (acc -> (acc,t)) (acc -> (acc,t->t)) b
--                    -> (acc -> a -> (acc,b)) -> acc -> s -> (acc,t)
-- mapAccumLOf (Accessor y) f = case y . TraversalTrait $ \α θ ζ -> _ of
--                                TraversalTrait w -> _
class Foldable f => ParaFoldable f where
  paraFoldr :: (a -> f a -> b -> b) -> b -> f a -> b
class (Traversable f, ParaFoldable f) => ParaTraversable f where
  paraTraverse :: Applicative m => (a -> f a -> m b) -> f a -> m (f b)
instance ParaFoldable [] where
  paraFoldr _ i [] = i
  paraFoldr f i (x:xs) = f x xs $ paraFoldr f i xs
instance ParaTraversable [] where
  paraTraverse _ [] = pure []
  paraTraverse f (x:xs) = (:) <$> f x xs <*> paraTraverse f xs
freeVersing :: ((Maybe (b,[b]) -> [b]) -> Maybe (a,m [b]) -> m [b])
                  -> [a] -> m [b]
freeVersing q = foldr (\x o -> q c $ Just (x, o)) (q c Nothing)
 where c Nothing = []
       c (Just (y,ys)) = y : ys
                


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

--  F O L D S

class FromGetter c => FromFold c where
  folded :: Foldable f => Optic c (f a) t a b

type Fold s t a b = ∀ c . FromFold c => Optic c s t a b

newtype FoldlTrait s t a b α β σ τ = FoldlTrait (b -> α -> b)

instance FromIso FoldlTrait where
  iso g _ = to g
instance FromLens FoldlTrait where
  lens g _ = to g
instance FromGetter FoldlTrait where
  to g = Accessor $ \(FoldlTrait q) -> FoldlTrait $ \b -> q b . g
instance FromFold FoldlTrait where
  folded = Accessor $ \(FoldlTrait q) -> FoldlTrait $ Foldable.foldl' q

type AFoldl s t a b = Optic FoldlTrait s t a b

foldl'Of :: AFoldl s t a b -> (b -> a -> b) -> b -> s -> b
foldl'Of (Accessor f) y = case f $ FoldlTrait y of FoldlTrait w -> w

newtype FoldrTrait s t a b α β σ τ = FoldrTrait (α -> b -> b)

instance FromIso FoldrTrait where
  iso g _ = to g
instance FromLens FoldrTrait where
  lens g _ = to g
instance FromGetter FoldrTrait where
  to g = Accessor $ \(FoldrTrait q) -> FoldrTrait $ q . g
instance FromFold FoldrTrait where
  folded = Accessor $ \(FoldrTrait q) -> FoldrTrait . flip $ Foldable.foldr q

type AFoldr s t a b = Optic FoldrTrait s t a b

foldrOf :: AFoldr s t a b -> (a -> b -> b) -> b -> s -> b
foldrOf (Accessor f) y = case f $ FoldrTrait y of FoldrTrait w -> flip w

foldMapOf :: Monoid b => AFoldr s t a b -> (a -> b) -> s -> b
foldMapOf f g = foldrOf f (mappend . g) mempty

--  S E T T E R S

class FromTraversal c => FromSetter c where
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
instance FromTraversal SetterTrait where
  traversed = Accessor $ \(SetterTrait q)
            -> SetterTrait $ freeVersing q
instance FromSetter SetterTrait where
  sets f = Accessor $ \(SetterTrait q) -> SetterTrait $ f q

type ASetter s t a b = Optic SetterTrait s t a b

infixr 4 %~, .~

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
Accessor f %~ m = case f $ SetterTrait m of SetterTrait q -> q

(.~) :: ASetter s t a b -> b -> s -> t
a .~ b = a %~ const b

