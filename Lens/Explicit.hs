-- |
-- Module      : Lens.Explicit
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ConstraintKinds #-}

module Lens.Explicit where


newtype Optic' l a s = Optic { runOptic :: l a a -> l s s }

class Æther c where
  data Light c a s α あ

--  G E T T E R S

class Æther c => ÆGet c where
  (^.) :: Optic' (Light c a s) a s -> s -> a

data Get

instance Æther Get where
  newtype Light Get a s α あ = Gets { runGets :: α -> a }
instance ÆGet Get where
  (^.) (Optic f) = runGets . f $ Gets id

--  S E T T E R S

class Æther c => ÆSet c where
  (%~) :: Optic' (Light c a s) a s -> (a -> a) -> s -> s

data Set

instance Æther Set where
  newtype Light Set a s α あ = Sets { runSets :: α -> あ }
instance ÆSet Set where
  (%~) (Optic f) = runSets . f . Sets


--  L E N S E S

type ÆLens c = (ÆGet c, ÆSet c)

data Lense

instance Æther Lense where
  data Light Lense a s α あ = Lenss { lensGets :: α -> a, lensSets :: α -> あ }
instance ÆGet Lense where
  (^.) (Optic f) = lensGets . f $ Lenss id id
instance ÆSet Lense where
  (%~) (Optic f) = lensSets . f . Lenss id

-- lens :: (s -> a) -> (s -> a -> s) -> Optic (Light Lense a s) a s
-- lens g s = Optic $ \(Lenss gs ss) -> Lenss _ _


--  I S O S

class ÆLens c => ÆIso c where
  from :: Optic' (Light c a s) a s -> Optic' (Light c s a) s a

data Ise

instance Æther Ise where
  data Light Ise a s α あ = Iss { isoGets :: α -> a, isoSets :: s -> あ }
instance ÆGet Ise where
  (^.) (Optic f) = isoGets . f $ Iss id undefined
instance ÆSet Ise where
  (%~) (Optic f) s = isoSets . f $ Iss id _
-- class ÆIso Ise where
--   from
