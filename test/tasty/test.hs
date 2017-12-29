-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE LambdaCase    #-}

module Main where

import Lens.Explicit

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding ((.), id)


main = defaultMain tests


data Foo = Foo
      { _a :: Int
      , _b :: String
      } deriving (Eq,Show)

a :: Lens' Foo Int
a = lens _a $ \s x -> s{_a=x}

b :: Lens' Foo String
b = lens _b $ \s x -> s{_b=x}

foo :: Foo
foo = Foo 700 "foo"

data Bar c = Bar
      { _c :: c
      , _d :: Bool
      } deriving (Eq,Show)

c :: Lens (Bar c) (Bar ζ) c ζ
c = lens _c $ \s x -> Bar x (_d s)

d :: Lens' (Bar c) Bool
d = lens _d $ \s x -> s{_d=x}

bar :: Bar Double
bar = Bar pi True

foobar :: Bar Foo
foobar = Bar foo False

negated :: Iso' Int Int
negated = iso negate negate

data Zab f = Fab f Bool
           | Fob String Int
       deriving (Eq,Show)

_Fab :: Prism (Zab f) (Zab g) (Bar f) (Bar g)
_Fab = prism (\(Bar ζ δ) -> Fab ζ δ)
             (\case
               Fab ζ δ -> Right $ Bar ζ δ
               Fob β α -> Left $ Fob β α )

_Fob :: Prism' (Zab f) Foo
_Fob = prism (\(Foo α β) -> Fob β α)
             (\case
               Fab ζ δ -> Left $ Fab ζ δ
               Fob β α -> Right $ Foo α β )



tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Getting"
     [ testCase "Monomorphic a" $ foo^.a @?= 700
     , testCase "Monomorphic b" $ foo^.b @?= "foo"
     , testCase "Polymorphic c" $ bar^.c @?= pi
     , testCase "Monomorphic d" $ bar^.d @?= True
     , testCase "Nested c.a" $ foobar^.c.a @?= 700
     , testCase "Nested d.id" $ foobar^.d.id @?= False
     , testCase "Composed a.negated" $ foo^.a.negated @?= -700
     , testCase "Reviewing _Fab" $ bar^.re _Fab @?= Fab pi True
     , testCase "Reviewing _Fob" $ foo^.re _Fob @?= (Fob "foo" 700 :: Zab ())
     ]
  , testGroup "Setting"
     [ testCase "Monomorphic a"
          $ (foo & a.~900) @?= Foo 900 "foo"
     , testCase "Monomorphic b"
          $ (foo & b%~(++"p")) @?= Foo 700 "foop"
     , testCase "Polymorphic c"
          $ (bar & c.~314) @?= Bar 314 True
     , testCase "Monomorphic d"
          $ (bar & d%~not) @?= Bar pi False
     , testCase "Nested c.a"
          $ (foobar & c.a .~ 900) @?= Bar (Foo 900 "foo") False
     , testCase "Nested d.id"
          $ (foobar & d.id %~ not) @?= Bar (Foo 700 "foo") True
     , testCase "Composed a.negated"
          $ (foo & a.negated %~ (+2)) @?= Foo 698 "foo"
     ]
  , testGroup "Lensing"
     [ testCase "Monomorphic a"
          $ (foo & a%%~Just .const 900) @?= Just (Foo 900 "foo")
     , testCase "Monomorphic b"
          $ (foo & b%%~Just.(++"p")) @?= Just (Foo 700 "foop")
     , testCase "Polymorphic c"
          $ (bar & c%%~Just .const 314) @?= Just (Bar 314 True)
     , testCase "Monomorphic d"
          $ (bar & d%%~Just .not) @?= Just (Bar pi False)
     , testCase "Nested c.a"
          $ (foobar & c.a %%~ Just .const 900) @?= Just (Bar (Foo 900 "foo") False)
     , testCase "Nested d.id"
          $ (foobar & d.id %%~ Just .not) @?= Just (Bar (Foo 700 "foo") True)
     , testCase "Composed a.negated"
          $ (foo & a.negated %%~ Just.(+2)) @?= Just (Foo 698 "foo")
     ]
  ]



