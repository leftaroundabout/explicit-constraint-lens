-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Main where

import Lens.Explicit

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding ((.), id)
import Control.Category


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

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Getting"
     [ testCase "Monomorphic a" $ foo^.a @?= 700
     , testCase "Monomorphic b" $ foo^.b @?= "foo"
     , testCase "Polymorphic c" $ bar^.c @?= pi
     , testCase "Monomorphic d" $ bar^.d @?= True
     , testCase "Nested c.a" $ foobar^.c.a @?= 700
     , testCase "Nested d.id" $ foobar^.d.id @?= False
     ]
  ]



