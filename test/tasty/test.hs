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



main = defaultMain tests


data Foo = Foo
      { _a :: Int
      , _b :: String
      } deriving (Eq,Show)

a :: Lens' Foo Int
a = lens _a $ \s x -> s{_a=x}

b :: Lens' Foo String
b = lens _b $ \s x -> s{_b=x}

data Bar c = Bar
      { _c :: c
      , _d :: Bool
      } deriving (Eq,Show)

c :: Lens (Bar c) (Bar ζ) c ζ
c = lens _c $ \s x -> Bar x (_d s)

d :: Lens' (Bar c) Bool
d = lens _d $ \s x -> s{_d=x}

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Getting"
     [ 
     ]
  ]



