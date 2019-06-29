{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnsaturatedTypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Symbol.Parser where

import GHC.TypeLits
import Data.Symbol.Ascii

import qualified Type.Prelude as Ty
import Type.Control.Monad.State

-- PARSER

type Input = [Symbol] -- assumed to be single chars

data Parser i a = MkParser (State i (Maybe a))

type family RunParser (p :: Parser i a) :: State i (Maybe a) where
  RunParser ('MkParser p) = p
  
type family Fail :: Parser i a where
  Fail = 'MkParser (Ty.Pure 'Nothing)

-- Instances for Parser

instance Ty.Functor (Parser i) where
  type Fmap f ('MkParser p) = 'MkParser (Ty.Fmap f Ty.<$> p)

instance Ty.Applicative (Parser i) where
  type Pure x = 'MkParser (Ty.Pure (Ty.Pure x))

type family BindParserImpl2
  (m :: (i, Maybe a))
  (k :: a -> Parser i b)
  :: (i, Maybe b) where
  BindParserImpl2 '(s, 'Nothing) k = '(s, 'Nothing)
  BindParserImpl2 '(s, 'Just x) k = RunState (RunParser (k x)) s

type family BindParserImpl
  (p :: i ~> (i, Maybe a))
  (k :: a -> Parser i b)
  (s :: i)
  :: (i, Maybe b) where
  BindParserImpl p k s = BindParserImpl2 (p s) k

instance Ty.Monad (Parser i) where
  type 'MkParser p >>= k = 'MkParser ('MkState (BindParserImpl (RunState p) k))

type family AlternativeParserImpl2
  (p :: (i, Maybe a))
  (q :: (i, Maybe a)) where
  AlternativeParserImpl2 '(s1, 'Just x) _ = '(s1, 'Just x)
  AlternativeParserImpl2 '(_, 'Nothing) '(s2, x) = '(s2, x)

type family AlternativeParserImpl
  (p :: i ~> (i, Maybe a))
  (q :: i ~> (i, Maybe a))
  (s :: i) where
  AlternativeParserImpl p q s = AlternativeParserImpl2 (p s) (q s)

instance Ty.Alternative (Parser i) where
  type Empty = Fail
  type 'MkParser p <|> 'MkParser q = 'MkParser ('MkState (AlternativeParserImpl (RunState p) (RunState q)))

-- Parser primitives

type Predicate a = a ~> Bool

type family SatisfyImpl2 b c cs where
  SatisfyImpl2 'False c cs = '(c ': cs, 'Nothing)
  SatisfyImpl2 'True c cs = '(cs, 'Just c)

type family SatisfyImpl (f :: Predicate Symbol) (s :: Input)
  :: (Input, Maybe Symbol) where
  SatisfyImpl f '[] = '( '[], 'Nothing)
  SatisfyImpl f (c ': cs) = SatisfyImpl2 (f c) c cs

type family Satisfy (f :: Predicate Symbol) :: Parser Input Symbol where
  Satisfy f = 'MkParser ('MkState (SatisfyImpl f))

-- type family StringImpl (s :: [Symbol]) :: Parser Input Symbol where
--   StringImpl
-- 
-- type family String (s :: Symbol) :: Parser Input Symbol where
--   String s = StringImpl (ToList s)
  
