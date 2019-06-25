{-# LANGUAGE Rank2Types #-}
-- |
-- Module       : System.Process.Microlens.Internal
-- Copyright 	: 2019 Emily Pillmore
-- License	: BSD
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies
--
-- This module provides the prisms missing form 'microlens'.
--
module System.Process.Microlens.Internal
( Prism
, Prism'
, prism
)where


type Optic p s t a b = p a b -> p s t

class Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> p a b -> p s t

class Profunctor p => Cocartesian p where
  right :: p a b -> p (Either c a) (Either c b)

type Prism s t a b = forall p. Cocartesian p => Optic p s t a b
type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism f g = dimap g (either id f) . right
