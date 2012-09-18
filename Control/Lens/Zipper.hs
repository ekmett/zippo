{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Zipper
-- Copyright   :  (C) 2012 Brandon Simmons
--                (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- We provide a simple, heterogenous, fully type-checked, generic zipper
-- implementation. This flavor of zipper doesn\'t use \"left\" and \"right\" for
-- navigation, and instead relies on lenses to indicate a child type to \"move
-- to\".
-------------------------------------------------------------------------------
module Control.Lens.Zipper (
  -- * Zipper type
    Zipper(..)
  -- ** Zipper history
  -- $history
  , Top(Top)
  , (:>)()
  , Hist(..)
  -- * Zipper operations
  -- ** Horizontal Motion
  , Zipped(..)
  -- ** Vertical Motion
  , up
  , down
  , move
  -- ** Starting and Stopping
  , zipper
  , close
  -- ** Internals
  , Level
  , closeLevel
  , level
 ) where

{- TODO
-      - excellent rewrite rules
-          - first look at core output of simple example
-          - add rules one-by-one, looking at core
-      - consider a newtype-wrapped submodule encapsulating monad return value
-          what we really want is a notation like:
-              move x
-              move y
-              foc <- move z
-              moveUp
-              modf (+foc)
-          quasiquotation, or can we shoe-horn this into proc notation?
-          otherwise add those combinators (in notes)
-      - more advanced motions a.la. pez?
-      - better demos
-      - pretty Show instance for Zipper
-}

import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Lens.Internal
import Control.Monad.Identity
import Data.Maybe
import Data.Foldable
import Data.Traversable

class Zipped f where
  -- | To read the current focus, use @'view' 'focus'@.
  --
  -- You can also @'set' 'focus'@ or @'over' 'focus'@ or use any other lens combinator.
  focus :: Simple Lens (f a) a

  -- | Try to move the 'Zipper' 'left' through the current 'Traversal'.
  left :: f a -> Maybe (f a)

  -- | Try to move the 'Zipper' 'right' through the current 'Traversal'.
  right :: f a -> Maybe (f a)

-- | A traditional list zipper. The constructor is not exported to preserve the invariant that
-- the number of elements in this zipper should not be allowed to change.
--
-- /Clowns to the left of me,
-- Jokers to the right, here I am,
-- Stuck in the middle with you./
data Level a = Level [a] a [a]

instance Functor Level where
  fmap f (Level ls a rs) = Level (f <$> ls) (f a) (f <$> rs)

instance Foldable Level where
  foldMap f = foldMap f . closeLevel

instance Traversable Level where
  traverse f (Level ls a rs) = Level <$> backwards traverse f ls <*> f a <*> traverse f rs

closeLevel :: Level a -> [a]
closeLevel (Level ls a rs) = reverse ls ++ a : rs

instance Comonad Level where
  extract (Level _ a _) = a
  extend f w@(Level ls m rs) = Level (gol (m:rs) ls) (f w) (gor (m:ls) rs) where
    gol zs [] = []
    gol zs (y:ys) = f (Level ys y zs) : gol (y:zs) ys
    gor ys [] = []
    gor ys (z:zs) = f (Level ys z zs) : gor (z:ys) zs

instance Zipped Level where
  focus f (Level ls a rs) = (\a' -> Level ls a' rs) <$> f a
  left (Level []     _ _ ) = Nothing
  left (Level (l:ls) a rs) = Just (Level ls l (a:rs))
  right (Level _  _ []    ) = Nothing
  right (Level ls a (r:rs)) = Just (Level (a:ls) r rs)

-- | Our zipper type, parameterized by a 'focus' and \"history stack\",
-- supporting completely type-checked zipper operations.
data Zipper h a = Zipper (h a) (Level a)

instance Zipped (Zipper h) where
  focus = level.focus
  left (Zipper h w) = Zipper h <$> left w
  right (Zipper h w) = Zipper h <$> right w

-------------------------------------------------------------------------------
-- Zipper History
-------------------------------------------------------------------------------

-- $zipper
-- These three types make up the heterogenous history stack, and the programmer
-- should never need to use them directly. They come together with 'Zipper' to
-- form types that look like, e.g.
--
-- @
-- -- a zipper on a Tree, with focus on a leaf "a" of a 2nd level subtree
-- z :: 'Zipper' ('Top' ':>' Tree a ':>' Tree a) a
-- @
--
-- This zipper works conceptually like the \"breadcrumbs\" navigation UI design
-- pattern, and the types reflect this visually.
--
-- Nevertheless user-provided type annotations should almost never be
-- necessary, so these will probably never appear in your code.

data (:>) h b c = Snoc (h b) [b] ([c] -> b) [b]
data Top a = Top

level :: Simple Lens (Zipper h a) (Level a)
level f (Zipper h w) = Zipper h <$> f w

-- | \"enter\" a data type. Move the 'focus' with 'left', 'right', 'down' and 'up'. Exit
-- the zipper with 'close'.
--
-- @'zipper' = 'Zipper' 'Top'@
zipper :: a -> Zipper Top a
zipper a = Zipper Top (Level [] a [])

class Hist h a c  where
     runHist :: h c -> [c] -> a

-- | Our only use of @TypeFamilies@. Thanks to Daniel Wagner for this trick.
instance a ~ b => Hist Top a b where
     runHist _ = head

instance Hist h a b => Hist (h :> b) a c where
     runHist (Snoc h ls k rs) cs = runHist h (reverse ls ++ k cs : rs)

-- | Exit the 'Zipper', rebuilding the structure @a@:
close :: Hist h a b => Zipper h b -> a
close (Zipper h w) = runHist h (closeLevel w)

-- | Navigate up a level in a 'Zipper' not already at 'Top'.
up :: Zipper (st :> b) c -> Zipper st b
up (Zipper (Snoc h ls k rs) w) = Zipper h (Level ls (k (closeLevel w)) rs)

-- | Navigate to the target of the supplied 'Lens'
--
-- @'down' :: 'Simple' 'Lens' a b -> 'Zipper' h a -> 'Zipper' (h ':>' a) b@
down :: SimpleLensLike (Context c c) b c -> Zipper h b -> Zipper (h :> b) c
down l (Zipper h (Level ls b rs)) = case l (Context id) b of
  Context k c -> Zipper (Snoc h ls (k . head) rs) (Level [] c [])

-- | Navigate to the first element indicated by the passed 'Traversal'.
--
-- You can move 'left' and 'right' through the 'Traversal' thereafter.
--
-- @'move' :: 'Simple' 'Traversal' a b -> 'Zipper' h a -> 'Maybe' ('Zipper' (h ':>' a) b)@
--
move :: SimpleLensLike (Bazaar c c) b c -> Zipper h b -> Maybe (Zipper (h :> b) c)
move l (Zipper h (Level ls b rs)) = case partsOf l (Context id) b of
  Context _ []     -> Nothing
  Context k (c:cs) -> Just (Zipper (Snoc h ls k rs) (Level [] c cs))
