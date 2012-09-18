{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses, FlexibleContexts
           , TypeFamilies
           , FlexibleInstances #-}
module Data.Lens.Zipper (
{- |
   We provide a simple, heterogenous, fully type-checked, generic zipper
   implementation. This flavor of zipper doesn\'t use \"left\" and \"right\" for
   navigation, and instead relies on lenses to indicate a child type to \"move
   to\".
-}

  -- * Zipper type
    Zipper(..)
  -- ** Zipper history
{- |
   These three types make up the heterogenous history stack, and the programmer
   should never need to use them directly. They come together with 'Zipper' to
   form types that look like, e.g.

   > -- a zipper on a Tree, with focus on a leaf "a" of a 2nd level subtree
   > z :: Zipper (Top :> Tree a :> Tree a) a

   This zipper works conceptually like the \"breadcrumbs\" navigation UI design
   pattern, and the types reflect this visually.

   Nevertheless user-provided type annotations should almost never be
   necessary, so these will probably never appear in your code.
-}
  , Top(..) , (:>)(..) , Hist(..)

  -- * Zipper operations
  , zipper , close
  -- ** Motions
  , move , moves, moveM , moveUp
  -- ** Focus
{- |
   In addition to these, 'viewf' can be used to view the focus.
-}
  , focus , setf , modf

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
import Control.Lens
import Control.Lens.Internal
import Control.Monad.Identity
import Data.Maybe

-- | Our zipper type, parameterized by a 'focus' and \"history stack\",
-- supporting completely type-checked zipper operations.
data Zipper st b = Zipper { hist  :: st b , viewf :: b }
data (:>) st b c = Snoc (st b) (c -> b)
data Top a = Top

-- | A lens on the focus of the zipper.
focus :: Simple Lens (Zipper st b) b
focus f (Zipper h v) = Zipper h <$> f v

-- | Set the zipper focus.
--
-- @'setf' = 'set' 'focus'@
setf :: Zipper st b -> b -> Zipper st b
setf z b = set focus b z

-- | Modify the zipper focus.
--
-- @'modf' = 'modify' 'focus'@
modf :: (b -> b) -> Zipper st b -> Zipper st b
modf = over focus

-- | \"enter\" a data type. Move the 'focus' with 'move' and 'moveUp'. Exit
-- the zipper with 'close'.
--
-- @'zipper' = 'Zipper' 'Top'@
zipper :: a -> Zipper Top a
zipper = Zipper Top

class Hist st a c  where
     runHist :: st c -> (c -> a)
-- | Our only use of @TypeFamilies@. Thanks to Daniel Wagner for this trick:

instance a ~ b => Hist Top a b where
     runHist _ = id

instance Hist st a b => Hist (st :> b) a c where
     runHist (Snoc st' cb) = runHist st' . cb

-- | Exit the zipper, rebuilding the structure @a@:
--
-- @'close' ('Zipper' st b) = 'runHist' st b@
close :: Hist st a b => Zipper st b -> a
close (Zipper st b) = runHist st b

-- | Navigate to child elements indicated by the passed 'Traversal', returning the new zippers in a list.
--
-- @
-- 'moves' :: 'Simple' 'Lens' b c      -> 'Zipper' st b -> ['Zipper' (st ':>' b) c]
-- 'moves' :: 'Simple' 'Traversal' b c -> 'Zipper' st b -> ['Zipper' (st ':>' b) c]
-- @
moves :: SimpleLensLike (Bazaar c c) b c -> Zipper st b -> [Zipper (st :> b) c]
moves l (Zipper st b) = map (\(Context f c) -> Zipper (Snoc st f) c) (holesOf l b)

-- | Navigate to the first element indicated by a 'Traversal'.
moveM :: SimpleLensLike (Bazaar c c) b c -> Zipper st b -> Maybe (Zipper (st :> b) c)
moveM l = listToMaybe . moves l

-- | Navigate to a child element indicated by the passed pure lens.
--
-- @
-- 'move' :: 'Simple' 'Lens' b c -> 'Zipper' st b -> ['Zipper' (st ':>' b) c]
-- @
move :: SimpleLensLike (Context c c) b c -> Zipper st b -> Zipper (st :> b) c
move l (Zipper st b) = case l (Context id) b of
  Context f c -> Zipper (Snoc st f) c

-- | navigate up a level in a zipper not already at 'Top'
--
-- @'moveUp' ('Zipper' ('Snoc' st cont) c) = 'Zipper' st '$' cont c@
moveUp :: Zipper (st :> b) c -> Zipper st b
moveUp (Zipper (Snoc st cont) c) = Zipper st $ cont c
