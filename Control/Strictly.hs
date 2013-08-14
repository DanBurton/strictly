
-- | Turn a function lazy in its arguments
-- into a function strict in its arguments.
-- 
-- It should be noted that this does nothing to change
-- the internals of a function. If the function is lazy
-- on the inside, these combinators cannot fix that.
-- They only change the external entry point to the function:
-- the indicated number of arguments will be forced
-- before attempting to evaluate the function.
-- 
-- For finer control over evaluation strategies,
-- use the @parallel@ package.
module Control.Strictly (
  -- * Weak Head Normal Form
  strictly1,
  strictly2,
  strictly3,
  -- * Normal Form
  veryStrictly1,
  veryStrictly2,
  veryStrictly3,
  ) where

import Control.DeepSeq (NFData, deepseq)

infixl 0 `strictly1`
infixl 0 `veryStrictly1`


-- | Equivalent to @$!@
-- 
-- @strictly1@ is idempotent.
-- 
-- > strictly1 (strictly1 f) x ≡ strictly1 f x
strictly1 :: (a -> b) -> a -> b
strictly1 f = \a -> a `seq` f a
{-# INLINE strictly1 #-}

strictly2 :: (a -> b -> c) -> a -> b -> c
strictly2 f = \a b -> a `seq` b `seq` f a b
{-# INLINE strictly2 #-}

strictly3 :: (a -> b -> c -> d) -> a -> b -> c -> d
strictly3 f = \a b c -> a `seq` b `seq` c `seq` f a b c
{-# INLINE strictly3 #-}


-- | Equivalent to @$!!@
-- 
-- @veryStrictly1@ is idempotent.
-- 
-- > veryStrictly1 (veryStrictly1 f) x ≡ veryStrictly1 f x
veryStrictly1 :: (NFData a) => (a -> b) -> a -> b
veryStrictly1 f = \a -> a `deepseq` f a
{-# INLINE veryStrictly1 #-}

veryStrictly2 :: (NFData a, NFData b) => (a -> b -> c) -> a -> b -> c
veryStrictly2 f = \a b -> a `deepseq` b `deepseq` f a b
{-# INLINE veryStrictly2 #-}

veryStrictly3 :: (NFData a, NFData b, NFData c) => (a -> b -> c -> d) -> a -> b -> c -> d
veryStrictly3 f = \a b c -> a `deepseq` b `deepseq` c `deepseq` f a b c
{-# INLINE veryStrictly3 #-}
