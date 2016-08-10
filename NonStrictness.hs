module NonStrictness where

import Debug.Trace (trace)

----------------------------------------------------------------------
-- * Introduction
----------------------------------------------------------------------

-- |
--
-- Haskell is non-strict.
--
-- Haskell is not purely lazy:
--
--   1. Pattern matching is (usually) strict
--
--   2. Strictness analysis
--
--
-- References:
--
--   * Haskell Programming from First Principles
--
--   * Haskell wiki

----------------------------------------------------------------------
-- * Bottom (undefined)
----------------------------------------------------------------------

-- |
--
-- >>> undefined
-- *** Exception: Prelude.undefined
--
--
-- fst :: (a, b) -> a
-- fst (x, _) = x
--
-- >>> fst (1, undefined)
-- 1
--
-- >>> fst (undefined, 2)
-- *** Exception: Prelude.undefined
--
--
-- snd :: (a, b) -> b
-- snd (_, y) = y
--
-- >>> snd (undefined, 2)
-- 2

----------------------------------------------------------------------
-- Direction of evaluation (non-strictness = outside in)
----------------------------------------------------------------------

-- |
--
-- (a + b) * c
--
--   == (*) (a + b) c
--
--     *
--    / \
--   +   c
--  / \
-- a   b
--
--
-- >>> (1 + 2) * 3
-- 9
--
-- >>> trace "*" ((trace "+" (1 + 2)) * 3)
-- *
-- +
-- 9
--
--
-- _ * 0 = 0
-- 0 * _ = 0
--
-- >>> (undefined + undefined) * 0
-- 0

----------------------------------------------------------------------
-- Non-strictness versus strictness
----------------------------------------------------------------------

hypo1 :: IO ()
hypo1 = do
  let
    x :: Int
    x = undefined
  y <- getLine
  case y of
    "x" -> print x; _ -> putStrLn "y"


hypo2 :: IO ()
hypo2 = do
  let
    x :: Int
    x = undefined
  y <- getLine
  case seq x y of
    "x" -> print x; _ -> putStrLn "y"

----------------------------------------------------------------------
-- seq
----------------------------------------------------------------------

-- |
--
-- >>> :type seq
-- seq :: a -> b -> b
--
-- seq undefined b = undefined
-- seq _         b = b
--
--
-- >>> fst (1, seq undefined 2)
-- 1
--
--
-- >>> seq undefined 1
-- *** Exception: Prelude.undefined
--
-- >>> seq (undefined, undefined) 1
-- 1
-- >>> seq ((,) undefined undefined) 1
-- 1
--
-- >>> seq (\_ -> undefined) 1
-- 1
--
-- >>> seq (undefined + undefined) 1
-- *** Exception: Prelude.undefined

----------------------------------------------------------------------
-- Pattern matching is strict
----------------------------------------------------------------------

data Test1
  = A Test2
  | B Test2
  deriving (Show)


data Test2
  = C Int
  | D Int
  deriving (Show)


forceNothing :: Test1 -> Int
forceNothing _ = 0


forceTest1 :: Test1 -> Int
forceTest1 (A _) = 1
forceTest1 (B _) = 2


forceTest2 :: Test1 -> Int
forceTest2 (A (C n)) = n
forceTest2 (B (C n)) = n
forceTest2 (A (D n)) = n
forceTest2 (B (D n)) = n

-- |
--
-- >>> forceNothing undefined
-- 0
--
-- >>> forceNothing (A undefined)
-- 0
--
-- >>> forceNothing (A (C undefined))
-- 0
--
--
-- >>> forceTest1 undefined
-- *** Exception: Prelude.undefined
--
-- >>> forceTest1 (A undefined)
-- 1
--
-- >>> forceTest1 (A (C undefined))
-- 1
--
--
-- >>> forceTest2 undefined
-- *** Exception: Prelude.undefined
--
-- >>> forceTest2 (A undefined)
-- *** Exception: Prelude.undefined
--
-- >>> forceTest2 (A (C undefined))
-- *** Exception: Prelude.undefined
--
-- >>> forceTest2 (A (C 3))
-- 3
--
--
-- Core (:set -ddump-simpl -dsuppress-all)
--
--
-- forceNothing = \ _ -> I# 0
--
--
-- forceTest1 =
--   \ ds_d4HU ->
--     case ds_d4HU of _ {
--       A ds1_d4HV -> I# 1;
--       B ds1_d4HW -> I# 2
--     }
--
--
-- forceTest2 =
--   \ ds_d4HR ->
--     case ds_d4HR of _ {
--       A ds1_d4HS ->
--         case ds1_d4HS of _ {
--           C n_a4zO -> n_a4zO;
--           D n_a4zQ -> n_a4zQ
--         };
--       B ds1_d4HT ->
--         case ds1_d4HT of _ {
--           C n_a4zP -> n_a4zP;
--           D n_a4zR -> n_a4zR
--         }
--     }
--

discriminatory1 :: Bool -> Int
discriminatory1 b =
  case b of
    False -> 0
    True -> 1


discriminatory2 :: Bool -> Int
discriminatory2 b =
  let
    x = undefined
  in
    case b of
      False -> 0
      True -> 1


discriminatory3 :: Bool -> Int
discriminatory3 b =
  if b then 1 else 0


discriminatory4 :: Bool -> Int
discriminatory4 False = 0
discriminatory4 True = 1

-- |
--
-- discriminatory1 =
--   \ b_a4zS ->
--     case b_a4zS of _ {
--       False -> I# 0;
--       True -> I# 1
--     }
--
--
-- discriminatory2 =
--   \ b_a4zT ->
--     case b_a4zT of _ {
--       False -> I# 0;
--       True -> I# 1
--     }
--
--
-- discriminatory3 =
--   \ b_a4zV ->
--     case b_a4zV of _ {
--       False -> I# 0;
--       True -> I# 1
--     }
--
--
-- discriminatory4 =
--   \ ds_d6qf ->
--     case ds_d6qf of _ {
--       False -> I# 0#;
--       True -> I# 1#
--     }

hypo3 :: IO ()
hypo3 = do
  let
    x :: Int
    x = undefined
  y <- seq x getLine
  case seq x y of
    "x" -> print x; _ -> putStrLn "y"

----------------------------------------------------------------------
-- What we can do
----------------------------------------------------------------------

-- |
--
-- >>> let xs = [1, 2, 3]
-- >>> tail xs
-- [2,3]
--
--
-- >>> let xs = [undefined, 2, 3]
-- >>> tail xs
-- [2,3]
--
--
-- >>> import Data.List (sort)
--
-- >>> head (sort [1, 2, 3, undefined])
-- ???

----------------------------------------------------------------------
-- * Thunks
----------------------------------------------------------------------

-- |
--
-- >>> let xs = [1, 2, 3] :: [Int]
-- >>> :sprint xs
-- xs = [1,2,3]
--
--
-- >>> let xs = [1, 2, 3]
-- >>> :sprint xs
-- xs = _
-- >>> :type xs
-- xs :: Num t => [t]
--
--
-- >>> let xs = [1, 2, id 3] :: [Int]
-- >>> :sprint xs
-- xs = [1,2,_]
--
--
-- >>> let xs = [1, 2, 3] :: [Int]
-- >>> let ys = xs ++ undefined
-- >>> :sprint ys
-- ys = _

----------------------------------------------------------------------
-- * Sharing
----------------------------------------------------------------------

-- |
--
-- >>> let x = trace "x" 1
-- >>> let y = trace "y" 2
-- >>> x + y
-- y
-- x
-- 3


fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-- fibo n =
--   trace ("fibo " ++ show (n - 1)) (fibo (n - 1))
--     + trace ("fibo " ++ show (n - 2)) (fibo (n - 2))


-- |
--
-- >>> let x = trace "x" (1 :: Int)
-- >>> let y = trace "y" (1 :: Int)
-- >>> x + y
-- x
-- y
-- 2
--
--
-- >>> let x = trace "x" (1 :: Int)
-- >>> x + x
-- x
-- 2
-- >>> x + x
-- 2
--
--
-- >>> let x = trace "x" (1 :: Int)
-- >>> id x + id x
-- x
-- 2
-- >>> id x + id x
-- 2
--
--
-- >>> let mx = Just (trace "1" 1)
-- >>> :sprint mx
-- mx = _
-- >>> mx
-- Just 1
-- 1
-- >>> :sprint mx
-- mx = _
-- >>> :type mx
-- mx :: Num a => Maybe a

----------------------------------------------------------------------
-- ** Explicit sharing
----------------------------------------------------------------------

twice :: Int
twice =
  trace "1 + 1" (1 + 1)
    * trace "1 + 1" (1 + 1)


once :: Int
once =
  let
    x = trace "1 + 1" (1 + 1)
  in
    x * x


forever :: Monad m => m a -> m b
forever a =
  let
    a' = a >> a'
  in
    a'
