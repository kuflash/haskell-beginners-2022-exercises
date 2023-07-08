{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , Chest (..)
    , DragonKind (..)
    , Dragon (..)
    , Reward (..)
    , FightResult (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV

import Data.Char (isSpace)

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct list =
  case list of
    [] -> 1
    (x:xs)
      | x == 0 -> 0
      | otherwise -> x * lazyProduct xs

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate list =
  case list of
    [] -> []
    (x:xs) -> x : x : duplicate xs

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt pos list = if pos < 0 then (Nothing, list) else walkAndRemove 0 [] list
  where
    walkAndRemove :: Int -> [a] -> [a] -> (Maybe a, [a])
    walkAndRemove currentPosition acc listTail =
      case listTail of
        [] -> (Nothing, acc)
        (x:xs)
          | currentPosition == pos -> (Just x, acc ++ xs)
          | currentPosition > pos -> (Nothing, acc ++ xs)
          | otherwise -> walkAndRemove (currentPosition + 1) (acc ++ [x]) xs

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: String -> String
dropSpaces = takeWhile (not . isSpace) . dropWhile isSpace

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
    When defining the type of a treasure chest, you don't know what
    treasures it stores inside, so your chest data type must be able
    to contain any possible treasure.
  * As a reward, the knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons have only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay a dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, the dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If a knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)

type Health = Int
type Attack = Int

data Chest a = Chest
    { chestGold :: Int,
      chestTreasure :: a
    }

data Knight = Knight
    { knightHealth    :: Health,
      knightAttack    :: Attack,
      knightEndurance :: Int
    }

data DragonKind
    = RedDragon
    | BlackDragon
    | GreenDragon

data Dragon a = Dragon
    { dragonKind :: DragonKind,
      dragonHealth :: Health,
      dragonAttack :: Attack,
      dragonChest :: Chest a
    }

data Reward a = Reward
    { rewardGold :: Int,
      rewardTreasure :: Maybe a,
      rewardExperience :: Int
    }

data FightResult a = Win (Reward a) | Die | Run

getExperience :: DragonKind -> Int
getExperience dragonKind =
  case dragonKind of
    RedDragon -> 100
    BlackDragon -> 150
    GreenDragon -> 250

getReward :: Dragon a -> Reward a
getReward dragon =
  case dragonKind dragon of
    GreenDragon ->
      Reward {
        rewardGold = chestGold (dragonChest dragon),
        rewardTreasure = Nothing,
        rewardExperience = getExperience (dragonKind dragon)
      }
    _ ->
      Reward {
        rewardGold = chestGold (dragonChest dragon),
        rewardTreasure = Just (chestTreasure (dragonChest dragon)),
        rewardExperience = getExperience (dragonKind dragon)
      }

attackDragon :: Knight -> Dragon a -> (Knight, Dragon a)
attackDragon knight dragon  =
  (
    knight { knightEndurance = knightEndurance knight - 1 },
    dragon { dragonHealth = dragonHealth dragon - knightAttack knight }
  )

attackKnight :: Dragon a -> Knight -> (Knight, Dragon a)
attackKnight dragon knight  =
  (
    knight { knightHealth = knightHealth knight - dragonAttack dragon },
    dragon
  )

attack :: (Knight, Dragon a) -> Int -> (Knight, Dragon a)
attack (knight, dragon) attackNumber
    | attackNumber `mod` 10 == 0 = attackKnight dragon knight
    | otherwise = attackDragon knight dragon

isDragonDie :: Dragon a -> Bool
isDragonDie dragon = dragonHealth dragon <= 0

isKnightDie :: Knight -> Bool
isKnightDie knight = knightHealth knight <= 0

isKnightRun :: Knight -> Bool
isKnightRun knight = knightEndurance knight <= 0

dragonFight :: Knight -> Dragon a -> FightResult a
dragonFight knight dragon = fight (knight, dragon) 0
  where
    fight :: (Knight, Dragon a) -> Int -> FightResult a
    fight (actualKnight, actualDragon) attackNumber
      | isKnightDie actualKnight = Die
      | isKnightRun actualKnight = Run
      | isDragonDie actualDragon = Win (getReward actualDragon)
      | otherwise = fight (attack (actualKnight, actualDragon) (attackNumber + 1 )) (attackNumber + 1 )

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing list =
  case list of
    [] -> True
    [_] -> True
    (x:y:xs)
      | x > y -> False
      | otherwise -> isIncreasing (y:xs)

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge list1 list2 =
  case (list1, list2) of
    ([], []) -> []
    (as, []) -> as
    ([], bs) -> bs
    (a:as, b:bs)
      | a < b -> a : merge as (b:bs)
      | otherwise -> b : merge (a:as) bs

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort [a,b]
  | a > b = merge [b] [a]
  | otherwise = merge [a] [b]
mergeSort list = merge (mergeSort (snd parts)) (mergeSort (fst parts))
  where
    listLength = length list
    middle = div listLength 2
    parts = splitAt middle list

{- | Haskell is famous for being a superb language for implementing
compilers and interpreters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit l) = Right l
eval vars (Var v) =
  case lookup v vars of
    (Just value) -> Right value
    _ -> Left (VariableNotFound v)
eval vars (Add fe se) =
  case (eval vars fe, eval vars se) of
    (Left fr, _) -> Left fr
    (_, Left sr) -> Left sr
    (Right fr, Right sr) -> Right (fr + sr)


{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}
constantFolding :: Expr -> Expr
constantFolding expression = build (collect [] 0 expression)
  where
    collect :: [String] -> Int -> Expr -> ([String], Int)
    collect vars foldedConst expr =
      case expr of
        (Lit l) -> (vars, foldedConst + l)
        (Var v) -> (vars ++ [v], foldedConst)
        (Add e1 e2) ->
          let
            e1Parts = collect vars foldedConst e1
            e2Parts = collect vars foldedConst e2
          in
            (vars ++ fst e1Parts ++ fst e2Parts, foldedConst + snd e1Parts + snd e2Parts)
    
    build :: ([String], Int) -> Expr
    build parts =
      case parts of
        ([], c) -> Lit c
        ([v], c)
          | c == 0 -> Var v
          | otherwise -> Add (Var v) (Lit c)
        (v:vs, c) -> Add (Var v) (build (vs, c))
