{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad

-- List of adventurers
data Adventurers = P1 | P2 | P5 | P10 deriving (Show, Eq)

-- Objetcs of the game (Adventureres + Lantern)
type Objetc = Either Adventurers ()

-- Time that each adventurer needs to cross the dridge
getTimeAdv :: Adventurers -> Int
getTimeAdv P1  = 1
getTimeAdv P2  = 2
getTimeAdv P5  = 5
getTimeAdv P10 = 10

------------------------------------------------------------------------------------------------
{-- Game State MEMORY:
 - The state of the game, i.e the current position of each objetc (Adventures and Lantern). 
 - The function (const False) represents the initial state of the game, with all Adventurers 
and the Lantern on the left side of the bridge.
 - Similarly, the function (const True) represents the end state of the game, with all 
adventurers and the lantern on the right side of the bridge. --}

-- Game Memory
type State = Objetc -> Bool

instance Show State where
    show s = (show . (fmap show)) [s (Left P1),
                                   s (Left P2),
                                   s (Left P5),
                                   s (Left P10),
                                   s (Right ())]

instance Eq State where
    (==) s1 s2 = and [s1 (Left P1)  == s2 (Left P1),
                      s1 (Left P2)  == s2 (Left P2),
                      s1 (Left P5)  == s2 (Left P5),
                      s1 (Left P10) == s2 (Left P10),
                      s1 (Right ()) == s2 (Right ())]

------------------------------------------------------------------------------------------------
-- Initial state of the Game
gInit :: State
gInit = const False

-- Desired final state of the Game
gEnd :: State
gEnd = const True

-- Changes the 'State' of the game for a given 'Object'
changeState :: Objetc -> State -> State
changeState a s = let v = s a
                  in (\x -> if x == a then not v else s x)

-- Changes the 'State' of the Game of a list of 'Objects'
mChangeState :: [Objetc] -> State -> State
mChangeState os s = foldr changeState s os

-- List of all moves
possibleMoves :: [[Objetc]]
possibleMoves = [
  [],                      -- No 'Object' changes state/ position {-- Optional Task 1 --}
  [(Left P1)],             -- 'Adventure P1' changes state/ position
  [(Left P2)],             -- 'Adventure P2' changes state/ position
  [(Left P5)],             -- 'Adventure P5' changes state/ position
  [(Left P10)],            -- 'Adventure P10' changes state/ position
  [(Left P2), (Left P1)],  -- 'Adventure P2 & P1' changes state/ position
  [(Left P5), (Left P1)],  -- 'Adventure P5 & P1' changes state/ position
  [(Left P10), (Left P1)], -- 'Adventure P10 & P1' changes state/ position
  [(Left P5), (Left P2)],  -- 'Adventure P5 & P2' changes state/ position
  [(Left P10), (Left P2)], -- 'Adventure P10 & P2' changes state/ position
  [(Left P10), (Left P5)]] -- 'Adventure P10 & P5' changes state/ position

-- Given a 'State' calculates the list of possible moves with respect to the 'Latern' position
moves :: State -> [[Objetc]]
moves s = filter (\list -> and (map (\a -> (s a) == (s (Right ()))) list)) possibleMoves

-- Changes the 'State' of the 'Objects' inside a list and gives a 'Duration State' with the 
-- time needed to perform the move and the resulting 'State' 
fun :: State -> [Objetc] -> Duration State
fun s [] = Duration (0, s)
fun s (h:t) = Duration (getTimeAdv((\(Left a) -> a) h), mChangeState ((h:t)++[(Right ())]) s)

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make. --}
allValidPlays :: State -> ListDur State
allValidPlays s = LD (map (fun s) (moves s))

------------------------------------------------------------------------------------------------
-- Receives the number n (number of individual moves), a function 'allValidPlays' and a 'State', 
-- and returns a 'List Duration' with all the moves that the adventures can make.
propagate :: Int -> (State -> ListDur State) -> State -> ListDur State
propagate 0 _   s = pure s
propagate 1 s_l s = s_l s
propagate n s_l s = do 
                     r <- (s_l s)
                     propagate (n-1) s_l r

{-- For a given number n and initial state, the function calculates all possible n-sequences 
of moves that the adventures can make. --}
exec :: Int -> State -> ListDur State
exec n s = propagate n (allValidPlays) s

------------------------------------------------------------------------------------------------
-- List of all 'Objects' that are 'Adventurers' 
players :: [Objetc]
players = [(Left P1), (Left P2), (Left P5), (Left P10)]

-- For a given 'Duration State' checks if the time is <=17 and if all players are at the 'State'
-- True, right side of the bridge (Safe!)
fun' :: Duration State -> Bool
fun' (Duration (i, x)) = ((<= 17) i) && (and (map x players))

{-- Is it possible for all adventurers to be on the other side in <=17 min and not exceeding 
5 moves ? --}
leq17 :: Bool
leq17 = let r = remLD (exec 5 gInit)
        in or (map (fun') r)

------------------------------------------------------------------------------------------------
-- 1st approach
------------------------------------------------------------------------------------------------
-- For a given 'Duration State' checks if the time is <17 and if all players are at the 'State'
-- True, right side of the bridge (Safe!)
funn :: Duration State -> Bool
funn (Duration (i, x)) = ((< 17) i) && (and (map x players))

{-- Is it possible for all adventurers to be on the other side in < 17 min ? --}
l17 :: Bool
l17 = let r = remLD (exec 17 gInit)
        in or (map (funn) r)

-------------------------------------------------------------------------------------------------
{-- Optional Task 2 --}
-------------------------------------------------------------------------------------------------
-- Another way to see if "it is possible for all adventurers to be on the other side in < 17 min"
-- is to star with n=1 and see if there are at least one 'State' with time <17 and all the
-- 'Adventurers' are in the safe state, if False we increase n to n+1 until we can conclude if 
-- it is possible for all adventurers to be on the other side in < 17 min.

-- Gets a condition, p.e (<17), and a 'Duration State' and checks if the time satisfies the
-- condition 'f' and all adventures are in the sata state.
fun2'' :: (Int -> Bool) -> Duration State -> Bool
fun2'' f (Duration (i, x)) = (f i) && (and (map x players)) 

-- Gets a n, number of exectution, and a condition for the time of a 'Duration State' and checks
-- if for all the possible moves in n executions, there are at least one 'Duration State' with 
-- time that respects the contition 'f' and all the 'Adventurers' in the safe state.
test :: Int -> (Int -> Bool) -> Bool
test n f = let r = remLD (exec n gInit)
           in or (map (fun2'' f) r)

------------------------------------------------------------------------------------------------
-- Example:
-- > test 1 (<17)       > test 1 (<=17)       > test 1 (<19)
-- False                False                 False
-- > test 2 (<17)       > test 2 (<=17)       > test 2 (<19)
-- False                False                 False
-- > test 3 (<17)       > test 3 (<=17)       > test 3 (<19)
-- False                False                 False
-- > test 4 (<17)       > test 4 (<=17)       > test 4 (<19)
-- False                False                 False
-- > test 5 (<17)       > test 5 (<=17)       > test 5 (<19)
-- False                True                  True
-- > test 6 (<17)       > test 6 (<=17)       > test 6 (<19)
-- False                True                  True

------------------------------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers. Recall the Knight's 
quest --}
data ListDur a = LD [Duration a] deriving Show

-- Transforms a "ListDur a" to a list of "Duration a" 
remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

-- Functor ListDur
instance Functor ListDur where
   fmap f = let r = \(Duration (i, a)) -> Duration (i, f a)
             in LD . ((map r).remLD)

-- Applicative ListDur
instance Applicative ListDur where
   pure x = LD [Duration (0, x)]
   l1 <*> l2 = LD $ do r1 <- remLD l1 
                       r2 <- remLD l2
                       m (r1, r2) where
                         m (Duration (i1, f), Duration(i2, a)) 
                            = return (Duration(i1 + i2, f a))

-- Monad ListDur
instance Monad ListDur where
   return = pure
   l >>= k = LD $ do r <- remLD l
                     m r where
                          m (Duration (il, x)) = 
                                let v = remLD (k x)
                                in map (\(Duration (im, x)) -> Duration (il + im, x)) v

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)

------------------------------------------------------------------------------------------------
{-- Optional Task 3 --}
------------------------------------------------------------------------------------------------
-- New type of data, 'Duration2', same as 'Duration' but this one has a list of the final and
-- previous states, [States]. 
data Duration2 a = Duration2 (Int, a, [State]) deriving Show

getDuration2 :: Duration2 a -> Int
getDuration2 (Duration2 (d,_,_)) = d

getValue2 :: Duration2 a -> a
getValue2 (Duration2 (_,x,_)) = x

getStates2 :: Duration2 a -> [State]
getStates2 (Duration2 (_,_,l)) = l

-- Functor Duration2
instance Functor Duration2 where
  fmap f (Duration2 (i,x,l)) = Duration2 (i, f x, l)

-- Applicative Duration2
instance Applicative Duration2 where
  pure x = (Duration2 (0,x,[]))
  (Duration2 (i,f,l)) <*> (Duration2 (j,x,m)) = (Duration2 (i+j, f x, m))

-- Monad Duration2 
instance Monad Duration2 where
    (Duration2 (i,x,l)) >>= k = 
        Duration2 (i + (getDuration2 (k x)), getValue2 (k x), l)
    return = pure

------------------------------------------------------------------------------------------------
-- New type of data, 'ListDur2', same as 'ListDur' but this one has a list 'Duration2'.
data ListDur2 a = LD2 [Duration2 a] deriving Show

-- Transforms a "ListDur2 a" to a list of "Duration2 a" 
remLD2 :: ListDur2 a -> [Duration2 a]
remLD2 (LD2 x) = x

-- Functor ListDur2
instance Functor ListDur2 where
   fmap f = let r = \(Duration2 (i, a, l)) -> Duration2 (i, f a, l)
             in LD2 . ((map r).remLD2)

-- Applicative ListDur2
instance Applicative ListDur2 where
   pure x = LD2 [Duration2 (0, x, [])]
   l1 <*> l2 = LD2 $ do r1 <- remLD2 l1 
                        r2 <- remLD2 l2
                        m (r1, r2) where
                          m (Duration2 (i1, f, l), Duration2(i2, a, m)) 
                             = return (Duration2(i1 + i2, f a, m))

-- Monad ListDur2
instance Monad ListDur2 where
   return = pure
   l >>= k = LD2 $ do r <- remLD2 l
                      m r where
                        m (Duration2 (il, x', l)) = 
                          let v = remLD2 (k x')
                            in map (\(Duration2 (im, x, n')) -> Duration2 (il + im, x, l++n')) v

------------------------------------------------------------------------------------------------
-- Changes the 'State' of the 'Objects' inside a list and gives a 'Duration2 State' with the 
-- time needed to perform the move, the resulting 'State' and puts the state in the list of 
-- states. 
fun4 :: State -> [Objetc] -> Duration2 State
fun4 s [] = Duration2 (0, s, [])
fun4 s (h:t) = let s' = mChangeState ((h:t)++[(Right ())]) s
               in Duration2 (getTimeAdv((\(Left a) -> a) h), s', [s])

{-- For a given state of the game, the function presents all the possible moves that the 
adventurers can make. --}
allValidPlays2 :: State -> ListDur2 State
allValidPlays2 s = LD2 (map (fun4 s) (moves s))

------------------------------------------------------------------------------------------------
-- Receives the number n (number of individual moves), a function 'allValidPlays' and a 'State', 
-- and returns a 'List Duration' with all the moves that the adventures can make.
propagate2 :: Int -> (State -> ListDur2 State) -> State -> ListDur2 State
propagate2 0 _ s = LD2 [Duration2 (0, s, [s])]
propagate2 n s_l s = do 
                     r <- (s_l s)
                     propagate2 (n-1) s_l r 

{-- For a given number n and initial state, the function calculates all possible n-sequences 
of moves that the adventures can make and the path to get there. --}
exec2 :: Int -> State -> ListDur2 State
exec2 n s = propagate2 n (allValidPlays2) s

------------------------------------------------------------------------------------------------
{-- Optional Task 4 --}
------------------------------------------------------------------------------------------------
{-- Test functions --}
-- Gets a conditon, f, to be applied to the time of the 'Duration2 State', checks if the
-- condition is True and if all the 'Adventurers' are in the safe state, returns True or
-- False if both conditions are satisfied.
fun'2 :: (Int -> Bool) -> Duration2 State -> Bool
fun'2 f (Duration2 (i, x, l)) = (f i) && (and (map x players)) 

-- Gets a n, number of exectution, and a condition for the time of a 'Duration State' and checks
-- if for all the possible moves in n executions there are at least one 'Duration State' with 
-- time that respects the contition 'f' and all the 'Adventurers' in the safe state. 
-- Returns True or False.
test1 :: Int -> (Int -> Bool) -> Bool
test1 n f = let r = remLD2 (exec2 n gInit)
            in or (map (fun'2 f) r)

-- Gets a n, number of exectution, and a condition for the time of a 'Duration State' and checks
-- if for all the possible moves in n executions there are at least one 'Duration State' with 
-- time that respects the contition 'f' and all the 'Adventurers' in the safe state. 
-- Returns (tt or ff, n), where 'n' is the number of diferrent solutions to get to a final 
-- 'Duration State' that satisfies the previous conditions.
test2 :: Int -> (Int -> Bool) -> (Bool, Int)
test2 n f = let r = remLD2 (exec2 n gInit)
                l = (filter (fun'2 f) r)
            in (length(l)/=0, length(l))

-- Gets a n, number of exectution, and a condition for the time of a 'Duration State' and checks
-- if for all the possible moves in n executions there are at least one 'Duration State' with 
-- time that respects the contition 'f' and all the 'Adventurers' in the safe state. 
-- Returns (tt or ff, n, l), where 'n' is the number of diferrent solutions to get to a final 
-- 'Duration State' that satisfies the previous conditions, and 'l' is a list of 'Duration State'
-- that that satisfies the previous conditions.
test3 :: Int -> (Int -> Bool) -> (Bool, Int, [Duration2 State])
test3 n f = let r = remLD2 (exec2 n gInit)
                l = (filter (fun'2 f) r)
            in (length(l)/=0, length(l), l)

------------------------------------------------------------------------------------------------
-- Example:

-- > test1 5 (<=17)
-- True

-- > test2 5 (<=17)
-- (True,2)

-- > test3 5 (<=17)
-- (True,2,
-- [Duration2 (17,["True","True","True","True","True"],
-- [["False","False","False","False","False"]
-- ,["True","True","False","False","True"]
-- ,["False","True","False","False","False"]
-- ,["False","True","True","True","True"]
-- ,["False","False","True","True","False"]
-- ,["True","True","True","True","True"]])
-- ,Duration2 (17,["True","True","True","True","True"],
-- [["False","False","False","False","False"]
-- ,["True","True","False","False","True"]
-- ,["True","False","False","False","False"]
-- ,["True","False","True","True","True"]
-- ,["False","False","True","True","False"]
-- ,["True","True","True","True","True"]])])