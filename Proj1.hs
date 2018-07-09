-- proj1 Haskell Declarative Programming
-- by Samuel Xu, 835273 at samuelx@student.unimelb.edu.au
--
--
-- This Program works in conjunction with Proj1Test and works
-- as the 'guessing' section of the game. The program attempts to
-- guess a chord a set of Pitches (a combination of note and an octave) 
-- from feedback given by the Proj1Test module.

-- Style Rules:
-- 78 character limit!
-- When declaring datatypes put a newline between each new type.
-- If line exceeds 78 characters, then separate with newlines between brackets
-- Name functions using camelCase, variables should be all lowercase.


-- Changelog
-- 0.01 - 4th Aug
--         Wrote initialGuess, thought about life.
-- 0.02 - 5th Aug
--         Wrote nextGuess, just guesses the same thing over and over.
-- 0.03 - 9th Aug
--         Wrote up the data structure of GameState. Seems to work ok.
-- 1.00 - 18th Aug
--         Working bruteforce implementation! (only an avg guess of 100!)
-- 2.00 - 21th Aug
--         Implemented a set reduction method from feedback. Now guesses
--         below 6.
-- 2.01 - 22nd Aug
--         Changed initial guess and now average guess is around 5.
-- 2.05 - 24th Aug
--         Iterated through all possible initial guesses with a script
--         and changed initial guess again. Avg guess is now ~4.9-5
-- 3.00 - 26th Aug 
--         Wrote up a scoring function which picks the next best guess
--         based on a score calculated from all possible targets.
--         Avg guess is now 4.3!
-- 3.01 - 28th Aug
--         Added a weighting to the scoring function. Average guess down
--         to sub 4.3
-- 3.05 - 1st Sept
--         Wrote a script which checked all the combinations of weighting.
--         Managed to get average down to approx 4.24
--         Final Submission

module Proj1 (initialGuess, nextGuess, GameState) where


-- Import necessary libraries
import Data.Char
import Data.Ord
import Data.List

-- Setup Pitch data structure.
-- Defines Note as Char and Octave as Int
-- Note: Use smart constructor to make sure no undefined values are input
data Pitch = Pitch { note :: Char
                   , oct :: Int
                   }
                deriving (Show, Eq)

-- Smart constructor for pitch to make sure no undefined pitches are
-- input
pitchConstruct :: Char -> Int -> Pitch
pitchConstruct note octave 
                | (notElem note ['A'..'G']) || (notElem octave [1..3]) = 
                    error "undefined pitch range"
                | otherwise = Pitch {note=note, oct=octave}

-- Gamestate holds the current set of valid values
data GameState = Record { set :: [[Pitch]]
                        }
                    deriving Show


-- Function takes no input arguments and returns a pair of an initial
-- guess and creates the set of possible values by creating a combination
-- set of all possible pitches.
-- Initial guess was found by iterating through all possible guesses,
-- compiling, running the program against all possible 1330 targets and then
-- picking the initial guess with the lowest average guess rate. 
initialGuess :: ([String],GameState)
initialGuess = (["B1","D1","F2"], Record (combination createSet 3))

-- Function creates a set of valid pitches to be used in initialGuess and
-- deleteFromSet.
-- Works by parsing strings into the data structure via parsePitch.
createSet :: [Pitch]
createSet = map parsePitch ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1"
                ,"D2","D3","E1","E2","E3","F1","F2","F3","G1","G2","G3"]

-- Next guess takes the previous guess, the set of remaining possible answers
-- and the score of the previous guess, and makes another guess with this
-- information.
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (prev, Record set) result = (next, (Record newSet))
    where
        -- Parse previous guess into data structure
        newPrev = (map parsePitch prev)
        -- Reduce the set based on the previous score and guess
        newSet = (reduceSet (deleteFromSet newPrev set) newPrev result)
        -- make the next guess by scoring all remaining guesses from set
        next = map unparsePitch (pickBest newSet)

-- Function takes a pitch as a string and parses it into the data structure
parsePitch :: String -> Pitch
parsePitch x = pitchConstruct (head x) (digitToInt (last x))

-- Function converts pitch back to array of string
unparsePitch :: Pitch -> String
unparsePitch x = ((note x):(show (oct x)))

-- Function creates combinations for the set
combination :: [Pitch] -> Int -> [[Pitch]]
combination _ 0 = [[]]
combination set n = [ x:xs | x:s <- tails set, xs <- combination s (n-1)]

                    {- Reducing the Set -}

-- Function deletes incorrect answers from the set by removing all guesses
-- with any of the current pitches from the remaining possible set of targets
deleteFromSet :: [Pitch] -> [[Pitch]] -> [[Pitch]]
deleteFromSet pitch set = set \\ (combination (pitch \\ createSet) 3)
              
-- Function reduces the set of possible targets by checking if other
-- guesses will return the same score as the previous guess. If they do not 
-- return the same score, then remove them from the set of possible targets 
-- via filter.
reduceSet :: [[Pitch]] -> [Pitch] -> (Int, Int, Int) -> [[Pitch]]
reduceSet set guess result = filter (\x -> (scoreGuess x guess == result)) set  

-- Function scores a guess against a target
scoreGuess :: [Pitch] -> [Pitch] -> (Int, Int, Int)
scoreGuess guess ref = (p, n, o)
    where 
        p = length (intersect guess ref)             
        n = abs (p - length (intersectNoDuplicate 
                    (checkNote guess) (checkNote ref)))
        o = abs (p - length (intersectNoDuplicate 
                    (checkOct guess) (checkOct ref)))

-- Returns an array of Notes from Pitches (to be used in scoreGuess)
checkNote :: [Pitch] -> [Char]
checkNote a = (map note a)

-- Returns an array of Octaves from Pitches (to be used in scoreGuess)
checkOct :: [Pitch] -> [Int]
checkOct a = (map oct a)

-- Returns an intersect of two arrays with no duplicates. This works by
-- taking the differences between the two arrays and checking the differences
-- with the initial array (to make sure there are only one instance for each
-- intersect).
intersectNoDuplicate :: (Eq a) => [a] -> [a] -> [a]
intersectNoDuplicate x y = x \\ (x \\ y)

                    {- Picking next best guess -}

-- Picks the next guess in a set by calculating a score for each possible 
-- guess against the rest of the set for that guess, and then picks the guess 
-- with the highest score
-- (i.e. the guess which has the highest chance of being the target)
-- Works by zipping the score with the guess and comparing guesses by the
-- score, and then picking the corresponding guess in the pair. 
pickBest :: [[Pitch]] -> [Pitch]
pickBest set = snd (maximumBy (comparing fst) 
                    (zip (map (\y -> calcScore y set) set) set))

-- Function takes a pitch and scores it against the rest of the set,
-- summing the scores into a single int (with a specified weighting) 
calcScore :: [Pitch] -> [[Pitch]] -> Int
calcScore check set = sum [y | y <- (map scoreSum 
                                (map (\y -> scoreGuess check y) set))]

-- Sums up the generated score with a weighting (derived generated after 
-- running many many many many many many many tests!!) towards correct notes.
scoreSum :: (Int,Int,Int) -> Int
scoreSum (x,y,z) = quot ((x*4)+(y*6)+z) 3





