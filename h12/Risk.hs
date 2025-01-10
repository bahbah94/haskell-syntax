{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (replicateM)
import Control.Monad.Random
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atk def) = do
    atkRolls <- replicateM (min (atk-1) 3) die
    defRolls <- replicateM (min def 2) die
    
    let pairs = zip (sortBy (flip compare) atkRolls) 
                    (sortBy (flip compare) defRolls)
        (atkLoss, defLoss) = foldr countCasualties (0,0) pairs
    
    return $ Battlefield (atk - atkLoss) (def - defLoss)
  where
    countCasualties (a,d) (atkL,defL) = 
        if unDV a > unDV d 
            then (atkL, defL+1) 
            else (atkL+1, defL)


invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield atk def)
 | def <= 0 = return b 
 | atk < 2 = return b 
 | otherwise = do
   newBatte <- battle b
   invade newBatte


successProb :: Battlefield -> Rand StdGen Double
successProb b = do
    -- Run 1000 invasions
    results <- replicateM 1000 (invade b)
    
    -- Count successful invasions (where defenders = 0)
    let successes = length $ filter (\(Battlefield _ def) -> def == 0) results
    
    -- Return probability (successes / 1000)
    return (fromIntegral successes / 1000.0)