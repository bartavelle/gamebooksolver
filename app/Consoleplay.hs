module Main where

import LoneWolf.Character
import LoneWolf.Chapter
import LoneWolf.Book02 (chapters)
import LoneWolf.Choices
import LoneWolf.Rules

import Text.Read
import qualified Data.IntMap.Strict as IM
import Control.Lens
import System.Random

cmap :: IM.IntMap Chapter
cmap = IM.fromList chapters

randomSelect :: Show a => [(a, Rational)] -> IO a
randomSelect lst = go lst <$> randomRIO (0,1 :: Double)
  where
   go [] _ = error (show lst)
   go [(o,_)] _ = o
   go ((o,p):os) roll | roll <= fromRational p = o
                      | otherwise = go os (roll - fromRational p)

runOutcome :: Character -> ChapterOutcome -> IO ()
runOutcome (Character cconstant cvariable) o = do
   let ns = update cconstant cvariable o
   taken <- randomSelect ns
   case taken of
       HasLost -> putStrLn "lost :("
       HasWon _ -> putStrLn "Won!"
       NewChapter cid nvariable hadcombat -> play cid (Character cconstant nvariable) hadcombat

status :: CharacterVariable -> IO ()
status (CharacterVariable hp inv) = putStrLn $ unwords ( ("hp:" ++ show hp) : map show (items inv) )

play :: ChapterId -> Character -> HadCombat -> IO ()
play cid char@(Character cconstant cvariable) hadcombat =
    case IM.lookup cid cmap of
        Nothing -> putStrLn ("Unknown chapter " ++ show cid)
        Just (Chapter _ d decisions) -> do
            let ndecisions = case hadcombat of
                                 DidFight -> AfterCombat decisions
                                 Didn'tFight -> decisions
            putStrLn ""
            status cvariable
            putStrLn ""
            putStrLn d
            case flattenDecision cconstant cvariable ndecisions of
                [] -> putStrLn ("No decisions!!!! " ++ show ndecisions)
                [(dsc, o)] -> putStrLn (unwords dsc) >> runOutcome char o
                lst -> do
                    mapM_ print (zip [0 :: Int ..] (map (unwords . fst) lst))
                    n <- getN (length lst)
                    runOutcome char (snd $ lst !! n)

getN :: Int -> IO Int
getN mx = do
    n <- getLine
    case readMaybe n of
        Just x | x >= 0 || x < mx -> return x
        _ -> getN mx

main :: IO ()
main = do
    let initialState = Character iconstant ivariable
        iconstant = CharacterConstant 25 15 [WeaponSkill ShortSword, Hunting, SixthSense, MindShield, MindBlast]
        ivariable = CharacterVariable 25 (emptyInventory & addItem (Weapon ShortSword) 1 & addItem Meal 2 & addItem Gold 25)
    play 1 initialState Didn'tFight
