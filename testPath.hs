
{------------------------------------------------------------------------------

  file testPath.hs
  (C) Peter Kolek  Release 0.1.5
  -------------------------------
  improve to show and demonstrate the important parts of Path / Way modules

------------------------------------------------------------------------------}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards  #-}

module Main where

import System.IO
import System.Console.CmdArgs
import Data.List (find)
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Path

------------- functions for testing Path / Way modules functions -------------

testPlusP = do
  a <- liftM plistFromMaybe $ fGetPList "test.paths"
  let ps = a ++ map reverseP a
      x = ps !! 3
      y = ps !! 4    
      z = ps !! 5
      w = ps !! 6
--    mapM_ putStrLn (map showP ps)
  print $ plusP x z
  print $ x <+> z
  print $ x <+> y
  putStrLn "A cesta bude dlha ..."
--  print $ sumP [x,z]
--  print $ sumP [x,y]
--  print $ sumP [x,z,w]
  putStrLn "sumww Way sum :" 
  print $ sumWw [x,z,w]
  let q = [x,z,w]
      qq = [x,y,w]
  putStrLn $ displayW q
  print (makeWay [x,z,w])
  print (makeWay [x,y,z])
  putStrLn $ showP x
  print x
  print (reverseP x)
  print (canChainP x z)
  print q
  putStrLn $ displayW q
  putStrLn $ displayW $ reverseW q
  print (isValidWay q)
  print (isValidWay qq)
  putStrLn $ displayW q
  putStrLn $ "Tramtadada ..... :" ++ showP (ps!!14)
  
  mapM_ (putStrLn . displayW) (take 1 $ solveWConstr 1493 250 (ps!!14) ps)
    
testConstr = do
  putStrLn "Constraint tests"
  let x = [1,2,3,4,5]
  putStrLn "Debug: shuffle 25 (length x) x "
  print $ shuffle 25 (length x) x
  putStrLn "Debug: read Paths from file"
  putStrLn "Debug: fGetPList \"test.paths\""
  x <- liftM plistFromMaybe $ fGetPList "test.paths"
  print $ take 10 x
  print $ sumW x
  -- putStrLn "Debug: sumW $ fGetPList1 \"test.paths\""
  _ <- liftM (print . sumW . plistFromMaybe) $ fGetPList1 "test.paths"
  xmap <- liftM plistFromMaybe $ fGetPList1 "test.paths"
  xxmap <- fGetPList11 "test.paths"
  if xmap == xxmap 
    then putStrLn "xmap == xxmap where xxmap = fGetPList1 'test.paths'"
    else putStrLn "xmap /= xxmap where xxmap = fGetPList1 'test.paths'"
  print $ take 2 xxmap
  let t = trFromList $ take 5 xxmap
  putStrLn $ showTr t
  putStrLn "upravit na ? km "
  xd <- getLine
  let xdist = fst $ head $ reads xd :: Int
  let adjt = finalRandAdj t xdist
  putStrLn $ "adjt Ma mat " ++ show xdist ++ " km ???"
  hFlush stdout 
  putStrLn $ showTr adjt
  let zmap = makeMapP $ (++) xmap $ map reverseP xmap
  let y = fromJust $ M.lookup "Praha" zmap
  putStrLn "Z Prahy vedu cesty ako :"      
  mapM_ (putStrLn . showP) $ take 3 $ fromJust $ M.lookup "Praha" zmap
  putStrLn "Z Plzne vedu cesty :"      
  mapM_ (putStrLn . showP) $ take 3 $ fromJust $ M.lookup "Plzen" zmap
  putStrLn "A z Vestca :"      
  return $ take 3 $ fromJust $ M.lookup "Vestec" zmap  
  
{------------------------------------------------------------------------------
helper functions here around startPoint and such
------------------------------------------------------------------------------}

printRslt :: Int -> [Way] -> IO()
printRslt howMany resultList = 
  mapM_ (putStrLn . displayW) $ take howMany resultList

printWay howMany resultList = 
  mapM_ (putStrLn . showW) $ take howMany resultList
  
printT howMany resultList = 
  mapM_ (putStrLn . showTr) $ take howMany resultList

badStartPointMsg stPoint file = do
  putStr   $ "\nI am sorry, start point " ++ stPoint
  putStrLn $ " is not in possible Path list in file " ++ file
  putStrLn "Please try again with a different start point ...\n"
  
{------------------------------------------------------------------------------
command line processing here
------------------------------------------------------------------------------}

data Commands = Comm { start :: String, paths :: String, rseed :: Int, 
     dist :: Int, month :: Int, year :: Int } deriving (Data, Typeable, Show)

comm = cmdArgsMode $ Comm {
  start = "Praha"
  ,paths = "test.paths"
  ,rseed = 777
  ,dist = 360
  ,month = 0
  ,year = 0
  }
  &= summary "\nKniha jizd\n(C) Peter Kolek"

{------------------------------------------------------------------------------
main program func here
------------------------------------------------------------------------------}

main = do
  Comm{..} <- cmdArgsRun comm
  a <- liftM plistFromMaybe $ fGetPList "test.paths"
  let ps = a ++ map reverseP a
      st = findStartP start ps 
      pMap = makeMapP ps
  if isNothing st 
    then badStartPointMsg start paths
    else do
      let -- rslt = solveWConstr rseed dist (fromJust st) ps
          rslt2 = buildTrList start start dist pMap
      putStrLn $ start ++ " " ++ show dist ++ " km  seed -> " ++ show rseed
--      printRslt 1 rslt   -- one line way display
--      printWay 1 rslt    -- table display
      putStrLn "\n Test of Travel class algorithm ... \n"
--      printRslt 1 $ mapM wayT rslt2   -- one line way display
      printT 10 rslt2    -- one line way display
--      print rslt2
      putStrLn $ "Number of results : " ++ show (length rslt2)
--      print $ map distT $ take 300 rslt2
--      print $ map (length . wayT) rslt2
      putStrLn $ showTr (last rslt2)

--  testPlusP
--  testConstr
