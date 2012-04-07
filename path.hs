{------------------------------------------------------------------------------

  file Path2.hs
  (C) 2012 Peter Kolek  Release 0.1.3
  -----------------------------------

  Module Path for Travel Report program
  Written for Haskell GHC 7.0.3 compiler

------------------------------------------------------------------------------}

{-# LANGUAGE PatternGuards #-}


module Path2 where

  import Control.Monad
  import Control.Monad.Trans
  import System.Random
  import Data.Maybe
  import qualified Data.Map as M

  data Path = Path { fromP :: String,
       	      	     toP :: String,
	             distP :: Int }  deriving (Show, Read, Eq)  
  
  type Way = [Path]

  plusP :: Path -> Path -> Maybe Int
  plusP a b 
    | canChainP a b = Just $ distP a + distP b
    | otherwise     = Nothing

  infixr 6 <+>
  (<+>) = plusP

  addP :: Int -> Path -> Int
  addP i p =  i + distP p

  sumP :: Way -> Int
  sumP []     = 0
  sumP (x:[]) = distP x
  sumP (x:xs) = distP x + sumP xs

  -- alternative to sumP
  sumW :: Way -> Int
  sumW = sum . map distP

  -- another alternative to sumP
  sumWw :: Way -> Int
  sumWw = foldl addP 0

  -- not necessary now
  numFromMaybe :: Maybe Int -> Int
  numFromMaybe Nothing  = 0
  numFromMaybe (Just x) = x

  prependPath :: Path -> Way -> Way
  prependPath p [] = [p]
  prependPath p ws
    | canChainP p (head ws) = p:ws
    | otherwise             = []

  appendPath :: Way -> Path -> Way
  appendPath [] p = p:[]
  appendPath ws p 
    | canChainP (last ws) p = ws ++ [p]
    | otherwise             = []

  chain :: Way -> Way -> Way
  chain [] [] = []
  chain w1 [] = w1
  chain [] w2 = w2
  chain w1 w2
    | canChainW w1 w2 = w1 ++ w2
    | otherwise       = []

  infixr 6 <++>
  (<++>) = chain

  makeWay :: [Path] -> [Path]
  makeWay [] = []
  makeWay (x:[]) = [x]
  makeWay (x:y:xs)
    | canChainP x y = x:(makeWay (y:xs))
    | otherwise      = [] 

  canChainP :: Path -> Path -> Bool
  canChainP p1 p2 = toP p1 == fromP p2

  canChainW :: Way -> Way -> Bool
  canChainW [] [] = True
  canChainW w1 [] = True
  canChainW [] w2 = True
  canChainW ws1 (w2:ws2) = canChainP (last ws1) w2

  reverseP :: Path -> Path
  reverseP Path { fromP = x, toP = y, distP = z} = 
  	   Path { fromP = y, toP = x, distP = z}

  reverseW :: Way -> Way
  reverseW = map reverseP . reverse

  isValidWay :: Way -> Bool
  isValidWay []		= True
  isValidWay (p:[])     = True
  isValidWay (p1:p2:[]) = canChainP p1 p2
  isValidWay (p1:p2:ps)
    | canChainP p1 p2   = isValidWay (p2:ps)
    | otherwise         = False
   
{------------------------------------------------------------------------------
  shuffle a list functionally ? calls for improvement, see real world haskell
------------------------------------------------------------------------------}

  shuffle :: Int -> Int -> [a] -> [a]
  shuffle seed 0   _  = []
  shuffle seed len xs = 
        let
           n       = fst $ randomR(0, len-1) (mkStdGen seed)
           (y, ys) = chooseR n xs
           ys'     = shuffle (seed+len) (len - 1) ys
        in y:ys'

  chooseR _ [] = error "choose: index out of range"
  chooseR 0 (x:xs) = (x, xs)
  chooseR i (x:xs) = let (y, ys) = chooseR (i - 1) xs in (y, x:ys)

{------------------------------------------------------------------------------
  Path constraint solver 
  generalising parameters, most to calculate based on needed total dist 
------------------------------------------------------------------------------}
  
  takePt :: Way -> Int -> Way -> Way
  takePt upTo _ [] = upTo
  takePt upTo wanted (p:ps)  
    | canChainP (last upTo) p = 
        if (sumW upTo + distP p) <= wanted 
        then takePt (upTo ++ [p]) wanted ps else upTo
    | null ps = upTo
    | otherwise = takePt upTo wanted (tail ps)

{------------------------------------------------------------------------------
  takePt builds a list of Paths that starts with a starting point, 
         and total distance is less than wanted max distance
  Usage : takePt upTo wanted ps  where :
    upTo is a list of one item : starting Path
    wanted is the max distance wanted to be achieved
    ps is a list of possible Paths to use
------------------------------------------------------------------------------}

  makeSubLists fnTake _ _ []               = []
  makeSubLists fnTake start wanted pp@(p:ps) = 
    (fnTake [start] wanted pp):(makeSubLists fnTake start wanted ps) 

  genInput :: Int ->[a] -> [a]
  genInput seed xs = 
    let howMany = 1000
        xx      = concat $ take howMany $ repeat $ shuffle seed (length xs) xs
    in shuffle seed (length xx) xx

  solveWConstr seed rsltWanted start ls = do
    let lim  = 30
        low  = rsltWanted - 2 * lim
    x <- makeSubLists takePt start rsltWanted $ genInput seed ls
    -- added constraint to finish at start, ie. fromP start == toP end
    guard $ fromP (head x) == toP (last x)
    guard $ sumP x > low
    return x

{------------------------------------------------------------------------------
  read a list of Paths from a file : named "xxxx.paths" 
------------------------------------------------------------------------------}

  listFromMaybe :: [Maybe a] -> [a]
  listFromMaybe x = do
    Just r <- x
    return r
  
  makePfromL :: [String] -> Maybe Path
  makePfromL (f:t:d:_) = Just Path { fromP = ff, toP = tt, distP = dd }
      where ff = f
            tt = t
            dd = fst . head $ (reads d :: [(Int,String)])
  makePfromL _ = Nothing

  plistFromMaybe :: [Maybe Path] -> [Path]
  plistFromMaybe x = do
    Just r <- x
    return r

  getPList :: String -> [Maybe Path]
  getPList = map (makePfromL . words) . filter (not . null) .lines 

  fGetPList :: String -> IO [Maybe Path]
  fGetPList = liftM getPList . readFile 
    
  makeMapP :: [Path] -> M.Map String [Path]
  makeMapP ps = M.fromListWith (++) $ map (\p -> ((fromP p),[p])) ps

{------------------------------------------------------------------------------
  top down -> point free -> coding experiments
------------------------------------------------------------------------------}
  
  fGetPList1 :: String -> IO [Maybe Path]  
  fGetPList1 file = do
    txt <- readFile file
    return $ getPList txt

  fGetPList2 :: String -> IO [Maybe Path]
  fGetPList2 f = readFile f >>= \x -> return $ getPList x

  fGetPList3 :: String -> IO [Maybe Path]
  fGetPList3 = return . liftM getPList =<< readFile
  
{------------------------------------------------------------------------------
  some display functions  for Paths and Ways 
------------------------------------------------------------------------------}

  showP :: Path -> String
  showP p = "Path: " ++ fromP p ++ " -> " ++ toP p ++ " => " 
            ++ show (distP p) ++ " km"

  -- short way print function
  -- improvement : add line break after 70 chars or so
  displayW :: Way -> String
  displayW ww@(w:ws) = 
    let showToP   = (" -> " ++) . toP
	showToPs  = concat . map (\x -> " -> " ++ toP x)
    in "Way : " ++ (fromP w) ++ (showToPs ww) ++ " => " ++ (show $ sumP ww)

  -- detailed Way print function
  -- use printf to improve output alignment
  showW :: Way -> String
  showW ws = descrW ++ linesW ++ line_ ++ totalW ++ "\n" where 
    descrW = "\nKniha jizd z : " ++ fromP (head ws) ++ "\n" ++ line_
    linesW = concat $ map ((++ "\n") . showP) ws
    totalW = "Celkem najeto : " ++ (show $ sumP ws)
    line_  = "---------------------------------------\n"

