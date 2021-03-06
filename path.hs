{------------------------------------------------------------------------------

  file Path.hs
  (C) 2012 Peter Kolek  Release 0.1.5
  -----------------------------------
  Module Path for Travel Report program
  Written for Haskell GHC 7.0.3 compiler

------------------------------------------------------------------------------}

{-# LANGUAGE PatternGuards #-}

module Path where

  import Control.Monad
  import Control.Monad.Trans
  import Control.Monad.State
  import System.Random
  import Data.Maybe
  import Data.List (find, nub)
  import qualified Data.Map as M
  import System.Random.Generators
  import Text.Printf

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

  -- adding a distance to Path, ie. new path with distance increased by i
  addDistP1 :: Path -> Int -> Path
  addDistP1 p i = makeP (fromP p) (toP p) $ i + distP p
  
  addDistP2 :: Path -> Int -> Path
  addDistP2 p i = p {distP = new} where new = i + distP p
  
  addDistP :: Path -> Int -> Path
  addDistP p i = p {distP = i + distP p}

  sumP :: Way -> Int
  sumP []     = 0
  sumP (x:[]) = distP x
  sumP (x:xs) = distP x + sumP xs

  -- alternative to sumP
  sumW :: Way -> Int
  sumW = sum . map distP

  addP :: Int -> Path -> Int
  addP i p = i + distP p
  
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
  appendPath [] p = [p]
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
    | canChainP x y = x:makeWay (y:xs)
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

  makeSubLists fnTake _ _ []                 = []
  makeSubLists fnTake start wanted pp@(p:ps) = 
    fnTake [start] wanted pp : makeSubLists fnTake start wanted ps

  genInput :: Int ->[a] -> [a]
  genInput seed xs = 
    let howMany = 600
        xx      = concat $ replicate howMany $ shuffle seed (length xs) xs
    in shuffle seed (length xx) xx

  solveWConstr seed rsltWanted start ls = do
    let lim  = 30
        low  = rsltWanted - 2 * lim
    x <- makeSubLists takePt start rsltWanted $ genInput seed ls
    -- added constraint to finish at start, ie. fromP start == toP end
    -- guard $ fromP (head x) == toP (last x)
    guard $ sumP x > low
    return x

{------------------------------------------------------------------------------
  more efficient algorithm
------------------------------------------------------------------------------}

  -- Tr as Travel
  data Tr = Tr { distT :: Int, startT :: String, endT :: String, wayT :: [Path] }
    deriving (Show, Read, Eq)
             
  -- make instance of Ord ??
             
  makeTr :: Int -> String -> String -> Way -> Tr
  makeTr distP startP endP listP
   | isValidWay listP && distP == distP' && startP == startP' && endP == endP'
     = Tr { distT = distP, startT = startP, endT = endP, wayT = listP }
   | otherwise
     = Tr { distT = 0, startT = "", endT = "", wayT = [] }
       where
         startP' = fromP (head listP)         
         endP' = toP (last listP)         
         distP' = sumW listP
  
  trFromList :: Way -> Tr
  trFromList listP
   | isValidWay listP
     = Tr { distT = distP', startT = startP', endT = endP', wayT = listP }
   | otherwise
     = Tr { distT = 0, startT = "", endT = "", wayT = [] }
       where
         startP' = fromP (head listP)         
         endP' = toP (last listP)         
         distP' = sumW listP
  
  singletonTr :: Path -> Tr
  singletonTr p = makeTr (distP p) (fromP p) (toP p) [p]

  emptyTr :: Tr
  emptyTr =  Tr { distT = 0, startT = "", endT = "", wayT = [] }

  -- append to Tr, non safe version, assumes that path is chainable with Tr
  addToTr :: Path -> Tr -> Tr
  addToTr path tr = 
    makeTr (distT tr + distP path) (startT tr) (toP path) (wayT tr ++ [path])
  
  -- predicate to tell if Tr is an empty Tr / alternative use maybe Tr
  isEmptyTr :: Tr -> Bool
  isEmptyTr = null . wayT 
  
  -- predicate to tell if Tr starts end ends in the same place
  isCircularTr :: Tr -> Bool
  isCircularTr tr = startT tr == endT tr

  buildTrList :: String -> String -> Int -> M.Map String [Path] -> [Tr]
  buildTrList startPoint endPoint wantedDist pMap = genRslt [] initList
    where
      lim = 25
      initList = map singletonTr $ fromMaybe [] $ M.lookup startPoint pMap
      rgen = runGenerator wantedDist $ rangeG (-1,1)
      genRslt :: [Tr] -> [Tr] -> [Tr]
      genRslt rslt [] = rslt
      genRslt rslt bld
        | length rslt > 1600 = map adjustTrRand rslt -- additional *STOP* cond
        | otherwise          = genRslt rslt' bld' where
            bld'   = if null bld then initList else build bld
            rsCond t = distT t > (wantedDist - lim) && isCircularTr t
            rslt'  = rslt ++ filter rsCond bld'
      -- restrict choice randomly somehow
      build :: [Tr] -> [Tr]
      build [] = []
      build buildList = do
        bld  <- buildList
        add  <- M.findWithDefault [] (endT bld) pMap        
        let bld' = addToTr add bld
        guard $ distT bld' <= wantedDist
        return bld'
      adjustTrRand :: Tr -> Tr
      adjustTrRand tr
        | null (wayT tr) = tr
        | otherwise      = trFromList ps 
        where 
          adjs = take (length (wayT tr)) rgen
          ps   = zipWith addDistP (wayT tr) adjs
              
{------------------------------------------------------------------------------
  read a list of Paths from a file : named "xxxx.paths" 
------------------------------------------------------------------------------}

  findStartP :: String -> Way -> Maybe Path 
  findStartP st = find (\x -> st == fromP x)

  pFromMaybe :: Maybe Path -> Path
  pFromMaybe Nothing  = Path { fromP = "nil", toP = "nil", distP = 0 }
  pFromMaybe (Just p) = p

  listFromMaybe :: [Maybe a] -> [a]
  listFromMaybe x = do
    Just r <- x
    return r

  plistFromMaybe :: [Maybe Path] -> [Path]
  plistFromMaybe x = do
    Just r <- x
    return r
  
  makePfromL :: [String] -> Maybe Path
  makePfromL (f:t:d:_) = Just Path { fromP = ff, toP = tt, distP = dd }
      where ff = f
            tt = t
            dd = fst . head $ (reads d :: [(Int,String)])
  makePfromL _ = Nothing

  makeP :: String -> String -> Int -> Path
  makeP ff tt dd = Path { fromP = ff, toP = tt, distP = dd }

  getPList :: String -> [Maybe Path]
  getPList = map (makePfromL . words) . filter (not . null) .lines 

  fGetPList :: String -> IO [Maybe Path]
  fGetPList = liftM getPList . readFile 
    
  makeMapP :: [Path] -> M.Map String [Path]
  makeMapP ps = M.fromListWith (++) $ map (\p -> (fromP p, [p])) ps

{------------------------------------------------------------------------------
  top down -> point free -> coding experiments
------------------------------------------------------------------------------}
  
  -- getPList1 is similar to getPList but gets rid of Maybe by using mapMaybe
  getPList1 :: String -> [Path]
  getPList1 = mapMaybe (makePfromL . words) . filter (not . null) .lines 
  
  -- same as fgetPList but uses getPList1 to get rid of Maybe in resulting list
  fGetPList11 :: String -> IO [Path]
  fGetPList11 = liftM getPList1 . readFile 

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
  
  showPP :: Path -> String
  showPP p = printf "Path: %-15s -> %-15s => %4i km" (fromP p) (toP p) (distP p)

  -- short way print function
  -- improvement : add line break after 70 chars or so
  displayW :: Way -> String
  displayW [] = "\nEmpty way\n"
  displayW ww@(w:ws) = 
    let showToP   = (" -> " ++) . toP
	showToPs  = concatMap (\x -> " -> " ++ toP x)
    in "Way : " ++ fromP w ++ showToPs ww ++ " => " ++ show (sumP ww)

  -- detailed Way print function
  -- use printf to improve output alignment
  showW :: Way -> String
  showW [] = "\nEmpty way\n"
  showW ws = descrW ++ linesW ++ line_ ++ totalW ++ "\n" where 
    descrW = "\nKniha jizd z : " ++ fromP (head ws) ++ "\n" ++ line_
    linesW = concatMap ((++ "\n") . showP) ws
    totalW = "Celkem najeto : " ++ show (sumP ws)
    line_  = "---------------------------------------------------\n"

  showTr :: Tr -> String  
  showTr tr | distT tr == 0 = "\nEmpty way\n"
            | otherwise     = descT ++ linesT ++ line_ ++ totalT ++ "\n" where
    line_  = "---------------------------------------------------\n"
    descT  = "\nKniha jizd z : " ++ startT tr ++ "\n" ++ line_
    totalT = "Celkem najeto : " ++ show (distT tr)
    linesT = concatMap ((++ "\n") . showPP) (wayT tr)
    
{------------------------------------------------------------------------------    
 run the generator inside buildTr and just take next rand numbers for use
------------------------------------------------------------------------------}

  finalRandAdj :: Tr -> Int -> Tr
  finalRandAdj tr dist
    | distT tr == dist = tr
    | distT tr < dist  = adj tr 1 (dist - distT tr)
    | distT tr > dist  = adj tr (-1) (distT tr - dist)   
    where gen = runGenerator dist $ rangeG (0,1)
          adj :: Tr -> Int -> Int -> Tr
          adj tr by steps = makeTr (distT tr + steps * by) (startT tr) (endT tr) 
                                   (adjPs (wayT tr) [] by steps gen)
          adjPs :: [Path] -> [Path] -> Int -> Int -> [Int] -> [Path]
          adjPs ps [] _ 0 _ = ps          
          adjPs ps bs _ 0 _ = reverse bs ++ ps 
          adjPs [] bs by tot (g:gs) = adjPs (reverse bs) [] by tot gs          
          adjPs (p:ps) bs by tot (g:gs) 
            | g == 0    = adjPs ps (p:bs) by tot gs 
            | otherwise = adjPs ps (makeP (fromP p) (toP p)
                                (distP p + by):bs) by (tot - 1) gs

