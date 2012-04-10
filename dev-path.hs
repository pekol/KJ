
{------------------------------------------------------------------------------
  file dev-path.hs
  development notes for KJ project
  git clone https://github.com/pekol/KJ

  (C) 2012 Peter Kolek  Release 0.1.5
  -------------------------------------

	Wanted :

        an algorithm based on trees :
           building from start
           building from end
             merging both solutions

        something like the following :
	  i <- gen input :: ... -> [Path] - then
	-- stream of suitable start points
	  s <- filter (\x -> start == fromP x) i
	  x <- tails i
	-- alternatively sols <- takePt ... x
	  guard ( head x `ele` s)
	  guard ( isValidWay x )
	  guard ( sumWw x <= rsltWanted )
	  return x	    

	module for dates, times processing

        split to two modules : Path and Way with generalised interface
           - i.e. generalised chain, take, etc., maybe in typeclasses 
           - improved randomization

	sumP between dist +/- 30, calc limits, list lengths etc
	   - take the closest solution to max - min / 2 ... ???

        Sampling from shuffled Possibles list --> better strategy yet ?
           until > min
             then  if > max return current result & restart search
                   if > mid and < max append to result, return rslt & restart
                   if still < mid try next one

        parsing input files i.e. "test.paths" or other with possible paths
        and reporting problems etc.
            - maybe use reads, parsec or other

        add and reflect in constraints some Paths (read from a file again)
        and time, means chaining must match (divide problem to subpaths) so
        that overall constraints (total distance/mileage, from, to points etc.
        will hold !!!

        check names of operators not to conflict with Applicative etc....

	later add date and time constraint for chaining paths
                  last toP point to point of origin (circle) ?

	:->  as cons  /possibly implement similar to List, recursive data type
	<>   empty Way ?
        emptyP ?  zeroP ? distP = anything from 0 -> infinity
        <:> chain Paths into a Way  -> type class for this ?

        more efficient algorithms - using Maps and choosing from these ...
        see iPhone notes

	maps :  map distP => map fromP => [ Path ]
                   map fromP => map distP => [ Path ]
	from x   to  y  with  dist = z   +\-   lim

	add ability to put certain Paths and Ways into the final Way as 
	additional constraints and Final = Way1 ++ DodanaCesta ++ Way2 etc. 

   add an empty Path ?
   if yes, then what for, just to represent wrong entries ???

------------------------------------------------------------------------------}

  makePfromL :: [String] -> Maybe Path
  makePfromL (f:t:d:_) = Just Path { fromP = ff, toP = tt, distP = dd }
    where ff = f
          tt = t
          dd = fst . head $ (reads d :: [(Int,String)])
{------------------------------------------------------------------------------

 what if f = t and d /= 0 and f /= t but d = 0 or is not number ??

    | isWord f && isword t && isint d = --> see above
    | otherwise                       = --> see below

 later some code to warn user of ill formed test.paths file
 possibly some kind of parser

 Maybe Path - Nothing if wrong, log for user to see later
       	      Writer monad
              in the program ignore and do not add into path list

------------------------------------------------------------------------------}

  makePfromL _ = Nothing
  --  makePfromL _ = Path { fromP = "nil", toP = "nil", distP = 0 }

{------------------------------------------------------------------------------
  rewrite with if, or just find better way to structure tis function

  takePt builds a list of Paths that starts with a starting point, 
         and total distance is less than wanted max distance
  Usage : takePt upTo wanted ps  where :
    upTo is a list of one item : starting Path
    wanted is the max distance wanted to be achieved
    ps is a list of possible Paths to use
------------------------------------------------------------------------------}
  
  takePt :: Way -> Int -> Way -> Way
  takePt upTo _ [] = upTo
  takePt upTo wanted (p:ps)  
    | canChainP (last upTo) p = 
        case (sumW upTo + distP p) <= wanted of
           True  -> takePt (upTo ++ [p]) wanted ps
           False -> upTo 
    | not (canChainP (last upTo) p) =
        case null ps of
           False -> takePt upTo wanted (tail ps)
           True  -> upTo

  takePt :: Way -> Int -> Way -> Way
  takePt upTo _ [] = upTo
  takePt upTo wanted (p:ps)  
    | canChainP (last upTo) p = 
        if (sumW upTo + distP p) <= wanted 
        then takePt (upTo ++ [p]) wanted ps else upTo 
    | not (canChainP (last upTo) p) =
        if null ps 
        then upTo else takePt upTo wanted (tail ps)

  makePfromL :: [String] -> Maybe Path
  makePfromL (f:t:d:_) = Just Path { fromP = ff, toP = tt, distP = dd }
    where ff = f
          tt = t
          dd = fst . head $ (reads d :: [(Int,String)])
  makePfromL _ = Nothing
          
{------------------------------------------------------------------------------
 what if f = t and d /= 0 and f /= t but d = 0 or is not number ??
    | isWord f && isword t && isint d = --> see above
    | otherwise                       = --> see below
 later some code to warn user of ill formed test.paths file
 possibly some kind of parser

 Maybe Path - Nothing if wrong, log for user to see later
       	      Writer monad
              in the program ignore and do not add into path list
------------------------------------------------------------------------------}

  plistFromMaybe :: [Maybe Path] -> [Path]
  plistFromMaybe x = do
    Just r <- x
    return r

  listFromMaybe :: [Maybe a] -> [a]
  listFromMaybe x = do
    Just r <- x
    return r
  
{------------------------------------------------------------------------------
  
  - improve Maybe handling in reading and using Maybe IO entries
  - better shuffle, work with random

------------------------------------------------------------------------------}

  Data Travel = Tr { dist::Int, start::String, end::String way::[Path] }
  -- trasa ?

  makeTr distInput startInput endInput listPath
   | isValidWay listPath && startInput == fromP (head listPath) ...
     = Tr { dist = distInput, start = startInput, end = endInput, way = listPath }
  
  singletonTr p = makeTr (distP p) (fromP p) (toP p) [p]

  addToTr path tr = 
    makeTr (dist tr + dist path) (start tr) (toP path) (way tr ++ [path])
  
  -- something like a Zipper ?
  buildTrList startPoint endPoint wantedDist pMap = build [] initList
    where
      lim = 45
      initList = map singletonTr $ M.lookup startPoint pMap 
      isComplTr tr = start tr == end tr
      build rsltList [] = rsltList
      build rsltList buildList = do
        bld  <- buildList 
        addi <- M.lookup (end bld) pMap
        bld' <- addToTr addi bld
        guard $ (dist bld') > wantedDist
        rslt <- filter isComplTr bld'
        guard $ dist rslt < (wantedDist - lim) 
        build (rsltList ++ return rslt) (return bld')
        
{------------------------------------------------------------------------------
  more efficient algorithm
------------------------------------------------------------------------------}

  -- Tr as Travel
  data Tr = Tr { distT :: Int, startT :: String, endT :: String, wayT :: [Path] }
    deriving (Show, Read, Eq)
             
  -- make instance of Ord ???
             
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
  
  singletonTr :: Path -> Tr
  singletonTr p = makeTr (distP p) (fromP p) (toP p) [p]

  addToTr :: Path -> Tr -> Tr
  addToTr path tr = 
    makeTr (distT tr + distP path) (startT tr) (toP path) (wayT tr ++ [path])
  
  -- predicate to tell if Tr starts end ends in the same place
  isCircularTr :: Tr -> Bool
  isCircularTr tr = startT tr == endT tr

  buildTrList :: String -> String -> Int -> (M.Map String [Path]) -> [Tr]
  buildTrList startPoint endPoint wantedDist pMap = genRslt [] initList
    where
      lim = 25
      initList = map singletonTr $ fromMaybe [] $ M.lookup startPoint pMap

      genRslt rslt [] = rslt
      genRslt rslt bld
        | length rslt > 1000  = rslt       -- additional *STOP* condition
        | otherwise           = genRslt rslt' bld' where
            bld'   = if null bld then initList else build bld
            rsCond = \t -> distT t > (wantedDist - lim) && isCircularTr t
            rslt'  = rslt ++ filter rsCond bld'

      build :: [Tr] -> [Tr]
      build [] = []
      build buildList = do
        bld  <- buildList
        addP <- fromMaybe [] $ M.lookup (endT bld) pMap
        bld' <- return $ addToTr addP bld
        guard $ (distT bld') <= wantedDist
        return bld'

-- used fromMaybe as we are in list Monad fail works fine as [], try MaybeT ?
--  buildTrList :: String -> String -> Int -> (M.Map String [Path]) -> [Tr]
--  buildTrList startPoint endPoint wantedDist pMap = build [] initList
--    where
--      lim = 25
--      initList = map singletonTr $ fromMaybe [] $ M.lookup startPoint pMap
--      build :: [Tr] -> [Tr] -> [Tr]
--      build rsltList [] = rsltList
--      build rsltList buildList = do
--        bld  <- buildList
--        addP <- fromMaybe [] $ M.lookup (endT bld) pMap
--        bld' <- return $ addToTr addP bld
--        guard $ (distT bld') <= wantedDist
--        let bld'' = return bld'
--            rslt  = filter (\t -> distT t > (wantedDist - lim)) bld''
--            rslt' = filter isCircularTr rslt
--      if length of way list inside Tr is greater than 6 ...            
--        if length (wayT (head bld'')) > 8
--        why this does not work ???           
--        if (length rslt' > 10)
--          then rslt'
--          else build (rslt' ++ rsltList) (nub bld'')
        
-- runState StateT with result added to State ???
        
-- sort by distT        
  
{------------------------------------------------------------------------------ 
  
  Stage 2 :

      combine all possible /reasonable/ smaller Travel's ?
      if (see above) works as designed, not necessary

  Problem to solve later ->

      structure based on fixed Paths, ie. 
         solve missing pieces with same solver and put together

  Improve /create/ command handling, add a parser for simple DSL
  to describe constraints ie. fixed Paths, start, end, dates, times etc.
     read from file description

  Choose final from acceptables

  adjusDist x = randomR (-1,3) -> each Path

------------------------------------------------------------------------------}

Example of current output :  
  
Praha 360 km  seed -> 777  
Way : Praha -> Rokycany -> Praha -> Pribram -> Dobris -> Pribram -> Praha => 338

Kniha jizd z : Praha
---------------------------------------
Path: Praha -> Rokycany => 84 km
Path: Rokycany -> Praha => 84 km
Path: Praha -> Pribram => 66 km
Path: Pribram -> Dobris => 19 km
Path: Dobris -> Pribram => 19 km
Path: Pribram -> Praha => 66 km
---------------------------------------
Celkem najeto : 338

