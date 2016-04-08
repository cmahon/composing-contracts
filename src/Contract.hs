module Contract where
 
--import List
--import Numeric
--import Control.Monad
import Data.Maybe
--import System
--import Text.XHtml.Strict
--import Data.Unique

data Currency = USD | GBP | EUR | ZAR | KYD | CHF  deriving (Eq, Show)

type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0

data Contract =
    Zero
  | One  Currency
  | Give Contract
  | And  Contract Contract
  | Or   Contract Contract
  | Cond    (Obs Bool)   Contract Contract
  | Scale   (Obs Double) Contract
  | When    (Obs Bool)   Contract
  | Anytime (Obs Bool)   Contract
  | Until   (Obs Bool)   Contract
  deriving Show

newtype Obs a = Obs (Date -> PR a)

instance Show a => Show (Obs a) where
  show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"

zero :: Contract
zero = Zero

one :: Currency -> Contract
one = One

give :: Contract -> Contract
give = Give

cAnd :: Contract -> Contract -> Contract
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

konst :: a -> Obs a
konst k = Obs (\t -> bigK k)

lift :: (a -> b) -> Obs a -> Obs b
lift f (Obs o) = Obs (\t -> PR $ map (map f) (unPr $ o t))


lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs o1) (Obs o2) = Obs (\t -> PR $ zipWith (zipWith f) (unPr $ o1 t) (unPr $ o2 t))

date :: Obs Date
date = Obs (\t -> PR $ timeSlices [t])

instance Num a => Num (Obs a) where
  fromInteger i = konst (fromInteger i)
  (+) = lift2 (+)
  (-) = lift2 (-)
  (*) = lift2 (*)
  abs = lift abs
  signum = lift signum

instance Eq a => Eq (Obs a) where
  (==) = undefined

(==*) :: Ord a => Obs a -> Obs a -> Obs Bool
(==*) = lift2 (==)

at :: Date -> Obs Bool
at t = date ==* konst t

(%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%<=) = lift2 (<=)
(%=)  = lift2 (==)
(%>=) = lift2 (>=)
(%>)  = lift2 (>)

european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) = anytime (between t1 t2)

between :: Date -> Date -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= konst t1) (date %<= konst t2)

newtype PR a = PR { unPr :: [RV a] } deriving Show

type RV a = [a]

takePr :: Int -> PR a -> PR a
takePr n (PR rvs) = PR $ take n rvs

horizonPr :: PR a -> Int
horizonPr (PR rvs) = length rvs

andPr :: PR Bool -> Bool
andPr (PR rvs) = all and rvs -- and (map and rvs)

data Model = Model {
  modelStart :: Date,
  disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
  exch       :: Currency -> Currency -> PR Double,
  absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
  rateModel  :: Currency -> PR Double
  }

exampleModel :: CalendarTime -> Model
exampleModel modelDate = Model {
  modelStart = (modelDate,0),
  disc       = disc,
  exch       = exch,
  absorb     = absorb,
  rateModel  = rateModel
  }
  where
    
    rates :: Double -> Double -> PR Double
    rates rateNow delta = PR $ makeRateSlices rateNow 1
      where
        makeRateSlices rateNow n = rateSlice rateNow n : makeRateSlices (rateNow-delta) (n+1)
        rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]
    
    rateModels :: [(Currency, PR Double)]
    rateModels = [(CHF, rates 7   0.8)
                ,(EUR, rates 6.5 0.25)
                ,(GBP, rates 8   0.5)
                ,(KYD, rates 11  1.2)
                ,(USD, rates 5   1)
                ,(ZAR, rates 15  1.5)
                ]
    
    rateModel :: Currency -> PR Double
    rateModel k =
      fromMaybe (error $ "rateModel: currency not found " ++ show k)
        (lookup k rateModels)
      -- case lookup k rateModels of
      --   Just x -> x
      --   Nothing -> error $ "rateModel: currency not found " ++ (show k)
      
    disc :: Currency -> (PR Bool, PR Double) -> PR Double
    disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)
      where
    
        discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
        discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
          if and bRv -- test for horizon
            then [pRv]
            else let rest@(nextSlice:_) = discCalc bs ps rs
                     discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                     thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
                                   bRv pRv discSlice
                 in thisSlice : rest
        
        prevSlice :: RV Double -> RV Double
        prevSlice [] = []
        prevSlice [_] = []
        -- prevSlice (_:[]) = []
        prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest

    absorb :: Currency -> (PR Bool, PR Double) -> PR Double
    absorb k (PR bSlices, PR rvs) =
      PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                   bSlices rvs

    exch :: Currency -> Currency -> PR Double
    exch k1 k2 = PR (konstSlices 1)
 
expectedValue :: RV Double -> RV Double -> Double
expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

expectedValuePr :: PR Double -> [Double]
expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice

probabilityLattice :: [RV Double]
probabilityLattice = probabilities pathCounts
  where

    probabilities :: [RV Integer] -> [RV Double]
    probabilities (sl:sls) = map (\n -> fromInteger n / fromInteger (sum sl)) sl : probabilities sls

    pathCounts :: [RV Integer]
    pathCounts = paths [1] where paths sl = sl : paths (zipWith (+) (sl++[0]) (0:sl))

evalC :: Model -> Currency -> Contract -> PR Double
evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
  where eval Zero           = bigK 0
        eval (One k2)       = exch k k2
        eval (Give c)       = -(eval c)
        eval (o `Scale` c)  = evalO o * eval c
        eval (c1 `And` c2)  = eval c1 + eval c2
        eval (c1 `Or` c2)   = max (eval c1) (eval c2)
        eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
        eval (When o c)     = disc   k (evalO o, eval c)
--      eval (Anytime o c)  = snell  k (evalO o, eval c)
        eval (Until o c)    = absorb k (evalO o, eval c)

evalO :: Obs a -> PR a
evalO (Obs o) = o time0

bigK :: a -> PR a
bigK x = PR (konstSlices x)
 
konstSlices :: a -> [[a]]
konstSlices x = nextSlice [x]
  where nextSlice sl = sl : nextSlice (x:sl)
 
datePr :: PR Date
datePr = PR $ timeSlices [time0]

timeSlices :: (Num t1,Enum t1) => [(t, t1)] -> [[(t, t1)]]
timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = lift3Pr (\b tru fal -> if b then tru else fal)

liftPr :: (a -> b) -> PR a -> PR b
liftPr f (PR a) = PR $ map (map f) a

lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
zipWithAll f as@(_:_) []       = as
zipWithAll f []       bs@(_:_) = bs
zipWithAll _ _        _        = []

instance Num a => Num (PR a) where
  fromInteger i = bigK (fromInteger i)
  (+) = lift2PrAll (+)
  (-) = lift2PrAll (-)
  (*) = lift2PrAll (*)
  abs = liftPr  abs
  signum = liftPr signum

instance Ord a => Ord (PR a) where
  max = lift2Pr max

instance Eq a => Eq (PR a) where
  (PR a) == (PR b) = a == b

xm :: Model
xm = exampleModel ()

evalX :: Contract -> PR Double
evalX = evalC xm USD

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))

c1 :: Contract
c1 = zcb t1 10 USD

t1 :: Date
t1 = mkDate t1Horizon

t1Horizon :: TimeStep
t1Horizon = 3

c11 :: Contract
c11 = european (mkDate 2)
        (zcb (mkDate 20) 0.4 USD `cAnd`
        zcb (mkDate 30) 9.3 USD `cAnd`
        zcb (mkDate 40) 109.3 USD `cAnd`
        give (zcb (mkDate 12) 100 USD))

pr1 :: PR Double
pr1 = evalX c1

tr1 :: [RV Double]
tr1 = unPr pr1

absorbEx :: Date -> Double -> Currency -> Contract
absorbEx t x k = cUntil (konst t %> date) (scale (konst x) (one k))

-- zcbImage = latticeImage pr1 "fig9" "png"

-- c1ExpectedValueUrl = chartUrl $ expectedValuePr pr1

-- rateEvolution = latticeImage (takePr (t1Horizon + 1) $ rateModel xm USD) "fig8" "png"

-- Appendix A - Tests

tolerance :: Double
tolerance = 0.001

testK :: Bool
testK = andPr $ liftPr (== 100) $ takePr 10 (bigK 100)

testProb :: Bool
testProb = (sum $ probabilityLattice !! 100) - 1 < tolerance

testPr1 :: Bool
testPr1 = andPr $ lift2Pr (\a b -> (abs (a - b)) < tolerance)
                           pr1
                           (PR [[8.641], [9.246,8.901], [9.709,9.524,9.346], [10,10,10,10]])

tests :: Bool
tests = and [testK
            ,testProb
            ,testPr1]

-- Appendix B - HTML table output for value process

{-
prToTable pr@(PR rvs) = table << (snd $ foldl renderSlice (0, noHtml) rvs)
  where
    horizon = horizonPr pr
    renderSlice (n, rows) rv = (n+1, rows +++ (tr $ td << (show n)
                                                  +++ (spacer $ horizon - n)
                                                  +++ (concatHtml (map renderCell rv))
                                                  +++ (spacer $ horizon - n + 1)))
    renderCell v = td ! [theclass "cell", colspan 2] << (showFFloat (Just 2) v "")
spacer 0 = noHtml
spacer n = td ! [theclass "sp", colspan n] << noHtml

-- Appendix C - Graphviz Output

latticeImage :: PR Double -> String -> String -> IO ExitCode
latticeImage pr baseName imageType =
  do writeTreeAsDot baseName pr
     runDot baseName imageType

printTree :: PR Double -> IO ()
printTree pr = mapM_ putStrLn (dotGraph (prToDot pr))


writeTreeAsDot :: String -> PR Double -> IO ()
writeTreeAsDot baseName pr = writeFile (baseName ++ dotExt) $ unlines (dotGraph (prToDot pr))

runDot :: String -> String -> IO ExitCode
runDot baseName fileType =
  system $ concat ["dot -T", fileType,
                   " -o ", baseName, ".", fileType, " ",
                   baseName, dotExt]

prToDot :: PR Double -> [String]
prToDot (PR rvs) = rvsToDot rvs


rvsToDot :: [RV Double] -> [String]
rvsToDot rvs = let numberedRvs = assignIds rvs 1
               in showNodes numberedRvs ++ treeToDot numberedRvs

dotExt = ".dot"

assignIds :: [RV a] -> Int -> [RV (Int, a)]
assignIds [] n = []
assignIds (rv:rvs) n = numberList (reverse rv) n : assignIds rvs (n + length rv)
numberList :: [a] -> Int -> [(Int, a)]
numberList l n = zip [n .. n + length l] l


showNodes :: [RV (Int, Double)] -> [String]
showNodes numberedRvs = concatMap showSlice (numberList numberedRvs 0)
  where showSlice (n, sl) = ("subgraph Slice" ++ show n ++ " { rank=same")
                            : (map (\(n,s) -> show n ++ nodeLabel s) sl)
                            ++ ["SL" ++ (show n) ++ " [label=\"" ++ show n ++ "\" style=solid peripheries=0] }"]

nodeLabel :: Double -> String
nodeLabel s = " [label=\"" ++ (showFFloat (Just 2) s "\"]")

treeToDot :: [RV (Int, a)] -> [String]
treeToDot [a] = []
treeToDot (a:b:rest) = dotJoin a (take (length a) b)
                    ++ dotJoin a (tail b)
                    ++ treeToDot (b:rest)
dotJoin :: RV (Int, a) -> RV (Int, a) -> [String]
dotJoin a b = zipWith (\(m,a) (n,b) -> (show m) ++ " -- " ++ (show n)) a b

dotGraph :: [String] -> [String]
dotGraph body = dotGraphHdr ++ (map formatDotStmt body) ++ ["}"]

dotGraphHdr :: [String]
dotGraphHdr = ["graph contract_lattice {"
                ,"  rankdir=LR;"
                ,"  dir=none;"
                ,"  node [style=filled color=pink shape=box fontsize=10 width=0.5 height=0.4];"]

formatDotStmt :: String -> String
formatDotStmt s = "  " ++ s ++ ";"
-}

-- Appendix D - Google chart

{-
chartUrl :: [Double] -> String
chartUrl vs = "http://chart.apis.google.com/chart?chs=300x200&cht=lc&chxt=x,y&chg=20,25,2,5&chxr=0,0,"
              ++ (show $ length vs - 1)
              ++ "|1," ++ (showFFloat (Just 1) ymin ",")
                       ++ (showFFloat (Just 1) ymax "&chd=t:")
              ++ (concat $ intersperse "," $ map (\y -> showFFloat (Just 1) y "") ys)
  where (ymin, ymax, ys) = chartScale vs 100

chartScale ys upper =
  let ymin = minimum ys
      ymax = maximum ys
      yrange = ymax - ymin
      yscale = upper/yrange
  in (ymin, ymax, map (\y -> (y - ymin) * yscale ) ys)
-}

-- Appendix E - Web interface

{-
newtype ExContr = ExContr (String, [Double], Bool) deriving (Read,Show,Eq)

useLatticeImage (ExContr (_, _, b)) = b

webPath = "/home/anton/usr/happs.org/public/"
-- webPath = "/home/anton/happs91/HAppS-Begin/public/"

tmpImgPath = "imgtmp/"

baseDotFilename = "pr-lattice"

pageTitle = "Composing contracts - simple charts"

mkUniqueName :: String -> IO String
mkUniqueName baseName =
  do u <- newUnique
     return $ baseName ++ (show $ hashUnique u)

renderEx :: ExContr -> IO Html
renderEx exSpec@(ExContr (contractId, args, lattice)) =
  let pr = evalEx exSpec
      expValChart = if contractId == "probs" then noHtml -- expected value is meaningless for the probabilities it relies on
                    else h3 << "Expected value" +++ image ! [src (chartUrl $ expectedValuePr pr)]
      imageType = "png"
  in if useLatticeImage exSpec
     then do baseName <- mkUniqueName baseDotFilename
             exitCode <- latticeImage pr (webPath ++ tmpImgPath ++ baseName) imageType
             let pageContents =
                   case exitCode of
                     ExitSuccess -> renderExampleForm exSpec (image ! [src latticeUrl, border 1]) expValChart
                                     where latticeUrl = "/" ++ tmpImgPath ++ baseName ++ "." ++ imageType
                     _ -> p << "renderEx: error generating lattice image"
             return $ renderExamplePage pageContents
     else return $ renderExamplePage $ renderExampleForm exSpec (prToTable pr) expValChart

renderExDefault = renderExamplePage $
                    renderExampleForm (ExContr ("zcb", [fromIntegral t1Horizon, 10], True))
                                      noHtml noHtml

renderExamplePage contents = renderPage pageTitle $
      p ! [align "right"] << anchor ! [href "/Contracts.html"] << "Source code"
  +++ contents

renderPage :: (HTML a, HTML b) => a -> b -> Html
renderPage hdg contents = (header << (styleSheet +++ thetitle << hdg))
                          +++ (body << (h1 << hdg +++ contents))

styleSheet :: Html
styleSheet = thelink ! [rel "stylesheet", thetype "text/css", href "/contracts.css" ] << noHtml
 
evalEx :: ExContr -> PR Double
evalEx (ExContr (name, args, f)) =
  case lookup name examples of
    Just (desc, defaultArgs, f) -> if length args >= length defaultArgs -- ignore extra args
                                   then f args          -- TODO: could handle argument defaulting here? See getArg.
                                   else dummyContract
    _ -> dummyContract
  where
    dummyContract = evalX $ zcb time0 0 USD -- TODO: proper error reporting (to web page if appropriate)

sanitize r = min (truncate r) 20

examples =
  -- Contracts
  [("zcb",   ("Zero-coupon bond",    [t1Horizon, 10],
                                          (\(r:x:_) -> evalX $ zcb (mkDate $ sanitize r) x USD)))
  ,("c11",   ("European option",     [],  (\_       -> evalX c11)))
  -- Underlyings
  ,("probs", ("Probability lattice", [9], (\(r:_)   -> let n = sanitize r + 1 in PR $ take n probabilityLattice)))
  ,("rates", ("Interest rate model", [9], (\(r:_)   -> let n = sanitize r + 1 in takePr n $ rateModel xm USD)))]

renderExampleForm (ExContr (contractId, args, showImage)) chart1 chart2 =
  form ! [method "GET", action "/contractEx"]
    << table << ((tr << (td << "Contract" +++ td << "Horizon" +++ td << "Value" +++ td << "Output"))
             +++ (tr << ((td $ select ! [name "contract"]
                                     << (map (\(id, (desc, defaultArgs, _)) ->
                                                 attrIf (id == contractId) selected (option ! [value id]) << desc)
                                             examples))
                     +++ (td << textfield "arg1" ! [value $ getArg contractId args 0, size "10"])
                     +++ (td << textfield "arg2" ! [value $ getArg contractId args 1, size "10"])
                     +++ (td << (attrIf      showImage  checked (radio "image" "True")  +++ "Image"))))
             +++ (tr  << (td << submit "submit" "Draw" +++ spacer 2
                      +++ td << (attrIf (not showImage) checked (radio "image" "False") +++ "Table"))))
    +++ chart1 +++ hr +++ chart2



getArg id l n = if n < length l then show $ l !! n
                else case lookup id examples of
                       Just (_, args, _) -> if n < length args
                                            then show $ args !! n else ""


attrIf False attr el = el
attrIf True  attr el = el ! [attr]
-}

-- Appendix F - HAppS server integration

{-
module Main where

import HAppS.Server.AlternativeHTTP
import HAppS.Server.HTTP.AltFileServe
import Control.Monad.State
import Numeric

import Contracts

instance FromData ExContr where
  fromData = do c    <- look "contract"
                arg1 <- look "arg1"
                arg2 <- look "arg2"
                img  <- look "image"
                return $ ExContr (c, map fst $ readFloat arg1
                                            ++ readFloat arg2, read img)

main :: IO ()
main = do simpleHTTP [dir "contractEx"
                        [withData $ \(ExContr t) ->
                           [anyRequest $ liftIO $ do out <- renderEx (ExContr t)
                                                     return $ toResponse out]
                        ,anyRequest $ ok $ toResponse renderExDefault]
                     ,fileServe ["Contracts.html"] "public" -- fileserving
                     ]
-}