-- TypeSynonymInstances: use type synonyms in instance definitions
-- FlexibleInstances: using complex, nested, or multiparameter types in instance definitions
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances , OverlappingInstances #-}

--Symbols read by the turing machine (TM)
data Symbol =  Blank | Zero | One  deriving Eq
start = Zero -- Blank
-- Rule for printing out symbols
instance Show Symbol where
         show Blank = "Blank" -- "_"
         show Zero = "Zero" -- "0"
         show One = "One" -- "1"
--tape is a linked list of symbols 
type Tape = [Symbol]
--useful conversions Tape <-> String
fromStringToTape :: String -> Tape
fromStringToTape [] = [Blank]
fromStringToTape (c:st) = (ctoTape c) : fromStringToTape st 
                 where 
                 ctoTape '0' = Zero
                 ctoTape '1' = One

fromTapeToString :: Tape -> String
fromTapeToString t@[Blank] = ""
fromTapeToString t@(s:ss) = (tape2str s) ++ fromTapeToString ss
                 where
                 tape2str Zero = "0"
                 tape2str One = "1"

--with leading start symbol
fromTapeToString' :: Tape -> String
fromTapeToString' t = (tape2str start) ++ "." ++ fromTapeToString t ++ "." ++ (tape2str start)
                 where
                 tape2str Blank = "_"
                 tape2str Zero = "0"
                 tape2str One = "1"

--Moves available for the head of the tape
data HeadMove = L | R | S deriving (Eq,Show)
--position of head along the tape are denoted by integers
type Tposition = Int
--Being a 'tape' entails, having a procedure to read/write the/a symbol at the current position and move the head along the tape (peeking on the next symbol)
class RwmTape t where
      tread :: t -> Tposition -> Symbol
--
      twrite :: t -> Tposition -> Symbol -> ( t , Tposition) --If tape content grows, 'current position' changes
--
      tmove :: t -> Tposition -> HeadMove -> Tposition 
--
      tlen :: t -> Tposition

instance RwmTape Tape where
      tread  tp p  
                  | (p<0) || (p>= tlen tp) = start
                  | otherwise = tp !! p
      twrite tp p s 
                  | p<0 = ( [s] ++ (take (-p-1) $ repeat start) ++ tp , p' )
                  | p>=(tlen tp) = ( tp ++ (take (p-(tlen tp)-1) $ repeat start) ++ [s] , p' )
                  | otherwise = ( (take p tp) ++ [s] ++ (drop (p+1) tp) , p )
                  where
                  p' | p<0 = p+1
                     | otherwise = p-1
      tmove tp p m =  p'
            where 
            p' 
              | m == L = p-1 -- infinite tape || max (p-1) 0 
              | m == R = p+1 -- infinite tape || min (p+1) (length tp - 1)
              | m == S = p
      tlen tp = length tp

--Possible states of a finite turing machine
data Statelabel = Q0 | Q1 | Q2 deriving (Eq,Show)
--Define what a transition on the TM means
data Transition = Nil | Trans Symbol Symbol HeadMove Statelabel 
instance Show Transition where 
         show Nil = ""
         show (Trans is fs m fq) = (show is) ++ " -> " ++ (show fs) ++ " " ++ (show m) ++ " " ++ (show fq)
--A state is a pair of a state label and and array of possible transitions 
type State = (Statelabel, [Transition])
instance (Show s, Show t) => Show (s,[t]) where 
         show (q, t) = (show q) ++ ":\n" ++ showtt t where
                 showtt [] = ""
                 showtt  (t:ts) = "\t" ++ (show t) ++ "\n" ++ (showtt ts)  
--A transition table, simply a list of states, precisely defines what our TM does
type TransitionTable = [State]
instance Show TransitionTable where
         show [] = ""
         show (s:ss) = show s ++ "\n" ++ show ss
--Basic definition of a Turing Machine (TM)
data TM = TM {
              --current state of the machine  out of a finite set  of states 'Statelabel'
              tmQ::Statelabel  
              ,
              --initial state
              tmQo::Statelabel
              --,
              --current symbol at tape's head out of a finite set of symbols 'Symbol'
              --tmS::Symbol
              ,
              --distinguished blank symbol
              tmSo::Symbol
              ,
              --transition table := partial function tmQ x tm∑ -> { tm∑ } x {HeadMove} x { tmQ }
              tmD::TransitionTable
              ,
              --current position along the tape --this is not nice: as TM has to know about a 'tape' definied outside 
              tmpos:: Tposition
              ,
              --the TM's tape
              tmtape::Tape
             }

class TuringMachine machine where
     initst :: machine -> Statelabel -> machine
     cstate :: machine -> Statelabel
     step   :: machine -> machine
     graph   :: machine -> IO ()

instance TuringMachine TM where
     initst tm q = TM { tmQo = q } 
     cstate tm = tmQ tm
     step tm = tm { tmQ = q , tmpos = p , tmtape = tp}
             where
             cq = tmQ tm 
             cp = tmpos tm 
             cs = tread (tmtape tm) cp
             --(xq, Trans xs s m q) = head $ filter (\(x,Trans ys yns ynq) -> x==cq && ys==cs) $ tmD tm
             (xq, Trans xs s m q) = 
                    let fltransym s (Trans xs _ _ _) 
                                                     | xs == s = True
                                                     | otherwise = False
                        fltcq q (x,y) 
                                   | x==q = True
                                   | otherwise = False
                        in head $ map (\(x,y)-> (x, (head . filter (fltransym cs)) y)) $ filter (fltcq cq) $ tmD tm
                    -- head $ map (\(x,y)-> (x, (head . filter (\(Trans xs _ _ _) ) ) y)) $ filter (\(x,y) -> x==cq) $ tmD tm
             ( tp , np ) = twrite (tmtape tm) cp s
             p = tmove (tmtape tm) np m
             {--
             p | m == L = cp-1 -- max (cp-1) 0
               | m == R = cp+1
               | m == S = cp
             --}
     graph tm = do
                print $ (tmD tm)

printCurrentMachineState :: TM -> IO ()
printCurrentMachineState m = do 
         print  $ (fromTapeToString' (tmtape m) ) ++ " Q:" ++ show (tmQ m) ++ " P:" ++ show (tmpos m) ++ " S:" ++  show (tread (tmtape m) (tmpos m))
--Run a turing machine
runTM :: TM -> Tape -> IO (TM)
runTM tm tp = do
         putStrLn $ "Starting...\nTape: " ++  (fromTapeToString' tp)
         let thistm = tm{tmtape=tp}
         let loop m 
                    | (cstate m) /= Q2 = do
                                         --print $ (fromTapeToString' (tmtape m) ) ++ " Q:" ++ show (tmQ m) ++ " P:" ++ show (tmpos m) ++ " S:" ++ (tread (tmtape m) (tmpos m))
                                         printCurrentMachineState m
                                         loop $ step m
                    | otherwise = do
                                  printCurrentMachineState m
                                  --putStr ""
                                  return m
             in loop thistm
         --print $ fromTapeToString' (tmtape thistm)
         print "Halted"
         return thistm
----------------------------------------------------------------

------------------------------------------------------------
--Example of Turing Machine 
transitions :: TransitionTable
transitions = [
               (Q0,
                   [
                    Trans Zero One R Q1 
                   ,
                    Trans One One L Q1 
                   ]
               )
               ,
               (Q1,
                   [
                    Trans Zero One L Q0 
                   ,
                    Trans One One R Q2 
                   ]
               )
               ,
               (Q2,
                   [
                    Nil
                   ]
               )
              ]

turingMach = TM {
             tmQ=Q0  --can be anything, but at the beginning should be initialized to initial state
            ,
             tmQo=Q0
            --,
            -- tmS=Blank --Idem as for state
            ,
             tmSo=Blank
            ,
            -- transition table
             tmD=transitions
            ,
             tmpos=0
            --Initial Blank tape
            ,
            tmtape=[Blank]
            }

tape = fromStringToTape "1110110011"

tflips s 
        | s == Zero = One
        | s == One  = Zero
        | otherwise = s

main = do
     {-- 
     print $ tape
     print $ fromTapeToString $ tape
     print $ tlen tape
     print $ tread tape 0
     print $ twrite tape 0 $ tflips $ tread tape 0
     print $ tmove tape R 0 
     --}
     graph turingMach 
     runTM turingMach tape
