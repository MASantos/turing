# turing
Implementing a Turing machine for playing with Haskell

   A dirty implementation of a Turing Machine in Haskell.

   Basically, it consists on a 'set' of Symbols, States & Transitions.
   Each States is given as a pair of type (Statelabel, [Transition]) listing all possible transitions off the given states.
   There is an infinite tape where the input 'code' is given and where the machine reads off from and writes to.
   The rest is boilerplate.

   Main abstract data types (ADT) are:
```
          data Symbol =  Blank | Zero | One  deriving Eq
          type Tape = [Symbol]
          data Statelabel = Q0 | Q1 | Q2 | HLT deriving (Eq,Show)
          data Transition = Nil | Trans Symbol Symbol HeadMove Statelabel
          type State = (Statelabel, [Transition])
          type TransitionTable = [State]
          --Basic definition of a Turing Machine (TM)
          data TM = TM {
                        --current state of the machine  out of a finite set  of states 'Statelabel'
                        tmQ::Statelabel
                        ,
                        --initial state
                        tmQo::Statelabel
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
```

# TODO: 
check consistencym, ...
