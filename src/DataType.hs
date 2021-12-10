module DataType
  ( Action(..)
  , Axis(..)
  , Card(..)
  , Color(..)
  , DCard(..)
  , Ext(..)
  , FaceDir(..)
  , Field(..)
  , GSt(..)
  , Pile(..)
  , PileType(..)
  , Rank(..) 
  , Suit(..)
  ) where

import qualified System.Random as R (StdGen)

-- CARD TYPES ------------------------------------------------------------------

data Rank    = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK 
  deriving (Eq, Ord, Bounded, Enum)
instance Show Rank where
  show RA  = "A";
  show R2  = "2"; show R3  = "3"; show R4  = "4"; show R5  = "5";
  show R6  = "6"; show R7  = "7"; show R8  = "8"; show R9  = "9";
  show R10 = [toEnum 0x2491] :: String; -- unicode ligature for one-char width 
  show RJ  = "J"; show RQ  = "Q"; show RK  = "K";

data Suit    = Spade | Heart | Club | Diamond 
  deriving (Eq, Ord, Bounded, Enum)
instance Show Suit where
  show Spade   = [toEnum 0x2660] :: String -- unicode characters for suits
  show Heart   = [toEnum 0x2665] :: String
  show Diamond = [toEnum 0x2666] :: String 
  show Club    = [toEnum 0x2663] :: String

data Card        = Card Rank Suit                    deriving (Eq, Show, Ord)

data FaceDir     = FaceUp | FaceDown                 deriving (Eq, Show, Ord)

data DCard       = DCard { _card    :: Card
                         , _facedir :: FaceDir }     deriving (Eq, Show, Ord)

data PileType    = SelfP | RivalP deriving (Eq, Show, Ord)

data Pile = Pile { _cards    :: [DCard]     
                 , _pileType :: PileType    
                 } deriving (Eq, Show)      

-- GAME TYPES ------------------------------------------------------------------

data Field = Field { _self :: Pile
                   , _rival :: Pile
                   } deriving (Eq, Show)

                                      
data GSt = GSt { _field   :: Field    
               , _seed    :: R.StdGen 
               , _deal :: [Card]
               , _done :: Int
               , _totalBet :: Int
               , _curBet :: Int
               } deriving (Show)

-- DISPLAY TYPES ---------------------------------------------------------------

data Color = Red | Black                       deriving (Eq, Show, Ord)

data Axis = NS | EW deriving (Eq, Show) -- data type for pile splay orientation

data Action = New | Hit | Stand | Reset | AddBet deriving (Eq, Show, Ord) -- data type for button action

data Ext =  RivalX | SelfX | ActionX Action
  deriving (Eq, Show, Ord)
