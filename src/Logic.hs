{-# LANGUAGE TemplateHaskell #-}

module Logic
  ( initialDeal
  , mkInitS
  , newGame
  , toColor
  , hitCard
  , selfScore
  , rivalScore
  , rivalTurn
  , finished
  , getTotalBet
  , getCurBet
  , resetGame
  , incBet
  ) where

import Data.List.Split (splitPlaces)
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head, Lens', lens, each )
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (next, StdGen, randomR)
import qualified System.Random.Shuffle as R (shuffle')

import DataType

-------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

-------------------------------------------------------------------------------

toColor :: Suit -> Color -- assigns colors to suits
toColor Spade = Black
toColor Club  = Black
toColor _     = Red

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank] -- list of all ranks

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit] -- list of all suits

-- given a game with a seed, get a new seed and use it to spawn a new game
newGame :: GSt -> GSt  
newGame s = let seed' = snd $ R.next $ s ^. seed 
            in  mkInitS seed' $ s ^. totalBet

resetGame :: GSt -> GSt
resetGame s = let seed' = snd $ R.next $ s ^. seed 
              in  mkInitS seed' 1000


finished :: GSt -> Int
finished s 
        | (ss <= 21) && (d == 0) = 0
        | (ss <= 21) && (d == 1) && ((rs > 21) || (rs < ss)) = 1
        | (ss > 21 || (d == 1 && ss < rs)) = 2
        | (d == 1 && ss == rs) = 3
        | otherwise = 4
        where ss = selfScore s
              rs = rivalScore s
              d = s ^. done

selfL :: Lens' Field [DCard] 
selfL = lens (\f -> f ^. self.cards)           
             (\f dcs -> f & self.cards .~ dcs) 

rivalL :: Lens' Field [DCard] 
rivalL = lens (\f -> f ^. rival.cards & each.facedir .~ FaceUp)           
              (\f dcs -> f & rival.cards .~ (dcs & each.facedir .~ FaceUp))


hitCard :: GSt -> GSt
hitCard s = s2
        where s2 = s1 & totalBet %~ (+ c)
              s1 = addSelfCard s
              c = case (finished s1) of
                      1 -> s ^. curBet
                      2 -> - s ^. curBet
                      otherwise -> 0

addSelfCard :: GSt -> GSt
addSelfCard s = s & seed .~ (snd $ R.next $ s ^. seed)
                  & field . selfL %~ ([DCard { _card = initialDeal !! (fst $ R.randomR (2, 49) (s ^. seed)),
                                           _facedir = FaceUp
                                        }] ++)
                          

rivalTurnHelper :: GSt -> GSt
rivalTurnHelper s = 
        if rivalScore s < selfScore s 
                then rivalTurnHelper $ addRivalCard s
                else s & done .~ 1
                       & field . rival . cards . each . facedir .~ FaceUp

rivalTurn :: GSt -> GSt
rivalTurn s = if (finished s /= 0)
        then s
        else s2 
        where s2 = s1 & totalBet %~ (+ c)
              s1 = rivalTurnHelper s
              c = case (finished s1) of
                      1 -> s ^. curBet
                      2 -> - s ^. curBet
                      otherwise -> 0


addRivalCard :: GSt -> GSt
addRivalCard s = s & seed .~ (snd $ R.next $ s ^. seed)
                   & done .~ 1
                   & field . rivalL %~ ([DCard { _card = initialDeal !! (fst $ R.randomR (2, 49) (s ^. seed)),
                                           _facedir = FaceUp
                                        }] ++)

cardVal :: DCard -> Int
cardVal DCard { _card = (Card r _),
                _facedir = _} 
        = case r of 
        RA -> 1
        R2 -> 2
        R3 -> 3
        R4 -> 4
        R5 -> 5
        R6 -> 6
        R7 -> 7
        R8 -> 8
        R9 -> 9
        _ -> 10

selfScore :: GSt -> Int
selfScore s = calcScore $ s ^. field . self . cards

rivalScore :: GSt -> Int
rivalScore s = calcScore $ s ^. field . rival . cards

calcScore :: [DCard] -> Int
calcScore cards = if (null $ filter (<=21) ret)
                        then minimum ret
                        else maximum $ filter (<=21) ret
                  where
                          ret = calcHelper cards 0

calcHelper :: [DCard] -> Int -> [Int]
calcHelper [] s = [s]
calcHelper (x:xs) s
  | cardVal x == 1 = calcHelper xs (s + 1) ++ calcHelper xs (s + 11)
  | otherwise = calcHelper xs $ s + (cardVal x)

getTotalBet :: GSt -> Int
getTotalBet s = s ^. totalBet

getCurBet :: GSt -> Int
getCurBet s = s ^. curBet

incBet :: GSt -> GSt
incBet s = if (finished s == 0 && length (s ^. field . self . cards) == 2 && s ^. curBet < 100)
               then s & curBet %~ (+ 10)
               else s


-- the default deal is a sorted list of cards. to be shuffled below
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]

-- take a random generator and create a game state...
mkInitS :: R.StdGen -> Int -> GSt
mkInitS seed total = GSt { _field = field 
                   , _seed = seed  
                   , _deal = deal   
                   , _done = 0
                   , _totalBet = total 
                   , _curBet = 10
                   }
  where
    deal  = R.shuffle' initialDeal 52 seed -- ...by shuffling the initialDeal
    field = Field { _self = self
                  , _rival = rival
                  }
    self = Pile { _cards  = [ DCard {  _card = c
                                   , _facedir = FaceUp
                                   }
                            | c <- take 2 deal
                            ]
                , _pileType = SelfP
                }
    rival = Pile { _cards  = [ DCard {  _card = c
                                   , _facedir = f
                                   }
                             | (c,f) <- zip (drop 50 deal) (FaceUp: repeat FaceDown)
                             ]
                , _pileType = RivalP
                }

