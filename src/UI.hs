module UI
  ( drawUI
  ) where

import Brick.AttrMap              (attrName)
import Brick.Types                (Widget, Padding(Pad))
import Brick.Widgets.Border       (vBorder, border, hBorder, borderWithLabel)
import Brick.Widgets.Border.Style 
import Brick.Widgets.Center       (center, hCenter)
import Brick.Widgets.Core
import Text.Printf                (printf)

import DataType
import Logic (toColor, finished, rivalScore, selfScore, getTotalBet, getCurBet)

-------------------------------------------------------------------------------

cardStyle :: Widget Ext -> Widget Ext -- borderwithstyle wrapper
cardStyle = withBorderStyle unicodeRounded . border 

rrGhost :: Pile -> Widget Ext -- renders a 'ghost' card with no content
rrGhost _ = withBorderStyle ghostRounded $ border $ str "  "
  where ghostRounded = BorderStyle 
          { bsIntersectFull = toEnum 0x253C
          , bsCornerTL      = toEnum 0x256D , bsCornerTR      = toEnum 0x256E
          , bsCornerBR      = toEnum 0x256F , bsCornerBL      = toEnum 0x2570
          , bsIntersectL    = toEnum 0x251C , bsIntersectR    = toEnum 0x2524
          , bsIntersectT    = toEnum 0x252C , bsIntersectB    = toEnum 0x2534
          , bsHorizontal    = ' '           , bsVertical      = ' '
          }

rrDCard :: Axis -> Int -> DCard -> Widget Ext -- renders a displaycard.
rrDCard axis idx dc = cropBy margin           -- are aware of their position
                    $ rrCard inner            -- within a splayed pile.
  where cropBy = if axis == NS then cropBottomBy else cropRightBy 
        margin = mkMargin axis idx (_facedir dc)
          where mkMargin :: Axis -> Int -> FaceDir -> Int
                mkMargin _  0 _        = 0
                mkMargin NS _ FaceUp   = 1
                mkMargin NS _ FaceDown = 2
                mkMargin EW _ FaceUp   = 1
                mkMargin EW _ FaceDown = 3
        inner  = if _facedir dc == FaceDown then Nothing else Just (_card dc)

rrCard :: Maybe Card -> Widget Ext               -- renders card internals
rrCard Nothing           = withAttr (attrName "bold")
                         $ cardStyle             -- either a card back
                         $ str ([toEnum 0x03BB, '='] :: String)
rrCard (Just (Card r s)) = withAttr (attrName c) -- or a card front.
                         $ cardStyle $ str $ show s ++ show r
  where c = if Red == toColor s then "redCard" else "blackCard"

rrDCards :: Axis -> [DCard] -> Widget Ext -- renders a pile of displaycards
rrDCards axis dcs = nBox [  rrDCard axis idx dc 
                         | (idx,dc) <- zip idxs dcs 
                         ]
  where nBox = if axis == NS then vBox else hBox -- aware of splay axis
        idxs = reverse [0..(length dcs - 1)]

rrPile :: Axis -> Pile -> Widget Ext -- renders a pile of cards
rrPile axis p = rrDCards axis $ reverse $ _cards p

rrPiles :: Axis -> Axis -> [Pile] -> Widget Ext -- renders a list of piles
rrPiles ax ax' ps = nBox [ rrPile ax' p 
                         | (p,idx) <- zip ps [0..] 
                         ]
  where nBox = if ax == NS then vBox else hBox -- along its own 2ndary axis


mkButton :: Action -> Widget Ext
mkButton action = reportExtent (ActionX action)
                $ padRight (Pad 2)
                $ str "[" <+> 
                ( withAttr (attrName "btnAttr")
                $ str (show action)
                ) <+> str "]"

scoreBox :: Int -> Widget Ext
scoreBox i = padRight (Pad 1)
           $ str "Score: "
         <+> withAttr (attrName "bold") (str $ printf "%2d" i)

movesBox :: Int -> Widget Ext
movesBox i = padRight (Pad 2)
           $ str "Moves: "
         <+> withAttr (attrName "bold") (str $ printf "%2d" i)

betBox :: String -> Int -> Widget Ext
betBox s i = withAttr (attrName "bold") (str s) <+> withAttr (attrName "betAttr") (str $ show i)

drawUI :: GSt -> [ Widget Ext ]
drawUI state = [ui]
  where 
    ui = center $ setAvailableSize (55,25) 
       $ hCenter logo 
       <=> board 
       <=> ((padLeft (Pad 6) $ betBox "Total Bet: " $ getTotalBet state) <+> (padLeft (Pad 10) $ betBox "Current Bet: " $ getCurBet state)) 
       <=> bottomBar
    logo     = str "    ____  __           __     _            __  \n   / __ )/ /___ ______/ /__  (_)___ ______/ /__\n  / __  / / __ `/ ___/ //_/ / / __ `/ ___/ //_/\n / /_/ / / /_/ / /__/ ,<   / / /_/ / /__/ ,<   \n/_____/_/\\__,_/\\___/_/|_|_/ /\\__,_/\\___/_/|_|  \n                       /___/                   \n\n"
                  --" ____  _     ____  ____  _  __    _  ____  ____  _  __\n/  _ \\/ \\   /  _ \\/   _\\/ |/ /   / |/  _ \\/   _\\/ |/ /\n| | //| |   | / \\||  /  |   /    | || / \\||  /  |   / \n| |_\\| |_/\\| |-|||  \\_ |   \\ /\\_| || |-|||  \\_ |   \\ \n\\____/\\____/\\_/ \\|\\____/\\_|\\_\\____/\\_/ \\|\\____/\\_|\\_\\"
    title      = case finished state of
                   0 -> " Blackjack "
                   1 -> " You win! "
                   2 -> " You lose... "
                   3 -> " Draw "
                   4 -> " Should not happen"
    board      = withBorderStyle unicodeRounded
               $ borderWithLabel (str title) 
               $ drawField $ _field state
    bottomBar   = padAll 1
               $ hCenter $ hBox (map mkButton [Reset, New, AddBet, Hit, Stand])

    drawField :: Field -> Widget Ext
    drawField f = (center deskUp) 
                  <=> (hCenter $ scoreBox (
                    if ((finished state) > 0)
                      then rivalScore state
                      else 0
                  ))
                  <=> hBorder 
                  <=> (center deskDown) 
                  <=> (hCenter $ scoreBox (selfScore state)) 
      where deskUp     = reportExtent RivalX $ rrPile EW $ _rival f
            deskDown   = reportExtent SelfX $ rrPile EW $ _self f

