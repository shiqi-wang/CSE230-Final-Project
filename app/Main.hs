{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad                (void)
import Graphics.Vty as Vty
import Lens.Micro.TH                (makeLenses)
import qualified System.Random as R (next, newStdGen)

import Brick.AttrMap 
import Brick.Main
import Brick.Types 
import Brick.Util 

import DataType
import UI        (drawUI)
import Logic     (newGame, resetGame, mkInitS, hitCard, rivalTurn, finished, incBet)

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

-------------------------------------------------------------------------------
appEvent :: GSt -> BrickEvent Ext e -> EventM Ext (Next GSt)
appEvent s (VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc        [] -> halt s
  Vty.EvKey (Vty.KChar 'q') [] -> halt s
  Vty.EvMouseDown col row _ _  -> do
    extents <- map extentName <$> findClickedExtents (col, row)
    case extents of 
      [ActionX New]            -> continue $ newGame s
      [ActionX Reset]           -> continue $ resetGame s
      [ActionX Hit]            -> if finished s /= 0
                                    then continue s
                                    else continue $ hitCard s
      [ActionX Stand]          -> continue $ rivalTurn s
      [ActionX AddBet]         -> continue $ incBet s
      _                        -> continue s
  _                            -> continue s 
appEvent s _                    = continue s

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [ ( attrName "redCard"   , (defAttr `withForeColor` Vty.red) `withStyle` bold)
     , ( attrName "blackCard" , (defAttr `withForeColor` Vty.white) `withStyle` bold)
     , ( attrName "btnAttr"   , (defAttr `withForeColor` Vty.cyan) `withStyle` underline)
     , ( attrName "bold"      , defAttr `withStyle` bold)
     , ( attrName "betAttr"   , (defAttr `withForeColor` Vty.yellow) `withStyle` bold)
     ]

app :: App GSt e Ext
app = App { appDraw         = drawUI          -- s -> [Widget n]
          , appChooseCursor = showFirstCursor -- s -> [CursorLocation n] 
                                              --   -> Maybe (CursorLocation n)
          , appHandleEvent  = appEvent        -- s -> BrickEvent n e 
                                              --   -> EventM n (Next s)
          , appStartEvent   = return          -- s -> EventM n s
          , appAttrMap      = const aMap      -- s -> AttrMap
          }

main :: IO ()
main = do
  let buildVty = do
        v <- Vty.mkVty =<< Vty.standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        return v
  n <- R.newStdGen
  void $ customMain buildVty Nothing app $ mkInitS n 1000




