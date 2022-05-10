{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HState.GraphViz where

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Types
import Data.GraphViz.Attributes.Complete
import HState.Core
import Data.List
import Data.Text.Lazy (Text)
import HState.Tutorial
import Data.GraphViz.Attributes.Complete (Attribute(BgColor))

fontChoice :: Attribute
fontChoice = FontName "Fira Code"

schemaToDot :: (Eq state, Labellable event) => Schema state event -> DotGraph state
schemaToDot Schema{..} = DotGraph
  { strictGraph = False
  , directedGraph = True
  , graphID = Nothing
  , graphStatements = DotStmts
      { attrStmts = 
        [ GraphAttrs 
          [ fontChoice
          , FontSize 10
          , NodeSep 1.25
          , RankSep [1.25]
          , BgColor [WC (RGB 0xfd 0xf9 0xf3) Nothing]
          ]
        , NodeAttrs 
          [ fontChoice
          , FontSize 10
          , Color [WC (RGB 0x2c 0x29 0x2d) Nothing]
          ]
        , EdgeAttrs 
          [ fontChoice
          , FontSize 8
          ]
        ]
      , subGraphs = []
      , nodeStmts = map mkNode $ nub $ concatMap (\(start, _, end) -> [start, end]) schemaValidTransitions
      , edgeStmts = map mkEdge schemaValidTransitions
      }
  }
  where
    mkEdge (from, edge, to) = DotEdge from to 
      [ xLabel edge
      ]
    mkNode state = DotNode state 
      [ shape BoxShape
      , Style [SItem Rounded []]
      , Margin $ DVal 0.2
      ]

renderSchema :: (Eq state, PrintDot state, Labellable event) => Schema state event -> Text
renderSchema = renderDot . toDot . schemaToDot

instance PrintDot WakingMachineState where
  unqtDot = unqtDot . show
  toDot = toDot . show

instance Labellable WakingMachineEvent where
  toLabelValue = toLabelValue . show

exampleStateMachine :: Text
exampleStateMachine = renderSchema wakingMachineSchema 