{-# LANGUAGE RecordWildCards #-}
module HState.GraphViz where

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Types
import HState.Core
import Data.List
import Data.Text.Lazy (Text)
import HState.Tutorial

schemaToDot :: (Eq state, Labellable event) => Schema state event -> DotGraph state
schemaToDot Schema{..} = DotGraph
  { strictGraph = False
  , directedGraph = True
  , graphID = Nothing
  , graphStatements = DotStmts
      { attrStmts = []
      , subGraphs = []
      , nodeStmts = map mkNode $ nub $ concatMap (\(start, _, end) -> [start, end]) schemaValidTransitions
      , edgeStmts = map mkEdge schemaValidTransitions
      }
  }
  where
    mkEdge (from, edge, to) = DotEdge from to 
      [ toLabel edge
      ]
    mkNode state = DotNode state []

renderSchema :: (Eq state, PrintDot state, Labellable event) => Schema state event -> Text
renderSchema = renderDot . toDot . schemaToDot

instance PrintDot WakingMachineState where
  unqtDot = unqtDot . show
  toDot = toDot . show

instance Labellable WakingMachineEvent where
  toLabelValue = toLabelValue . show

exampleStateMachine :: Text
exampleStateMachine = renderSchema wakingMachineSchema 