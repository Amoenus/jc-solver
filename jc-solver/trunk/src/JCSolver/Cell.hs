module JCSolver.Cell (
    Cell (..),
    ) where

data Cell = CellEmpty | CellFilled | CellBlocked deriving Eq

instance Show Cell where
    show CellEmpty = "_"
    show CellFilled = "#"
    show CellBlocked = "."