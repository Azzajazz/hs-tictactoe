module Player (
    Player(..),
    otherPlayer, -- Player -> Player
    playerSquare -- Player -> Square
) where

import Board (Square(..))

data Player = P1 | P2
instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

-- Returns the other player
otherPlayer :: Player -> Player
otherPlayer P1 = P2
otherPlayer P2 = P1

-- Returns the Square corresponding to the player.
playerSquare :: Player -> Square
playerSquare P1 = X
playerSquare P2 = O
