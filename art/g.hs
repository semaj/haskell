
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

PlayerHeight = 10
PlayerWidth = 10

data Player = Player { x :: Float, y :: Float } 

data World = World { player1 :: Picture,
                     player2 :: Picture,
                     ball :: Picture
                   }
initialWorld :: World
initialWorld = World { player1 = (circleSolid 6.0),
                       player2 = (circleSolid 6.0),
                       ball = (circleSolid 5.0) }

draw :: World -> Picture
draw world = Translate 10 10 (ball world)
              
step :: Float -> World -> World
step f world = world

event :: Event -> World -> World
event e world = world

-- | Display the last event received as text.
main
 = play (InWindow "GameEvent" (700, 600) (10, 10))
        white
        1
        initialWorld
        draw
        event
        step


