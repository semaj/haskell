
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

playerHeight = 100
playerWidth = 10
ballRadius = 9.0

data Player = Player { playerx :: Float, playery :: Float } 
data Ball = Ball { ballx :: Float, bally :: Float } 
data World = World { player1 :: Player,
                     player2 :: Player,
                     ball :: Ball }

drawPlayer :: Player -> Picture
drawPlayer player = Translate (playerx player) (playery player) (rectangleSolid playerWidth playerHeight)

drawBall :: Ball -> Picture
drawBall ball = Translate (ballx ball) (bally ball) (circleSolid ballRadius)

initialWorld :: World
initialWorld = World { player1 = Player (-200) 0,
                       player2 = Player 200 0,
                       ball = Ball 0 0 }

draw :: World -> Picture
draw world = Pictures [drawBall (ball world), drawPlayer (player1 world), drawPlayer (player2 world)]
              
step :: Float -> World -> World
step f world = world

event :: Event -> World -> World
event e world = world

-- | Display the last event received as text.
main
 = play (InWindow "PONG" (500, 500) (10, 10))
        white
        1
        initialWorld
        draw
        event
        step


