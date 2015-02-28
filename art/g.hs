
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

playerHeight = 100
playerWidth = 10
ballRadius = 9.0

data Player = Player { px :: Float, py :: Float } 
data Ball = Ball { bx :: Float, by :: Float, bxv :: Float, byv :: Float } 
data World = World { player1 :: Player,
                     player2 :: Player,
                     ball :: Ball }

drawPlayer :: Player -> Picture
drawPlayer player = Translate (px player) (py player) (rectangleSolid playerWidth playerHeight)

drawBall :: Ball -> Picture
drawBall ball = Translate (bx ball) (by ball) (circleSolid ballRadius)

initialWorld :: World
initialWorld = World { player1 = Player (-200) 0,
                       player2 = Player 200 0,
                       ball = Ball 0 0 5 5 }

draw :: World -> Picture
draw world = Pictures [ drawBall (ball world),
                        drawPlayer (player1 world),
                        drawPlayer (player2 world) ]

ballHittingPlayer :: Player -> Ball -> Bool
ballHittingPlayer p b
  | (bx b) /= (px p) = False
  | bally < ptop && bally > pbot = True
  | otherwise = False
  where ptop = (py p) + (playerHeight / 2)
        pbot = (py p) - (playerHeight / 2)
        bally = by b

ballHittingWall :: Ball -> Bool
ballHittingWall b
  | (by b) == 0 = True
  | (by b) == 500 = True
  | otherwise = False

stepBall :: Player -> Player -> Ball -> Ball
stepBall p1 p2 b
  | ballHittingPlayer p1 b || ballHittingPlayer p2 b 
    = Ball ((bx b) - (bxv b)) ((by b) + (byv b)) (- (bxv b)) (byv b)
  | ballHittingWall b 
    = Ball ((bx b) + (bxv b)) ((by b) - (byv b)) (bxv b) (- (byv b))
  | otherwise
    = Ball ((bx b) + (bxv b)) ((by b) + (byv b)) (bxv b) (byv b)
              
step :: Float -> World -> World
step f world = let p1 = player1 world
                   p2 = player2 world
                   b = ball world
                   in World p1 p2 (stepBall p1 p2 (ball world))


event :: Event -> World -> World
event e world = world

-- | Display the last event received as text.
main
 = play (InWindow "PONG" (500, 500) (10, 10))
        white
        10
        initialWorld
        draw
        event
        step


