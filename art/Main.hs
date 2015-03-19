module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

playerHeight = 100
playerWidth = 10
ballRadius = 9.0

data Player = Player { px :: Float, py :: Float } 
data Ball = Ball { bx :: Float, by :: Float, bxv :: Float, byv :: Float } 
instance Show Ball where
  show b = "bx: " ++ (show (bx b)) ++ " by: " ++ (show (by b)) ++ " bxv: " ++ (show (bxv b)) ++ " byv: " ++ (show (byv b))

instance Show Player where
  show p = "px: " ++ (show (px p)) ++ " py: " ++ (show (py p))

data World = World { player1 :: Player,
                     player2 :: Player,
                     ball :: Ball }

drawPlayer :: Player -> Picture
drawPlayer player = Translate (px player)
                              (py player)
                              (rectangleSolid playerWidth playerHeight)

drawBall :: Ball -> Picture
drawBall ball = Translate (bx ball) (by ball) (circleSolid ballRadius)

initialWorld :: World
initialWorld = World { player1 = Player (-301) 0,
                       player2 = Player 301 0,
                       ball = Ball 0 0 3 5 }

draw :: World -> Picture
draw world = Pictures [ drawBall (ball world),
                        drawPlayer (player1 world),
                        drawPlayer (player2 world) ]
 


ballHittingPlayer1 :: Player -> Ball -> Bool
ballHittingPlayer1 p b
  | bally < ptop && bally > pbot && (bx b) <= (px p) = True
  | otherwise = False
  where ptop = (py p) + (playerHeight / 2)
        pbot = (py p) - (playerHeight / 2)
        bally = by b

ballHittingPlayer :: Player -> Ball -> Bool
-- ballHittingPlayer p b | trace ((show b) ++ "\n" ++ (show p)) False = undefined
ballHittingPlayer p b
  | bally < ptop && bally > pbot && (bx b) >= (px p) = True
  | otherwise = False
  where ptop = (py p) + (playerHeight / 2)
        pbot = (py p) - (playerHeight / 2)
        bally = by b

ballOutOfPlay :: Ball -> Bool
ballOutOfPlay b = if (bx b) > 303 || (bx b) < -303 then True else False

ballHittingWall :: Ball -> Bool
ballHittingWall b
  | (by b) <= -250 = True
  | (by b) >= 250 = True
  | otherwise = False

stepBall :: Player -> Player -> Ball -> Ball
stepBall p1 p2 b
   | (ballOutOfPlay b) = undefined
   | (ballHittingPlayer p2 b) -- || (ballHittingPlayer p1 b)
     = Ball ((bx b) - (bxv b)) ((by b) + (byv b)) (- (bxv b)) (byv b)
   | (ballHittingPlayer1 p1 b)
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
event (EventKey (SpecialKey KeyUp) _ _ _) world
  = World (player1 world) 
          (Player (px (player2 world)) (10 + (py (player2 world))))
          (ball world)
event (EventKey (SpecialKey KeyDown) _ _ _) world
  = World (player1 world)
          (Player (px (player2 world)) ((py (player2 world)) - 10))
          (ball world)
event (EventKey (Char 'w') _ _ _) world
  = World (Player (px (player1 world)) ((py (player1 world)) + 10))
          (player2 world)
          (ball world)
event (EventKey (Char 's') _ _ _) world
  = World (Player (px (player1 world)) ((py (player1 world)) - 10))
          (player2 world) 
          (ball world)
event event world = world

-- | Display the last event received as text.
main
 = play (InWindow "PONG" (600, 500) (10, 10))
        white
        50
        initialWorld
        draw
        event
        step


