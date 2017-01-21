module Main where

import GameObject
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Geometry.Line
import Data.Maybe

type Radius = Float 
type Position = (Float, Float)

-- data CollisionType = TopCollision 
--                    | BottomCollision
--                    | LeftCollision
--                    | RightCollision
--                    | NoCollision
--                    deriving (Eq) 

data PongoutGame = Game
  { ball :: GameObject         -- ^ Lopta
  , ballVel :: (Float, Float)  -- ^ Brzina lopte (x, y).
  , player1 :: GameObject      -- ^ Prvi igrac
  , player2 :: GameObject      -- ^ Drugi igrac
  , player1Left :: Bool        -- ^ Kretanje prvog igraca u levo.
  , player1Right :: Bool       -- ^ Kretanje prvog igraca u desno.
  , player2Left :: Bool        -- ^ Kretanje drugog igraca u levo.
  , player2Right :: Bool       -- ^ Kretanje drugog igraca u desno.
  }

windowSizeWidth :: Int
windowSizeWidth = 800

windowSizeHeight :: Int
windowSizeHeight = 700

windowOffset :: Int
windowOffset = 10

window :: Display
window = InWindow "Pongout" (windowSizeWidth, windowSizeHeight) (windowOffset, windowOffset)

background :: Color
background = white

fps :: Int
fps = 60

ballRadius :: Float
ballRadius = 20

wallHeight :: Float
wallHeight = 690

wallWidth :: Float
wallWidth = 10

playerHeight :: Float
playerHeight = 20

playerWidth :: Float
playerWidth = 80

backgroundImage :: GameObject
backgroundImage  = createGameObject (0, 0) (900, 900) ("images/background.png", 1920, 1080) 

leftWall :: GameObject
leftWall = createGameObject (-390, 0) (wallWidth, wallHeight) ("images/wall.png", 1500, 1500) 

rightWall :: GameObject
rightWall = createGameObject (390, 0) (wallWidth, wallHeight) ("images/wall.png", 1500, 1500) 

initialState :: PongoutGame
initialState = Game
  { ball = createGameObject (0, 0) (ballRadius, ballRadius) ("images/ball.png", 311, 309) 
  , ballVel = (300, 0)
  , player1 = createGameObject (40, (-320)) (playerWidth, playerHeight) ("images/paddle.png", 2400, 930) 
  , player2 = createGameObject ((-80), 320) (playerWidth, playerHeight) ("images/paddle.png", 2400, 930) 
  , player1Left = False
  , player1Right = False
  , player2Left = False
  , player2Right = False
  }

resetGame :: PongoutGame -> PongoutGame
resetGame game = game 
  { ball = createGameObject (0, 0) (ballRadius, ballRadius) ("images/ball.png", 311, 309) 
  , ballVel = (300, 0)
  , player1 = createGameObject (40, (-320)) (playerWidth, playerHeight) ("images/paddle.png", 2400, 930) 
  , player2 = createGameObject ((-80), 320) (playerWidth, playerHeight) ("images/paddle.png", 2400, 930) 
  , player1Left = False
  , player1Right = False
  , player2Left = False
  , player2Right = False
  }

-- | Azuriranje stanja igre
update :: Float -> PongoutGame -> PongoutGame
update seconds currentGame = wallBounce $ movePlayer $ moveBall seconds currentGame


-- | Funkcija iscrtavanja
render :: PongoutGame -- ^ Stanje igre.
       -> Picture      -- ^ Slika stanja igre.
render game =
  pictures [backgroundPicture,
            balls,
            walls,
            players]
  where
    -- Pozadina.
    backgroundPicture = pictures [drawGameObject backgroundImage]

    --  Lopta.
    ballPicture :: GameObject -> Picture
    ballPicture obj = drawGameObject obj

    balls = pictures [ballPicture (ball game)]

    --  Zidovi.
    wall :: GameObject -> Picture
    wall obj = drawGameObject obj

    walls = pictures [wall leftWall, wall rightWall]

    --  Igraci.
    paddle :: GameObject -> Picture
    paddle obj = drawGameObject obj

    players = pictures [paddle (player1 game), paddle (player2 game)]


-- | Kretanje lopte
moveBall :: Float        -- ^ Broj sekundi od proslog azuriranja pozicije.
         -> PongoutGame -- ^ Inicijalno stanje igre.
         -> PongoutGame -- ^ Dobijeno stanje igre, sa azuriranom pozicijom lopte.
moveBall seconds game = game { ball = newBall }
  where
    -- Stara brzina i lokacija.
    (x, y) = getGameObjectCoordinates (ball game)
    (vx, vy) = ballVel game

    -- Nova pozicija.
    x' = vx * seconds
    y' = vy * seconds

    newBall = moveGameObject (ball game) x' y'


movePlayer :: PongoutGame -> PongoutGame
movePlayer game = game { player1 = newPlayer1, player2 = newPlayer2 }
  where
    -- Pozicije i nacini kretanja igraca
    (player1X, player1Y) = getGameObjectCoordinates (player1 game)
    (player2X, player2Y) = getGameObjectCoordinates (player2 game)
    leftArrowHeld = player1Left game 
    rightArrowHeld = player1Right game 
    aHeld = player2Left game 
    dHeld = player2Right game

    -- Izracunavanje pomeraja igraca
    player1Step = if (player1X > -60 + fromIntegral windowSizeWidth / 2) then -7
                  else if (player1X < 60 -fromIntegral windowSizeWidth / 2) then 7 
                  else if leftArrowHeld == True then -7
                  else if rightArrowHeld then 7 
                  else 0

    player2Step = if (player2X > -60 + fromIntegral windowSizeWidth / 2) then -7
                  else if (player2X < 60 -fromIntegral windowSizeWidth / 2) then 7 
                  else if aHeld == True then -7 
                  else if dHeld then 7 
                  else 0

    newPlayer1 = moveGameObject (player1 game) player1Step 0
    newPlayer2 = moveGameObject (player2 game) player2Step 0


-- | Obradjivanje kolizije sa zidovima
wallBounce :: PongoutGame -> PongoutGame
wallBounce game = game { ballVel = (vx', vy') }
  where
    -- Stara brzina i lokacija.
    (x, y) = getGameObjectCoordinates (ball game)
    (vx, vy) = ballVel game

    (collisionTypeLeftWall, angleLeft1, angleRight1, angleTop1, angleBottom1) = detectCollision (ball game) leftWall 

    (collisionTypeRightWall, angleLeft2, angleRight2, angleTop2, angleBottom2) = detectCollision (ball game) rightWall

    --Nova brzina u odnosu na vrstu kolizije.
    vx' = if collisionTypeLeftWall == LeftCollision || 
             collisionTypeLeftWall == RightCollision ||
             collisionTypeRightWall == LeftCollision ||
             collisionTypeRightWall == RightCollision
          then -vx else vx

    vy' = if collisionTypeLeftWall == TopCollision || 
             collisionTypeLeftWall == BottomCollision ||
             collisionTypeRightWall == TopCollision ||
             collisionTypeRightWall == BottomCollision
          then -vy else vy


-- | Reagovanje na dogadjaje tastature
handleKeys :: Event -> PongoutGame -> PongoutGame
-- Kontrole prvog igraca <- i ->
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { player1Left = True }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { player1Right = True }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { player1Left = False }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { player1Right = False }
-- Kontrole drugog igraca 'a' i 'd'
handleKeys (EventKey (Char 'a') Down _ _) game = game { player2Left = True }
handleKeys (EventKey (Char 'd') Down _ _) game = game { player2Right = True }
handleKeys (EventKey (Char 'a') Up _ _) game = game { player2Left = False }
handleKeys (EventKey (Char 'd') Up _ _) game = game { player2Right = False }
-- Default
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
