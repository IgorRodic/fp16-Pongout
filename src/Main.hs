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
  , bricksArray :: [Brick]     -- ^ Plocice
  , pause :: Bool              -- ^ Indikator da li je igra pauzirana
  , start :: Bool              -- ^ Indikator da li je igra pokrenuta
  , reset :: Bool              -- ^ Indikator da li je igra resetovana
  }


-- plocice (x_koordinata, y_koordinata, razbijena)
type Brick = (Float, Float, Bool)

brickX :: Brick -> Float
brickX (x, _, _) = x

brickY :: Brick -> Float
brickY (_, y, _) = y

brickDestroyed :: Brick -> Bool
brickDestroyed (_, _, destroyed) = destroyed

makeBrick :: (Float,Float) -> Brick
makeBrick (x,y) = (x, y, False)

loadLevel :: Int -> [Brick]
loadLevel 1 = map makeBrick [((-360), 90), ((-295), 90), ((-230), 90), ((-165), 90), ((-95), 90), ((-35), 90), (35, 90), (100, 90), (165, 90), (230, 90), (295, 90), (360, 90),
                             ((-260), 45), ((-195), 45), ((-65), 45), (0, 45), (65, 45), (195, 45), (260, 45),
                             ((-345), 0), ((-280), 0), ((-215), 0), ((-150), 0), (150, 0), (215, 0), (280, 0), (345, 0),
                             ((-260), (-45)), ((-195), (-45)), ((-65), (-45)), (0, (-45)), (65, (-45)), (195, (-45)), (260, (-45)),
                             ((-360), (-90)), ((-295), (-90)), ((-230), (-90)), ((-165), (-90)), ((-95), (-90)), ((-35), (-90)), (35, (-90)), (100, (-90)), (165, (-90)), (230, (-90)), (295, (-90)), (360, (-90))
                            ]

loadLevel 2 = map makeBrick [((-360), 90), ((-295), 90), ((-230), 90), ((-145), 90), ((-80), 90), ((-15), 90), (90, 90), (155, 90), (255, 90), (320, 90),
                             ((-360), 45), ((-145), 45), ((-15), 45), (70, 45), (205, 45), (340, 45),
                             ((-360), 0), ((-295), 0), ((-230), 0), ((-145), 0), ((-80), 0), ((-15), 0), (90, 0), (320, 0),
                             ((-360), (-45)), ((-145), (-45)), (130, (-45)), (280, (-45)),
                             ((-360), (-90)), ((-145), (-90)), (205,(-90))
                            ] 

loadLevel 3 = map makeBrick [((-360), (-90)), ((-295), (-90)), ((-230), (-90)), ((-165), (-90)), ((-95), (-90)), ((-35), (-90)), (35, (-90)), (100, (-90)), (165, (-90)), (230, (-90)), (295, (-90)), (360, (-90)),                            
                             ((-360), (-45)), ((-200), (-45)), ((-40), (-45)), (120, (-45)), (280, (-45)),
                             ((-320), 0), ((-240), 0), ((-160), 0), (-80,0), (0,0), (80, 0), (160, 0), (240, 0), (320, 0),
                             ((-280), 45), ((-120), 45), (40, 45), (200, 45), (360, 45),
                             ((-360), 90), ((-295), 90), ((-230), 90), ((-165), 90), ((-95), 90), ((-35), 90), (35, 90), (100, 90), (165, 90), (230, 90), (295, 90), (360, 90)
                            ]

loadLevel 4 = map makeBrick [((-360), (-90)), ((-295), (-90)), ((-230), (-90)), ((-95), (-90)), ((-35), (-90)), (35, (-90)), (165, (-90)), (230, (-90)), (295, (-90)),                            
                             ((-360), (-45)), ((-295), (-45)), ((-230), (-45)), ((-95), (-45)), ((-35), (-45)), (35, (-45)), (165, (-45)), (230, (-45)), (295, (-45)),                           
                             ((-360), 0), ((-230), 0), ((-95), 0), (35, 0), (165, 0), (295, 0),                           
                             ((-360), 45), ((-230), 45), ((-165), 45), ((-95), 45), (35, 45), (100, 45), (165, 45), (295, 45), (360, 45),                            
                             ((-360), 90), ((-230), 90), ((-165), 90), ((-95), 90), (35, 90), (100, 90), (165, 90), (295, 90), (360, 90)
                            ]
                             
loadLevel _ = []

brickWidth :: Float
brickWidth = 40

brickHeight :: Float
brickHeight = 20

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

menuHeight :: Float
menuHeight = 350

menuWidth :: Float
menuWidth = 650

backgroundImage :: GameObject
backgroundImage  = createGameObject (0, 0) (900, 900) ("images/background.png", 1920, 1080) 

leftWall :: GameObject
leftWall = createGameObject (-390, 0) (wallWidth, wallHeight) ("images/wall.png", 1500, 1500) 

rightWall :: GameObject
rightWall = createGameObject (390, 0) (wallWidth, wallHeight) ("images/wall.png", 1500, 1500)

beginMenu :: GameObject
beginMenu = createGameObject (0, 0) (menuWidth, menuHeight) ("images/menu1.png", 650 , 350)

pauseMenu :: GameObject
pauseMenu = createGameObject (0, 0) (menuWidth, menuHeight) ("images/menu2.png", 650 , 350)

noMenu :: GameObject
noMenu = createGameObject (0,0) (0,0) ("images/menu2.png", 650 , 350)

createBrick :: Brick -> GameObject
createBrick brick = createGameObject ((brickX brick), (brickY brick)) (brickWidth, brickHeight) ("images/brick.png", 304, 122)

createBricksObjects :: [Brick] -> [GameObject]
createBricksObjects bricks = map (\b -> createBrick b) notDestroyed
                          where
                             notDestroyed = filter (\x -> not (brickDestroyed x)) bricks

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
  , bricksArray = loadLevel 1
  , pause = True
  , reset = False
  , start = False
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
  , bricksArray = loadLevel 1
  , pause = False
  , start = True
  , reset = False 
  }

-- | Azuriranje stanja igre
update :: Float -> PongoutGame -> PongoutGame
update seconds currentGame = if (reset currentGame) then 
                                resetGame currentGame
                              else if (not $ pause currentGame) then 
                                (wallBounce $ movePlayer $ bricksBounce $ moveBall seconds currentGame)
                              else currentGame


-- | Funkcija iscrtavanja
render :: PongoutGame -- ^ Stanje igre.
       -> Picture      -- ^ Slika stanja igre.
render game =
  pictures [backgroundPicture,
            bricks,
            balls,
            walls,
            players,
            menu]
  where
    -- Pozadina.
    backgroundPicture = pictures [drawGameObject backgroundImage]

    -- Plocice
    brickPicture :: GameObject -> Picture
    brickPicture obj = drawGameObject obj

    bricksObj = createBricksObjects (bricksArray game)

    bricks = Pictures (foldr (\x acc -> [(brickPicture x)] ++ acc) [Blank] bricksObj)
 
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

    -- Menu
    menuPicture :: GameObject -> Picture
    menuPicture obj = drawGameObject obj

    menu = if (pause game) then 
               (if (start game) then 
                  (pictures [menuPicture pauseMenu])
               else (pictures [menuPicture beginMenu]))
           else (pictures [menuPicture noMenu])



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

    collisionTypeLeftWall = getCollisionType $ detectCollision (ball game) leftWall 

    collisionTypeRightWall = getCollisionType $ detectCollision (ball game) rightWall

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

-- | Obradjivanje kolizije sa plocicama
bricksBounce :: PongoutGame -> PongoutGame
bricksBounce game = game { ballVel = (vx', vy'), bricksArray = bricksArray' }
  where    
    collisions = map (\b -> getCollisionType (detectCollision (ball game) b)) (createBricksObjects (bricksArray game))
    collisionsTypes = filter (\c -> c /= NoCollision ) collisions
    
    typeCollision = if (collisionsTypes /= []) then (head collisionsTypes) else NoCollision

    vx' = if typeCollision == LeftCollision || 
             typeCollision == RightCollision
          then -(fst (ballVel game)) else (fst (ballVel game))

    vy' = if typeCollision == TopCollision || 
             typeCollision == BottomCollision 
          then -(snd (ballVel game)) else (snd (ballVel game))

    bricksArray' = changeBrickArray (bricksArray game) (ball game)

changeBrickArray :: [Brick] -> GameObject -> [Brick]
changeBrickArray bricks ball = map (\b -> ((brickX b), (brickY b), (makeDestroy b))) bricks
    where makeDestroy b = if ((not(brickDestroyed b)) && (getCollisionType(detectCollision ball (createBrick b))/=NoCollision)) then True
                          else (brickDestroyed b)


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
-- Pauziranje igre 'p'
handleKeys (EventKey (Char 'p') Down _ _) game = if (start game) then (game { pause = not (pause game) }) else game
handleKeys (EventKey (Char 'p') Up _ _) game = game
-- Pokretanje igre 'n'
handleKeys (EventKey (Char 'n') Down _ _) game = if (pause game) then (game { reset = True, start = True, pause = False }) else game
handleKeys (EventKey (Char 'n') Up _ _) game = game
-- Default
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
