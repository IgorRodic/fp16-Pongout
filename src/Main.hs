module Main where

import GameObject
import Level
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Geometry.Line
import Data.Maybe

type Radius = Float 
type Position = (Float, Float)

data PongoutGame = Game
  { ball1 :: GameObject        -- ^ Prva lopta
  , ball2 :: GameObject        -- ^ Druga lopta
  , ball1Vel :: (Float, Float) -- ^ Brzina prve lopte (x, y).
  , ball2Vel :: (Float, Float) -- ^ Brzina druge lopte (x, y).
  , player1 :: GameObject      -- ^ Prvi igrac
  , player2 :: GameObject      -- ^ Drugi igrac
  , player1Left :: Bool        -- ^ Kretanje prvog igraca u levo.
  , player1Right :: Bool       -- ^ Kretanje prvog igraca u desno.
  , player2Left :: Bool        -- ^ Kretanje drugog igraca u levo.
  , player2Right :: Bool       -- ^ Kretanje drugog igraca u desno.
  , bricksArray :: [Brick]     -- ^ Plocice
  , level :: Int               -- ^ Nivo
  , nextLevel :: Bool          -- ^ Indikator da li se prelazi na sledeci nivo 
  , gameEnd :: Bool            -- ^ Indikator da li je igra zavrsena
  , pause :: Bool              -- ^ Indikator da li je igra pauzirana
  , start :: Bool              -- ^ Indikator da li je igra pokrenuta
  , reset :: Bool              -- ^ Indikator da li je igra resetovana
  , player1Points :: Int       -- ^ Poeni prvog igraca.
  , player2Points :: Int       -- ^ Poeni drugog igraca.
  }

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

backgroundWidth :: Float
backgroundWidth = 900

backgroundHeight :: Float
backgroundHeight = 900

winnerWidth :: Float
winnerWidth = 500

winnerHeight :: Float
winnerHeight = 250

backgroundImages :: [GameObject]
backgroundImages = [ createGameObject (0, 0) (backgroundWidth, backgroundHeight) ("images/background1.png", 1920, 1080),
                     createGameObject (0, 0) (backgroundWidth, backgroundHeight) ("images/background2.png", 1920, 1080),
                     createGameObject (0, 0) (backgroundWidth, backgroundHeight) ("images/background3.png", 1920, 1080),
                     createGameObject (0, 0) (backgroundWidth, backgroundHeight) ("images/background4.png", 1920, 1080),
                     createGameObject (0, 0) (backgroundWidth, backgroundHeight) ("images/background.png", 1920, 1080)
                   ]

winner1 :: GameObject
winner1 = createGameObject (0, 0) (winnerWidth, winnerHeight) ("images/winner1.png", 500, 250)

winner2 :: GameObject
winner2 = createGameObject (0, 0) (winnerWidth, winnerHeight) ("images/winner2.png", 500, 250) 

leftWall :: GameObject
leftWall = createGameObject (-390, 0) (wallWidth, wallHeight) ("images/wall.png", 1500, 1500) 

rightWall :: GameObject
rightWall = createGameObject (390, 0) (wallWidth, wallHeight) ("images/wall.png", 1500, 1500)

beginMenu :: GameObject
beginMenu = createGameObject (0, 0) (menuWidth, menuHeight) ("images/menu1.png", 650 , 350)

pauseMenu :: GameObject
pauseMenu = createGameObject (0, 0) (menuWidth, menuHeight) ("images/menu2.png", 650 , 350)

nextLevelImage :: GameObject
nextLevelImage = createGameObject (0, 105) (550, 90) ("images/nextLevel.png", 550, 90)

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
  { ball1 = createGameObject (0, -100) (ballRadius, ballRadius) ("images/ball1.png", 581, 604) 
  , ball2 = createGameObject (0, 100) (ballRadius, ballRadius) ("images/ball2.png", 581, 604) 
  , ball1Vel = (0, -200)
  , ball2Vel = (0, 200)
  , player1 = createGameObject (40, (-320)) (playerWidth, playerHeight) ("images/player1.png", 581, 257) 
  , player2 = createGameObject ((-80), 320) (playerWidth, playerHeight) ("images/player2.png", 581, 257) 
  , player1Left = False
  , player1Right = False
  , player2Left = False
  , player2Right = False
  , bricksArray = loadLevel 1
  , level = 1
  , nextLevel = False
  , gameEnd = False
  , pause = True
  , reset = False
  , start = False
  , player1Points = 0
  , player2Points = 0
  }

resetGame :: PongoutGame -> PongoutGame
resetGame game = game 
  { ball1 = createGameObject (0, -100) (ballRadius, ballRadius) ("images/ball1.png", 581, 604) 
  , ball2 = createGameObject (0, 100) (ballRadius, ballRadius) ("images/ball2.png", 581, 604) 
  , ball1Vel = (0, -200)
  , ball2Vel = (0, 200)
  , player1 = createGameObject (40, (-320)) (playerWidth, playerHeight) ("images/player1.png", 581, 257) 
  , player2 = createGameObject ((-80), 320) (playerWidth, playerHeight) ("images/player2.png", 581, 257) 
  , player1Left = False
  , player1Right = False
  , player2Left = False
  , player2Right = False
  , bricksArray = loadLevel 1
  , level = 1
  , gameEnd = False
  , nextLevel = False
  , pause = False
  , start = True
  , reset = False 
  , player1Points = 0
  , player2Points = 0
  }

-- | Azuriranje stanja igre
update :: Float -> PongoutGame -> PongoutGame
update seconds currentGame = if (reset currentGame) then 
                                resetGame currentGame
                              else if (not $ pause currentGame) then 
                                (paddleBounce $ wallBounce $ bricksBounce $ movePlayer $ checkPoints $ moveBalls seconds currentGame)
                              else currentGame


-- | Funkcija iscrtavanja
render :: PongoutGame -- ^ Stanje igre.
       -> Picture     -- ^ Slika stanja igre.
render game =
  pictures [backgroundPicture,
            bricks,
            balls,
            walls,
            players,
            menu,
            nextLevelPicture,
            pointsRectangle 300 350,
            pointsText ("Points: " ++ (show $ player1Points game)) 305 (-305),
            pointsRectangle (-295) 355,
            pointsText ("Points: " ++ (show $ player2Points game)) 310 290,
            winner
            ]
  where
    -- Pozadina.
    backgroundPicture = pictures [drawGameObject (backgroundImages !! ((level game)-1))]

    -- Plocice
    brickPicture :: GameObject -> Picture
    brickPicture obj = drawGameObject obj

    bricksObj = createBricksObjects (bricksArray game)

    bricks = Pictures (foldr (\x acc -> [(brickPicture x)] ++ acc) [Blank] bricksObj)
 
    --  Lopta.
    ballPicture :: GameObject -> Picture
    ballPicture obj = drawGameObject obj

    balls = pictures [ballPicture (ball1 game), ballPicture (ball2 game)]

    --  Zidovi.
    wall :: GameObject -> Picture
    wall obj = drawGameObject obj

    walls = pictures [wall leftWall, wall rightWall]

    --  Igraci.
    paddle :: GameObject -> Picture
    paddle obj = drawGameObject obj

    players = pictures [paddle (player1 game), paddle (player2 game)]

    -- Menu.
    menuPicture :: GameObject -> Picture
    menuPicture obj = drawGameObject obj

    menu = if ((pause game) && (not (gameEnd game))) then 
               (if (start game) then 
                  (pictures [menuPicture pauseMenu])
               else (pictures [menuPicture beginMenu]))
           else (pictures [menuPicture noMenu])

    -- Sledeci nivo
    nextLevelPicture = if ((nextLevel game) && (not (gameEnd game))) then
                    pictures [drawGameObject nextLevelImage]
                else
                    pictures [drawGameObject noMenu]
    -- Poeni.
    -- Prikaz poena
    pointsRectangle :: Float -> Float -> Picture
    pointsRectangle x y = rotate 90 $
                          translate x y $
                          color pointsRectangleColor $
                          rectangleSolid 30 100

    pointsRectangleColor = dark green

    pointsText :: String -> Float -> Float -> Picture
    pointsText tekst x y = translate x y $
                           color pointsTextColor $
                           scale 0.1 0.1 $
                           text tekst

    pointsTextColor = white

    -- Prikaz pobednika
    winner = if (gameEnd game) then 
                (if ((player1Points game) > (player2Points game)) then 
                    (pictures [drawGameObject winner1]) 
                else (pictures [drawGameObject winner2])) 
             else (pictures [drawGameObject noMenu])

-- Provera da li je potrebno promeniti nivo
changeLevel :: PongoutGame -> Bool
changeLevel game = if ((maxPoints `div` 1000) + 1) > (level game) then True else False
    where
        maxPoints = if ((player1Points game) > (player2Points game)) then (player1Points game) else (player2Points game)

-- Promena nivoa
checkPoints :: PongoutGame -> PongoutGame
checkPoints game = game {level = level', pause = pause', bricksArray = bricksArray', nextLevel = nextLevel', gameEnd = gameEnd' }
    where 
        level' = if (changeLevel game) then (((level) game) + 1) else (level game)
        pause' = if (changeLevel game) then True else False
        bricksArray' = if (changeLevel game) then (loadLevel level') else (bricksArray game)
        nextLevel' = if (changeLevel game) then True else False
        gameEnd' = if (level' > 4) then True else False

-- | Kretanje lopte.
moveBalls :: Float        -- ^ Broj sekundi od proslog azuriranja pozicije.
          -> PongoutGame -- ^ Inicijalno stanje igre.
          -> PongoutGame -- ^ Dobijeno stanje igre, sa azuriranom pozicijom lopte.
moveBalls seconds game = game { ball1 = newBall1, ball2 = newBall2 }
  where
    -- Stare brzine.
    (vx1, vy1) = ball1Vel game
    (vx2, vy2) = ball2Vel game

    -- Nove pozicije.
    x1' = vx1 * seconds
    y1' = vy1 * seconds

    x2' = vx2 * seconds
    y2' = vy2 * seconds

    newBall1 = moveGameObject (ball1 game) x1' y1'
    newBall2 = moveGameObject (ball2 game) x2' y2'


movePlayer :: PongoutGame -> PongoutGame
movePlayer game = game { player1 = newPlayer1, player2 = newPlayer2 }
  where
    -- Pozicije i nacini kretanja igraca.
    (player1X, player1Y) = getGameObjectCoordinates (player1 game)
    (player2X, player2Y) = getGameObjectCoordinates (player2 game)
    leftArrowHeld = player1Left game 
    rightArrowHeld = player1Right game 
    aHeld = player2Left game 
    dHeld = player2Right game

    -- Izracunavanje pomeraja igraca.
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


-- | Obradjivanje kolizije sa zidovima.
wallBounce :: PongoutGame -> PongoutGame
wallBounce game = game { ball1 = newBall1, 
                         ball2 = newBall2, 
                         ball1Vel = vxReset1, 
                         ball2Vel = vxReset2, 
                         player1Points = oldPlayer1Points + newPlayer1Ponts, 
                         player2Points = oldPlayer2Points + newPlayer2Ponts }
  where
    collisionTypeLeftWall1 = getCollisionType $ detectCollision (ball1 game) leftWall 
    collisionTypeRightWall1 = getCollisionType $ detectCollision (ball1 game) rightWall

    collisionTypeLeftWall2 = getCollisionType $ detectCollision (ball2 game) leftWall 
    collisionTypeRightWall2 = getCollisionType $ detectCollision (ball2 game) rightWall

    --Nova brzina prve lopte.
    vx1' = if collisionTypeLeftWall1 == LeftCollision || 
             collisionTypeLeftWall1 == RightCollision ||
             collisionTypeRightWall1 == LeftCollision ||
             collisionTypeRightWall1 == RightCollision
          then -(fst $ ball1Vel game) else (fst $ ball1Vel game)

    vy1' = if collisionTypeLeftWall1 == TopCollision || 
             collisionTypeLeftWall1 == BottomCollision ||
             collisionTypeRightWall1 == TopCollision ||
             collisionTypeRightWall1 == BottomCollision
          then -(snd $ ball1Vel game) else (snd $ ball1Vel game)

    --Nova brzina druge lopte.
    vx2' = if collisionTypeLeftWall2 == LeftCollision || 
             collisionTypeLeftWall2 == RightCollision ||
             collisionTypeRightWall2 == LeftCollision ||
             collisionTypeRightWall2 == RightCollision
          then -(fst $ ball2Vel game) else (fst $ ball2Vel game)

    vy2' = if collisionTypeLeftWall2 == TopCollision || 
             collisionTypeLeftWall2 == BottomCollision ||
             collisionTypeRightWall2 == TopCollision ||
             collisionTypeRightWall2 == BottomCollision
          then -(snd $ ball2Vel game) else (snd $ ball2Vel game)

    --Provera da li je loptica prosla iza nekog od igraca.
    (x1, y1) = getGameObjectCoordinates (ball1 game)
    (x2, y2) = getGameObjectCoordinates (ball2 game)
    (oldPlayer1Points, oldPlayer2Points) = ((player1Points game), (player2Points game))

    vxReset1 = if y1 < -380 || y1 > 380
                 then (0, -100)
               else (vx1', vy1')

    newPlayer1Ponts = if y1 < -380
                        then -30
                      else if y1 > 380
                        then 100
                      else 0
    newBall1 = if newPlayer1Ponts /= 0
                 then resetGameObject (ball1 game) 0 (-100)
               else
                 (ball1 game)

    vxReset2 = if y2 < -380 || y2 > 380
                 then (0, 100)
               else (vx2', vy2')

    newPlayer2Ponts = if y2 > 380
                        then -30
                      else if y2 < -380
                        then 100
                      else 0
    newBall2 = if newPlayer2Ponts /= 0
                 then resetGameObject (ball2 game) 0 100
               else
                 (ball2 game)


-- | Obradjivanje kolizije sa plocicama.
bricksBounce :: PongoutGame -> PongoutGame
bricksBounce game = game { ball1Vel = (vx1', vy1'), 
                           ball2Vel = (vx2', vy2'), 
                           bricksArray = bricksArray'',
                           player1Points = oldPlayer1Points + newPlayer1Ponts, 
                           player2Points = oldPlayer2Points + newPlayer2Ponts }
  where    
    (oldPlayer1Points, oldPlayer2Points) = ((player1Points game), (player2Points game))

    collisions1 = map (\b -> getCollisionType (detectCollision (ball1 game) b)) (createBricksObjects (bricksArray game))
    collisionsTypes1 = filter (\c -> c /= NoCollision ) collisions1
    typeCollision1 = if (collisionsTypes1 /= []) then (head collisionsTypes1) else NoCollision

    collisions2 = map (\b -> getCollisionType (detectCollision (ball2 game) b)) (createBricksObjects (bricksArray game))
    collisionsTypes2 = filter (\c -> c /= NoCollision ) collisions2
    typeCollision2 = if (collisionsTypes2 /= []) then (head collisionsTypes2) else NoCollision

    vx1' = if typeCollision1 == LeftCollision || 
             typeCollision1 == RightCollision
          then -(fst $ ball1Vel game) else (fst $ ball1Vel game)

    vy1' = if typeCollision1 == TopCollision || 
             typeCollision1 == BottomCollision 
          then -(snd $ ball1Vel game) else (snd $ ball1Vel game)

    vx2' = if typeCollision2 == LeftCollision || 
             typeCollision2 == RightCollision
          then -(fst $ ball2Vel game) else (fst $ ball2Vel game)

    vy2' = if typeCollision2 == TopCollision || 
             typeCollision2 == BottomCollision 
          then -(snd $ ball2Vel game) else (snd $ ball2Vel game)

    -- Izracunavanje poena.
    newPlayer1Ponts = if typeCollision1 /= NoCollision
                        then 20
                      else 0

    newPlayer2Ponts = if typeCollision2 /= NoCollision
                        then 20
                      else 0

    bricksArray'  = changeBrickArray (bricksArray game) (ball1 game)
    bricksArray'' = changeBrickArray bricksArray' (ball2 game)

changeBrickArray :: [Brick] -> GameObject -> [Brick]
changeBrickArray bricks ball = map (\b -> ((brickX b), (brickY b), (makeDestroy b))) bricks
    where makeDestroy b = if ((not(brickDestroyed b)) && (getCollisionType(detectCollision ball (createBrick b))/=NoCollision)) then True
                          else (brickDestroyed b)


-- | Obradjivanje kolizije sa igracima.
paddleBounce :: PongoutGame -> PongoutGame
paddleBounce game = game { ball1Vel = (vx1', vy1'), ball2Vel = (vx2', vy2') }
  where
    (collisionTypePlayer1, leftAnglePlayer1, rightAnglePlayer1, topAnglePlayer1, bottomAnglePlayer1) = detectCollision (ball1 game) (player1 game)
    (collisionTypePlayer2, leftAnglePlayer2, rightAnglePlayer2, topAnglePlayer2, bottomAnglePlayer2) = detectCollision (ball1 game) (player2 game)

    (collisionTypePlayer1', leftAnglePlayer1', rightAnglePlayer1', topAnglePlayer1', bottomAnglePlayer1') = detectCollision (ball2 game) (player1 game)
    (collisionTypePlayer2', leftAnglePlayer2', rightAnglePlayer2', topAnglePlayer2', bottomAnglePlayer2') = detectCollision (ball2 game) (player2 game)

    -- Azurirana brzina prve lopte.
    vx1' = -- Prvi igrac.
          if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 < -10
            then 10*topAnglePlayer1
          else if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 >= -10 && topAnglePlayer1 <= 0
            then -60
          else if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 >= 0 && topAnglePlayer1 <= 10
            then 60
          else if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 > 10
            then 10*topAnglePlayer1
          else if collisionTypePlayer1 == LeftCollision
            then -500
          else if collisionTypePlayer1 == RightCollision
            then 500
          -- Drugi igrac.
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 < -10
            then 10*bottomAnglePlayer2
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 >= -10 && bottomAnglePlayer2 <= 0
            then -60
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 >= 0 && bottomAnglePlayer2 <= 10
            then 60
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 > 10
            then 10*bottomAnglePlayer2
          else if collisionTypePlayer2 == LeftCollision
            then -500
          else if collisionTypePlayer2 == RightCollision
            then 500
          else fst $ ball1Vel game

    vy1' = -- Prvi igrac.
          if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 < -10
            then (-10)*topAnglePlayer1
          else if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 >= -10 && topAnglePlayer1 <= 0
            then 350
          else if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 >= 0 && topAnglePlayer1 <= 10
            then 350
          else if collisionTypePlayer1 == TopCollision && topAnglePlayer1 /= 0 && topAnglePlayer1 > 10
            then 10*topAnglePlayer1
          else if collisionTypePlayer1 == LeftCollision
            then -500
          else if collisionTypePlayer1 == RightCollision 
            then -500
          -- Drugi igrac.
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 < -10
            then 15*bottomAnglePlayer2
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 >= -10 && bottomAnglePlayer2 <= 0
            then -350
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 >= 0 && bottomAnglePlayer2 <= 10
            then -350
          else if collisionTypePlayer2 == BottomCollision && bottomAnglePlayer2 /= 0 && bottomAnglePlayer2 > 10
            then (-15)*bottomAnglePlayer2
          else if collisionTypePlayer2 == LeftCollision
            then 500
          else if collisionTypePlayer2 == RightCollision
            then 500
          else snd $ ball1Vel game

    -- Azurirana brzina druge lopte.
    vx2' = -- Prvi igrac.
          if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' < -10
            then 10*topAnglePlayer1'
          else if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' >= -10 && topAnglePlayer1' <= 0
            then -60
          else if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' >= 0 && topAnglePlayer1' <= 10
            then 60
          else if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' > 10
            then 10*topAnglePlayer1'
          else if collisionTypePlayer1' == LeftCollision
            then -500
          else if collisionTypePlayer1' == RightCollision
            then 500
          -- Drugi igrac.
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' < -10
            then 10*bottomAnglePlayer2'
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' >= -10 && bottomAnglePlayer2' <= 0
            then -60
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' >= 0 && bottomAnglePlayer2' <= 10
            then 60
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' > 10
            then 10*bottomAnglePlayer2'
          else if collisionTypePlayer2' == LeftCollision
            then -500
          else if collisionTypePlayer2' == RightCollision
            then 500
          else fst $ ball2Vel game

    vy2' = -- Prvi igrac.
          if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' < -10
            then (-10)*topAnglePlayer1'
          else if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' >= -10 && topAnglePlayer1' <= 0
            then 350
          else if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' >= 0 && topAnglePlayer1' <= 10
            then 350
          else if collisionTypePlayer1' == TopCollision && topAnglePlayer1' /= 0 && topAnglePlayer1' > 10
            then 10*topAnglePlayer1'
          else if collisionTypePlayer1' == LeftCollision
            then -500
          else if collisionTypePlayer1 == RightCollision
            then -500
          -- Drugi igrac.
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' < -10
            then 10*bottomAnglePlayer2'
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' >= -10 && bottomAnglePlayer2' <= 0
            then -350
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' >= 0 && bottomAnglePlayer2' <= 10
            then -350
          else if collisionTypePlayer2' == BottomCollision && bottomAnglePlayer2' /= 0 && bottomAnglePlayer2' > 10
            then (-10)*bottomAnglePlayer2'
          else if collisionTypePlayer2' == LeftCollision
            then 500
          else if collisionTypePlayer2' == RightCollision
            then 500
          else snd $ ball2Vel game


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
handleKeys (EventKey (Char 'p') Down _ _) game = if (start game && (not (gameEnd game))) then (game { pause = not (pause game), nextLevel = False }) else game
handleKeys (EventKey (Char 'p') Up _ _) game = game
-- Pokretanje igre 'n'
handleKeys (EventKey (Char 'n') Down _ _) game = if (pause game) then (game { reset = True, start = True, pause = False, nextLevel = False }) else game
handleKeys (EventKey (Char 'n') Up _ _) game = game
-- Default
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
