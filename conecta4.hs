
-- DANIEL PEINADO GINÉS Y DAVID INFANTES RULLO

{- CONECTA 4.
    ESCRIBIR conectaCuatro EN TERMINAL PARA EMPEZAR A JUGAR.
    Observaciones:
        Para que no se diese el caso de que a mismos movimientos se diese siempre la misma partida,
    intentamos importar la librería System.Random para elegir aleatoriamente entre las jugadas con la mejor valoración.
    Sin embargo, no hemos sido capaces ya que dicha librería no estaba instalada y no hemos sabido instalarla.
                                                                                                                        -}
import Data.List
import Data.Maybe

data Jugador = Jugador1 | Jugador2
    deriving (Eq, Show, Read)

type Tablero = [[Int]]

data Posicion = Posicion Tablero Jugador
    deriving (Read)
instance Show Posicion where
    show (Posicion tablero jugador) = printMatriz tablero ++ show jugador ++ "\n"
        where printMatriz (xs:xss) = printLinea xs ++ printMatriz xss
              printMatriz _ = ""
              printLinea (y:ys) = caracter:' ':printLinea ys
                where caracter
                        |y == 1 = 'O' 
                        |y == -1 = 'X'
                        |otherwise = '.'
              printLinea _ = "\n"

tableroInicial :: Posicion
tableroInicial = Posicion (replicate 6 $ replicate 7 0) Jugador1

type Movimiento = Int


posiblesMovimientos :: Posicion -> [Movimiento]
posiblesMovimientos (Posicion tablero jugador) = [i | (i, col) <- columnas, elem 0 col]
    where columnas = zip [0..6] (transpose tablero)
    
hacerMovimiento :: Posicion -> Movimiento -> Posicion
hacerMovimiento (Posicion tablero jugador) n
    |n <= 6 && (elem n (posiblesMovimientos (Posicion tablero jugador))) = Posicion nuevoTablero siguienteJugador
    |otherwise = Posicion tablero jugador
    where columnas = transpose tablero
          finalColumna = dropWhile (==0) (columnas !! n)
          nuevaColumna = replicate (5 - length finalColumna) 0 ++ ficha:finalColumna
          (siguienteJugador, ficha) = if jugador == Jugador1 then (Jugador2, 1) else (Jugador1, -1)
          nuevoTablero = transpose $ take n columnas ++ nuevaColumna : drop (n+1) columnas

hayGanador :: Posicion -> Maybe String
hayGanador (Posicion tablero jugador)
    |horizontal || vertical || diag = Just (show jugadorPrevio)
    |otherwise = Nothing
    where horizontal = hayCuatroEnRaya tablero
          vertical = hayCuatroEnRaya $ transpose tablero
          diag = hayCuatroEnRaya $ diagonales tablero
          jugadorPrevio = if jugador == Jugador1 then Jugador2 else Jugador1
        
hayCuatroEnRayaLinea :: (Eq a, Num a) => [a] -> Bool
hayCuatroEnRayaLinea (x:xs)
    |length iz >= 4 && x /= 0 = True
    |otherwise = hayCuatroEnRayaLinea dr
    where (iz, dr) = span (==x) (x:xs)
hayCuatroEnRayaLinea _ = False

hayCuatroEnRaya :: (Eq a, Num a) => [[a]] -> Bool
hayCuatroEnRaya (xs:xss) = (hayCuatroEnRayaLinea $ dropWhile (==0) xs) || hayCuatroEnRaya xss
hayCuatroEnRaya _ = False

diagonalPrincipal :: [[a]] -> [a]
diagonalPrincipal [[]] = []
diagonalPrincipal (xs:[]) = [head xs]
diagonalPrincipal (x:xs) = head x : (diagonalPrincipal $ map tail xs)

diagonales :: [[a]] -> [[a]]
diagonales xss = diagTipo1 xss ++ diagTipo2 xss ++ diagTipo1 zss ++ diagTipo2 zss
    where 
          zss = map reverse xss
          diagTipo1 (ys:yss)
              |length (ys:yss) >= 4 = diagonalPrincipal (ys:yss) : diagTipo1 yss
              |otherwise = [] 
          diagTipo2 yss = diagTipo1 $ transpose yss

evaluacion :: Posicion -> Int
evaluacion (Posicion tablero jugador)
    | isNothing ganador = sum $ zipWith (*) puntuaciones (map sum  tablero)
    | otherwise = if fromJust ganador == "Jugador1" then maximo else maximo*(-1)
    where ganador = hayGanador (Posicion tablero jugador)
          puntuaciones = [1..4] ++ reverse [1..3]
          maximo = (maxBound::Int) `div` 2


minimax :: Int -> ([Int]->Int) -> ([Int]->Int) -> Posicion -> Int
minimax profundidad peor mejor posicion
    |(profundidad==0) || null siguientes || not (isNothing (hayGanador posicion)) = evaluacion posicion
    |otherwise = mejor $ map (minimax (profundidad - 1) mejor peor) siguientes
    where siguientes = [hacerMovimiento posicion movimiento | movimiento <- posiblesMovimientos posicion]
            
minimax' :: Int -> ([Int] -> Int) -> ([Int] -> Int) -> Posicion -> Posicion
minimax' prof' peor mejor posicion
    |replicate 7 ((maxBound::Int) `div` 2) == evaluaciones = menosMala siguientes 
    |elem (Just "Jugador2") (map hayGanador siguientes) = jugadaGanadora siguientes
    |otherwise = eleccion evaluacionesFinales
    where siguientes = [hacerMovimiento posicion movimiento | movimiento <- posiblesMovimientos posicion]
          evaluaciones = map (minimax prof' peor mejor) siguientes
          evaluacionesFinales = zip siguientes evaluaciones
          eleccion :: [(Posicion, Int)] -> Posicion
          eleccion (x:y:xs)
            |snd x < snd y = eleccion (x:xs)
            |snd x > snd y = eleccion (y:xs)
            |otherwise = eleccion (w:xs)
            where w = if length xs >= 3 then y else x
          eleccion [x] = fst x
          
menosMala :: [Posicion] -> Posicion
menosMala [x] = x
menosMala (x:xs)
    |(elem (Just "Jugador1") aux) || (elem (Just "Jugador2") aux) = menosMala xs
    |otherwise = x
    where siguientes = [hacerMovimiento x movimiento | movimiento <- posiblesMovimientos x]
          aux = map hayGanador siguientes

jugadaGanadora :: [Posicion] -> Posicion
jugadaGanadora [x] = x
jugadaGanadora (x:xs)
    |hayGanador x == Just "Jugador2" = x
    |otherwise = jugadaGanadora xs


play :: Posicion -> Int -> IO String
play (Posicion tablero Jugador1) dificultad = do putStr "¿En qué columna quieres jugar? "
                                                 jugada <- getLine
                                                 let newPosicion = hacerMovimiento (Posicion tablero Jugador1) (read jugada :: Int)
                                                 putStr $ show newPosicion
                                                 if isNothing $ hayGanador newPosicion then play newPosicion dificultad else return "YOU WIN"
                                     
play (Posicion tablero Jugador2) dificultad = do let newPosicion = minimax' dificultad minimum maximum (Posicion tablero Jugador2)
                                                 putStr $ show newPosicion
                                                 if isNothing $ hayGanador newPosicion then play newPosicion dificultad else return "TRY AGAIN"
                                                 
playDosJugadores :: Posicion -> IO String
playDosJugadores (Posicion tablero jugador) = do putStr "¿En qué columna quieres jugar? "
                                                 jugada <- getLine
                                                 let newPosicion = hacerMovimiento (Posicion tablero jugador) (read jugada :: Int)
                                                 putStr $ show newPosicion
                                                 if isNothing $ hayGanador newPosicion then playDosJugadores newPosicion else return "YOU WIN"
                                

conectaCuatro :: IO String
conectaCuatro = do putStrLn "¿Cuál es tu modalidad de juego?"
                   putStrLn "1. J1 vs. CPU"
                   putStrLn "2. J1 vs. J2"
                   opcion <- leeInt
                   case opcion of
                    1 -> do putStrLn "¿Qué nivel de dificultad quieres?"
                            putStrLn "1. Fácil"
                            putStrLn "2. Medio"
                            putStrLn "3. Difícil"
                            opcion' <- leeInt
                            putStr $ show tableroInicial
                            case opcion' of
                                1 -> do play tableroInicial 2
                                2 -> do play tableroInicial 3
                                3 -> do play tableroInicial 5
                    2 -> do putStr $ show tableroInicial
                            playDosJugadores tableroInicial        
                            
leeInt :: IO Int
leeInt = do linea <- getLine
            return (read linea)
                   


                           

          
