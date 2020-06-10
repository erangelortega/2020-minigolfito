module Lib where
import Text.Show.Functions
import Data.Function

laVerdad = True

{-De los participantes nos interesará el nombre del jugador, el de su padre y sus habilidades (fuerza y precisión). -}

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)


type Puntos = Int

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)


-- PUNTO 1 -------------------------
{-Modelar los palos usados en el juego que a partir de una determinada habilidad 
generan un tiro que se compone por velocidad, precisión y altura.-}

-- **************************--
type Palo = Habilidad -> Tiro
-- **************************--

-- El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
putter :: Palo
putter habilidad = UnTiro { velocidad = 10, precision = (*2).precisionJugador $ habilidad, altura = 0 }

{-La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.-}
madera :: Palo
madera habilidad = UnTiro { velocidad = 100, precision = (`div` 2).precisionJugador $ habilidad, altura = 5 }

{-Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la 
fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.
-}

hierro :: Int -> Palo
hierro n habilidad = UnTiro {
  velocidad = (* n).fuerzaJugador $ habilidad, 
  precision = precisionJugador  habilidad `div` n, 
  altura = max (n - 3) 0 
  }


{-Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego. -}
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1 .. 10] 



-- PUNTO 2
{-Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.-}

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador


--PUNTO 3 -----

between :: (Eq a, Ord a) => a -> a -> a -> Bool
between menor mayor nro = menor <= nro && nro <= mayor

detenerTiro :: Tiro
detenerTiro = UnTiro 0 0 0

vaAlRasDelSuelo :: Tiro -> Bool
vaAlRasDelSuelo = (== 0). altura

-- **************************--
--    VERSION 1 --
-- **************************--
data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,   {- estoy pasando funciones-}
  efectoLuegoDeSuperar :: Tiro -> Tiro
}     {-no pongo deriving porque no voy hacer operaciones con las funciones-}

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal
  | (puedeSuperar obstaculo) tiroOriginal = (efectoLuegoDeSuperar obstaculo) tiroOriginal
  | otherwise = detenerTiro

{-Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. 
Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0. -}

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiroOriginal = tiroOriginal {
  velocidad = velocidad tiroOriginal * 2,
  precision = 100,
  altura = 0
  }


{-Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna 
el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.-}

laguna :: Int -> Obstaculo
laguna largoLaguna = UnObstaculo superaLaguna (efectoLaguna largoLaguna)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura ) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoLaguna tiroOriginal = tiroOriginal { altura = altura tiroOriginal `div` largoLaguna }

{-Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95.
 Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.-}

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = between 5 20 (velocidad tiro) && vaAlRasDelSuelo tiro && precision tiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = detenerTiro

-- **************************--
--    VERSION 2 --
-- **************************--
type ObstaculoV2 = Tiro -> Tiro

obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> ObstaculoV2
obstaculoSuperableSi condicion efecto tiroOriginal 
  | condicion tiroOriginal = efecto tiroOriginal
  | otherwise = detenerTiro


tunelConRampita2 :: ObstaculoV2 
tunelConRampita2 = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita 

laguna2 :: Int -> ObstaculoV2 
laguna2 largoLaguna = obstaculoSuperableSi superaLaguna (efectoLaguna largoLaguna)

hoyo2 :: ObstaculoV2 
hoyo2 = obstaculoSuperableSi superaHoyo efectoHoyo



-- PUNTO 4 ----------

-- **************************--
--    VERSION 1 --
-- **************************--
{-a) Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo-}

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (paloSirve obstaculo jugador) palos

paloSirve :: Obstaculo -> Jugador -> Palo -> Bool
paloSirve obstaculo jugador = puedeSuperar obstaculo . golpe jugador


-- **************************--
--    VERSION 2 -- crearon el DATA como en la version 1 y terminaron igual
-- **************************--

{-b) Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.-}

cantObstaculosConsecutivosSuperados :: Tiro -> [Obstaculo] -> Int {-con recursividad-}
cantObstaculosConsecutivosSuperados tiro [] = 0
cantObstaculosConsecutivosSuperados tiro (obstaculo : obstaculos)
  | puedeSuperar obstaculo tiro = 1 + cantObstaculosConsecutivosSuperados (efectoLuegoDeSuperar obstaculo tiro) obstaculos
  | otherwise = 0

-- **************************--
--    Bonus
-- **************************--
cantObstaculosConsecutivosSuperados' :: Tiro -> [Obstaculo] -> Int {-sin recursividad-}
cantObstaculosConsecutivosSuperados' tiro obstaculos 
  = (length . takeWhile (\(obstaculo , tiroQueLlega) -> puedeSuperar obstaculo tiroQueLlega) . zip obstaculos . tirosSucesivos tiro) obstaculos 

tirosSucesivos :: Tiro -> [Obstaculo] -> [Tiro]
tirosSucesivos tiroOriginal obstaculos 
  = foldl (\tirosGenerados obstaculo -> tirosGenerados ++ [efectoLuegoDeSuperar obstaculo (last tirosGenerados)]) [tiroOriginal] obstaculos
  
{-Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.-}
maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos
  = maximoSegun (flip cantObstaculosConsecutivosSuperados obstaculos . golpe jugador) palos

-- PUNTO --
{-Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó el torneo 
si tiene más puntos que los otros niños.-}

jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDelTorneo 
  =  map (padre.jugadorDeTorneo) . filter ( not.gano puntosDelTorneo) $ puntosDelTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDelTorneo puntosDelJugador 
  = all ((< puntosGanados puntosDelJugador) .puntosGanados) . filter (/= puntosDelJugador) $ puntosDelTorneo