module Lib where
import Text.Show.Functions

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


-- PUNTO 1 -------------------------
{-Modelar los palos usados en el juego que a partir de una determinada habilidad 
generan un tiro que se compone por velocidad, precisión y altura.-}

type Puntos = Int

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Palo = Habilidad -> Tiro

configurarTiro ::  Int -> Int -> Int -> Tiro 
configurarTiro velocidad precision altura = UnTiro { velocidad = velocidad, precision = precision, altura = altura }

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
hierro n habilidad = UnTiro { velocidad = (* n).fuerzaJugador $ habilidad, precision = (`div` n).precisionJugador $ habilidad, altura = alturaHierro n }

alturaHierro :: Int -> Int
alturaHierro n = max 0  (n-3)


-- PUNTO 2
palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3 ,hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

golpe :: Palo -> Jugador -> Tiro
golpe palo = palo.habilidad


--PUNTO 3 -----
{-Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. 
Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0. -}

type Obstaculo = Tiro -> Tiro

data TipoObstaculo = TunelConRampita | Laguna {largoLaguna ::Int}| Hoyo deriving (Show, Eq)

tunelConRampita :: Obstaculo
tunelConRampita (UnTiro veloc prec alt) 
  | prec > 90 && alt == 0 = UnTiro {velocidad = veloc * 2, precision = 100, altura = 0 }
  | otherwise = detener

{-Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna 
el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.-}

laguna :: Int -> Obstaculo
laguna largoLaguna (UnTiro veloc prec alt)
  | veloc > 80 && (alt >= 1 && alt <= 5) = UnTiro {velocidad = veloc, precision = prec, altura = div alt largoLaguna }
  | otherwise = detener

{-Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95.
 Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.-}

hoyo :: Obstaculo
hoyo (UnTiro veloc prec alt)
  | (veloc >= 5 && veloc <= 20) && prec > 95 && alt == 0 = detener 
  | otherwise = detener

detener :: Tiro
detener = UnTiro 0 0 0

obstaculoSuperado :: TipoObstaculo -> Tiro -> Tiro
obstaculoSuperado TunelConRampita tiro = tunelConRampita tiro
obstaculoSuperado (Laguna largoLaguna) tiro = laguna largoLaguna tiro
obstaculoSuperado Hoyo tiro = hoyo tiro

-- PUNTO 4 ----------
{-Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo-}

palosUtiles :: Persona -> 



{-type Obstaculo = Type -> Bool
efectoObstaculo :: Obstaculo -> Tiro -> Tiro
efectoObstaculo obstaculoTunel tiro  = cnjwcenwkec
efectoObstaculo (obstaculoLaguna long) = 

alRazDelSuelo :: Tiro -> Bool
alRazDelSuelo = (==0).altura 

gra

tunel :: 
tunel =

laguna :: Tiro -> Obstaculo
laguna =

hoyo :: Obstaculo
hoyo =

comoQueda laguna tiro | f tiro =
                      |
comoQueda tunel


type Obstaculo = Tiro -> Bool

-- Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
-- independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, 
-- la precisión pasa a ser 100 y la altura 0.

rampita :: Obstaculo
rampita tiro = (>90)(precision tiro) && (==0) (altura tiro)

-- Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
-- Luego de superar una laguna el tiro llega con la misma velocidad y precisión, 
-- pero una altura equivalente a la altura original dividida por el largo de la laguna.

laguna :: Int -> Obstaculo 
laguna altLaguna tiro= (>80)(velocidad tiro) && (>1)(altura tiro) && (<5)(altura tiro)

-- Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
-- Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

hoyo :: Obstaculo
hoyo tiro = between 5 20 (velocidad tiro) && (==0)(altura tiro) && (>95)(precision tiro)-}

