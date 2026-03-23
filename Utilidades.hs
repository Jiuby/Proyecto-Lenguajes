-- ================================================================
-- MODULO: Utilidades.hs
-- ================================================================
-- Funciones auxiliares pequenas que se reutilizan en varios modulos:
--   - Separar texto por un caracter
--   - Convertir horas a minutos y viceversa
--   - Pedir la hora al usuario
--   - Saber si un estudiante sigue adentro
-- ================================================================

module Utilidades where

import Tipos   -- necesitamos el tipo Registro para 'estaAdentro'


-- ----------------------------------------------------------------
-- splitOn
-- ----------------------------------------------------------------
-- Divide un String en partes usando un caracter separador.
--
-- Ejemplo:
--   splitOn ',' "1001,Ana,Sistemas"  =>  ["1001", "Ana", "Sistemas"]
--   splitOn ',' "hola"               =>  ["hola"]
--   splitOn ',' ""                   =>  [""]
--
-- Funciona con 'foldr': recorre el texto de derecha a izquierda.
-- Cada vez que encuentra el separador, abre una nueva parte vacia.
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]   -- caso base: texto vacio => una sola parte vacia
splitOn sep texto = foldr paso [""] texto
  where
    -- 'paso' se ejecuta para cada caracter del texto
    paso caracter (parteActual:resto)
        | caracter == sep = "" : parteActual : resto  -- nuevo segmento
        | otherwise       = (caracter : parteActual) : resto
    paso _ [] = []


-- ----------------------------------------------------------------
-- horaAMinutos
-- ----------------------------------------------------------------
-- Convierte una hora en formato "HH:MM" a minutos desde las 00:00.
-- Esto hace que restar horas sea tan simple como restar dos numeros.
--
-- Ejemplo:
--   horaAMinutos "02:30"  =>  150   (2*60 + 30)
--   horaAMinutos "08:00"  =>  480   (8*60 + 0)
horaAMinutos :: String -> Int
horaAMinutos hora =
    let (h, resto) = break (== ':') hora  -- separa "HH" y ":MM"
        m          = drop 1 resto          -- elimina el ':' para quedarse con "MM"
    in  read h * 60 + read m


-- ----------------------------------------------------------------
-- minutosAHora
-- ----------------------------------------------------------------
-- Convierte minutos a formato "HH:MM".
-- Es la operacion inversa de horaAMinutos.
--
-- Ejemplo:
--   minutosAHora 150  =>  "02:30"
--   minutosAHora 480  =>  "08:00"
minutosAHora :: Int -> String
minutosAHora total =
    let horas   = total `div` 60   -- division entera
        minutos = total `mod` 60   -- residuo
    in  rellenar horas ++ ":" ++ rellenar minutos
  where
    -- Agrega un '0' adelante si el numero es de un solo digito
    -- Ejemplo: 5 => "05",  12 => "12"
    rellenar n = if n < 10 then "0" ++ show n else show n


-- ----------------------------------------------------------------
-- pedirHora
-- ----------------------------------------------------------------
-- Accion IO que le pide al usuario que escriba la hora actual.
-- Devuelve el String que el usuario ingreso.
pedirHora :: IO String
pedirHora = do
    putStr "Ingresa la hora actual (HH:MM): "
    getLine


-- ----------------------------------------------------------------
-- estaAdentro
-- ----------------------------------------------------------------
-- Dice si un Registro corresponde a un estudiante que
-- todavia no ha registrado salida.
--
-- Usa pattern matching sobre Maybe:
--   Nothing => no tiene hora de salida => sigue adentro => True
--   Just _  => ya tiene hora de salida => ya se fue    => False
estaAdentro :: Registro -> Bool
estaAdentro reg = case regSalida reg of
    Nothing -> True
    Just _  -> False
