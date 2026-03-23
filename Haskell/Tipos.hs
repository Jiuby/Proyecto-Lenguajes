-- ================================================================
-- MODULO: Tipos.hs
-- ================================================================
-- Define los tipos de datos que usa TODO el programa.
-- Este es el modulo mas basico, todos los demas lo importan.
-- ================================================================

module Tipos where


-- 'Estudiante' representa la informacion de un estudiante
-- tal como viene guardada en University.txt
--
-- Cada campo es un String:
--   estId     => numero de identificacion  (ej: "1001")
--   estNombre => nombre completo           (ej: "Ana Torres")
--   estCarrera => programa academico       (ej: "Ingenieria de Sistemas")
data Estudiante = Estudiante
    { estId      :: String
    , estNombre  :: String
    , estCarrera :: String
    }


-- 'Registro' representa a un estudiante que entro a la universidad.
-- Se guarda en memoria mientras el programa corre,
-- y tambien se persiste en records.txt.
--
-- El campo 'regSalida' usa Maybe:
--   Nothing       => el estudiante AUN esta adentro
--   Just "14:30"  => el estudiante salio a las 14:30
--
-- Maybe es util porque la salida puede o no existir todavia.
data Registro = Registro
    { regId      :: String        -- ID del estudiante
    , regNombre  :: String        -- Nombre completo
    , regCarrera :: String        -- Carrera
    , regEntrada :: String        -- Hora de entrada en formato "HH:MM"
    , regSalida  :: Maybe String  -- Hora de salida (Nothing = sigue adentro)
    }
