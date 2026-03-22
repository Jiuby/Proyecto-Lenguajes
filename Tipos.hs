-- ================================================================
-- MÓDULO: Tipos.hs
-- ================================================================
-- Define los tipos de datos que usa TODO el programa.
-- Este es el módulo más básico, todos los demás lo importan.
-- ================================================================

module Tipos where


-- 'Estudiante' representa la información de un estudiante
-- tal como viene guardada en University.txt
--
-- Cada campo es un String:
--   estId     => número de identificación  (ej: "1001")
--   estNombre => nombre completo           (ej: "Ana Torres")
--   estCarrera => programa académico       (ej: "Ingeniería de Sistemas")
data Estudiante = Estudiante
    { estId      :: String
    , estNombre  :: String
    , estCarrera :: String
    }


-- 'Registro' representa a un estudiante que entró a la universidad.
-- Se guarda en memoria mientras el programa corre,
-- y también se persiste en records.txt.
--
-- El campo 'regSalida' usa Maybe:
--   Nothing       => el estudiante AÚN está adentro
--   Just "14:30"  => el estudiante salió a las 14:30
--
-- Maybe es útil porque la salida puede o no existir todavía.
data Registro = Registro
    { regId      :: String        -- ID del estudiante
    , regNombre  :: String        -- Nombre completo
    , regCarrera :: String        -- Carrera
    , regEntrada :: String        -- Hora de entrada en formato "HH:MM"
    , regSalida  :: Maybe String  -- Hora de salida (Nothing = sigue adentro)
    }
