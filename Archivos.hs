-- ================================================================
-- MÓDULO: Archivos.hs
-- ================================================================
-- Todo lo relacionado con leer y escribir archivos:
--   - Cargar University.txt  => lista de Estudiantes
--   - Cargar records.txt     => lista de Registros
--   - Guardar records.txt    => persiste los cambios
-- ================================================================

module Archivos where

import Data.List   (intercalate)      -- para unir campos con comas
import System.IO.Error (catchIOError) -- para no crashear si el archivo no existe
import Tipos                          -- Estudiante, Registro
import Utilidades  (splitOn)          -- para separar las líneas por ','


-- ----------------------------------------------------------------
-- cargarEstudiantes
-- ----------------------------------------------------------------
-- Lee University.txt y devuelve una lista de Estudiante.
--
-- Formato esperado de cada línea:
--   id,nombre,carrera
-- Ejemplo:
--   1001,Ana Torres,Ingeniería de Sistemas
--
-- Pasos:
--   1. readFile lee todo el archivo como un String
--   2. lines   divide el String en una lista de líneas
--   3. filter  elimina las líneas vacías
--   4. map     convierte cada línea en un Estudiante
cargarEstudiantes :: FilePath -> IO [Estudiante]
cargarEstudiantes archivo = do
    contenido <- readFile archivo
    let lineas = filter (not . null) (lines contenido)
    return (map parsearEstudiante lineas)

-- Convierte una línea de texto en un Estudiante.
-- Usa pattern matching sobre la lista de partes separadas por ','.
parsearEstudiante :: String -> Estudiante
parsearEstudiante linea = case splitOn ',' linea of
    (i:n:c:_) -> Estudiante i n c             -- línea completa
    (i:n:_)   -> Estudiante i n "Desconocida" -- falta la carrera
    (i:_)     -> Estudiante i "?" "?"         -- solo tiene ID
    []        -> Estudiante "?" "?" "?"       -- línea vacía


-- ----------------------------------------------------------------
-- cargarRegistros
-- ----------------------------------------------------------------
-- Lee records.txt y devuelve una lista de Registro.
--
-- Formato esperado de cada línea:
--   id,nombre,carrera,entrada,salida
-- Ejemplo con salida:    1001,Ana Torres,Sistemas,08:00,10:30
-- Ejemplo sin salida:    1002,Luis Gomez,Sistemas,09:15,
--                                                       ^ vacío = sigue adentro
cargarRegistros :: FilePath -> IO [Registro]
cargarRegistros archivo = do
    contenido <- readFile archivo
    let lineas = filter (not . null) (lines contenido)
    return (map parsearRegistro lineas)

-- Convierte una línea de texto en un Registro.
-- Si el campo de salida está vacío (""), se guarda como Nothing.
-- Si tiene un valor, se guarda como Just "HH:MM".
parsearRegistro :: String -> Registro
parsearRegistro linea = case splitOn ',' linea of
    (i:n:c:en:sal:_) ->
        Registro i n c en (if null sal then Nothing else Just sal)
    (i:n:c:en:_) ->
        Registro i n c en Nothing
    _ ->
        Registro "?" "?" "?" "00:00" Nothing


-- ----------------------------------------------------------------
-- guardarRegistros
-- ----------------------------------------------------------------
-- Escribe la lista de Registros en records.txt.
-- Sobreescribe el archivo completo (no agrega al final).
--
-- Pasos:
--   1. Convierte cada Registro en una línea de texto
--   2. 'unlines' une la lista con saltos de línea al final
--   3. writeFile sobreescribe el archivo con ese texto
guardarRegistros :: FilePath -> [Registro] -> IO ()
guardarRegistros archivo registros =
    writeFile archivo (unlines (map registroALinea registros))

-- Convierte un Registro en una línea de texto para guardar.
-- 'intercalate "," lista' une los elementos con comas.
-- Ejemplo: ["1001","Ana","Sistemas","08:00",""]  =>  "1001,Ana,Sistemas,08:00,"
registroALinea :: Registro -> String
registroALinea reg = intercalate ","
    [ regId      reg
    , regNombre  reg
    , regCarrera reg
    , regEntrada reg
    , case regSalida reg of
        Nothing -> ""    -- sin salida => campo vacío
        Just t  -> t     -- con salida => la hora
    ]


-- ----------------------------------------------------------------
-- cargarRegistrosSeguros
-- ----------------------------------------------------------------
-- Igual que cargarRegistros, pero si el archivo NO existe
-- simplemente devuelve una lista vacía en vez de crashear.
--
-- 'catchIOError accion manejador' ejecuta 'accion' y si falla
-- ejecuta 'manejador' con el error. Aquí ignoramos el error (\_ )
-- y devolvemos [].
cargarRegistrosSeguros :: FilePath -> IO [Registro]
cargarRegistrosSeguros archivo =
    catchIOError (cargarRegistros archivo) (\_ -> return [])
