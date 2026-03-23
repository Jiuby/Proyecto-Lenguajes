-- ================================================================
-- MODULO: Main.hs
-- ================================================================
-- Punto de entrada del programa.
-- Solo se encarga del menu y del bucle principal.
-- Toda la logica esta en los otros modulos.
--
-- Estructura del proyecto:
--   Tipos.hs        => tipos de datos (Estudiante, Registro)
--   Utilidades.hs   => funciones pequenas reutilizables
--   Archivos.hs     => leer y escribir archivos
--   Operaciones.hs  => las 5 funciones del sistema
--   Main.hs         => menu y punto de entrada  (este archivo)
-- ================================================================

module Main where

import System.IO (hSetBuffering, stdout, BufferMode(..))
import Tipos                          -- Registro
import Archivos      (cargarRegistrosSeguros)
import Operaciones   -- registrarEntrada, buscarEstudiante, etc.


-- ----------------------------------------------------------------
-- mostrarMenu
-- ----------------------------------------------------------------
-- Imprime en pantalla las opciones disponibles.
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n============================="
    putStrLn "  SISTEMA DE REGISTRO - EAFIT"
    putStrLn "============================="
    putStrLn "1. Registrar entrada"
    putStrLn "2. Buscar estudiante por ID"
    putStrLn "3. Calcular tiempo de permanencia"
    putStrLn "4. Listar estudiantes del archivo"
    putStrLn "5. Registrar salida"
    putStrLn "6. Salir"
    putStr   "Elige una opcion: "


-- ----------------------------------------------------------------
-- bucle
-- ----------------------------------------------------------------
-- Bucle principal del programa. Usa RECURSION en lugar de while/for.
--
-- En Haskell no existen variables mutables ni ciclos tradicionales.
-- En su lugar, 'bucle' se llama a si misma pasando la lista
-- actualizada como argumento. El estado del programa vive
-- en ese argumento que va pasando de llamada en llamada.
--
-- Ejemplo de flujo:
--   bucle []                    -- inicia con lista vacia
--     => usuario elige "1"
--     => registrarEntrada [] devuelve [registro1]
--   bucle [registro1]           -- nueva llamada con lista actualizada
--     => usuario elige "1"
--     => registrarEntrada [registro1] devuelve [registro1, registro2]
--   bucle [registro1, registro2]
--     => usuario elige "6"      -- fin, no hay llamada recursiva
--
-- La recursion termina cuando el usuario elige "6".
bucle :: [Registro] -> IO ()
bucle registros = do
    mostrarMenu
    opcion <- getLine
    case opcion of

        "1" -> do
            -- registrarEntrada devuelve la lista actualizada
            -- se la pasamos a la siguiente llamada recursiva
            nuevos <- registrarEntrada registros
            bucle nuevos

        "2" -> do
            -- buscarEstudiante solo consulta, no modifica la lista
            buscarEstudiante registros
            bucle registros   -- pasamos la misma lista sin cambios

        "3" -> do
            -- calcularTiempo no usa la lista de registros
            calcularTiempo
            bucle registros

        "4" -> do
            -- listarEstudiantes lee University.txt directamente
            listarEstudiantes
            bucle registros

        "5" -> do
            -- registrarSalida devuelve la lista con la salida marcada
            nuevos <- registrarSalida registros
            bucle nuevos

        "6" ->
            -- No hacemos llamada recursiva => el programa termina
            putStrLn "Hasta luego!"

        _ -> do
            -- Cualquier otra entrada es invalida
            putStrLn "[X] Opcion invalida. Intenta de nuevo."
            bucle registros   -- volvemos a mostrar el menu


-- ----------------------------------------------------------------
-- main
-- ----------------------------------------------------------------
-- Primera funcion que ejecuta Haskell al correr el programa.
-- Carga los registros existentes y arranca el bucle.
main :: IO ()
main = do
    -- Sin esto, el texto puede no aparecer inmediatamente en pantalla
    hSetBuffering stdout NoBuffering

    putStrLn "Cargando registros..."

    -- Intentamos cargar records.txt. Si no existe, empezamos con []
    registros <- cargarRegistrosSeguros "records.txt"

    putStrLn $ "Sistema listo. " ++ show (length registros) ++ " registro(s) cargado(s)."

    -- Iniciamos el bucle con los registros que habia guardados
    bucle registros
