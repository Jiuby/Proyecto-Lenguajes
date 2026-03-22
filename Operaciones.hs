-- ================================================================
-- MÓDULO: Operaciones.hs
-- ================================================================
-- Contiene las 5 funciones principales del sistema:
--   1. registrarEntrada  => Check In
--   2. buscarEstudiante  => Search by ID
--   3. calcularTiempo    => Time Calculation
--   4. listarEstudiantes => List Students
--   5. registrarSalida   => Check Out
-- ================================================================

module Operaciones where

import Data.List  (find)    -- para buscar en listas
import Tipos                -- Estudiante, Registro
import Utilidades           -- horaAMinutos, minutosAHora, pedirHora, estaAdentro
import Archivos             -- cargarEstudiantes, guardarRegistros


-- ----------------------------------------------------------------
-- 1) REGISTRAR ENTRADA (Check In)
-- ----------------------------------------------------------------
-- Registra la entrada de un estudiante a la universidad.
--
-- Flujo:
--   - Pide el ID
--   - Si ya está adentro => avisa y no hace nada
--   - Si no está adentro => busca sus datos en University.txt
--                        => si no está en el archivo, los pide manualmente
--                        => crea un Registro con salida = Nothing
--                        => lo agrega a la lista y guarda en records.txt
--
-- Recibe: la lista actual de registros
-- Devuelve: la lista actualizada (con el nuevo registro)
registrarEntrada :: [Registro] -> IO [Registro]
registrarEntrada registros = do
    putStr "Ingresa el ID del estudiante: "
    sid <- getLine

    -- 'find' recorre la lista buscando el primero que cumpla la condición.
    -- Aquí buscamos un registro activo (mismo ID y todavía adentro).
    case find (\r -> regId r == sid && estaAdentro r) registros of

        Just _ -> do
            -- Ya existe un registro activo => no hacemos nada
            putStrLn "⚠ El estudiante ya está registrado adentro."
            return registros

        Nothing -> do
            -- No está adentro, procedemos a registrar su entrada
            estudiantes <- cargarEstudiantes "University.txt"

            -- Buscamos sus datos en University.txt para no tener que escribirlos
            (nombre, carrera) <- case find (\e -> estId e == sid) estudiantes of
                Just e  -> return (estNombre e, estCarrera e)
                Nothing -> do
                    -- No está en el archivo, pedimos los datos a mano
                    putStrLn "ID no encontrado en University.txt. Ingresa los datos:"
                    putStr "  Nombre: "  ; n <- getLine
                    putStr "  Carrera: " ; c <- getLine
                    return (n, c)

            hora <- pedirHora

            -- Creamos el nuevo Registro. La salida empieza en Nothing
            -- porque el estudiante acaba de entrar y no ha salido aún.
            let nuevo       = Registro sid nombre carrera hora Nothing
                actualizado = registros ++ [nuevo]  -- agregamos al final de la lista

            guardarRegistros "records.txt" actualizado
            putStrLn $ "✓ Entrada registrada: " ++ nombre ++ " a las " ++ hora
            return actualizado


-- ----------------------------------------------------------------
-- 2) BUSCAR ESTUDIANTE POR ID
-- ----------------------------------------------------------------
-- Muestra la información de un estudiante si está actualmente adentro.
-- No modifica la lista, solo consulta => no devuelve nada relevante.
buscarEstudiante :: [Registro] -> IO ()
buscarEstudiante registros = do
    putStr "Ingresa el ID a buscar: "
    sid <- getLine

    case find (\r -> regId r == sid && estaAdentro r) registros of
        Nothing ->
            putStrLn "✗ Estudiante no encontrado o ya registró salida."
        Just r -> do
            putStrLn "\n--- Estudiante encontrado ---"
            putStrLn $ "  ID     : " ++ regId r
            putStrLn $ "  Nombre : " ++ regNombre r
            putStrLn $ "  Carrera: " ++ regCarrera r
            putStrLn $ "  Entrada: " ++ regEntrada r


-- ----------------------------------------------------------------
-- 3) CALCULAR TIEMPO DE PERMANENCIA
-- ----------------------------------------------------------------
-- Pide dos horas al usuario y calcula la diferencia.
-- Es independiente: no necesita la lista de registros.
--
-- Usa horaAMinutos para convertir a números y poder restar.
-- Usa minutosAHora para mostrar el resultado en formato legible.
calcularTiempo :: IO ()
calcularTiempo = do
    putStr "Hora de entrada (HH:MM): "
    entrada <- getLine
    putStr "Hora de salida  (HH:MM): "
    salida  <- getLine

    let diff = horaAMinutos salida - horaAMinutos entrada

    if diff < 0
        then putStrLn "✗ La hora de salida es anterior a la de entrada."
        else putStrLn $ "✓ Tiempo en la universidad: "
                     ++ minutosAHora diff
                     ++ "  (" ++ show diff ++ " minutos)"


-- ----------------------------------------------------------------
-- 4) LISTAR ESTUDIANTES DEL ARCHIVO
-- ----------------------------------------------------------------
-- Carga University.txt y muestra todos los estudiantes registrados.
-- No usa la lista en memoria, lee directo del archivo.
listarEstudiantes :: IO ()
listarEstudiantes = do
    estudiantes <- cargarEstudiantes "University.txt"
    if null estudiantes
        then putStrLn "✗ No hay estudiantes en University.txt."
        else do
            putStrLn "\n--- Lista de estudiantes (University.txt) ---"
            -- 'mapM_' es como 'map' pero para acciones IO.
            -- Ejecuta 'mostrarEstudiante' para cada elemento de la lista.
            mapM_ mostrarEstudiante estudiantes
            putStrLn $ "Total: " ++ show (length estudiantes) ++ " estudiante(s)."
  where
    mostrarEstudiante e =
        putStrLn $ "  " ++ estId e ++ " | " ++ estNombre e ++ " | " ++ estCarrera e


-- ----------------------------------------------------------------
-- 5) REGISTRAR SALIDA (Check Out)
-- ----------------------------------------------------------------
-- Registra la salida de un estudiante y calcula cuánto tiempo estuvo.
--
-- Flujo:
--   - Pide el ID
--   - Si no está adentro => avisa y no hace nada
--   - Si está adentro    => pide la hora de salida
--                        => actualiza su registro con esa hora
--                        => guarda en records.txt
--                        => muestra cuánto tiempo estuvo
--
-- Recibe: la lista actual de registros
-- Devuelve: la lista actualizada (con la salida marcada)
registrarSalida :: [Registro] -> IO [Registro]
registrarSalida registros = do
    putStr "Ingresa el ID del estudiante: "
    sid <- getLine

    case find (\r -> regId r == sid && estaAdentro r) registros of
        Nothing -> do
            putStrLn "✗ Estudiante no encontrado o ya registró salida."
            return registros

        Just r -> do
            hora <- pedirHora

            -- 'map marcarSalida' recorre TODOS los registros.
            -- Solo modifica el que tenga el mismo ID y esté adentro.
            -- Los demás los deja exactamente igual.
            let actualizado = map (marcarSalida sid hora) registros

            guardarRegistros "records.txt" actualizado

            -- Calculamos el tiempo restando minutos
            let diff = horaAMinutos hora - horaAMinutos (regEntrada r)
            putStrLn $ "✓ Salida registrada a las " ++ hora
            putStrLn $ "  Tiempo en la universidad: "
                    ++ minutosAHora diff
                    ++ "  (" ++ show diff ++ " minutos)"
            return actualizado


-- ----------------------------------------------------------------
-- marcarSalida (función auxiliar de registrarSalida)
-- ----------------------------------------------------------------
-- Recibe un ID, una hora y un Registro.
-- Si el Registro es del estudiante buscado Y sigue adentro,
-- le actualiza la hora de salida.
-- En cualquier otro caso, devuelve el Registro sin cambios.
--
-- Nota: 'reg { regSalida = Just hora }' es la sintaxis de Haskell
-- para "crear una copia de reg con el campo regSalida cambiado".
marcarSalida :: String -> String -> Registro -> Registro
marcarSalida sid hora reg
    | regId reg == sid && estaAdentro reg = reg { regSalida = Just hora }
    | otherwise                           = reg
