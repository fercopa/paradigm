{-# LANGUAGE DoAndIfThenElse #-}

module SpellChecker (do_spellcheck) where

import CommandLine
import Dictionary
import Document
import Data.Char
import System.IO.Error
import Control.Exception               -- Aca habria que descomentarlo si es necesario...

type Word = String

-- La funcion 'do_spellcheck' es la funcion que se encarga de manejar
-- el proceso de chequeo ortografico. Esto incluye, cargar el diccionario,
-- abrir el archivo a procesar, procesar el archivo y luego guardar el
-- diccionario y el archivo de entrada ya procesado.
-- Toma como argumento los argumentos de linea de comando de tipo 'Params'.
do_spellcheck :: Params -> IO ()
do_spellcheck (Params filenameIn dictionaryMain) =
    do
        dict <- dict_load dictionaryMain
        if (dict_contains "-1" dict)    -- Verifica que el diccionario sea valido.
            then do putStrLn "ERROR: Diccionario no valido."
                    return ()
            else do doc <- doc_open filenameIn "out.txt"
                    dictToSave <- (process_document doc dict dict_new)
                    if (dict_contains "-1" dictToSave)    -- Verifica que las palabras del documento sean validas.
                        then do putStrLn "ERROR: Documento no valido."
                                return ()
                        else do dict_save dictionaryMain dictToSave
                                doc_close doc
                                putStr "El documento "
                                putStr filenameIn
                                putStr " ha sido procesado. Resultados en out.txt\n"
                                return()

-- La funcion 'process_document' ejecuta el proceso de chequeo ortografico.
-- Para ello, procesa el archivo palabra por palabra, copiandolas al archivo
-- de salida y consultando al usuario sobre que accion realizar ante una
-- palabra desconocida.
-- Cuando se termina de procesar el archivo, lo cual es señalizado mediante
-- una excepcion por 'doc_get_word', retorna el diccionario (el cual puede
-- haber sido modificado) para guardarlo.
process_document :: Document -> Dictionary -> Dictionary -> IO Dictionary
process_document document dictMain dictIgnored =
        do
            currentWord <- (doc_get_word document)
            if ((length currentWord) <= 30) then    -- Verifica que la palabra sea valida.
                if ((dict_contains currentWord dictMain) || (dict_contains currentWord dictIgnored))    -- Si se encuentra en los diccionarios.
                    then do doc_put_word currentWord document    -- Agrega la palabra al documento de salida.
                            dictMain' <- (process_document document dictMain dictIgnored)    -- Sigue procesando el documento.
                            return(dictMain')
                    else do (word, dictMainM, dictIgnoredM) <- (consult_user currentWord dictMain dictIgnored)    -- Tratamiento de palabra desconocida.
                            doc_put_word word document    -- Agrega la palabra al documento de salida.
                            dictMain' <- (process_document document dictMainM dictIgnoredM)    -- Sigue procesando el documento.
                            return(dictMain')
            else return(dict_add "-1" dict_new)    -- Palabra no valida.
        `catch`    -- excepcion por 'doc_get_word' -> EOF del documento.
        \e -> if isEOFError e then return(dictMain)
                else ioError e

-- Verifica si una palabra es conocida, en cuyo caso, continua
-- con el procesamiento del archivo, sin realizar ninguna accion.
-- Si la palabra no es conocida, pregunta al usuario que accion
-- realizar con la misma. Las acciones pueden ser aceptar, ignorar
-- o reemplazar.
consult_user ::  Word -> Dictionary -> Dictionary -> IO (Word, Dictionary, Dictionary)
consult_user wordUnknown dicMain dicIgnored =
    let
        alpha_char :: Word -> Bool    -- La palabra posse caracteres especiales.
        alpha_char [] = True
        alpha_char (x:xs) |(isAlpha x) == False = False
                          |otherwise = alpha_char xs

        max_word :: Word -> Bool    -- La palabra no cumple con el maximo permitido.
        max_word (xs) |(length xs) > 30 = False
                      |otherwise  = True

        replace :: IO Word    -- Reemplaza una palabra.
        replace = do
            putStrLn "Ingrese una nueva palabra: "
            newWord <- getLine
            if ((alpha_char newWord) && (max_word newWord))
                then return(newWord)
                else do putStrLn "ERROR: Palabra no valida."
                        replace

        ask :: Word -> IO Word    -- Pregunta ante una palabra desconocida.
        ask word = do
            putStr "Palabra no reconocida: "
            putStr word
            putChar '\n'
            putStrLn "Aceptar (a) - Ignorar (i) - Reemplazar (r): "
            c <- getLine
            case c of
                "a" -> return(c)
                "i" -> return(c)
                "r" -> return(c)
                _   -> ask word    -- Opcion incorrecta.

    in
        do
            option <- ask wordUnknown
            case option of
                "a" -> return(wordUnknown, dict_add wordUnknown dicMain, dicIgnored)    -- Agrega al diccionario principal.
                "i" -> return(wordUnknown, dicMain, dict_add wordUnknown dicIgnored)    -- Agrega al diccionario de palabras ignoradas.
                "r" -> do newWord <- replace    -- Reemplaza la palabra.
                          return(newWord, dicMain, dicIgnored)
                _   -> consult_user wordUnknown dicMain dicIgnored    -- Opcion incorrecta.
