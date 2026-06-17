module Main where

import Control.Exception (IOException, catch)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.IO (IOMode(..), withFile, hPutStr, hPutStrLn)

--------------------------------------------------------------------------------
-- Tipos de domínio
--------------------------------------------------------------------------------

data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog
  = Add
  | Remove
  | Update
  | ListItems
  | Report
  | Query
  | QueryFail
  deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read, Eq)

type ResultadoOperacao = (Inventario, LogEntry)

data AppState = AppState
  { estadoInventario :: Inventario
  , estadoLogs       :: [LogEntry]
  } deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Arquivos
--------------------------------------------------------------------------------

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

logFile :: FilePath
logFile = "Auditoria.log"

--------------------------------------------------------------------------------
-- Funções auxiliares puras
--------------------------------------------------------------------------------

mkItem :: String -> String -> Int -> String -> Item
mkItem = Item

itemToDetails :: Item -> String
itemToDetails i = intercalate ";"
  [ "id=" ++ itemID i
  , "nome=" ++ nome i
  , "qtd=" ++ show (quantidade i)
  , "cat=" ++ categoria i
  ]

removeDetails :: String -> Int -> String
removeDetails ident qtd = intercalate ";"
  [ "id=" ++ ident
  , "qtd=" ++ show qtd
  ]

queryDetails :: String -> String
queryDetails ident = "id=" ++ ident

mkSuccessLog :: UTCTime -> AcaoLog -> String -> LogEntry
mkSuccessLog ts a det = LogEntry ts a det Sucesso

mkFailureLog :: UTCTime -> AcaoLog -> String -> String -> LogEntry
mkFailureLog ts a det msg = LogEntry ts a det (Falha msg)

normalize :: String -> String
normalize = map toLower

safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s of
  [(n, rest)] | all isSpace rest -> Just n
  _ -> Nothing

splitOnSemicolon :: String -> [String]
splitOnSemicolon [] = []
splitOnSemicolon s = case break (== ';') s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitOnSemicolon rest

fieldValue :: String -> String -> Maybe String
fieldValue key details = go (splitOnSemicolon details)
  where
    prefix = key ++ "="
    go [] = Nothing
    go (x:xs)
      | take (length prefix) x == prefix = Just (drop (length prefix) x)
      | otherwise = go xs

parseItemFromDetails :: String -> Maybe Item
parseItemFromDetails det = do
  ident <- fieldValue "id" det
  n     <- fieldValue "nome" det
  qStr  <- fieldValue "qtd" det
  cat   <- fieldValue "cat" det
  q     <- safeReadInt qStr
  pure (Item ident n q cat)

parseIdAndQty :: String -> Maybe (String, Int)
parseIdAndQty det = do
  ident <- fieldValue "id" det
  qStr  <- fieldValue "qtd" det
  q     <- safeReadInt qStr
  pure (ident, q)

applySuccessLog :: Inventario -> LogEntry -> Inventario
applySuccessLog inv (LogEntry _ Add det Sucesso) =
  case parseItemFromDetails det of
    Just item -> M.insert (itemID item) item inv
    Nothing   -> inv
applySuccessLog inv (LogEntry _ Update det Sucesso) =
  case parseItemFromDetails det of
    Just item -> M.insert (itemID item) item inv
    Nothing   -> inv
applySuccessLog inv (LogEntry _ Remove det Sucesso) =
  case parseIdAndQty det of
    Just (ident, qtd) ->
      case M.lookup ident inv of
        Just item ->
          let novoQtd = quantidade item - qtd
          in if novoQtd > 0
                then M.insert ident item { quantidade = novoQtd } inv
                else M.delete ident inv
        Nothing -> inv
    Nothing -> inv
applySuccessLog inv _ = inv

replayLogs :: [LogEntry] -> Inventario
replayLogs = foldl applySuccessLog M.empty . filter isSuccess
  where
    isSuccess (LogEntry _ _ _ Sucesso) = True
    isSuccess _ = False

ordenarInventario :: Inventario -> [Item]
ordenarInventario = sortOn itemID . M.elems

--------------------------------------------------------------------------------
-- Lógica pura
--------------------------------------------------------------------------------

addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem ts ident n qtd cat inv
  | null ident = Left "O itemID não pode ser vazio."
  | null n = Left "O nome não pode ser vazio."
  | null cat = Left "A categoria não pode ser vazia."
  | qtd <= 0 = Left "A quantidade deve ser maior que zero."
  | M.member ident inv = Left ("Já existe um item com itemID '" ++ ident ++ "'.")
  | otherwise =
      let item = mkItem ident n qtd cat
          novoInv = M.insert ident item inv
          logEnt = mkSuccessLog ts Add (itemToDetails item)
      in Right (novoInv, logEnt)

updateItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
updateItem ts ident n qtd cat inv
  | null ident = Left "O itemID não pode ser vazio."
  | null n = Left "O nome não pode ser vazio."
  | null cat = Left "A categoria não pode ser vazia."
  | qtd < 0 = Left "A quantidade não pode ser negativa."
  | not (M.member ident inv) = Left ("Item '" ++ ident ++ "' não encontrado.")
  | otherwise =
      let item = mkItem ident n qtd cat
          novoInv = M.insert ident item inv
          logEnt = mkSuccessLog ts Update (itemToDetails item)
      in Right (novoInv, logEnt)

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem ts ident qtd inv
  | null ident = Left "O itemID não pode ser vazio."
  | qtd <= 0 = Left "A quantidade removida deve ser maior que zero."
  | otherwise =
      case M.lookup ident inv of
        Nothing -> Left ("Item '" ++ ident ++ "' não encontrado.")
        Just item
          | quantidade item < qtd -> Left ("Estoque insuficiente para '" ++ ident ++ "'. Atual: " ++ show (quantidade item) ++ ", solicitado: " ++ show qtd ++ ".")
          | otherwise ->
              let novoQtd = quantidade item - qtd
                  novoInv = if novoQtd == 0
                              then M.delete ident inv
                              else M.insert ident item { quantidade = novoQtd } inv
                  logEnt = mkSuccessLog ts Remove (removeDetails ident qtd)
              in Right (novoInv, logEnt)

queryItem :: UTCTime -> String -> Inventario -> Either String (Maybe Item, LogEntry)
queryItem ts ident inv
  | null ident = Left "O itemID não pode ser vazio."
  | otherwise =
      case M.lookup ident inv of
        Nothing -> Left ("Item '" ++ ident ++ "' não encontrado.")
        Just item -> Right (Just item, mkSuccessLog ts Query (queryDetails ident))

--------------------------------------------------------------------------------
-- Análise de logs
--------------------------------------------------------------------------------

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErr
  where
    isErr (LogEntry _ _ _ (Falha _)) = True
    isErr _ = False

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem ident = filter hasItem
  where
    hasItem (LogEntry _ _ det _) = maybe False (== ident) (fieldValue "id" det)

itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  case sortOn (negate . snd) (M.toList counts) of
    []    -> Nothing
    x : _ -> Just x
  where
    counts = M.fromListWith (+)
      [ (ident, 1 :: Int)
      | LogEntry _ ac det Sucesso <- logs
      , ac `elem` [Add, Remove, Update]
      , Just ident <- [fieldValue "id" det]
      ]

--------------------------------------------------------------------------------
-- Persistência
--------------------------------------------------------------------------------

readMaybeFile :: Read a => FilePath -> IO (Maybe a)
readMaybeFile path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else catch (do
      content <- readFile path
      pure (case reads content of
        [(x, _)] -> Just x
        _        -> Nothing))
      handler
  where
    handler :: IOException -> IO (Maybe a)
    handler _ = pure Nothing

readLogFile :: FilePath -> IO [LogEntry]
readLogFile path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else catch (do
      content <- readFile path
      pure (mapMaybe readOne (lines content)))
      handler
  where
    readOne line = case reads line of
      [(x, _)] -> Just x
      _        -> Nothing
    handler :: IOException -> IO [LogEntry]
    handler _ = pure []

loadState :: IO AppState
loadState = do
  logs <- readLogFile logFile
  mInv <- readMaybeFile inventarioFile
  let inv = fromMaybe (replayLogs logs) mInv
  pure (AppState inv logs)

saveInventory :: Inventario -> IO ()
saveInventory inv =
  withFile inventarioFile WriteMode $ \h ->
    hPutStr h (show inv)

appendLog :: LogEntry -> IO ()
appendLog logEnt =
  withFile logFile AppendMode $ \h ->
    hPutStrLn h (show logEnt)

--------------------------------------------------------------------------------
-- Interface de terminal
--------------------------------------------------------------------------------

tokenize :: String -> [String]
tokenize = go False [] []
  where
    go _ cur acc [] = finalize cur acc
    go inQ cur acc (c:cs)
      | c == '"' = go (not inQ) cur acc cs
      | not inQ && isSpace c =
          case cur of
            [] -> go inQ [] acc cs
            _  -> go inQ [] (reverse cur : acc) cs
      | otherwise = go inQ (c:cur) acc cs

    finalize [] acc = reverse acc
    finalize cur acc = reverse (reverse cur : acc)

printInventory :: Inventario -> IO ()
printInventory inv = do
  putStrLn "\nInventário atual:"
  if null (M.elems inv)
    then putStrLn "(vazio)"
    else mapM_ printItem (ordenarInventario inv)
  where
    printItem i = putStrLn $ intercalate " | "
      [ "ID: " ++ itemID i
      , "Nome: " ++ nome i
      , "Qtd: " ++ show (quantidade i)
      , "Categoria: " ++ categoria i
      ]

showLogs :: [LogEntry] -> IO ()
showLogs [] = putStrLn "Nenhum log encontrado."
showLogs xs = mapM_ print xs

showReport :: AppState -> IO ()
showReport (AppState inv logs) = do
  putStrLn "\n===== RELATÓRIO ====="
  putStrLn $ "Itens em inventário: " ++ show (M.size inv)
  putStrLn $ "Total de logs: " ++ show (length logs)
  putStrLn $ "Logs de erro: " ++ show (length (logsDeErro logs))
  case itemMaisMovimentado logs of
    Nothing -> putStrLn "Item mais movimentado: nenhum dado suficiente."
    Just (ident, n) -> putStrLn $ "Item mais movimentado: " ++ ident ++ " (" ++ show n ++ " operações)"
  putStrLn "\nÚltimos erros:"
  let erros = reverse (logsDeErro logs)
  if null erros
    then putStrLn "Nenhum erro registrado."
    else mapM_ print (take 5 erros)
  putStrLn "=====================\n"

showHistoryForItem :: String -> [LogEntry] -> IO ()
showHistoryForItem ident logs = do
  putStrLn $ "\nHistórico do item '" ++ ident ++ "':"
  let hist = historicoPorItem ident logs
  if null hist
    then putStrLn "Nenhum registro encontrado."
    else mapM_ print hist

helpText :: IO ()
helpText = putStrLn $ unlines
  [ "Comandos disponíveis:"
  , "  add <id> <nome> <quantidade> <categoria>"
  , "  update <id> <nome> <quantidade> <categoria>"
  , "  remove <id> <quantidade>"
  , "  query <id>"
  , "  list"
  , "  report"
  , "  report errors"
  , "  report item <id>"
  , "  help"
  , "  exit"
  , ""
  , "Observação: use aspas para nomes/categorias com espaço."
  , "Exemplo: add P001 \"Teclado Mecânico\" 10 \"Periféricos\""
  ]

handleSuccess :: AppState -> Inventario -> LogEntry -> IO AppState
handleSuccess st novoInv logEnt = do
  saveInventory novoInv
  appendLog logEnt
  putStrLn "Operação concluída com sucesso."
  pure st { estadoInventario = novoInv, estadoLogs = estadoLogs st ++ [logEnt] }

handleFailure :: AcaoLog -> AppState -> String -> IO AppState
handleFailure ac st err = do
  ts <- getCurrentTime
  let logEnt = mkFailureLog ts ac "" err
  appendLog logEnt
  putStrLn $ "Erro: " ++ err
  pure st { estadoLogs = estadoLogs st ++ [logEnt] }

handleQueryResult :: AppState -> Either String (Maybe Item, LogEntry) -> IO AppState
handleQueryResult st (Right (mItem, logEnt)) = do
  appendLog logEnt
  case mItem of
    Just item -> putStrLn $ "Encontrado: " ++ show item
    Nothing   -> pure ()
  pure st { estadoLogs = estadoLogs st ++ [logEnt] }
handleQueryResult st (Left err) = handleFailure QueryFail st err

processCommand :: AppState -> String -> IO AppState
processCommand st raw =
  case tokenize raw of
    [] -> pure st
    (cmd:args) -> case normalize cmd of
      "add" ->
        case args of
          [ident, n, qtdStr, cat] ->
            case safeReadInt qtdStr of
              Just qtd -> do
                ts <- getCurrentTime
                case addItem ts ident n qtd cat (estadoInventario st) of
                  Right (novoInv, logEnt) -> handleSuccess st novoInv logEnt
                  Left err                -> handleFailure Add st err
              Nothing -> handleFailure Add st "Quantidade inválida."
          _ -> handleFailure Add st "Uso: add <id> <nome> <quantidade> <categoria>"
      "update" ->
        case args of
          [ident, n, qtdStr, cat] ->
            case safeReadInt qtdStr of
              Just qtd -> do
                ts <- getCurrentTime
                case updateItem ts ident n qtd cat (estadoInventario st) of
                  Right (novoInv, logEnt) -> handleSuccess st novoInv logEnt
                  Left err                -> handleFailure Update st err
              Nothing -> handleFailure Update st "Quantidade inválida."
          _ -> handleFailure Update st "Uso: update <id> <nome> <quantidade> <categoria>"
      "remove" ->
        case args of
          [ident, qtdStr] ->
            case safeReadInt qtdStr of
              Just qtd -> do
                ts <- getCurrentTime
                case removeItem ts ident qtd (estadoInventario st) of
                  Right (novoInv, logEnt) -> handleSuccess st novoInv logEnt
                  Left err                -> handleFailure Remove st err
              Nothing -> handleFailure Remove st "Quantidade inválida."
          _ -> handleFailure Remove st "Uso: remove <id> <quantidade>"
      "query" ->
        case args of
          [ident] -> do
            ts <- getCurrentTime
            handleQueryResult st (queryItem ts ident (estadoInventario st))
          _ -> handleQueryResult st (Left "Uso: query <id>")
      "list" -> do
        ts <- getCurrentTime
        let logEnt = mkSuccessLog ts ListItems "listagem"
        appendLog logEnt
        printInventory (estadoInventario st)
        pure st { estadoLogs = estadoLogs st ++ [logEnt] }
      "report" ->
        case args of
          [] -> do
            ts <- getCurrentTime
            let logEnt = mkSuccessLog ts Report "relatorio-geral"
            showReport st
            appendLog logEnt
            pure st { estadoLogs = estadoLogs st ++ [logEnt] }
          ["errors"] -> do
            ts <- getCurrentTime
            let logEnt = mkSuccessLog ts Report "relatorio-erros"
            putStrLn "\n===== LOGS DE ERRO ====="
            showLogs (logsDeErro (estadoLogs st))
            putStrLn "========================\n"
            appendLog logEnt
            pure st { estadoLogs = estadoLogs st ++ [logEnt] }
          ["item", ident] -> do
            ts <- getCurrentTime
            let logEnt = mkSuccessLog ts Report ("relatorio-item:" ++ ident)
            showHistoryForItem ident (estadoLogs st)
            appendLog logEnt
            pure st { estadoLogs = estadoLogs st ++ [logEnt] }
          _ -> handleFailure Report st "Uso: report | report errors | report item <id>"
      "help" -> helpText >> pure st
      _ -> do
        putStrLn "Comando não reconhecido. Digite 'help' para ver a lista de comandos."
        pure st

shouldExit :: String -> Bool
shouldExit s = case tokenize s of
  (cmd:_) -> normalize cmd `elem` ["exit", "quit"]
  _       -> False

loop :: AppState -> IO ()
loop st = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if shouldExit line
    then putStrLn "Encerrando..."
    else do
      st' <- processCommand st line
      loop st'

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Sistema de Inventário em Haskell"
  putStrLn "Carregando dados..."
  st <- loadState
  putStrLn $ "Itens carregados: " ++ show (M.size (estadoInventario st))
  putStrLn $ "Logs carregados: " ++ show (length (estadoLogs st))
  putStrLn "Digite 'help' para ver os comandos."
  loop st
