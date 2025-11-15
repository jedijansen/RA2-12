module Main where

import Inventory.Types
import Inventory.Logic
import Inventory.Reports

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO
import Data.Time.Clock (getCurrentTime)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaFile :: FilePath
auditoriaFile = "Auditoria.log"

-- Carregar inventário (espera que o arquivo seja o 'show' do Inventario)
carregarInventario :: IO Inventario
carregarInventario = do
  exists <- doesFileExist inventarioFile
  if not exists then return Map.empty
  else do
    s <- readFile inventarioFile
    case readMaybe s of
      Just inv -> return inv
      Nothing  -> do
        putStrLn "Aviso: não foi possível desserializar Inventario.dat. Iniciando vazio."
        return Map.empty

-- Carregar logs: cada linha um Show LogEntry
carregarLogs :: IO [LogEntry]
carregarLogs = do
  exists <- doesFileExist auditoriaFile
  if not exists then return []
  else do
    s <- readFile auditoriaFile
    let ls = filter (not . null) (lines s)
        parsed = mapMaybe readMaybe ls
    return parsed

appendLogArquivo :: LogEntry -> IO ()
appendLogArquivo le = appendFile auditoriaFile (show le ++ "\n")

salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile inventarioFile (show inv)

-- Parser mínimo: separa por espaços. Para nomes com espaços usar "_" ou aspas (não tratado aqui).
tokens :: String -> [String]
tokens = words

main :: IO ()
main = do
  createDirectoryIfMissing True "data"
  inv <- carregarInventario
  logs <- carregarLogs
  putStrLn $ "Inventário carregado: " ++ show (Map.size inv) ++ " itens. Logs carregados: " ++ show (length logs)
  loop inv logs

loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  putStrLn "\nDigite comando (help para ajuda):"
  l <- getLine
  now <- getCurrentTime
  case tokens l of
    [] -> loop inv logs
    ("help":_) -> do
      putStrLn "Comandos: add <id> <nome_sem_espacos> <quantidade> <categoria> | remove <id> <q> | update <id> <q> | hist <id> | ultimas | movimentados | erros | report | listar | sair"
      loop inv logs

    ("add":iid:nome:qStr:categoriaParts) ->
      case readMaybe qStr :: Maybe Int of
        Nothing -> putStrLn "quantidade inválida" >> loop inv logs
        Just q -> do
          let nomeFull = nome -- simplificação: nome sem espaços
              novo = mkItem iid nomeFull q (unwords categoriaParts)
          case addItem now novo inv of
            Left err -> do
              let le = mkLogEntry now (Add iid q) ("falha add: " ++ err) (Falha err)
              appendLogArquivo le
              putStrLn $ "ERRO: " ++ err
              loop inv (le:logs)
            Right (inv', le) -> do
              salvarInventario inv'
              appendLogArquivo le
              putStrLn $ "Item adicionado: " ++ show (itemID novo)
              loop inv' (le:logs)

    ("remove":iid:qStr:_) ->
      case readMaybe qStr :: Maybe Int of
        Nothing -> putStrLn "quantidade inválida" >> loop inv logs
        Just q ->
          case removeItem now iid q inv of
            Left err -> do
              let le = mkLogEntry now (Remove iid q) ("falha remove: " ++ err) (Falha err)
              appendLogArquivo le
              putStrLn $ "ERRO: " ++ err
              loop inv (le:logs)
            Right (inv', le) -> do
              salvarInventario inv'
              appendLogArquivo le
              putStrLn "Removido (ou reduzido quantidade)."
              loop inv' (le:logs)

    ("update":iid:qStr:_) ->
      case readMaybe qStr :: Maybe Int of
        Nothing -> putStrLn "quantidade inválida" >> loop inv logs
        Just q ->
          case updateQty now iid q inv of
            Left err -> do
              let le = mkLogEntry now (UpdateQty iid q) ("falha update: " ++ err) (Falha err)
              appendLogArquivo le
              putStrLn $ "ERRO: " ++ err
              loop inv (le:logs)
            Right (inv', le) -> do
              salvarInventario inv'
              appendLogArquivo le
              putStrLn "Quantidade atualizada."
              loop inv' (le:logs)

    ("hist":iid:_) -> do
      let h = historicoPorItem iid logs
      mapM_ (putStrLn . show) h
      loop inv logs

    ("ultimas":_) -> do
      let u = ultimaTentativaPorAcao logs
      mapM_ (\(k,v) -> putStrLn (k ++ " -> " ++ show v)) u
      loop inv logs

    ("movimentados":_) -> do
      let r = rankingMovimentacao logs
      mapM_ (\(i,c) -> putStrLn (i ++ " : " ++ show c)) r
      loop inv logs

    ("erros":_) -> do
      let es = logsDeErro logs
      putStrLn $ "Erros: " ++ show (length es)
      mapM_ (putStrLn . show) es
      loop inv logs

    ("report":_) -> do
      let resumo = resumoPorCategoria inv logs
      putStrLn "Resumo por categoria (categoria, nº itens distintos, somaQtd):"
      mapM_ (putStrLn . show) resumo
      loop inv logs

    ("listar":_) -> do
      putStrLn "Inventário atual (itens):"
      mapM_ (putStrLn . show) (Map.elems inv)
      loop inv logs

    ("sair":_) -> putStrLn "Encerrando..."

    _ -> putStrLn "Comando inválido. help para ajuda." >> loop inv logs
