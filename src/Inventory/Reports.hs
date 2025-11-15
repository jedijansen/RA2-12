module Inventory.Reports
  ( historicoPorItem
  , ultimaTentativaPorAcao
  , rankingMovimentacao
  , itemMaisMovimentado
  , logsDeErro
  , resumoPorCategoria
  , topNMovimentados
  ) where

import Inventory.Types
  ( Item(..)
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  , Inventario
  )
import Data.List (sortOn, foldl')
import Data.Ord (Down(..))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

-- 1) históricoPorItem :: ItemID -> [LogEntry] -> [LogEntry]
-- Retorna todos os logs que envolvem o ItemID, em ordem cronológica (mais antigo -> mais novo).
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid logs =
  let filtrar le = case acao le of
        Add i _      -> i == iid
        Remove i _   -> i == iid
        UpdateQty i _-> i == iid
        _             -> False
  in filter filtrar (sortOn timestamp logs)

-- 2) ultimaTentativaPorAcao :: [LogEntry] -> [(String, LogEntry)]
-- Para cada tipo de ação (Add, Remove, UpdateQty, Listar, Report) devolve o log mais recente daquele tipo.
-- A chave retornada é uma String representando o "tipo" (ex: "Add", "Remove", ...).
ultimaTentativaPorAcao :: [LogEntry] -> [(String, LogEntry)]
ultimaTentativaPorAcao logs =
  let tagOf le = case acao le of
        Add _ _    -> "Add"
        Remove _ _ -> "Remove"
        UpdateQty _ _ -> "UpdateQty"
        List       -> "List"
        Report     -> "Report"
        QueryFail  -> "QueryFail"
      folder m le =
        let k = tagOf le
        in case M.lookup k m of
             Nothing -> M.insert k le m
             Just old -> if timestamp le >= timestamp old then M.insert k le m else m
      m = foldl' folder M.empty logs
  in M.toList m

-- 3) rankingMovimentacao :: [LogEntry] -> [(String, Int)]
-- Conta quantas vezes cada ItemID aparece em logs (Add/Remove/UpdateQty).
rankingMovimentacao :: [LogEntry] -> [(String, Int)]
rankingMovimentacao logs =
  let ids = mapMaybe (\le -> case acao le of
                        Add i _ -> Just i
                        Remove i _ -> Just i
                        UpdateQty i _ -> Just i
                        _ -> Nothing) logs
      counts = foldl' (\m i -> M.insertWith (+) i 1 m) M.empty ids
  in sortOn (Down . snd) (M.toList counts)

-- 4) itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado logs =
  case rankingMovimentacao logs of
    [] -> Nothing
    ((iid, _):_) -> Just iid

-- 5) logsDeErro :: [LogEntry] -> [LogEntry]
-- Filtra apenas logs com status Falha
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isFalha
  where
    isFalha le = case status le of
                   Falha _ -> True
                   _       -> False

-- 6) resumoPorCategoria :: Inventario -> [LogEntry] -> [(String, Int, Int)]
-- Para cada categoria retorna (categoria, total_itens_distintos_na_categoria, soma_quantidades)
-- Função pura: útil para relatórios sumarizados.
resumoPorCategoria :: Inventario -> [LogEntry] -> [(String, Int, Int)]
resumoPorCategoria inventario _logs =
  -- baseado apenas no inventario atual (não altera estado)
  let itens = map snd (M.toList inventario)
      byCat = foldl' (\m it -> M.insertWith
                                (\(nc1, q1) (nc2, q2) -> (nc1 + nc2, q1 + q2))
                                (categoria it) (1, quantidade it) m
                     ) M.empty itens
  in map (\(cat,(n,q)) -> (cat, n, q)) (M.toList byCat)

-- 7) topNMovimentados :: Int -> [LogEntry] -> [(String, Int)]
-- Retorna os N itens mais movimentados (por número de eventos)
topNMovimentados :: Int -> [LogEntry] -> [(String, Int)]
topNMovimentados n logs = take n (rankingMovimentacao logs)
