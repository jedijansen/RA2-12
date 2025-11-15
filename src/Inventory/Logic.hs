module Inventory.Logic
  ( addItem
  , removeItem
  , updateQty
  , listItems
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Inventory.Types
  ( Item(..)
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  , Inventario
  , ResultadoOperacao
  , mkLogEntry
  , lookupItem
  )

-- | Adiciona um item ao inventário.
-- Validação: o ID do item deve ser único (não pode existir no inventário).
-- Retorna Left com mensagem de erro se o ID já existe, Right com o inventário atualizado e log.
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem timestamp item inventario =
  case lookupItem (itemID item) inventario of
    Just _ ->
      let mensagemErro = "Item com ID '" ++ itemID item ++ "' já existe no inventário"
          logEntry = mkLogEntry timestamp (Add (itemID item) (quantidade item)) mensagemErro (Falha mensagemErro)
       in Left mensagemErro
    Nothing ->
      let novoInventario = Map.insert (itemID item) item inventario
          detalhes = "Item adicionado: ID=" ++ itemID item ++ ", Nome=" ++ nome item ++ ", Qtd=" ++ show (quantidade item) ++ ", Categoria=" ++ categoria item
          logEntry = mkLogEntry timestamp (Add (itemID item) (quantidade item)) detalhes Sucesso
       in Right (novoInventario, logEntry)

-- | Remove um item do inventário.
-- Validação: o item deve existir no inventário.
-- Retorna Left com mensagem de erro se o item não existe, Right com o inventário atualizado e log.
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem timestamp itemID quantidade inventario =
  case lookupItem itemID inventario of
    Nothing ->
      let mensagemErro = "Item com ID '" ++ itemID ++ "' não existe no inventário"
          logEntry = mkLogEntry timestamp (Remove itemID quantidade) mensagemErro (Falha mensagemErro)
       in Left mensagemErro
    Just item ->
      let novoInventario = Map.delete itemID inventario
          detalhes = "Item removido: ID=" ++ itemID ++ ", Nome=" ++ nome item ++ ", Quantidade=" ++ show quantidade
          logEntry = mkLogEntry timestamp (Remove itemID quantidade) detalhes Sucesso
       in Right (novoInventario, logEntry)

-- | Atualiza a quantidade de um item no inventário.
-- Validação: o item deve existir e a nova quantidade deve ser >= 0.
-- Se a nova quantidade for menor que a atual, valida se há estoque suficiente (não aplicável aqui, pois estamos definindo a quantidade diretamente).
-- Retorna Left com mensagem de erro se o item não existe ou quantidade inválida, Right com o inventário atualizado e log.
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty timestamp itemID novaQuantidade inventario
  | novaQuantidade < 0 =
      let mensagemErro = "Quantidade inválida: " ++ show novaQuantidade ++ " (deve ser >= 0)"
          logEntry = mkLogEntry timestamp (UpdateQty itemID novaQuantidade) mensagemErro (Falha mensagemErro)
       in Left mensagemErro
  | otherwise =
      case lookupItem itemID inventario of
        Nothing ->
          let mensagemErro = "Item com ID '" ++ itemID ++ "' não existe no inventário"
              logEntry = mkLogEntry timestamp (UpdateQty itemID novaQuantidade) mensagemErro (Falha mensagemErro)
           in Left mensagemErro
        Just itemAntigo ->
          let itemAtualizado = itemAntigo { quantidade = novaQuantidade }
              novoInventario = Map.insert itemID itemAtualizado inventario
              detalhes = "Quantidade atualizada: ID=" ++ itemID ++ ", Nome=" ++ nome itemAntigo ++ ", Qtd antiga=" ++ show (quantidade itemAntigo) ++ ", Qtd nova=" ++ show novaQuantidade
              logEntry = mkLogEntry timestamp (UpdateQty itemID novaQuantidade) detalhes Sucesso
           in Right (novoInventario, logEntry)

-- | Lista todos os itens do inventário.
-- Sempre retorna sucesso, mesmo se o inventário estiver vazio.
-- Retorna Right com o inventário (inalterado) e log de sucesso.
listItems :: UTCTime -> Inventario -> Either String ResultadoOperacao
listItems timestamp inventario =
  let totalItens = Map.size inventario
      itensLista = Map.elems inventario
      detalhes = "Listagem de itens: total=" ++ show totalItens ++ ", itens=" ++ formatarItens itensLista
      logEntry = mkLogEntry timestamp List detalhes Sucesso
   in Right (inventario, logEntry)

-- | Helper puro para formatar lista de itens para o log.
-- Formata os itens em uma string legível.
formatarItens :: [Item] -> String
formatarItens [] = "[]"
formatarItens itens = "[" ++ intercalar ", " (map formatarItem itens) ++ "]"
  where
    formatarItem item = "Item{ID=" ++ itemID item ++ ",Nome=" ++ nome item ++ ",Qtd=" ++ show (quantidade item) ++ ",Cat=" ++ categoria item ++ "}"

-- | Helper puro para intercalar um separador entre elementos de uma lista.
-- Equivalente a Data.List.intercalate, mas implementado aqui para evitar dependências extras.
intercalar :: String -> [String] -> String
intercalar _ [] = ""
intercalar _ [x] = x
intercalar sep (x:xs) = x ++ sep ++ intercalar sep xs

