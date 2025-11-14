module Main where

import System.Directory (createDirectoryIfMissing)
import System.IO
import Data.Time.Clock (getCurrentTime)
import Inventory.Types (Item(..), mkItem, LogEntry(..), StatusLog(..), AcaoLog(..))
import qualified Data.Map as Map

main :: IO ()
main = do
  createDirectoryIfMissing True "data"
  let itens = [ mkItem "A001" "Parafuso_3mm" 100 "Ferramentas"
              , mkItem "A002" "Porca_3mm" 200 "Ferramentas"
              , mkItem "A003" "Arruela_3mm" 150 "Ferramentas"
              , mkItem "B001" "Cabo_HDMI" 50 "Eletronicos"
              , mkItem "B002" "Fonte_5V" 30 "Eletronicos"
              , mkItem "C001" "Papel_A4" 500 "Escritorio"
              , mkItem "C002" "Caneta_Azul" 300 "Escritorio"
              , mkItem "D001" "Lampada_LED" 80 "Iluminacao"
              , mkItem "E001" "Bateria_AAA" 120 "Baterias"
              , mkItem "F001" "Fita_Isolante" 90 "Eletricidade"
              ]
  writeFile "data/exemplo_inventario.dat" (show (Map.fromList (map (\it -> (itemID it, it)) itens)))
  now <- getCurrentTime
  let logs = [ show $ LogEntry now (Add "A001" 10) "adicionado init" Sucesso
             , show $ LogEntry now (Remove "A001" 5) "removido init" Sucesso
             , show $ LogEntry now (UpdateQty "A002" 250) "update init" Sucesso
             , show $ LogEntry now (Add "C001" 100) "adicionado init" Sucesso
             , show $ LogEntry now (Remove "X999" 1) "tentativa item inexistente" (Falha "Item ausente")
             ]
  writeFile "data/exemplo_auditoria.log" (unlines logs)
  putStrLn "Arquivos data/exemplo_inventario.dat e data/exemplo_auditoria.log criados."
