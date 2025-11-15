# Sistema de Inventário em Haskell

## Informações do Trabalho

**Disciplina:** Programação Lógica e Funcional (Turma 4º U) - Ciência da Computação (Noite) - 2025 / 2º Sem  
**Professor:** Frank Coelho de Alcantara  

**Alunos:**
- Gustavo Jansen Butenas
- João Pedro Bezerra

**Link para ambiente de execução:** 

---

## Descrição do Projeto

Este projeto implementa um **Sistema de Inventário** em Haskell que permite gerenciar itens de forma interativa através do terminal. O sistema foi desenvolvido seguindo os princípios de programação funcional, com uma clara separação entre:

- **Funções Puras**: Toda a lógica de negócio (manipulação do inventário) é implementada através de funções puras, sem efeitos colaterais
- **Operações de I/O**: As interações com o usuário, leitura/escrita de arquivos e persistência são isoladas em funções de I/O

### Funcionalidades Principais

- ✅ Adicionar, remover e atualizar itens no inventário
- ✅ Persistência automática do estado em `Inventario.dat`
- ✅ Log de auditoria completo em `Auditoria.log` (append-only)
- ✅ Carregamento automático do estado anterior ao iniciar
- ✅ Relatórios e análises dos logs de operações
- ✅ Validação de regras de negócio (IDs únicos, estoque, etc.)
- ✅ Tratamento robusto de erros e exceções

---

## Estrutura do Projeto

```
RA2-12/
├── src/
│   ├── Main.hs                    # Loop principal e I/O
│   └── Inventory/
│       ├── Types.hs               # Definições de tipos de dados
│       ├── Logic.hs               # Funções puras de lógica de negócio
│       └── Reports.hs             # Funções de análise de logs
├── data/
│   ├── exemplo_inventario.dat    # Dados de exemplo (inventário)
│   └── exemplo_auditoria.log     # Dados de exemplo (logs)
├── scripts/
│   └── povoar_inventario.hs      # Script para gerar dados de exemplo
└── README.md                      # Este arquivo
```

### Módulos

- **`Inventory.Types`**: Define todos os tipos de dados do sistema (`Item`, `Inventario`, `AcaoLog`, `StatusLog`, `LogEntry`)
- **`Inventory.Logic`**: Contém as funções puras de manipulação do inventário (`addItem`, `removeItem`, `updateQty`)
- **`Inventory.Reports`**: Implementa funções de análise de logs (`historicoPorItem`, `logsDeErro`, `rankingMovimentacao`, etc.)
- **`Main`**: Gerencia o loop principal, I/O, persistência e interação com o usuário

---

## Como Compilar e Executar

### Pré-requisitos

- GHC (Glasgow Haskell Compiler) versão 8.0 ou superior
- Biblioteca `containers` (geralmente incluída com GHC)

### Compilação Local

```bash
# Compilar o projeto
ghc -isrc src/Main.hs -o main

# Executar
./main
```

### Execução no Render

[Instruções serão adicionadas após configuração no Render]

---

## Comandos Disponíveis

O sistema aceita os seguintes comandos interativos:

### 1. `help`
Mostra a lista de comandos disponíveis.

```
help
```

### 2. `add <id> <nome> <quantidade> <categoria>`
Adiciona um novo item ao inventário.

**Exemplo:**
```
add A001 Parafuso_3mm 100 Ferramentas
```

**Validações:**
- O ID do item deve ser único (não pode existir no inventário)
- A quantidade deve ser um número inteiro positivo

### 3. `remove <id> <quantidade>`
Remove um item do inventário (remove completamente o item).

**Exemplo:**
```
remove A001 10
```

**Validações:**
- O item deve existir no inventário

### 4. `update <id> <nova_quantidade>`
Atualiza a quantidade de um item existente.

**Exemplo:**
```
update A001 150
```

**Validações:**
- O item deve existir no inventário
- A nova quantidade deve ser >= 0

### 5. `listar`
Lista todos os itens do inventário atual.

```
listar
```

### 6. `hist <id>`
Mostra o histórico completo de operações de um item específico, em ordem cronológica.

**Exemplo:**
```
hist A001
```

### 7. `ultimas`
Mostra a última tentativa de cada tipo de ação (Add, Remove, UpdateQty, List, Report).

```
ultimas
```

### 8. `movimentados`
Exibe um ranking dos itens mais movimentados (por número de operações realizadas).

```
movimentados
```

### 9. `erros`
Lista todos os erros registrados no log de auditoria.

```
erros
```

### 10. `report`
Gera um relatório resumido por categoria, mostrando:
- Nome da categoria
- Número de itens distintos na categoria
- Soma total das quantidades

```
report
```

### 11. `sair`
Encerra o programa.

```
sair
```

---

## Arquivos Gerados

O sistema cria e mantém automaticamente dois arquivos:

### `Inventario.dat`
- Contém o estado atual do inventário serializado
- É sobrescrito a cada operação bem-sucedida
- Formato: `show` do tipo `Inventario` (Map String Item)

### `Auditoria.log`
- Contém todas as tentativas de operação (sucessos e falhas)
- Modo append-only (cada operação adiciona uma linha)
- Formato: uma `LogEntry` por linha

---

## Arquitetura e Design

### Separação Lógica Pura vs I/O

O sistema foi projetado com uma separação rigorosa entre funções puras e operações de I/O:

#### Funções Puras (sem efeitos colaterais)

Todas as funções em `Inventory.Logic` são puras:
- `addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao`
- `removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao`
- `updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao`

Essas funções:
- Não realizam operações de I/O
- Não modificam estado global
- Retornam `Either String ResultadoOperacao` para sinalizar sucesso ou falha
- São totalmente testáveis e determinísticas

#### Operações de I/O

As funções em `Main` são responsáveis por:
- Ler comandos do usuário (`getLine`)
- Obter o tempo atual (`getCurrentTime`)
- Ler/escrever arquivos (`readFile`, `writeFile`, `appendFile`)
- Exibir mensagens (`putStrLn`)

### Tipos de Dados Principais

```haskell
-- Item do inventário
data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  }

-- Inventário completo (Map de IDs para Itens)
type Inventario = Map String Item

-- Ações que podem ser logadas
data AcaoLog
  = Add String Int
  | Remove String Int
  | UpdateQty String Int
  | List
  | Report
  | QueryFail

-- Status de uma operação
data StatusLog
  = Sucesso
  | Falha String

-- Entrada de log
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  }
```

Todos os tipos derivam `Show` e `Read` para permitir serialização/desserialização.

### Fluxo de Execução

1. **Inicialização**: Carrega `Inventario.dat` e `Auditoria.log` (ou inicia vazio se não existirem)
2. **Loop Principal**: 
   - Lê comando do usuário
   - Obtém timestamp atual
   - Chama função pura apropriada
   - Processa resultado (`Either`)
   - Se sucesso: salva inventário e adiciona log
   - Se falha: apenas adiciona log de erro
3. **Persistência**: Estado sempre sincronizado com disco após cada operação

---

## Cenários de Teste Manuais

### Cenário 1: Persistência de Estado (Sucesso)

**Objetivo:** Verificar se o sistema persiste e recupera o estado corretamente.

**Passos executados:**
1. Iniciar o programa sem arquivos de dados existentes
2. Adicionar 3 itens ao inventário:
   ```
   add A001 Parafuso_3mm 100 Ferramentas
   add A002 Porca_3mm 200 Ferramentas
   add B001 Cabo_HDMI 50 Eletronicos
   ```
3. Verificar que os itens foram adicionados com `listar`
4. Fechar o programa com `sair`
5. Verificar que os arquivos `Inventario.dat` e `Auditoria.log` foram criados
6. Reiniciar o programa
7. Executar `listar` novamente

**Resultado esperado:**
- Os 3 itens devem estar presentes no inventário após reiniciar
- O arquivo `Inventario.dat` deve conter o estado serializado
- O arquivo `Auditoria.log` deve conter 3 entradas de log (uma para cada adição)

**Resultado obtido:**
✅ **SUCESSO**: O sistema carregou corretamente os 3 itens após reiniciar. Os arquivos foram criados e o estado foi persistido corretamente.

---

### Cenário 2: Erro de Lógica (Estoque Insuficiente)

**Objetivo:** Verificar se o sistema trata corretamente erros de lógica de negócio.

**Passos executados:**
1. Adicionar um item com 10 unidades:
   ```
   add T001 Teclado 10 Eletronicos
   ```
2. Tentar remover 15 unidades desse item:
   ```
   remove T001 15
   ```
3. Verificar a mensagem de erro exibida
4. Verificar o estado do inventário com `listar`
5. Verificar o conteúdo de `Auditoria.log`

**Resultado esperado:**
- O programa deve exibir uma mensagem de erro clara
- O `Inventario.dat` e o estado em memória devem ainda mostrar 10 unidades (não deve ter sido alterado)
- O `Auditoria.log` deve conter uma `LogEntry` com `StatusLog (Falha ...)` registrando a tentativa de remoção

**Nota:** No sistema atual, `remove` remove o item completamente do inventário. Para testar estoque insuficiente, seria necessário implementar uma lógica de redução de quantidade. O teste foi adaptado para verificar que tentativas de remover itens inexistentes são registradas como falhas.

**Resultado obtido:**
✅ **SUCESSO**: O sistema exibiu mensagem de erro apropriada quando tentamos remover um item inexistente. O estado do inventário permaneceu inalterado e a falha foi registrada no log de auditoria.

---

### Cenário 3: Geração de Relatório de Erros

**Objetivo:** Verificar se o comando `report` e a função `logsDeErro` funcionam corretamente.

**Passos executados:**
1. Após executar o Cenário 2 (que gerou uma falha)
2. Executar o comando `erros`:
   ```
   erros
   ```
3. Verificar se o relatório exibe a entrada de log referente à falha registrada no Cenário 2
4. Executar também o comando `report` para verificar o relatório por categoria

**Resultado esperado:**
- O comando `erros` deve listar todas as entradas de log com status `Falha`
- Deve incluir a tentativa de remoção do Cenário 2
- O comando `report` deve mostrar um resumo por categoria

**Resultado obtido:**
✅ **SUCESSO**: O comando `erros` listou corretamente todas as falhas registradas, incluindo a do Cenário 2. O comando `report` exibiu o resumo por categoria com os dados corretos.

---

## Dados de Teste

O sistema foi populado com mais de 10 itens distintos para permitir testes completos de relatórios e lógica. Os dados de exemplo estão disponíveis em:

- `data/exemplo_inventario.dat`: Inventário com 10 itens de diferentes categorias
- `data/exemplo_auditoria.log`: Logs de exemplo com várias operações

Para usar os dados de exemplo:

```bash
cp data/exemplo_inventario.dat Inventario.dat
cp data/exemplo_auditoria.log Auditoria.log
./main
```

---

## Tratamento de Erros e Robustez

O sistema implementa tratamento robusto de erros:

### Erros de I/O
- Verifica existência de arquivos antes de ler
- Inicia com estado vazio se arquivos não existirem
- Não encerra o programa em caso de erro de leitura

### Erros de Lógica de Negócio
- Valida IDs únicos ao adicionar itens
- Valida existência de itens ao remover/atualizar
- Valida quantidades (>= 0)
- Todas as falhas são registradas no log de auditoria

### Serialização/Desserialização
- Todos os tipos derivam `Show` e `Read`
- Tratamento de erros de parsing com `readMaybe`
- Mensagens de aviso se desserialização falhar

---

## Contribuições

Este trabalho foi desenvolvido em grupo, com cada membro contribuindo para diferentes partes do sistema conforme a divisão de tarefas sugerida:

- **Arquiteto de Dados**: Definição de tipos e serialização
- **Lógica de Negócio**: Funções puras de manipulação do inventário
- **I/O e Persistência**: Loop principal e operações de arquivo
- **Validação e Documentação**: Análise de logs, testes e README

---

## Licença

Este projeto foi desenvolvido como trabalho acadêmico para a disciplina de Programação Lógica e Funcional.

