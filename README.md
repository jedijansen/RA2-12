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


#
