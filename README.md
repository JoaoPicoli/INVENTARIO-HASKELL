# Sistema de Inventário em Haskell

## Contexto da Atividade

Este projeto foi desenvolvido como parte de uma atividade acadêmica da disciplina de Programação Lógica e Funcional, com o objetivo de praticar conceitos de Haskell.O sistema implementa um inventário interativo via terminal, permitindo o gerenciamento de itens com operações de adicionar, remover, atualizar, consultar e gerar relatórios.

---

## Funcionalidades do Sistema

- Adição de itens ao inventário
- Remoção de itens com controle de estoque
- Atualização de itens existentes
- Consulta individual de itens
- Listagem completa do inventário
- Geração de relatórios (erros, histórico e estatísticas)
- Persistência automática em arquivo (`Inventario.dat`)
- Log de auditoria append-only (`Auditoria.log`)
- Recuperação automática de estado ao iniciar o sistema

---

## Como executar

O projeto pode ser executado em ambientes como:

GDB Online, Replit (não consegui usar essa plataforma), Terminal local com GHC (foi o que eu usei nesse caso)

Comando:

```bash 
runghc Main.hs
````

---

# Cenários de Teste

---

## Cenário 1 — Persistência de Estado (Sucesso)

### Passos executados:

* Inicialização do sistema sem arquivos
* Inserção de 3 itens
* Encerramento do programa
* Reinicialização do sistema
* Verificação do carregamento do estado

### Comando de teste:

```bash
list
```

### Evidência (Logs / Terminal)

![Cenário 1 - execução](cenario1.png)

---

## Cenário 2 — Erro de Lógica (Estoque Insuficiente)

### Passos executados:

* Adição de item com estoque inicial
* Tentativa de remoção acima da quantidade disponível
* Verificação de erro retornado
* Confirmação de integridade do inventário

### Comando de teste:

```bash
remove P010 15
```

### Evidência (Erro no terminal)

![Erro estoque insuficiente](cenario2.png)

### Evidência (Inventário após erro)

![Inventário após falha](cenario2.1.png)

---

## Cenário 3 — Geração de Relatórios

### Passos executados:

* Execução de operações com falhas
* Geração de relatório de erros
* Análise de logs filtrados
* Consulta de histórico por item

### Comandos de teste:

```bash
report errors
report
report item P010
```

### Evidência (Relatório geral)

![Report item](cenario3.png)

---

# Estrutura do Projeto

* `Main.hs` → Código principal do sistema
* `Inventario.dat` → Persistência do estado atual
* `Auditoria.log` → Log de auditoria (append-only)

---

# Link de Execução Online

Clique na imagem abaixo para acessar o ambiente de execução no GDB Online:

[![Executar no GDB Online](gdbonline.png)](https://www.onlinegdb.com/ExBrpouqR)

---


