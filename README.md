# ⚡ Espelhar Elétrica (AutoCAD AutoLISP) - v17.0

Plugin profissional em **AutoLISP** para **AutoCAD** desenvolvido para resolver o problema clássico de espelhamento (`MIRROR`) em projetos elétricos prediais e residenciais.

O comando nativo `MIRROR` do AutoCAD frequentemente inverte a simbologia elétrica (interruptores de cabeça para baixo, etiquetas invertidas, condutores e fiações fora de posição, ordem de circuitos invertida e ganchos de neutro/terra apontando para o lado errado). Este plugin automatiza a correção geométrica completa instantaneamente.

---

## 🚀 Funcionalidades Principais

### 1. Espelhamento Inteligente Automático (`EE` / `EMIRROR`)
- **Detecção Automática do Eixo:** Detecta automaticamente se a linha de espelho é Horizontal (Cima/Baixo) ou Vertical (Esquerda/Direita).
- **Compatibilidade Bilíngue:** Suporta AutoCAD em Português e Inglês com supressão de OSNAP (`_non`).

### 2. Espelho Horizontal (Cima / Baixo)
- **Interruptores e Tags:** Desvira interruptores (`S`, `S_...`) e tags de iluminação (`1|Q`) mantendo-os em pé, girando no próprio centro geométrico.
- **Fiação Apoiada com Deslocamento Zero:** Espelha cada bloco de condutor (retornos, fase, neutro, terra) exatamente pela linha de inserção, mantendo os traços apoiados na linha guia e apontando para **CIMA**.
- **Offset Padronizado de Textos:**
  - Bitolas (`#...`) descem `0.40m`.
  - Circuitos sobem `0.40m`.
- **Proteção Total:** Tomadas, eletrodutos e equipamentos não sofrem alterações indevidas.

### 3. Espelho Vertical (Esquerda / Direita)
- **Sentido Correto dos Condutores:** Reespelha os blocos para que os ganchos de condutores (neutro/terra) apontem sempre para a **ESQUERDA**.
- **Critério Rígido de Ordem:** Preserva rigorosamente a ordem física dos circuitos:
  - O circuito da **posição 1** permanece na **posição 1 (à esquerda)**.
  - O circuito da **posição 2** permanece na **posição 2 (no meio)**.
  - O circuito da **posição 3** permanece na **posição 3 (à direita)**.
- **Distância Exata de 0.1250:** Mantém o vão livre milimétrico de **`0.1250`** entre cada bloco de condutores adjacente.
- **Alinhamento dos Textos:** Cada texto de circuito fica perfeitamente centralizado acima do seu respectivo bloco de condutores.

---

## 🛠️ Comandos Disponíveis

| Comando | Sinônimo | Descrição |
| :--- | :--- | :--- |
| **`EE`** | `EMIRROR` | **Espelho Inteligente Automático:** Selecione os objetos e a linha de espelho para espelhar e corrigir tudo de uma só vez. |
| **`DV`** | `DESVIRAR` | **Desvirar Seleção Existente:** Corrige elementos de um ambiente que já foi espelhado com o comando comum do AutoCAD. |
| **`DF`** | `DESVIRARFIA` | **Ajuste Direto de Fiação:** Corrige apenas a fiação e textos selecionados em espelhos horizontais. |
| **`TF`** | `TROCARFIA` | **Alternar Dois Circuitos:** Clique em dois textos de circuito para inverter a posição de ambos manualmente. |
| **`CFE`** | `CFGELET` | **Adicionar Bloco Protegido:** Clique em qualquer bloco na tela para adicioná-lo à lista de símbolos protegidos. |
| **`INSP`** | `INFO` | **Inspetor de Entidade:** Clique em qualquer elemento na tela para verificar Nome de Bloco, Camada (Layer) e Conteúdo. |

---

## 📦 Instalação e Carregamento no AutoCAD

### Opção 1: Carregamento Direto no Console
Cole a linha abaixo no console do AutoCAD e aperte `Enter`:

```lisp
(load "espelhar_eletrica.lsp")
```
*(ou forneça o caminho completo do arquivo).*

### Opção 2: Carregamento pelo `APPLOAD`
1. No AutoCAD, digite o comando `APPLOAD` e aperte `Enter`.
2. Localize e selecione o arquivo `espelhar_eletrica.lsp`.
3. Clique em **Load** (ou adicione ao **Startup Suite** para carregar automaticamente em todos os desenhos).

---

## 📂 Estrutura do Repositório

```text
├── espelhar_eletrica.lsp   # Código-fonte principal em AutoLISP
├── .gitignore              # Ignora arquivos temporários do AutoCAD (*.bak, *.dwl)
├── LICENSE                 # Licença MIT
└── README.md               # Documentação técnica do projeto
```

---

## 📄 Licença

Distribuído sob a licença [MIT](LICENSE). Desenvolvido por **tallestreng-dev**.
