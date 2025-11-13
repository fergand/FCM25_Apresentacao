Boas práticas de colaboração usando Git

# Criação de branch pessoal

Para evitar conflitos de `push/pull`, cada colaborador deve criar sua própria branch:

```bash
git checkout -b seu-nome
```

Por exemplo: git checkout -b elias


# Criação de scripts pessoal

Cada colaborar deverá criar um arquivo .R com o seu nome:

* fernando.R
* elias.R
* gabriel.R

com esses arquivos, cada um deverá fazer as análises para o qual foi designado a fazer

# Commits

Faça commits com mensagens claras sobre o que foi feito:

```bash

git add analise.R
git commit -m "add: análise exploratória inicial"
git push origin elias
```

# Atualize sua brach antes de integrar com `main`

Antes de integrar seu trabalho ao `main`, utilize:

```bash
git checkout main
git pull origin main
git checkout sua-branch
git merge main
```

Resolva conflitos, se houver:

```bash
git add <arquivo>
git commit -m "fix: resolvendo conflitos"
```

# Integrando no arquivo principal

Quando seu conteúdo estiver finalizado:

1. Coloque sua análise ou texto diretamente em `Apresentação.qmd`
2. Faça o merge da sua branch no `main`:

```bash
git checkout main
git merge sua-branch
git push origin main
```

# Dicas para commits

Segue uma lista de palavras diretas para utilizarmos em commits

* add:      adicionando arquivo/seção/código
* fix:      corrigindo erro
* update:   modificando algo já existente
* refactor: limpando ou reorganizando código
* remove:   deletando algo
* docs:     documentação ou texto
* style:    formatação
* plot:     ajustes em gráficos
* data:     alterações nos dados
* test:     experimentos ou testes