[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/X15MPpfH)
# Trabalho prático de BCC328 - Construção de Compiladores I

## Instruções

- Construção do container (Primeiro acesso)
```
docker-compose up -d --build 
docker-compose exec sl bash
```

- Iniciando container (Acessos Posteriores)
```
docker-compose up -d
docker-compose exec sl bash
```

- Para preparar os arquivos usando CABAL

```
cabal build 
```

```
cabal run bcc328 <Caminho_Arquivo_Teste> opcional -> [lexer | parser | pretty | check | interp]
```

# Exemplos dados pelo professor <caminhos possíveis>

```
test/example1_factorial.exp
test/example2_people.exp
test/example3_reverse.exp
test/example4_bmi.exp
test/example5_id.exp
test/example6_map.exp
```

- Exemplo
```
cabal run bcc328 -- test/example1_factorial.exp
cabal run bcc328 -- test/example1_factorial.exp --lexer
cabal run bcc328 -- test/example1_factorial.exp --check
```

- Preparando os arquivos para testar isoladamente Alex e Happy
```
alex src/Exp/Frontend/Lexer/Alex/ExpLexer.x
happy src/Exp/Frontend/Parser/Happy/ExpParser.y --ghc
```

- Para rodar o compilador forma génerica
```
runghc -isrc app/Main.hs <Caminho_Arquivo_Teste> opcional -> [lexer | parser | pretty] 
```

- Exemplo
```
runghc -isrc app/Main.hs test/example1_factorial.exp
runghc -isrc app/Main.hs test/example1_factorial.exp --lexer
```

# Exemplos dados pelo professor <caminhos possíveis>

```
test/example1_factorial.exp
test/example2_people.exp
test/example3_reverse.exp
test/example4_bmi.exp
test/example5_id.exp
test/example6_map.exp
```
