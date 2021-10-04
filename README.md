![Alerta de Fogo](https://github.com/dsa2021/dsa2021.github.io/blob/main/R/projetoqueimadas/fogo.png?raw=true)


# Projeto: Previsão de risco de fogo em Vegetação

## Página do Produto Final

[https://dsa2021.github.io/](https://dsa2021.github.io/)

## Entregas do Projeto

- [x] [Vídeo de Apresentação de nosso trabalho](https://github.com/dsa2021/dsa2021.github.io/blob/main/docs/video_turma47_2021-10-03_16.32.50.mkv)
- [x] [Sumário Executivo em PDF](https://github.com/dsa2021/dsa2021.github.io/blob/main/docs/sumario_executivo.pdf)
- [x] [**Produto**(Relatório Analítico) em R MArkdown publicado na plataforma RPubs](https://rpubs.com/joicerss/815685)  
- [x] [Script em R versionado - projetoqueimadas](https://github.com/dsa2021/dsa2021.github.io/tree/main/R/projetoqueimadas)
- [x] [Fonte de dados utilizada para treinamento e demonstração dos modelos](https://github.com/dsa2021/dsa2021.github.io/blob/main/R/projetoqueimadas/Focos_2021-06-26_2021-06-27.csv)
- [x] [Repositório Git](https://github.com/dsa2021/dsa2021.github.io)

## Participantes:

- Ângela
- Humberto
- João
- Joice
- Petrônio(Mentor)


## Problema a ser resolvido

- Problemática de Queimadas no Brasil, como prever sua ocorrência?
- Fontes abaixo para leitura que ilustram esse problema e alguns exemplos de links com bases de dados atreladas ao tema.  

    1.[Arquivos sobre Queimadas no Brasil - INPE](https://queimadas.dgi.inpe.br/queimadas/dados-abertos/#arquivos)  
    2.[Portal de Queimadas _ INPE](https://queimadas.dgi.inpe.br/queimadas/portal)  
    3.[Notebook de Exemplo](https://queimadas.dgi.inpe.br/queimadas/dados-abertos/exemplos/csv2nc.html)  
    4.[Portal da NASA sobre Wildfires](https://earthdata.nasa.gov/learn/toolkits/wildfires)  
    5.[Dashboard Queimadas no Brasil](http://appcombo.com.br/?import=Queimadas%20no%20Brasil)  
    6.[Base de dados de Queimadas](https://basedosdados.org/dataset/banco-de-dados-de-queimadas) 


## Objeto do Projeto

- A ferramenta Alerta de Fogo, desenvolvida para a previsão de risco de fogo da vegetação, visa orientar a tomada de decisão de gestores das iniciativas pública e privada face ao desafio de promoção do desenvolvimento econômico sustentável.

## Cronograma 

| Etapas                        | Tempo     |
|-------------------------------|-----------|
| Apresentação                  | 2 Semanas |
| Coletivização de conhecimento | 3 Semanas |
| Problematização               | 2 Semanas |
| Análise exploratória          | 4 Semanas |
| Implementação                 | 4 Semanas |
| Finalização                   | 3 Semanas |


#### Apresentação
- Etapa em que os participantes esclareciam seus conhecimentos, sua formação e como poderiam contribuir para o grupo.

#### Coletivização de conhecimento
- Etapa em que foi demonstrada a importância da utilização de versionamento de Código e explanação de ferramentas do Github como: **Github Actions**(para agendar scripts que coletariam dados automaticamente) e **Github Pages**(para hospedar nossa página do produto final).
- Importante a sintonia dos participantes e o conhecimento ofertado a todos como forma de nivelamento do grupo. 
- Destaque para temas discutidos, como: ETL(robô extrator de informações), utilização de API e versionamento de código fonte no Github.
- Alguns artefatos produzidos nesta etapa:
   1. [Extrator de dados de Queimadas agendado via Github Actions](https://github.com/dsa2021/dsa2021.github.io/blob/main/etl/extrator_queimadas.py) e [Coletas do extrator diariamente armazenadas](https://github.com/dsa2021/dsa2021.github.io/tree/main/data)
   2. [Documentação base para iniciantes usarem principais comandos Git](https://github.com/dsa2021/dsa2021.github.io/blob/main/docs/comandos_git.md)
 
 #### Problematização
 - Etapa em que foram discutidos os diversos prismas relativos ao Problema de Queimadas no Brasil e onde foi definido o chamado **Risco Fogo** como recorte do problema que focaríamos no nosso trabalho. 

#### Análise exploratória
- Etapa em que foi escolhida a base de dados do **Projeto/Portal Queimadas do INPE**(link acima) para alguns experimentos exploratórios.
- Para ilustrar as diversas formas de fazer uma análise exploratória, também foi demonstrada a utilização de AutomL ou ferramentas que fazem análises exploratórias de forma mais automática, como forma complementar de adição de conhecimento ao grupo. 
- [Notebook em Python com algumas análises e algumas ferramentas demonstradas](https://github.com/dsa2021/dsa2021.github.io/blob/main/analise/analise_exploratoria_humberto.ipynb)
- No entanto, a melhor análise exploratória escolhida pelo grupo foi feita em Linguagem R com aplicação de regressão linear e seus modelos/resultados subsidiam o Produto Final.

#### Implementação
- Nesta etapa o principal desafio foi integrar o código feito em R, com um visão analítica de contexto(adicionada com o R MArkdown).
- Portanto, foi desafiante persuadir uma implementação una, que noteou o *storytelling* dos dados tratados no nosso problema.

#### Finalização
- Por fim, discutimos os aspectos conclusivos do trabalho, organização das entregas a serem feitas e ressaltamos a importância do conjunto ser versionado e integrado em interface web. 
- Fica como desafio futuro integrar o robô coletor de informações com o modelo de **Machine Learning** para previsão online de queimadas, no momento da coleta automatizada.
- Foi publicado na plataforma **RPubs** uma [versão](https://rpubs.com/joicerss/815685), posteriormente integrado ao nosso repositório, acarretando nossa página do Produto entregue: 


- https://dsa2021.github.io/




