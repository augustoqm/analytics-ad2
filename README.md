analytics-ad2
=============

Repositório de Código para os aplicativos desenvolvidos para a disciplina Análise de Dados 2.

Requisitos
----------
Para executar o aplicativo **ad2_home** é necessário instalar alguns pacotes R:
```R
install.packages(c("shiny", "plyr", "reshape", "ggplot2", "gridExtra", "boot", "ROCR"))
```

Tutorial
--------
Após instalar os pacotes necessários, execute os comandos abaixo e o aplicativo
**ad2_home** será carregado e executado em sua máquina (com um servidor local).

```R
library(shiny)
runGitHub("analytics-ad2", "augustoqm", subdir = "web/ad2_home")
```

Caso deseje executar esse aplicativo *offline* faça o download desse repositório
e no diretório raiz execute os seguintes comandos R:

```R
library(shiny)
runApp("web/ad2_home")
```

Atenção
-------
As versões do aplicativo **ad2_class** também estão disponíveis nesse repositório, 
no entanto, caso execute perceberá que faltam informações pois as submissões dos 
alunos e as bases de dados não são publicadas.