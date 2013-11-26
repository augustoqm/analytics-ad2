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

