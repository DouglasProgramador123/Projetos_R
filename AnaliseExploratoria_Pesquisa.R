library(readxl)
library("ggplot2")
library(tidyverse)
library(dplyr)

##colocar seu path do arquivo xlsx

path <- "C:\\Trabalhos\\pesquisa.xlsx"
alunospesquisa <- read_excel(path)
View(alunospesquisa)

dados <- data.frame(alunospesquisa)

##foi criado conjuntos de turmas de acordo com suas caracteristicas a serem analisadas
turmas <-c(unique(dados$TURMA))
manha <-c(turmas[substr(turmas, 4, 4) > 4])
noite <-c(turmas[substr(turmas, 4, 4) < 5])
segunda <-c(turmas[substr(turmas, 3, 3) == 2])
terca <-c(turmas[substr(turmas, 3, 3) == 3])
quarta <-c(turmas[substr(turmas, 3, 3) == 4])
quinta <-c(turmas[substr(turmas, 3, 3) == 5])
sexta <-c(turmas[substr(turmas, 3, 3) == 6])
ultimotempo <-c(turmas[substr(turmas, 4, 4) == 3])

##foi criada uuma funcao para manipular os dados em um dataframe

preferencia <- function(pesq, manha, noite, segunda, terca, quarta, quinta, sexta, ultimotempo)
{
  ##foi criado uma funcao para "filtrar" de acordo com a entrada
  
  filtro <- function(turma=NULL, sexonum=NULL)
  {
    if (is.null(turma) && is.null(sexonum)) {
      return(c(nrow(filter(pesq, PESQ1 == "P")),
               nrow(filter(pesq, PESQ1 == "R")),
               nrow(filter(pesq, PESQ1 == "H"))))
    } else if (is.null(turma)) {
      return(c(nrow(filter(pesq, SEXO==sexonum, PESQ1 == "P")),
               nrow(filter(pesq, SEXO==sexonum, PESQ1 == "R")),
               nrow(filter(pesq, SEXO==sexonum, PESQ1 == "H"))))
    } else if (is.null(sexonum)) {
      return(c(nrow(filter(pesq, TURMA %in%turma, PESQ1 == "P")),
               nrow(filter(pesq, TURMA %in%turma, PESQ1 == "R")),
               nrow(filter(pesq, TURMA %in%turma, PESQ1 == "H"))))
    } else
      return(c(nrow(filter(pesq, TURMA %in%turma, SEXO==sexonum, PESQ1 == "P")),
               nrow(filter(pesq, TURMA %in%turma, SEXO==sexonum, PESQ1 == "R")),
               nrow(filter(pesq, TURMA %in%turma, SEXO==sexonum, PESQ1 == "H"))))
  }
  
  return(data.frame(pesquisa=c("P", "R", "H"),
                    total=filtro(),
                    manha=filtro(manha),
                    noite=filtro(noite),
                    sexomasculino=filtro(NULL,1),
                    sexofeminino=filtro(NULL,2),
                    sexomasculinomanha=filtro(manha, 1),
                    sexofemininomanha=filtro(manha, 2),
                    sexomasculinonoite=filtro(noite, 1),
                    sexofemininonoite=filtro(noite, 2),
                    turmasegunda=filtro(segunda),
                    turmaterca=filtro(terca),
                    turmaquarta=filtro(quarta),
                    turmaquinta=filtro(quinta),
                    turmasexta=filtro(sexta),
                    turmaultimotempo=filtro(ultimotempo)))
}

preferenciatabel<-preferencia(dados, manha, noite, segunda, terca, quarta, quinta, sexta, ultimotempo)
View(preferenciatabel)

##funcao de barplot de cada coluna da pesquisa

bargraph<-function(titulo, graph, ylimmin, ylimmax, ynum, labelsnum)
{
  return(text(x = bar<-barplot(graph,
                               main = titulo,
                               xlab = "Preferência",
                               ylab = "Total de alunos",
                               ylim = c(ylimmin,ylimmax),
                               names.arg = c("Presencial", "Remoto", "Híbrido")), 
              y = graph + ynum,
              labels= round(graph,labelsnum)))
}

##geracao dos graficos da tabela preferencia

graphtotal <-c(preferenciatabel$total)
titulo = "Todos os alunos"
bargraph(titulo, graphtotal, 0, 200, 10, 2)

graphmanha <-c(preferenciatabel$manha)
titulo = "Todos os alunos - Turno da manha"
bargraph(titulo, graphmanha, 0, 71, 5, 2)

graphnoite <-c(preferenciatabel$noite)
titulo = "Todos os alunos - Turno da noite"
bargraph(titulo, graphnoite, 0, 100, 5, 2)

graphsexomasculino <-c(preferenciatabel$sexomasculino)
titulo = "Todos os alunos - sexo masculino"
bargraph(titulo, graphsexomasculino, 0, 160, 8, 2)

graphsexofeminino <-c(preferenciatabel$sexofeminino)
titulo = "Todos os alunos - sexo feminino"
bargraph(titulo, graphsexofeminino, 0, 40, 3, 2)

graphsexomasculinomanha <-c(preferenciatabel$sexomasculinomanha)
titulo = "Todos os alunos - sexo masculino - Turno da manhã"
bargraph(titulo, graphsexomasculinomanha, 0, 60, 3, 2)

graphsexofemininomanha <-c(preferenciatabel$sexofemininomanha)
titulo = "Todos os alunos - sexo feminino - Turno da manhã"
bargraph(titulo, graphsexofemininomanha, 0, 13, 0.7, 2)

graphsexomasculinonoite <-c(preferenciatabel$sexomasculinonoite)
titulo = "Todos os alunos - sexo masculino - Turno da noite"
bargraph(titulo, graphsexomasculinonoite, 0, 80, 5, 2)

graphsexofemininonoite <-c(preferenciatabel$sexofemininonoite)
titulo = "Todos os alunos - sexo feminino - Turno da noite"
bargraph(titulo, graphsexofemininonoite, 0, 26, 1.2, 2)

graphturmasegunda <-c(preferenciatabel$turmasegunda)
titulo = "Todos os alunos - turma da segunda"
bargraph(titulo, graphturmasegunda, 0, 70, 5, 2)

graphturmaterca <-c(preferenciatabel$turmaterca)
titulo = "Todos os alunos - turma da terca"
bargraph(titulo, graphturmaterca, 0, 40, 3, 2)

graphturmaquarta <-c(preferenciatabel$turmaquarta)
titulo = "Todos os alunos - turma da quarta"
bargraph(titulo, graphturmaquarta, 0, 50, 3, 2)

graphturmaquinta <-c(preferenciatabel$turmaquinta)
titulo = "Todos os alunos - turma da quinta"
bargraph(titulo, graphturmaquinta, 0, 200, 10, 2)

graphturmasexta <-c(preferenciatabel$turmasexta)
titulo = "Todos os alunos - turma da sexta"
bargraph(titulo, graphturmasexta, 0, 30, 2, 2)

##bom analisar o ultimo tempo da noite se a maioria preferiria remoto
graphturmaultimotempo <-c(preferenciatabel$turmaultimotempo)
titulo = "Todos os alunos - ultimo tempo da noite"
bargraph(titulo, graphturmaultimotempo, 0, 55, 3, 2)