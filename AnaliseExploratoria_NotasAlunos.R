
install.packages("tidyverse",dependencies = TRUE)
install.packages("outliers",dependencies = TRUE)
install.packages("ggplot",dependencies = TRUE)
install.packages("plyr",dependencies = TRUE)

library(readxl)
library("ggplot")
library("ggplot2")
library("outliers")
library("plyr")
library(tidyverse)
library(readr)

#Importe os dados da planilha "Taxas - Escolas 2010" hospedada em 
#https://dados.gov.br/dataset/media-de-alunos-por-turma-na-educacao-basica

path <- "C:\\Trabalho\\escolas_media_alunos_turma_2010.xls"

pnNorte <- read_excel(path,sheet = "NORTE", na = "--", skip = 8)
pnNordesteExt <- read_excel(path,sheet = "NORDESTE-EXT MA E BA", na = "--", skip = 8)
pnNordesteSom <- read_excel(path,sheet = "NORDESTE - SOMENTE MA E BA", na = "--", skip = 8)
pnSudeste <- read_excel(path,sheet = "SUDESTE", na = "--", skip = 8)
pnSul <- read_excel(path,sheet = "SUL", na = "--", skip = 8)
pnCentro <- read_excel(path,sheet = "CENTRO-OESTE", na = "--", skip = 8)

View(pnNorte)
View(pnNordesteExt)
View(pnNordesteSom)
View(pnSudeste)
View(pnSul)
View(pnCentro)

norte <-data.frame(PrimeiroAno=c(pnNorte$`1º Ano`),
                   SegundoAno=c(pnNorte$`1ª série/ 2° ano`),
                   TerceiroAno=c(pnNorte$`2ª série/ 3° ano`),
                   QuartoAno=c(pnNorte$`3ª série/ 4° ano`),
                   QuintoAno=c(pnNorte$`4ª série/ 5° ano`),
                   SextoAno=c(pnNorte$`5ª série/ 6° ano`),
                   SetimoAno=c(pnNorte$`6ª série/ 7° ano`),
                   OitavoAno=c(pnNorte$`7ª série/ 8° ano`),
                   NonoAno=c(pnNorte$`8ª série/ 9° ano`))

nordesteExt <-data.frame(PrimeiroAno=c(pnNordesteExt$`1º Ano`),
                         SegundoAno=c(pnNordesteExt$`1ª série/ 2° ano`),
                         TerceiroAno=c(pnNordesteExt$`2ª série/ 3° ano`),
                         QuartoAno=c(pnNordesteExt$`3ª série/ 4° ano`),
                         QuintoAno=c(pnNordesteExt$`4ª série/ 5° ano`),
                         SextoAno=c(pnNordesteExt$`5ª série/ 6° ano`),
                         SetimoAno=c(pnNordesteExt$`6ª série/ 7° ano`),
                         OitavoAno=c(pnNordesteExt$`7ª série/ 8° ano`),
                         NonoAno=c(pnNordesteExt$`8ª série/ 9° ano`))

nordesteSom <-data.frame(PrimeiroAno=c(pnNordesteSom$`1º Ano`),
                         SegundoAno=c(pnNordesteSom$`1ª série/ 2° ano`),
                         TerceiroAno=c(pnNordesteSom$`2ª série/ 3° ano`),
                         QuartoAno=c(pnNordesteSom$`3ª série/ 4° ano`),
                         QuintoAno=c(pnNordesteSom$`4ª série/ 5° ano`),
                         SextoAno=c(pnNordesteSom$`5ª série/ 6° ano`),
                         SetimoAno=c(pnNordesteSom$`6ª série/ 7° ano`),
                         OitavoAno=c(pnNordesteSom$`7ª série/ 8° ano`),
                         NonoAno=c(pnNordesteSom$`8ª série/ 9° ano`))

sudeste <-data.frame(PrimeiroAno=c(pnSudeste$`1º Ano`),
                     SegundoAno=c(pnSudeste$`1ª série/ 2° ano`),
                     TerceiroAno=c(pnSudeste$`2ª série/ 3° ano`),
                     QuartoAno=c(pnSudeste$`3ª série/ 4° ano`),
                     QuintoAno=c(pnSudeste$`4ª série/ 5° ano`),
                     SextoAno=c(pnSudeste$`5ª série/ 6° ano`),
                     SetimoAno=c(pnSudeste$`6ª série/ 7° ano`),
                     OitavoAno=c(pnSudeste$`7ª série/ 8° ano`),
                     NonoAno=c(pnSudeste$`8ª série/ 9° ano`))

sul <-data.frame(PrimeiroAno=c(pnSul$`1º Ano`),
                 SegundoAno=c(pnSul$`1ª série/ 2° ano`),
                 TerceiroAno=c(pnSul$`2ª série/ 3° ano`),
                 QuartoAno=c(pnSul$`3ª série/ 4° ano`),
                 QuintoAno=c(pnSul$`4ª série/ 5° ano`),
                 SextoAno=c(pnSul$`5ª série/ 6° ano`),
                 SetimoAno=c(pnSul$`6ª série/ 7° ano`),
                 OitavoAno=c(pnSul$`7ª série/ 8° ano`),
                 NonoAno=c(pnSul$`8ª série/ 9° ano`))

centro <-data.frame(PrimeiroAno=c(pnCentro$`1º Ano`),
                    SegundoAno=c(pnCentro$`1ª série/ 2° ano`),
                    TerceiroAno=c(pnCentro$`2ª série/ 3° ano`),
                    QuartoAno=c(pnCentro$`3ª série/ 4° ano`),
                    QuintoAno=c(pnCentro$`4ª série/ 5° ano`),
                    SextoAno=c(pnCentro$`5ª série/ 6° ano`),
                    SetimoAno=c(pnCentro$`6ª série/ 7° ano`),
                    OitavoAno=c(pnCentro$`7ª série/ 8° ano`),
                    NonoAno=c(pnCentro$`8ª série/ 9° ano`))

#foi criada uma forma de acessar cada regiao atraves de uma variavel
regiao <- norte
medNortePrimeiro <- mean(regiao$PrimeiroAno, na.rm = TRUE)
medNorteSegundo <- mean(regiao$SegundoAno, na.rm = TRUE)
medNorteTerceiro <- mean(regiao$TerceiroAno, na.rm = TRUE)
medNorteQuarto <- mean(regiao$QuartoAno, na.rm = TRUE)
medNorteQuinto <- mean(regiao$QuintoAno, na.rm = TRUE)
medNorteSexto <- mean(regiao$SextoAno, na.rm = TRUE)
medNorteSetimo <- mean(regiao$SetimoAno, na.rm = TRUE)
medNorteOitavo <- mean(regiao$OitavoAno, na.rm = TRUE)
medNorteNono <- mean(regiao$NonoAno, na.rm = TRUE)

regiao <-nordesteExt
medNordesteExtPrimeiro <- mean(regiao$PrimeiroAno, na.rm = TRUE) 
medNordesteExtSegundo <- mean(regiao$SegundoAno, na.rm = TRUE)
medNordesteExtTerceiro <- mean(regiao$TerceiroAno, na.rm = TRUE)
medNordesteExtQuarto <- mean(regiao$QuartoAno, na.rm = TRUE)
medNordesteExtQuinto <- mean(regiao$QuintoAno, na.rm = TRUE)
medNordesteExtSexto <- mean(regiao$SextoAno, na.rm = TRUE)
medNordesteExtSetimo <- mean(regiao$SetimoAno, na.rm = TRUE)
medNordesteExtOitavo <- mean(regiao$OitavoAno, na.rm = TRUE)
medNordesteExtNono <- mean(regiao$NonoAno, na.rm = TRUE)

regiao <- nordesteSom
medNordesteSomPrimeiro <- mean(regiao$PrimeiroAno, na.rm = TRUE) 
medNordesteSomSegundo <- mean(regiao$SegundoAno, na.rm = TRUE)
medNordesteSomTerceiro <- mean(regiao$TerceiroAno, na.rm = TRUE)
medNordesteSomQuarto <- mean(regiao$QuartoAno, na.rm = TRUE)
medNordesteSomQuinto <- mean(regiao$QuintoAno, na.rm = TRUE)
medNordesteSomSexto <- mean(regiao$SextoAno, na.rm = TRUE)
medNordesteSomSetimo <- mean(regiao$SetimoAno, na.rm = TRUE)
medNordesteSomOitavo <- mean(regiao$OitavoAno, na.rm = TRUE)
medNordesteSomNono <- mean(regiao$NonoAno, na.rm = TRUE)

regiao <- sudeste
medSudestePrimeiro <- mean(regiao$PrimeiroAno, na.rm = TRUE) 
medSudesteSegundo <- mean(regiao$SegundoAno, na.rm = TRUE)
medSudesteTerceiro <- mean(regiao$TerceiroAno, na.rm = TRUE)
medSudesteQuarto <- mean(regiao$QuartoAno, na.rm = TRUE)
medSudesteQuinto <- mean(regiao$QuintoAno, na.rm = TRUE)
medSudesteSexto <- mean(regiao$SextoAno, na.rm = TRUE)
medSudesteSetimo <- mean(regiao$SetimoAno, na.rm = TRUE)
medSudesteOitavo <- mean(regiao$OitavoAno, na.rm = TRUE)
medSudesteNono <- mean(regiao$NonoAno, na.rm = TRUE)

regiao <- sul
medSulPrimeiro <- mean(regiao$PrimeiroAno, na.rm = TRUE) 
medSulSegundo <- mean(regiao$SegundoAno, na.rm = TRUE)
medSulTerceiro <- mean(regiao$TerceiroAno, na.rm = TRUE)
medSulQuarto <- mean(regiao$QuartoAno, na.rm = TRUE)
medSulQuinto <- mean(regiao$QuintoAno, na.rm = TRUE)
medSulSexto <- mean(regiao$SextoAno, na.rm = TRUE)
medSulSetimo <- mean(regiao$SetimoAno, na.rm = TRUE)
medSulOitavo <- mean(regiao$OitavoAno, na.rm = TRUE)
medSulNono <- mean(regiao$NonoAno, na.rm = TRUE)

regiao <- centro 
medCentroPrimeiro <- mean(regiao$PrimeiroAno, na.rm = TRUE) 
medCentroSegundo <- mean(regiao$SegundoAno, na.rm = TRUE)
medCentroTerceiro <- mean(regiao$TerceiroAno, na.rm = TRUE)
medCentroQuarto <- mean(regiao$QuartoAno, na.rm = TRUE)
medCentroQuinto <- mean(regiao$QuintoAno, na.rm = TRUE)
medCentroSexto <- mean(regiao$SextoAno, na.rm = TRUE)
medCentroSetimo <- mean(regiao$SetimoAno, na.rm = TRUE)
medCentroOitavo <- mean(regiao$OitavoAno, na.rm = TRUE)
medCentroNono <- mean(regiao$NonoAno, na.rm = TRUE)

#Funcao para grafico de barras

grafico_mediaRegiao <- function(medRegiao, nomePlot, ylimMin= 0, ylimMax= 30){
  grafico <- c(medRegiao[[1]], medRegiao[[2]], medRegiao[[3]], medRegiao[[4]], medRegiao[[5]],
               medRegiao[[6]], medRegiao[[7]], medRegiao[[8]], medRegiao[[9]])
  series <- c("1º Ano", "2º Ano", "3º Ano", "4º Ano", "5º Ano", "6º Ano", "7º Ano","8º Ano","9º Ano")
  bar <- barplot(grafico,
                 main = nomePlot,
                 xlab = "Série",
                 ylab = "Média",
                 ylim =c(ylimMin,ylimMax),
                 names.arg = series) 
  text(x = bar, y = grafico+1, labels = round(grafico,2))
}

#Criacao dos graficos de barras

medNorte <- list(medNortePrimeiro,medNorteSegundo,medNorteTerceiro,medNorteQuarto,medNorteQuinto,
                 medNorteSexto,medNorteSetimo,medNorteOitavo,medNorteNono)
medRegiao <- medNorte
nomePlot <- "Média de Alunos - Região Norte"
grafico_mediaRegiao(medRegiao, nomePlot)

medNordesteExt <- list(medNordesteExtPrimeiro,medNordesteExtSegundo,medNordesteExtTerceiro,
                       medNordesteExtQuarto,medNordesteExtQuinto,medNordesteExtSexto,medNordesteExtSetimo,
                       medNordesteExtOitavo,medNordesteExtNono)
medRegiao <- medNordesteExt
nomePlot <- "Média de Alunos - Região Nordeste (exceto Maranhão e Bahia)"
grafico_mediaRegiao(medRegiao, nomePlot)

medNordesteSom <- list(medNordesteSomPrimeiro,medNordesteSomSegundo,medNordesteSomTerceiro,
                       medNordesteSomQuarto,medNordesteSomQuinto,medNordesteSomSexto,
                       medNordesteSomSetimo,medNordesteSomOitavo,medNordesteSomNono)
medRegiao <- medNordesteSom
nomePlot <- "Média de Alunos - Região Nordeste (somente Maranhão e Bahia)"
grafico_mediaRegiao(medRegiao, nomePlot)

medSudeste <- list(medSudestePrimeiro,medSudesteSegundo,medSudesteTerceiro,medSudesteQuarto,
                   medSudesteQuinto,medSudesteSexto,medSudesteSetimo,medSudesteOitavo,medSudesteNono)
medRegiao <- medSudeste
nomePlot <- "Média de Alunos - Região Sudeste"
grafico_mediaRegiao(medRegiao, nomePlot)

medSul <- list(medSulPrimeiro,medSulSegundo,medSulTerceiro,medSulQuarto,medSulQuinto,medSulSexto,
               medSulSetimo,medSulOitavo,medSulNono)
medRegiao <- medSul
nomePlot <- "Média de Alunos - Região Sul"
grafico_mediaRegiao(medRegiao, nomePlot)

medCentro <- list(medCentroPrimeiro,medCentroSegundo,medCentroTerceiro,medCentroQuarto,medCentroQuinto,
                  medCentroSexto,medCentroSetimo,medCentroOitavo,medCentroNono)
medRegiao <- medCentro
nomePlot <- "Média de Alunos - Região Centro Oeste"
grafico_mediaRegiao(medRegiao, nomePlot)

#analise exploratoria

#foi criada uma funcao para reduzir a quantidade de codigo e facilitar a criacao da analise de cada regiao

analise_Regiao <- function(regiao){
  return(data.frame(Media = c(summary(regiao$PrimeiroAno)[[4]],summary(regiao$SegundoAno)[[4]],
                              summary(regiao$TerceiroAno)[[4]],summary(regiao$QuartoAno)[[4]],
                              summary(regiao$QuintoAno)[[4]],summary(regiao$SextoAno)[[4]],
                              summary(regiao$SetimoAno)[[4]],summary(regiao$OitavoAno)[[4]],
                              summary(regiao$NonoAno)[[4]]),
                    Mediana = c(summary(regiao$PrimeiroAno)[[3]],summary(regiao$SegundoAno)[[3]],
                                summary(regiao$TerceiroAno)[[3]],summary(regiao$QuartoAno)[[3]],
                                summary(regiao$QuintoAno)[[3]],summary(regiao$SextoAno)[[3]],
                                summary(regiao$SetimoAno)[[3]],summary(regiao$OitavoAno)[[3]],
                                summary(regiao$NonoAno)[[3]]),
                    Minimo = c(summary(regiao$PrimeiroAno)[[1]],summary(regiao$SegundoAno)[[1]],
                               summary(regiao$TerceiroAno)[[1]],summary(regiao$QuartoAno)[[1]],
                               summary(regiao$QuintoAno)[[1]],summary(regiao$SextoAno)[[1]],
                               summary(regiao$SetimoAno)[[1]],summary(regiao$OitavoAno)[[1]],
                               summary(regiao$NonoAno)[[1]]),
                    Maximo = c(summary(regiao$PrimeiroAno)[[6]],summary(regiao$SegundoAno)[[6]],
                               summary(regiao$TerceiroAno)[[6]],summary(regiao$QuartoAno)[[6]],
                               summary(regiao$QuintoAno)[[6]],summary(regiao$SextoAno)[[6]],
                               summary(regiao$SetimoAno)[[6]],summary(regiao$OitavoAno)[[6]],
                               summary(regiao$NonoAno)[[6]]),
                    PrimeiroQuartil = c(quantile(regiao$PrimeiroAno, na.rm = TRUE)[[2]],quantile(regiao$SegundoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$TerceiroAno, na.rm = TRUE)[[2]],quantile(regiao$QuartoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$QuintoAno, na.rm = TRUE)[[2]],quantile(regiao$SextoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$SetimoAno, na.rm = TRUE)[[2]],quantile(regiao$OitavoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$NonoAno, na.rm = TRUE)[[2]]),
                    SegundoQuartil = c(quantile(regiao$PrimeiroAno, na.rm = TRUE)[[3]],quantile(regiao$SegundoAno, na.rm = TRUE)[[3]],
                                       quantile(regiao$TerceiroAno, na.rm = TRUE)[[3]],quantile(regiao$QuartoAno, na.rm = TRUE)[[3]],
                                       quantile(regiao$QuintoAno, na.rm = TRUE)[[3]],quantile(regiao$SextoAno, na.rm = TRUE)[[3]],
                                       quantile(regiao$SetimoAno, na.rm = TRUE)[[3]],quantile(regiao$OitavoAno, na.rm = TRUE)[[3]],
                                       quantile(regiao$NonoAno, na.rm = TRUE)[[3]]),
                    TerceiroQuartil = c(quantile(regiao$PrimeiroAno, na.rm = TRUE)[[4]],quantile(regiao$SegundoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$TerceiroAno, na.rm = TRUE)[[4]],quantile(regiao$QuartoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$QuintoAno, na.rm = TRUE)[[4]],quantile(regiao$SextoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$SetimoAno, na.rm = TRUE)[[4]],quantile(regiao$OitavoAno, na.rm = TRUE)[[4]],
                                        quantile(regiao$NonoAno, na.rm = TRUE)[[4]]),
                    QuartoQuartil = c(quantile(regiao$PrimeiroAno, na.rm = TRUE)[[5]],quantile(regiao$SegundoAno, na.rm = TRUE)[[5]],
                                      quantile(regiao$TerceiroAno, na.rm = TRUE)[[5]],quantile(regiao$QuartoAno, na.rm = TRUE)[[5]],
                                      quantile(regiao$QuintoAno, na.rm = TRUE)[[5]],quantile(regiao$SextoAno, na.rm = TRUE)[[5]],
                                      quantile(regiao$SetimoAno, na.rm = TRUE)[[5]],quantile(regiao$OitavoAno, na.rm = TRUE)[[5]],
                                      quantile(regiao$NonoAno, na.rm = TRUE)[[5]]),
                    DesvioPadrao = c(sd(regiao$PrimeiroAno, na.rm = TRUE),sd(regiao$SegundoAno, na.rm = TRUE),
                                     sd(regiao$TerceiroAno, na.rm = TRUE),sd(regiao$QuartoAno, na.rm = TRUE),
                                     sd(regiao$QuintoAno, na.rm = TRUE),sd(regiao$SextoAno, na.rm = TRUE),
                                     sd(regiao$SetimoAno, na.rm = TRUE),sd(regiao$OitavoAno, na.rm = TRUE),
                                     sd(regiao$NonoAno, na.rm = TRUE)),
                    Outliers = c(length(regiao$PrimeiroAno[which(regiao$PrimeiroAno %in% c(boxplot.stats(regiao$PrimeiroAno)$out))]),
                                 length(regiao$SegundoAno[which(regiao$SegundoAno %in% c(boxplot.stats(regiao$SegundoAno)$out))]),
                                 length(regiao$TerceiroAno[which(regiao$TerceiroAno %in% c(boxplot.stats(regiao$TerceiroAno)$out))]),
                                 length(regiao$QuartoAno[which(regiao$QuartoAno %in% c(boxplot.stats(regiao$QuartoAno)$out))]),
                                 length(regiao$QuintoAno[which(regiao$QuintoAno %in% c(boxplot.stats(regiao$QuintoAno)$out))]),
                                 length(regiao$SextoAno[which(regiao$SextoAno %in% c(boxplot.stats(regiao$SextoAno)$out))]),
                                 length(regiao$SetimoAno[which(regiao$SetimoAno %in% c(boxplot.stats(regiao$SetimoAno)$out))]),
                                 length(regiao$OitavoAno[which(regiao$OitavoAno %in% c(boxplot.stats(regiao$OitavoAno)$out))]),
                                 length(regiao$NonoAno[which(regiao$NonoAno %in% c(boxplot.stats(regiao$NonoAno)$out))]))))
}

analise_Norte <- analise_Regiao(norte)
view(analise_Norte)

analise_nordesteExt <- analise_Regiao(nordesteExt)
view(analise_nordesteExt)

analise_nordesteSom <- analise_Regiao(nordesteSom)
view(analise_nordesteSom)

analise_sudeste <- analise_Regiao(sudeste)
view(analise_sudeste)

analise_Sul <- analise_Regiao(sul)
view(analise_Sul)

analise_centro <- analise_Regiao(centro)
view(analise_centro)

#funcao para analisar todos os outliers unicos de cada regiao

outliers <- function(regiao_text){
  if(regiao_text == "norte"){
    pn_regiao <- pnNorte
    regiao <- norte
  } else if(regiao_text == "nordesteExt"){
    pn_regiao <- pnNordesteExt
    regiao <- nordesteExt
  } else if(regiao_text == "nordesteSom"){
    pn_regiao <- pnNordesteSom
    regiao <- nordesteSom
  } else if(regiao_text == "sudeste"){
    pn_regiao <- pnSudeste
    regiao <- sudeste
  } else if(regiao_text == "sul"){
    pn_regiao <- pnSul
    regiao <- sul
  } else if(regiao_text == "centro"){
    pn_regiao <- pnCentro
    regiao <- centro
  }
  
  outlier_lista <- c(which(regiao$PrimeiroAno %in% c(boxplot.stats(regiao$PrimeiroAno)$out)))
  outliers_primeiro = pn_regiao %>% filter(`1º Ano` %in% regiao$PrimeiroAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$SegundoAno %in% c(boxplot.stats(regiao$SegundoAno)$out)))
  outliers_segundo = pn_regiao %>% filter(`1ª série/ 2° ano` %in% regiao$SegundoAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$TerceiroAno %in% c(boxplot.stats(regiao$TerceiroAno)$out)))
  outliers_terceiro = pn_regiao %>% filter(`2ª série/ 3° ano` %in% regiao$TerceiroAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$QuartoAno %in% c(boxplot.stats(regiao$QuartoAno)$out)))
  outliers_quarto = pn_regiao %>% filter(`3ª série/ 4° ano` %in% regiao$QuartoAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$QuintoAno %in% c(boxplot.stats(regiao$QuintoAno)$out)))
  outliers_quinto = pn_regiao %>% filter(`4ª série/ 5° ano` %in% regiao$QuintoAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$SextoAno %in% c(boxplot.stats(regiao$SextoAno)$out)))
  outliers_sexto = pn_regiao %>% filter(`5ª série/ 6° ano` %in% regiao$SextoAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$SetimoAno %in% c(boxplot.stats(regiao$SetimoAno)$out)))
  outliers_setimo = pn_regiao %>% filter(`6ª série/ 7° ano` %in% regiao$SetimoAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$OitavoAno %in% c(boxplot.stats(regiao$OitavoAno)$out)))
  outliers_oitavo = pn_regiao %>% filter(`7ª série/ 8° ano` %in% regiao$OitavoAno[outlier_lista])
  
  outlier_lista <- c(which(regiao$NonoAno %in% c(boxplot.stats(regiao$NonoAno)$out)))
  outliers_nono = pn_regiao %>% filter(`8ª série/ 9° ano` %in% regiao$NonoAno[outlier_lista])
  
  return (rbind(outliers_primeiro, outliers_segundo, outliers_terceiro, outliers_quarto, outliers_quinto,
                outliers_sexto, outliers_setimo, outliers_oitavo, outliers_nono))
}

outlier_norte <- unique(outliers("norte"))
view(outlier_norte)

outlier_nordesteExt <- unique(outliers("nordesteExt"))
view(outlier_nordesteExt)

outlier_nordesteSom <- unique(outliers("nordesteSom"))
view(outlier_nordesteSom)

outlier_sudeste <- unique(outliers("sudeste"))
view(outlier_sudeste)

outlier_Sul <- unique(outliers("sul"))
view(outlier_Sul)

outlier_centro <- unique(outliers("centro"))
view(outlier_centro)

#foi criada uma forma de exportar os outliers para arquivo csv a ser analisado

csv_path <- "C:\\Trabalho"
write.table(outlier_norte, file = paste(csv_path,"\\Outliers_NORTE.csv",sep=""), col.names=FALSE,sep=",")
write.table(outlier_nordesteSom, file = paste(csv_path,"\\Outliers_NORDESTE-EXT MA E BA.csv",sep=""), col.names=FALSE,sep=",")
write.table(outlier_nordesteExt, file = paste(csv_path,"\\Outliers_NORDESTE-SOMENTE MA E BA.csv",sep=""), col.names=FALSE,sep=",")
write.table(outlier_sudeste, file = paste(csv_path,"\\Outliers_SUDESTE.csv",sep=""), col.names=FALSE,sep=",")
write.table(outlier_Sul, file = paste(csv_path,"\\Outliers_SUL.csv",sep=""), col.names=FALSE,sep=",")
write.table(outlier_centro, file = paste(csv_path,"\\Outliers_CENTRO-OESTE.csv",sep=""), col.names=FALSE,sep=",")
