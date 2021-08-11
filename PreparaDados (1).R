rm(list=ls())
memory.limit()
memory.limit(size=3000000000)
######Prepara dados para rodar simulaÃƒÂ§ÃƒÂ£o sem usar shyni. 

#------------------------ imports ------------------------
{
library("readxl")
library("doParallel")
library("stringr")
library("compiler")
library("ggplot2")
library(dplyr)
library(tidyr)
}
#----------------------- carregar funÃƒÂ§ÃƒÂµes -----------------
##Indica diretÃƒÂ³rio onde salvou as funÃƒÂ§ÃƒÂµes
setwd("E:\\Coisas do dropbox\\Dissertação\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)")
{
  source("calculaValoresPagamento.R")
  source("calculaValoresPagamentoAberta.R")
  source("estimaTempoAteMorte.R")
  source("estimaTempoAteSaida.R")
  source("EstimaTx.R")
  source("geraGraficoPercentil.R")
  source("gerarLx.R")
  source("gerarTDU.R")
  source("gerarTMDTDU.R")
  source("idadeTipoAposentadoria.R")
  source("lancarErro.r")
  source("piramide.R")
  source("resumeEstadoServidor.R")
#  source("resumeEstadoServidorAberta.R")
  source("rodaSimulacao.R")
 # source("rodaSimulacaoAberta.R")
  #source("estimaIdadeAposentadoriaRPPS.R")
 # source("estimaIdadeAposentadoriaRGPS.R")
  source("EstimaPopEntrada.R")
  #source("rodaSimulacaoAbertaGrande.R")
  #source("calculaValoresPagamentoAberta")
}



##Define parÃ¢metros
{
  Diretorio="E:\\Coisas do dropbox\\Dissertação\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)"      
  TetoINSS=5645.80   ###Verificar valor
  Tempo=50
  Rodadas=2500  ##Mude 100 para testar, mas os resultados oficiais devem ser com quantidade maior, de pelo menos 2500 rodadas para municÃƒï¿½pios e 1000 para estados.  
  SalMinimo = 954 #estabelece salario minimo de 954
  
  }



##Carrega dados inciais
DiretorioDadosIniciais="E:\\Coisas do dropbox\\Dissertação\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados"
DiretorioGrardaResultados="E:\\Coisas do dropbox\\Dissertação\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Resultados"


{
setwd(DiretorioDadosIniciais)
tabelaInvalidez=read.csv("AlvaroVindas.csv", sep=";", dec=",")
#mun_grupo <-read.csv("municipio_grupomuniciElaine.csv", sep=",")
#mun_grupo = data.frame(Município = unique(mun_grupo$Município), Grupo=1) #Tirar isso
mun_grupo = data.frame(Município = "421223", Grupo=1)
TxSalarial=read.csv("AumentoSalarialPorEnte.csv", sep=";", dec=",")
}


dados = data.frame()

for(i in 1:1) {
  row <- mun_grupo[i,]
  Municipio= toString(row[1])
  #Grupo = toString(row[2])
  
  Dados=read.csv(paste0(DiretorioDadosIniciais, "\\Populacao Inicial\\",Municipio," .csv"), sep=",", dec=".")
  
  Dados=Dados[Dados$EstadoInicial==1,]   ##Para este projeto sÃ³ interessam os ativos
  taxaAumentoSalarial=TxSalarial$taxaAumentoSalarial[TxSalarial$NomeMunicipio==Municipio]
  tabelaMortalidade= read.csv(paste0(DiretorioDadosIniciais,"\\Tabuas\\TabelaVida ",Municipio," .csv"), sep=",", dec=".")
  #df =  rodaSimulacao (taxaAumentoSalarial, DadosServidores, Diretorio, Rodadas, Tempo, tabelaMortalidade, tabelaInvalidez,  Municipio,TetoINSS)
  
  #Categoria = unique(Dados$Categoria)
  Categoria = "Outros"
  for(c in Categoria){
    
    DadosServidores = Dados[Dados$Categoria==c,]
    
    da =  rodaSimulacao (taxaAumentoSalarial, DadosServidores, Diretorio, Rodadas, Tempo, tabelaMortalidade, tabelaInvalidez,  Municipio,TetoINSS)
    
    dado = data.frame("Ano" = da[1],
                      "Municipio" = Municipio,
                      "Categoria"=c,
                      "Minimo"=da[2],
                      "IC  0.95 % Menor"=da[3],
                      "Media"=da[4],
                      "IC  0.95 % Maior"=da[5],
                      "Maximo"=da[6],
                      "Tempo medio ate saida para aposentadoria"  = da[7])
    dados = rbind(dados,dado)  
  }
  
  write.csv(dados,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada.csv"), row.names=FALSE)
  
  #Resultados medios
  dados2020_Media = dados %>% filter(Ano==2020) %>% select (Ano, Municipio, Categoria, Media)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = Media)
  write.csv(dados2020_Media,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2020 Media.csv"), row.names = FALSE)
  
  dados2025_Media = dados %>% filter(Ano==2025) %>% select (Ano, Municipio, Categoria, Media)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = Media)
  write.csv(dados2025_Media,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2025 Media.csv"), row.names = FALSE)
  
  dados2030_Media = dados %>% filter(Ano==2030) %>% select (Ano, Municipio, Categoria, Media)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = Media)
  write.csv(dados2030_Media,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2030 Media.csv"), row.names = FALSE)
  
  #Resultados máximo
  dados2020_Maximo = dados %>% filter(Ano==2020) %>% select (Ano, Municipio, Categoria, IC..0.95...Maior)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Maior)
  write.csv(dados2020_Maximo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2020 Maximo.csv"), row.names = FALSE)
  
  dados2025_Maximo = dados %>% filter(Ano==2025) %>% select (Ano, Municipio, Categoria, IC..0.95...Maior)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Maior)
  write.csv(dados2025_Maximo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2025 Maximo.csv"), row.names = FALSE)
  
  dados2030_Maximo = dados %>% filter(Ano==2030) %>% select (Ano, Municipio, Categoria, IC..0.95...Maior)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Maior)
  write.csv(dados2030_Maximo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2030 Maximo.csv"), row.names = FALSE)
  
  #Resultados mínimos
  dados2020_Minimo = dados %>% filter(Ano==2020) %>% select (Ano, Municipio, Categoria, IC..0.95...Menor)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Menor)
  write.csv(dados2020_Minimo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2020 Minino.csv"), row.names = FALSE)
  
  dados2025_Minino = dados %>% filter(Ano==2025) %>% select (Ano, Municipio, Categoria, IC..0.95...Menor)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Menor)
  write.csv(dados2025_Minino,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2025 Minino.csv"), row.names = FALSE)
  
  dados2030_Minimo = dados %>% filter(Ano==2030) %>% select (Ano, Municipio, Categoria, IC..0.95...Menor)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Menor)
  write.csv(dados2030_Minimo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2030 Minimo.csv"), row.names = FALSE)
  
  print(i)
}


# Quando der erro, fechar o pgm e atualizar a área de trabalho do pc
   
#OS QUE NÃO RODARAM pq não tem tabela de vida: 421265 (i=2835), 500627 (i=3262), 150475(i=3459), 422000 (i=5080)
#os que nao rodaram dos 449: 431454 (i=123)
#aJUSTE DOS MUNICIPIOS QUE NAO TEM PESSOAS EM ALGUM SEXO, EM ALGUMA CATEGORIA

tabelaMunicipios = read.csv("C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Dissertação\\Capacidade de contratação dos municípios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados\\municipio_grupomuniciElaineDepoisdos449.csv")
tabelaMunicipios$Município = as.character(tabelaMunicipios$Município)
Servidores = read.csv("C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Dissertação\\Capacidade de contratação dos municípios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados\\ServidoresTotal08-02.csv")
Servidores$Município = as.character(Servidores$Município)
Servidores = inner_join(tabelaMunicipios, Servidores, by="Município")
Servidores$Município = as.character(Servidores$Município)
Servidores$EducaçãoFem=NULL
Servidores$EducaçãoMas=NULL
Servidores$SaúdeFem=NULL
Servidores$SaúdeMas=NULL
Servidores$OutrosFem=NULL
Servidores$OutrosMas=NULL
Servidores$simulacao=NULL

#Separando os grupos de servidores
{
  ServidoresEducacaoFem = Servidores[(Servidores$Sexo==1&Servidores$Categoria=="Educação"),]
  ServidoresEducacaoMas = Servidores[(Servidores$Sexo==2&Servidores$Categoria=="Educação"),]
  ServidoresSaudeFem = Servidores[(Servidores$Sexo==1&Servidores$Categoria=="Saúde"),]
  ServidoresSaudeMas = Servidores[(Servidores$Sexo==2&Servidores$Categoria=="Saúde"),]
  ServidoresOutrosMas = Servidores[(Servidores$Sexo==1&Servidores$Categoria=="Outros"),]
  ServidoresOutrosFem = Servidores[(Servidores$Sexo==2&Servidores$Categoria=="Outros"),]
  
}

#Educação Feminino
EducacaoFem = tabelaMunicipios %>% select(Município, EducaçãoFem)
#Não Ajustar
EducacaoFemNaoAjustar = EducacaoFem[EducacaoFem$EducaçãoFem!=0,]
vetorEducacaoFemNaoAjustar = data.frame(Município = unique(EducacaoFemNaoAjustar$Município))
vetorEducacaoFemNaoAjustar = inner_join(vetorEducacaoFemNaoAjustar,
                                        ServidoresEducacaoMas, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
EducacaoFemAjustar = EducacaoFem[EducacaoFem$EducaçãoFem==0,]
vetorEducacaoFemAjustar = data.frame(Município = unique(EducacaoFemAjustar$Município))
vetorEducacaoFemAjustar = inner_join(vetorEducacaoFemAjustar,
                                     ServidoresEducacaoMas, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria

#Educação Masculino
EducacaoMas = tabelaMunicipios %>% select(Município, EducaçãoMas)
#Não Ajustar
EducacaoMasNaoAjustar = EducacaoMas[EducacaoMas$EducaçãoMas!=0,]
vetorEducacaoMasNaoAjustar = data.frame(Município = unique(EducacaoMasNaoAjustar$Município))
vetorEducacaoMasNaoAjustar = inner_join(vetorEducacaoMasNaoAjustar,
                                        ServidoresEducacaoFem, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
EducacaoMasAjustar = EducacaoMas[EducacaoMas$EducaçãoMas==0,]
vetorEducacaoMasAjustar = data.frame(Município = unique(EducacaoMasAjustar$Município))
vetorEducacaoMasAjustar = inner_join(vetorEducacaoMasAjustar,
                                     ServidoresEducacaoFem, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria

#Saúde Feminino
SaudeFem = tabelaMunicipios %>% select(Município, SaúdeFem)
#Não Ajustar
SaudeFemNaoAjustar = SaudeFem[SaudeFem$SaúdeFem!=0,]
vetorSaudeFemNaoAjustar = data.frame(Município = unique(SaudeFemNaoAjustar$Município))
vetorSaudeFemNaoAjustar = inner_join(vetorSaudeFemNaoAjustar,
                                        ServidoresSaudeMas, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
SaudeFemAjustar = SaudeFem[SaudeFem$SaúdeFem==0,]
vetorSaudeFemAjustar = data.frame(Município = unique(SaudeFemAjustar$Município))
vetorSaudeFemAjustar = inner_join(vetorSaudeFemAjustar,
                                     ServidoresSaudeMas, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria

#Educação Masculino
SaudeMas = tabelaMunicipios %>% select(Município, SaúdeMas)
#Não Ajustar
SaudeMasNaoAjustar = SaudeMas[SaudeMas$SaúdeMas!=0,]
vetorSaudeMasNaoAjustar = data.frame(Município = unique(SaudeMasNaoAjustar$Município))
vetorSaudeMasNaoAjustar = inner_join(vetorSaudeMasNaoAjustar,
                                        ServidoresSaudeFem, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
SaudeMasAjustar = SaudeMas[SaudeMas$SaúdeMas==0,]
vetorSaudeMasAjustar = data.frame(Município = unique(SaudeMasAjustar$Município))
vetorSaudeMasAjustar = inner_join(vetorSaudeMasAjustar,
                                     ServidoresSaudeFem, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria

#Outros Feminino
OutrosFem = tabelaMunicipios %>% select(Município, OutrosFem)
#Não Ajustar
OutrosFemNaoAjustar = OutrosFem[OutrosFem$OutrosFem!=0,]
vetorOutrosFemNaoAjustar = data.frame(Município = unique(OutrosFemNaoAjustar$Município))
vetorOutrosFemNaoAjustar = inner_join(vetorOutrosFemNaoAjustar,
                                     ServidoresOutrosMas, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
OutrosFemAjustar = OutrosFem[OutrosFem$OutrosFem==0,]
vetorOutrosFemAjustar = data.frame(Município = unique(OutrosFemAjustar$Município))
vetorOutrosFemAjustar = inner_join(vetorOutrosFemAjustar,
                                  ServidoresOutrosMas, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria

#Educação Masculino
OutrosMas = tabelaMunicipios %>% select(Município, OutrosMas)
#Não Ajustar
OutrosMasNaoAjustar = OutrosMas[OutrosMas$OutrosMas!=0,]
vetorOutrosMasNaoAjustar = data.frame(Município = unique(OutrosMasNaoAjustar$Município))
vetorOutrosMasNaoAjustar = inner_join(vetorOutrosMasNaoAjustar,
                                     ServidoresOutrosFem, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
OutrosMasAjustar = OutrosMas[OutrosMas$OutrosMas==0,]
vetorOutrosMasAjustar = data.frame(Município = unique(OutrosMasAjustar$Município))
vetorOutrosMasAjustar = inner_join(vetorOutrosMasAjustar,
                                  ServidoresOutrosFem, by="Município") #Vertor de mun que não tem mulheres na categoria com os dados dos homens dessa categoria


#Ajuste 
Município = unique(vetorEducacaoMasAjustar$Município) #Mudar aqui
dados = vetorEducacaoMasAjustar #Mudar aqui
dado = data.frame()
for(i in Município){
  Dados = dados[dados$Município==i,]
  Dados[1,8]=2 #Mudar aqui para 2 quando o sexo for masculino
  dado = rbind(dado, Dados)
  print(i)
}
vetorEducacaoMasAjustado = rbind(dado, vetorEducacaoMasNaoAjustar) #Mudar aqui



ServidoresAjustado = rbind(vetorEducacaoFemAjustado, vetorEducacaoMasAjustado,
                           vetorSaudeFemAjustado, vetorSaudeMasAjustado,
                           vetorOutrosFemAjustado, vetorOutrosMasAjustado)

write.csv(ServidoresAjustado, "C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Dissertação\\Capacidade de contratação dos municípios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados\\ServidoresRodarDepois449.csv")
