rm(list=ls())
memory.limit()
memory.limit(size=3000000000)
######Prepara dados para rodar simulaÃ§Ã£o sem usar shyni. 

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
#----------------------- carregar funÃ§Ãµes -----------------
##Indica diretÃ³rio onde salvou as funÃ§Ãµes
setwd("E:\\Coisas do dropbox\\Disserta��o\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)")
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



##Define parâmetros
{
  Diretorio="E:\\Coisas do dropbox\\Disserta��o\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)"      
  TetoINSS=5645.80   ###Verificar valor
  Tempo=50
  Rodadas=2500  ##Mude 100 para testar, mas os resultados oficiais devem ser com quantidade maior, de pelo menos 2500 rodadas para municÃ�pios e 1000 para estados.  
  SalMinimo = 954 #estabelece salario minimo de 954
  
  }



##Carrega dados inciais
DiretorioDadosIniciais="E:\\Coisas do dropbox\\Disserta��o\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados"
DiretorioGrardaResultados="E:\\Coisas do dropbox\\Disserta��o\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Resultados"


{
setwd(DiretorioDadosIniciais)
tabelaInvalidez=read.csv("AlvaroVindas.csv", sep=";", dec=",")
#mun_grupo <-read.csv("municipio_grupomuniciElaine.csv", sep=",")
#mun_grupo = data.frame(Munic�pio = unique(mun_grupo$Munic�pio), Grupo=1) #Tirar isso
mun_grupo = data.frame(Munic�pio = "421223", Grupo=1)
TxSalarial=read.csv("AumentoSalarialPorEnte.csv", sep=";", dec=",")
}


dados = data.frame()

for(i in 1:1) {
  row <- mun_grupo[i,]
  Municipio= toString(row[1])
  #Grupo = toString(row[2])
  
  Dados=read.csv(paste0(DiretorioDadosIniciais, "\\Populacao Inicial\\",Municipio," .csv"), sep=",", dec=".")
  
  Dados=Dados[Dados$EstadoInicial==1,]   ##Para este projeto só interessam os ativos
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
  
  #Resultados m�ximo
  dados2020_Maximo = dados %>% filter(Ano==2020) %>% select (Ano, Municipio, Categoria, IC..0.95...Maior)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Maior)
  write.csv(dados2020_Maximo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2020 Maximo.csv"), row.names = FALSE)
  
  dados2025_Maximo = dados %>% filter(Ano==2025) %>% select (Ano, Municipio, Categoria, IC..0.95...Maior)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Maior)
  write.csv(dados2025_Maximo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2025 Maximo.csv"), row.names = FALSE)
  
  dados2030_Maximo = dados %>% filter(Ano==2030) %>% select (Ano, Municipio, Categoria, IC..0.95...Maior)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = IC..0.95...Maior)
  write.csv(dados2030_Maximo,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada 2030 Maximo.csv"), row.names = FALSE)
  
  #Resultados m�nimos
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


# Quando der erro, fechar o pgm e atualizar a �rea de trabalho do pc
   
#OS QUE N�O RODARAM pq n�o tem tabela de vida: 421265 (i=2835), 500627 (i=3262), 150475(i=3459), 422000 (i=5080)
#os que nao rodaram dos 449: 431454 (i=123)
#aJUSTE DOS MUNICIPIOS QUE NAO TEM PESSOAS EM ALGUM SEXO, EM ALGUMA CATEGORIA

tabelaMunicipios = read.csv("C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Disserta��o\\Capacidade de contrata��o dos munic�pios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados\\municipio_grupomuniciElaineDepoisdos449.csv")
tabelaMunicipios$Munic�pio = as.character(tabelaMunicipios$Munic�pio)
Servidores = read.csv("C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Disserta��o\\Capacidade de contrata��o dos munic�pios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados\\ServidoresTotal08-02.csv")
Servidores$Munic�pio = as.character(Servidores$Munic�pio)
Servidores = inner_join(tabelaMunicipios, Servidores, by="Munic�pio")
Servidores$Munic�pio = as.character(Servidores$Munic�pio)
Servidores$Educa��oFem=NULL
Servidores$Educa��oMas=NULL
Servidores$Sa�deFem=NULL
Servidores$Sa�deMas=NULL
Servidores$OutrosFem=NULL
Servidores$OutrosMas=NULL
Servidores$simulacao=NULL

#Separando os grupos de servidores
{
  ServidoresEducacaoFem = Servidores[(Servidores$Sexo==1&Servidores$Categoria=="Educa��o"),]
  ServidoresEducacaoMas = Servidores[(Servidores$Sexo==2&Servidores$Categoria=="Educa��o"),]
  ServidoresSaudeFem = Servidores[(Servidores$Sexo==1&Servidores$Categoria=="Sa�de"),]
  ServidoresSaudeMas = Servidores[(Servidores$Sexo==2&Servidores$Categoria=="Sa�de"),]
  ServidoresOutrosMas = Servidores[(Servidores$Sexo==1&Servidores$Categoria=="Outros"),]
  ServidoresOutrosFem = Servidores[(Servidores$Sexo==2&Servidores$Categoria=="Outros"),]
  
}

#Educa��o Feminino
EducacaoFem = tabelaMunicipios %>% select(Munic�pio, Educa��oFem)
#N�o Ajustar
EducacaoFemNaoAjustar = EducacaoFem[EducacaoFem$Educa��oFem!=0,]
vetorEducacaoFemNaoAjustar = data.frame(Munic�pio = unique(EducacaoFemNaoAjustar$Munic�pio))
vetorEducacaoFemNaoAjustar = inner_join(vetorEducacaoFemNaoAjustar,
                                        ServidoresEducacaoMas, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
EducacaoFemAjustar = EducacaoFem[EducacaoFem$Educa��oFem==0,]
vetorEducacaoFemAjustar = data.frame(Munic�pio = unique(EducacaoFemAjustar$Munic�pio))
vetorEducacaoFemAjustar = inner_join(vetorEducacaoFemAjustar,
                                     ServidoresEducacaoMas, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria

#Educa��o Masculino
EducacaoMas = tabelaMunicipios %>% select(Munic�pio, Educa��oMas)
#N�o Ajustar
EducacaoMasNaoAjustar = EducacaoMas[EducacaoMas$Educa��oMas!=0,]
vetorEducacaoMasNaoAjustar = data.frame(Munic�pio = unique(EducacaoMasNaoAjustar$Munic�pio))
vetorEducacaoMasNaoAjustar = inner_join(vetorEducacaoMasNaoAjustar,
                                        ServidoresEducacaoFem, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
EducacaoMasAjustar = EducacaoMas[EducacaoMas$Educa��oMas==0,]
vetorEducacaoMasAjustar = data.frame(Munic�pio = unique(EducacaoMasAjustar$Munic�pio))
vetorEducacaoMasAjustar = inner_join(vetorEducacaoMasAjustar,
                                     ServidoresEducacaoFem, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria

#Sa�de Feminino
SaudeFem = tabelaMunicipios %>% select(Munic�pio, Sa�deFem)
#N�o Ajustar
SaudeFemNaoAjustar = SaudeFem[SaudeFem$Sa�deFem!=0,]
vetorSaudeFemNaoAjustar = data.frame(Munic�pio = unique(SaudeFemNaoAjustar$Munic�pio))
vetorSaudeFemNaoAjustar = inner_join(vetorSaudeFemNaoAjustar,
                                        ServidoresSaudeMas, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
SaudeFemAjustar = SaudeFem[SaudeFem$Sa�deFem==0,]
vetorSaudeFemAjustar = data.frame(Munic�pio = unique(SaudeFemAjustar$Munic�pio))
vetorSaudeFemAjustar = inner_join(vetorSaudeFemAjustar,
                                     ServidoresSaudeMas, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria

#Educa��o Masculino
SaudeMas = tabelaMunicipios %>% select(Munic�pio, Sa�deMas)
#N�o Ajustar
SaudeMasNaoAjustar = SaudeMas[SaudeMas$Sa�deMas!=0,]
vetorSaudeMasNaoAjustar = data.frame(Munic�pio = unique(SaudeMasNaoAjustar$Munic�pio))
vetorSaudeMasNaoAjustar = inner_join(vetorSaudeMasNaoAjustar,
                                        ServidoresSaudeFem, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
SaudeMasAjustar = SaudeMas[SaudeMas$Sa�deMas==0,]
vetorSaudeMasAjustar = data.frame(Munic�pio = unique(SaudeMasAjustar$Munic�pio))
vetorSaudeMasAjustar = inner_join(vetorSaudeMasAjustar,
                                     ServidoresSaudeFem, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria

#Outros Feminino
OutrosFem = tabelaMunicipios %>% select(Munic�pio, OutrosFem)
#N�o Ajustar
OutrosFemNaoAjustar = OutrosFem[OutrosFem$OutrosFem!=0,]
vetorOutrosFemNaoAjustar = data.frame(Munic�pio = unique(OutrosFemNaoAjustar$Munic�pio))
vetorOutrosFemNaoAjustar = inner_join(vetorOutrosFemNaoAjustar,
                                     ServidoresOutrosMas, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
OutrosFemAjustar = OutrosFem[OutrosFem$OutrosFem==0,]
vetorOutrosFemAjustar = data.frame(Munic�pio = unique(OutrosFemAjustar$Munic�pio))
vetorOutrosFemAjustar = inner_join(vetorOutrosFemAjustar,
                                  ServidoresOutrosMas, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria

#Educa��o Masculino
OutrosMas = tabelaMunicipios %>% select(Munic�pio, OutrosMas)
#N�o Ajustar
OutrosMasNaoAjustar = OutrosMas[OutrosMas$OutrosMas!=0,]
vetorOutrosMasNaoAjustar = data.frame(Munic�pio = unique(OutrosMasNaoAjustar$Munic�pio))
vetorOutrosMasNaoAjustar = inner_join(vetorOutrosMasNaoAjustar,
                                     ServidoresOutrosFem, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria
##Ajustar
OutrosMasAjustar = OutrosMas[OutrosMas$OutrosMas==0,]
vetorOutrosMasAjustar = data.frame(Munic�pio = unique(OutrosMasAjustar$Munic�pio))
vetorOutrosMasAjustar = inner_join(vetorOutrosMasAjustar,
                                  ServidoresOutrosFem, by="Munic�pio") #Vertor de mun que n�o tem mulheres na categoria com os dados dos homens dessa categoria


#Ajuste 
Munic�pio = unique(vetorEducacaoMasAjustar$Munic�pio) #Mudar aqui
dados = vetorEducacaoMasAjustar #Mudar aqui
dado = data.frame()
for(i in Munic�pio){
  Dados = dados[dados$Munic�pio==i,]
  Dados[1,8]=2 #Mudar aqui para 2 quando o sexo for masculino
  dado = rbind(dado, Dados)
  print(i)
}
vetorEducacaoMasAjustado = rbind(dado, vetorEducacaoMasNaoAjustar) #Mudar aqui



ServidoresAjustado = rbind(vetorEducacaoFemAjustado, vetorEducacaoMasAjustado,
                           vetorSaudeFemAjustado, vetorSaudeMasAjustado,
                           vetorOutrosFemAjustado, vetorOutrosMasAjustado)

write.csv(ServidoresAjustado, "C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Disserta��o\\Capacidade de contrata��o dos munic�pios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados\\ServidoresRodarDepois449.csv")
