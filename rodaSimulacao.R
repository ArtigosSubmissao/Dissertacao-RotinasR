rodaSimulacao <- function (taxaAumentoSalarial, DadosServidores, Diretorio,  Rodadas, Tempo, tabelaMortalidade, tabelaInvalidez,  Municipio,TetoINSS){
  #print(Rodadas)
  
  
  #' Definições de tamanhos e dimencoes dos graficos gerados 
 #AQUI PARAM_PLOT = list(
    #parametro cex.main - tamanho da letra do titulo
  #AQUI  tamanhoTitulo = 3.0,
    
    #parametros width e height - dimensoes da imagem gerada
  #AQUI  larguraImg = 850,
  #AQUI  alturaImg = 800,
    
    #parametros width e height - dimensoes da imagem gerada
  #AQUI  larguraImg = 850,
  #AQUI  alturaImg = 800,
    
    #parametro cex.lab - tamanho dos titulos
  #AQUI  tamanhoTituloEixo = 2.5,
    
    #parametros cex.axis- tamanho das letras dos eixos
  #AQUI  tamanhoValorEixo = 2.5,
    
    #parametro lwd - controls the line width.
  #AQUI  espessuraLinhas = 2.0,
  #AQUI   tamanhoLinha = 2.0,
    
    #parametros cex - tamando da legenda
  #AQUI  tamanhoLegenda = 2.5
    
  #AQUI )
  
  
  #' Definições de tamanhos e dimencoes dos graficos gerados
  #AQUI {
    #parametros width e height - dimensoes da imagem gerada
  #AQUI  larguraImg = 1600
  #AQUI  alturaImg = 1300 # "colinha" --> width = larguraImg,height = alturaImg
    #parametro cex.main - tamanho da letra do titulo
  #AQUI  tamanhoTitulo = 1.5 
    #parametro cex.lab - tamanho dos titulos
  #AQUI  tamanhoTituloEixo = 1.25
    #parametros cex.axis- tamanho das letras dos eixos
  #AQUI  tamanhoValorEixo = 1
    #parametro lwd e cex, espessura e altura das linhas 
  #AQUI   espessuraLinhas = 2 ## 2.3
  #AQUI  tamanhoLinha = 2
    # "colinha" --> lwd = espessuraLinhas, cex.axis=tamanhoValorEixo,cex.lab=tamanhoTituloEixo, cex=tamanhoLinha,cex.main=tamanhoTitulo
    #parametros para a legenda (lwd e cex)
  #AQUI  espessuraLegenda = 1
  #AQUI  tamanhoLegenda = 1
    #parametros para linhas de marca??es (lwd,cex.axis,cex.lab,cex)
  #AQUI  espessuraLinhasMarcacao = 1
  #AQUI   tamanhoValorEixoLinhasMarcacao = 1
  #AQUI  tamanhoTituloEixoLinhasMarcacao = 0.5
  #AQUI  espessuraPontosMarcacao = 1
  #AQUI   tamanhoValorEixoPontosMarcacao = 1
  #AQUI   tamanhoTituloEixoPontosMarcacao = 1
  #AQUI  tamanhoTituloPontosMarcacao = 1
  #AQUI  tamanhoPontosMarcacao = 1 # "colinha" --> lwd = espessuraPontosMarcacao, cex.axis=tamanhoValorEixoPontosMarcacao,cex.lab=tamanhoTituloEixoPontosMarcacao, cex=tamanhoPontosMarcacao, cex.main=tamanhoTituloPontosMarcacao
  #AQUI   tamanhoLinhasMarcacao = 1 # "colinha" --> lwd = espessuraLinhasMarcacao, cex.axis=tamanhoValorEixoLinhasMarcacao,cex.lab=tamanhoTituloEixoLinhasMarcacao, cex=tamanhoLinhasMarcacao
    # "colinha" --> lwd = espessuraLinhasMarcacao, cex=tamanhoLinhasMarcacao
    
    ## pointsize
  #AQUI  tamanhoPointsize = 40
  #AQUI }
  
  
  tryCatch({
    #' Neste trecho as funcoes mais custosas sao compiladas, 
    #' trazendo um ganho de desempenho
    expressao = expression({
      #' Trecho de código é transformado em uma expressão para ser avaliada
      #' no tryCatch
      #AQUI calculaValoresPagamento = cmpfun(calculaValoresPagamento)
      #estimaDadosFimBeneficio1 = cmpfun(estimaDadosFimBeneficio1)
      #estimaDadosSaiAtivo = cmpfun(estimaDadosSaiAtivo)
      #estimaEstadosServidor = cmpfun(estimaEstadosServidor)
      #aqui estimaIdadeAposentadoria = cmpfun(estimaIdadeAposentadoriaRPPS)
      #estimaIdadeEntrada = cmpfun(estimaIdadeEntrada)
      #estimaSeDeixouDependente = cmpfun(estimaSeDeixouDependente)
      estimaTempoAteMorte = cmpfun(estimaTempoAteMorte)
      #estimaTempoBeneficio1 = cmpfun(estimaTempoBeneficio1)
      #estimaTempoBeneficio2 = cmpfun(estimaTempoBeneficio2)
      #estimaTempoRPPS = cmpfun(estimaTempoRPPS)
      EstimaTx = cmpfun(EstimaTx)
      geraGraficoPercentil = cmpfun(geraGraficoPercentil)
      #GraficosMudaEstado = cmpfun(GraficosMudaEstado)
      #GraficosNRodadas = cmpfun(GraficosNRodadas)
      #aqui idadeTipoAposentadoriaRPPS = cmpfun(idadeTipoAposentadoriaRPPS)
      #piramide = cmpfun(piramide)
      resumeEstadoServidor = cmpfun(resumeEstadoServidor)
      
    })
    
    #' expressão criada é avaliada
    eval(expressao)
  }, error = function(e) {
    
    lancarErro("Erro durante a compilação de funções", "rodaSimulacao",e)
    stop()
  })
  
  #AQUI tryCatch({
  #AQUI  DataHora = gsub(":", "-", as.character(Sys.time()))
  #AQUI   DiretorioSaida = paste(Diretorio, "/Resultados/", DataHora,"- ", Municipio," ", Rodadas," Rodadas", sep = "") 
  #AQUI  dir.create(file.path(paste(Diretorio, "/Resultados/", DataHora,"- ", Municipio," ", Rodadas, " Rodadas",sep = "")), showWarnings = FALSE)
  #AQUI },error = function(e){
  #AQUI  lancarErro("Erro criação do diretório de saida","rodaSimulacao",e)
  #AQUI   stop()
  #AQUI })
  
  
  
  
  tryCatch({
    
    #' Leitura dos arquivos necessários
    
    #Gerando TMD e TDU dinamicamente atraves do arquivo tabelas.RData
    
    TMDTDU = gerarTMDTDU(tabelaMortalidade,tabelaInvalidez)
    TMD =TMDTDU[[1]]
    TabuasMorte = TMDTDU[[2]]
    #load(paste(DIRETORIO_RAIZ,"TabelaEsperancaDeVida.RData",sep = ""))
    
    #aqui DadosServidores = cbind(DadosServidores, IdadeAposentadoria = estimaIdadeAposentadoriaRPPS(DadosServidores)[,1], TipoAposentadoria = estimaIdadeAposentadoriaRPPS(DadosServidores)[,2])
    DadosServidores = DadosServidores
    TamanhoPopulacao = nrow(DadosServidores)
    
    ## Taxa de aumento salarial calculada previamente
    #taxaAumentoSalarial = DadosRPPS$taxaAumentoSalarial
    
  },error=function(e){
    
    lancarErro("Erro ao carregar alguns dados: TMD, TDU...","rodaSimulacao",e)
    stop()
    
  })
  
  
  
  ##Alguns gráficos sobre a populacao
  
  #tryCatch({
 #   setwd(DiretorioSaida)
 #   
 #   png(width = PARAM_PLOT$larguraImg,height = PARAM_PLOT$alturaImg ,paste("Piramide Etaria ",TamanhoPopulacao,"servidores ativos iniciais.png"))
 #   piramide(DadosServidores[DadosServidores$EstadoInicial==1,],titulo=(paste0(sum(DadosServidores$EstadoInicial==1)," Ativos Iniciais")))
 #   dev.off()
    
 # },error = function(e){
  #  lancarErro("Erro ao gerar graficos - FazGraficosSimulacao.R","rodaSimulacao",e)
  #  stop()
 # })
  
  
 # tryCatch({
 #   png(width = PARAM_PLOT$larguraImg,height = PARAM_PLOT$alturaImg,paste("Remuneracao ",sum(DadosServidores$EstadoInicial==1),"Ativos iniciais.png"))
 #   boxplot(DadosServidores$Salario[DadosServidores$EstadoInicial==1]~DadosServidores$Sexo[DadosServidores$EstadoInicial==1], main=" Remuneracao dos ativos iniciais", names=c("Mulheres", "Homens"), ylab="Remuneracao (R$)",cex.axis = 2,cex.lab = 2, cex = 2, cex.main = 2)
 #   dev.off()
    
 # },error = function(e){
 #   lancarErro("Erro ao gerar arquivo servidores.png","rodaSimulacao",e)
 #   stop()
 # })
  
  
  
  
  
  
  
  tryCatch({
    
    #AQUInCores <- detectCores()
    #AQUI cl <- makeCluster(nCores)
    #AQUI registerDoParallel(nCores)
   # aqui Pagamentos=ListaSalarioContribuicao =ListaSalarioContribuicaoAcimaDoTeto= ListaMatrizResumoServidor = 0
    TempoAteSaida=  0
    Blocos = 5
  },error=function(e){
    
    lancarErro("Erro ao inicializar dados de simulação","rodaSimulacao",e)
    stop()
    
  })
  
  
  
  ListaResultados = foreach(i = 1:Blocos) %do%{
    
    TempoAteSaida = estimaTempoAteSaida(DadosServidores,TMD, Rodadas/Blocos)
    ListaMatrizResumoServidor = resumeEstadoServidor(TempoAteSaida,DadosServidores, Tempo)
    
    #aqui  tryCatch({
   #aqui   #Pagamentos = calculaValoresPagamento(DadosServidores,TempoAteSaida,taxaAumentoSalarial,Tempo,Rodadas/Blocos,TetoINSS)
      #aqui Pagamentos = calculaValoresPagamento(DadosServidores,TempoAteSaida,taxaAumentoSalarial,Tempo,TetoINSS)
      #aqui ListaSalarioContribuicao=Pagamentos[[1]]
      #aqui ListaSalarioContribuicaoAcimaDoTeto=Pagamentos[[2]]
    #aqui  },error=function(e){
      
    #aqui   lancarErro("Erro na calculaValoresPagamento","rodaSimulacao",e)
    #aqui   stop()
      
    #aqui  })
    #aqui   return(c(list(SalarioContribuicao = ListaSalarioContribuicao),list(SalarioContribuicaoAcimaDoTeto = ListaSalarioContribuicaoAcimaDoTeto),list(Resumo = ListaMatrizResumoServidor)
    #aqui  ))
    return(list(Resumo = ListaMatrizResumoServidor))
    }
  
  #write.csv(MatrizEstadosServidor, paste(DiretorioSaida, "/Matriz Estado Servidor.csv", sep=""))
  
  
    #aqui stopCluster(cl)
  
  
  tryCatch({
    #'calculos pós simulação
    
    #SalarioContribuicao =SalarioContribuicaoAcimaDoTeto=MatrizResumoServidor= matrix(NA, nrow = Tempo,ncol = 0)
    MatrizResumoServidor= matrix(NA, nrow = Tempo,ncol = 0)
    
    for (i in 1:Blocos) {
    #SalarioContribuicao = cbind(SalarioContribuicao, ListaResultados[[i]]$SalarioContribuicao)
    #SalarioContribuicaoAcimaDoTeto = cbind(SalarioContribuicaoAcimaDoTeto, ListaResultados[[i]]$SalarioContribuicaoAcimaDoTeto)
    MatrizResumoServidor = cbind(MatrizResumoServidor,ListaResultados[[i]]$Resumo)
      
    }
    
    
    rm(ListaResultados)
    invisible(gc())
    
  }, error = function(e) {
    lancarErro("Erro em calculos pós simulação","rodaSimulacao",e)
    stop()
  })
  
  
  
    #aqui tryCatch({
    #aqui  png(pointsize =  tamanhoPointsize, width = 1600,height = 1300,filename = paste(DiretorioSaida, "/SalarioContribuicao.png",sep = ""))
    #aqui geraGraficoPercentil(SalarioContribuicao/1e+03, 0.95, min(SalarioContribuicao/1e+03),max(SalarioContribuicao/1e+03), "Salarios de Contribuicao Total","Mil R$", "topright",DiretorioSaida)
    #aqui  dev.off()
    #aqui},error = function(e){
    #aqui  lancarErro("Erro ao gerar arquivo SalarioContribuicao.png","rodaSimulacao",e)
      #aqui   stop()
    #aqui })
  
#AQUI  #tryCatch({
   # png(pointsize =  tamanhoPointsize, width = 1600,height = 1300,filename = paste(DiretorioSaida, "/SalarioContribuicaoAcimaDoTeto.png",sep = ""))
  #  geraGraficoPercentil(SalarioContribuicaoAcimaDoTeto/1e+03, 0.95, min(SalarioContribuicaoAcimaDoTeto/1e+03),max(SalarioContribuicaoAcimaDoTeto/1e+03), "Salarios de Contribuicao Total Acima do Teto do INSS","Mil R$", "topright",DiretorioSaida)
  #  dev.off()
 # },error = function(e){
 #   lancarErro("Erro ao gerar arquivo SalarioContribuicaoAcimaDoTeto.png","rodaSimulacao",e)
#    stop()
 # })
  
  
  #AQUI  tryCatch({
  #AQUI  png(pointsize =  tamanhoPointsize, width = 1600,height = 1300,filename = paste(DiretorioSaida, "/Ativos.png",sep = ""))
  #AQUI   geraGraficoPercentil(MatrizResumoServidor, 0.95, min(MatrizResumoServidor),max(MatrizResumoServidor), "Ativos", "Ativos", "topright",DiretorioSaida)
  #AQUI  dev.off()
  #AQUI },error = function(e){
  #AQUI   lancarErro("Erro ao gerar arquivo Ativos.png","rodaSimulacao",e)
  #AQUI   stop()
  #AQUI })
  
  
  
  
  ####Indica medidas resumo para as projeções.   
  ##Calcula tempo médio até saída
  t=seq(1:Tempo)
  TempoMedio=rep(0,Rodadas)
  for (i in 1:Rodadas){
    TempoMedio[i]=sum((MatrizResumoServidor[,i]-c(MatrizResumoServidor[,i][-1],0))*t)/MatrizResumoServidor[,i][1]
  }
  #####Calcula Soma dos valores das contribuições futuras e retorna a mediana dos valores estimados em todas as rodadas. 
 # MedidasResumo=cbind.data.frame(median(colSums(SalarioContribuicaoAcimaDoTeto)), median(colSums(SalarioContribuicao)), median(TempoMedio))
 # names(MedidasResumo)=c("Mediana da Soma dos Salarios de Contribuição Acima do Teto Projetados", "Mediana da Soma dos Salarios de Contribuição Projetados", "Tempo médio até saída para aposentadoria")
  
  #write.csv(MedidasResumo, paste(DiretorioSaida, "\\Medidas Resumo.csv", sep=""))
  geraGraficoPercentil=function(MatrizValores,IntervaloConfianca,Ymin, Ymax, Titulo, NomeY, PosicaoLegenda, DiretorioSaida){
    #Graficos por percentil
    Tempo=dim(MatrizValores[])[1]
    
    #parametro cex.main - tamanho da letra do titulo
    tamanhoTitulo = 1.5
    #parametro cex.lab - tamanho dos titulos
    tamanhoTituloEixo = 1.25
    #parametros cex.axis- tamanho das letras dos eixos
    tamanhoValorEixo = 1
    #parametro lwd e cex, espessura e altura das linhas 
    espessuraLinhas = 5 ## 2.3
    tamanhoLinha = 5 # "colinha" --> lwd = espessuraLinhas, cex.axis=tamanhoValorEixo,cex.lab=tamanhoTituloEixo, cex=tamanhoLinha,cex.main=tamanhoTitulo
    #parametros para a legenda (lwd e cex)
    espessuraLegenda = 3
    tamanhoLegenda = 1
    #parametros para linhas de marca��es (lwd,cex.axis,cex.lab,cex)
    espessuraLinhasMarcacao = 3
    tamanhoValorEixoLinhasMarcacao = 1
    tamanhoTituloEixoLinhasMarcacao = 1
    tamanhoLinhasMarcacao = 1 # "colinha" --> lwd = espessuraLinhasMarcacao, cex.axis=tamanhoValorEixoLinhasMarcacao,cex.lab=tamanhoTituloEixoLinhasMarcacao, cex=tamanhoLinhasMarcacao
    # "colinha" --> lwd = espessuraLinhasMarcacao, cex=tamanhoLinhasMarcacao
    
    QuartilInferior=QuartilSuperior=Media=Minimo=Maximo=0
    for (t in 1:Tempo) {
      QuartilInferior[t]=quantile(MatrizValores[][t,],(1-IntervaloConfianca)/2)
      QuartilSuperior[t]=quantile(MatrizValores[][t,],IntervaloConfianca+(1-IntervaloConfianca)/2)
      Media[t]=mean(MatrizValores[][t,])
      Maximo[t]=max(MatrizValores[][t,])
      Minimo[t]=min(MatrizValores[][t,])
    }
    
    #' tratamento de um valor inesperado. Pode ocorrer uma divisão por zero
    #' e os valores de ymin e ymax acaba sendo Inf e invalido para o plot
    if((Ymin == -Inf)||(Ymin == Inf))
      Ymin= -10
    
    if((Ymax == -Inf)||(Ymax == Inf))
      Ymax= 10
    
    ##Salva tabelas com resultados
    Ano = data.frame(Ano=seq(2019,2068))
    Resultado <- cbind(Ano,Minimo,QuartilInferior,Media, QuartilSuperior,Maximo)
    Resultado = Resultado[(Resultado$Ano==2020|Resultado$Ano==2025|Resultado$Ano==2030),]
    colnames(Resultado) <- c("Ano","Minimo",paste("IC ", IntervaloConfianca,"%","Menor"),"Media",paste("IC ", IntervaloConfianca,"%","Maior"), "Maximo")
    
    return(Resultado)
  }
  
  #ResultadoAberta = geraGraficoPercentil(MatrizResumoServidorAberta, 0.95, min(MatrizResumoServidorAberta),max(MatrizResumoServidorAberta), "Ativos na Populacao Aberta", "Ativos", "topright",DiretorioSaida)
  ResultadoFechada = geraGraficoPercentil(MatrizResumoServidor, 0.95, min(MatrizResumoServidor),max(MatrizResumoServidor), "Ativos", "Ativos", "topright",DiretorioSaida)
  MedidasResumo = cbind.data.frame(ResultadoFechada,
                                   # ResultadoAberta,
                                   median(TempoMedio))
 
  #Salvando os observa��es
  Ano = data.frame(Ano=seq(2019,2068))
  MatrizResumoServidorSalvar = cbind(Ano, MatrizResumoServidor)
  write.csv(MatrizResumoServidorSalvar,paste0(DiretorioGrardaResultados,"/Observa��esPop.csv"), row.names = FALSE)
  
  return(MedidasResumo)
  
  
}#end function


