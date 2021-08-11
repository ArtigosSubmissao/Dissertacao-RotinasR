#' geraGraficoPercentil function
#' @param value
#' @return value
#' @export



##2018-11-13 - incÁuÌ nessa funÁ„o o salvamento das tabelas com as informaÁıes. (Cristiane)


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
  #parametros para linhas de marcaÁıes (lwd,cex.axis,cex.lab,cex)
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
  
  #' tratamento de um valor inesperado. Pode ocorrer uma divis√£o por zero
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
  write.csv(Resultado, paste(DiretorioSaida, "\\" , Titulo, ".csv", sep="", row.names=FALSE))
  
  
  plot(Media, type="l", ylim=c(Ymin,Ymax), xlab="Tempo", ylab=NomeY,main=Titulo,lwd = espessuraLinhas, cex.axis=tamanhoValorEixo,cex.lab=tamanhoTituloEixo, cex=tamanhoLinha,cex.main=tamanhoTitulo)
  lines (QuartilInferior, type="l",lty=2, col=4,lwd = espessuraLinhasMarcacao, cex.axis=tamanhoValorEixoLinhasMarcacao,cex.lab=tamanhoTituloEixoLinhasMarcacao, cex=tamanhoLinhasMarcacao)
  lines (QuartilSuperior, type="l",lty=2, col=4,lwd = espessuraLinhasMarcacao, cex.axis=tamanhoValorEixoLinhasMarcacao,cex.lab=tamanhoTituloEixoLinhasMarcacao, cex=tamanhoLinhasMarcacao)
  lines (Maximo, type="l", lty=3, col="orange", lwd = espessuraLinhasMarcacao, cex.axis=tamanhoValorEixoLinhasMarcacao,cex.lab=tamanhoTituloEixoLinhasMarcacao, cex=tamanhoLinhasMarcacao)
  lines (Minimo, type="l", lty=3, col="orange", lwd = espessuraLinhasMarcacao, cex.axis=tamanhoValorEixoLinhasMarcacao,cex.lab=tamanhoTituloEixoLinhasMarcacao, cex=tamanhoLinhasMarcacao)
  legend (PosicaoLegenda,lty = c(1,2,3),lwd=espessuraLegenda ,col=c(1,4,"orange"),c("Media", paste0("IC ", IntervaloConfianca,"%"), "Extremos"),bty="n",cex= tamanhoLegenda)
  abline(h=0)
  
  return(Resultado)
  
}

