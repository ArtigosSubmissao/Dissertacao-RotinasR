#' estimaTempoRPS function
#' @param value
#' @return value
#' @export

estimaTempoAteSaida = function(DadosServidores,TMD, Rodadas){
###Calcula tempo at? a sa?de por morte ou invalidez pela TMD.

Idade = DadosServidores$Idade
Sexo = DadosServidores$Sexo
IdadeAposentadoria = DadosServidores$IdadeAposentadoria
#IdadeAposentadoria[is.na(IdadeAposentadoria)] = -1
EstadoInicial = DadosServidores$EstadoInicial
TempoAteSaida = TempoParaAposentadoria = matrix(-1,nrow=length(Idade),ncol=Rodadas)
###Calcula tempo at? a sa?da pelas t?buas definidas.

#IndicesAtivo = which(EstadoInicial==1)

tryCatch({
  
  TempoAteSaida[EstadoInicial==1,]=estimaTempoAteMorte(matrix(rep(Idade[EstadoInicial==1],Rodadas), ncol=Rodadas),matrix(rep(Sexo[EstadoInicial==1],Rodadas), ncol=Rodadas),TMD$lxTotalTMD.M,TMD$lxTotalTMD.F)#Tempo até a saída por morte ou invalidez de acordo com a TMD

},error = function(e){
  lancarErro("Erro ao estimar tempo ate morte", "estimaTempoAteSaida",e)
  stop()
})

#limite=c(min(TempoAteSaida[,1]), max(TempoAteSaida[,1]))  ##Garante que os dois gr?ficos ficar?o na mesma escala
#png(paste("Tempo at? morte ou invalidez x Idade - ",dim(DadosServidores)[1], "Servidores .png"))
#plot(Idade[Sexo==1],TempoAteSaida[,1][Sexo==1], xlab="Idade atual", ylab="Tempo at? a sa?da por invalidez ou morte", col="red",pch=1, ylim=limite,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(Idade[Sexo==2],TempoAteSaida[,1][Sexo==2], xlab="Idade atual", ylab="Tempo at? a sa?da por invalidez ou morte", col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#legend("topright", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

TempoParaAposentadoria[EstadoInicial==1,] = rep(IdadeAposentadoria[EstadoInicial==1]-Idade[EstadoInicial==1], Rodadas)
#TempoParaAposentadoria[TempoParaAposentadoria<0] = -1
TempoAteSaida[TempoAteSaida>TempoParaAposentadoria]=TempoParaAposentadoria[TempoAteSaida>TempoParaAposentadoria]

#TempoAteSaida[IndicesAtivo,]=TempoAteSaida[IndicesAtivo,]+1 #Corre??o para satisfazer a condi??o de que as contribui??es s?o feitas no in?cio de cada per?odo, e n?o no final
#TempoAteSaida[IndicesAtivo,] = TempoAteSaida[IndicesAtivo,]+(Idade[IndicesAtivo]-DadosServidores$IdadeEntradaRPPS[IndicesAtivo])
TempoAteSaida[TempoAteSaida<0] = 0


#png(paste("TempoAteSaida x Idade - ",dim(DadosServidores)[1], "Servidores .png"))
#plot(Idade[Sexo==1],TempoAteSaida[,1][Sexo==1], xlab="Idade atual", ylab="Tempo como ativo", col="red",pch=1, ylim=limite,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(Idade[Sexo==2],TempoAteSaida[,1][Sexo==2], col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#legend(x=50, y = limite[2], c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

return(TempoAteSaida)
}



