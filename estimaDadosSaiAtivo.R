#' estimaDadosSaiAtivo function
#' @param value
#' @return value
#' @export

estimaDadosSaiAtivo=function(TempoRPPS,DadosServidores,TMD,ProbFamilia,Rodadas) {
##Motivo pelo qual saiu da situa??o de ativo.
  TamanhoPopulacao=length(DadosServidores[[1]])

  Idade = DadosServidores$Idade
  Sexo = DadosServidores$Sexo
  IdadeAposentadoria = DadosServidores$IdadeAposentadoria
  EstadoInicial = DadosServidores$EstadoInicial


MotivoSaiAtivo = matrix(rep(6,Rodadas),ncol=Rodadas)
SexoBeneficiario1 = matrix(rep(Sexo,Rodadas),ncol=Rodadas)
IdadeBeneficiario1 = matrix(rep(Idade,Rodadas),ncol=Rodadas)

return(list(MotivoSaiAtivo,SexoBeneficiario1,IdadeBeneficiario1))
}

