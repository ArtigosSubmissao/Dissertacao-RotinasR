#' resumeEstadoServidor function
#' @param value
#' @return value
#' @export

resumeEstadoServidor = function (TempoAteSaida,DadosServidores,Tempo){ #Faz a contagem de estados de servidores para cada ano

  NumeroPopulacao = nrow(TempoAteSaida)
  Rodadas = ncol(TempoAteSaida)
  TempoAtivo = TempoAteSaida #tempo de atividade a partir do momento atual
  TaAtivo = 0
  TempoAtivo[DadosServidores$EstadoInicial!=1,] = 0

  #Cada elemento da lista representa, respectivamente, o total de 1=ativos, 2=inv?lidos, 3=aposentados, 4=filhos recebendo benef?cio, 5=c?njuges recebendo benef?cios, 6=mortos sem deixar benefici?rio
  Ativos = matrix(0,Tempo,Rodadas)
  for(r in 1:Rodadas) {
    for (t in 1:Tempo) {
      TaAtivo = TempoAtivo[,r]>=t #Se o tempo de atividade do servidor for maior ou igual ao tempo atual, significa que ele ainda est? ativo
      Ativos[t,r] = sum(TaAtivo)
    }
  }

  return(Ativos)
}
