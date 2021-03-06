EstimaPopEntrada=function(DadosServidores,taxaAumentoSalarial, SalMinimo){
  ##Estima a população inicial, assumindo que quem se aposenta é substituído po outro na mesma função e com mesmo sala´rio inicial, e mesmo sexo e idade em que o anterior entrou. 
  
  PopInicial=DadosServidores
  PopInicial$Salario=DadosServidores$Salario*(1+taxaAumentoSalarial)^(-(DadosServidores$Idade-DadosServidores$IdadeEntradaRPPS+DadosServidores$TempoRGPS))
  PopInicial$Salario[PopInicial$Salario<SalMinimo]=SalMinimo
  PopInicial$Idade=DadosServidores$IdadeEntradaRPPS
  PopInicial$EstadoInicial=1
  
  
  ###Evita pessoas ingressando com idade muito avan�ada, pr�xima � aposentadoria, que atrasam a converg�ncia do programa. 
  PopInicial$Idade[PopInicial$Idade<18]=18   ##Menores n�o podem trabalhar no servi�o p�blico
  PopInicial$Idade[PopInicial$Idade>40]=floor(runif(length(PopInicial$Idade[PopInicial$Idade>40]), 18,40))   #Acima de 50 anos � poss�vel se aposentar. Para garantir que nenhum est� em idade de aposentadoria, todos os novos entrados t�m at� 50 anos. 
  PopInicial$TempoRGPS[(PopInicial$Idade-PopInicial$TempoRGPS)<18]=PopInicial$Idade[(PopInicial$Idade-PopInicial$TempoRGPS)<18]-18   ##N�o aceita que tenha come�ado a contribuir antes dos 18 anos de idade
  
  ##N�o aceita que tenha mais de 25 anos de contribui��o, pois implica risco de j� poder se aposentar; e programa n�o convergeria, pois tempo at� aposentadoria seria sempre igual a 0. 
  PopInicial$TempoRGPS[PopInicial$TempoRGPS>20]=floor(runif(length(PopInicial$TempoRGPS[PopInicial$TempoRGPS>20]), 0,20)) ##N�o aceita que tenha mais de 25 anos de contribui��o anterior, pois assim j� poderia se aposentar. 
  
  ##Se, pelas altea��es, a idade de entrada se tornou maior que a atual, assume que entrou naquele ano
  PopInicial$IdadeEntradaRPPS=PopInicial$Idade
  
  PopInicial$IdadeAposentadoria=estimaIdadeAposentadoriaRPPS(PopInicial)[,1]
  PopInicial$TipoAposentadoria=estimaIdadeAposentadoriaRPPS(PopInicial)[,2]
  
  
  return(PopInicial)
}



