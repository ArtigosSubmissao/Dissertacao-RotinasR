EstimaPopEntrada=function(DadosServidores,taxaAumentoSalarial, SalMinimo){
  ##Estima a populaÃ§Ã£o inicial, assumindo que quem se aposenta Ã© substituÃ­do po outro na mesma funÃ§Ã£o e com mesmo salaÂ´rio inicial, e mesmo sexo e idade em que o anterior entrou. 
  
  PopInicial=DadosServidores
  PopInicial$Salario=DadosServidores$Salario*(1+taxaAumentoSalarial)^(-(DadosServidores$Idade-DadosServidores$IdadeEntradaRPPS+DadosServidores$TempoRGPS))
  PopInicial$Salario[PopInicial$Salario<SalMinimo]=SalMinimo
  PopInicial$Idade=DadosServidores$IdadeEntradaRPPS
  PopInicial$EstadoInicial=1
  
  
  ###Evita pessoas ingressando com idade muito avançada, próxima à aposentadoria, que atrasam a convergência do programa. 
  PopInicial$Idade[PopInicial$Idade<18]=18   ##Menores não podem trabalhar no serviço público
  PopInicial$Idade[PopInicial$Idade>40]=floor(runif(length(PopInicial$Idade[PopInicial$Idade>40]), 18,40))   #Acima de 50 anos é possível se aposentar. Para garantir que nenhum está em idade de aposentadoria, todos os novos entrados têm até 50 anos. 
  PopInicial$TempoRGPS[(PopInicial$Idade-PopInicial$TempoRGPS)<18]=PopInicial$Idade[(PopInicial$Idade-PopInicial$TempoRGPS)<18]-18   ##Não aceita que tenha começado a contribuir antes dos 18 anos de idade
  
  ##Não aceita que tenha mais de 25 anos de contribuição, pois implica risco de já poder se aposentar; e programa não convergeria, pois tempo até aposentadoria seria sempre igual a 0. 
  PopInicial$TempoRGPS[PopInicial$TempoRGPS>20]=floor(runif(length(PopInicial$TempoRGPS[PopInicial$TempoRGPS>20]), 0,20)) ##Não aceita que tenha mais de 25 anos de contribuição anterior, pois assim já poderia se aposentar. 
  
  ##Se, pelas alteações, a idade de entrada se tornou maior que a atual, assume que entrou naquele ano
  PopInicial$IdadeEntradaRPPS=PopInicial$Idade
  
  PopInicial$IdadeAposentadoria=estimaIdadeAposentadoriaRPPS(PopInicial)[,1]
  PopInicial$TipoAposentadoria=estimaIdadeAposentadoriaRPPS(PopInicial)[,2]
  
  
  return(PopInicial)
}



