idadeTipoAposentadoriaRPPS = function (DadosServidores){
  ##Estima idade mínima de aposentadoria programada.
  
  #'Parâmetros
  tempoDeContribuicao=10             # Tempo que um servidor precisa contribuir com o regime para aposentadoria.
  idadeMinimaMulherIdade=60          # Idade mínima para aposentadoria por idade (feminina).
  idadeMinimaHomemIdade=65           # Idade mínima para aposentadoria por idade (masculina).
  idadeMinimaMulherTempoIdade=55     # Idade mínima para aposentadoria por idade e tempo de contribuição (feminina).
  idadeMinimaHomemTempoIdade=60      # Idade mínima para aposentadoria por idade e tempo de contribuição (masculina).
  contribuicaoMulher=30              # Tempo de contribuição necessário para a mulher se aposentar por idade e tempo de contribuição.
  contribuicaoHomem=35               # Tempo de contribuição necessário para o homem se aposentar por idade e tempo de contribuição.
  idadeMinimaCompulsoria=75          # Idade mínima para homem/mulher se aposentar compulsóriamente.
  idadeMinMulherProf=55              # Idade mínima para uma professora se aposentar.
  idadeMinHomemProf=60               # Idade mínima para um professor se aposentar.
  contribuicaoMulherProf=25          # Contribuição mínima para uma mulher que seja professora.
  contribuicaoHomemProf=30           # Contribuição mínima para um homem professor.
  MaiorIdadeRPPSIdadeMulher = 50     # Maior idade que uma mulher pode começar a contribuir com RPPS para se aposentar por idade
  MaiorIdadeRPPSIdadeHomem = 55      # Maior idade que uma mulher pode começar a contribuir com RPPS para se aposentar por idade
  MenorIdadeContribuicaoTempo = 25   # Menor idade de contribuição para aposentadoria por tempo idade
  contribuicaoEspecial=25            # Tempo de contribuição necessário para aposentadoria especial
  tipoAposentadoriaTempo = 7         # Valor atribuído ao tipo de aposentadoria por tempo de contribuição
  tipoAposentadoriaIdade = 8         # Valor atribuído ao tipo de aposentadoria por idade
 # tipoAposentadoriaEspecial = 9      # Valor atribuído ao tipo de aposentadoria especial
  tipoAposentadoriaCompulsoria = 10  # Valor atribuído ao tipo de aposentadoria compulsória
  idadeLimite = 300                  # Valor alto atribuído a idade de aposentadoria de um servidor que não poderá se aposentar por insalubridade
  
  
  
  TamanhoPopulacao=length(DadosServidores[,1])             #Declara o tamanho da população inicial.
  DadosServidores[is.na(DadosServidores)] = 0
  Sexo = DadosServidores$Sexo                              #Sexo: 1 - feminino, 2 - masculino.
  Idade = DadosServidores$Idade                            #Idade do servidor.
  EstadoInicial = DadosServidores$EstadoInicial            #Estado em que o servidor se encontra.  
  IdadeEntradaRPPS = DadosServidores$IdadeEntradaRPPS      #Idade que o servidor começou a contribuir com o RPPS.
  TempoRGPS = DadosServidores$TempoRGPS                    #Tempo(em anos) que o servidor contribuiu com o RGPS.
 # tempoContribuicaoEspecial=DadosServidores$Insalubridade  #Tempo(em anos) que o servidor contribuiu em trabalho insalubre.
  SerProfessor = DadosServidores$EProfessor                #Declara se o servidor é professor.
  #atividadeEspecial = DadosServidores$AtividadeEspecial    #Informa se o servidor trabalha com alguma atividade insalubre.
  IdadeAposentadoria=AposentadoriaPorTempo=AposentadoriaPorIdade=AposentadoriaEspecial=replicate(TamanhoPopulacao,NA)
  TipoAposentadoria = vector (length=TamanhoPopulacao)                                # Criaum vetor para armazenar o tipo de aposentadoria.
  quantidadeMulher <- length(DadosServidores$Sexo[Sexo==1])                           # Quantidade de pessoas do sexo feminino.
  quantidadeHomem <- length(DadosServidores$Sexo[Sexo==2])                            # Quantidade de pessoas do sexo masculino.
  vetIdadeMinMulherIdade <- replicate(quantidadeMulher,idadeMinimaMulherIdade)        # Vetor com a idade mínima que uma mulher pode se aposentar por idade.
  vetIdadeMinHomemIdade <- replicate(quantidadeHomem,idadeMinimaHomemIdade)           # Vetor com a idade mínima que um homem pode se aposentar por idade.
  quantidadeNaoProfa <- length(DadosServidores$Sexo[(Sexo==1) & (SerProfessor==0)])   # Quantidade de pessoas do sexo feminino que não é professora.
  quantidadeProfa <- length(DadosServidores$Sexo[(Sexo==1) & (SerProfessor==1)])      # Quantidade de pessoas do sexo feminino que é professora.
  quantidadeNaoProf <- length(DadosServidores$Sexo[(Sexo==2) & (SerProfessor==0)])    # Quantidade de pessoas do sexo masculino que não é professor.
  quantidadeProf <- length(DadosServidores$Sexo[(Sexo==2) & (SerProfessor==1)])       # Quantidade de pessoas do sexo masculino que é professor.
  vetIdadeMinNaoProfa <- replicate(quantidadeNaoProfa, idadeMinimaMulherTempoIdade)   # Vetor com a idade mínima que uma mulher que não é pode se aposentar por idade.
  vetIdadeMinProfa <- replicate(quantidadeProfa, idadeMinMulherProf)                  # Vetor com a idade mínima que uma mulher que é pode se aposentar por idade.
  vetIdadeMinNaoProf <- replicate(quantidadeNaoProf, idadeMinimaHomemTempoIdade)      # Vetor com a idade mínima que um homem que não é pode se aposentar por idade.
  vetIdadeMinProf <- replicate(quantidadeProf, idadeMinHomemProf)                     # Vetor com a idade mínima que um homem que é pode se aposentar por idade.
  idadeInicioDeContribuicao <- (Idade-(Idade-DadosServidores$IdadeEntradaRPPS+DadosServidores$TempoRGPS))  # Idade que o indivíduo começou a contribuir.
  
  
 #TIPOS DE APOSENTADORIA E SEUS RESPECTIVOS PARÂMETROS: 
  ## Idade de aposentadoria por idade: AposentadoriaPorIdade
  ## Idade de Aposentadoria por idade e tempo de contribuição: AposentadoriaPorTempo
  ## Idade de aposentadoria especial: AposentadoriaEspecial
  ## Menor idade em que é elegível à aposentadoria: IdadeAposentadoria
  
  
  #'Elegibilidade à aposentadoria por idade. 
  #'Se faz necessário ter contribuído 10 anos no serviço público.
  #'Idade mínima de 60 anos, se mulher, ou 65 anos, se homem.
  
  AposentadoriaPorIdade[Sexo==1 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeMulher]=pmax(vetIdadeMinMulherIdade[idadeInicioDeContribuicao[Sexo==1]<=MaiorIdadeRPPSIdadeMulher],Idade[Sexo==1 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeMulher]+1)
  AposentadoriaPorIdade[Sexo==1 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeMulher]=(idadeInicioDeContribuicao)[Sexo==1 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeMulher]+tempoDeContribuicao
  AposentadoriaPorIdade[Sexo==2 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeHomem]=pmax(vetIdadeMinHomemIdade[idadeInicioDeContribuicao[Sexo==2]<=MaiorIdadeRPPSIdadeHomem],Idade[Sexo==2 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeHomem]+1)
  AposentadoriaPorIdade[Sexo==2 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeHomem]=(idadeInicioDeContribuicao)[Sexo==2 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeHomem]+tempoDeContribuicao
  
  
  
  
  #Elegibilidade à aposentadoria por tempo de contribuição e idade.
  ##Idade mínima de 60 anos, se mulher, ou 65 anos, se homem.
  ##Contribuição mínima de 30 anos, se mulher, ou 35 anos, se homem.
  ##Se entrou antes dos 25 anos de idade, aposenta por tempo de contribuição aos 60, se homem, e aos 55, se mulher.
  ##Caso seja professor, esta idade é reduzida em 5 anos, ou seja, 55 para homens e 50 para mulheres.
  ##É atribuido o valor 0 caso NÃO seja professor e valor 1 caso SEJA professor.
  
  AposentadoriaPorTempo[(Sexo==1) & (SerProfessor==0)]=pmax((idadeInicioDeContribuicao[(Sexo==1) & (SerProfessor==0)]+contribuicaoMulher),vetIdadeMinNaoProfa,Idade[(Sexo==1) & (SerProfessor==0)]+1)
  AposentadoriaPorTempo[(Sexo==1) & (SerProfessor==1)]=pmax((idadeInicioDeContribuicao[(Sexo==1) & (SerProfessor==1)]+contribuicaoMulherProf),vetIdadeMinProfa,Idade[(Sexo==1) & (SerProfessor==1)]+1)
  AposentadoriaPorTempo[(Sexo==2) & (SerProfessor==0)]=pmax((idadeInicioDeContribuicao[(Sexo==2) & (SerProfessor==0)]+contribuicaoHomem),vetIdadeMinNaoProf,Idade[(Sexo==2) & (SerProfessor==0)]+1)
  AposentadoriaPorTempo[(Sexo==2) & (SerProfessor==1)]=pmax((idadeInicioDeContribuicao[(Sexo==2) & (SerProfessor==1)]+contribuicaoHomemProf),vetIdadeMinProf,Idade[(Sexo==2) & (SerProfessor==1)]+1)
  
  
  
  #Elegibilidade à aposentadoria especial
  ##Um indivíduo tem direito a aposentadoria especial, caso tenha contribuido por 25 anos sob condições insalubres.
  ##Neste caso não é considerado o sexo do indivíduo nem se é professor.
  #AposentadoriaEspecial[tempoContribuicaoEspecial >= contribuicaoEspecial] = Idade[tempoContribuicaoEspecial >= contribuicaoEspecial]+1
  #AposentadoriaEspecial[(atividadeEspecial==0) & (tempoContribuicaoEspecial < contribuicaoEspecial)] = idadeLimite
  #AposentadoriaEspecial[(atividadeEspecial==1) & (tempoContribuicaoEspecial < contribuicaoEspecial)] = Idade[(atividadeEspecial==1)&(tempoContribuicaoEspecial<contribuicaoEspecial)]-tempoContribuicaoEspecial[(atividadeEspecial==1)&(tempoDeContribuicao<contribuicaoEspecial)]+contribuicaoEspecial
  
  
  
  
  
  for (i in 1:TamanhoPopulacao) {
    if (AposentadoriaPorTempo[i]<=AposentadoriaPorIdade[i] & AposentadoriaPorTempo[i]<=idadeMinimaCompulsoria & (AposentadoriaPorTempo[i]-IdadeEntradaRPPS[i]+TempoRGPS[i])>=MenorIdadeContribuicaoTempo) {
      IdadeAposentadoria[i] = AposentadoriaPorTempo[i]
      TipoAposentadoria[i] = tipoAposentadoriaTempo
    } else if (AposentadoriaPorIdade[i]<=AposentadoriaPorTempo[i] & AposentadoriaPorIdade[i]<=idadeMinimaCompulsoria) {
      IdadeAposentadoria[i] = AposentadoriaPorIdade[i]
      TipoAposentadoria[i] = tipoAposentadoriaIdade
    } else {
      IdadeAposentadoria[i] = idadeMinimaCompulsoria
      TipoAposentadoria[i] = tipoAposentadoriaCompulsoria
    }
  }
  #IdadeAposentadoria=apply(rbind(AposentadoriaPorIdade,AposentadoriaPorTempo,replicate(TamanhoPopulacao,70)),MARGIN=2,FUN=min)
  IdadeAposentadoria[EstadoInicial!=1] = NA
  TipoAposentadoria[EstadoInicial!=1] = NA
  
  
  
  #png(paste("Idade de aposentadoria por sexo - ", pop, "Servidores.png"))
  #ymin=min(r)
  #ymax=max(r)
  #plot(IdadeEntradaRPPS[Sexo==1],r[Sexo==1],xlab="Idade de entrada", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
  #points(IdadeEntradaRPPS[Sexo==2],r[Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
  #legend(x=18, y = 70, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
  #dev.off()
  
  return(cbind(IdadeAposentadoria,TipoAposentadoria))
}

