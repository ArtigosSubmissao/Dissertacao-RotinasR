estimaIdadeAposentadoriaRGPS = function (DadosServidores){
  ##Estima idade mÃ?nima de aposentadoria programada.
  {
  #'ParÃ¢metros
  tempoDeContribuicao=15             # Tempo que um servidor precisa contribuir com o regime para aposentadoria.
  idadeMinimaMulherIdade=60          # Idade mÃ?nima para aposentadoria por idade (feminina).
  idadeMinimaHomemIdade=65           # Idade mÃ?nima para aposentadoria por idade (masculina).
  idadeMinimaMulherTempoIdade=0  #N�o existe idade minima   # Idade mÃ?nima para aposentadoria por idade e tempo de contribuiÃ§Ã£o (feminina).
  idadeMinimaHomemTempoIdade=0   #N�o existe idade minima    # Idade mÃ?nima para aposentadoria por idade e tempo de contribuiÃ§Ã£o (masculina).
  contribuicaoMulher=30              # Tempo de contribuiÃ§Ã£o necessÃ¡rio para a mulher se aposentar por idade e tempo de contribuiÃ§Ã£o.
  contribuicaoHomem=35               # Tempo de contribuiÃ§Ã£o necessÃ¡rio para o homem se aposentar por idade e tempo de contribuiÃ§Ã£o.
  idadeMinimaCompulsoria=75          # Idade mÃ?nima para homem/mulher se aposentar compulsÃ³riamente.
  idadeMinMulherProf=0            #N�o existe idade minima  (aposentadoria por tempo)  # Idade mÃ?nima para uma professora se aposentar.
  idadeMinHomemProf=0              #N�o existe idade minima (aposentadoria por tempo) # Idade mÃ?nima para um professor se aposentar.
  contribuicaoMulherProf=25          # ContribuiÃ§Ã£o mÃ?nima para uma mulher que seja professora.
  contribuicaoHomemProf=30           # ContribuiÃ§Ã£o mÃ?nima para um homem professor.
  MaiorIdadeRPPSIdadeMulher = 45     # Maior idade que uma mulpode comeÃ§ar a contribuir com RPPS para se aposentar por idade
  MaiorIdadeRPPSIdadeHomem = 50      # Maior idade que uma mulher pode comeÃ§ar a contribuir com RPPS para se aposentar por idade
  MenorIdadeContribuicaoTempo = 0 #N�o existe idade m�nima   # Menor idade de contribuiÃ§Ã£o para aposentadoria por tempo idade
 # contribuicaoEspecial=25            # Tempo de contribuiÃ§Ã£o necessÃ¡rio para aposentadoria especial
  tipoAposentadoriaTempo = 7         # Valor atribuÃ?do ao tipo de aposentadoria por tempo de contribuiÃ§Ã£o
  tipoAposentadoriaIdade = 8         # Valor atribuÃ?do ao tipo de aposentadoria por idade
 # tipoAposentadoriaEspecial = 9      # Valor atribuÃ?do ao tipo de aposentadoria especial
  tipoAposentadoriaCompulsoria = 10  # Valor atribuÃ?do ao tipo de aposentadoria compulsÃ³ria
  idadeLimite = 300                  # Valor alto atribuÃ?do a idade de aposentadoria de um servidor que nÃ£o poderÃ¡ se aposentar por insalubridade
 # tipoAposentadoriaInvalidez=2       #Nos casos em que a idade atual do aposentado é menor que a idade em que ele poderia se aposenatr por aposentadoria programada, assumiu-se que ele se aposentou por invalidez.  
  #tipoAposentadoriaPensao=5          #Algumas pessoas já começam como pensionistas. Nesse caso, atribuo 5, indicando que é conjuge pensionista. 
  }
  
  {
  TamanhoPopulacao=length(DadosServidores[,1])             #Declara o tamanho da populaÃ§Ã£o inicial.
  DadosServidores[is.na(DadosServidores)] = 0
  Sexo = DadosServidores$Sexo                              #Sexo: 1 - feminino, 2 - masculino.
  Idade = DadosServidores$Idade                            #Idade do servidor.
  EstadoInicial = DadosServidores$EstadoInicial            #Estado em que o servidor se encontra.  
  IdadeEntradaRPPS = DadosServidores$IdadeEntradaRPPS      #Idade que o servidor comeÃ§ou a contribuir com o RPPS.
  TempoRGPS = DadosServidores$TempoRGPS                    #Tempo(em anos) que o servidor contribuiu com o RGPS.
 # tempoContribuicaoEspecial=DadosServidores$Insalubridade  #Tempo(em anos) que o servidor contribuiu em trabalho insalubre.
  SerProfessor = DadosServidores$EProfessor                #Declara se o servidor Ã© professor.
 # atividadeEspecial = DadosServidores$AtividadeEspecial    #Informa se o servidor trabalha com alguma atividade insalubre.
 # IdadeAposentadoria=AposentadoriaPorTempo=AposentadoriaPorIdade=AposentadoriaEspecial=replicate(TamanhoPopulacao,NA)
  IdadeAposentadoria=AposentadoriaPorTempo=AposentadoriaPorIdade=replicate(TamanhoPopulacao,NA)
  TipoAposentadoria = vector (length=TamanhoPopulacao)                                # Criaum vetor para armazenar o tipo de aposentadoria.
  quantidadeMulher <- length(DadosServidores$Sexo[Sexo==1])                           # Quantidade de pessoas do sexo feminino.
  quantidadeHomem <- length(DadosServidores$Sexo[Sexo==2])                            # Quantidade de pessoas do sexo masculino.
  vetIdadeMinMulherIdade <- replicate(quantidadeMulher,idadeMinimaMulherIdade)        # Vetor com a idade mÃ?nima que uma mulher pode se aposentar por idade.
  vetIdadeMinHomemIdade <- replicate(quantidadeHomem,idadeMinimaHomemIdade)           # Vetor com a idade mÃ?nima que um homem pode se aposentar por idade.
  quantidadeNaoProfa <- length(DadosServidores$Sexo[(Sexo==1) & (SerProfessor==0)])   # Quantidade de pessoas do sexo feminino que nÃ£o Ã© professora.
  quantidadeProfa <- length(DadosServidores$Sexo[(Sexo==1) & (SerProfessor==1)])      # Quantidade de pessoas do sexo feminino que Ã© professora.
  quantidadeNaoProf <- length(DadosServidores$Sexo[(Sexo==2) & (SerProfessor==0)])    # Quantidade de pessoas do sexo masculino que nÃ£o Ã© professor.
  quantidadeProf <- length(DadosServidores$Sexo[(Sexo==2) & (SerProfessor==1)])       # Quantidade de pessoas do sexo masculino que Ã© professor.
  vetIdadeMinNaoProfa <- replicate(quantidadeNaoProfa, idadeMinimaMulherTempoIdade)   # Vetor com a idade mÃ?nima que uma mulher que nÃ£o Ã© pode se aposentar por idade.
  vetIdadeMinProfa <- replicate(quantidadeProfa, idadeMinMulherProf)                  # Vetor com a idade mÃ?nima que uma mulher que Ã© pode se aposentar por idade.
  vetIdadeMinNaoProf <- replicate(quantidadeNaoProf, idadeMinimaHomemTempoIdade)      # Vetor com a idade mÃ?nima que um homem que nÃ£o Ã© pode se aposentar por idade.
  vetIdadeMinProf <- replicate(quantidadeProf, idadeMinHomemProf)                     # Vetor com a idade mÃ?nima que um homem que Ã© pode se aposentar por idade.
  #idadeInicioDeContribuicao <- (Idade-(Idade-DadosServidores$IdadeEntradaRPPS+DadosServidores$TempoRGPS))  # Idade que o indivÃ?duo comeÃ§ou a contribuir.
  idadeInicioDeContribuicao <- DadosServidores$IdadeY  # Idade que o indivÃ?duo comeÃ§ou a contribuir.
  }
  
  #TIPOS DE APOSENTADORIA E SEUS RESPECTIVOS PARÃ‚METROS: 
  ## Idade de aposentadoria por idade: AposentadoriaPorIdade
  ## Idade de Aposentadoria por idade e tempo de contribuiÃ§Ã£o: AposentadoriaPorTempo
  ## Idade de aposentadoria especial: AposentadoriaEspecial
  ## Menor idade em que Ã© elegÃ?vel Ã  aposentadoria: IdadeAposentadoria
  
  
  #'Elegibilidade Ã  aposentadoria por idade. 
  #'Se faz necessÃ¡rio ter contribuÃ?do 10 anos no serviÃ§o pÃºblico.
  #'Idade mÃ?nima de 60 anos, se mulher, ou 65 anos, se homem.
  {
  AposentadoriaPorIdade[Sexo==1 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeMulher]=pmax(vetIdadeMinMulherIdade[idadeInicioDeContribuicao[Sexo==1]<=MaiorIdadeRPPSIdadeMulher],Idade[Sexo==1 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeMulher]+1)
  AposentadoriaPorIdade[Sexo==1 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeMulher]=(idadeInicioDeContribuicao)[Sexo==1 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeMulher]+tempoDeContribuicao
  AposentadoriaPorIdade[Sexo==2 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeHomem]=pmax(vetIdadeMinHomemIdade[idadeInicioDeContribuicao[Sexo==2]<=MaiorIdadeRPPSIdadeHomem],Idade[Sexo==2 & (idadeInicioDeContribuicao)<=MaiorIdadeRPPSIdadeHomem]+1)
  AposentadoriaPorIdade[Sexo==2 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeHomem]=(idadeInicioDeContribuicao)[Sexo==2 & (idadeInicioDeContribuicao)>MaiorIdadeRPPSIdadeHomem]+tempoDeContribuicao
  }
  
  
  
  #Elegibilidade Ã  aposentadoria por tempo de contribuiÃ§Ã£o e idade.
  ##Idade mÃ?nima de 60 anos, se mulher, ou 65 anos, se homem.
  ##ContribuiÃ§Ã£o mÃ?nima de 30 anos, se mulher, ou 35 anos, se homem.
  ##Se entrou antes dos 25 anos de idade, aposenta por tempo de contribuiÃ§Ã£o aos 60, se homem, e aos 55, se mulher.
  ##Caso seja professor, esta idade Ã© reduzida em 5 anos, ou seja, 55 para homens e 50 para mulheres.
  ##Ã‰ atribuido o valor 0 caso NÃƒO seja professor e valor 1 caso SEJA professor.
  {
  AposentadoriaPorTempo[(Sexo==1) & (SerProfessor==0)]=pmax((idadeInicioDeContribuicao[(Sexo==1) & (SerProfessor==0)]+contribuicaoMulher),vetIdadeMinNaoProfa,Idade[(Sexo==1) & (SerProfessor==0)]+1)
  AposentadoriaPorTempo[(Sexo==1) & (SerProfessor==1)]=pmax((idadeInicioDeContribuicao[(Sexo==1) & (SerProfessor==1)]+contribuicaoMulherProf),vetIdadeMinProfa,Idade[(Sexo==1) & (SerProfessor==1)]+1)
  AposentadoriaPorTempo[(Sexo==2) & (SerProfessor==0)]=pmax((idadeInicioDeContribuicao[(Sexo==2) & (SerProfessor==0)]+contribuicaoHomem),vetIdadeMinNaoProf,Idade[(Sexo==2) & (SerProfessor==0)]+1)
  AposentadoriaPorTempo[(Sexo==2) & (SerProfessor==1)]=pmax((idadeInicioDeContribuicao[(Sexo==2) & (SerProfessor==1)]+contribuicaoHomemProf),vetIdadeMinProf,Idade[(Sexo==2) & (SerProfessor==1)]+1)
  }
  
  
  #Elegibilidade Ã  aposentadoria especial
  ##Um indivÃ?duo tem direito a aposentadoria especial, caso tenha contribuido por 25 anos sob condiÃ§Ãµes insalubres.
  ##Neste caso nÃ£o Ã© considerado o sexo do indivÃ?duo nem se Ã© professor.
#  AposentadoriaEspecial[tempoContribuicaoEspecial >= contribuicaoEspecial] = Idade[tempoContribuicaoEspecial >= contribuicaoEspecial]+1
#  AposentadoriaEspecial[(atividadeEspecial==0) & (tempoContribuicaoEspecial < contribuicaoEspecial)] = idadeLimite
#  AposentadoriaEspecial[(atividadeEspecial==1) & (tempoContribuicaoEspecial < contribuicaoEspecial)] = Idade[(atividadeEspecial==1)&(tempoContribuicaoEspecial<contribuicaoEspecial)]-tempoContribuicaoEspecial[(atividadeEspecial==1)&(tempoDeContribuicao<contribuicaoEspecial)]+contribuicaoEspecial
  
  # troquei IdadeEntradaRPPS[i]+TempoRGPS[i] por idadeInicioDeContribuicao na linha 102
  for (i in 1:TamanhoPopulacao) {
    if ((AposentadoriaPorTempo[i]<=AposentadoriaPorIdade[i]) & (AposentadoriaPorTempo[i]<=idadeMinimaCompulsoria) & (AposentadoriaPorTempo[i]-idadeInicioDeContribuicao[i]>=MenorIdadeContribuicaoTempo)) {
      IdadeAposentadoria[i] = AposentadoriaPorTempo[i]
      TipoAposentadoria[i] = tipoAposentadoriaTempo
    } else if ((AposentadoriaPorIdade[i]<=AposentadoriaPorTempo[i]) & (AposentadoriaPorIdade[i]<=idadeMinimaCompulsoria)) {
      IdadeAposentadoria[i] = AposentadoriaPorIdade[i]
      TipoAposentadoria[i] = tipoAposentadoriaIdade
    } else {
      IdadeAposentadoria[i] = idadeMinimaCompulsoria
      TipoAposentadoria[i] = tipoAposentadoriaCompulsoria
    }
  }
  
  
  ###Corrige idade de aposentadoria para quando a pessoa j� deveria ter se aposentado. 
  IdadeAposentadoria[IdadeAposentadoria<=DadosServidores$Idade & EstadoInicial==1]=DadosServidores$Idade[IdadeAposentadoria<=Idade & EstadoInicial==1]+1
  
  #IdadeAposentadoria=apply(rbind(AposentadoriaPorIdade,AposentadoriaPorTempo,replicate(TamanhoPopulacao,70)),MARGIN=2,FUN=min)
  #aqui TipoAposentadoria[EstadoInicial==3 & Idade<=IdadeAposentadoria]  = tipoAposentadoriaInvalidez
  #aqui IdadeAposentadoria[EstadoInicial==3 & Idade<=IdadeAposentadoria] = Idade[EstadoInicial==3 & Idade<=IdadeAposentadoria]-1
  
  #aqui  TipoAposentadoria[EstadoInicial==5]  = tipoAposentadoriaPensao
  #aqui IdadeAposentadoria[EstadoInicial==5] = Idade[EstadoInicial==5]-10
  #if (DadosRPPS$TempoCriacaoRPPS<10) IdadeAposentadoria[EstadoInicial==5]=max(0,Idade[EstadoInicial==5]-DadosRPPS$TempoCriacaoRPPS+1) 
  
  
  #png(paste("Idade de aposentadoria por sexo - ", pop, "Servidores.png"))
  #ymin=min(r)
  #ymax=max(r)
  #plot(IdadeEntradaRPPS[Sexo==1],r[Sexo==1],xlab="Idade de entrada", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
  #points(IdadeEntradaRPPS[Sexo==2],r[Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
  #legend(x=18, y = 70, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
  #dev.off()
  
  

  return(cbind(IdadeAposentadoria,TipoAposentadoria))
}
