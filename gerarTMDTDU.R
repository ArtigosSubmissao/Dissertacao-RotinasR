### ESSE ALGORITIMO CRIA A TABELA "TMD", QUE ? USADA 
### essa fun??o ? a que est? todas as 4 tabelas (duas de mortalidade e duas de invalidez)
### ambas do mesmo tamanho, s? muda que uma ? masculina e outra feminina. Elas tem o mesmo
### titulo das colunas para que quando o shiny voltar com a palavra, quando a fun??o que voc?
### criou ler, conseguir capturar a coluna desejada para ambas tabelas


gerarTMDTDU<- function(nomeTabelaMortalidade, nomeTabelaInvalidez){
 
 
#  tryCatch({
#    #' carregando as tabeas de invalidez e mortalidade
#    load(paste(DIRETORIO_RAIZ,"tabelas.RData",sep = ""))
#   
#  },error = function(e){
#    lancarErro("Erro ao carregar arquivo tabelas.RData", "geraTMD",e)
#    stop()
#  })
  
  tryCatch({
  tabelaMorteFem = nomeTabelaMortalidade$mort_fem
 
  tabelaInvalidezFem = nomeTabelaInvalidez$inv_fem
  },error = function(e){
    lancarErro("Erro na busca da tabela buscarColunaTabela","geraTMD",e)
    stop()
  })
  
  tryCatch({
  tabelaMorteMasc = nomeTabelaMortalidade$mort_masc
  tabelaInvalidezMasc = nomeTabelaInvalidez$inv_masc
},error = function(e){
  lancarErro("Erro na busca da tabela buscarColunaTabela","geraTMD",e)
  stop()
})
 
  tryCatch({
  dadosLxFem = gerarLx(tabelaMorteFem, tabelaInvalidezFem,"F")
  dadosLxMasc = gerarLx(tabelaMorteMasc, tabelaInvalidezMasc, "M")
  idade <- c(0:(min(length(tabelaMorteMasc), length(tabelaInvalidezMasc))-1)) #
  TMD = cbind(idade,dadosLxFem, dadosLxMasc)
  },error = function(e){
    lancarErro("Erro ao gerar dados Lx","geraTMD",e)
    stop()
  })
  
  
  
  #print(paste("tamanho: ",length(tabelaMorteMasc)))
  
  TDU = gerarTDU(tabelaMorteFem,tabelaMorteMasc)

  
  return(list(as.data.frame(TMD), TDU))
  
}









          