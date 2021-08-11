#' rodaSimulacao function
#' @param descricaoErro - descrição textual do erro
#' @param nomeFuncao - nome da função onde ocorreu o erro
lancarErro = function(descricaoErro, nomeFuncao, erro){
  calls <- sys.calls()
  
  print("==================== TRECHO DE CODIGO =========================")
  for(i in 1:length(calls)){
    callTxt = as.character(calls[[i]]) 
    for(k in 1:length(callTxt)){
      txt = callTxt[k]
      
      if(str_detect(txt, "lancarErro"))
        print(calls[[i]])
    }
  }
  print("====================ERRO=========================")
  #Descricao do erro
  print(paste("DESCRICAO: ", descricaoErro))
  print(paste("FUNCAO: ", nomeFuncao))
  print(paste("Erro: ", erro))
  print("--------------------------------------------------")
  
 
  
}