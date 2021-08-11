#' Função gera a tabela de decremento unico 
#' @param value
#' @return value
#' @export

gerarTDU <- function(tabelaMorteFem,tabelaMorteMasc){
  
  
  # print(dim(tabelaMorteMasc)[1])
  lxMorte.M=rep(NA,length(tabelaMorteMasc[[1]]))
  lxMorte.M[1]= 100000
  
  tryCatch({
    
    for(i in 2:length(tabelaMorteMasc)) {
      lxMorte.M[i] = lxMorte.M[i-1]-(lxMorte.M[i-1]*tabelaMorteMasc[i-1])
    }
  },error = function(e){
    lancarErro("Erro no algoritmo ", "gerarTDU",e)
    stop()
  })
  
 
  
  lxMorte.F=rep(NA,length(tabelaMorteFem))
  lxMorte.F [1]= 100000
  
  tryCatch({
    for(i in 2:length(tabelaMorteFem)) {
      lxMorte.F[i] = lxMorte.F[i-1]-(lxMorte.F[i-1]*tabelaMorteFem[i-1])
    }
  },error = function(e){
    lancarErro("Erro no algoritmo ", "gerarTDU",e)
    stop()
  })
  
  
  idade <- 1:length(tabelaMorteMasc)
  
  TabuasMorte <- data.frame(idade,lxMorte.F,lxMorte.M)
  
  return(TabuasMorte)
}


