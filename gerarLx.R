#' Constroi os valores de Lx de morte e invalidez total para ambos os sexos
#' 
#' @param tabelaMorte - Matriz que representa a tabela de morte
#' @param tabelaIvalidez - Matriz que representa a tabela de invalidez
gerarLx <- function(tabelaMorte, tabelaInvalidez, sexo){
  ### essa parte é a que vai ser substituia pela função, repare que pra escolher a tabela
  ### eu estou usando o "$" para escolher qual tabua eu quero na tabela.
  
  
  #mortalidade_dados_m = as.matrix(tab.mort.masc$IBGE_2010)
  # invalidez_dados_m = as.matrix 
  mortalidade_dados_m = as.matrix(tabelaMorte)
  invalidez_dados_m = as.matrix (tabelaInvalidez)
  
  ##As tabelas podem ser de tamanhos diferentes. Então, padronizei pelo tamanho da maior para cosntruir a TMD. 
  IdadeMaxima=min(length(tabelaMorte), length(tabelaInvalidez))
  mortalidade_dados_m=mortalidade_dados_m[1:IdadeMaxima]
  invalidez_dados_m=invalidez_dados_m[1:IdadeMaxima]
  
  # mortalidade_dados_m = tabelaMorte
  #  invalidez_dados_m = tabelaInvalidez
  
  ############################################################################
  
  
  qMorteM = (mortalidade_dados_m*(1-(invalidez_dados_m/2)))
  q_inv_m =(invalidez_dados_m*(1-((1/2)*mortalidade_dados_m)))
  q_total_m = qMorteM + q_inv_m
  
  ###### Achando "lxt" por meio de funÃ§Ã£o #######
  lxTotalTMD.M=rep(NA,length(q_total_m))
  lxTotalTMD.M [1]= 1
  for(i in 2:length(q_total_m)) {
    lxTotalTMD.M[i] = lxTotalTMD.M[i-1]-(lxTotalTMD.M[i-1]*q_total_m[i-1])
  }
  ############################################
  d_mort_m = lxTotalTMD.M*qMorteM
  d_inv_m = lxTotalTMD.M*q_inv_m
  d_total_m = lxTotalTMD.M*q_total_m
  ###### achar "lxMorteTMD" e "lxInvalidezTMD"  ######
  
  lxMorteTMD.M = rev(cumsum(rev(d_mort_m)))
  lxInvalidezTMD.M = rev(cumsum(rev(d_inv_m)))
  
  #idade <- c(0:(NROW(tabs.mort.e.inv)-1))
  lxMorteTMD = lxMorteTMD.M
  lxInvalidezTMD = lxInvalidezTMD.M
  lxTotalTMD = lxTotalTMD.M
  
  
  tmd = cbind(lxMorteTMD,lxInvalidezTMD,lxTotalTMD)
  sexo = paste(".",sexo, sep="")
  
  for (i in 1:ncol(tmd)) {
    colnames(tmd)[i] = paste(colnames(tmd)[i], sexo,sep="")
  }
  
  
  return(tmd)
  
}




