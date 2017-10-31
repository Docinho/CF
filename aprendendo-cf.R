library(dplyr)

setwd(dir = "CF/")
dados_alemanha <- read.csv(file = "lastfm-matrix-germany.csv")
head(dados_alemanha[, c(1, 200:220)])

# eliminando a coluna de usuario para fazer um collaborative filtering por item 
alemanha_ib <- (dados_alemanha[,!(names(dados_alemanha) %in% c("user"))])

# Criando uma função cosseno para analisar a similaridade
getCosine <- function(x, y){
  this.cosine <- (sum(x*y)/(sqrt(sum(x*x))*(sqrt(sum(y*y)))))
  return(this.cosine)
}

holder <- matrix(NA, nrow=ncol(alemanha_ib), ncol = ncol(alemanha_ib), dimnames = list(colnames(alemanha_ib), colnames(alemanha_ib)))
dados_alemanha_ib_similaridade <- as.data.frame(holder)
# percorrendo o dataframe para comparar cada uma das musicas
# percorrebdi as colunas
for(coluna in 1:ncol(alemanha_ib)){
# loop pelas colunas para cada coluna
  for(col in 1:ncol(alemanha_ib)) {
    dados_alemanha_ib_similaridade[coluna,col] = getCosine(alemanha_ib[coluna], alemanha_ib[col])
  }
}

dados_alemanha_vizinhos <- matrix(NA, nrow = ncol(dados_alemanha_ib_similaridade), ncol = 11, dimnames = list(colnames(dados_alemanha_ib_similaridade)))

#encontrando os vizinhos
for(elemento in 1:ncol(alemanha_ib)) {
  dados_alemanha_vizinhos[elemento,] <- (t(head(n=11, rownames(dados_alemanha_ib_similaridade[order(dados_alemanha_ib_similaridade[,elemento], decreasing = T),][elemento]))))
}
