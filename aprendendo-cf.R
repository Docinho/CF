library(dplyr)

setwd(dir = "CF/")
dados_alemanha <- read.csv(file = "lastfm-matrix-germany.csv")
head(dados_alemanha[, c(1, 200:220)])

# eliminando a coluna de usuario para fazer um collaborative filtering por item 
alemanha_ib <- (dados_alemanha[,!names(dados_alemanha) %in% c("user")])

# Criando uma função cosseno para analisar a similaridade
getCosine <- function(x, y){
  this.cosine <- (sum(x*y)/(sqrt(sum(y*y))))
  return(this.cosine)
}

alemanha_ib_similaridade <- matrix(NA, nrow=ncol(alemanha_ib), ncol = nrow(alemanha_ib), dimnames = list(colnames(alemanha_ib), rownames(alemanha_ib)))

# percorrendo o dataframe para comparar cada uma das musicas
# percorrebdi as colunas
for(coluna in 1:ncol(alemanha_ib)){
# loop pelas colunas para cada coluna
  for(col in 1:ncol(alemanha_ib)) {
    alemanha_ib_similaridade[coluna,col] <- getCosine(as.matrix(alemanha_ib[coluna]), as.matrix(alemanha_ib[col]))
  }
}

dados_alemanha_vizinhos <- matrix(NA, nrow = ncol(alemanha_ib_similaridade), ncol = 11, dimnames = list(colnames(alemanha_ib_similaridade)))

#encontrando os vizinhos
for(elemento in 1:ncol(alemanha_ib)) {
  dados_alemanha_vizinhos[elemento,] <- (t(head(n=11, rownames(alemanha_ib_similaridade[order(alemanha_ib_similaridade[,elemento], decreasing = T),])[elemento])))
}
