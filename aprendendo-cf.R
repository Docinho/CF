library(dplyr)

setwd(dir = "Área de Trabalho/CF/")
data.germany <- read.csv(file = "lastfm-matrix-germany.csv")
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


## rankeando usuarios

# Create a score function with a formula: sumproduct(purchaseHistory, similarities)/sum(similarities)
# getScore <- function(history, similarities) {
#   x <- sum(history*similarities)/sum(similarities)
#   x
# }
# 
# holder<-matrix(NA, nrow = nrow(dados_alemanha), ncol=ncol(dados_alemanha)-1, dimnames = list((dados_alemanha$user), colnames(dados_alemanha[-1])))
# 
# for(usuario in 1:nrow(holder)) {
#   for(banda in 1:ncol(holder)){
#     user <- rownames(holder)[usuario]
#     product <- colnames(holder)[banda]
#     
#     if(as.integer(dados_alemanha[dados_alemanha$user==user, product]) == 1) {
#       holder[usuario, banda]<-""
#     }else {
#       topN <- ((head(n=11,(dados_alemanha_ib_similaridade[order(dados_alemanha_ib_similaridade[,product],decreasing = T),][product]))))
#       topN.names <- as.character(rownames(topN))
#       topN.similarities <- as.numeric(topN[,1])
#       
#       topN.names <- topN.names[-1]
#       topN.similarities <- topN.similarities[-1]
#       
#       topN.purchase <- dados_alemanha[,c("user", topN.names)]
#       topN.userPurchases<-topN.purchase[topN.purchase$user==user,]
#       topN.userPurchases<-as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
#      
#       holder[usuario, banda] <- getScore(similarities = topN.similarities, history = topN.userPurchases)
#     }
#   }
# }
#   
# dados_alemanha_scores_usuario <- holder
# dados.alemanha.score.usuario.holder <- matrix(NA, nrow = nrow(dados_alemanha_scores_usuario), ncol = 100, dimnames = list(rownames(dados_alemanha_scores_usuario)))
# for(i in 1:nrow(dados_alemanha_scores_usuario)) {
#   dados.alemanha.score.usuario.holder[i] <- names(head(n = 100, (dados_alemanha_scores_usuario[,order(dados_alemanha_scores_usuario[i,], decreasing = T)])[i,]))
# }
# #Choose an item and check if a user consumed that item
# #Get the similarities of that item’s top X neighbours
# #Get the consumption record of the user of the top X neighbours
# #Calculate the score 
# 

data.germany.ibs <- (data.germany[,!(names(data.germany) %in% c("user"))])

# Create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# Create a placeholder dataframe listing item vs. item
holder <- matrix(NA, nrow=ncol(data.germany.ibs),ncol=ncol(data.germany.ibs),dimnames=list(colnames(data.germany.ibs),colnames(data.germany.ibs)))
data.germany.ibs.similarity <- as.data.frame(holder)

# Lets fill in those empty spaces with cosine similarities
for(i in 1:ncol(data.germany.ibs)) {
  for(j in 1:ncol(data.germany.ibs)) {
    data.germany.ibs.similarity[i,j]= getCosine(data.germany.ibs[i],data.germany.ibs[j])
  }
}


# Get the top 10 neighbours for each
data.germany.neighbours <- matrix(NA, nrow=ncol(data.germany.ibs.similarity),ncol=11,dimnames=list(colnames(data.germany.ibs.similarity)))

for(i in 1:ncol(data.germany.ibs)) 
{
  data.germany.neighbours[i,] <- (t(head(n=11,rownames(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,i],decreasing=TRUE),][i]))))
}
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

# A placeholder matrix
holder <- matrix(NA, nrow=nrow(data.germany),ncol=ncol(data.germany)-1,dimnames=list((data.germany$user),colnames(data.germany[-1])))

# Loop through the users (rows)
for(i in 1:nrow(holder)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(holder)) 
  {
    # Get the user's name and th product's name
    # We do this not to conform with vectors sorted differently 
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
    if(as.integer(data.germany[data.germany$user==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same song
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- data.germany[,c("user",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # close else statement
  } # end product for loop   
} # end user for loop

# Output the results to a file
data.germany.user.scores <- holder
write.csv(file="final-user-scores.csv",data.germany.user.scores)

# Lets make our recommendations pretty
data.germany.user.scores.holder <- matrix(NA, nrow=nrow(data.germany.user.scores),ncol=100,dimnames=list(rownames(data.germany.user.scores)))
for(i in 1:nrow(data.germany.user.scores)) 
{
  data.germany.user.scores.holder[i,] <- names(head(n=100,(data.germany.user.scores[,order(data.germany.user.scores[i,],decreasing=TRUE)])[i,]))
}