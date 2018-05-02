#################################################
## INF-0611 - Trabalho Final
##        Rafael Fernando Ribeiro
##        Thiago Gomes Mar?al Pereira
#################################################

library(ggplot2)
require(reshape2)
library(wvtool)
library(OpenImageR)

#setwd("C:\\Users\\rafae\\Documents\\INF0611_Final")
setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-611/Tarefas/INF0611_TrabalhoFinal")

#-----------------------------------------------------------
# Fun??es
# ----------------------------------------------------------

# Recurrence Plot
recurrence_plot <- function(vector) {
  rp <- matrix(nrow = length(vector), ncol=length(vector))
  for (i in 1:length(vector)) {
    for (j in 1:length(vector)) {
      if (abs(vector[i] - vector[j]) <= episilon) {
        rp[i,j] <- 1
      } else {
        rp[i,j] <- 0
      }
    }
  }
  rp
}

#extrair o vetor de carecteristicas (255 dimensoes) usando LBP
extrairLBP <- function(img){
  desc <- lbp(img, 1)
  h <- hist(desc$lbp.ori, plot=FALSE, breaks = 0:255)
  return(h$counts)
}

#L1 ou Manhattan
dist.L1 <- function(x, y){
  d = 0
  for (i in c(1:length(x))) {
    d = d+abs(x[i]-y[i])
  }
  return(d)
}

# Buscar K imagens mais pr?ximas
#parametros:
#  M: matriz que comtem os vetore de carateristicas
#  query: o nome do arquivo de consulta
#  K: o numero de imagens mais proximas a serem devolvidas
#retorno:
#  a lista dos nomes das K-imagens mais proximas

buscarMaisProximos <- function(M, query, dist.func, K){
  distancias <- apply(M, 2, dist.func, query)
  
  distancias <- order(distancias, decreasing = FALSE)
  
  return(distancias[1:K])
}

# Retorna a lista de imagens, baseado nos ?ndices
lista_imagens <- function (vector) {
  return (db_original[vector, 1])
}

# normalizar dados
normalize <- function (values) {
  print(dim(values))
  (values - mean(values))/sd(values)
}

# Calculo de Distancia L2
dist.L2 <- function(x, y){
  d = 0
  for (i in c(1:length(x))) {
    d = d+(x[i]-y[i])^2
  }
  return(sqrt(d))
}

# Calculo de Distancia Cossenos
dist.cos <- function(x, y){
  return( sum(x*y)/sqrt(sum(x^2)*sum(y^2)) )
}

# Calculo das distancias
dist.calc <- function(db, query, dist.func) {
  print("Calculando distancias")
  qrow <- nrow(query)
  qcol <- ncol(query)
  drow <- nrow(db)
  dcol <- ncol(db)
  
  #l <- list()
  proximos <- matrix(nrow = length(lbp_queries[1,]), ncol = MAIS_PROXIMOS)
  for (i in c(1:length(lbp_queries[1,]))) {
    proximos[i,] <- buscarMaisProximos(lbp_originais, lbp_queries[,i], dist.func, MAIS_PROXIMOS)
    proximos[i,] <- lista_imagens(proximos[i,])
  }
  
  return(proximos)
}

calc.precision <- function(q, distances, k) {
  tp <- sum(distances[1:k] == q)
  fp <- k
  return (tp/fp)
}

calc.recall <- function(q, db, distances, k) {
  tp <- sum(distances[1:k] == q)
  fn <- sum(db[,1] == q)
  return (tp/fn)
}

# calcular precision e recall
calc.prec_recall <- function(db, qy, distances, k) {
  print("Calculando precision recall")
  l <- list()
  qrow <- nrow(qy)
  ks <- c(seq(5, 100, by = 5))
  
  for (k in c(1:(100/5))) {
    precision <- c()
    recall <- c()
    for (q in c(1:qrow)) {
      precision <- c(precision, calc.precision(distances[q,1], distances[q,-1], ks[k]))
      recall <- c(recall, calc.recall(distances[q,1], db, distances[q,-1], ks[k]))
    }
    l[[k]] <- list(k=ks[k],  
                   precision = precision, precision_mean=mean(precision), 
                   recall = recall,  recall_mean=mean(recall))
  }
  return (l)
}

descriptor.get <- function(db, descriptor, rp_images = NULL) {
  
  if (descriptor == "HOG") {
    hog_db <- sapply(rp_images, HOG,cells = 8, orientations = 6)
    hog_db <- t(hog_db)
    db_new <- cbind(as.data.frame(db[,1]),as.data.frame(hog_db))
  }
  else if (descriptor == "LBP") {
    print("Extraindo LBP")
    lbp_db <- sapply(rp_images, extrairLBP)
    lbp_db <- t(lbp_db)
    db_new <- cbind(as.data.frame(db[,1]),as.data.frame(lbp_db))
  }
  else{
    db_new <- db
  }
  return(db_new)
}

plot.classes <- function (db) {
  # plotar as classes
  for (i in unique(db[,1])){
    
    data <- data.frame(cbind(t(db[db[,1]==i, 2:129][1:3,]), vars=c(1:128)))
    colnames(data) <- c("a","b","c", "time")
    df <- melt(data ,  id.vars = 'time', variable.name = 'series')
    #ggplot(df, aes(time,value)) + geom_line(aes(colour = series))
    ggplot(df, aes(time,value)) + geom_line() + facet_grid(series ~ .) + ggtitle(paste("Classe", i))
    ggsave(paste("classe_",i, ".png",sep = ""))
  }
}

plot.prec_recall <- function(prec_recall, title) {
  p <- c()
  r <- c()
  for (l in prec_recall) {
    p <- c(p,l$precision_mean)
    r <- c(r, l$recall_mean)
  }
  data <- data.frame(precision=p, recall=r)
  ggplot(data = data) + geom_point(aes(x=recall, y=precision)) + geom_line(aes(x=recall, y=precision)) + ggtitle(title)
  ggsave(paste(title, ".png", sep=""))  
}

check.normalizado <- function (db) {
  # check mean = 0 , sd = 1 (dados normalizados?)
  apply(db[,2:129], 1, mean)
  apply(db[,2:129], 1, sd)
}
# ----------------------------------------------------------
main.processar <- function(db_original, qy_original, tipo, dist.function, db_rp = NULL, qy_rp = NULL) {
  
 # db_desc <- descriptor.get(db_original, tipo, db_rp)
 # qy_desc <- descriptor.get(qy_original, tipo, qy_rp)
  
  #for tests
  #qy_desc <- qy_desc[1:10,]
  
  distances <- dist.calc(db_desc, qy_desc, dist.function)
  distances <- cbind(qy_original[,1], distances)
  
  prec_recall <- calc.prec_recall(db_desc, qy_desc, distances)
  
  #plot.prec_recall(prec_recall)
  
  return (list("type" = tipo, "function"=dist.function, "distance" = distances, "prec_recall" = prec_recall))
}

main <- function () {
  
  summary(db_original[,-1])
  
  episilon <- sd(apply(db_original, 1, sd))
  
  plot.classes(db_original)
  
  #ggplot(data=db_original,aes(x=V1)) + geom_histogram(bins = 15, fill=I("blue"), col=I("black"),alpha = .4)
  #ggsave("classes_histogram.png")
  
  check.normalizado(db_original)
  check.normalizado(qy_original)

  #normal_l2 <- main.processar(db_original, qy_original, "None", dist.L2)
  #normal_cos <- main.processar(db_original, qy_original, "None", dist.cos)
  
  #dtw_l2 <- main.processar(db_original, qy_original, "None", dist.dtw_L2)
  #dtw_cos <- main.processar(db_original, qy_original, "None", dist.dtw_L2)
  
  lbp_l1 <- main.processar(db_original, qy_original, "LBP", dist.L1, rp, queries_rp)
  #lbp_l2 <- main.processar(db_original, qy_original, "LBP", dist.L2, rp, queries_rp)
  #lbp_cos <- main.processar(db_original, qy_original, "LBP", dist.cos, rp, queries_rp)
  
  #hog_l2 <- main.processar(db_original, qy_original, "HOG", dist.L2, rp, queries_rp)
  #hog_cos <- main.processar(db_original, qy_original, "HOG", dist.cos, rp, queries_rp)
  
  # plotar precision x recall todas as buscas
  # ggplot()
}

# Main
db_original <- read.csv("SwedishLeaf_TRAIN.csv", header = F)
qy_original <- read.csv("SwedishLeaf_TEST.csv", header = F)

# gera imagens do recurrence plot
#rp <- recurrence_plot(db_original)
#queries_rp <- recurrence_plot(qy_original)

main()
