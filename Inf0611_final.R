#################################################
## INF-0611 - Trabalho Final
##        Rafael Fernando Ribeiro
##        Thiago Gomes Marçal Pereira
#################################################

library(ggplot2)
require(reshape2)
library(wvtool)
library(OpenImageR)
library(dtw)

setwd("C:\\Users\\rafaelr\\Documents\\INF611F\\trabalho_final")

#-----------------------------------------------------------
# Funções
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

count <- 0
#extrair o vetor de carecteristicas (255 dimensoes) usando LBP
extrairLBP <- function(img){
  count <- count + 1
  #print(paste("Extraindo LBP:", count))
  desc <- lbp(img, 1)
  h <- hist(desc$lbp.ori, plot=FALSE, breaks = 0:255)
  #h <- hist(desc$lbp.u2, plot=FALSE, breaks = 0:58)
  return(h$counts)
}

# Buscar K imagens mais próximas
#parametros:
#  M: matriz que comtem os vetore de carateristicas
#  query: o nome do arquivo de consulta
#  K: o numero de imagens mais proximas a serem devolvidas
#retorno:
#  a lista dos nomes das K-imagens mais proximas
buscarMaisProximos <- function(M, query, dist.func, K){
  distancias <- apply(M, 1, dist.func, query)
  
  distancias <- order(distancias, decreasing = FALSE)
  
  return(distancias[1:K])
}

# Retorna a lista de imagens, baseado nos índices
lista_imagens <- function (vector) {
  return (db_original[vector, 1])
}

# normalizar dados
normalize <- function (values) {
  print(dim(values))
  (values - mean(values))/sd(values)
}

#L1 ou Manhattan
dist.L1 <- function(x, y){
  d = 0
  for (i in c(1:length(x))) {
    d = d+abs(x[i]-y[i])
  }
  return(as.numeric(d))
}

# Calculo de Distancia L2
dist.L2 <- function(x, y){
  d = 0
  for (i in c(1:length(x))) {
    d = d+(x[i]-y[i])^2
  }
  return(as.numeric(sqrt(d)))
}

# Calculo de Distancia Cossenos
dist.cos <- function(x, y){
  return (as.numeric(sum(x*y)/sqrt(sum(x^2)*sum(y^2))))
}

dist.dtw_L1 <- function(x, y) {
  return (dtw(x,y, distance.only = T, dist.method = "manhattan")$distance)     
}

dist.dtw_L2 <- function(x, y) {
  return (dtw(x,y, distance.only = T, dist.method = "L2")$distance)     
}

dist.dtw_cos <- function(x, y) {
  return (dtw(x,y, distance.only = T, dist.method = "cosine")$distance)     
}

# Calculo das distancias
dist.calc <- function(db, query, dist.func) {
  
  MAIS_PROXIMOS <- 100

  proximos <- matrix(nrow = nrow(query), ncol = MAIS_PROXIMOS)
  for (i in c(1:nrow(query))) {
    proximos[i,] <- buscarMaisProximos(db, query[i,], dist.func, MAIS_PROXIMOS)
    proximos[i,] <- lista_imagens(proximos[i,])
  }
  
  return(proximos)
}

calc.precision <- function(q, db, distances, k) {
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
  
  l <- list()
  qrow <- nrow(qy)
  ks <- c(seq(5, 100, by = 5))
  
  for (k in c(1:(100/5))) {
    precision <- c()
    recall <- c()
    for (q in c(1:qrow)) {
      precision <- c(precision, calc.precision(distances[q,1], db, distances[q,-1], ks[k]))
      recall <- c(recall, calc.recall(distances[q,1], db, distances[q,-1], ks[k]))
    }
    l[[k]] <- list(k=ks[k],  
                   precision = precision, precision_mean=mean(precision), 
                   recall = recall,  recall_mean=mean(recall))
  }
  return (l)
}

descriptor.get <- function(db, descriptor = "None", rp_images = NULL) {
  
  if (descriptor == "HOG") {
    hog_db <- sapply(rp_images, HOG,cells = 8, orientations = 6)
    hog_db <- t(hog_db)
    #db_new <- cbind(as.data.frame(db[,1]),as.data.frame(hog_db))
    db_new <- hog_db
  }
  else if (descriptor == "LBP") {
    lbp_db <- sapply(rp_images, extrairLBP)
    lbp_db <- t(lbp_db)
    #db_new <- cbind(as.data.frame(db[,1]),as.data.frame(lbp_db))
    db_new <- lbp_db
  }
  else{
    db_new <- as.matrix(db[,-1])
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
    
    ggplot(data=db_original,aes(x=as.factor(V1))) + geom_bar() + xlab("Classes")
    ggsave("classes_histogram.png")
  }
}

plot.prec_recall <- function(prec_recall, title = "") {
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

plot.prec_recall_all <- function(title = "All") {
  
  count <- 1
  colors <- c("Normal-L1", "Normal-L2", "DTW-L1", "DTW-L2", "LBP-L1", "LBP-L2", "HOG-L1", "HOG-L2")
  
  lall <- list()
  for (prec_recall in list(normal_l1$prec_recall, normal_l2$prec_recall, dtw_l1$prec_recall, dtw_l2$prec_recall, lbp_l1$prec_recall, lbp_l2$prec_recall, hog_l1$prec_recall, hog_l2$prec_recall)) {  
    p <- c()
    r <- c()
    
    for (l in prec_recall) {
      p <- c(p,l$precision_mean)
      r <- c(r, l$recall_mean)
    }

    data <- data.frame(precision=p, recall=r)
    lall[[count]] <- data
    count <- count + 1
  }

  g <- ggplot() + 
    geom_point(data = lall[[1]], aes(x=recall, y=precision, color = colors[1])) + 
    geom_line(data =lall[[1]], aes(x=recall, y=precision, color = colors[1])) +
    
    geom_point(data = lall[[2]], aes(x=recall, y=precision, color = colors[2])) + 
    geom_line(data =lall[[2]], aes(x=recall, y=precision, color = colors[2])) +
    
    geom_point(data = lall[[3]], aes(x=recall, y=precision, color = colors[3])) + 
    geom_line(data =lall[[3]], aes(x=recall, y=precision, color = colors[3])) +
    
    geom_point(data = lall[[4]], aes(x=recall, y=precision, color = colors[4])) + 
    geom_line(data =lall[[4]], aes(x=recall, y=precision, color = colors[4])) +
    
    geom_point(data = lall[[5]], aes(x=recall, y=precision, color = colors[5])) + 
    geom_line(data =lall[[5]], aes(x=recall, y=precision, color = colors[5])) +
    
    geom_point(data = lall[[6]], aes(x=recall, y=precision, color = colors[6])) + 
    geom_line(data =lall[[6]], aes(x=recall, y=precision, color = colors[6])) +
    
    geom_point(data = lall[[7]], aes(x=recall, y=precision, color = colors[7])) + 
    geom_line(data =lall[[7]], aes(x=recall, y=precision, color = colors[7])) +
    
    geom_point(data = lall[[8]], aes(x=recall, y=precision, color = colors[8])) + 
    geom_line(data =lall[[8]], aes(x=recall, y=precision, color = colors[8])) +
    
    labs(colour="Métodos")

  
  print(g)
  ggsave(paste(title, ".png", sep=""))  
}

check.normalizado <- function (db) {
  # check mean = 0 , sd = 1 (dados normalizados?)
  apply(db[,2:129], 1, mean)
  apply(db[,2:129], 1, sd)
}
# ----------------------------------------------------------
main.processar <- function(db_desc, qy_desc, tipo, dist.function, title="") {
  
  distances <- dist.calc(db_desc, qy_desc, dist.function)
  distances <- cbind(qy_original[,1], distances)
  
  prec_recall <- calc.prec_recall(db_original, qy_original, distances)
  
  plot.prec_recall(prec_recall, title)
  
  return (list("type" = tipo, "function"=dist.function, "distance" = distances, "prec_recall" = prec_recall))
}

main <- function () {
  
  summary(db_original[,-1])
  summary(as.factor(db_original[,1]))
  
  episilon <- sd(apply(db_original, 1, sd))
  
  plot.classes(db_original)
  
  check.normalizado(db_original)
  check.normalizado(qy_original)
  
  db_desc <- descriptor.get(db_original)
  qy_desc <- descriptor.get(qy_original)
  
  normal_l1 <- main.processar(db_desc, qy_desc, "None", dist.L1, "None - L1")
  normal_l2 <- main.processar(db_desc, qy_desc, "None", dist.L2, "None - L2")
  #normal_cos <- main.processar(db_desc, qy_desc, "None", dist.cos, "None - Cosseno")
  
  dtw_l1 <- main.processar(db_desc, qy_desc, "None", dist.dtw_L1, "DTW - L1")
  dtw_l2 <- main.processar(db_desc, qy_desc, "None", dist.dtw_L2, "DTW - L2")
  #dtw_cos <- main.processar(db_desc, qy_desc, "None", dist.dtw_L2, "DTW - Cosseno")
  
  db_desc_lbp <- descriptor.get(db_original, "LBP", rp)
  qy_desc_lbp <- descriptor.get(qy_original, "LBP", queries_rp)
  
  lbp_l1 <- main.processar(db_desc_lbp, qy_desc_lbp, "LBP", dist.L1, "LBP - L1")
  lbp_l2 <- main.processar(db_desc_lbp, qy_desc_lbp, "LBP", dist.L2, "LBP - L2")
  #lbp_cos <- main.processar(db_desc_lbp, qy_desc_lbp, "LBP", dist.cos, "LBP - Cosseno")
  
  db_desc_hog <- descriptor.get(db_original, "HOG", rp)
  qy_desc_hog <- descriptor.get(qy_original, "HOG", queries_rp)
  
  hog_l1 <- main.processar(db_desc_hog, qy_desc_hog, "HOG", dist.L1, "HOG - L1")
  hog_l2 <- main.processar(db_desc_hog, qy_desc_hog, "HOG", dist.L2, "HOG - L2")
  #hog_cos <- main.processar(db_desc_hog, qy_desc_hog, "HOG", dist.cos, "HOG - Cosseno")
  
  # check classes com melhor e pior classificação
  
  
  # plotar precision x recall todas as buscas
  plot.prec_recall_all()
}

# Main
db_original <- read.csv("SwedishLeaf_TRAIN.csv", header = F)
qy_original <- read.csv("SwedishLeaf_TEST.csv", header = F)

# gera imagens do recurrence plot
#rp <- recurrence_plot(db_original)
#queries_rp <- recurrence_plot(qy_original)

#main()
