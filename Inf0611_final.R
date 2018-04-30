#################################################
## INF-0611 - Trabalho Final
##        Rafael Fernando Ribeiro
##        Thiago Gomes Marçal Pereira
#################################################

library(ggplot2)
require(reshape2)
library(wvtool)
library(OpenImageR)

setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-611/Tarefas/INF0611_TrabalhoFinal")

#-----------------------------------------------------------
# Funções
# ----------------------------------------------------------
sax.vocab <- list("3"=c(-0.43,0.43), "4"=c(-0.67,0,0.67), "5"=c(-0.84, -0.25, 0.25, 0.84), "6"=c(-0.97,-0.43,0,0.43,0.97), "7"=c(-1.07,-0.57, -0.18, 0.18, 0.57, 1.07))

sax.alpha <- function(values, vocab_size) {
  res <- c()
  for (v in values) {
    count <- 1
    for (i in sax.vocab[[vocab_size]]) {
      if (v <= i) {
        res <- c(res, count)
        break
      }
      count <- count + 1
    }
    if (count == length(sax.vocab[[vocab_size]])+1) {
      res <- c(res, count)
    }
  }
  res
}

sax.paa <- function (values, size) {
  
  res <- c()
  for (i in c(seq(from =1, to= length(values), by = size))) {
    res <- c(res, mean(values[i:(i+size-1)]))
  }
  res
}

sax.dist.min <- function (v1, v2, vocab_size, n, w) {
  
  res <- c()
  for (i in c(1:length(v1))) {
    if (abs(v1[i]-v2[i]) > 1) {
      res <- c(res, (sax.vocab[[vocab_size]][(max(v1[i],v2[i])-1)] - sax.vocab[[vocab_size]][(min(v1[i],v2[i]))]))
    }
    else {
      res <- c(res,0)
    }
  }
  sqrt(n/w) * sqrt(sum(res^2))
}

# Recurrence Plot
recurrence_plot <- function(vector) {
  rp <- matrix(nrow = length(vector), ncol=length(vector))
  sdvec <- sd(vector)
  for (i in 1:length(vector)) {
    for (j in 1:length(vector)) {
      if (abs(vector[i] - vector[j]) <= sdvec) {
        rp[i,j] <- 1
      } else {
        rp[i,j] <- 0
      }
    }
  }
  rp
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
  
  qrow <- nrow(query)
  qcol <- ncol(query)
  drow <- nrow(db)
  dcol <- ncol(db)
  
  l <- list()
  for (q in 1:qrow) {
    dists <- c()
    for (d in 1:drow) {
      dists <- c(dists, dist.func(query[q,2:qcol], db[d,2:dcol]))
    }
    data <- data.frame(db[1:drow,1], dists)
    colnames(data) <- c("target", "dist")
    l[[q]] <- data[order(data[,2]),][1:(min(100,drow)),]
  }
  return(l)
}

calc.precision <- function(q, db, distances, k) {
  tp <- sum(distances$target[1:k] == q)
  fp <- k
  return (tp/fp)
}

calc.recall <- function(q, db, distances, k) {
  tp <- sum(distances$target[1:k] == q)
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
       precision <- c(precision, calc.precision(qy[q,1], db, distances[[q]], ks[k]))
       recall <- c(recall, calc.recall(qy[q,1], db, distances[[q]], ks[k]))
    }
    l[[k]] <- list(k=ks[k],  
                     precision = precision, precision_mean=mean(precision), 
                     recall = recall,  recall_mean=mean(recall))
  }
  return (l)
}

descriptor.get <- function(db, descriptor) {
  
  if (descriptor == "SAX"){
    
  }
  else if (descriptor == "RP") {
    
  }
  else{
    db <- db
  }
  return(db)
}

plot.classes <- function (db) {
  # plotar as classes
  for (i in unique(db[,1])){
    
    data <- data.frame(cbind(t(db[db[,1]==i, 2:129][1:3,]), vars=c(1:128)))
    colnames(data) <- c("a","b","c", "time")
    df <- melt(data ,  id.vars = 'time', variable.name = 'series')
    #ggplot(df, aes(time,value)) + geom_line(aes(colour = series))
    ggplot(df, aes(time,value)) + geom_line() + facet_grid(series ~ .) + ggtitle(paste("Classe", i))
  }
}

plot.prec_recall <- function(prec_recall) {
  p <- c()
  r <- c()
  for (l in prec_recall) {
    p <- c(p,l$precision_mean)
    r <- c(r, l$recall_mean)
  }
  data <- data.frame(precision=p, recall=r)
  ggplot(data = data) + geom_point(aes(x=recall, y=precision)) + geom_line(aes(x=recall, y=precision))
}
# ----------------------------------------------------------

# Main
db_original <- read.csv("SwedishLeaf_TRAIN.csv", header = F)
qy_original <- read.csv("SwedishLeaf_TEST.csv", header = F)

summary(db_original[,2:129])

#plot.classes(db_original)

# check mean = 0 , sd = 1 (dados normalizados?)
apply(db_original[,2:129], 1, mean)
apply(db_original[,2:129], 1, sd)

apply(qy_original[,2:129], 1, mean)
apply(qy_original[,2:129], 1, sd)


db_desc <- descriptor.get(db_original, "None")
qy_desc <- descriptor.get(qy_original, "None")

qy_desc <- qy_desc[1:20,]

start_time <- Sys.time()
distances <- dist.calc(db_desc, qy_desc, dist.L2)
end_time <- Sys.time()
print(end_time - start_time)

prec_recall <- calc.prec_recall(db_desc, qy_desc, distances)

plot.prec_recall(prec_recall)

# Calcular recurrence Plot para cada item 
rp <- list()
for (i in c(1:length(db_original[,1]))) {
  rp[[i]] <- recurrence_plot(db_original[i, -1])
}





