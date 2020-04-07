library(planoCH)

open.files <- function(
  path
)
{
  setwd(path)
  files<-as.vector(list.files())
  data <- lapply(X=files,FUN = function(m) df <- read.csv(m, header = TRUE, sep = ",",quote = "\"",,fill=T))
  results<- lapply(X= data, cols)
  print(results)
}
  
cols<-function(
  file
){
  #alpha = q
  #q = c(0.5,1,1.5,2,2.5)
  q = 1/2
  beta <- 1/2
  eu = NULL 
  e1 = NULL
  edu = NULL
  for(i in 1:ncol(file)){
    result<-file[,i][complete.cases(file[,i])]
    n = length(result)
    Ce <- rep(1, n)
    Cm <- c(1, rep(0, n - 1))
    
    eu[i] <- get.euclidean.distance.to.uniform(result)
    aux <-get.euclidean.disequilibrium.to.uniform(result)
    edu[i] <- as.numeric(aux[3])
    
    hu <- get.hellinger.distance.to.uniform(result)
    hdu <- get.hellinger.disequilibrium.to.uniform(result)

    wu <- get.wootters.distance.to.uniform(result)
    wdu <- get.wootters.disequilibrium.to.uniform(result)

    shu <- get.KLS.distance.to.uniform(result)
    shdu <- get.KLS.disequilibrium.to.uniform(result)


    ru <- get.KLR.distance.to.uniform(result, q = q)
    rdu <- get.KLR.disequilibrium.to.uniform(result, q = q)

    tu <- get.KLT.distance.to.uniform(result, q = q)
    tdu <- get.KLT.disequilibrium.to.uniform(result, q = q)

    jsu <- get.jensen.divergence.to.uniform(result, disorder.fun = get.shannon.disorder)
    jsdu <- get.JS.disequilibrium.to.uniform(result)

    js2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.shannon.disorder)
    js2du <- get.JS.disequilibrium.to.uniform(result, beta = 2/3)

    jru <- get.jensen.divergence.to.uniform(result, disorder.fun = get.renyi.disorder, q = q)
    jrdu <- get.JR.disequilibrium.to.uniform(result, q = q)

    jr2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.renyi.disorder, q = q)
    jr2du <- get.JR.disequilibrium.to.uniform(result, beta = 2/3, q = q)

    jtu <- get.jensen.divergence.to.uniform(result, disorder.fun = get.tsallis.disorder, q = q)
    jtdu <- get.JT.disequilibrium.to.uniform(result, q = q)

    jt2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.tsallis.disorder, q = q)
    jt2du <- get.JT.disequilibrium.to.uniform(result, beta = 2/3, q = q)

    jgu <- get.jensen.divergence.to.uniform(result, disorder.fun = get.escort.tsallis.disorder, q = q)
    jgdu <- get.JG.disequilibrium.to.uniform(result, q = q)

    jg2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.escort.tsallis.disorder, q = q)
    jg2du <- get.JG.disequilibrium.to.uniform(result, beta = 2/3, q = q)

  }
  
  a <- data.frame(
    "eu" = eu,
    "edu-D" = edu)
}
