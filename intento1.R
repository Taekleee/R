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
    e1[i] <- get.euclidean.distance(result, Ce)
    aux <-get.euclidean.disequilibrium.to.uniform(result)
    edu[i] <- as.numeric(aux[3])
    ed1 <- get.euclidean.disequilibrium(result, Ce, Cm)
    edf <- get.euclidean.disequilibrium.by.fmla(result, Ce)
    
    hu <- get.hellinger.distance.to.uniform(result)
    h1 <- get.hellinger.distance(result, Ce)
    hdu <- get.hellinger.disequilibrium.to.uniform(result)
    hd1 <- get.hellinger.disequilibrium(result, Ce, Cm)

    wu <- get.wootters.distance.to.uniform(result)
    w1 <- get.wootters.distance(result, Ce)
    wdu <- get.wootters.disequilibrium.to.uniform(result)
    wd1 <- get.wootters.disequilibrium(result, Ce, Cm)
    wdf <- get.wootters.disequilibrium.by.fmla(result, Ce)
    
    shu <- get.KLS.distance.to.uniform(result)
    sh1 <- get.KLS.distance(result, Ce)
    shdu <- get.KLS.disequilibrium.to.uniform(result)
    shd1 <- get.KLS.disequilibrium(result, Ce, Cm)
    shd2 <- get.KLS.disequilibrium.by.fmla(result, Ce)
    

    ru <- get.KLR.distance.to.uniform(result, q = q)
    r1 <- get.KLR.distance(result, Ce, q = q)
    rdu <- get.KLR.disequilibrium.to.uniform(result, q = q)
    rd1 <- get.KLR.disequilibrium(result, Ce, Cm, q = q)
    rd2 <- get.KLR.disequilibrium.by.fmla(result, Ce, q = q)
    
    tu <- get.KLT.distance.to.uniform(result, q = q)
    t1 <- get.KLT.distance(result, Ce, q = q)
    tdu <- get.KLT.disequilibrium.to.uniform(result, q = q)
    td1 <- get.KLT.disequilibrium(result, Ce, Cm, q = q)
    td2 <- get.KLT.disequilibrium.by.fmla(result, Ce, q = q)

    jsu <- get.jensen.divergence.to.uniform(result, disorder.fun = get.shannon.disorder)
    js1 <- get.jensen.divergence(result, Ce, disorder.fun = get.shannon.disorder)
    jsdu <- get.JS.disequilibrium.to.uniform(result)
    jsd1 <- get.JS.disequilibrium(result, Ce, Cm)

    js2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.shannon.disorder)
    js2 <- get.jensen.divergence(result, Ce, beta = 2/3, disorder.fun = get.shannon.disorder)
    js2du <- get.JS.disequilibrium.to.uniform(result, beta = 2/3)
    js2d1 <- get.JS.disequilibrium(result, Ce, Cm, beta = 2/3)

    jru <- get.jensen.divergence.to.uniform(result, disorder.fun = get.renyi.disorder, q = q)
    jr1 <- get.jensen.divergence(result, Ce, disorder.fun = get.renyi.disorder, q = q)
    jrdu <- get.JR.disequilibrium.to.uniform(result, q = q)
    jrd1 <- get.JR.disequilibrium(result, Ce, Cm, q = q)

    jr2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.renyi.disorder, q = q)
    jr2 <- get.jensen.divergence(result, Ce, beta = 2/3, disorder.fun = get.renyi.disorder, q = q)
    jr2du <- get.JR.disequilibrium.to.uniform(result, beta = 2/3, q = q)
    jr2d1 <- get.JR.disequilibrium(result, Ce, Cm, beta = 2/3, q = q)

    jtu <- get.jensen.divergence.to.uniform(result, disorder.fun = get.tsallis.disorder, q = q)
    jt1 <- get.jensen.divergence(result, Ce, disorder.fun = get.tsallis.disorder, q = q)
    jtdu <- get.JT.disequilibrium.to.uniform(result, q = q)
    jtd1 <- get.JT.disequilibrium(result, Ce, Cm, q = q)

    jt2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.tsallis.disorder, q = q)
    jt2 <- get.jensen.divergence(result, Ce, beta = 2/3, disorder.fun = get.tsallis.disorder, q = q)
    jt2du <- get.JT.disequilibrium.to.uniform(result, beta = 2/3, q = q)
    jt2d1 <- get.JT.disequilibrium(result, Ce, Cm, beta = 2/3, q = q)

    jgu <- get.jensen.divergence.to.uniform(result, disorder.fun = get.escort.tsallis.disorder, q = q)
    jg1 <- get.jensen.divergence(result, Ce, disorder.fun = get.escort.tsallis.disorder, q = q)
    jgdu <- get.JG.disequilibrium.to.uniform(result, q = q)
    jgd1 <- get.JG.disequilibrium(result, Ce, Cm, q = q)

    jg2u <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.escort.tsallis.disorder, q = q)
    jg2 <- get.jensen.divergence(result, Ce, beta = 2/3, disorder.fun = get.escort.tsallis.disorder, q = q)
    jg2du <- get.JG.disequilibrium.to.uniform(result, beta = 2/3, q = q)
    jg2d1 <- get.JG.disequilibrium(result, Ce, Cm, beta = 2/3, q = q)

  }
  
  a <- data.frame(
    "eu" = eu,
    "e1" = e1,
    "edu-D" = edu)
}
