library(planoCH)
library(stringr)
q = c(0.5,1.5,2,2.5)
j = 1
# Authors : Catalina Andrea Morales Rojas
#           Juan Antonio Fernández Muñoz

# Version : V0.1

# Year    : 2020 

# ============================================================= #
# This script has the goal of apply multiple tools to compute   #
# statistical complexity measures (SCM) in the way of C = QxH   #
# (where C is complexity, Q is Desiquilibrium and H is Entropy  #
# from csv format files containing different types of cerebral  #
# blood pressure measures in different types of patients.       #
# ============================================================= #


# ---------------------- Functions ---------------------- 

# Description: This function open a group of .csv files located 
#              in a certain folder using the folder path 
#              (example: home/user/documents/folder), then, 
#              calls a function named "cols" which apply
#              different tools to compute Desiquilibrium 
#              values. Then, write the results in new csv files.

# Input: Path of the folder with .csv files with data to analyze.

open.files <- function(
  path
)
{
  setwd(path)
  print(q)
  files<-(list.files())
  data <- lapply(X=files,FUN = function(m) df <- read.csv(m, header = TRUE, sep = ",",quote = "\"",,fill=T))
  
  for(i in 1:length(q)){
    results<- lapply(X= data, cols)
    write.files(results,files)
    j<<- j + 1
  }
}
  


# Description: This function calculate the different types of Desiquilibrium
#              from a given .csv file of the folder.

# Input: File to be analyzed.

cols<-function(
  file
){
  #alpha = q
  q = q[j]
  beta <- 1/2
  eu = NULL 
  edu = NULL
  hu = NULL
  hdu = NULL
  wu = NULL
  wdu = NULL
  shu = NULL
  shdu = NULL
  ru = NULL
  rdu = NULL
  tu = NULL
  tdu = NULL
  jsu = NULL
  jsdu = NULL
  jsdu = NULL
  js2u = NULL
  js2du = NULL
  jru = NULL
  jrdu = NULL
  jr2u = NULL
  jr2du = NULL
  jtu = NULL
  jtdu = NULL
  jt2u = NULL 
  jt2du = NULL
  jgu = NULL
  jgdu = NULL
  jgu = NULL
  jgdu = NULL
  jg2u = NULL
  jg2du = NULL

  for(i in 1:ncol(file)){
    result<-file[,i][complete.cases(file[,i])]
    n = length(result)
    Ce <- rep(1, n)
    Cm <- c(1, rep(0, n - 1))
    
    eu[i] <- get.euclidean.distance.to.uniform(result)
    edu[i] <- as.numeric( get.euclidean.disequilibrium.to.uniform(result)[3])
    
    hu[i] <- get.hellinger.distance.to.uniform(result)
    hdu[i] <- as.numeric(get.hellinger.disequilibrium.to.uniform(result)[3])

    wu[i] <- get.wootters.distance.to.uniform(result)
    wdu[i] <- as.numeric(get.wootters.disequilibrium.to.uniform(result)[3])

    shu[i] <- get.KLS.distance.to.uniform(result)
    shdu[i] <- as.numeric(get.KLS.disequilibrium.to.uniform(result)[3])


    ru[i] <- get.KLR.distance.to.uniform(result, q = q)
    rdu[i] <- as.numeric(get.KLR.disequilibrium.to.uniform(result, q = q)[3])

    tu[i] <- get.KLT.distance.to.uniform(result, q = q)
    tdu[i] <- as.numeric(get.KLT.disequilibrium.to.uniform(result, q = q)[3])

    jsu[i] <- get.jensen.divergence.to.uniform(result, disorder.fun = get.shannon.disorder)
    jsdu[i] <- as.numeric(get.JS.disequilibrium.to.uniform(result)[3])

    js2u[i] <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.shannon.disorder)
    js2du[i] <- as.numeric(get.JS.disequilibrium.to.uniform(result, beta = 2/3)[3])

    jru[i] <- get.jensen.divergence.to.uniform(result, disorder.fun = get.renyi.disorder, q = q)
    jrdu[i] <- as.numeric(get.JR.disequilibrium.to.uniform(result, q = q)[3])

    jr2u[i] <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.renyi.disorder, q = q)
    jr2du[i] <- as.numeric(get.JR.disequilibrium.to.uniform(result, beta = 2/3, q = q)[3])

    jtu[i] <- get.jensen.divergence.to.uniform(result, disorder.fun = get.tsallis.disorder, q = q)
    jtdu[i] <- as.numeric(get.JT.disequilibrium.to.uniform(result, q = q)[3])

    jt2u[i] <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.tsallis.disorder, q = q)
    jt2du[i] <-as.numeric(get.JT.disequilibrium.to.uniform(result, beta = 2/3, q = q)[3])

    jgu[i] <- get.jensen.divergence.to.uniform(result, disorder.fun = get.escort.tsallis.disorder, q = q)
    jgdu[i] <- as.numeric(get.JG.disequilibrium.to.uniform(result, q = q)[3])

    jg2u[i] <- get.jensen.divergence.to.uniform(result, beta = 2/3, disorder.fun = get.escort.tsallis.disorder, q = q)
    jg2du[i] <- as.numeric(get.JG.disequilibrium.to.uniform(result, beta = 2/3, q = q)[3])
  }
  
  data <- data.frame(
    "eu" = eu, "edu - Q" = edu, "hu" = hu, "hdu - Q" = hdu, "wu" = wu, "wdu - Q" = wdu, "shu" = shu, "shdu - Q" = shdu, "ru" = ru,
    "rdu - Q" = rdu, "tu" = tu, "tdu - Q" = tdu, "jsu" = jsu, "jsdu - Q" = jsdu, "js2u" = js2u, "js2du - Q" = js2du, "jru" = jru,
    "jrdu - Q" = jrdu, "jr2u" = jr2u, "jr2du - Q" = jr2u, "jtu" = jtu, "jtdu - Q" = jtdu, "jt2u" = jt2u, "jt2du - Q" = jt2du, "jgu" = jgu,
    "jgdu - Q" = jgdu, "jgu" = jgu, "jgdu - Q" = jgdu, "jg2u" = jg2u, "jg2du - Q" = jg2du
     )

  data
}



# Description: This function write the results of the different 
#             Desiquilibriums of an input csv file, in an output
#             csv file.

# Input: Data set with the values of the input csv file and the name
#        the output csv file.
write.files<- function(
  data,
  files
){
  ifelse(!dir.exists("../Resultados"),dir.create("../Resultados"),print("La carpeta ya existe"))
  setwd("../Resultados")
  for(i in 1:length(data)){
    print("Escribiendo archivos...")
    write.csv(data[i], file = str_c(files[i],q[j],".csv"), row.names = FALSE)
  }
  print("Terminado")
}