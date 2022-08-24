setwd("C:/Users/capel/Dropbox/TCC/Genetic Algorithms")
library("readxl")
library("dplyr")
library("GA")


mt <- read_excel("Matriz teste M2.xlsx")
mt <- mt %>% select(-1)
mt <- as.matrix(mt)
dimnames(mt) <- (list(c(1:ncol(mt)),c(1:ncol(mt))))
mt

#mm <- as.matrix((read_excel("Matriz teste M2.xlsx")) %>% select(-1) )


#Definir cromossomo

    #Y é um cromossomo apenas para apresentar os pontos de "corte" entre os módulos 
    #comprimento de y deve ser igual ao número de elmentos - 1 
ncol(mt)
#x <- c(0,0,2)
x <- c(0,0,0,0,0,0,6,3)

#x <- c(0,0,0,0,0,0,0,0,0,0,16,15,14,9,5,3)
length(x)
#soma dos elementos dentro da matriz
func <- function (x){
md <- sum(mt[1:x[length(x)],0:x[length(x)]])

for (i in 0:length(x)){

  if (x[length(x)-(i+1)] == 0){
    md <- md + sum(mt[(x[length(x)-i]+1):ncol(mt),(x[length(x)-i]+1):nrow(mt)])
    break
  } else {
    md <- md + sum(mt[(x[length(x)-i]+1):(x[length(x)-(1+i)]),(x[length(x)-i]+1):(x[length(x)-(1+i)])])
  }
  
  
}
MI <- md - (sum(mt) - md)

#função penalização
MI <- ifelse(x[i] < 0, MI * 0.001, MI)

print(MI)


}
func(x)

#matrix de limites                                                          
mat <- matrix(rep(c(0,ncol(mt)),length(x)),length(x),2,byrow = TRUE)
mat




library("rgenoud")
GA <- genoud(func, nvars = length(x), max=TRUE, Domains = mat, boundary.enforcement=0, data.type.int = TRUE)
GA$value
GA$par


mt[(x[length(x)-i]+1):(x[length(x)-(1+i)]),(x[length(x)-i]+1):(x[length(x)-(1+i)])]


??"Rgenoud"


