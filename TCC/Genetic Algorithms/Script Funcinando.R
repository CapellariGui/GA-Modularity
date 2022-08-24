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

md <- sum(mt[0:x[length(x)],0:x[length(x)]])

for (i in 0:length(x)){

  if (x[length(x)-(i+1)] == 0){
    md <- md + sum(mt[(x[length(x)-i]+1):ncol(mt),(x[length(x)-i]+1):nrow(mt)])
    print(md)
    break
  } else {
    md <- md + sum(mt[(x[length(x)-i]+1):(x[length(x)-(1+i)]),(x[length(x)-i]+1):(x[length(x)-(1+i)])])
  }
  print(md)  
  
}

#Conexões fora dos módulos (fm) = total de conexões (sum(mt)) - conexões dentro dos módulos (md)

# MI = conexões dentro dos módulos - conexões fora dos módulos

MI <- md - (sum(mt) - md)
MI
############################### GA ###################################

### Vetor padrão, varia de acordo com o tamanho da matriz ###

j <- c(1:ncol(mt))
a <- c(rep.int(j,ncol(mt)))
a

length(a)

### Esse será o vetor binário ###
# Como gerar esse vetor automaticamente? # 
k <- c(0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,1,0,0,0,
       0,0,1,0,0,0,0,0,0)

b = a*k
b






############################### MI JUNG ###############################


# MI=(MI1*W1)+(MI2*W2)+(MI3*W3)

#número de módulos é a quantidade de valor diferentes de zero em X + 1

nm <- sum(x!=0)+1
nm

#número de elementos/componentes é igual a quantidade de colunas na matriz 

nc <- ncol(mt)
nc


sum(mt[(x[length(x)-1]):ncol(mt),0:x[length(x)]])

sum(mt[(x[length(x)]+1):ncol(mt),0:x[length(x)]])
sum(mt[0:x[length(x)],(x[length(x)]+1):ncol(mt)])

BC <- function(i,j) {
  sum(mt)
  
}

WC

BS

WS

p

q

R






k <- 1
for (f in 1:nm-1){
  for (g in f+1:nm){
    k <- k+f+g
    print (k)
    
  }
} 
  
  
  
  
  
  
  
  
  
  
  

