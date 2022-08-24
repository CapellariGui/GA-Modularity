library("readxl")
library("dplyr")
library("GA")
m1 <- read_excel("Matriz teste M1.xlsx")
m1
m1 <- m1 %>% select(-1)
m1
m1 <- as.matrix(m1)


dimnames(m1) <- (list(c("1", "2" , "3", "4"),c("1", "2" , "3", "4")))
m1
is.matrix(m1)

x <- c(0,0,2)
length(x)

#sum(M1[0:x[7];0:x[7]])+sum(M1[x[7]:length(m1);x[7]:nrow(m1)])

#MI = (soma de aij que pertence a Mn) - (soma de aij que não pertence a Mn)
m1
sum(m1[0:2,0:2])
sum(m1[0:x[length(x)],0:x[length(x)]])

m1[0:x[length(x)],0:x[length(x)]]

m1[(x[length(x)]+1):ncol(m1),(x[length(x)]+1):nrow(m1)]

m1[(x[length(x)]+1):ncol(m1),0:x[length(x)]]

m1[0:x[length(x)],(x[length(x)]+1):ncol(m1)]

MI <- function(x) {(sum(m1[0:x[length(x)],0:x[length(x)]]) +
  sum(m1[(x[length(x)]+1):ncol(m1),(x[length(x)]+1):nrow(m1)]) - 
  (sum(m1[(x[length(x)]+1):ncol(m1),0:x[length(x)]]) + sum(m1[0:x[length(x)],(x[length(x)]+1):ncol(m1)])))}



#MI <- function(x) {(sum(m1[0:x,0:x]) +
 #                     sum(m1[(x+1):ncol(m1),(x+1):nrow(m1)]) - 
  #                    (sum(m1[(x+1):ncol(m1),0:x]) + sum(m1[0:x,(x+1):ncol(m1)])))}

MI(x)
x

#GA <- ga(type = "real-valued", fitness = MI, nBits = (length(x)), popSize = 100, maxiter = 1000, run = 100,
#         lower = 0, upper = 3)
#summary(GA)
#warnings()
#?ga

#curve(MI, from = 1, to = (nrow(m1)-1))
#?curve

########### ------------------- Teste com matriz maior ----------------- ####################

m2 <- read_excel("Matriz teste M2.xlsx")
m2 <- m2 %>% select(-1)
m2 <- as.matrix(m2)
m2

dimnames(m2) <- (list(c(1:ncol(m2)),c(1:ncol(m2))))
m2


y <- c(0,0,0,0,0,7,5,2)
length(y)

# Y é um cromossomo apenas para apresentar os pontos de "corte" entre os módulos 
# comprimento de y deve ser igual ao número de elmentos - 1 


MI2 <- function(x) {(sum(m2[0:y[length(y)],0:y[length(y)]]) +
                      sum(m2[(y[length(y)]+1):ncol(m2),(y[length(y)]+1):nrow(m2)]) - 
                      (sum(m2[(y[length(y)]+1):ncol(m2),0:y[length(y)]]) + 
                         sum(m2[0:y[length(y)],(y[length(y)]+1):ncol(m2)])))}

                    



#soma dos elementos dentro da matriz

dm <- sum(m2[0:y[length(y)],0:y[length(y)]]) +
  sum(m2[(y[length(y)-0]+1):(y[length(y)-(1+0)]),(y[length(y)-0]+1):(y[length(y)-(1+0)])]) +
  sum(m2[(y[length(y)-1]+1):ncol(m2),(y[length(y)-1]+1):nrow(m2)])

fm <- dm - (sum(m2) - dm)


md <- sum(m2[0:y[length(y)],0:y[length(y)]])

for (i in 0:length(y)){
  #print(i)
  #print(y[length(y)-(i)])
  
  if (y[length(y)-(i+1)] == 0){
    md <- md + sum(m2[(y[length(y)-i]+1):ncol(m2),(y[length(y)-i]+1):nrow(m2)])
    print(md)
    break
    } else {
      md <- md + sum(m2[(y[length(y)-i]+1):(y[length(y)-(1+i)]),(y[length(y)-i]+1):(y[length(y)-(1+i)])])
    }
  print(md)  
  
}













                                                       
