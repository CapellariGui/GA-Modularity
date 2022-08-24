##### setup  ######
{
#setwd("C:/Users/capel/Dropbox/TCC/Genetic Algorithms")
library("readxl")
library("dplyr")
library("GA")
library("rgenoud")
  }

{
mt <- read_excel("Matriz teste M3.xlsx")
mt <- mt %>% select(-1)
mt <- as.matrix(mt)
dimnames(mt) <- (list(c(1:ncol(mt)),c(1:ncol(mt))))
mt
ncol(mt)


#Definir cromossomo

x <- c(0,0,0,0,0,0,0,0,0,0,16,15,14,9,5,3,0) 
 
#length(x)
#rm(x)
##### x teste com resultado do Genoud e outras possibilidades #####


#x <- c(0,0,0,0,16,14,12,10,9,8,7,6,4,3,2,1,0)
#x <- c(16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0)
#x <- c(0,0,0,0,0,0,0,0,0,0,0,0,14,9,6,3,0)
#x <- c(0,0,0,0,0,0,0,0,0,0,0,0,13,9,5,3,0)




############################### MI JUNG ###############################


# MI=(MI1*W1)+(MI2*W2)+(MI3*W3)


#número de módulos é a quantidade de valor diferentes de zero em X + 1

#nm <- sum(x!=0)+1
#nm

#número de elementos/componentes é igual a quantidade de colunas na matriz 

#nc <- ncol(mt)
#nc
}


############### Cálculo de MI ###############
MI <- function(x){
#nm <- sum(x!=0)+2
nm <- 7
nc <- ncol(mt)
BC <- 0
WC <- 0
SWC <- WC #soma do WC
WCj <- 0
BS <- 0
WS <- 0 
WSj <- 0 
ST <- 0
q <- 0 
p <- 0 
qp <- 0 
  
for (i in 1:(nm)){
  j <- i+1
  
  if (i == (nm-1)){
    WCj <- sum((mt[(x[ncol(mt)-(i)]+1):nc , (x[ncol(mt)-(i)]+1):nc])!=0)
    WSj <- sum(mt[(x[ncol(mt)-(i)]+1):nc , (x[ncol(mt)-(i)]+1):nc])
   
    ST <- ST + ((BC/WC)+(BC/WCj)+(BS/WS)+(BS/WSj))
    SWC <- SWC + WC
    q <- x[ncol(mt)-(j-1)]
    p <- (x[ncol(mt)-(i-1)]+1)
    qp <- qp + (q-p+1)^2
    
    #print(WCj)
    #print(ST)
  } else {
  
  if (i == nm){
    WC <- sum((mt[(x[ncol(mt)-(i-1)]+1):nc , (x[ncol(mt)-(i-1)]+1):nc])!=0)
    WS <- sum(mt[(x[ncol(mt)-(i-1)]+1):nc , (x[ncol(mt)-(i-1)]+1):nc])
   
    ST <- ST + ((BC/WC)+(BC/WCj)+(BS/WS)+(BS/WSj))
    SWC <- SWC + WC
    q <- nc
    p <- (x[ncol(mt)-(i-1)]+1)
    qp <- qp + (q-p+1)^2
    
    
    #print(ST)
    break
  } else {
    WC <- {
      sum((mt[(x[ncol(mt)-(i-1)]+1):(x[ncol(mt)-(j-1)]) , (x[ncol(mt)-(i-1)]+1): (x[ncol(mt)-(j-1)])])!=0)
    }
    BC <- {
      sum((mt[(x[ncol(mt)-(j-1)]+1):ncol(mt),(x[ncol(mt)-(i-1)]+1):x[ncol(mt)-(j-1)]])!=0) +  
        sum((mt[(x[ncol(mt)-(i-1)]+1):x[ncol(mt)-(j-1)],(x[ncol(mt)-(j-1)]+1):ncol(mt)])!=0)  
    }
    WCj <- {
      sum((mt[(x[ncol(mt)-(i)]+1):(x[ncol(mt)-(j)]) , (x[ncol(mt)-(i)]+1):(x[ncol(mt)-(j)])])!=0)
    }
    WS <- {
      sum(mt[(x[ncol(mt)-(i-1)]+1):(x[ncol(mt)-(j-1)]) , (x[ncol(mt)-(i-1)]+1): (x[ncol(mt)-(j-1)])])
    }
    BS <- {
      sum(mt[(x[ncol(mt)-(j-1)]+1):ncol(mt),(x[ncol(mt)-(i-1)]+1):x[ncol(mt)-(j-1)]]) +  
        sum(mt[(x[ncol(mt)-(i-1)]+1):x[ncol(mt)-(j-1)],(x[ncol(mt)-(j-1)]+1):ncol(mt)])  
    }
    WSj <- {
      sum(mt[(x[ncol(mt)-(i)]+1):(x[ncol(mt)-(j)]) , (x[ncol(mt)-(i)]+1):(x[ncol(mt)-(j)])])
    }
    q <- x[ncol(mt)-(j-1)]
    p <- (x[ncol(mt)-(i-1)]+1)
    qp <- qp + (q-p+1)^2
    
  }

ST <- ST + ((BC/WC)+(BC/WCj)+(BS/WS)+(BS/WSj))
SWC <- SWC + WC


}
}

#Cálculo individual dos índices MI1, MI2 e MI3

M1 <- 1-(ST/(2*nm*(nm-1)))
M2 <- (SWC + nc)/qp
M3 <- 0.9375

#Índice de modularidade MI
#MI <- ((abs(M1)) * (1/3)) + ((abs(M2)) * (1/3)) + ((abs(M3)) * (1/3))
MI <- (M1 * (1/3)) + (M2 * (1/3)) + (M3 * (1/3))


#### PENALIZAÇÃO (sum(x!=0) != (nm-1) || ####
offset <- 1.5

MI <- ifelse(x[ncol(mt)] != 0 || sum(x!=0)+1 != nm || M1 <= 0 || M2 <= 0 || M3 <= 0 || M1 > 1 ||  M2 > 1 || M3 > 1 , MI <- 0, MI)
#MI <- ifelse(M1 <= 0 || M2 <= 0 || M3 <= 0 || M1 > 1 ||  M2 > 1 || M3 > 1 , MI <- 0, MI)
#

#Valor de retorno
print(M1)
print(M2)
print(M3)

return(MI)
}



MI(x)

mat <- matrix(rep(c(0,ncol(mt)-1),ncol(mt)),ncol(mt),2,byrow = TRUE)
mat
(x[ncol(mt)-(i)]+1):(x[ncol(mt)-(j)]) 

#### Genoud ####
GA <- genoud(MI, nvars = ncol(mt), max=TRUE, Domains = mat, data.type.int = TRUE, pop.size = 100, wait.generations = 100 )

GA
GA$value
GA$par
x <- GA$par

#### GA ####


#GA <- ga(type = "binary", fitness = MI, nBits = ncol(mt), upper = max, lower = 0)

#plot(GA)
#summary(GA)
#x <- GA@solution


