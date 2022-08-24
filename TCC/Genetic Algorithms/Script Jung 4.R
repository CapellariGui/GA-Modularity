##### setup  ######
{
setwd("C:/Users/capel/Dropbox/TCC/Genetic Algorithms")
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
#mt
#ncol(mt)
}
##### Cromossomos binário para decimal #####

#Vetor a ser gerado no GA

vga <- as.integer(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
         0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
         0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))


vga

#Vetor padrão 

vp <- as.integer(rep(c(0:(ncol(mt)-1)),ncol(mt)))
vp          

v <- as.integer(vga*vp)
v

#Cromossomo x final 
x <- as.integer(c(sum(v[1:17]), sum(v[18:34]), sum(v[35:51]), sum(v[52:68]), sum(v[69:85]), 
       sum(v[86:102]), sum(v[103:119]), sum(v[120:136]), sum(v[137:153]), sum(v[154:170]),
       sum(v[171:187]), sum(v[188:204]), sum(v[205:221]), sum(v[222:238]), sum(v[239:255]), 
       sum(v[256:272]), sum(v[273:289]) ))
x

############### Cálculo de MI ###############
MI <- function(vga){
  
  
vp <- as.integer(rep(c(0:(ncol(mt)-1)),ncol(mt))  )
v <- as.integer(vga*vp)
x <- as.integer(c(sum(v[1:17]), sum(v[18:34]), sum(v[35:51]), sum(v[52:68]), sum(v[69:85]), 
       sum(v[86:102]), sum(v[103:119]), sum(v[120:136]), sum(v[137:153]), sum(v[154:170]),
       sum(v[171:187]), sum(v[188:204]), sum(v[205:221]), sum(v[222:238]), sum(v[239:255]), 
       sum(v[256:272]), sum(v[273:289]) )  )
    
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
SR1 <- 0
SR2 <- 0
  
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
#M3 
for (i in 1:(ncol(mt)-1)){
  j <- i+1
  
  SR1 <- SR1 + ((1-(j-i)/(ncol(mt)-1))*(mt[i,j] +mt[j,i])/max(mt))
  SR2 <- SR2 + ((mt[i,j] +mt[j,i])/max(mt))
  
  M3 <- SR1/SR2
  #return(M3)
}



#Índice de modularidade MI
#MI <- ((abs(M1)) * (1/3)) + ((abs(M2)) * (1/3)) + ((abs(M3)) * (1/3))
MI <- (M1 * (1/3)) + (M2 * (1/3)) + (M3 * (1/3))


#Penalidades (sum(x!=0) != (nm-1) || 
#offset <- 1.5

#MI <- ifelse(x[ncol(mt)] != 0 || sum(x!=0)+1 != nm || M1 <= 0 || M2 <= 0 || M3 <= 0 || M1 > 1 ||  M2 > 1 || M3 > 1 , MI <- 0, MI)
#MI <- ifelse(M1 <= 0 || M2 <= 0 || M3 <= 0 || M1 > 1 ||  M2 > 1 || M3 > 1 , MI <- 0, MI)
#

#Valor de retorno
print(M1)
print(M2)
print(M3)

return(MI)
}

MI(vga)

##### Matriz de limites para genoud #####
mat <- matrix(rep(c(0,1),ncol(mt)),(ncol(mt)^2),2,byrow = TRUE)
mat
(x[ncol(mt)-(i)]+1):(x[ncol(mt)-(j)]) 

#### Genoud ####
#GA <- genoud(MI, nvars = (ncol(mt)^2), max=TRUE, Domains = mat, data.type.int = TRUE, pop.size = 100, wait.generations = 100 )
GA
GA$value
GA$par
x <- GA$par

##### GA #####
GA <- ga(type = "binary", fitness = MI, nBits = (ncol(mt)^2), upper = max)

plot(GA)
summary(GA)
x <- GA@solution

i <- 6
j <- i + 1



mt[(x[ncol(mt)-(i-1)]+1):(x[ncol(mt)-(j-1)]) , (x[ncol(mt)-(i-1)]+1): (x[ncol(mt)-(j-1)])]


