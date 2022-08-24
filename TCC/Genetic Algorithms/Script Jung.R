setwd("C:/Users/capel/Dropbox/TCC/Genetic Algorithms")
library("readxl")
library("dplyr")
library("GA")
library("rgenoud")

{
mt <- read_excel("Matriz teste M3.xlsx")
mt <- mt %>% select(-1)
mt <- as.matrix(mt)
dimnames(mt) <- (list(c(1:ncol(mt)),c(1:ncol(mt))))
mt
ncol(mt)


#Definir cromossomo

x <- c(0,0,0,0,0,0,0,0,0,0,16,15,14,9,5,3,0) 
length(x)

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


############### Cálculo de MI1 ###############
MI1 <- function(x){
nm <- (sum(x!=0)+1)
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
    WCj <- sum((mt[(x[length(x)-(i)]+1):nc , (x[length(x)-(i)]+1):nc])!=0)
    WSj <- sum(mt[(x[length(x)-(i)]+1):nc , (x[length(x)-(i)]+1):nc])
   
    ST <- ST + ((BC/WC)+(BC/WCj)+(BS/WS)+(BS/WSj))
    SWC <- SWC + WC
    q <- x[length(x)-(j-1)]
    p <- (x[length(x)-(i-1)]+1)
    qp <- qp + (q-p+1)^2
  #  print((x[length(x) - (i)] + 1):(x[length(x) - (j)]) )
    
   
    #print(ST)
  } else {
  
  if (i == nm){
    WC <- sum((mt[(x[length(x)-(i-1)]+1):nc , (x[length(x)-(i-1)]+1):nc])!=0)
    WS <- sum(mt[(x[length(x)-(i-1)]+1):nc , (x[length(x)-(i-1)]+1):nc])
   
    ST <- ST + ((BC/WC)+(BC/WCj)+(BS/WS)+(BS/WSj))
    SWC <- SWC + WC
    q <- nc
    p <- (x[length(x)-(i-1)]+1)
    qp <- qp + (q-p+1)^2
 #   print((x[length(x) - (i)] + 1):(x[length(x) - (j)]) )
    
   
    #print(ST)
    break
  } else {
    WC <- {
      sum((mt[(x[length(x)-(i-1)]+1):(x[length(x)-(j-1)]) , (x[length(x)-(i-1)]+1): (x[length(x)-(j-1)])])!=0)
    }
    BC <- {
      sum((mt[(x[length(x)-(j-1)]+1):ncol(mt),(x[length(x)-(i-1)]+1):x[length(x)-(j-1)]])!=0) +  
        sum((mt[(x[length(x)-(i-1)]+1):x[length(x)-(j-1)],(x[length(x)-(j-1)]+1):ncol(mt)])!=0)  
    }
    WCj <- {
      sum((mt[(x[length(x)-(i)]+1):(x[length(x)-(j)]) , (x[length(x)-(i)]+1):(x[length(x)-(j)])])!=0)
    }
    WS <- {
      sum(mt[(x[length(x)-(i-1)]+1):(x[length(x)-(j-1)]) , (x[length(x)-(i-1)]+1):(x[length(x)-(j-1)])])
    }
    BS <- {
      sum(mt[(x[length(x)-(j-1)]+1):ncol(mt),(x[length(x)-(i-1)]+1):x[length(x)-(j-1)]]) +  
        sum(mt[(x[length(x)-(i-1)]+1):x[length(x)-(j-1)],(x[length(x)-(j-1)]+1):ncol(mt)])  
    }
    WSj <- {
      sum(mt[(x[length(x)-(i)]+1):(x[length(x)-(j)]) , (x[length(x)-(i)]+1):(x[length(x)-(j)])])
    }
    q <- x[length(x)-(j-1)]
    p <- (x[length(x)-(i-1)]+1)
    qp <- qp + (q-p+1)^2
    
  }

ST <- ST + ((BC/WC)+(BC/WCj)+(BS/WS)+(BS/WSj))
SWC <- SWC + WC
#print(ST)
#print((x[length(x) - (i)] + 1):(x[length(x) - (j)]) )

}
}

M1 <- 1-(ST/(2*nm*(nm-1)))
#print(M1)
M2 <- (SWC + nc)/qp
#print(M2)
M3 <- 0.9375
MI <- (M1 * (1/3)) + (M2 * (1/3)) + (M3 * (1/3))

return (MI)
#return(nm)

}


MI1(x)
x
X
mat <- matrix(rep(c(0,ncol(mt)-1),ncol(mt)),ncol(mt),2,byrow = TRUE)
mat

#rm(x)
(x[length(x)-(i)]+1):(x[length(x)-(j)]) 
#### Genoud ####

GA <- genoud(MI1, nvars = ncol(mt), max=TRUE, Domains = mat, data.type.int = TRUE, pop.size = 100 )
GA
GA$value
GA$par
X <- GA$par
Y
x

?ga
########## BC > Número de conexões entre os módulos i-ésimo e j-ésimo #####
i <- 1
j <- i + 1
mt[(x[length(x)-(i-1)]+1):x[length(x)-(j-1)],(x[length(x)-(j-1)]+1):ncol(mt)]
mt[(x[length(x)-(j-1)]+1):ncol(mt),(x[length(x)-(i-1)]+1):x[length(x)-(j-1)]]



for (i in 1:(nm-1)) {
  j <- i+1
  BC <- function(i,j) {
    sum((mt[(x[length(x)-(j-1)]+1):ncol(mt),(x[length(x)-(i-1)]+1):x[length(x)-(j-1)]])!=0) +  
      sum((mt[(x[length(x)-(i-1)]+1):x[length(x)-(j-1)],(x[length(x)-(j-1)]+1):ncol(mt)])!=0)  
  }
  print(BC)    
}

BC <- 0
for (i in 1:(nm-1)) {
  j <- i+1
  BC <- BC + {
    sum((mt[(x[length(x)-(j-1)]+1):ncol(mt),(x[length(x)-(i-1)]+1):x[length(x)-(j-1)]])!=0) +  
      sum((mt[(x[length(x)-(i-1)]+1):x[length(x)-(j-1)],(x[length(x)-(j-1)]+1):ncol(mt)])!=0)  
  }
  
  print(BC)  
}


########## WC > Número de conexões dentro do i-ésimo módulo ##########


{
  #WC <- 0
  #for (i in 1:(nm-1)){
  #  j <- i+1
  #  WC <- WC + {
  #    sum((mt[(x[length(x)-(i-1)]+1):(x[length(x)-(j-1)]) , (x[length(x)-(i-1)]+1): (x[length(x)-(j-1)])])!=0)
  #  }
  #print(WC)
  #  }
  
  #i <- 7
  #j <- i + 1 
  #(mt[(x[length(x)-(i-1)]+1):nc , (x[length(x)-(i-1)]+1):nc])
  
  #(x[length(x)-(i-1)]+1)
  #(x[length(x)-(j-1)])
  #nc
}

WC <- 0
for (i in 1:(nm)){
  j <- i+1
  if (i == nm){
    WC <- WC + sum(mt[(x[length(x)-(i-1)]+1):nc , (x[length(x)-(i-1)]+1):nc])
    print(WC)
    break
  } else {
    WC <- WC + {
      sum((mt[(x[length(x)-(i-1)]+1):(x[length(x)-(j-1)]) , (x[length(x)-(i-1)]+1): (x[length(x)-(j-1)])])!=0)
    }
  }
  print(WC)
}

########## BS > Soma da intereção dos valores entre os módulos i-ésimo e j-ésimo ##########
BS <- 0 
for (i in 1:(nm-1)) {
  j <- i+1
  BS <- BS + {
    sum(mt[(x[length(x)-(j-1)]+1):ncol(mt),(x[length(x)-(i-1)]+1):x[length(x)-(j-1)]]) +  
      sum(mt[(x[length(x)-(i-1)]+1):x[length(x)-(j-1)],(x[length(x)-(j-1)]+1):ncol(mt)])  
  }
  
  print(BS)  
}

########## WS > Soma da intereção dos valores dentro do i-ésimo módulo ##########
WS <- 0
for (i in 1:(nm)){
  j <- i+1
  if (i == nm){
    WS <- WS + sum(mt[(x[length(x)-(i-1)]+1):nc , (x[length(x)-(i-1)]+1): nc])
    print(WS)
    break
  } else {
  WS <- WS + {
    sum(mt[(x[length(x)-(i-1)]+1):(x[length(x)-(j-1)]) , (x[length(x)-(i-1)]+1): (x[length(x)-(j-1)])])
  }
  }
  print(WS)
}



########## q > Índice do último componente do i-ésimo módulo ##########

i <- 1
j <- i + 1

x[length(x)-(j-1)]

######### p > Índice do primeiro componente do i-ésimo módulo #########

(x[length(x)-(i-1)]+1)

##### R Rij is the value of the ith row and jth column element in DSM #####

Rmax <- max(mt)

SR1 <- SR1 + ((1-(j-i)/(nc-1))*(Rij + Rji)/Rmax)
SR2 <- SR2 + ((Rij + Rji)/Rmax)

M3 <- SR1/SR2



########## Acompanhamento ##########

# Calculos dos W não estão pegando último módulo - Solucionado
# Lembrar de que na fórmula há WSi, WSj, WCi e WCj - Solucionado 
# Módificar gene do primeiro módulo no vetor cromossomo de 0 para 1  - Solucionado - Acredito que não haverá necessidade
# Estudar como idenificar o valor de Rij 



k <- 1
for (f in 1:5){
  for (g in f+1:6){
    k <- k+f+g
    print (k, f, g)
    
    
  }
} 

for (i in 1:7){
  
j <- i+1

M <- (mt[(x[length(x)-(i)]+1):(x[length(x)-(j)]) , (x[length(x)-(i)]+1):(x[length(x)-(j)])])
print(M)
}
