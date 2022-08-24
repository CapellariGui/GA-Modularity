# https://medium.com/genetic-algorithm-ga-using-r-package-rgenoud/genetic-algorithm-ga-with-r-package-rgenoud-d176daa5543e

##### Exemple 1 ##### 

setwd("C:/Users/capel/Dropbox/TCC/Genetic Algorithms")
install.packages("rgenoud")
library("rgenoud")

##---
# Using GA to solve an equation with floating points.
##---
#Define the objective function
func <-  function(sol){
  #GA provides the solution/population as a vector of length 3  
  x1 <- sol[1]
  x2 <- sol[2]
  x3 <- sol[3]
  # Equation
  y=3*x1 + tan(x2) - cos(x3)
  #Return the value to GA  
  return(y)
}

func(sol)



# Set the boundary values for each parameter X1,X2 and X3
# X1 should be between value 3 and 5. x2 between 2 and 8.
mat <- matrix(c(3,5,2,8,1,4),3,2,byrow = TRUE)
mat
# Run the GA Algorithm
GA <- genoud(func,nvars = 3,max = TRUE,pop.size = 100,max.generations = 100,wait.generations = 10,Domains = mat,boundary.enforcement = 2,print.level = 2)
GA
# Maximum possible solution of the equations 
GA$value
# The parameters of the solutions
GA$par

sol <- GA$par
##### Exemple 2 ##### 


#GA to solve discrete data to simulate multiple scenarios and use constrains and penalty.
library("rgenoud")
#Create the tables
Prod <- data.frame(prod=c('P1','P2','P3','P4','P5','P6','P7','P8','P9','P10'),Vol=c(5,10,15,5,20,15,10,20,5,5))
WH <- data.frame(WH=c('1','2','3','4'),WH.Cost=c(5,10,12,15),Capacity=c(25,25,25,55))
func <- function(sol){
  
  #Add the product-warehouse allignment by GA to the product table
  Prod <- cbind(Prod,sol)
  
  #Obtain the warehouse costs based on the stocking allginments
  Prod <- merge(Prod,WH,by.x="sol",by.y="WH")
  
  #Determine the total warehouse stocking costs
  Prod$cost <- Prod$Vol * Prod$WH.Cost
  
  #Total stocking Cost
  Tot.Cost <- sum(Prod$cost)
  
  #Check for Warehouse Capacity
  Cap <- aggregate(Prod$Vol,by=list(Prod$sol),FUN=sum)
  Cap$Group.1 <- as.factor(Cap$Group.1)
  
  #Apply Penalty if the capacity constrains are not met
  Tot.Cost <- ifelse(Cap[Cap$Group.1=='1',2] > 25 || Cap[Cap$Group.1=='2',2] > 25 || Cap[Cap$Group.1=='3',2] > 25 || Cap[Cap$Group.1=='4',2] > 55,Tot.Cost * 1000,Tot.Cost)
  #Return Value
  return(Tot.Cost)
}#End of objective function
# Define Boundary Matrix
# Each product should be stocked across any of the 4 warehouses
mat <- matrix(rep(c(1,4),10),10,2,byrow = TRUE)
GA <- genoud(func,nvars = 10,max = FALSE,pop.size = 500,max.generations = 100,wait.generations = 10,Domains = mat,boundary.enforcement = 2,data.type.int = TRUE)
#data.type.int will ensure GA produces only integers between the set domains ie 1 & 4
# Maximum possible solution of the equations 
GA$value
# The parameters of the solutions
GA$par

sol <- GA$par
func(sol)
??rgenoud
