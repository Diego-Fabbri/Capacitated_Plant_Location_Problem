#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Capacitated Plant Location Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set number of Days
n_days <- 1461

#Set transportation cost
CT <- 0.06

#Set numbers of potential location
n <- 6

#Set number of customers
m <- 7 

#Set demands
d <- c(36, 42, 34, 50, 27, 30, 43)

#Set capacities
cap <- c(80, 90, 110, 120, 100, 120)

#Set fixed costs
CF <- c(321420, 350640, 379860, 401775, 350640, 336030)
for(i in 1:n){
  CF[i] <- CF[i]/n_days
}
#Set warehousing costs
Cs <- c(0.15, 0.18, 0.2, 0.18, 0.15, 0.17)

#Set distances

dist <- matrix(c(18,23,19,21,24,17,9,
                 21,18,17,23,11,18,20,
                 27,18,17,20,23,9,18,
                 16,23,9,31,21,23,10,
                 31,20,18,19,10,17,18,
                 18,17,29,21,22,18,8), nrow = 6, byrow = TRUE )

#Set Costs
costs <- matrix(rep(1,n*m),nrow= n,ncol=m)

for(i in 1:n) {
  for (j in 1:m) {
    costs[i,j] <- 2*CT*dist[i,j]*d[j] + Cs[i]*d[j]
   }
}

#Check feasibility condition
total_demand <- sum(d)
total_capacity <- sum(cap)

if(total_demand <= total_capacity){
  condition <- TRUE
} else{ condition <- FALSE}


#Build Model
if(condition == TRUE){

Model <- MIPModel() %>%
  add_variable(x[i,j], i = 1:n, j = 1:m, type = "continuous", lb=0) %>% #define variables
  add_variable(y[i], i = 1:n , type = "binary") %>%
   set_objective(expr = sum_expr(y[i]*CF[i], i = 1:n) + sum_expr(x[i,j]*costs[i,j], i = 1:n, j = 1:m),
                sense = "min") %>% #define objective
  add_constraint(sum_expr(x[i,j],i = 1:n) == 1, j = 1:m) %>% #define constraints
  add_constraint(sum_expr(x[i,j]*d[j],j = 1:m) <= y[i]*cap[i], i = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1))

#Model summary
##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

##Variables
for (r in 1:n) {
  tmp_y <- get_solution(Model, y[i]) %>%
    filter(variable == "y", i == r) %>%
    select(value)
  
  
  if(tmp_y !=0) {print(paste("--->y[", r , "] =", tmp_y))}
}

for (r in 1:n) {
  for(c in 1:m){
  tmp_x <- get_solution(Model, x[i,j]) %>%
    filter(variable == "x", i == r, j == c) %>%
    select(value)
  
  
  if(tmp_x !=0) {print(paste("--->x[", r,",",c, "] =", tmp_x))}
}
}

} else {print(paste("Feasibility Condition does not hold")) }


