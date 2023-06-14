#===========================================
# This script solves the MILP model for the 
# problem 2: chip-set logistics. 

#-------------------------------------------
# Import the lpSolve package. 

library(lpSolve)

#===========================================
# The MILP Model:

#-------------------------------------------
# The Objective Function:

# 26 variables, one for each link
objective.in <- c( 100 ,  60  ,  100 ,  100 ,  400 ,  400 ,  
                   180 ,  180 ,  20  ,  20  ,  160 ,  300 ,
                   8   ,  160 ,  20  ,  10  ,  200 ,  240 ,  
                   24  ,  40  ,  240 ,  16  ,  40  ,  240 ,  
                   20  ,  140 )


# The Constraints:

m1 <- matrix(c(1,	1,	1,	1,	1,	1,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               -1,	0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,
               0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	0,	1,	-1,
               0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	-1,	1
               ), nrow=5, byrow=TRUE)

m2 <- diag(26) # each link can transport at most 400k

const.mat <- rbind(m1,m2) # join previous arrays of constrains

const.dir <- c("<=","<=","<=","=","=",rep("<=", 26))

const.rhs <- c(400, 600, 200, 400, 180, rep(400, 26))

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = TRUE)
model
model$solution

#-------------------------------------------
# by relaxing the model to all positive rational numbers:

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = FALSE)
model
model$solution

#-------------------------------------------
# try again by RELAXING PRODUCTION CAPACITY constrains:

m1 <- matrix(c(# 1,	1,	1,	1,	1,	1,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 1 produce at most 400k
               # -1,	0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,  #Fabric 2 produce at most 600K
               # 0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 2 produce at most 200K
               0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	0,	1,	-1,   #Plant 1 demand exactly 400K
               0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	-1,	1     #Plant 1 demand exactly 180K
               ), nrow=2, byrow=TRUE)

m2 <- diag(26) # each link can transport at most 400k

const.mat <- rbind(m1,m2) # join previous arrays of constrains

const.dir <- c(#"<=","<=","<=",
               "=","=",rep("<=", 26))

const.rhs <- c(#400, 600, 200,
               400, 180, rep(400, 26))

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = TRUE)
model
model$solution


#-------------------------------------------
# try again by RELAXING DEMAND CONSTRAINS. Plants now demand only 10% of original demands:

m1 <- matrix(c(
  1,	1,	1,	1,	1,	1,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 1 produce at most 400k
  -1,	0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 2 produce at most 600K
  0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 2 produce at most 200K
  0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	0,	1,	-1,   #Plant 1 demand exactly least 400K/10
  0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	-1,	1     #Plant 1 demand exactly 180K/10
), nrow=5, byrow=TRUE)

m2 <- diag(26) # each link can transport at most 400k

const.mat <- rbind(m1,m2) # join previous arrays of constrains

const.dir <- c("<=","<=","<=",
               "=","=", rep("<=", 26))

const.rhs <- c(  400, 600, 200,
                 400/10, 180/10, 
                 rep(400, 26))

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = TRUE)
model
model$solution


#-------------------------------------------
# try again by RELAXING TRANSPORT CAPACITIES:

m1 <- matrix(c(
  1,	1,	1,	1,	1,	1,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 1 produce at most 400k
  -1,	0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	-1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 2 produce at most 600K
  0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,    #Fabric 2 produce at most 200K
  0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	0,	1,	-1,   #Plant 1 demand exactly least 400K/10
  0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	0,	-1,	0,	0,	-1,	0,	0,	-1,	-1,	1     #Plant 1 demand exactly 180K/10
), nrow=5, byrow=TRUE)

# m2 <- diag(26) # each link can transport at most 400k

const.mat <- rbind(m1)#),m2) # join previous arrays of constrains

const.dir <- c("<=","<=","<=",
               "=","="#, rep("<=", 26)
               )

const.rhs <- c(  400, 600, 200,
                 400, 180#, rep(400, 26)
                 )

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = TRUE)
model
model$solution
