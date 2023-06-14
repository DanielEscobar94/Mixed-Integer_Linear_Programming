#===========================================
# This script solves the MILP model for the 
# problem 1: nurse planning. 

#-------------------------------------------
# Import the lpSolve package. 

library(lpSolve)

#===========================================
# The MILP Model:

#-------------------------------------------
# The Objective Function:

objective.in <- c(1250,1315,1440,1440,1440,1440,1375, #coefficient vector for 14 variables
                  450, 450, 450, 485, 560, 560, 525)

#-------------------------------------------
# The Constraints:

# first five constraints
const.mat <- matrix(c(1,0,0,1,1,1,1,1,0,0,0,0,1,1, # Monday
                      1,1,0,0,1,1,1,1,1,0,0,0,0,1, # Tuesday
                      1,1,1,0,0,1,1,1,1,1,0,0,0,0, # Wednesday
                      1,1,1,1,0,0,1,0,1,1,1,0,0,0, # Thursday
                      1,1,1,1,1,0,0,0,0,1,1,1,0,0, # Friday
                      0,1,1,1,1,1,0,0,0,0,1,1,1,0, # Saturday
                      0,0,1,1,1,1,1,0,0,0,0,1,1,1, # Sunday
                      1,1,1,1,1,1,1,-3,-3,-3,-3,-3,-3,-3 # Par-time <= 0.25 Total 
                      ), nrow=8,byrow=TRUE)

#-------------------------------------------
# The Equality/inequality Signs:

const.dir <- c(rep(">=",8))

#-------------------------------------------
# The Right Hand Side Parameters (Constants):

const.rhs <- c(17, 13, 15, 19, 14, 16, 11, 0)

#-------------------------------------------
# Mathematical Programming Setting:

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = TRUE)
model
model$solution

