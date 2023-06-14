#===========================================
# This script solves the MILP model for the 
# problem 3: Internet cable production. 

#-------------------------------------------
# Import the lpSolve package. 

library(lpSolve)

#===========================================
# The MILP Model:

#-------------------------------------------
# The Objective Function:

# 26 variables, one for each link
objective.in <- c( 9.66,	9.86,	10.66,	11.06,
                   9.66,	9.86,	10.66,	11.06,
                   9.66,	9.86,	10.66,	11.06,
                   0.2,	0.2,
                   0.2,	0.2,
                   0.2,	0.2,
                   0.2,	0.2)

# The Constraints:

# the coefficient matrix for constrains
const.mat <- matrix(c(
  0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 1
  0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 1
  0,	0,	0,	0,	0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 2
  0,	0,	0,	0,	0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 2
  0,	0,	0,	0,	0,	0,	0,	0,	0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 3
  1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,	0,	0,     # Demand to be fulfill cable A, month 1
  0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,	0,     # Demand to be fulfill cable B, month 1
  0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,     # Demand to be fulfill cable A, month 2
  0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,     # Demand to be fulfill cable B, month 2
  0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,     # Demand to be fulfill cable A, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	1,	0,	-1,    # Demand to be fulfill cable B, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,     # Excess is 0 by beginning of January for cable A
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,     # Excess is 0 by beginning of January for cable B
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,     # Excess is 0 by end of March for cable A
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1     # Excess is 0 by end of March for cable A
  ), nrow=16, byrow=TRUE)

const.dir <- c('<=','<=','<=','<=','<=','<=',    # available hours
               '=','=','=','=','=','=',          # Demand to be fulfill 
               '=','=',                          # Excess is 0 by beginning of January
               '=','='                           # Excess is 0 by end of March
)

const.rhs <- c(1400,3000,600,800,2000,600,        # available hours
               8000,2000,16000,10000,6000,10000,  # Demand to be fulfill 
               0, 0,                              # Excess is 0 by beginning of January
               0, 0                               # Excess is 0 by end of March
)

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
# Try again with relaxed constrains
# This time there can be excess by the end of march

const.mat <- matrix(c(
  0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 1
  0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 1
  0,	0,	0,	0,	0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 2
  0,	0,	0,	0,	0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 2
  0,	0,	0,	0,	0,	0,	0,	0,	0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 3
  1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,	0,	0,         # Demand to be fulfill cable A, month 1
  0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,	0,         # Demand to be fulfill cable B, month 1
  0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,         # Demand to be fulfill cable A, month 2
  0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,         # Demand to be fulfill cable B, month 2
  0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,         # Demand to be fulfill cable A, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	1,	0,	-1,        # Demand to be fulfill cable B, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,         # Excess is 0 by beginning of January for cable A
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0#,         # Excess is 0 by beginning of January for cable B
  # 0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,         # Excess is 0 by end of March for cable A
  # 0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1         # Excess is 0 by end of March for cable A
  ), nrow=14, byrow=TRUE)

const.dir <- c('<=','<=','<=','<=','<=','<=',    # available hours
               '=','=','=','=','=','=',          # Demand to be fulfill 
               '=','='#,                         # Excess is 0 by beginning of January
               # '=','='                         # Excess is 0 by end of March
               )

const.rhs <- c(1400,3000,600,800,2000,600,        # available hours
               8000,2000,16000,10000,6000,10000,  # Demand to be fulfill 
               0,0#,                              # Excess is 0 by beginning of January
               # 0, 0                             # Excess is 0 by end of March
               )

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = FALSE)
model
model$solution

#-------------------------------------------
# Try again with relaxing hour availability constrains

const.mat <- matrix(c(
  # 0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 1
  # 0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 1
  # 0,	0,	0,	0,	0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 2
  # 0,	0,	0,	0,	0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 2
  # 0,	0,	0,	0,	0,	0,	0,	0,	0.3,	0,	0.24,	0,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 1, month 3
  # 0,	0,	0,	0,	0,	0,	0,	0,	0,	0.32,	0,	0.28,	0,	0,	0,	0,	0,	0,	0,	0, # available hours plant 2, month 3
  1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,	0,	0,         # Demand to be fulfill cable A, month 1
  0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,	0,         # Demand to be fulfill cable B, month 1
  0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,	0,         # Demand to be fulfill cable A, month 2
  0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,	0,         # Demand to be fulfill cable B, month 2
  0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,	0,	-1,	0,         # Demand to be fulfill cable A, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	1,	0,	-1,        # Demand to be fulfill cable B, month 3
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,         # Excess is 0 by beginning of January for cable A
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0#,         # Excess is 0 by beginning of January for cable B
  # 0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,         # Excess is 0 by end of March for cable A
  # 0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1         # Excess is 0 by end of March for cable A
  ), nrow=8, byrow=TRUE)

const.dir <- c(#'<=','<=','<=','<=','<=','<=',    # available hours
               '=','=','=','=','=','=',          # Demand to be fulfill 
               '=','='#,                         # Excess is 0 by beginning of January
               # '=','='                         # Excess is 0 by end of March
)

const.rhs <- c(#1400,3000,600,800,2000,600,        # available hours
               8000,2000,16000,10000,6000,10000,  # Demand to be fulfill 
               0,0#,                              # Excess is 0 by beginning of January
               # 0, 0                             # Excess is 0 by end of March
)

model <- lp(direction="min",
            objective.in = objective.in,
            const.mat = const.mat,
            const.dir = const.dir,
            const.rhs = const.rhs,
            all.int = TRUE)
model
model$solution

