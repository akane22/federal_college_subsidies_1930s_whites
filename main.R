setwd("C:/Users/abkan/Documents/Jaynes Education/replication package/")


fig_width <- 7.5 
fig_height <- 6

# libraries
library(tidyverse)
library(stringr)
library(doParallel)
library(foreach)
library(doRNG)
library(latex2exp)
library(spatstat)

# parameters
mean_c <- 1 #mean of ability distribution
sd_c <- 0.15 # standard deviation of ability distribution
b <- 0.5 # weight of father's ability in ability assignment
d <- 0.2 #sd of random variable in ability assignment
cpi_old <- 17.5 # 1925 CPI for adjusting G1 wages
cpi_new <- 13.9 # 1939 CPI
beta <- .65 # altruism discount on future utility
beta_prime <- .91 # time discount on future utility
s_male <- 0.8 #remittance percent for sons
s_female <- 0.2 #remittance percent for daughters
g2_kids <- 2.2 # expected number of kids G2s will have
portion_of_budget_num <- 4 # how earnings are distributed among household for consumption, children receive wage / (4+number of children), parents receive double
g2_male_life_expectancy <- 66.92
g2_female_life_expectancy <- 21.5 + 47.66
g1_life_expectancy <- 26 + 37.83
g1_marriage_age <- 26
g1_period2 <- g1_life_expectancy - g1_marriage_age
male_marriage_age <- 25.5
female_marriage_age <- 23.5
diff <- (g2_female_life_expectancy - g2_male_life_expectancy) + (male_marriage_age - female_marriage_age) # women expect to marry older husbands with shorter life expectancy, so this is the difference between expected G2 period length for men and women
x <- 0.75 # elasticity of substitution
p <- .55 # probability with which sons chosen to be sent to college if only one gender can be sent
start_school <- 6 # age at which children start school
g2_first_period_hours <- 1348/2000 #hours G2 men work in first period of life
g2_first_period_hours_female <- 1225/2000 #hours G2 women work in first period of life
g2_college_men_hours <- 255/2000 #hours worked by G2 men while attending college (does not include work-study)
g2_college_women_hours <- 250/2000 #hours worked by G2 women while attending college (does not include work-study)
g2_second_period_hours <- 1100/2000 #hours G2 men expect to work in second period
g2_second_period_hours_female <- g2_second_period_hours #women are assumed to be homemakers, so these are the husband's hours worked
g1_hours <- 3100/2000 #hours worked for G1s in second period
C = .2975 #595/2000 #annual cost of college including room and board
stipend_budget <- 33230000
educ_range <- 0:16
n <- 1000 #number of simulations
rep_eq_stipend <- 137.01124/2000
universal_stipend <- 113.278805/2000


# data cleaning
source("source/census_data_cleaning.R")

# functions
source("source/utility_max_functions.R")

# stipend simulations


# replication equilibrium
stipend <- rep_eq_stipend
source("source/educational_attainment_simulation.R")
c_cutoff_rep_eq <- c_cutoff

# stipend = 0
stipend <- 0
source("source/educational_attainment_simulation.R")

# universal policy
stipend <- universal_stipend
source("source/educational_attainment_simulation.R")

# figures
source("source/figures_1_and_2_supply_and_demand_and_a(S).R")

stipend <- rep_eq_stipend*2000
universal_stipend <- universal_stipend*2000
source("source/figures_4_5_6_7_8_grad_by_ses_and_male_female_comparisons.R")

# robustness checks

