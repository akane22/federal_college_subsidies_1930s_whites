
# Initialize for parallelization
numCores <- detectCores()
registerDoParallel(numCores)

# Set up and parameters
set.seed(9021)
registerDoRNG(9021)


# This makes a template of possible grade levels to evaluate utility
template <- expand.grid(rep(list(0:16), 2))
template <- template %>% rename(educ_men = Var1, educ_women = Var2)
template$utility <- NA

stipend_vec <- runif(n, 90, 160)/2000

# Read data
men <- read.csv("data/1940_men_cleaned.csv") %>% filter(nhwhite == 1) %>% rename(educ = HIGRADE) # read 1940 census, men
men$hourwage <- as.numeric(men$hourwage)
men$hourwage[is.nan(men$hourwage)] <- NA
women <- read.csv("data/1940_women_cleaned.csv") %>% filter(nhwhite == 1) %>% rename(educ = HIGRADE) # read 1940 census, women
women$hourwage <- as.numeric(women$hourwage)
women$hourwage[is.nan(women$hourwage)] <- NA

# Wage Regression
# create function to do variable interactions
interact <- function(data, first, second){
  parse(text = paste0(data, '$', first, '_', second, ' <- ', data, '$', first, '*', data, '$', second))
}

eval(interact('men', 'educ', 'exper'))
eval(interact('men', 'exper', 'south'))
eval(interact('women', 'educ', 'exper'))
eval(interact('women', 'exper', 'south'))

model <- as.formula("hourwage ~ educ + exper + educ_exper + exper_south + south + professional_managers + 
                         foremen + professional_sales + professional_elites + professional_technical + academic_elites")

zero <- men %>% filter(hourwage <= 0 | hoursworked == 0) #separate everyone making non positive wages or working 0 hours
men <- men %>% filter(hoursworked > 0) %>% filter(hourwage > 0) #filter so just men with positive wages and positive hours worked
lm_men <- lm(model, men) #regression
zero$hourwage <- predict(lm_men, zero) #for men with 0 wages or 0 hours worked, impute wage with regression
men <- bind_rows(men, zero) #combine the two dataframes again

lm_women <- lm(model, women %>% filter(hoursworked > 0)) # regression on women, only for women with positive hours worked. Since hours worked includes unpaid family labor, we still include in regression those with zero wages because this factors into expectation of wages when choosing educational investment

# define generations
g3 <- men %>% filter(yearbirth >= 1917 & yearbirth <= 1926)
g2 <- men %>% filter(yearbirth >= 1912 & yearbirth <= 1916)
g1 <- men %>% filter(yearbirth <= 1906)
women_g2 <- women %>% filter(year_of_birth >= 1912 & year_of_birth <=1916)


# We use 1940 Census data which have earnings from 1939. We assume G1 makes the education investment decision for G2 in 1925 (on average). As a result, we estimate the wages G1's would have earned 14 years prior by removing 14 years of the experience coefficients in the regression
g1$hourwage_t <- g1$hourwage - 14*lm_men$coefficients[["exper"]] - 14*g1$educ*lm_men$coefficients[["educ_exper"]] - 
  14*g1$south*lm_men$coefficients[["exper_south"]]
g1$hourwage_t[which(g1$hourwage_t < 0)] <- 0 #Anyone who's wages have become negative due to the experience adjustment we set to zero.
g1$hourwage_t <- g1$hourwage_t*(cpi_old/cpi_new)*g1_hours #adjust to 1925 prices

means_lmg2 <- mean_wages(lm_men, "male") # calculate expected first and second period wages for g2 men using regression
means_lmg2_women <- mean_wages(lm_women, "female")# calculate expected first and second period wages for g2 women using regression


# In order to assign c values for the children, we have to calculate each G1's wage percentile. 
# We do this separately for North and South because there is a disparity in wages between the two
g1$w_percentile <- NA
p_south <- ecdf(g1$hourwage[which(g1$south == 1)]) #ecdf function is empirical cdf. p <- ecdf(data) is a function in next line...
g1$w_percentile[which(g1$south == 1)] <- p_south(g1$hourwage[which(g1$south == 1)]) # ...p(data) gives the percentile

p_nsouth <- ecdf(g1$hourwage[which(g1$south == 0)])
g1$w_percentile[which(g1$south == 0)] <- p_nsouth(g1$hourwage[which(g1$south == 0)])

# We will get some with wage percentile 0 and some with wage percentile 100. 
# Since this is converted to a value in the normal distribution, we can't use 0 or 1 (normal is continuous rv so 0th percentile is -infinity and
# 100th is +infinity), so we set wage percentile 0 to 0.0001 and percentile 1 to 1-0.0001
g1$w_percentile[which(g1$w_percentile < 0.0001)] <- 0.0001
g1$w_percentile[which(g1$w_percentile > (1 - 0.0001))] <- 1 - 0.0001


output_table <- 
  foreach(j = stipend_vec, .combine = 'rbind') %dopar% {
    g1$c <- sapply(g1$w_percentile, c_fcn, mean = mean_c, sd = sd_c)
    g1$c[which(g1$c < 0.01)] <- 0.01 
    
    for(i in 1:nrow(g1)) {
      output <- educational_attainment_g1(wage_g1 = g1$hourwage_t[i], c = g1$c[i], 
                                          south = g1$south[i], N = g1$CHBORN[i], 
                                          beta=beta, beta_prime = beta_prime, C = C, 
                                          s_male = s_male, s_female = s_female, 
                                          g2_college_men_hours =g2_college_men_hours, 
                                          g2_college_women_hours = g2_college_women_hours, 
                                          stipend = j)
      g1$son_education[i] <- output$educ_men
      g1$daughter_education[i] <- output$educ_women
      g1$sons[i] <- output$sons
      g1$daughters[i] <- output$daughters
      g1$type[i] <- output$type
      g1$second_time[i] <- output$second_time
    }
    
    
    stipend_recipients <- which(g1$type == 2 & (g1$son_education == 12 | g1$daughter_education == 12))
    
    stipend_df <- g1[stipend_recipients,]
    g1 <- g1[-stipend_recipients,]
    stipend_df <- stipend_df[, -which(names(stipend_df) %in% c("son_education", "daughter_education"))]
    
    for(i in 1:nrow(stipend_df)){
      output <- educational_attainment_stipend(wage_g1 = stipend_df$hourwage_t[i], 
                                               c = stipend_df$c[i], south = stipend_df$south[i], 
                                               sons = stipend_df$sons[i], daughters = stipend_df$daughters[i], 
                                               second_time = stipend_df$second_time[i], 
                                               beta = beta, beta_prime = beta_prime, 
                                               C = C, s_male = s_male, s_female = s_female, 
                                               g2_college_men_hours = g2_college_men_hours, 
                                               g2_college_women_hours = g2_college_women_hours, 
                                               stipend = j)
      stipend_df$son_education[i] <- output$educ_men
      stipend_df$daughter_education[i] <- output$educ_women
    }
    g1 <- rbind(g1, stipend_df)
    vec <- c(0, sort(g1$c[(g1$type == 2 & g1$sons > 0 & g1$son_education > 12) | 
                            (g1$type == 2 & g1$daughters & g1$daughter_education > 12)]))
    tuitions_supplied <- round(stipend_budget/(j*2000), 2)
    i <- 1
    male_sti <- g1[g1$son_education > 12 & g1$sons > 0 & g1$type == 2 & g1$c > vec[1], c("sons", "c", "son_education")]
    female_sti <- g1[g1$daughter_education > 12 & g1$daughters > 0 & g1$type == 2 & g1$c > vec[1], c("daughters", "c", "daughter_education")]
    male_sti$son_education <- male_sti$son_education - 12
    female_sti$daughter_education <- female_sti$daughter_education - 12
    tuitions_demanded <- round((sum((male_sti$son_education)*(1/male_sti$c)*male_sti$sons) + 
                                  sum((female_sti$daughter_education)*(1/female_sti$c)*female_sti$daughters))*6.67, 2)
    excess_demand <- tuitions_demanded - tuitions_supplied
    
    while(excess_demand > 0){
      i <- i + 1
      male_sti <- male_sti[male_sti$c > vec[i],]
      female_sti <- female_sti[female_sti$c > vec[i],]
      tuitions_demanded <- round((sum((male_sti$son_education)*(1/male_sti$c)*male_sti$sons) + 
                                    sum((female_sti$daughter_education)*(1/female_sti$c)*female_sti$daughters))*6.67, 2)
      excess_demand <- tuitions_demanded - tuitions_supplied
    }
    tuitions <- tuitions_demanded
    stipends <- tuitions*(j*2000)
    c_cutoff <- vec[i]
    
    eligible_sons_percent <-  sum(g1$sons[which(g1$son_education > 12 & g1$type == 2 & g1$c > c_cutoff)])/
      sum(g1$sons[which(g1$son_education > 12 & g1$type == 2)])
    eligible_daughters_percent <-  sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2 & g1$c > c_cutoff)])/
      sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)])
    
    g1$son_education[which(g1$c <= c_cutoff & g1$son_education > 12 & g1$type == 2)] <- 12
    g1$daughter_education[which(g1$c <= c_cutoff & g1$daughter_education > 12 & g1$type == 2)] <- 12
    min_c_sti <- min(g1$c[which(g1$son_education>12 & g1$type == 2 & g1$sons > 0)], 
                     g1$c[which(g1$daughter_education > 12 & g1$type == 2 & g1$daughters > 0)])
    mean_c_sti <- (sum(g1$c[which(g1$sons > 0 & g1$type == 2 & g1$son_education > 12)]*
                         g1$sons[which(g1$sons > 0 & g1$type == 2 & g1$son_education > 12)]) + 
                     sum(g1$c[which(g1$daughters > 0 & g1$type == 2 & g1$daughter_education > 12)]*
                           g1$daughters[which(g1$daughters > 0 & g1$type == 2 & g1$daughter_education > 12)]))/
      (sum(g1$sons[which(g1$son_education > 12 & g1$type == 2)]) + 
         sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)]))
    
    g1$son_education[which(g1$sons == 0)] <- NA
    g1$daughter_education[which(g1$daughters == 0)] <- NA
    
    min_c_no_sti <- min(min(g1$c[which(g1$sons > 0 & g1$type == 1 & g1$son_education > 12)]), 
                        min(g1$c[which(g1$daughters > 0 & g1$type == 1 & g1$daughter_education > 12)])) 
    mean_c_no_sti <- (sum(g1$c[which(g1$sons > 0 & g1$type == 1 & g1$son_education > 12)]*
                            g1$sons[which(g1$sons > 0 & g1$type == 1 & g1$son_education > 12)]) + 
                        sum(g1$c[which(g1$daughters > 0 & g1$type == 1 & g1$daughter_education > 12)]*
                              g1$daughters[which(g1$daughters > 0 & g1$type == 1 & g1$daughter_education > 12)]))/
      (sum(g1$sons[which(g1$son_education > 12 & g1$type == 1)]) + 
         sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 1)]))
    
    act_men_college <- length(which(g2$educ > 12))/nrow(g2)
    act_women_college <- length(which(women_g2$educ > 12))/nrow(women_g2)
    act_men_grad <- length(which(g2$educ == 16))/nrow(g2)
    act_women_grad <- length(which(women_g2$educ == 16))/nrow(women_g2)
    
    sim_men_college <- sum(g1$sons[which(g1$son_education > 12)])/sum(g1$sons)
    sim_women_college <- sum(g1$daughters[which(g1$daughter_education > 12)])/sum(g1$daughters)
    sim_men_grad <- sum(g1$sons[which(g1$son_education == 16)])/sum(g1$sons)
    sim_women_grad <- sum(g1$daughters[which(g1$daughter_education == 16)])/sum(g1$daughters)
    
    data.frame(stipend = j, a_cutoff_for_stipend = c_cutoff, min_a_stipend = min_c_sti, 
               min_a_no_stipend = min_c_no_sti, mean_a_stipend = mean_c_sti, 
               mean_a_no_stipend = mean_c_no_sti, quantity_stipends_demanded = tuitions, 
               dollar_stipends_demanded = stipends, quantity_stipends_supplied = stipend_budget/(j*2000),
               dollars_stipends_supplied = stipend_budget, college_men_act = act_men_college, 
               college_men_sim = sim_men_college, grad_men_act = act_men_grad, grad_men_sim = sim_men_grad,
               college_women_act = act_women_college, college_women_sim = sim_women_college, 
               grad_women_act = act_women_grad, grad_women_sim = sim_women_grad)
  }

stopImplicitCluster()
output_table$stipend <- output_table$stipend*2000
write.csv(output_table, "output/simulated_matrix_with college_percentages.csv", row.names = FALSE)