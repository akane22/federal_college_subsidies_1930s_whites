
# Initialize for parallelization
numCores <- detectCores()
registerDoParallel(numCores)

# Set up and parameters
set.seed(9021)
registerDoRNG(9021) # for reproducible results

# This makes a template of possible grade levels to evaluate utility
template <- expand.grid(rep(list(0:16), 2))
template <- template %>% rename(educ_men = Var1, educ_women = Var2)
template$utility <- NA


## Read data and then run simulation

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

g1$c <- sapply(g1$w_percentile, c_fcn, mean = mean_c, sd = sd_c) # assign ability to g2s
g1$c[which(g1$c < 0.01)] <- 0.01 # in case of negative or 0 assigned ability, set to low positive value, 0.01


## Simulation
output <- foreach(i = 1:nrow(g1), .combine = rbind) %dopar% { # simulation
  educational_attainment_g1(wage_g1 = g1$hourwage_t[i], c = g1$c[i], south = g1$south[i], N = g1$CHBORN[i], 
                            beta = beta, beta_prime = beta_prime, C = C, s_male = s_male, s_female = s_female, 
                            g2_college_men_hours = g2_college_men_hours,  g2_college_women_hours =  g2_college_women_hours,
                            stipend = stipend)
}
g1$son_education <- output$educ_men
g1$daughter_education <- output$educ_women
g1$sons <- output$sons
g1$daughters <- output$daughters
g1$type <- output$type
g1$second_time <- output$second_time
min_a_not_qualified <- NA

if(stipend > 0){ # for type 2 households who with g2's who graduate high school, simulate educational attainment if stipend were made available 
  
  stipend_recipients <- which(g1$type == 2 & (g1$son_education == 12 | g1$daughter_education == 12))
  
  stipend_df <- g1 %>% slice(stipend_recipients)
  g1 <- g1 %>% slice(-stipend_recipients)
  stipend_df <- stipend_df %>% select(-c(son_education , daughter_education))
  
  output <- foreach(i = 1:nrow(stipend_df), .combine = rbind) %dopar% {
    educational_attainment_stipend(wage_g1 = stipend_df$hourwage_t[i], c = stipend_df$c[i], south = stipend_df$south[i], 
                                   sons = stipend_df$sons[i], daughters = stipend_df$daughters[i], 
                                   second_time = stipend_df$second_time[i], beta = beta, beta_prime = beta_prime, 
                                   C = C, s_male = s_male, s_female = s_female,  g2_college_men_hours = g2_college_men_hours,  
                                   g2_college_women_hours =  g2_college_women_hours, 
                                   stipend = stipend)
  }
  
  stipend_df$son_education <- output$educ_men
  stipend_df$daughter_education <- output$educ_women
  
  min_a_not_qualified <- min(stipend_df$c[which(stipend_df$son_education > 12)], stipend_df$c[which(stipend_df$daughter_education > 12)])
  
  g1 <- rbind(g1 , stipend_df)
}
stopImplicitCluster()



# Calculate ability cutoff for stipend such that supply = demand
if(stipend > 0){
    vec <- c(0, sort(g1$c[(g1$type == 2 & g1$sons > 0 & g1$son_education > 12) | (g1$type == 2 & g1$daughters > 0 & g1$daughter_education > 12)]))
    tuitions_supplied <- round(stipend_budget/(stipend*2000), 2)
    i <- 1
    male_sti <- g1[g1$son_education > 12 & g1$sons > 0 & g1$type == 2 & g1$c >= vec[1], c("sons", "c", "son_education")]
    female_sti <- g1[g1$daughter_education > 12 & g1$daughters > 0 & g1$type == 2 & g1$c >= vec[1], c("daughters", "c", "daughter_education")]
    male_sti$son_education <- male_sti$son_education - 12
    female_sti$daughter_education <- female_sti$daughter_education - 12
    tuitions_demanded <- round((sum((male_sti$son_education)*(1/male_sti$c)*male_sti$sons) + 
                                  sum((female_sti$daughter_education)*(1/female_sti$c)*female_sti$daughters))*6.67, 2)
    excess_demand <- tuitions_demanded  -tuitions_supplied
    if(excess_demand < 0){
      c_cutoff <- 0
    } else {
      while(excess_demand >= 0){
        i <- i + 1
        male_sti <- male_sti[male_sti$c >= vec[i],]
        female_sti <- female_sti[female_sti$c >= vec[i],]
        tuitions_demanded <- round((sum((male_sti$son_education)*(1/male_sti$c)*male_sti$sons) + 
                                      sum((female_sti$daughter_education)*(1/female_sti$c)*female_sti$daughters))*6.67, 2)
        excess_demand <- tuitions_demanded - tuitions_supplied
      }
      c_cutoff <- vec[i - 1]
    }
  
  eligible_sons_percent <-  sum(g1$sons[which(g1$son_education > 12 & g1$type == 2 & g1$c >= c_cutoff)])/
    sum(g1$sons[which(g1$son_education > 12 & g1$type == 2)])
  eligible_daughters_percent <-  sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2 & g1$c >= c_cutoff)])/
    sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)])
  
  
  g1$son_education[which(g1$c < c_cutoff & g1$son_education > 12 & g1$type == 2)] <- 12
  g1$daughter_education[which(g1$c < c_cutoff & g1$daughter_education > 12 & g1$type == 2)] <- 12
  
} else {
  c_cutoff <- 0
}

g1$son_education[which(g1$sons == 0)] <- NA
g1$daughter_education[which(g1$daughters == 0)] <- NA
write.csv(g1, paste("output/g1_simulation_S = ", stipend*2000, ".csv", sep="")) # output g1 men dataframe with simulated education of children


## Figure 3: G2 men's and women's simulated vs. actual educational attainment

# Plot of men's simulated education vs. actual
colors <- c("Actual"="red", "Simulated" = "blue")
ggplot(g1) + stat_density(aes(x = son_education, weight = sons, color = "Simulated"), geom="line", size = .75, adjust = 1.6) + 
  stat_density(data = g2, aes(x = educ, color = "Actual"), geom = "line", size = .75, adjust = 1) + 
  scale_x_continuous(breaks = seq(0, 16, 1)) + labs(x = "Educational Attainment", y = "") + 
  scale_color_manual("", values = colors) + ggtitle("Men") + theme_classic()
ggsave(paste("figures/g2 men's educational comparison_S = ", stipend*2000, ".png", sep = ""))


# Plot of women's simulated education vs. actual
women_g2 <- women %>% filter(yearbirth >= 1912 & yearbirth <= 1916)
ggplot(g1) + stat_density(aes(x = son_education, weight = sons, color = "Simulated"), geom = "line", size = .75, adjust = 1.6)  +
  stat_density(data = women_g2, aes(x = educ, color = "Actual"), geom = "line", size = .75, adjust = 1) +
  scale_x_continuous(breaks = seq(0, 16, 1)) + labs(x = "Educational Attainment", y = "") + 
  scale_color_manual("", values = colors) + ggtitle("Women") + theme_classic()
ggsave(paste("figures/g2 women's educational comparison_S = ", stipend*2000, ".png", sep = ""))


# Simulated vs. actual education percentages
sim1 <- count(x = g1, son_education, wt = sons)
sim1$n <- sim1$n/sum(g1$sons)
act <- as.data.frame(table(g2$educ)/nrow(g2))
sons_college_sim <- sum(sim1$n[which(sim1$son_education %in% c(13, 14, 15, 16))])
son_college_act <- sum(act$Freq[which(act$Var1 %in% c(13, 14, 15, 16))])

sim1_w <- count(x = g1, daughter_education, wt = daughters)
sim1_w$n <- sim1_w$n/sum(g1$daughters)
act_w <- as.data.frame(table(women_g2$educ)/nrow(women_g2))
daughters_college_sim <- sum(sim1_w$n[which(sim1_w$daughter_education %in% c(13, 14, 15, 16))])
daughters_college_act <- sum(act_w$Freq[which(act_w$Var1 %in% c(13, 14, 15, 16))])

ed_percentages <- data.frame(educ = 0:16, mens_act = NA, mens_sim = NA, womens_act = NA, womens_sim = NA)
for(i in 1:nrow(ed_percentages)){
  ed_percentages$mens_act[i] <- length(which(g2$educ == (i - 1)))/nrow(g2)
  ed_percentages$mens_sim[i] <- sum(g1$sons[which(g1$son_education == (i - 1))])/sum(g1$sons)
  ed_percentages$womens_act[i] <- length(which(women_g2$educ == (i - 1)))/nrow(women_g2)
  ed_percentages$womens_sim[i] <- sum(g1$daughters[which(g1$daughter_education == (i - 1))])/sum(g1$daughters)
}
ed_percentages$mens_diff <- ed_percentages$mens_act - ed_percentages$mens_sim
ed_percentages$womens_dff <- ed_percentages$womens_act - ed_percentages$womens_sim
write.csv(ed_percentages, paste("output/education percentage comparison_S = ", stipend*2000, ".csv", sep = ""))



# Calculate then print relevant outputs
percent_stipend_men <- sum(g1$sons[which(g1$son_education > 12 & g1$type == 2)])/sum(g1$sons[which(g1$son_education > 12)])

percent_stipend_women <- sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)])/sum(g1$daughters[which(g1$daughter_education > 12)])

percent_stipend_all <- (sum(g1$sons[which(g1$son_education > 12 & g1$type == 2)])+sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)]))/(sum(g1$sons[which(g1$son_education > 12)])+sum(g1$daughters[which(g1$daughter_education > 12)]))

percent_stipend_recipients_women <- sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)])/(sum(g1$sons[which(g1$son_education > 12 & g1$type == 2)])+sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)]))

male_sti <- g1 %>% filter(sons > 0) %>% filter(type == 2)
female_sti <- g1 %>% filter(daughters > 0) %>% filter(type == 2)
tuitions <- sum(pmax(male_sti$son_education - 12,0)*(1/male_sti$c)*male_sti$sons) + sum(pmax(female_sti$daughter_education-12,0)*(1/female_sti$c)*female_sti$daughters)
stipends <- tuitions*(stipend*2000)
stipends <- (stipends/(10^6))*6.67
tuitions <- tuitions*6.67

min_c_sti <- min(min(g1$c[which(g1$sons > 0 & g1$type == 2 & g1$son_education > 12)]), min(g1$c[which(g1$daughters > 0 & g1$type == 2 & g1$daughter_education > 12)]))
min_c_no_sti <- min(min(g1$c[which(g1$sons > 0 & g1$type == 1 & g1$son_education > 12)]), min(g1$c[which(g1$daughters > 0 & g1$type == 1 & g1$daughter_education > 12)])) 
mean_c_sti <- (sum(g1$c[which(g1$sons > 0 & g1$type == 2 & g1$son_education > 12)]*g1$sons[which(g1$sons > 0 & g1$type == 2 & g1$son_education > 12)]) + sum(g1$c[which(g1$daughters > 0 & g1$type == 2 & g1$daughter_education > 12)]*g1$daughters[which(g1$daughters > 0 & g1$type == 2 & g1$daughter_education > 12)]))/(sum(g1$sons[which(g1$son_education > 12 & g1$type == 2)]) + sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 2)]))

mean_c_no_sti <- (sum(g1$c[which(g1$sons > 0 & g1$type == 1 & g1$son_education > 12)]*g1$sons[which(g1$sons > 0 & g1$type == 1 & g1$son_education > 12)]) + sum(g1$c[which(g1$daughters > 0 & g1$type == 1 & g1$daughter_education > 12)]*g1$daughters[which(g1$daughters > 0 & g1$type == 1 & g1$daughter_education > 12)]))/(sum(g1$sons[which(g1$son_education > 12 & g1$type == 1)]) + sum(g1$daughters[which(g1$daughter_education > 12 & g1$type == 1)]))

total_g2 <- sum(g1$sons) + sum(g1$daughters)


cat(paste(" Quantity of stipends demanded = ", tuitions), "\n",
    paste("Quantity of stipends supplied = ", stipend_budget/(stipend*2000)), "\n",
    paste("Stipend demanded in millions of dollars = $", stipends), "\n",
    paste("Stipend supplied in millions of dollars = $", round(stipend_budget/(10^6), 2)), "\n",
    paste("Number of simulated G2s = ", total_g2), "\n",
    paste("Stipend ability cutoff = ", c_cutoff), "\n",
    paste("Minimum ability among stipend rejections = ", min_a_not_qualified), "\n",
    paste("Minimum ability - stipend recipients = ", min_c_sti), "\n", 
    paste("Minimum ability - non-stipend recipients = ", min_c_no_sti), "\n",
    paste("Mean ability - stipend recipients = ", mean_c_sti), "\n",
    paste("Mean ability - non stipend recipients = ", mean_c_no_sti), "\n",
    paste("Percentage of male college students receiving stipend = ", percent_stipend_men), "\n",
    paste("Percentage of female college students receiving stipend = ", percent_stipend_women), "\n", 
    paste("Percentage of all college students receiving stipend = ", percent_stipend_all), "\n",
    paste("Percentage of stipend recipients who are women = ", percent_stipend_recipients_women), "\n",
    paste("Simulated percent with some college - men = ", sons_college_sim), "\n",
    paste("Actual percent with some college - men = ", son_college_act), "\n",
    paste("Simulated percent with some college - women = ", daughters_college_sim), "\n", 
    paste("Actual percent with some college - women = ", daughters_college_act),
    file = paste("output/relevent statistics_S = ", stipend*2000, ".txt")
)
