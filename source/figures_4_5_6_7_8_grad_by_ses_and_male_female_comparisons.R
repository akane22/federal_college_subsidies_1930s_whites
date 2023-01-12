
## Section calculates wage deciles in order to make figures 4-8. to get wage deciles, we use 1935 wages even though educational investment decision takes place in 1925
## to do this, take regression of 1940 men's wages and add 5 years experience to g1 transformed wages and adjust to 1935 prices
men <- read.csv("data/1940_men_cleaned.csv") %>% filter(nhwhite == 1) %>% rename(educ = HIGRADE)
men$hourwage <- as.numeric(men$hourwage)
men$hourwage[is.nan(men$hourwage)] <- NA


interact <- function(data, first, second){
  parse(text = paste0(data,'$', first, '_', second, ' <- ', data, '$', first, '*', data, '$', second))
}

eval(interact('men', 'educ', 'exper'))
eval(interact('men', 'exper', 'south'))
model <- as.formula("hourwage ~ educ + exper + educ_exper + exper_south + south + 
                    professional_managers + foremen + professional_sales + professional_elites + professional_technical + academic_elites")

zero <- men %>% filter(hourwage <= 0 | hoursworked == 0) # separate everyone making non positive wages or working 0 hours
men <- men %>% filter(hoursworked > 0) %>% filter(hourwage > 0) #filter so just men with positive wages and positive hours worked
lm_men <- lm(model, men) #regression
rm("men")

g1 <- read.csv(paste("output/g1_simulation_S = ", stipend, ".csv", sep = ""))
g1_no_sti <- read.csv(paste("output/g1_simulation_S = ", 0, ".csv", sep = ""))
g1_universal <- read.csv(paste("output/g1_simulation_S = ", universal_stipend, ".csv", sep = ""))


# For dataset when stipend = replication equilibrium stipend
g1$hourwage_t <- g1$hourwage_t*(2000/3100)*(13.9/17.5) + 14*lm_men$coefficients[["exper"]] + 
  (14)*g1$educ*lm_men$coefficients[["educ_exper"]] + 14*g1$south*lm_men$coefficients[["exper_south"]] # return wages to 1939
g1$hourwage_t <- (g1$hourwage_t - (4*lm_men$coefficients[["exper"]] + 
                                     (4)*g1$educ*lm_men$coefficients[["educ_exper"]] + 
                                     4*g1$south*lm_men$coefficients[["exper_south"]]))*(13.7/13.9) # adjust wages to 1935
 
# Calculate median father's wage for stipend receiving and non-stipend receiving college students
g1$son_education[which(is.na(g1$son_education))] <- 0
g1$daughter_education[which(is.na(g1$daughter_education))] <- 0
g1$weight <- g1$sons*pmax(sign(g1$son_education - 12), 0) + g1$daughters*pmax(sign(g1$daughter_education - 12), 0)
g1$son_education[which(g1$sons == 0)] <- NA
g1$daughter_education[which(g1$daughters == 0)] <- NA
median_income_sti <- weighted.median(g1$hourwage_t[which(g1$type == 2 & g1$weight > 0)], g1$weight[which(g1$type == 2 & g1$weight > 0)])
median_income_no_sti <- weighted.median(g1$hourwage_t[which(g1$type == 1 & g1$weight > 0)], g1$weight[which(g1$type == 1 & g1$weight > 0)])

cat(paste(" Median father's income of stipend recipients = ", median_income_sti*2000), "\n",
    paste("Median father's income of non-stipend college students = ", median_income_no_sti*2000), 
    file = "output/median_income_stipend_vs_non_stipend.txt"
)


# For dataset when stipend = 0
g1_no_sti$hourwage_t <- g1_no_sti$hourwage_t*(2000/3100)*(13.9/17.5) + 14*lm_men$coefficients[["exper"]] + 
  (14)*g1_no_sti$educ*lm_men$coefficients[["educ_exper"]] + 14*g1_no_sti$south*lm_men$coefficients[["exper_south"]] # return wages to 1939
g1_no_sti$hourwage_t <- (g1_no_sti$hourwage_t - (4*lm_men$coefficients[["exper"]] + 
                                                   (4)*g1_no_sti$educ*lm_men$coefficients[["educ_exper"]] + 
                                                   4*g1_no_sti$south*lm_men$coefficients[["exper_south"]]))*(13.7/13.9) # adjust wages to 1935

# For dataset with universal policy
g1_universal$hourwage_t <- g1_universal$hourwage_t*(2000/3100)*(13.9/17.5) + 14*lm_men$coefficients[["exper"]] + 
  (14)*g1_universal$educ*lm_men$coefficients[["educ_exper"]] + 14*g1_universal$south*lm_men$coefficients[["exper_south"]] # return wages to 1939
g1_universal$hourwage_t <- (g1_universal$hourwage_t - (4*lm_men$coefficients[["exper"]] + 
                                                   (4)*g1_universal$educ*lm_men$coefficients[["educ_exper"]] + 
                                                   4*g1_universal$south*lm_men$coefficients[["exper_south"]]))*(13.7/13.9) # adjust wages to 1935

## this section makes figures for college graduation of high ability students (above stipend cutoff) by father's wage decile
## under the rationed and universal policies
g1_new <- g1 %>% filter(c >= c_cutoff_rep_eq)
g1_no_sti_new <- g1_no_sti %>% filter(c >= c_cutoff_rep_eq) 
g1_universal_new <- g1_universal %>% filter(c >= c_cutoff_rep_eq)

# For a given quantile size and dataframe, calculate percent of quantile graduating college 
quantile_grad_percents <- function(size, df){ 
  probas <- seq(0, 1, by = size)
  quantiles <- quantile(g1$hourwage_t*g1$hoursworked, probs = probas)
  percents <- rep(NA, length(probas) - 1)
  
  for(i in 1:length(percents)){
    percents[i] <- (sum(df$sons[which(df$son_education == 16 & 
                                        df$hourwage_t >= quantiles[i] & 
                                        df$hourwage_t < quantiles[i + 1])]) + 
                      sum(df$daughters[which(df$daughter_education == 16 & 
                                               df$hourwage_t >= quantiles[i] & 
                                               df$hourwage_t < quantiles[i + 1])]))/
      (sum(df$sons[which(df$hourwage_t >= quantiles[i] & 
                           df$hourwage_t < quantiles[i + 1])]) + 
         sum(df$daughters[which(df$hourwage_t >= quantiles[i] & 
                                  df$hourwage_t < quantiles[i+1])]))
  }
  return(percents)
}
deciles <- quantile_grad_percents(0.1, g1_new) # college grad by decile for high ability students under rationed policy
deciles_no_sti <- quantile_grad_percents(0.1, g1_no_sti_new) # college grad by decile for high ability students when stipend = 0
deciles_universal <- quantile_grad_percents(0.1, g1_universal_new) # college grad by decile for high ability students under universal policy
deciles_no_sti_universal <- quantile_grad_percents(0.1, g1_no_sti) # college grad by decile for high ability students when stipend = 0

comparison <- data.frame(`Percent Graduate` = deciles,
                         `Percent Graduate without Stipend` = deciles_no_sti,
                         Difference = deciles - deciles_no_sti, 
                         `Percent Graduate: Universal` = deciles_universal,
                         `Percent Graduate without Stipend: Universal` = deciles_no_sti_universal,
                         `Difference: Universal` = deciles_universal - deciles_no_sti_universal)
write.csv(comparison, "output/numbers_for_figures_5_6.csv") # save csv file with graduation by decile under the two policies and when stipend equals zero along with differences


# figure 4: barplot of college graduation of high ability students by father's wage decile
png(file = "figures/figure_4_barplot_college_grad_high_ability_students_father_wage_decile.png",
    fig.width = fig_width, fig.height = fig_height)
barplot(deciles, main = "College Graduation of High Ability Students by Father's Wage Decile", 
        xlab = "Decile", ylab="", names.arg=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
        col = "springgreen3", density = 30, yaxt = "n", ylim = c(0, 0.7))
axis(side = 2, las = 2, at = pretty(deciles), lab = paste(pretty(deciles)*100, "%", sep = ""))
dev.off()



# figure 5: comparison of college grad of high ability students by father's wage decile to stipend = 0
png(file = "figures/figure_5_college_grad_high_ability_students_father_wage_decile_comparison.png", 
    fig.width = fig_width, fig.height = fig_height)
plot(1:10, deciles, main = "College Graduation of Students by Father's Wage Decile", xlab = "Decile", 
     ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 0.7), col = alpha("blue", 0.5), 
     pch = 19, cex = 1.5)
axis(side = 1, at = 1:10, lab = 1:10)
axis(side = 2, las = 2, at = pretty(deciles),lab = paste(pretty(deciles)*100, "%", sep = ""))
points(1:10, deciles_no_sti, col = alpha("red", 0.5), pch = 19, cex = 1.5)
legend("topleft", legend = c(paste("S = ", round(stipend, 2), sep = ""), "S = 0"), pch = c(19, 19), 
       col = c(alpha("blue", 0.5), alpha("red", 0.5)))
dev.off()


# figure 6: comparison of college grad of high ability students by father's wage decile to stipend = 0 under universal policy
png(file = "figures/figure_6_college_grad_high_ability_students_father_wage_decile_comparison.png", 
    fig.width = fig_width, fig.height = fig_height)
plot(1:10, deciles_universal, 
     main = "College Graduation of Students by Father's Wage Decile, Universal Policy", 
     xlab = "Decile", ylab = "", xaxt = "n", yaxt = "n", ylim=c(0, 0.4), 
     col = alpha("blue", 0.5), pch = 19, cex = 1.5)
axis(side = 1, at = 1:10, lab = 1:10)
axis(side = 2, las = 2, at = pretty(deciles_universal ), lab = paste(pretty(deciles)*100, "%", sep = ""))
points(1:10, deciles_no_sti_universal, col = alpha("red", 0.5), pch = 19, cex = 1.5)
legend("topleft", legend = c(paste("S = ", round(universal_stipend, 2), sep = ""), "S = 0"), pch = c(19, 19), 
       col = c(alpha("blue", 0.5), alpha("red", 0.5)))
dev.off()


## this section makes figures plotting male-Female differences in college attendance and graduation by father's wage decile under rationed policy

# For a given quantile size and dataframe, outputs percentage point difference in male and female college graduation for each quantile
quantile_grad_percents_difference <- function(size, df){
  probas <- seq(0, 1, by = size)
  quantiles <- quantile(g1$hourwage_t, probs = probas)
  percent_difference <- rep(NA, length(probas) - 1)
  
  for(i in 1:length(percent_difference)){
    percent_difference[i] <- (sum(df$sons[which(df$son_education == 16 & 
                                                  df$hourwage_t >= quantiles[i] & 
                                                  df$hourwage_t < quantiles[i + 1])])/
                                sum(df$sons[which(df$hourwage_t >= quantiles[i] & 
                                                    df$hourwage_t<quantiles[i + 1])])) - 
      (sum(df$daughters[which(df$daughter_education == 16 & 
                                df$hourwage_t >= quantiles[i] & 
                                df$hourwage_t < quantiles[i + 1])])/
         sum(df$daughters[which(df$hourwage_t >= quantiles[i] & 
                                  df$hourwage_t < quantiles[i + 1])]))
  }
  return(percent_difference)
}

# For a given quantile size and dataframe, outputs percentage point difference in male and female college attndance for each quantile
quantile_attend_percents_difference <- function(size, df){
  probas <- seq(0, 1, by =size)
  quantiles <- quantile(g1$hourwage_t, probs = probas)
  percent_difference <- rep(NA, length(probas) - 1)
  
  for(i in 1:length(percent_difference)){
    percent_difference[i] <- (sum(df$sons[which(df$son_education >= 13 & 
                                                  df$hourwage_t >= quantiles[i] & 
                                                  df$hourwage_t < quantiles[i + 1])])/
                                sum(df$sons[which(df$hourwage_t >= quantiles[i] & 
                                                    df$hourwage_t<quantiles[i + 1])])) - 
      (sum(df$daughters[which(df$son_education >= 13 & 
                                df$hourwage_t >= quantiles[i] & 
                                df$hourwage_t < quantiles[i + 1])])/
         sum(df$daughters[which(df$hourwage_t >= quantiles[i] & 
                                  df$hourwage_t < quantiles[i + 1])]))
  }
  return(percent_difference)
}
deciles_diff <- quantile_grad_percents_difference(0.1, g1_new) # difference in college attendance under rationed policy
deciles_no_sti_diff <- quantile_grad_percents_difference(0.1, g1_no_sti) # difference in college attendance with no stipend

decile_attend_diff <- quantile_attend_percents_difference(0.1, g1_new) # difference in college graduation under rationed policy
decile_attend_no_sti_diff <- quantile_attend_percents_difference(0.1, g1_no_sti) # difference in college gradution with no stipend


male_female_diff <- data.frame(`Attendance Difference No Stipend` = decile_attend_no_sti_diff,
                               `Attendance Difference Stipend` = decile_attend_diff, 
                               `Graduation Difference No Stipend` = deciles_no_sti_diff, 
                               `Graduation Difference Stipend` = deciles_diff)
write.csv(male_female_diff, "output/numbers_for_figures_7_8.csv") # save csv with gender differences in college gradution and attendence with and without stipend


# figure 7: male female difference in college attendance with replication equilibrium stipend and no stipend
png(file = "figures/figure_7_male_female_difference_college_attendance.png", 
    fig.width = fig_width, fig.height = fig_height)
plot(1:10, decile_attend_diff, main = "Male-Female Difference in College Attendance Rate",  
     xlab = "Decile", ylab = "", xaxt = "n", yaxt = "n", ylim = c(-0.3, 0.3), 
     col = alpha("blue", 0.5), pch = 19, cex = 1.5)
axis(side = 1, at = 1:10, lab = 1:10)
axis(side = 2,las = 2)
points(1:10, decile_attend_no_sti_diff, col = alpha("red", 0.5), pch = 19, cex = 1.5)
legend("topleft", legend = c(paste("S = ", round(stipend, 2), sep = ""), "S = 0"), pch = c(19, 19), 
       col = c(alpha("blue", 0.5), alpha("red", 0.5)))
dev.off()


# figure 8: male-female difference in college graduation with replkication equilibrium stipend and no stipend
png(file = "figures/figure_8_male_female_difference_college_graduation.png", 
    fig.width = fig_width, fig.height = fig_height)
plot(1:10, deciles_diff, main = "Male-Female Difference in College Graduation Rate", 
     xlab = "Decile", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 0.2), 
     col = alpha("blue", 0.5), pch = 19, cex = 1.5)
axis(side = 1, at = 1:10, lab = 1:10)
axis(side = 2, las = 2, at = seq(0, 0.2, by = 0.05))
points(1:10, deciles_no_sti_diff, col = alpha("red", 0.5), pch = 19, cex = 1.5)
legend("topleft", legend = c(paste("S = ", round(stipend, 2), sep = ""), "S = 0"), 
       pch = c(19, 19), col = c(alpha("blue", 0.5), alpha("red", 0.5)))
dev.off()


