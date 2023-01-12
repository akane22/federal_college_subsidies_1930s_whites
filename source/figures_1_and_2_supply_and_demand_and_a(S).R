
abar <- 113.278805

simulated_data <- read.csv("output/simulated_matrix_with college_percentages.csv") # simulation of demand for stipends and ability cutoff for n values of stipend

simulated_data$stipend_sq <- simulated_data$stipend^2
regression <- lm(a_cutoff_for_stipend ~ stipend + stipend_sq, 
                 data = simulated_data[simulated_data$a_cutoff_for_stipend > 0,]) # a(S) modeled as quadratic in stipend for cutoffs greater than \bar{a}

# summary(regress2)

# figure 1: supply and demand
vals <- seq(min(simulated_data$stipend[simulated_data$a_cutoff_for_stipend > 0]), max(simulated_data$stipend), 0.1) # values to calculate stipend supply

png(file = "figures/figure_1_supply_and_demand.png", width = fig_width, 
    height = fig_height)
plot(simulated_data$stipend, stipend_budget/simulated_data$stipend, pch = 20, 
     col = "blue", yaxt = "n", ylab = "", xlab = "S", 
     ylim = c(min(simulated_data$quantity_stipends_demanded), max(stipend_budget/simulated_data$stipend))) # stipend supply is budget/stipend amount
axis(side = 2, las = 2, at = c(100000, 150000, 200000, 250000,300000,350000),
     labels=formatC(c(100000, 150000, 200000, 250000, 300000, 350000), 
                    big.mark = ",", format = "d"))
points(simulated_data$stipend, simulated_data$quantity_stipends_demanded, col = "red", pch = 20) # plot of simulated stipend demand
legend("topright", legend = c(TeX(r'($N(S)$)'), TeX(r'($D(S,a(S))$)')), 
       pch = c(20, 20), col = c("blue", "red"))
abline(v = abar, lty = 2) #vertical line at \bar{a}
dev.off()


# figure 2: a(S) with fitted regression values
predicted <- predict(regression, list(stipend = vals, stipend_sq = vals^2)) # predicted a(S) using regression for grid of values

png(file = "figures/figure_2_a(S).png", width = fig_width, height = fig_height)
plot(simulated_data$stipend, simulated_data$a_cutoff_for_stipend, xlab = "S", 
     pch = 20, cex = 0.75, yaxt = "n", ylab = TeX(r'($a(S)$)'), 
     col = "springgreen3") # plot a(S)
axis(side = 2, las = 2)
lines(vals, predicted, lwd = 3, lty = 5) # plot regression fitted values
legend("topleft", legend = c('', ''), pch = c(20, NA), 
       col = c("springgreen3", "black"), text.width = 8)
legend("topleft", legend = c(TeX(r'($a(S)$)'), TeX(r'($\hat{a}(S)$)')), 
       lty = c(NA, 5), lwd = c(NA, 3), col = c("springgreen3", "black"), bty = 'n')
abline(v = abar, lty = 2) # vertical line at \bar{a}
dev.off()
