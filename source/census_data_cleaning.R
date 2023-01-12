`%notin%` <- Negate(`%in%`)



## Read and clean data



census <- read.csv("data/1940_census_raw.csv") %>% select(-c(X, YEAR))
census$HIGRADE[which(census$HIGRADE > 19)] <- 19
census <- census %>% filter(AGE >= 18) # restrict to adults only


# Educational attainment
census$HIGRADE[which(census$HIGRADE == 0)] <- NA # recode missing HIGRADE as NA
census$HIGRADE[which(census$HIGRADE == 1 | census$HIGRADE == 2)] <- 3 # "none" and "nursery school" recode as kindergarten
census$HIGRADE <- census$HIGRADE - 3 # adjust codes so HIGRADE corresponds with number of grades completed (e.g. 12 = high school graudate)
census$exper <- census$AGE - (census$HIGRADE + 6)


# Calculate hourly wages and hours worked from annual wage income
census$INCWAGE[which(census$INCWAGE == 999999 | census$INCWAGE == 999998)] <- NA #recode missing as NA
census <- census %>% filter(is.na(INCWAGE) == FALSE) # remove entries with missing income
census$weekwage <- census$INCWAGE/census$WKSWORK1
census$hourwage <- census$weekwage/census$HRSWORK1
census$hoursworked <- census$WKSWORK1*census$HRSWORK1


# Create race-ethnicity dummies
census$HISPAN[which(census$HISPAN >= 2 & census$HISPAN <= 4)] <- 1

census$white <- 0
census$white[which(census$RACE == 1)] <- 1

census$black <- 0
census$black[which(census$RACE == 2)] <- 1

census$asian <- 0
census$asian[which(census$RACE >= 4 & census$RACE <= 6)] <- 1

census$nhwhite <- 0
census$nhwhite[which(census$white == 1 & census$HISPAN == 0)] <- 1

census$nhblack <- 0
census$nhblack[which(census$black == 1 & census$HISPAN == 0)] <- 1

census$nhasian <- 0
census$nhasian[which(census$asian == 1 & census$HISPAN == 0)] <- 1



# Create region dummies
census$south <- 0
census$south[which(census$REGION >= 31 & census$REGION <= 34)] <- 1

census$midwest <- 0
census$midwest[which(census$REGION >= 21 & census$REGION <= 23)] <- 1

census$west <- 0
census$west[which(census$REGION >= 41 & census$REGION <= 43)] <- 1

census$REGION[which(census$REGION > 90)] <- NA


# Create occupational dummies
census$professional_managers <- 0
census$professional_managers[which(census$OCC1950 %in% c(200,210,240,250,280,290,300))] <- 1

census$foremen <- 0
census$foremen[which(census$OCC1950 == 523)] <- 1

census$professional_sales <- 0
census$professional_sales[which(census$OCC1950 %in% c(400,450,490,470,480))] <- 1

census$professional_elites <- 0
census$professional_elites[which(census$OCC1950 %in% c(0,55,36,75,1,32))] <- 1

census$professional_technical <- 0
census$professional_technical[which(census$OCC1950 %in% c(2,46,43,49,44,42,7,3,45,47,68,81,83,41,73,99))] <- 1

census$academic_elites <- 0
census$academic_elites[which(census$OCC1950 %in% c(12,29,14,15,16,18,27))] <- 1


# For G1s we need all children ever born to the household. In order to do this, we have to take fertility information from women, link with husbands, 
# and assign number of children to the husband. For missing values, we impute number of children with a regression on wage, region, education, and experience
census$yearbirth <- 1939 - census$AGE

women <- census %>% filter(SEX == 2) %>% select(-SPLOC) %>% rename(SPLOC = PERNUM)
women$id <- paste(women$SERIAL, women$SPLOC) # used to merge husbands and wives


men <- census %>% filter(SEX == 1) %>% select(-CHBORN)
remove_men <- which(men$OCC1950 == 595) # Remove members of armed services 
remove_men <- c(remove_men, which(men$OCC1950 %in% c(390,690,350,583,997,683) & men$hourwage > 3 & men$hourwage < 6))# Remove certain occupations with hourly wages between 3 and 6, as these are expected to be errors
remove_women <- which(women$OCC1950 == 595) # Remove members of armed services 
remove_women <- c(remove_women, which(women$OCC1950 %in% c(390,690,350,583,997,683) & women$hourwage > 3 & women$hourwage < 6)) # Remove certain occupations with hourly wages between 3 and 6, as these are expected to be errors

men <- men %>% slice(-remove_men)
men$id <- paste(men$SERIAL, men$SPLOC)
g1_husband_wife <- men %>% filter(SPLOC != 0) %>% filter(yearbirth <= 1906) # select all G1 men with spouses in the dataset
women_match <- women %>% filter(id %in% g1_husband_wife$id) %>% select(c(CHBORN, id))
g1_husband_wife <- merge(g1_husband_wife, women_match)# merged dataframe has for every g1 man number of children born to linked wife
women <- women %>% slice(-remove_women)

child_reg <- lm(CHBORN ~ INCWAGE + REGION + HIGRADE + exper, data = g1_husband_wife %>% filter(CHBORN > 0))
men <- men %>% filter(id %notin% g1_husband_wife$id)
remove <- which(men$yearbirth <= 1906 & men$MARST == 6) # remove men G1 men who never married, as they did not make educational investment decisions
men <- men %>% slice(-remove)
men <- bind_rows(men, g1_husband_wife)
men$CHBORN[which(is.na(men$CHBORN))] <- round(predict(child_reg, men %>% filter(is.na(CHBORN))))#impute number of children when missing
men$CHBORN[which(men$CHBORN == 0)] <- round(predict(child_reg, men %>% filter(CHBORN == 0))) #impute number of children when missing

men$hourwage <- as.character(men$hourwage) #save as character to preserve precision
women$hourwage <- as.character(women$hourwage) # save as character to preserve precision


# the results in the paper were generated using datasets that included 1950 and 1960 Censuses and other variables.
# the way those data were cleaned (including various transformations extraneous to this paper)
# created rows of the 1940 entries were in a different order than the dataframes here here.
# In order for results to be completely reproducible, we re-order the dataframes here so that they produce identical numbers to the text of the paper.
men$unique_id <- paste(men$SERIAL, men$PERNUM)
women$unique_id <- paste(women$SERIAL, women$SPLOC)

order_men <- read.csv("data/cleaned_dataset_order_men.csv")
order_women <- read.csv("data/cleaned_dataset_order_women.csv")

men <- merge(men, order_men) %>% arrange(order)
women <- merge(women, order_women) %>% arrange(order)

write.csv(men, "data/1940_men_cleaned.csv",row.names = FALSE)
write.csv(women,"data/1940_women_cleaned.csv", row.names=FALSE)
rm("census", "men", "women")



