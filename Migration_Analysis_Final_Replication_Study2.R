######### Setup ######### 
remove(list=ls())
set.seed(8675309)
memory.limit(size=20000) 
options(xtable.comment = FALSE)

library(tidyverse)
library(xtable)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(car)
library(splines)
library(readxl)
library(stargazer)
library(psych)
library(lemon)
library(boot)
library(cjoint)
library(cregg)
library(margins)
library(FindIt)
library(ggpubr)

# Data files needed (place in local WD): 
# Study 2 (US): usclimate_exp1.csv
# Study 2 (Germany): Climate Migration 1_ Article- Germany_September 7, 2019_09.31.csv

########################### Study 2: US ########################### 
######### Import data ######### 
us_article <- read.csv(file = 'usclimate_exp1.csv', 
                       stringsAsFactors = T)

us_article <- us_article[3:nrow(us_article), ] #Remove header

######### Recoding ######### 
us_article$PARTISANSHIP6 <- NA

for(i in 1:nrow(us_article)){
  if(us_article[i, "PARTISANSHIP_D"]=="Strong Democrat"){
    us_article[i, "PARTISANSHIP6"]<- 6}
  if(us_article[i, "PARTISANSHIP_D"]=="Not very strong Democrat"){
    us_article[i, "PARTISANSHIP6"]<- 5}
  if(us_article[i, "PARTISANSHIP_I"]=="Closer to the Democratic Party"){
    us_article[i, "PARTISANSHIP6"]<- 4}
  if(us_article[i, "PARTISANSHIP_I"]=="Closer to the Republican Party"){
    us_article[i, "PARTISANSHIP6"]<- 3}
  if(us_article[i, "PARTISANSHIP_R"]=="Not very strong Republican"){
    us_article[i, "PARTISANSHIP6"]<- 2}
  if(us_article[i, "PARTISANSHIP_R"]=="Strong Republican"){
    us_article[i, "PARTISANSHIP6"]<- 1}
}  

us_article$FP_ORIENTATION_1 <- car::recode(us_article$FP_ORIENTATION_1,
                                           "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
us_article$FP_ORIENTATION_2 <- car::recode(us_article$FP_ORIENTATION_2,
                                           "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
#reverse coded 
us_article$FP_ORIENTATION_3 <- car::recode(us_article$FP_ORIENTATION_3,
                                           "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")

#reverse coded
us_article$FP_ORIENTATION_4 <-  car::recode(us_article$FP_ORIENTATION_4,
                                            "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")

us_article$SOC_DOM_1 <- car::recode(us_article$SOC_DOM_1,
                                    "'Definitely favor'=1; 'Somewhat favor'=2; 'Neither oppose nor favor'=3; 'Somewhat oppose'=4; 'Definitely oppose'=5")
#reverse coded
us_article$SOC_DOM_2 <- car::recode(us_article$SOC_DOM_2,
                                    "'Definitely favor'=5; 'Somewhat favor'=4; 'Neither oppose nor favor'=3; 'Somewhat oppose'=2; 'Definitely oppose'=1")

us_article$SOC_DOM_3 <- car::recode(us_article$SOC_DOM_3,
                                    "'Definitely favor'=1; 'Somewhat favor'=2; 'Neither oppose nor favor'=3; 'Somewhat oppose'=4; 'Definitely oppose'=5")

#reverse coded
us_article$SOC_DOM_4 <- car::recode(us_article$SOC_DOM_4,
                                    "'Definitely favor'=5; 'Somewhat favor'=4; 'Neither oppose nor favor'=3; 'Somewhat oppose'=2; 'Definitely oppose'=1")

us_article$EMPATHY_1 <- car::recode(us_article$EMPATHY_1,
                                    "'Describes me very well'=5; 'Describes me fairly well'=4; 'Describes me moderately well'=3; 'Describes me very little'=2; 'Does not describe me at all'=1")
#reverse coded
us_article$EMPATHY_2 <- car::recode(us_article$EMPATHY_2,
                                    "'Describes me very well'=1; 'Describes me fairly well'=2; 'Describes me moderately well'=3; 'Describes me very little'=4; 'Does not describe me at all'=5")
#reverse coded
us_article$EMPATHY_3 <- car::recode(us_article$EMPATHY_3,
                                    "'Describes me very well'=1; 'Describes me fairly well'=2; 'Describes me moderately well'=3; 'Describes me very little'=4; 'Does not describe me at all'=5")

us_article$EMPATHY_4 <- car::recode(us_article$EMPATHY_4,
                                    "'Describes me very well'=5; 'Describes me fairly well'=4; 'Describes me moderately well'=3; 'Describes me very little'=2; 'Does not describe me at all'=1")

#reverse coded
us_article$MIGRATION_1 <- car::recode(us_article$MIGRATION_1,
                                      "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")
us_article$MIGRATION_2 <- car::recode(us_article$MIGRATION_2,
                                      "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
us_article$MIGRATION_3 <- car::recode(us_article$MIGRATION_3,
                                      "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
us_article$MIGRATION_4 <- car::recode(us_article$MIGRATION_4,
                                      "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")

#reverse coded
us_article$MIGRATION_5 <- car::recode(us_article$MIGRATION_5,
                                      "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")
us_article$MIGRATION_6 <- car::recode(us_article$MIGRATION_6,
                                      "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")

#reverse coded
us_article$CLIMATE_MIG_1 <- car::recode(us_article$CLIMATE_MIG_1,
                                        "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")
us_article$CLIMATE_MIG_2 <- car::recode(us_article$CLIMATE_MIG_2,
                                        "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
us_article$CLIMATE_MIG_3 <- car::recode(us_article$CLIMATE_MIG_3,
                                        "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
us_article$CLIMATE_MIG_4 <- car::recode(us_article$CLIMATE_MIG_4,
                                        "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")

#reverse coded 
us_article$CLIMATE_MIG_5 <- car::recode(us_article$CLIMATE_MIG_5,
                                        "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")
us_article$CLIMATE_MIG_6 <- car::recode(us_article$CLIMATE_MIG_6,
                                        "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")

#reverse coded
us_article$CLIMATE_1 <- car::recode(us_article$CLIMATE_1,
                                    "'Definitely Agree'=1; 'Somewhat Agree'=2; 'Neither Agree Nor Disagree'=3; 'Somewhat Disagree'=4; 'Definitely Disagree'=5")
us_article$CLIMATE_2 <- car::recode(us_article$CLIMATE_2,
                                    "'Definitely Agree'=5; 'Somewhat Agree'=4; 'Neither Agree Nor Disagree'=3; 'Somewhat Disagree'=2; 'Definitely Disagree'=1")
us_article$CLIMATE_3 <- car::recode(us_article$CLIMATE_3,
                                    "'Definitely Agree'=5; 'Somewhat Agree'=4; 'Neither Agree Nor Disagree'=3; 'Somewhat Disagree'=2; 'Definitely Disagree'=1")
us_article$CLIMATE_4 <- car::recode(us_article$CLIMATE_4,
                                    "'Definitely Agree'=5; 'Somewhat Agree'=4; 'Neither Agree Nor Disagree'=3; 'Somewhat Disagree'=2; 'Definitely Disagree'=1")
#reverse coded
us_article$CLIMATE_5 <- car::recode(us_article$CLIMATE_5,
                                    "'Definitely Agree'=1; 'Somewhat Agree'=2; 'Neither Agree Nor Disagree'=3; 'Somewhat Disagree'=4; 'Definitely Disagree'=5")
us_article$CLIMATE_6 <- car::recode(us_article$CLIMATE_6,
                                    "'Definitely Agree'=5; 'Somewhat Agree'=4; 'Neither Agree Nor Disagree'=3; 'Somewhat Disagree'=2; 'Definitely Disagree'=1")

#Scale 1 is cc, 2 is cm, 3 is m
us_article$REL_IMPORT_SCALE_1 <- car::recode(us_article$REL_IMPORT_SCALE_1,
                                             "'Top priority'=5; 'Fairly high priority'=4; 'Medium level priority'=3; 'Slight priority'=2; 'Not a priority at all'=1")
us_article$REL_IMPORT_SCALE_2 <- car::recode(us_article$REL_IMPORT_SCALE_2,
                                             "'Top priority'=5; 'Fairly high priority'=4; 'Medium level priority'=3; 'Slight priority'=2; 'Not a priority at all'=1")
us_article$REL_IMPORT_SCALE_3 <- car::recode(us_article$REL_IMPORT_SCALE_3,
                                             "'Top priority'=5; 'Fairly high priority'=4; 'Medium level priority'=3; 'Slight priority'=2; 'Not a priority at all'=1")
us_article <- us_article %>% 
  mutate(PARTISANSHIP_bin = ifelse(PARTISANSHIP6>3, "D", "R"),
         AGE = as.numeric(Age)) %>% 
  dplyr::rename(MIG_LEVELS = Q76_1,
                ANTHRO_CC = Q77_1,
                REL_IMPORT_SCALE_CLIMATE = REL_IMPORT_SCALE_1,
                REL_IMPORT_SCALE_MIGRATION = REL_IMPORT_SCALE_3,
                REL_IMPORT_SCALE_CLIMATEMIGRATION = REL_IMPORT_SCALE_2)

us_article$MIG_LEVELS <- car::recode(us_article$MIG_LEVELS,
                                     "'Increased a Lot'=5; 'Increased a Little'=4; 'Stay the Same'=3; 'Decreased a Little'=2; 'Decreased a Lot'=1")

us_article$PARTISANSHIP_bin <- as.factor(us_article$PARTISANSHIP_bin)
us_article$PARTISANSHIP_num <- as.numeric(us_article$PARTISANSHIP_bin)
us_article$GENDER_num <- ifelse(us_article$GENDER == "Female", 1, 0)
us_article$EDUCATION_num <- as.numeric(car::recode(us_article$EDUCATION,
                                                   "'Post-graduate degree'=6; 'College graduate'=5; 'Some college/AssociateÃ¢â¬Å¡ÃâÃÂ´s degree'=4; 
                                                   'Trade or vocational certification'=3; 'High school graduate/GED'=2; 'Elementary or some high school'=1"))-1
us_article$IDEOLOGY_num <- as.numeric(car::recode(us_article$IDEOLOGY,
                                                  "'Extremely liberal'=7; 'Liberal'=6; 'Slightly liberal'=5; 
                                                  'Moderate, middle of the road'=4; 
                                                  'Slightly conservative'=3; 'Conservative'=2; 'Extremely conservative'=1"))-1
us_article$RELIGIOSITY_num <- as.numeric(car::recode(us_article$RELIGIOSITY,
                                                     "'More than once a week'=6; 'Once a week'=5; 'A few times a month'=4; 
                                                     'A few times a year'=3; 'Once a year or less'=2; 'Never'=1"))-1
us_article$NATIVE_BORN_num <- ifelse(us_article$NATIVE_BORN == "United States", 1, 0)
us_article$EMPLOYMENT_num <- as.numeric(car::recode(us_article$EMPLOYMENT,
                                                    "'Employed full time'=7; 'Employed part time'=6; 'Self-employed'=5; 
                                                    'Student'=4; 
                                                    'Homemaker'=3; 'Retired'=2; 'Unemployed '=1"))-1
us_article$TRUST_GOVT_num <- as.numeric(car::recode(us_article$TRUST_GOVT,
                                                    "'Most of the time'=3; 'Only some of the time'=2; 'Just about always'=1"))-1
us_article$POL_INTEREST_num <- as.numeric(car::recode(us_article$POL_INTEREST,
                                                      "'Most of the time'=4; 
                                                      'Some of the time'=3; 'Only now and then'=2; 'Hardly at all'=1"))-1

us_article$border_state_indicator <- 1*(us_article$state_region %in% c("TX", "CA", "AZ", "NM"))


us_article$border_state_indicator_noCA <- 1*(us_article$state_region %in% c("TX", "AZ", "NM"))

us_article$urban_indicator <- 1*(us_article$city %in% c("New York", "Los Angeles", "Chicago", 
                                                        "Houston", "Phoenix", "Philadelphia",
                                                        "San Antonio", "San Diego", "Dallas", "San Jose")) 

######### Construct scales ######### 
climate <- data.frame(us_article[,c("CLIMATE_1", "CLIMATE_2", "CLIMATE_3", "CLIMATE_4", "CLIMATE_5", "CLIMATE_6")])
migration <- data.frame(us_article[,c("MIGRATION_1", "MIGRATION_2", "MIGRATION_3")]) # CHANGED TO THE FIRST THREE ITEMS
climate_migration <- data.frame(us_article[,c("CLIMATE_MIG_1", "CLIMATE_MIG_2", "CLIMATE_MIG_3", "CLIMATE_MIG_4", "CLIMATE_MIG_5", "CLIMATE_MIG_6")])
fp_orientation <- data.frame(us_article[,c("FP_ORIENTATION_1", "FP_ORIENTATION_2", "FP_ORIENTATION_3", "FP_ORIENTATION_4")])
soc_dom <- data.frame(us_article[,c("SOC_DOM_1", "SOC_DOM_2", "SOC_DOM_3", "SOC_DOM_4")])
empathy <- data.frame(us_article[,c("EMPATHY_1", "EMPATHY_2", "EMPATHY_3", "EMPATHY_4")])

climate <- data.frame(sapply(climate, FUN= function(x) as.numeric(x))-1)
migration <- data.frame(sapply(migration, FUN= function(x) as.numeric(x))-1)
climate_migration <- data.frame(sapply(climate_migration, FUN= function(x) as.numeric(x))-1)
fp_orientation <- data.frame(sapply(fp_orientation, FUN= function(x) as.numeric(x))-1)
soc_dom <- data.frame(sapply(soc_dom, FUN= function(x) as.numeric(x))-1)
empathy <- data.frame(sapply(empathy, FUN= function(x) as.numeric(x))-1)

#calculate chronbach's alpha for each index
psych::alpha(climate) 
psych::alpha(migration) 
psych::alpha(climate_migration) 
psych::alpha(fp_orientation)  
psych::alpha(soc_dom) 
psych::alpha(empathy) 

#Create the index variable as the mean score on the individual items
us_article$climate_index <- apply(climate, MARGIN = 1, FUN = mean)
us_article$migration_index <- apply(migration, MARGIN = 1, FUN = mean)
us_article$climate_migration_index <- apply(climate_migration, MARGIN = 1, FUN = mean)
us_article$fp_orientation_index <- apply(fp_orientation, MARGIN = 1, FUN = mean)
us_article$soc_dom_index <- apply(soc_dom, MARGIN = 1, FUN = mean)
us_article$empathy_index <- apply(empathy, MARGIN = 1, FUN = mean)

######### Balance, summary stats ######### 
table(us_article$TREATMENT) #Distribution of treatment to check RA

vars <- c('AGE', 'fp_orientation_index', 'soc_dom_index', "empathy_index", 
          'PARTISANSHIP_num', "GENDER_num", "EDUCATION_num",
          "IDEOLOGY_num", "RELIGIOSITY_num", "NATIVE_BORN_num",
          "EMPLOYMENT_num", "TRUST_GOVT_num", "POL_INTEREST_num")

var_labels <- c("Age", "Foreign Policy Orientation", "Social Dominance", "Empathy",
                "Partisanship", "Gender", "Education", "Ideology", "Religiosity", "Native Born",
                "Employment", "Trust in Government", "Political Interest")

treats <- c("US Migration", "World Migration", "US Climate", "World Climate", "US Climate Migration", "World Climate Migration")
treat_conditions <- c(1:6)

balmat <- data.frame(matrix(NA,length(vars)*length(treats), 6))
colnames(balmat) <- c("Var.",  "Treatment ID", "Treatment", "T-Test P val.", 
                      "Ctrl. Mean", "Treatment Mean") 

j <- c()
for(i in 1:length(vars)){
  a <- rep(var_labels[i], 6)
  j <- append(x = j, values = a)}

balmat[, 1] <- j
balmat[, 2] <- rep((1:6), 13)
balmat[, 3] <- rep(treats, 13)

counter <- 0
for (i in 1:length(vars)){
  for (j in 1:length(treat_conditions)){
    string <- paste('t_test <- t.test(us_article$', vars[i], '[us_article$TREATMENT==', treat_conditions[j], '], us_article$', 
                    vars[i], '[us_article$TREATMENT=="Control"])', 
                    sep = "", collapse = "")
    eval(parse(text=string))
    balmat[(counter+j),4] <- round(t_test$p.value, digits = 3)
    balmat[(counter+j),5] <- round(t_test$estimate[2], digits = 3)
    balmat[(counter+j),6] <- round(t_test$estimate[1], digits = 3)
    
  }
  counter <- counter+6
}

balmat[balmat$t_pval<.1, ] 

xtable(balmat, 
       font.size = "tiny", caption = "Experiment 1 Balance Tests, US Sample")

vars <- c('AGE', 'fp_orientation_index', 'soc_dom_index', "empathy_index", 
          'PARTISANSHIP6', "GENDER_num", "EDUCATION_num",
          "IDEOLOGY_num", "RELIGIOSITY_num", "NATIVE_BORN_num",
          "EMPLOYMENT_num", "TRUST_GOVT_num", "POL_INTEREST_num")

sum_stats <- data.frame(matrix(NA,length(vars), 7))
colnames(sum_stats) <- c("Var.", "Min.", "1st Qu.",  "Median",
                         "Mean", "3rd Qu.",    "Max." )
sum_stats[, 1] <- var_labels

for (i in 1:length(vars)){
  string <- paste('sum <- summary(us_article$',vars[i], ')',
                  sep = "", collapse = "")
  eval(parse(text=string))
  sum_stats[i, 2:7] <- sum
  
}

xtable(sum_stats, caption = "Experiment 1 Summary Statistics, US Sample", digits = c(0, 0, 0, 2, 2, 2, 2, 0))

######### T-tests and bootstraps ######### 
us_article$TREATMENT <- factor(us_article$TREATMENT)

outcomes <- c("climate_index", "migration_index", "climate_migration_index")
tmat <- data.frame(matrix(NA,length(outcomes)*length(treats), 11))
colnames(tmat) <- c("treat_condition", "treat_id", "outcome", "t_pval", 
                    "control_mean", "treat_mean", "mean_diff", "ci_low", "ci_high",
                    "Location", "Prime")  


k <- c()
for(i in 1:length(outcomes)){
  a <- rep(outcomes[i], 6)
  k <- append(x = k, values = a)}

tmat[, 3] <- k
tmat[, 1] <- rep((1:6), 3)
tmat[, 2] <- rep(treats, 3)
tmat[, 10] <- rep(c("US", "World"), 9)
tmat[, 11] <- rep(c("Migration","Migration", "Climate",  "Climate","Climate Migration","Climate Migration"), 3)

counter <- 0
for (i in 1:length(outcomes)){
  for (j in 1:length(treat_conditions)){
    string <- paste('t_test <- t.test(us_article$', outcomes[i], '[us_article$TREATMENT==', treat_conditions[j], '], us_article$', 
                    outcomes[i], '[us_article$TREATMENT=="Control"])', 
                    sep = "", collapse = "")
    eval(parse(text=string))
    tmat[(counter+j),4] <- round(t_test$p.value, digits = 3)
    tmat[(counter+j),5] <- round(t_test$estimate[2], digits = 3)
    tmat[(counter+j),6] <- round(t_test$estimate[1], digits = 3)
    tmat[(counter+j),7] <- tmat[(counter+j),6]-tmat[(counter+j),5]
    tmat[(counter+j),8] <- round(t_test$conf.int[1], digits = 3)
    tmat[(counter+j),9] <- round(t_test$conf.int[2], digits = 3)
  }
  counter <- counter+6
}

tmat[tmat$t_pval<.1, ]

t_test <- t.test(us_article$climate_migration_index[us_article$TREATMENT==6], us_article$climate_migration_index[us_article$TREATMENT=="Control"], )

#bootstrap results 
bsmat <- data.frame(matrix(NA,length(outcomes)*length(treats), 9))
colnames(bsmat) <- c("treat_condition", "treat_id", "outcome", "bs_mean_diff", 
                     "bs_se", "bs_ci_low", "bs_ci_high",
                     "Location", "Prime")  

k <- c()
for(i in 1:length(outcomes)){
  a <- rep(outcomes[i], 6)
  k <- append(x = k, values = a)}

bsmat[, 3] <- k
bsmat[, 1] <- rep((1:6), 3)
bsmat[, 2] <- rep(treats, 3)
bsmat[, 8] <- rep(c("US", "World"), 9)
bsmat[, 9] <- rep(c("Migration","Migration", "Climate",  "Climate","Climate Migration","Climate Migration"), 3)

boot_cm_diff <- function(d, i, condition){
  d2 <- d[i, ]
  diff <- (mean(d2$climate_migration_index[d2$TREATMENT==condition])-
             mean(d2$climate_migration_index[d2$TREATMENT=="Control"]))
  return <- diff
}

boot_m_diff <- function(d, i, condition){
  d2 <- d[i, ]
  diff <- (mean(d2$migration_index[d2$TREATMENT==condition])-
             mean(d2$migration_index[d2$TREATMENT=="Control"]))
  return <- diff
}

boot_cc_diff <- function(d, i, condition){
  d2 <- d[i, ]
  diff <- (mean(d2$climate_index[d2$TREATMENT==condition])-
             mean(d2$climate_index[d2$TREATMENT=="Control"]))
  return <- diff
}

counter <- 0
for (i in 1:length(outcomes)){
  for(j in 1:length(treat_conditions)){
    boot_cc <- boot(data = us_article, statistic = boot_cc_diff, R=1000, 
                    condition=treat_conditions[j])
    bsmat[(counter+j), 4] <- mean(boot_cc$t)
    bsmat[(counter+j), 5] <- sd(boot_cc$t)
    bsmat[(counter+j), 6] <- quantile(boot_cc$t, c(0.025, 0.975))[1]
    bsmat[(counter+j), 7] <- quantile(boot_cc$t, c(0.025, 0.975))[2]
    
    boot_m <- boot(data = us_article, statistic = boot_m_diff, R=1000,
                   condition=treat_conditions[j])
    bsmat[(counter+j), 4] <- mean(boot_m$t)
    bsmat[(counter+j), 5] <- sd(boot_m$t)
    bsmat[(counter+j), 6] <- quantile(boot_m$t, c(0.025, 0.975))[1]
    bsmat[(counter+j), 7] <- quantile(boot_m$t, c(0.025, 0.975))[2]
    
    boot_cm <- boot(data = us_article, statistic = boot_cm_diff, R=1000,
                    condition=treat_conditions[j])
    bsmat[(counter+j), 4] <- mean(boot_cm$t)
    bsmat[(counter+j), 5] <- sd(boot_cm$t)
    bsmat[(counter+j), 6] <- quantile(boot_cm$t, c(0.025, 0.975))[1]
    bsmat[(counter+j), 7] <- quantile(boot_cm$t, c(0.025, 0.975))[2]
    
  }
  counter <- counter+6
}

#Migration levels outcome
outcomes2 <- c("MIG_LEVELS")
tmat2 <- data.frame(matrix(NA,length(outcomes2)*length(treats), 11))
colnames(tmat2) <- c("treat_condition", "treat_id", "outcome", "t_pval", 
                     "control_mean", "treat_mean", "mean_diff", "ci_low", "ci_high",
                     "Location", "Prime")  

tmat2[, 3] <- rep(outcomes2, 6)
tmat2[, 1] <- 1:6
tmat2[, 2] <- treats
tmat2[, 10] <- rep(c("US", "World"), 3)
tmat2[, 11] <- rep(c("Migration","Migration", "Climate",  "Climate","Climate Migration","Climate Migration"), 1)

us_article$MIG_LEVELS <- as.numeric(us_article$MIG_LEVELS)

counter <- 0
for (j in 1:length(treat_conditions)){
  string <- paste('t_test <- t.test(us_article$MIG_LEVELS[us_article$TREATMENT==', treat_conditions[j], '], us_article$MIG_LEVELS', '[us_article$TREATMENT=="Control"])', 
                  sep = "", collapse = "")
  eval(parse(text=string))
  tmat2[(counter+j),4] <- round(t_test$p.value, digits = 3)
  tmat2[(counter+j),5] <- round(t_test$estimate[2], digits = 3)
  tmat2[(counter+j),6] <- round(t_test$estimate[1], digits = 3)
  tmat2[(counter+j),7] <- tmat[(counter+j),6]-tmat[(counter+j),5]
  tmat2[(counter+j),8] <- round(t_test$conf.int[1], digits = 3)
  tmat2[(counter+j),9] <- round(t_test$conf.int[2], digits = 3)
 
}

######### Regression models ######### 
#Unweighted
lm_climate <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+AGE+
                   fp_orientation_index+soc_dom_index+empathy_index+
                   NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                   RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                   border_state_indicator +urban_indicator,data=us_article)
lm_climate_migration <- lm(climate_migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                             fp_orientation_index+soc_dom_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                             border_state_indicator+urban_indicator,data=us_article)
lm_migration <- lm(migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                     fp_orientation_index+soc_dom_index+empathy_index+
                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                     border_state_indicator+urban_indicator, data=us_article)

stargazer(lm_climate, lm_migration, lm_climate_migration,
          header=FALSE, 
          title = "Issue Importance: US Sample, Unweighted",
          dep.var.caption = "",
          font.size = "tiny",
          model.numbers = F,
          dep.var.labels.include = F,
          model.names = F,
          column.labels = c("Climate", "Migration", "Climate Migration"),
          digits=2, 
          no.space=T,
          column.sep.width= "0pt", 
          omit.stat = c("f", "ser", "rsq"),
          covariate.labels = c("Treat: US Migration", "Treat: Word Migration", "Treat: US Climate",
                               "Treat: World Climate", "Treat: US Climate Migration", 
                               "Treat: World Climate Migration",
                               "Partisanship",
                               "Age",
                               "Foreign Policy Orientation",
                               "Social Dominance",
                               "Empathy",
                               "Native Born",
                               "Gender",
                               "Education",
                               "Ideology",
                               "Religiosity",
                               "Trust in Government",
                               "Political Interest",
                               "Employment Status",
                               "Border State",
                               "Urban"))

########################### Study 2: Germany ########################### 
######### Import data ######### 
ger_article <- read.csv(file = 'Climate Migration 1_ Article- Germany_September 7, 2019_09.31.csv', 
                        stringsAsFactors = T)
ger_article <- ger_article[3:nrow(ger_article), ]

######### Recoding ######### 
ger_article$FP_ORIENTATION_1 <- car::recode(ger_article$FP_ORIENTATION_1,
                                            "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
ger_article$FP_ORIENTATION_2 <- car::recode(ger_article$FP_ORIENTATION_2,
                                            "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")

#reverse coded 
ger_article$FP_ORIENTATION_3 <- car::recode(ger_article$FP_ORIENTATION_3,
                                            "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")

ger_article$EMPATHY_1 <- car::recode(ger_article$EMPATHY_1,
                                     "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")

#reverse coded
ger_article$EMPATHY_2 <- car::recode(ger_article$EMPATHY_2,
                                     "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")

#reverse coded
ger_article$EMPATHY_3 <- car::recode(ger_article$EMPATHY_3,
                                     "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")

ger_article$EMPATHY_4 <- car::recode(ger_article$EMPATHY_4,
                                     "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")

#reverse coded
ger_article$MIGRATION_1 <- car::recode(ger_article$MIGRATION_1,
                                       "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")

ger_article$MIGRATION_2 <- car::recode(ger_article$MIGRATION_2,
                                       "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
ger_article$MIGRATION_3 <- car::recode(ger_article$MIGRATION_3,
                                       "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
ger_article$MIGRATION_4 <- car::recode(ger_article$MIGRATION_4,
                                       "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
#reverse coded
ger_article$MIGRATION_5 <- car::recode(ger_article$MIGRATION_5,
                                       "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")
ger_article$MIGRATION_6 <- car::recode(ger_article$MIGRATION_6,
                                       "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
#reverse coded
ger_article$CLIMATE_MIG_1 <- car::recode(ger_article$CLIMATE_MIG_1,
                                         "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")
ger_article$CLIMATE_MIG_2 <- car::recode(ger_article$CLIMATE_MIG_2,
                                         "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
ger_article$CLIMATE_MIG_3 <- car::recode(ger_article$CLIMATE_MIG_3,
                                         "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
ger_article$CLIMATE_MIG_4 <- car::recode(ger_article$CLIMATE_MIG_4,
                                         "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")

#reverse coded 
ger_article$CLIMATE_MIG_5 <- car::recode(ger_article$CLIMATE_MIG_5,
                                         "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")
ger_article$CLIMATE_MIG_6 <- car::recode(ger_article$CLIMATE_MIG_6,
                                         "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")

#reverse coded
ger_article$CLIMATE_1 <- car::recode(ger_article$CLIMATE_1,
                                     "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")
ger_article$CLIMATE_2 <- car::recode(ger_article$CLIMATE_2,
                                     "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
ger_article$CLIMATE_3 <- car::recode(ger_article$CLIMATE_3,
                                     "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
ger_article$CLIMATE_4 <- car::recode(ger_article$CLIMATE_4,
                                     "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
#reverse coded
ger_article$CLIMATE_5 <- car::recode(ger_article$CLIMATE_5,
                                     "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")
ger_article$CLIMATE_6 <- car::recode(ger_article$CLIMATE_6,
                                     "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")

ger_article$REL_IMPORT_SCALE_1 <- car::recode(ger_article$REL_IMPORT_SCALE_1,
                                              "'HÃ¶chste PrioritÃ¤t'=5; 'Ziemlich hohe PrioritÃ¤t'=4; 'Mittlere PrioritÃ¤t'=3; 'Geringe PrioritÃ¤t'=2; 'Gar keine PrioritÃ¤t'=1")
ger_article$REL_IMPORT_SCALE_2 <- car::recode(ger_article$REL_IMPORT_SCALE_2,
                                              "'HÃ¶chste PrioritÃ¤t'=5; 'Ziemlich hohe PrioritÃ¤t'=4; 'Mittlere PrioritÃ¤t'=3; 'Geringe PrioritÃ¤t'=2; 'Gar keine PrioritÃ¤t'=1")
ger_article$REL_IMPORT_SCALE_3 <- car::recode(ger_article$REL_IMPORT_SCALE_3,
                                              "'HÃ¶chste PrioritÃ¤t'=5; 'Ziemlich hohe PrioritÃ¤t'=4; 'Mittlere PrioritÃ¤t'=3; 'Geringe PrioritÃ¤t'=2; 'Gar keine PrioritÃ¤t'=1")
ger_article <- ger_article %>% 
  mutate(AGE = as.numeric(Age)) %>% 
  dplyr::rename(MIG_LEVELS = Q76_1,
                ANTHRO_CC = Q77_1,
                REL_IMPORT_SCALE_CLIMATE = REL_IMPORT_SCALE_1,
                REL_IMPORT_SCALE_MIGRATION = REL_IMPORT_SCALE_3,
                REL_IMPORT_SCALE_CLIMATEMIGRATION = REL_IMPORT_SCALE_2)

ger_article$MIG_LEVELS <- car::recode(ger_article$MIG_LEVELS,
                                      "'Stark zunehmen'=5; 'Etwas zunehmen'=4; 'Gleich bleiben'=3; 'Etwas abnehmen'=2; 'Stark abnehmen'=1")

ger_article$GENDER_num <- ifelse(ger_article$GENDER == "Weiblich", 1, 0)
ger_article$EDUCATION_num <- as.numeric(car::recode(ger_article$EDUCATION,
                                                    "'Abgeschlossenes Hochschulstudium'=6; 'Angefangenes Hochschulstudium'=5; 'Abitur'=4; 
                                                    'Facabitur'=3; 'Realschulabschluss'=2; 'Haptschulabschluss'=1"))-1
ger_article$IDEOLOGY_num <- as.numeric(car::recode(ger_article$IDEOLOGY,
                                                   "'Extrem liberal'=7; 'Liberal'=6; 'Etwas liberal'=5; 
                                                   'Moderat, die gemÃÂ¤ÃÅ¸igte Mitte'=4; 
                                                   'Etwas konservativ'=3; 'Konservativ'=2; 'Extrem konservativ'=1"))-1
ger_article$RELIGIOSITY_num <- as.numeric(car::recode(ger_article$RELIGIOSITY,
                                                      "'Mehr als einmal die Woche'=6; 'WÃÂ¶chentlich '=5; 'Ein paar Mal im Monat'=4; 
                                                      'Ein paar Mal im Jahr'=3; 'Einmal im Jahr oder weniger'=2; 'Nie'=1"))-1
ger_article$NATIVE_BORN_num <- ifelse(ger_article$NATIVE_BORN == "Deutschland", 1, 0)
ger_article$EMPLOYMENT_num <- as.numeric(car::recode(ger_article$EMPLOYMENT,
                                                     "'Angestellt in Vollzeit'=7; 'Angestellt in Teilzeit'=6; 'SelbststÃÂ¤ndig'=5; 
                                                     'Student'=4; 
                                                     'Hausfrau'=3; 'Im Ruhestand'=2; 'Arbeitslos'=1"))-1
ger_article$TRUST_GOVT_num <-as.numeric(car::recode(ger_article$TRUST_GOVT,
                                                    "'Fast immer'=3; 'Meistens'=2; 'Nur manchmal'=1"))-1
ger_article$POL_INTEREST_num <- as.numeric(car::recode(ger_article$POL_INTEREST,
                                                       "'Mesitens'=4; 
                                                       'Manchmal'=3; 'Nur ab und zu'=2; 'Kaum'=1"))-1
ger_states <- read.csv(file = 'germany_state_key.csv') 

names(ger_states) <- c("qualtrics_code", "state_name", "region",
                       "east_indicator", "east_indicator2",
                       "east_indicator3", "east_indicator4",
                       "east_indicator5", "east_indicator6", 
                       "east_indicator7")

ger_article <- ger_article %>% 
  mutate(state_num = as.numeric(paste(state_region))) %>% 
  left_join(ger_states, by=c("state_num"= "qualtrics_code"))

ger_article$urban_indicator <- 1*(ger_article$city %in% c("Berlin", "Hamburg", "Munich",
                                                          "Cologne", "Frankfurt Am Main", "Stuttgart",
                                                          "Dusseldorf", "Dortmund", "Essen", "Leipzig")) 

######### Construct scales ######### 
climate_ger <- data.frame(ger_article[,c("CLIMATE_1", "CLIMATE_2", "CLIMATE_3", "CLIMATE_4", "CLIMATE_5", "CLIMATE_6")])
migration_ger <- data.frame(ger_article[,c("MIGRATION_1", "MIGRATION_2", "MIGRATION_3")]) #Reduced items
climate_migration_ger <- data.frame(ger_article[,c("CLIMATE_MIG_1", "CLIMATE_MIG_2", "CLIMATE_MIG_3", "CLIMATE_MIG_4", "CLIMATE_MIG_5", "CLIMATE_MIG_6")])
fp_orientation_ger <- data.frame(ger_article[,c("FP_ORIENTATION_1", "FP_ORIENTATION_2", "FP_ORIENTATION_3")])
empathy_ger <- data.frame(ger_article[,c("EMPATHY_1", "EMPATHY_2", "EMPATHY_3", "EMPATHY_4")])

climate_ger <- data.frame(sapply(climate_ger, FUN= function(x) as.numeric(x)))
migration_ger <- data.frame(sapply(migration_ger, FUN= function(x) as.numeric(x))-1)
climate_migration_ger <- data.frame(sapply(climate_migration_ger, FUN= function(x) as.numeric(x))-1)
fp_orientation_ger <- data.frame(sapply(fp_orientation_ger, FUN= function(x) as.numeric(x))-1)
empathy_ger <- data.frame(sapply(empathy_ger, FUN= function(x) as.numeric(x))-1)

#calculate chronbach's alpha for each index
psych::alpha(climate_ger) 
psych::alpha(migration_ger) 
psych::alpha(climate_migration_ger) 
psych::alpha(fp_orientation_ger) 
psych::alpha(empathy_ger) 

#r=create the index variable as the mean score on the individual items
ger_article$climate_index <- apply(climate_ger, MARGIN = 1, FUN = mean)
ger_article$migration_index <- apply(migration_ger, MARGIN = 1, FUN = mean)
ger_article$climate_migration_index <- apply(climate_migration_ger, MARGIN = 1, FUN = mean)
ger_article$fp_orientation_index <- apply(fp_orientation_ger, MARGIN = 1, FUN = mean)
ger_article$empathy_index <- apply(empathy_ger, MARGIN = 1, FUN = mean)

######### Balance, summary stats ######### 
table(ger_article$TREATMENT)

vars <- c('AGE', 'fp_orientation_index',  "empathy_index", 
          "GENDER_num", "EDUCATION_num",
          "IDEOLOGY_num", "RELIGIOSITY_num", "NATIVE_BORN_num",
          "EMPLOYMENT_num", "TRUST_GOVT_num", "POL_INTEREST_num")

var_labels <- c("Age", "Foreign Policy Orientation",  "Empathy",
                "Gender", "Education", 
                "Ideology", "Religiosity", "Native Born",
                "Employment", "Trust in Government", "Political Interest")

treats <- c("Germany Migration", "World Migration", "Germany Climate", "World Climate",
            "Germany Climate Migration", "World Climate Migration")
treat_conditions <- c(1:6)

balmat <- data.frame(matrix(NA,length(vars)*length(treats), 6))
colnames(balmat) <- c("Var.",  "Treatment ID", "Treatment", "T-Test P val.", 
                      "Ctrl. Mean", "Treatment Mean") 

j <- c()
for(i in 1:length(vars)){
  a <- rep(var_labels[i], 6)
  j <- append(x = j, values = a)}

balmat[, 1] <- j
balmat[, 2] <- rep((1:6), 11)
balmat[, 3] <- rep(treats, 11)

counter <- 0
for (i in 1:length(vars)){
  for (j in 1:length(treat_conditions)){
    string <- paste('t_test <- t.test(ger_article$', vars[i], '[ger_article$TREATMENT==', treat_conditions[j], '], ger_article$', 
                    vars[i], '[ger_article$TREATMENT=="Control"])', 
                    sep = "", collapse = "")
    eval(parse(text=string))
    balmat[(counter+j),4] <- round(t_test$p.value, digits = 3)
    balmat[(counter+j),5] <- round(t_test$estimate[2], digits = 3)
    balmat[(counter+j),6] <- round(t_test$estimate[1], digits = 3)
    
  }
  counter <- counter+6
}

balmat[balmat$t_pval<.1, ] 

xtable(balmat[, c(1, 3:6)], 
       font.size = "tiny", caption = "Experiment 1 Balance Tests, German Sample")

sum_stats <- data.frame(matrix(NA,length(vars), 7))
colnames(sum_stats) <- c("Var.", "Min.", "1st Qu.",  "Median",
                         "Mean", "3rd Qu.",    "Max." )
sum_stats[, 1] <- var_labels

for (i in 1:length(vars)){
  string <- paste('sum <- summary(ger_article$',vars[i], ')',
                  sep = "", collapse = "")
  eval(parse(text=string))
  sum_stats[i, 2:7] <- sum
  
}

xtable(sum_stats, caption = "Experiment 1 Summary Statistics, German Sample", digits = c(0, 0, 0, 2, 2, 2, 2, 0))

sum(ger_article$AGE < 18)

######### T-Tests ######### 
ger_article$TREATMENT <- factor(ger_article$TREATMENT)

outcomes <- c("climate_index", "migration_index", "climate_migration_index")
tmat <- data.frame(matrix(NA,length(outcomes)*length(treats), 11))
colnames(tmat) <- c("treat_condition", "treat_id", "outcome", "t_pval", 
                    "control_mean", "treat_mean", "mean_diff", "ci_low", "ci_high",
                    "Location", "Prime")  


k <- c()
for(i in 1:length(outcomes)){
  a <- rep(outcomes[i], 6)
  k <- append(x = k, values = a)}

tmat[, 3] <- k
tmat[, 1] <- rep((1:6), 3)
tmat[, 2] <- rep(treats, 3)
tmat[, 10] <- rep(c("GER", "World"), 9)
tmat[, 11] <- rep(c("Migration","Migration", "Climate",  "Climate","Climate Migration","Climate Migration"), 3)

counter <- 0
for (i in 1:length(outcomes)){
  for (j in 1:length(treat_conditions)){
    string <- paste('t_test <- t.test(ger_article$', outcomes[i], '[ger_article$TREATMENT==', treat_conditions[j], '], ger_article$', 
                    outcomes[i], '[ger_article$TREATMENT=="Control"])', 
                    sep = "", collapse = "")
    eval(parse(text=string))
    tmat[(counter+j),4] <- round(t_test$p.value, digits = 3)
    tmat[(counter+j),5] <- round(t_test$estimate[2], digits = 3)
    tmat[(counter+j),6] <- round(t_test$estimate[1], digits = 3)
    tmat[(counter+j),7] <- tmat[(counter+j),6]-tmat[(counter+j),5]
    tmat[(counter+j),8] <- round(t_test$conf.int[1], digits = 3)
    tmat[(counter+j),9] <- round(t_test$conf.int[2], digits = 3)
  }
  counter <- counter+6
}

tmat[tmat$t_pval<.1, ] 

#Migration level outcome
outcomes3 <- c("MIG_LEVELS")
tmat3 <- data.frame(matrix(NA,length(outcomes3)*length(treats), 11))
colnames(tmat3) <- c("treat_condition", "treat_id", "outcome", "t_pval", 
                     "control_mean", "treat_mean", "mean_diff", "ci_low", "ci_high",
                     "Location", "Prime")  


tmat3[, 3] <- rep(outcomes3, 6)
tmat3[, 1] <- 1:6
tmat3[, 2] <- treats
tmat3[, 10] <- rep(c("GER", "World"), 3)
tmat3[, 11] <- rep(c("Migration","Migration", "Climate",  "Climate","Climate Migration","Climate Migration"), 1)

ger_article$MIG_LEVELS <- as.numeric(ger_article$MIG_LEVELS)

counter <- 0
for (j in 1:length(treat_conditions)){
  string <- paste('t_test <- t.test(ger_article$MIG_LEVELS[ger_article$TREATMENT==', treat_conditions[j], '], ger_article$MIG_LEVELS', '[ger_article$TREATMENT=="Control"])', 
                  sep = "", collapse = "")
  eval(parse(text=string))
  tmat3[(counter+j),4] <- round(t_test$p.value, digits = 3)
  tmat3[(counter+j),5] <- round(t_test$estimate[2], digits = 3)
  tmat3[(counter+j),6] <- round(t_test$estimate[1], digits = 3)
  tmat3[(counter+j),7] <- tmat[(counter+j),6]-tmat[(counter+j),5]
  tmat3[(counter+j),8] <- round(t_test$conf.int[1], digits = 3)
  tmat3[(counter+j),9] <- round(t_test$conf.int[2], digits = 3)

}

######### Regression models ######### 
#Unweighted
lm_climate <- lm(climate_index ~ TREATMENT+AGE+
                   fp_orientation_index+empathy_index+
                   NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                   RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                   east_indicator + urban_indicator,
                 data=ger_article)
lm_climate_migration <- lm(climate_migration_index ~ TREATMENT+AGE+
                             fp_orientation_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                             east_indicator + urban_indicator,
                           data=ger_article)
lm_migration <- lm(migration_index ~ TREATMENT+AGE+
                     fp_orientation_index+empathy_index+
                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                     east_indicator + urban_indicator,
                   data=ger_article)

stargazer(lm_climate, lm_migration, lm_climate_migration,
          header=FALSE, 
          title = "Issue Importance: German Sample, Unweighted",
          dep.var.caption = "",
          # font.size = "tiny",
          model.numbers = F,
          dep.var.labels.include = F,
          model.names = F,
          column.labels = c("Climate", "Migration", "Climate Migration"),
          digits=2, 
          no.space=T,
          column.sep.width= "0pt", 
          omit.stat = c("f", "ser", "rsq"),
          covariate.labels = c("Treat: GER Migration", "Treat: Word Migration", "Treat: GER Climate",
                               "Treat: World Climate", "Treat: GER Climate Migration", 
                               "Treat: World Climate Migration",
                               "Age",
                               "Foreign Policy Orientation",
                               "Empathy",
                               "Native Born",
                               "Gender",
                               "Education",
                               "Ideology",
                               "Religiosity",
                               "Trust in Government",
                               "Political Interest",
                               "Employment Status",
                               "Eastern State",
                               "Urban"))
