######### Setup ######### 
remove(list=ls())
set.seed(8675309)
memory.limit(size=20000) 
options(xtable.comment = FALSE)
wd <- "C:/Users/sbari/Dropbox/Research/Climate Migration Paper/Final Files" #Change to your local WD

setwd(wd)

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
# 1. Study 1 (US): conjoint_design.dat
# 2. Study 1 (US): conjoint_design_internal.dat
# 3. Study 1 (US): Climate Migration 2_ Conjoint- USA_September 10, 2019_08.56.csv
# 4. Study 1 (Germany): conjoint_design_german2.dat
# 5. Study 1 (Germany): conjoint_design_german2_internal.dat
# 6. Study 1 (Germany): Climate Migration 2_ Conjoint- Germany_September 11, 2019_02.58.csv
# 7. Study 1 and Study 2 (Germany): germany_state_key.csv
# 8. Study 2 (US): usclimate_exp1.csv
# 9. Study 2 (Germany): Climate Migration 1_ Article- Germany_September 7, 2019_09.31.csv
# 9. Study 3 Follow-Up (US): Migration Follow-Up_August 1, 2020_12.19.csv


########################### Study 1: US ########################### 
######### Import data ######### 
cjt_us_design <- makeDesign(type="file", filename= "conjoint_design.dat")

cjt_us_design_internal <- makeDesign(type="file", filename= 'conjoint_design_internal.dat')

cjt_us_data <- read.qualtrics(filename = "Climate Migration 2_ Conjoint- USA_September 10, 2019_08.56.csv", 
                              new.format = T, 
                              respondentID = "ResponseId",
                              responses=c("force_choice", "Q184", "Q186", 
                                          "Q188", "Q190", "Q192",
                                          "Q194", "Q196", "Q198"),
                              covariates=c("GENDER_resp", "EDUCATION_resp",	"IDEOLOGY",
                                           'PARTISANSHIP',	'PARTISANSHIP_D',	'PARTISANSHIP_R',	'PARTISANSHIP_I', 'RELIGIOSITY_resp',
                                           'NATIVE_BORN',	'EMPLOYMENT_resp',
                                           'Age',	'TRUST_GOVT',	'POL_INTEREST',
                                           'FP_ORIENTATION_1',	'FP_ORIENTATION_2',	'FP_ORIENTATION_3',	'FP_ORIENTATION_4',
                                           'SOC_DOM_1',	'SOC_DOM_2',	'SOC_DOM_3',	'SOC_DOM_4',
                                           'EMPATHY_1',	'EMPATHY_2',	'EMPATHY_3',	'EMPATHY_4',
                                           'city', 'state_region'))

cjt_us_data_relig <- read.qualtrics(filename = "Climate Migration 2_ Conjoint- USA_September 10, 2019_08.56.csv", 
                                    new.format = T, 
                                    respondentID = "ResponseId",
                                    responses=c("force_choice", "Q184", "Q186", 
                                                "Q188", "Q190", "Q192",
                                                "Q194", "Q196", "Q198"),
                                    covariates=c('RELIGIOSITY_resp'))

cjt_us_data_rank <- read.qualtrics(filename = "Climate Migration 2_ Conjoint- USA_September 10, 2019_08.56.csv", 
                                   new.format = T, 
                                   respondentID = "ResponseId",
                                   responses=c("force_choice", "Q184", "Q186", 
                                               "Q188", "Q190", "Q192",
                                               "Q194", "Q196", "Q198"),
                                   covariates=c("GENDER_resp", "EDUCATION_resp",	"IDEOLOGY",
                                                'PARTISANSHIP',	'PARTISANSHIP_D',	'PARTISANSHIP_R',	'PARTISANSHIP_I',
                                                'NATIVE_BORN',	'EMPLOYMENT_resp',
                                                'Age',	'TRUST_GOVT',	'POL_INTEREST',
                                                'FP_ORIENTATION_1',	'FP_ORIENTATION_2',	'FP_ORIENTATION_3',	'FP_ORIENTATION_4',
                                                'SOC_DOM_1',	'SOC_DOM_2',	'SOC_DOM_3',	'SOC_DOM_4',
                                                'EMPATHY_1',	'EMPATHY_2',	'EMPATHY_3',	'EMPATHY_4',
                                                'city', 'state_region'),
                                   ranks = c("RATE_MIG1","RATE_MIG2",
                                             "Q168", "Q169",
                                             "Q170", "Q171",
                                             "Q172", "Q173",
                                             "Q174", "Q175",
                                             "Q176", "Q177",
                                             "Q178", "Q179",
                                             "Q180", "Q181",
                                             "Q182", "Q183"))

cjt_us_data_rank <- cjt_us_data_rank %>% 
  dplyr::rename(rank_outcome = selected) %>% 
  dplyr::select(rank_outcome)

cjt_us_data_relig <- cjt_us_data_relig %>% 
  dplyr::select(RELIGIOSITY_resp)

cjt_us_data <- cbind(cjt_us_data, cjt_us_data_rank, cjt_us_data_relig)

cjt_us_data <- cjt_us_data[!is.na(cjt_us_data$selected), ]

######### Recoding ######### 
cjt_us_data <- cjt_us_data[cjt_us_data$Occupation != 'Unemployer', ]
cjt_us_data$Occupation <- factor(cjt_us_data$Occupation)

cjt_us_data$PARTISANSHIP6 <- NA

for(i in 1:nrow(cjt_us_data)){
  if(cjt_us_data[i, "PARTISANSHIP_D"]==1){
    cjt_us_data[i, "PARTISANSHIP6"]<- 6}
  if(cjt_us_data[i, "PARTISANSHIP_D"]==2){
    cjt_us_data[i, "PARTISANSHIP6"]<- 5}
  if(cjt_us_data[i, "PARTISANSHIP_I"]==1){
    cjt_us_data[i, "PARTISANSHIP6"]<- 4}
  if(cjt_us_data[i, "PARTISANSHIP_I"]==2){
    cjt_us_data[i, "PARTISANSHIP6"]<- 3}
  if(cjt_us_data[i, "PARTISANSHIP_R"]==2){
    cjt_us_data[i, "PARTISANSHIP6"]<- 2}
  if(cjt_us_data[i, "PARTISANSHIP_R"]==1){
    cjt_us_data[i, "PARTISANSHIP6"]<- 1}
}  

cjt_us_data$FP_ORIENTATION_1 <- car::recode(cjt_us_data$FP_ORIENTATION_1,
                                            "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
cjt_us_data$FP_ORIENTATION_2 <- car::recode(cjt_us_data$FP_ORIENTATION_2,
                                            "'Definitely agree'=5; 'Somewhat agree'=4; 'Neither agree nor disagree'=3; 'Somewhat disagree'=2; 'Definitely disagree'=1")
#reverse coded 
cjt_us_data$FP_ORIENTATION_3 <- car::recode(cjt_us_data$FP_ORIENTATION_3,
                                            "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")
#reverse coded
cjt_us_data$FP_ORIENTATION_4 <-  car::recode(cjt_us_data$FP_ORIENTATION_4,
                                             "'Definitely agree'=1; 'Somewhat agree'=2; 'Neither agree nor disagree'=3; 'Somewhat disagree'=4; 'Definitely disagree'=5")

cjt_us_data$SOC_DOM_1 <- car::recode(cjt_us_data$SOC_DOM_1,
                                     "'Definitely favor'=1; 'Somewhat favor'=2; 'Neither oppose nor favor'=3; 'Somewhat oppose'=4; 'Definitely oppose'=5")
#reverse coded
cjt_us_data$SOC_DOM_2 <- car::recode(cjt_us_data$SOC_DOM_2,
                                     "'Definitely favor'=5; 'Somewhat favor'=4; 'Neither oppose nor favor'=3; 'Somewhat oppose'=2; 'Definitely oppose'=1")

cjt_us_data$SOC_DOM_3 <- car::recode(cjt_us_data$SOC_DOM_3,
                                     "'Definitely favor'=1; 'Somewhat favor'=2; 'Neither oppose nor favor'=3; 'Somewhat oppose'=4; 'Definitely oppose'=5")
#reverse coded
cjt_us_data$SOC_DOM_4 <- car::recode(cjt_us_data$SOC_DOM_4,
                                     "'Definitely favor'=5; 'Somewhat favor'=4; 'Neither oppose nor favor'=3; 'Somewhat oppose'=2; 'Definitely oppose'=1")

cjt_us_data$EMPATHY_1 <- car::recode(cjt_us_data$EMPATHY_1,
                                     "'Describes me very well'=5; 'Describes me fairly well'=4; 'Describes me moderately well'=3; 'Describes me very little'=2; 'Does not describe me at all'=1")
#reverse coded
cjt_us_data$EMPATHY_2 <- car::recode(cjt_us_data$EMPATHY_2,
                                     "'Describes me very well'=1; 'Describes me fairly well'=2; 'Describes me moderately well'=3; 'Describes me very little'=4; 'Does not describe me at all'=5")
#reverse coded
cjt_us_data$EMPATHY_3 <- car::recode(cjt_us_data$EMPATHY_3,
                                     "'Describes me very well'=1; 'Describes me fairly well'=2; 'Describes me moderately well'=3; 'Describes me very little'=4; 'Does not describe me at all'=5")
cjt_us_data$EMPATHY_4 <- car::recode(cjt_us_data$EMPATHY_4,
                                     "'Describes me very well'=5; 'Describes me fairly well'=4; 'Describes me moderately well'=3; 'Describes me very little'=2; 'Does not describe me at all'=1")

cjt_us_data <- cjt_us_data %>% 
  mutate(PARTISANSHIP_bin = ifelse(PARTISANSHIP6>3, "D", "R"),
         AGE = as.numeric(Age)) 

cjt_us_data$border_state_indicator <- 1*(cjt_us_data$state_region %in% c("TX", "CA", "AZ", "NM"))

cjt_us_data$border_state_indicator_noCA <- 1*(cjt_us_data$state_region %in% c("TX", "AZ", "NM"))

cjt_us_data$urban_indicator <- 1*(cjt_us_data$city %in% c("New York", "Los Angeles", "Chicago", 
                                                          "Houston", "Phoenix", "Philadelphia",
                                                          "San Antonio", "San Diego", "Dallas", "San Jose")) 

cjt_us_data <- cjt_us_data[5:nrow(cjt_us_data), ]

cjt_us_data$EDUCATION_num <- as.numeric(cjt_us_data$EDUCATION_resp)-1
cjt_us_data$IDEOLOGY_num <- as.numeric(car::recode(cjt_us_data$IDEOLOGY,
                                                   "'Extremely liberal'=7; 'Liberal'=6; 'Slightly liberal'=5; 
'Moderate, middle of the road'=4; 
      'Slightly conservative'=3; 'Conservative'=2; 'Extremely conservative'=1"))-1
cjt_us_data$RELIGIOSITY_num <- as.numeric(cjt_us_data$RELIGIOSITY_resp)-1
cjt_us_data$NATIVE_BORN_num <- ifelse(cjt_us_data$NATIVE_BORN == "United States", 1, 0)
cjt_us_data$EMPLOYMENT_num <- as.numeric(car::recode(cjt_us_data$EMPLOYMENT,
                                                     "'17'=7; '16'=6; '21'=5; '18'=4; 
      '20'=3; '19'=2; '17 '=1"))
cjt_us_data$TRUST_GOVT_num <- as.numeric(car::recode(cjt_us_data$TRUST_GOVT,
                                                     "'Most of the time'=3; 'Only some of the time'=2; 'Just about always'=1"))-1
cjt_us_data$POL_INTEREST_num <- as.numeric(car::recode(cjt_us_data$POL_INTEREST,
                                                       "'Most of the time'=4; 
      'Some of the time'=3; 'Only now and then'=2; 'Hardly at all'=1"))-1

######### Construct scales ######### 
fp_orientation <- data.frame(cjt_us_data[,c("FP_ORIENTATION_1", "FP_ORIENTATION_2", "FP_ORIENTATION_3", "FP_ORIENTATION_4")])
soc_dom <- data.frame(cjt_us_data[,c("SOC_DOM_1", "SOC_DOM_2", "SOC_DOM_3", "SOC_DOM_4")])
empathy <- data.frame(cjt_us_data[,c("EMPATHY_1", "EMPATHY_2", "EMPATHY_3", "EMPATHY_4")])


fp_orientation <- data.frame(sapply(fp_orientation, FUN= function(x) as.numeric(x))-1)
soc_dom <- data.frame(sapply(soc_dom, FUN= function(x) as.numeric(x))-1)
empathy <- data.frame(sapply(empathy, FUN= function(x) as.numeric(x))-1)

#calculate chronbach's alpha for each index
# psych::alpha(fp_orientation) 
# psych::alpha(soc_dom) 
# psych::alpha(empathy) 

#r=create the index variable as the mean score on the individual items
cjt_us_data$fp_orientation_index <- apply(fp_orientation, MARGIN = 1, FUN = mean)
cjt_us_data$soc_dom_index <- apply(soc_dom, MARGIN = 1, FUN = mean)
cjt_us_data$empathy_index <- apply(empathy, MARGIN = 1, FUN = mean)

######### Balance, summary stats ######### 
cjt_us_data$PARTISANSHIP_bin <- as.factor(cjt_us_data$PARTISANSHIP_bin)
cjt_us_data$PARTISANSHIP_num <- as.numeric(cjt_us_data$PARTISANSHIP_bin)
cjt_us_data$GENDER_num <- ifelse(cjt_us_data$GENDER == "Female", 1, 0)

vars <- c('AGE', 'fp_orientation_index', 'soc_dom_index', "empathy_index", 
          'PARTISANSHIP_num', "GENDER_num", "EDUCATION_num",
          "IDEOLOGY_num",  "NATIVE_BORN_num",
          "EMPLOYMENT_num", "TRUST_GOVT_num", "POL_INTEREST_num"
          ,"RELIGIOSITY_num"
)

var_labels <- c("Age", "Foreign Policy Orientation", "Social Dominance", "Empathy",
                "Partisanship", "Gender", "Education", "Ideology", "Native Born",
                "Employment", "Trust in Government", "Political Interest"
                ,"Religiosity"
)

sum_stats <- data.frame(matrix(NA,length(vars), 7))
colnames(sum_stats) <- c("Var.", "Min.", "1st Qu.",  "Median",
                         "Mean", "3rd Qu.",    "Max." )
sum_stats[, 1] <- var_labels

for (i in 1:length(vars)){
  string <- paste('sum <- summary(cjt_us_data$',vars[i], ')',
                  sep = "", collapse = "")
  eval(parse(text=string))
  sum_stats[i, 2:7] <- sum
  
}

xtable(sum_stats, caption = "Experiment 1 Summary Statistics, US Sample", digits = c(0, 0, 0, 2, 2, 2, 2, 0))

######### Conjoint main analysis ######### 
baselines <- list()
baselines$Vulnerability <- 'None'
baselines$Reason.for.migration <- 'Economic opportunity'
baselines$Occupation <- 'Unemployed'
baselines$Language.Fluency <- 'None'
baselines$Origin <- 'Another region in your country'

cjt_us_results <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                 Reason.for.migration + Religion + Vulnerability, 
                               data = cjt_us_data,
                               cluster= T,
                               respondent.id = 'Response.ID',
                               design= cjt_us_design, 
                               baselines=baselines)

summary(cjt_us_results)$amce
xtable(summary(cjt_us_results)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, US Sample (Compared to baseline levels")

levels.test<-list()
levels.test[["Gender"]]<-c("Female","Male")
levels.test[["Language Fluency"]]<-c('None', "Broken", "Fluent")
levels.test[["Occupation"]]<-c("Unemployed","Cleaner", "Doctor", "Teacher")
levels.test[["Origin"]]<-c("Same Country", "Afghanistan", "Ethiopia", "Myanmar", "Ukraine")
levels.test[["Reason.for.migration"]]<-c("Economic","Drought", "Flooding", "Persecution", "Wildfires")
levels.test[["Religion"]]<-c("Agnostic","Christian", "Muslim")
levels.test[["Vulnerability"]]<-c("None","Food insc.", "No family", "Physical handicap", "PTSD")

plot.amce(cjt_us_results, xlab="Expected Change in Migrant Profile Selection, US",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability"),
          # xlim=c(-0.07, .2),
          text.size=9)

######### Robustness ######### 
#Only respondents who complete all 9 tasks
cjt_us_data_only9tasks <- cjt_us_data %>% 
  group_by(Response.ID) %>% 
  mutate(resp_profiles_count = n()) 

cjt_us_data_only9tasks <- cjt_us_data_only9tasks[cjt_us_data_only9tasks$resp_profiles_count > 17, ]

nrow(cjt_us_data) - nrow(cjt_us_data_only9tasks) 
length(unique(cjt_us_data$Response.ID)) -
  length(unique(cjt_us_data_only9tasks$Response.ID))

cjt_us_results_only_9tasks <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                             Reason.for.migration + Religion + Vulnerability, 
                                           data = cjt_us_data_only9tasks,
                                           cluster= T,
                                           respondent.id = 'Response.ID',
                                           design= cjt_us_design, 
                                           baselines=baselines)

summary(cjt_us_results_only_9tasks)$amce
xtable(summary(cjt_us_results_only_9tasks)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, US Sample (Compared to baseline levels- \n Only respondents who complete all 9 tasks")

summary(cjt_us_results_only_9tasks)$amce$Estimate - summary(cjt_us_results)$amce$Estimate

#Compare internal and external migrant profiles
cjt_us_data_internal <- cjt_us_data[! (cjt_us_data$Origin %in% "Another region in your country" & 
                                         cjt_us_data$Reason.for.migration %in% 
                                         "Political/religious/ethnic persecution"), ]

cjt_us_data_internal_strict <- cjt_us_data[! (cjt_us_data$Origin %in% "Another region in your country") , ]

cjt_us_data_internal_actual <- cjt_us_data[(cjt_us_data$Origin %in% "Another region in your country"), ]

nrow(cjt_us_data) - nrow(cjt_us_data_internal)

length(unique(cjt_us_data$Response.ID)) -
  length(unique(cjt_us_data_internal$Response.ID))

cjt_us_results_internal <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                          Reason.for.migration + Religion + Vulnerability, 
                                        data = cjt_us_data_internal,
                                        cluster= T,
                                        respondent.id = 'Response.ID',
                                        design= cjt_us_design, 
                                        baselines=baselines)

round(summary(cjt_us_results_internal)$amce$Estimate  - summary(cjt_us_results)$amce$Estimate, 3)

us_int_table1 <- as_tibble(summary(cjt_us_results_internal)$amce[, c(1:4, 7)]) %>% 
  cbind(round(summary(cjt_us_results_internal)$amce$Estimate  - summary(cjt_us_results)$amce$Estimate, 3))

names(us_int_table1)[6] <- "Est. Diff. from Full Model"

xtable(us_int_table1, digits = 3,
       font.size = "small", caption = "AMCE, US Sample (Compared to baseline levels- \n Excluding 'implausible' internal migration profiles")

baselines$Origin <- 'Ukraine'
cjt_us_results_internal_strict <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                                 Reason.for.migration + Religion + Vulnerability, 
                                               data = cjt_us_data_internal_strict,
                                               cluster= T,
                                               respondent.id = 'Response.ID',
                                               design= cjt_us_design, 
                                               baselines=baselines)

cjt_us_results2 <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                  Reason.for.migration + Religion + Vulnerability, 
                                data = cjt_us_data,
                                cluster= T,
                                respondent.id = 'Response.ID',
                                design= cjt_us_design, 
                                baselines=baselines)

us_int_table2 <- as_tibble(summary(cjt_us_results_internal_strict)$amce[, c(1:4, 7)]) %>% 
  cbind(round(summary(cjt_us_results_internal_strict)$amce$Estimate  -
                summary(cjt_us_results2)$amce$Estimate[c(1:7, 9:20)], 3))

xtable(us_int_table2, digits = 3,
       font.size = "small", caption = "AMCE, US Sample (Compared to baseline levels- \n Excluding all internal migration profiles")

summary(cjt_us_results_internal_strict)$amce$Estimate  - summary(cjt_us_results2)$amce$Estimate[c(1:7, 9:20)]

place <- round(summary(cjt_us_results_internal_strict)$amce$Estimate  - summary(cjt_us_results2)$amce$Estimate[c(1:7, 9:20)], 4)

cjt_us_internal_comparisons <- cbind(summary(cjt_us_results_internal)$amce$Attribute, 
                                     summary(cjt_us_results_internal)$amce$Level, 
                                     round(summary(cjt_us_results_internal)$amce$Estimate  - summary(cjt_us_results)$amce$Estimate, 4),
                                     c(place[1:9], NA, place[10:19]))

colnames(cjt_us_internal_comparisons) <- c("Attribute", "Level", "Full Model - Implausible Internal Profiles Removed", "Full Model - All Internal Profiles Removed")

xtable(cjt_us_internal_comparisons, 
       font.size = "small", caption = "AMCE Differences, US Sample (Compared to baseline levels- \n Full Set Compared to Restricted Internal Sets \n Baseline Origin for Second Comparison Changed to Ukraine")

baselines <- list()
baselines$Vulnerability <- 'None'
baselines$Reason.for.migration <- 'Economic opportunity'
baselines$Occupation <- 'Unemployed'
baselines$Language.Fluency <- 'Broken'

cjt_us_results_internal_actual <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation  +
                                                 Reason.for.migration + Religion + Vulnerability, 
                                               data = cjt_us_data_internal_actual,
                                               cluster= T,
                                               respondent.id = 'Response.ID',
                                               design= cjt_us_design_internal, 
                                               baselines=baselines)

cjt_us_results3 <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                  Reason.for.migration + Religion + Vulnerability, 
                                data = cjt_us_data,
                                cluster= T,
                                respondent.id = 'Response.ID',
                                design= cjt_us_design, 
                                baselines=baselines)

us_ext_table <- as_tibble(summary(cjt_us_results_internal_actual)$amce[, c(1:4, 7)]) %>% 
  cbind(round(summary(cjt_us_results_internal_actual)$amce$Estimate  - summary(cjt_us_results3)$amce$Estimate[c(1:6, 11:20)], 3))

xtable(us_ext_table, digits = 3,
       font.size = "small", caption = "AMCE, US Sample (Compared to baseline levels- \n Excluding all external migration profiles")

#Only respondents 18 and over
cjt_us_data_age_subset <- subset(x = cjt_us_data, subset = AGE > 17)

nrow(cjt_us_data) - nrow(cjt_us_data_age_subset) 
length(unique(cjt_us_data$Response.ID)) -
  length(unique(cjt_us_data_age_subset$Response.ID))

baselines <- list()
baselines$Vulnerability <- 'None'
baselines$Reason.for.migration <- 'Economic opportunity'
baselines$Occupation <- 'Unemployed'
baselines$Language.Fluency <- 'None'
baselines$Origin <- 'Another region in your country'

cjt_us_results_age_subset <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                            Reason.for.migration + Religion + Vulnerability, 
                                          data = cjt_us_data_age_subset,
                                          cluster= T,
                                          respondent.id = 'Response.ID',
                                          design= cjt_us_design, 
                                          baselines=baselines)

summary(cjt_us_results_age_subset)$amce
xtable(summary(cjt_us_results_age_subset)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, US Sample Age Over 18 (Compared to baseline levels)")

summary(cjt_us_results_age_subset)$amce$Estimate - summary(cjt_us_results)$amce$Estimate

#Look at rank DV
cjt_us_rank <- cjoint::amce(rank_outcome ~ Gender + Language.Fluency + Occupation + Origin +
                              Reason.for.migration + Religion + Vulnerability , 
                            data = cjt_us_data,
                            cluster= T,
                            respondent.id = 'Response.ID',
                            design= cjt_us_design, 
                            baselines=baselines)

summary(cjt_us_rank)$amce
xtable(summary(cjt_us_rank)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, US Sample (Compared to baseline levels")

plot.amce(cjt_us_rank, xlab="Expected Change in Migrant Profile Selection, US (Rating Results)",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability"),
          # xlim=c(-0.07, .2),
          text.size=9)

#Interactions
cjt_us_interaction <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                     Reason.for.migration + Religion + Vulnerability + 
                                     Reason.for.migration*Origin, 
                                   data = cjt_us_data,
                                   cluster= T,
                                   respondent.id = 'Response.ID',
                                   design= cjt_us_design, 
                                   baselines=baselines)
plot.amce(cjt_us_interaction, xlab="Expected Change in Migrant Profile Selection, US",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability", "Origin*Reason"),
          # xlim=c(-0.07, .2),
          text.size=9)

cjt_us_interaction2 <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                      Reason.for.migration + Religion + Vulnerability + 
                                      Reason.for.migration*Vulnerability, 
                                    data = cjt_us_data,
                                    cluster= T,
                                    respondent.id = 'Response.ID',
                                    design= cjt_us_design, 
                                    baselines=baselines)
plot.amce(cjt_us_interaction2, xlab="Expected Change in Migrant Profile Selection, US",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability", "Reason*Vulnerability"),
          # xlim=c(-0.07, .2),
          text.size=9)


######### Marginal Means and Subgroups ######### 
cjt_us_data$Language.Fluency2 <- car::recode(cjt_us_data$Language.Fluency,
                                             "'None'='None_lf'")
cjt_us_data$Origin <- car::recode(cjt_us_data$Origin,
                                  "'Another region in your country'='Same Country'")
cjt_us_data$Reason.for.migration <- car::recode(cjt_us_data$Reason.for.migration,
                                                "'Economic opportunity'='Economic'; 'Political/religious/ethnic persecution'='Persecution'")
cjt_us_data$Religion <- car::recode(cjt_us_data$Religion,
                                    "'Agnostic'='Athiest'")
cjt_us_data$Vulnerability <- car::recode(cjt_us_data$Vulnerability,
                                         "'Food insecurity'='Food insc.'; 'Physically handicapped'='Physical handicap'; 'No surviving family members'='No family';'Post Traumatic Stress Disorder (PTSD)'='PTSD'")

us_mms <- selected ~ Language.Fluency2 + Occupation + 
  Gender + Origin +
  Religion + Vulnerability + Reason.for.migration

us_mms_interaction <- selected ~ Language.Fluency2 + Occupation + 
  Gender + Origin +
  Religion + Vulnerability + Reason.for.migration +
  Reason.for.migration*Origin

plot(mm(cjt_us_data, us_mms, id = ~Response.ID), vline = 0.5, xlab = "Conjoint Marginal Means, US", 
     legend_pos = "none")

plot(mm(cjt_us_data, us_mms_interaction, id = ~Response.ID), vline = 0.5, xlab = "Conjoint Marginal Means, US", 
     legend_pos = "none")


cjt_us_data$PARTISANSHIP_bin <- relevel(cjt_us_data$PARTISANSHIP_bin, "R")
cjt_us_data <- cjt_us_data %>% 
  mutate(empathy_bin_mean = as.factor(ifelse(empathy_index>2.20, "emp_high", "emp_low")),
         empathy_bin_quart = as.factor(ifelse(empathy_index>2.50, "emp_high", 
                                              ifelse(empathy_index<1.75,"emp_low", NA))),
         age_bin_mean = as.factor(ifelse(AGE>44.12, "age_high", "age_low")),
         age_bin_quart = as.factor(ifelse(AGE>60.00 , "age_high", 
                                          ifelse(empathy_index<28.00,"age_low", NA))),
         origin_binary = as.factor(ifelse(Origin=="Same Country", "Same Country", "Other Country")),
         education_college_bin = as.factor(ifelse(EDUCATION_num> 4, "college_degree", "no_college_degree")),
         employed_bin = as.factor(ifelse(EMPLOYMENT_num %in% c(1, 6, 5), "employed", "not_employed")),
         unemployed_bin = as.factor(ifelse(EMPLOYMENT_num==7, "unemployed", "not_unemployed")) 
  )

us_mms_origin_bin <- selected ~ Language.Fluency2 + Occupation + 
  Gender + origin_binary +
  Religion + Vulnerability + Reason.for.migration

us_mms_origin_bin_interaction <- selected ~ Language.Fluency2 + Occupation + 
  Gender + origin_binary +
  Religion + Vulnerability + Reason.for.migration +
  Reason.for.migration*origin_binary

#collapse origin
plot(mm(cjt_us_data, us_mms_origin_bin, id = ~Response.ID), vline = 0.5, xlab = "Conjoint Marginal Means, US", 
     legend_pos = "none", alpha=.9)

#origin * reason
mm_diffs_origin_bin <- mm_diffs(data = cjt_us_data, 
                                formula = selected ~ Language.Fluency2 + Occupation + 
                                  Gender + 
                                  Religion + Vulnerability + Reason.for.migration,
                                by = ~origin_binary, 
                                id = ~Response.ID)

#difference of same country - other country
mm_diffs_origin_bin <- mm_diffs_origin_bin[, c(4:7, 9)]

mm_diffs_origin_bin[18:22, ]

colnames(mm_diffs_origin_bin) <- c("Feature", "Level", "Est.", "SE", "P")
xtable(mm_diffs_origin_bin, digits=3, 
       font.size = "small", caption = "Marginal Mean Differences by Origin, US Sample (same country - other country)")

mm_by_part_bin <- cj(cjt_us_data, 
                     selected ~ Language.Fluency2 + Occupation + 
                       Gender + Origin +
                       Religion + Vulnerability + Reason.for.migration,
                     id = ~Response.ID, estimate = "mm", by = ~PARTISANSHIP_bin)

# test of whether any of the interactions between the by variable and feature levels differ from zero
cj_anova(cjt_us_data, 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~PARTISANSHIP_bin)

mm_by_empathy_bin_mean <- cj(cjt_us_data, 
                             selected ~ Language.Fluency2 + Occupation + 
                               Gender + Origin +
                               Religion + Vulnerability + Reason.for.migration,
                             id = ~Response.ID, estimate = "mm", by = ~empathy_bin_mean)

cj_anova(cjt_us_data, 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~empathy_bin_mean)

mm_by_empathy_bin_quart <- cj(cjt_us_data, 
                              selected ~ Language.Fluency2 + Occupation + 
                                Gender + Origin +
                                Religion + Vulnerability + Reason.for.migration,
                              id = ~Response.ID, estimate = "mm", by = ~empathy_bin_quart)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~empathy_bin_quart)

mm_by_age_bin_mean <- cj(cjt_us_data, 
                         selected ~ Language.Fluency2 + Occupation + 
                           Gender + Origin +
                           Religion + Vulnerability + Reason.for.migration,
                         id = ~Response.ID, estimate = "mm", by = ~age_bin_mean)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~age_bin_mean)

mm_by_age_bin_qurat <- cj(cjt_us_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~age_bin_quart)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~age_bin_quart)

mm_by_gender <- cj(cjt_us_data, 
                   selected ~ Language.Fluency2 + Occupation + 
                     Gender + Origin +
                     Religion + Vulnerability + Reason.for.migration,
                   id = ~Response.ID, estimate = "mm", by = ~Gender)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~Gender)

mm_by_border_state <- cj(cjt_us_data, 
                         selected ~ Language.Fluency2 + Occupation + 
                           Gender + Origin +
                           Religion + Vulnerability + Reason.for.migration,
                         id = ~Response.ID, estimate = "mm", by = ~border_state_indicator)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~border_state_indicator)

mm_by_college_degree <- cj(cjt_us_data, 
                           selected ~ Language.Fluency2 + Occupation + 
                             Gender + Origin +
                             Religion + Vulnerability + Reason.for.migration,
                           id = ~Response.ID, estimate = "mm", by = ~education_college_bin)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~education_college_bin)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~employed_bin)

cj_anova(na.omit(cjt_us_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~unemployed_bin)

mm_by_employed <- cj(cjt_us_data, 
                     selected ~ Language.Fluency2 + Occupation + 
                       Gender + Origin +
                       Religion + Vulnerability + Reason.for.migration,
                     id = ~Response.ID, estimate = "mm", by = ~employed_bin)

mm_by_unemployed <- cj(cjt_us_data, 
                       selected ~ Language.Fluency2 + Occupation + 
                         Gender + Origin +
                         Religion + Vulnerability + Reason.for.migration,
                       id = ~Response.ID, estimate = "mm", by = ~unemployed_bin)

plot(mm_by_part_bin, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (US)", 
     legend_title = "PID") + 
  scale_color_manual(name="PID", labels= c("Dem", "Rep"), values = c("blue", "red"))

plot(mm_by_age_bin_mean, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (US)", 
     legend_title = "Age")  + 
  scale_color_manual(values=c("blue", "red"), name="Age", labels= c("4th quart.", "1st quart."))

plot(mm_by_border_state, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (US)", 
     legend_title = "State") +  
  ggplot2::scale_color_manual(values=c("blue", "red"),name="State", labels= c("Non-Border", "Border"))

plot(mm_by_empathy_bin_quart, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (US)", 
     legend_title = "Empathy") +  
  ggplot2::scale_color_manual(values=c("blue", "red"),name="State", labels= c("4th quart.", "1st quart."))

plot(mm_by_college_degree, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (US)", 
     legend_title = "College Degree") +
  scale_color_manual(name="Education", labels= c("College Degree", "No College Degree"), values = c("blue", "red"))

plot(mm_by_employed, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (US)", 
     legend_title = "Employment") +
  scale_color_manual(name="Employment", labels= c("Employed", "Not Employed"), values = c("blue", "red"))

plot(mm_by_unemployed, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (US)", 
     legend_title = "Employment") +
  scale_color_manual(name="Employment", labels= c("Not Unemployed", "Unemployed"), values = c("blue", "red"))

diff_mms_by_part_bin <- cj(cjt_us_data, 
                           selected ~ Language.Fluency2 + Occupation + 
                             Gender + Origin +
                             Religion + Vulnerability + Reason.for.migration,
                           id = ~Response.ID, estimate = "mm_diff",
                           by = ~PARTISANSHIP_bin)

plot(rbind(mm_by_part_bin, diff_mms_by_part_bin), legend_pos = "none") + ggplot2::facet_wrap(~BY, ncol = 3L)
plot(diff_mms_by_part_bin, legend_pos = "none",
     xlab= "Marginal Mean Diff., D-R") #difference in marginal means R and D


######### Diagnostics #########
plot(cj_freqs(cjt_us_data, us_mms, id = ~Response.ID), legend_pos = "none", ylab = "Frequency of Level in Conjoint Design (US)") 

plot(cj(cjt_us_data, us_mms, id = ~Response.ID, by = ~profile, estimate = "mm"), group = "profile", vline = 0.5, legend_pos = "none", xlab= "Marginal Mean, Left vs Right Profile (US)")

########################### Study 1: Germany  ########################## 
######### Import data ######### 
cjt_ger_design2 <- makeDesign(type="file", filename= 'conjoint_design_german2.dat')

cjt_ger_design2_internal <- makeDesign(type="file", filename= 'conjoint_design_german2_internal.dat')


cjt_ger_data <- read.qualtrics(filename = "Climate Migration 2_ Conjoint- Germany_September 11, 2019_02.58.csv", 
                               new.format = T, 
                               respondentID = "ResponseId",
                               responses=c("force_choice", "Q184", "Q186", 
                                           "Q188", "Q190", "Q192",
                                           "Q194", "Q196", "Q198"),
                               covariates=c("GENDER_resp", "EDUCATION_resp",	"IDEOLOGY",
                                            'RELIGIOSITY_resp',	'NATIVE_BORN',	'EMPLOYMENT_resp',
                                            'Age',	'TRUST_GOVT',	'POL_INTEREST',	
                                            'FP_ORIENTATION_1',	'FP_ORIENTATION_2',	'FP_ORIENTATION_3',	'FP_ORIENTATION_4',
                                            'EMPATHY_1',	'EMPATHY_2',	'EMPATHY_3',	'EMPATHY_4',
                                            'city', 'state_region'))

cjt_ger_data_trust_gov <- read.qualtrics(filename = "Climate Migration 2_ Conjoint- Germany_September 11, 2019_02.58.csv", 
                                         new.format = T, 
                                         respondentID = "ResponseId",
                                         responses=c("force_choice", "Q184", "Q186", 
                                                     "Q188", "Q190", "Q192",
                                                     "Q194", "Q196", "Q198"),
                                         covariates=c('TRUST_GOVT'))

cjt_ger_data_rank <- read.qualtrics(filename = "Climate Migration 2_ Conjoint- Germany_September 11, 2019_02.58.csv", 
                                    new.format = T, 
                                    respondentID = "ResponseId",
                                    responses=c("force_choice", "Q184", "Q186", 
                                                "Q188", "Q190", "Q192",
                                                "Q194", "Q196", "Q198"),
                                    covariates=c("GENDER_resp", "EDUCATION_resp",	"IDEOLOGY",
                                                 'RELIGIOSITY_resp',	'NATIVE_BORN',	'EMPLOYMENT_resp',
                                                 'Age',	'TRUST_GOVT',	'POL_INTEREST',	
                                                 'FP_ORIENTATION_1',	'FP_ORIENTATION_2',	'FP_ORIENTATION_3',	'FP_ORIENTATION_4',
                                                 'EMPATHY_1',	'EMPATHY_2',	'EMPATHY_3',	'EMPATHY_4',
                                                 'city', 'state_region'),
                                    ranks = c("RATE_MIG1","RATE_MIG2",
                                              "Q168", "Q169",
                                              "Q170", "Q171",
                                              "Q172", "Q173",
                                              "Q174", "Q175",
                                              "Q176", "Q177",
                                              "Q178", "Q179",
                                              "Q180", "Q181",
                                              "Q182", "Q183"))

cjt_ger_data_rank <- cjt_ger_data_rank %>% 
  dplyr::rename(rank_outcome = selected) %>% 
  dplyr::select(rank_outcome)

cjt_ger_data_trust_gov <- cjt_ger_data_trust_gov %>% 
  dplyr::select(TRUST_GOVT)

cjt_ger_data <- cbind(cjt_ger_data, cjt_ger_data_rank, cjt_ger_data_trust_gov)

cjt_ger_data <- cjt_ger_data[!is.na(cjt_ger_data$selected), ]

######### Recoding ######### 
ger_states <- read.csv(file = 'germany_state_key.csv') 

names(ger_states) <- c("qualtrics_code", "state_name", "region",
                       "east_indicator", "east_indicator2",
                       "east_indicator3", "east_indicator4",
                       "east_indicator5", "east_indicator6",
                       "east_indicator7")

cjt_ger_data <- cjt_ger_data %>%
  mutate(state_num = as.numeric(paste(state_region))) %>%
  left_join(ger_states, by=c("state_num"= "qualtrics_code"))

cjt_ger_data$urban_indicator <- 1*(cjt_ger_data$city %in% c("Berlin", "Hamburg", "Munich",
                                                            "Cologne", "Frankfurt Am Main", "Stuttgart",
                                                            "Dusseldorf", "Dortmund", "Essen", "Leipzig"))

cjt_ger_data$FP_ORIENTATION_1 <- car::recode(cjt_ger_data$FP_ORIENTATION_1,
                                             "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
cjt_ger_data$FP_ORIENTATION_2 <- car::recode(cjt_ger_data$FP_ORIENTATION_2,
                                             "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
#reverse coded 
cjt_ger_data$FP_ORIENTATION_3 <- car::recode(cjt_ger_data$FP_ORIENTATION_3,
                                             "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")

cjt_ger_data$EMPATHY_1 <- car::recode(cjt_ger_data$EMPATHY_1,
                                      "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
#reverse coded
cjt_ger_data$EMPATHY_2 <- car::recode(cjt_ger_data$EMPATHY_2,
                                      "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")

#reverse coded
cjt_ger_data$EMPATHY_3 <- car::recode(cjt_ger_data$EMPATHY_3,
                                      "'Stimme vollstÃ¤ndig zu'=1; 'Stimme weitestgehend zu'=2; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=4; 'Stimme gar nicht zu'=5")

cjt_ger_data$EMPATHY_4 <- car::recode(cjt_ger_data$EMPATHY_4,
                                      "'Stimme vollstÃ¤ndig zu'=5; 'Stimme weitestgehend zu'=4; 'Stimme weder zu noch nicht zu'=3; 'Stimme weniger zu'=2; 'Stimme gar nicht zu'=1")
cjt_ger_data <- cjt_ger_data %>% 
  mutate(AGE = as.numeric(Age)) 

cjt_ger_data$GENDER_num <- ifelse(cjt_ger_data$GENDER_resp == 2, 1, 0)

cjt_ger_data$EDUCATION_num <- as.numeric(car::recode(cjt_ger_data$EDUCATION,
                                                     "'Abgeschlossenes Hochschulstudium'=6; 'Angefangenes Hochschulstudium'=5; 'Abitur'=4; 
                                                     'Facabitur'=3; 'Realschulabschluss'=2; 'Haptschulabschluss'=1"))-1
cjt_ger_data$IDEOLOGY_num <- as.numeric(car::recode(cjt_ger_data$IDEOLOGY,
                                                    "'Extrem liberal'=7; 'Liberal'=6; 'Etwas liberal'=5; 
                                                    'Moderat, die gemÃÂ¤ÃÅ¸igte Mitte'=4; 
                                                    'Etwas konservativ'=3; 'Konservativ'=2; 'Extrem konservativ'=1"))-1
cjt_ger_data$RELIGIOSITY_num <- as.numeric(car::recode(cjt_ger_data$RELIGIOSITY,
                                                       "'Mehr als einmal die Woche'=6; 'WÃÂ¶chentlich '=5; 'Ein paar Mal im Monat'=4; 
                                                       'Ein paar Mal im Jahr'=3; 'Einmal im Jahr oder weniger'=2; 'Nie'=1"))-1
cjt_ger_data$NATIVE_BORN_num <- ifelse(cjt_ger_data$NATIVE_BORN == 1, 1, 0)

cjt_ger_data$EMPLOYMENT_num <- as.numeric(car::recode(cjt_ger_data$EMPLOYMENT,
                                                      "'17'=7; '16'=6; '21'=5; '18'=4; 
                                                      '20'=3; '19'=2; '17 '=1"))
cjt_ger_data$TRUST_GOVT_num <-as.numeric(cjt_ger_data$TRUST_GOVT)-1
cjt_ger_data$POL_INTEREST_num <- as.numeric(car::recode(cjt_ger_data$POL_INTEREST,
                                                        "'Mesitens'=4; 
                                                        'Manchmal'=3; 'Nur ab und zu'=2; 'Kaum'=1"))-1

######### Construct scales ######### 
fp_orientation_ger <- data.frame(cjt_ger_data[,c("FP_ORIENTATION_1", "FP_ORIENTATION_2", "FP_ORIENTATION_3")])
empathy_ger <- data.frame(cjt_ger_data[,c("EMPATHY_1", "EMPATHY_2", "EMPATHY_3", "EMPATHY_4")])

fp_orientation_ger <- data.frame(sapply(fp_orientation_ger, FUN= function(x) as.numeric(x))-1)
empathy_ger <- data.frame(sapply(empathy_ger, FUN= function(x) as.numeric(x))-1)

#calculate chronbach's alpha for each index
# psych::alpha(fp_orientation_ger)  
# psych::alpha(empathy_ger) 

#r=create the index variable as the mean score on the individual items
cjt_ger_data$fp_orientation_index <- apply(fp_orientation_ger, MARGIN = 1, FUN = mean)
cjt_ger_data$empathy_index <- apply(empathy_ger, MARGIN = 1, FUN = mean)

######### Balance, summary stats ######### 
vars <- c('AGE', 'fp_orientation_index',  "empathy_index", 
          "GENDER_num", "EDUCATION_num",
          "IDEOLOGY_num",  "NATIVE_BORN_num",
          "EMPLOYMENT_num", "TRUST_GOVT_num",
          "POL_INTEREST_num","RELIGIOSITY_num"
)

var_labels <- c("Age", "Foreign Policy Orientation",  "Empathy",
                "Gender", "Education", "Ideology", "Native Born",
                "Employment", "Trust in Government", 
                "Political Interest","Religiosity"
)

sum_stats <- data.frame(matrix(NA,length(vars), 7))
colnames(sum_stats) <- c("Var.", "Min.", "1st Qu.",  "Median",
                         "Mean", "3rd Qu.",    "Max." )
sum_stats[, 1] <- var_labels

for (i in 1:length(vars)){
  string <- paste('sum <- summary(cjt_ger_data$',vars[i], ')',
                  sep = "", collapse = "")
  eval(parse(text=string))
  sum_stats[i, 2:7] <- sum
  
}

xtable(sum_stats, caption = "Experiment 1 Summary Statistics, GER Sample", digits = c(0, 0, 0, 2, 2, 2, 2, 0))

######### Conjoint main analysis ######### 
names(cjt_ger_data)[23] <- "Occupation"
names(cjt_ger_data)[25] <- "Gender"
names(cjt_ger_data)[27] <- "Reason.for.migration"
names(cjt_ger_data)[29] <- "Origin"
names(cjt_ger_data)[31] <- "Religion"
names(cjt_ger_data)[34] <- "Language.Fluency"
names(cjt_ger_data)[36] <- "Vulnerability"

baselines <- list()
baselines$Gender <- 'Weiblich'
baselines$Vulnerability <- 'Keine'
baselines$'Reason.for.migration' <- 'Wirtschaftliche Perspektive'
baselines$Occupation <- 'Arbeitslos'
baselines$'Language Fluency' <- 'Keine'
baselines$Origin <- 'Aus einem anderen Teil Ihres Landes'

cjt_ger_results <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                  Reason.for.migration + Religion + Vulnerability, 
                                data = cjt_ger_data,
                                cluster= T,
                                respondent.id = 'Response.ID',
                                design= cjt_ger_design2, 
                                baselines=baselines
)

summary(cjt_ger_results)$amce
xtable(summary(cjt_ger_results)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, German Sample (Compared to baseline levels")


levels.test<-list()
levels.test[["Gender"]]<-c("Female","Male")
levels.test[["Language Fluency"]]<-c('None', "Fluent", "Broken")
levels.test[["Occupation"]]<-c("Unemployed","Doctor", "Teacher", "Cleaner")
levels.test[["Origin"]]<-c("Same Country", "Ethiopia", "Afghanistan", "Myanmar", "Ukraine")
levels.test[["Reason.for.migration"]]<-c("Economic","Flooding", "Drought", "Persecution", "Wildfires")
levels.test[["Religion"]]<-c("Athiest","Christian", "Muslim")
levels.test[["Vulnerability"]]<-c("None","Food insc.", "Physical handicap", "No family", "PTSD")

plot.amce(cjt_ger_results, xlab="Expected Change in Migrant Profile Selection, Germany",
          main="",
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability"),
          xlim=c(-0.11, .2),
          text.size=9)

######### Robustness ######### 
#Only respondents who complete all 9 tasks
cjt_ger_data_only9tasks <- cjt_ger_data %>% 
  group_by(Response.ID) %>% 
  mutate(resp_profiles_count = n()) 

cjt_ger_data_only9tasks <- cjt_ger_data_only9tasks[cjt_ger_data_only9tasks$resp_profiles_count > 17, ]

nrow(cjt_ger_data) - nrow(cjt_ger_data_only9tasks) 
length(unique(cjt_ger_data$Response.ID)) -
  length(unique(cjt_ger_data_only9tasks$Response.ID))

cjt_ger_results_only_9tasks <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                              Reason.for.migration + Religion + Vulnerability, 
                                            data = cjt_ger_data_only9tasks,
                                            cluster= T,
                                            respondent.id = 'Response.ID',
                                            design= cjt_ger_design2, 
                                            baselines=baselines)

summary(cjt_ger_results_only_9tasks)$amce
xtable(summary(cjt_ger_results_only_9tasks)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, German Sample (Compared to baseline levels- \n Only respondents who complete all 9 tasks")

summary(cjt_ger_results_only_9tasks)$amce$Estimate - summary(cjt_ger_results)$amce$Estimate

#Compare internal and external migrant profiles
cjt_ger_data_internal <- cjt_ger_data[! (cjt_ger_data$Origin %in% "Aus einem anderen Teil Ihres Landes" & 
                                           cjt_ger_data$Reason.for.migration %in% 
                                           c("DÃ¼rre", "WaldbrÃ¤nde",
                                             "Politische/religiÃ¶se/ethnische Verfolgung")), ]

cjt_ger_data_internal_strict <- cjt_ger_data[! (cjt_ger_data$Origin %in% "Aus einem anderen Teil Ihres Landes"), ]


length(unique(cjt_ger_data$Response.ID)) -
  length(unique(cjt_ger_data_internal$Response.ID))

cjt_ger_results_internal <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                           Reason.for.migration + Religion + Vulnerability, 
                                         data = cjt_ger_data_internal,
                                         cluster= T,
                                         respondent.id = 'Response.ID',
                                         design= cjt_ger_design2, 
                                         baselines=baselines)

summary(cjt_ger_results_internal)$amce$Estimate  - summary(cjt_ger_results)$amce$Estimate

ger_int_table1 <- as_tibble(summary(cjt_ger_results_internal)$amce[, c(1:4, 7)]) %>% 
  cbind(round(summary(cjt_ger_results_internal)$amce$Estimate  - summary(cjt_ger_results)$amce$Estimate, 3))

xtable(ger_int_table1, digits = 3,
       font.size = "small", caption = "AMCE, Ger Sample (Compared to baseline levels- \n Excluding 'implausible' internal migration profiles")

baselines$Origin <- 'Ukraine'

cjt_ger_results_internal_strict <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                                  Reason.for.migration + Religion + Vulnerability, 
                                                data = cjt_ger_data_internal_strict,
                                                cluster= T,
                                                respondent.id = 'Response.ID',
                                                design= cjt_ger_design2, 
                                                baselines=baselines)

cjt_ger_results2 <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                   Reason.for.migration + Religion + Vulnerability, 
                                 data = cjt_ger_data,
                                 cluster= T,
                                 respondent.id = 'Response.ID',
                                 design= cjt_ger_design2, 
                                 baselines=baselines
)

ger_int_table2 <- as_tibble(summary(cjt_ger_results_internal_strict)$amce[, c(1:4, 7)]) %>% 
  cbind(round(summary(cjt_ger_results_internal_strict)$amce$Estimate  - summary(cjt_ger_results2)$amce$Estimate[c(1:8, 10:20)], 3))

xtable(ger_int_table2, digits = 3,
       font.size = "small", caption = "AMCE, German Sample (Compared to baseline levels- \n Excluding all internal migration profiles")

summary(cjt_ger_results_internal_strict)$amce$Estimate  - summary(cjt_ger_results2)$amce$Estimate[c(1:8, 10:20)]

place_ger <- round(summary(cjt_ger_results_internal_strict)$amce$Estimate  - summary(cjt_ger_results2)$amce$Estimate[c(1:8, 10:20)], 4)

cjt_ger_internal_comparisons <- cbind(summary(cjt_ger_results_internal)$amce$Attribute, 
                                      summary(cjt_ger_results_internal)$amce$Level, 
                                      round(summary(cjt_ger_results_internal)$amce$Estimate  - summary(cjt_ger_results)$amce$Estimate, 4),
                                      c(place_ger[1:9], NA, place_ger[10:19]))

colnames(cjt_ger_internal_comparisons) <- c("Attribute", "Level", "Full Model - Implausible Internal Profiles Removed", "Full Model - All Internal Profiles Removed")

xtable(cjt_ger_internal_comparisons, 
       font.size = "small", caption = "AMCE Differences, German Sample (Compared to baseline levels- \n Full Set Compared to Restricted Internal Sets \n Baseline Origin for Second Comparison Changed to Ukraine")

baselines <- list()
baselines$Gender <- 'Weiblich'
baselines$Vulnerability <- 'Keine'
baselines$'Reason.for.migration' <- 'Wirtschaftliche Perspektive'
baselines$Occupation <- 'Arbeitslos'
baselines$'Language Fluency' <- 'Gebrochen'

cjt_ger_data_internal_actual <- cjt_ger_data[(cjt_ger_data$Origin %in% "Aus einem anderen Teil Ihres Landes"), ]

cjt_ger_results_internal_actual <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation  +
                                                  Reason.for.migration + Religion + Vulnerability, 
                                                data = cjt_ger_data_internal_actual,
                                                cluster= T,
                                                respondent.id = 'Response.ID',
                                                design= cjt_ger_design2_internal, 
                                                baselines=baselines)

cjt_ger_results3 <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation  +
                                   Reason.for.migration + Religion + Vulnerability, 
                                 data = cjt_ger_data,
                                 cluster= T,
                                 respondent.id = 'Response.ID',
                                 design= cjt_ger_design2, 
                                 baselines=baselines
)

ger_ext_table <- as_tibble(summary(cjt_ger_results_internal_actual)$amce[, c(1:4, 7)]) %>% 
  cbind(round(summary(cjt_ger_results_internal_actual)$amce$Estimate  - summary(cjt_ger_results3)$amce$Estimate[c(1:2, 4:16)], 3))

xtable(ger_ext_table, digits = 3,
       font.size = "small", caption = "AMCE, Ger Sample (Compared to baseline levels- \n Excluding external migration profiles")

#Only respondents 18 and over
cjt_ger_data_age_subset <- subset(x = cjt_ger_data, subset = AGE > 17)

nrow(cjt_ger_data) - nrow(cjt_ger_data_age_subset) 
length(unique(cjt_ger_data$Response.ID)) -
  length(unique(cjt_ger_data_age_subset$Response.ID))

names(cjt_ger_data_age_subset)[23] <- "Occupation"
names(cjt_ger_data_age_subset)[25] <- "Gender"
names(cjt_ger_data_age_subset)[27] <- "Reason.for.migration"
names(cjt_ger_data_age_subset)[29] <- "Origin"
names(cjt_ger_data_age_subset)[31] <- "Religion"
names(cjt_ger_data_age_subset)[34] <- "Language.Fluency"
names(cjt_ger_data_age_subset)[36] <- "Vulnerability"

baselines <- list()
baselines$Gender <- 'Weiblich'
baselines$Vulnerability <- 'Keine'
baselines$'Reason.for.migration' <- 'Wirtschaftliche Perspektive'
baselines$Occupation <- 'Arbeitslos'
baselines$'Language Fluency' <- 'Keine'
baselines$Origin <- 'Aus einem anderen Teil Ihres Landes'

cjt_ger_results_age_subset <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                             Reason.for.migration + Religion + Vulnerability, 
                                           data = cjt_ger_data_age_subset,
                                           cluster= T,
                                           respondent.id = 'Response.ID',
                                           design= cjt_ger_design2, 
                                           baselines=baselines
)

summary(cjt_ger_results_age_subset)$amce
xtable(summary(cjt_ger_results_age_subset)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, German Sample Age Over 18 (Compared to baseline levels)")

summary(cjt_ger_results_age_subset)$amce$Estimate - summary(cjt_ger_results)$amce$Estimate

#Rank DV
cjt_ger_rank <- cjoint::amce(rank_outcome ~ Gender + Language.Fluency + Occupation + Origin +
                               Reason.for.migration + Religion + Vulnerability , 
                             data = cjt_ger_data,
                             cluster= T,
                             respondent.id = 'Response.ID',
                             design= cjt_ger_design2, 
                             baselines=baselines)

summary(cjt_ger_rank)$amce
xtable(summary(cjt_ger_rank)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, German Sample (Compared to baseline levels): Rating Outcome")

plot.amce(cjt_ger_rank, xlab="Expected Change in Migrant Profile Selection, Germany (Rating Results)",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability"),
          # xlim=c(-0.07, .2),
          text.size=9)

#Interactions
cjt_ger_interaction <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                      Reason.for.migration + Religion + Vulnerability + 
                                      Reason.for.migration*Origin, 
                                    data = cjt_ger_data,
                                    cluster= T,
                                    respondent.id = 'Response.ID',
                                    design= cjt_ger_design2, 
                                    baselines=baselines)
plot.amce(cjt_ger_interaction, xlab="Expected Change in Migrant Profile Selection, Germany",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability", "Origin*Reason"),
          # xlim=c(-0.07, .2),
          text.size=9)

cjt_ger_interaction2 <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                                       Reason.for.migration + Religion + Vulnerability + 
                                       Reason.for.migration*Vulnerability, 
                                     data = cjt_ger_data,
                                     cluster= T,
                                     respondent.id = 'Response.ID',
                                     design= cjt_ger_design2, 
                                     baselines=baselines)
plot.amce(cjt_ger_interaction2, xlab="Expected Change in Migrant Profile Selection, Germany",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability", "Reason*Vulnerability"),
          # xlim=c(-0.07, .2),
          text.size=9)

######### Marginal Means ######### 
cjt_ger_data$Language.Fluency2 <- car::recode(cjt_ger_data$Language.Fluency,
                                              "'FlieÃYend'='Fluent'; 'Gebrochen'='Broken'; 'Keine'='None_lf'")
cjt_ger_data$Occupation <- car::recode(cjt_ger_data$Occupation,
                                       "'Arbeitslos'='Unemployed'; 'Arzt'='Doctor'; 'Lehrer'='Teacher';'Reinigungskraft'='Cleaner'")
cjt_ger_data$Gender <- car::recode(cjt_ger_data$Gender,
                                   "'MÃ¤nnlich'='Male'; 'Weiblich'='Female'")
cjt_ger_data$Origin <- car::recode(cjt_ger_data$Origin,
                                   "'Ã\"thiopien'='Ethiopia'; 'Aus einem anderen Teil Ihres Landes'='Same Country'")
cjt_ger_data$Vulnerability <- car::recode(cjt_ger_data$Vulnerability,
                                          "'ErnÃ¤hrungsunsicherheit'='Food insc.'; 'KÃ¶rperliche Behinderung'='Physical handicap'; 'Keine'='None';'Keine Ã¼berlebenden Familienmitglieder'='No family'; 'PTBS (Posttraumatische BelastungsstÃ¶rung)'='PTSD'")
cjt_ger_data$Reason.for.migration <- car::recode(cjt_ger_data$Reason.for.migration,
                                                 "'Ãoberflutung'='Flooding'; 'DÃ¼rre'='Drought'; 'Politische/religiÃ¶se/ethnische Verfolgung'='Persecution';'WaldbrÃ¤nde'='Wildfires';'Wirtschaftliche Perspektive'='Economic'")
cjt_ger_data$Origin <- car::recode(cjt_ger_data$Origin,
                                   "'Aus einem anderen Teil Ihres Landes'='Same Country'")

ger_mms <- selected ~ Language.Fluency2 + Occupation + 
  Gender + Origin +
  Religion + Vulnerability + Reason.for.migration

plot(mm(cjt_ger_data, ger_mms, id = ~Response.ID), vline = 0.5, xlab = "Conjoint Marginal Means, GER", 
     legend_pos = "none")

cjt_ger_data <- cjt_ger_data %>% 
  mutate(empathy_bin_mean = as.factor(ifelse(empathy_index>2.205, "emp_high", "emp_low")),
         empathy_bin_quart = as.factor(ifelse(empathy_index>2.50, "emp_high", 
                                              ifelse(empathy_index<2.0,"emp_low", NA))),
         age_bin_mean = as.factor(ifelse(AGE>45.64, "age_high", "age_low")),
         age_bin_quart = as.factor(ifelse(AGE>60.00 , "age_high", 
                                          ifelse(empathy_index<31.00,"age_low", NA))),
         origin_binary = as.factor(ifelse(Origin=="Same Country", "Same Country", "Other Country")),
         education_college_bin = as.factor(ifelse(EDUCATION_num> 4, "college_degree", "no_college_degree")),
         employed_bin = as.factor(ifelse(EMPLOYMENT_num %in% c(1, 6, 5), "employed", "not_employed")),
         unemployed_bin = as.factor(ifelse(EMPLOYMENT_num==7, "unemployed", "not_unemployed")) 
  )

ger_mms_origin_bin <- selected ~ Language.Fluency2 + Occupation + 
  Gender + origin_binary +
  Religion + Vulnerability + Reason.for.migration

ger_mms_origin_bin_interaction <- selected ~ Language.Fluency2 + Occupation + 
  Gender + origin_binary +
  Religion + Vulnerability + Reason.for.migration +
  Reason.for.migration*origin_binary

#collapse origin
plot(mm(cjt_ger_data, ger_mms_origin_bin, id = ~Response.ID), vline = 0.5, xlab = "Conjoint Marginal Means, Germany", 
     legend_pos = "none", alpha=.9)

#origin * reason
mm_diffs_origin_bin <- mm_diffs(data = cjt_ger_data, 
                                formula = selected ~ Language.Fluency2 + Occupation + 
                                  Gender + 
                                  Religion + Vulnerability + Reason.for.migration,
                                by = ~origin_binary, 
                                id = ~Response.ID)

#difference of same country - other country
mm_diffs_origin_bin <- mm_diffs_origin_bin[, c(4:7, 9)]
mm_diffs_origin_bin[18:22, ]
colnames(mm_diffs_origin_bin) <- c("Feature", "Level", "Est.", "SE", "P")
xtable(mm_diffs_origin_bin, digits=3, 
       font.size = "small", caption = "Marginal Mean Differences by Origin, US Sample (same country - other country)")

mm_by_empathy_bin_mean <- cj(cjt_ger_data, 
                             selected ~ Language.Fluency2 + Occupation + 
                               Gender + Origin +
                               Religion + Vulnerability + Reason.for.migration,
                             id = ~Response.ID, estimate = "mm", by = ~empathy_bin_mean)

cj_anova(cjt_ger_data, 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~empathy_bin_mean)

mm_by_empathy_bin_quart <- cj(cjt_ger_data, 
                              selected ~ Language.Fluency2 + Occupation + 
                                Gender + Origin +
                                Religion + Vulnerability + Reason.for.migration,
                              id = ~Response.ID, estimate = "mm", by = ~empathy_bin_quart)

cj_anova(na.omit(cjt_ger_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~empathy_bin_quart)

mm_by_age_bin_mean <- cj(cjt_ger_data, 
                         selected ~ Language.Fluency2 + Occupation + 
                           Gender + Origin +
                           Religion + Vulnerability + Reason.for.migration,
                         id = ~Response.ID, estimate = "mm", by = ~age_bin_mean)

cj_anova(na.omit(cjt_ger_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~age_bin_mean)

mm_by_age_bin_qurat <- cj(cjt_ger_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~age_bin_quart)

cj_anova(na.omit(cjt_ger_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~age_bin_quart)

mm_by_gender <- cj(cjt_ger_data, 
                   selected ~ Language.Fluency2 + Occupation + 
                     Gender + Origin +
                     Religion + Vulnerability + Reason.for.migration,
                   id = ~Response.ID, estimate = "mm", by = ~Gender)

cj_anova(na.omit(cjt_ger_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~Gender)

mm_by_border_state <- cj(cjt_ger_data, 
                         selected ~ Language.Fluency2 + Occupation + 
                           Gender + Origin +
                           Religion + Vulnerability + Reason.for.migration,
                         id = ~Response.ID, estimate = "mm", by = ~east_indicator)

mm_by_border_state2 <- cj(cjt_ger_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~east_indicator2)

mm_by_border_state3 <- cj(cjt_ger_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~east_indicator3)

mm_by_border_state4 <- cj(cjt_ger_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~east_indicator4)

mm_by_border_state5 <- cj(cjt_ger_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~east_indicator5)

mm_by_border_state6 <- cj(cjt_ger_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~east_indicator6)

mm_by_border_state7 <- cj(cjt_ger_data, 
                          selected ~ Language.Fluency2 + Occupation + 
                            Gender + Origin +
                            Religion + Vulnerability + Reason.for.migration,
                          id = ~Response.ID, estimate = "mm", by = ~east_indicator7)

cj_anova(na.omit(cjt_ger_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~education_college_bin)

cj_anova(na.omit(cjt_ger_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~employed_bin)

cj_anova(na.omit(cjt_ger_data), 
         selected ~ Language.Fluency2 + Occupation + 
           Gender + Origin +
           Religion + Vulnerability + Reason.for.migration,
         by = ~unemployed_bin)

mm_by_college <- cj(cjt_ger_data, 
                    selected ~ Language.Fluency2 + Occupation + 
                      Gender + Origin +
                      Religion + Vulnerability + Reason.for.migration,
                    id = ~Response.ID, estimate = "mm", by = ~education_college_bin)

mm_by_employed <- cj(cjt_ger_data, 
                     selected ~ Language.Fluency2 + Occupation + 
                       Gender + Origin +
                       Religion + Vulnerability + Reason.for.migration,
                     id = ~Response.ID, estimate = "mm", by = ~employed_bin)

mm_by_unemployed <- cj(cjt_ger_data, 
                       selected ~ Language.Fluency2 + Occupation + 
                         Gender + Origin +
                         Religion + Vulnerability + Reason.for.migration,
                       id = ~Response.ID, estimate = "mm", by = ~unemployed_bin)

plot(mm_by_college, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (GER)", 
     legend_title = "College Degree") +
  scale_color_manual(name="Education", labels= c("College Degree", "No College Degree"), values = c("blue", "red"))

plot(mm_by_employed, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (GER)", 
     legend_title = "Employment") +
  scale_color_manual(name="Employment", labels= c("Employed", "Not Employed"), values = c("blue", "red"))

plot(mm_by_unemployed, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (GER)", 
     legend_title = "Employment") +
  scale_color_manual(name="Employment", labels= c("Not Unemployed", "Unemployed"), values = c("blue", "red"))

plot(mm_by_empathy_bin_mean, group = "BY", vline = 0.5, xlab = "Marginal Mean Diff. (GER)", 
     legend_title = "Empathy") +
  scale_color_manual(name="Empathy", 
                     labels= c("Low", "High"),
                     values = c("blue", "red"))

borderplot1 <- plot(mm_by_border_state, group = "BY", vline = 0.5, xlab = "", 
                    legend_title = "Border State") +
  scale_color_manual(name="Border State2", labels= c("Non-Border", "Border"), 
                     values = c("blue", "red")) +
  theme(legend.position="none")

borderplot2 <- plot(mm_by_border_state2, group = "BY", vline = 0.5, xlab = "", 
                    legend_title = "Border State") +
  scale_color_manual(name="Border State1", labels= c("Non-Border", "Border"), 
                     values = c("blue", "red")) +
  theme(legend.position="none")

borderplot3 <- plot(mm_by_border_state3, group = "BY", vline = 0.5, xlab = "", 
                    legend_title = "Border State") +
  scale_color_manual(name="Border State3", labels= c("Non-Border", "Border"), 
                     values = c("blue", "red")) +
  theme(legend.position="none")

borderplot4 <- plot(mm_by_border_state4, group = "BY", vline = 0.5, xlab = "", 
                    legend_title = "Border State") +
  scale_color_manual(name="Border State4", labels= c("Non-Border", "Border"), 
                     values = c("blue", "red")) +
  theme(legend.position="none")

borderplot5 <- plot(mm_by_border_state5, group = "BY", vline = 0.5, xlab = "", 
                    legend_title = "Border State") +
  scale_color_manual(name="Border State5", labels= c("Non-Border", "Border"), 
                     values = c("blue", "red")) +
  theme(legend.position="none")

borderplot6 <- plot(mm_by_border_state6, group = "BY", vline = 0.5, xlab = "", 
                    legend_title = "Border State") +
  scale_color_manual(name="Border State6", labels= c("Non-Border", "Border"), 
                     values = c("blue", "red")) +
  theme(legend.position="none")

borderplot7 <- plot(mm_by_border_state7, group = "BY", vline = 0.5, xlab = "", 
                    legend_title = "Border State") +
  scale_color_manual(name="Border State7", labels= c("Non-Border", "Border"), 
                     values = c("blue", "red")) +
  theme(legend.position="none")

borderplots1 <- ggarrange(borderplot1, borderplot2, borderplot3, 
                         ncol=1, nrow=3) 
annotate_figure(borderplots1, #plot window needs to be rly big
                # top=text_grob("Robustness of Border State Indicator: Marginal Mean Differences", face="bold"),
                bottom=text_grob("Blue: Non-border; Red: Border"))

borderplots2 <- ggarrange(borderplot4, #borderplot5, borderplot6,
                          borderplot7,
                          ncol=1, nrow=3) 
annotate_figure(borderplots2, #plot window needs to be rly big
                # top=text_grob("Robustness of Border State Indicator: Marginal Mean Differences", face="bold"),
                bottom=text_grob("Blue: Non-border; Red: Border"))

######### Diagnostics #########
plot(cj_freqs(cjt_ger_data, ger_mms, id = ~Response.ID), legend_pos = "none", ylab = "Frequency of Level in Conjoint Design (GER)") 

plot(cj(cjt_ger_data, ger_mms, id = ~Response.ID, by = ~profile, estimate = "mm"), group = "profile", vline = 0.5, legend_pos = "none", xlab= "Marginal Mean, Left vs Right Profile (GER)")

######### Main Figure #########
coef.vec <- c(0.035, 0.081, 0.037, 0.086, 0.040, 0.060, 0.076, 0.162)
se.vec <- c(0.012, 0.012, 0.012, 0.012, 0.011, 0.012, 0.012, 0.013)

#excl all international profiles
coef.vec.internal <- c(0.049, 0.085, 0.080, 0.13, 0.059, 0.072, 0.075, 0.150)
se.vec.internal <- c(0.025, 0.031, 0.025, 0.029, 0.024, 0.029, 0.025, 0.029)

#excl all internal profiles
coef.vec.international <- c(0.032, 0.080, 0.026, 0.079, 0.035, 0.057, 0.076, 0.164)
se.vec.international <- c(0.013, 0.013, 0.013, 0.013, 0.013, 0.013, 0.013, 0.014)

ymin <- coef.vec-qnorm(.95)*se.vec
ymax <- coef.vec+qnorm(.95)*se.vec

ymin.internal <- coef.vec.internal-qnorm(.95)*se.vec.internal
ymax.internal <- coef.vec.internal+qnorm(.95)*se.vec.internal

ymin.international <- coef.vec.international-qnorm(.95)*se.vec.international
ymax.international <- coef.vec.international+qnorm(.95)*se.vec.international

var.names <- c("Drought, U.S.", "Drought, Ger.", "Flooding, U.S.", "Flooding, Ger.", "Wildfires, U.S.", "Wildfires, Ger.", "Persecution, U.S.", "Persecution, Ger.")
reasons <- c("Drought", "Drought", "Flooding", "Flooding", "Wildfires", "Wildfires", "Persecution", "Persecution")
Country <- rep(c("US", "Ger"), 4)

reason_data <- data.frame(coef.vec, se.vec, var.names, reasons, Country, ymin, ymax,
                          coef.vec.internal, se.vec.internal, ymin.internal, ymax.internal,
                          coef.vec.international, se.vec.international, 
                          ymin.international, ymax.international)
reason_data$Country <- relevel(reason_data$Country, "US")
reason_data$reasons <- factor(reason_data$reasons, levels = c("Persecution", "Wildfires", "Flooding", "Drought"))


all_profs <- ggplot(data=reason_data, mapping = aes(y=coef.vec, x=reasons, colour=Country, shape=Country))+
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed")+
  scale_y_continuous(limits=c(-.02, .2),
                     breaks=seq(-.02, .2, 0.02), 
                     labels = c("", 0,"", .04,"", .08,"", .12,"", .16,"", .2))+
  scale_colour_grey(start=0, end=.61)+ 
  geom_pointrange(mapping = aes(ymin=ymin, ymax=ymax))+
  coord_flip()+
  theme_classic() +
  theme(legend.position="None", axis.ticks.length=unit(.25, "cm"), 
        axis.title.x = element_text(vjust=-0.5),
        axis.title.y = element_text(vjust=3),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "All Profiles", 
       # caption="", 
       x="", y=""
       # y="Average Marginal Component Effect (AMCE)",
       # x="Reason for Migration \n (Baseline = Economic Opportunity)"
  )

internal_profs <- ggplot(data=reason_data, mapping = aes(y=coef.vec.internal, x=reasons, colour=Country, shape=Country))+
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed")+
  scale_y_continuous(limits=c(-.02, .2),
                     breaks=seq(-.02, .2, 0.02), 
                     labels = c("", 0,"", .04,"", .08,"", .12,"", .16,"", .2))+
  scale_colour_grey(start=0, end=.61)+ 
  geom_pointrange(mapping = aes(ymin=ymin.internal, ymax=ymax.internal))+
  coord_flip()+
  theme_classic() +
  theme(legend.position="None", axis.ticks.length=unit(.25, "cm"), 
        axis.title.x = element_text(vjust=-0.5),
        axis.title.y = element_text(vjust=3),
        plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Internal Profiles",
    # caption="", 
    x="", y=""
    # y="Average Marginal Component Effect (AMCE)",
    # x="Reason for Migration \n (Baseline = Economic Opportunity)"
  )

international_profs <- ggplot(data=reason_data, mapping = aes(y=coef.vec.international, x=reasons, colour=Country, shape=Country))+
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed")+
  scale_y_continuous(limits=c(-.02, .2),
                     breaks=seq(-.02, .2, 0.02), 
                     labels = c("", 0,"", .04,"", .08,"", .12,"", .16,"", .2))+
  scale_colour_grey(start=0, end=.61)+ 
  geom_pointrange(mapping = aes(ymin=ymin.international, ymax=ymax.international))+
  coord_flip()+
  theme_classic() +
  theme(legend.position="None", axis.ticks.length=unit(.25, "cm"), 
        axis.title.x = element_text(vjust=-0.5),
        axis.title.y = element_text(vjust=3),
        plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "International Profiles",
    # caption="",
    x="", y=""
    # y="Average Marginal Component Effect (AMCE)",
    # x="Reason for Migration \n (Baseline = Economic Opportunity)"
  )

reasons_plots <- ggarrange(all_profs, international_profs, internal_profs, 
                           ncol=3, nrow=1) 

annotate_figure(reasons_plots,
                bottom=text_grob("Average Marginal Component Effect (AMCE)")
                # left=text_grob("Reason for Migration \n (Baseline = Economic Opportunity)")
)


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
migration <- data.frame(us_article[,c("MIGRATION_1", "MIGRATION_2", "MIGRATION_3", "MIGRATION_4", "MIGRATION_5", "MIGRATION_6")])
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

#Unweighted interactions
lm_climate_emp <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+AGE+
                       fp_orientation_index+soc_dom_index+empathy_index+
                       NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                       RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                       border_state_indicator +urban_indicator+
                       TREATMENT*empathy_index,data=us_article)
lm_climate_migration_emp <- lm(climate_migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                                 fp_orientation_index+soc_dom_index+empathy_index+
                                 NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                 RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                 border_state_indicator+urban_indicator+
                                 TREATMENT*empathy_index,data=us_article)
lm_migration_emp <- lm(migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                         fp_orientation_index+soc_dom_index+empathy_index+
                         NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                         RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                         border_state_indicator+urban_indicator+
                         TREATMENT*empathy_index, data=us_article)


stargazer(lm_climate_emp, lm_migration_emp, lm_climate_migration_emp,
          header=FALSE, 
          title = "Issue Importance: US Sample, Unweighted, Interaction of Treatment with Empathy",
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
                               "Urban",
                               "Treat: US Migration*Empathy", "Treat: Word Migration*Empathy",
                               "Treat: US Climate*Empathy","Treat: World Climate*Empathy", 
                               "Treat: US Climate Migration*Empathy", "Treat: World Climate Migration*Empathy"))

lm_climate_border <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+AGE+
                          fp_orientation_index+soc_dom_index+empathy_index+
                          NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                          RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                          border_state_indicator +urban_indicator+
                          TREATMENT*border_state_indicator,data=us_article)
lm_climate_migration_border <- lm(climate_migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                                    fp_orientation_index+soc_dom_index+empathy_index+
                                    NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                    RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                    border_state_indicator+urban_indicator+
                                    TREATMENT*border_state_indicator,data=us_article)
lm_migration_border <- lm(migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                            fp_orientation_index+soc_dom_index+empathy_index+
                            NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                            RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                            border_state_indicator+urban_indicator+
                            TREATMENT*border_state_indicator, data=us_article)


stargazer(lm_climate_border, lm_migration_border, lm_climate_migration_border,
          header=FALSE, 
          title = "Issue Importance: US Sample, Unweighted, Interaction of Treatment with Border State",
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
                               "Urban",
                               "Treat: US Migration*Border State", "Treat: Word Migration*Border State",
                               "Treat: US Climate*Border State","Treat: World Climate*Border State", 
                               "Treat: US Climate Migration*Border State", "Treat: World Climate Migration*Border State"))


lm_climate_native <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+AGE+
                          fp_orientation_index+soc_dom_index+empathy_index+
                          NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                          RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                          border_state_indicator +urban_indicator+
                          TREATMENT*NATIVE_BORN_num,data=us_article)
lm_climate_migration_native <- lm(climate_migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                                    fp_orientation_index+soc_dom_index+empathy_index+
                                    NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                    RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                    border_state_indicator+urban_indicator+
                                    TREATMENT*NATIVE_BORN_num,data=us_article)
lm_migration_native <- lm(migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                            fp_orientation_index+soc_dom_index+empathy_index+
                            NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                            RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                            border_state_indicator+urban_indicator+
                            TREATMENT*NATIVE_BORN_num, data=us_article)


stargazer(lm_climate_native, lm_migration_native, lm_climate_migration_native,
          header=FALSE, 
          title = "Issue Importance: US Sample, Unweighted, Interaction of Treatment with Native Born",
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
                               "Urban",
                               "Treat: US Migration*Native Born", "Treat: Word Migration*Native Born",
                               "Treat: US Climate*Native Born","Treat: World Climate*Native Born", 
                               "Treat: US Climate Migration*Native Born", "Treat: World Climate Migration*Native Born"))

lm_climate_part <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+AGE+
                        fp_orientation_index+soc_dom_index+empathy_index+
                        NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                        RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                        border_state_indicator +urban_indicator+
                        TREATMENT*PARTISANSHIP6,data=us_article)
lm_climate_migration_part <- lm(climate_migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                                  fp_orientation_index+soc_dom_index+empathy_index+
                                  NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                  RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                  border_state_indicator+urban_indicator+
                                  TREATMENT*PARTISANSHIP6,data=us_article)
lm_migration_part <- lm(migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                          fp_orientation_index+soc_dom_index+empathy_index+
                          NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                          RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                          border_state_indicator+urban_indicator+
                          TREATMENT*PARTISANSHIP6, data=us_article)


stargazer(lm_climate_part, lm_migration_part, lm_climate_migration_part,
          header=FALSE, 
          title = "Issue Importance: US Sample, Unweighted, Interaction of Treatment with Partisanship",
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
                               "Urban",
                               "Treat: US Migration*Partisanship", "Treat: Word Migration*Partisanship",
                               "Treat: US Climate*Partisanship","Treat: World Climate*Partisanship", 
                               "Treat: US Climate Migration*Partisanship", "Treat: World Climate Migration*Partisanship"))

lm_climate_age <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+AGE+
                       fp_orientation_index+soc_dom_index+empathy_index+
                       NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                       RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                       border_state_indicator +urban_indicator+
                       TREATMENT*AGE,data=us_article)
lm_climate_migration_age <- lm(climate_migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                                 fp_orientation_index+soc_dom_index+empathy_index+
                                 NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                 RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                 border_state_indicator+urban_indicator+
                                 TREATMENT*AGE,data=us_article)
lm_migration_age <- lm(migration_index ~ TREATMENT+PARTISANSHIP6+AGE+
                         fp_orientation_index+soc_dom_index+empathy_index+
                         NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                         RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                         border_state_indicator+urban_indicator+
                         TREATMENT*AGE, data=us_article)


stargazer(lm_climate_age, lm_migration_age, lm_climate_migration_age,
          header=FALSE, 
          title = "Issue Importance: US Sample, Unweighted, Interaction of Treatment with Age",
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
                               "Urban",
                               "Treat: US Migration*Age", "Treat: Word Migration*Age",
                               "Treat: US Climate*Age","Treat: World Climate*Age", 
                               "Treat: US Climate Migration*Age", "Treat: World Climate Migration*Age"))

#Weighted
wt_lm_climate <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+
                      # Age1825+ reference category	
                      Age2634+	Age3554+	Age5564+	Age65+
                      fp_orientation_index+soc_dom_index+empathy_index+
                      NATIVE_BORN_num+GENDER_num+
                      # HighSchool+	reference category
                      SomeCollege+	Bachelor+	PostBachelor+
                      +IDEOLOGY_num+
                      RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                      border_state_indicator +urban_indicator,data=us_article, weights = wt)

wt_lm_climate_migration <- lm(climate_migration_index ~  TREATMENT+PARTISANSHIP6+
                                # Age1825+ reference category	
                                Age2634+	Age3554+	Age5564+	Age65+
                                fp_orientation_index+soc_dom_index+empathy_index+
                                NATIVE_BORN_num+GENDER_num+
                                # HighSchool+	reference category
                                SomeCollege+	Bachelor+	PostBachelor+
                                +IDEOLOGY_num+
                                RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                border_state_indicator +urban_indicator,data=us_article, weights = wt)

wt_lm_migration <- lm(migration_index ~  TREATMENT+PARTISANSHIP6+
                        # Age1825+ reference category	
                        Age2634+	Age3554+	Age5564+	Age65+
                        fp_orientation_index+soc_dom_index+empathy_index+
                        NATIVE_BORN_num+GENDER_num+
                        # HighSchool+	reference category
                        SomeCollege+	Bachelor+	PostBachelor+
                        +IDEOLOGY_num+
                        RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                        border_state_indicator +urban_indicator,data=us_article, weights = wt)

stargazer(wt_lm_climate, wt_lm_migration, wt_lm_climate_migration,
          header=FALSE, 
          title = "Issue Importance: US Sample, Weighted",
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
          notes = "Omitted reference categories are 18-25 for age and high school for education.",
          notes.append = T,
          covariate.labels = c("Treat: US Migration", "Treat: Word Migration", "Treat: US Climate",
                               "Treat: World Climate", "Treat: US Climate Migration", 
                               "Treat: World Climate Migration",
                               "Partisanship",
                               "Age Bin: 26-34", "Age Bin: 35-54", "Age Bin: 55-64", "Age Bin: 65+",
                               "Foreign Policy Orientation",
                               "Social Dominance",
                               "Empathy",
                               "Native Born",
                               "Gender",
                               "Ed. Bin: Some College", "Ed. Bin: Bachelor", "Ed. Bin: Post Bachelor",
                               "Ideology",
                               "Religiosity",
                               "Trust in Government",
                               "Political Interest",
                               "Employment Status",
                               "Border State",
                               "Urban"))


#weighted interactions
wt_lm_climate_emp <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+
                          # Age1825+ reference category	
                          Age2634+	Age3554+	Age5564+	Age65+
                          fp_orientation_index+soc_dom_index+empathy_index+
                          NATIVE_BORN_num+GENDER_num+
                          # HighSchool+	reference category
                          SomeCollege+	Bachelor+	PostBachelor+
                          +IDEOLOGY_num+
                          RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                          border_state_indicator +urban_indicator+
                          TREATMENT*empathy_index,
                        data=us_article, weights = wt)

wt_lm_climate_migration_emp <- lm(climate_migration_index ~  TREATMENT+PARTISANSHIP6+
                                    # Age1825+ reference category	
                                    Age2634+	Age3554+	Age5564+	Age65+
                                    fp_orientation_index+soc_dom_index+empathy_index+
                                    NATIVE_BORN_num+GENDER_num+
                                    # HighSchool+	reference category
                                    SomeCollege+	Bachelor+	PostBachelor+
                                    +IDEOLOGY_num+
                                    RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                    border_state_indicator +urban_indicator+
                                    TREATMENT*empathy_index,
                                  data=us_article, weights = wt)

wt_lm_migration_emp <- lm(migration_index ~  TREATMENT+PARTISANSHIP6+
                            # Age1825+ reference category	
                            Age2634+	Age3554+	Age5564+	Age65+
                            fp_orientation_index+soc_dom_index+empathy_index+
                            NATIVE_BORN_num+GENDER_num+
                            # HighSchool+	reference category
                            SomeCollege+	Bachelor+	PostBachelor+
                            +IDEOLOGY_num+
                            RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                            border_state_indicator +urban_indicator+
                            TREATMENT*empathy_index,
                          data=us_article, weights = wt)

stargazer(wt_lm_climate_emp, wt_lm_migration_emp, wt_lm_climate_migration_emp,
          header=FALSE, 
          title = "Issue Importance: US Sample, Weighted, Interaction of Treatment with Empathy",
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
          notes = "Omitted reference categories are 18-25 for age and high school for education.",
          notes.append = T,
          covariate.labels = c("Treat: US Migration", "Treat: Word Migration", "Treat: US Climate",
                               "Treat: World Climate", "Treat: US Climate Migration", 
                               "Treat: World Climate Migration",
                               "Partisanship",
                               "Age Bin: 26-34", "Age Bin: 35-54", "Age Bin: 55-64", "Age Bin: 65+",
                               "Foreign Policy Orientation",
                               "Social Dominance",
                               "Empathy",
                               "Native Born",
                               "Gender",
                               "Ed. Bin: Some College", "Ed. Bin: Bachelor", "Ed. Bin: Post Bachelor",
                               "Ideology",
                               "Religiosity",
                               "Trust in Government",
                               "Political Interest",
                               "Employment Status",
                               "Border State",
                               "Urban",
                               "Treat: US Migration*Empathy", "Treat: Word Migration*Empathy",
                               "Treat: US Climate*Empathy","Treat: World Climate*Empathy", 
                               "Treat: US Climate Migration*Empathy", "Treat: World Climate Migration*Empathy"))


wt_lm_climate_border <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+
                             # Age1825+ reference category	
                             Age2634+	Age3554+	Age5564+	Age65+
                             fp_orientation_index+soc_dom_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+
                             # HighSchool+	reference category
                             SomeCollege+	Bachelor+	PostBachelor+
                             +IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                             border_state_indicator +urban_indicator+
                             TREATMENT*border_state_indicator,
                           data=us_article, weights = wt)

wt_lm_climate_migration_border <- lm(climate_migration_index ~  TREATMENT+PARTISANSHIP6+
                                       # Age1825+ reference category	
                                       Age2634+	Age3554+	Age5564+	Age65+
                                       fp_orientation_index+soc_dom_index+empathy_index+
                                       NATIVE_BORN_num+GENDER_num+
                                       # HighSchool+	reference category
                                       SomeCollege+	Bachelor+	PostBachelor+
                                       +IDEOLOGY_num+
                                       RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                       border_state_indicator +urban_indicator+
                                       TREATMENT*border_state_indicator,
                                     data=us_article, weights = wt)

wt_lm_migration_border <- lm(migration_index ~  TREATMENT+PARTISANSHIP6+
                               # Age1825+ reference category	
                               Age2634+	Age3554+	Age5564+	Age65+
                               fp_orientation_index+soc_dom_index+empathy_index+
                               NATIVE_BORN_num+GENDER_num+
                               # HighSchool+	reference category
                               SomeCollege+	Bachelor+	PostBachelor+
                               +IDEOLOGY_num+
                               RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                               border_state_indicator +urban_indicator+
                               TREATMENT*border_state_indicator,
                             data=us_article, weights = wt)

stargazer(wt_lm_climate_border, wt_lm_migration_border, wt_lm_climate_migration_border,
          header=FALSE, 
          title = "Issue Importance: US Sample, Weighted, Interaction of Treatment with Border State",
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
          notes = "Omitted reference categories are 18-25 for age and high school for education.",
          notes.append = T,
          covariate.labels = c("Treat: US Migration", "Treat: Word Migration", "Treat: US Climate",
                               "Treat: World Climate", "Treat: US Climate Migration", 
                               "Treat: World Climate Migration",
                               "Partisanship",
                               "Age Bin: 26-34", "Age Bin: 35-54", "Age Bin: 55-64", "Age Bin: 65+",
                               "Foreign Policy Orientation",
                               "Social Dominance",
                               "Empathy",
                               "Native Born",
                               "Gender",
                               "Ed. Bin: Some College", "Ed. Bin: Bachelor", "Ed. Bin: Post Bachelor",
                               "Ideology",
                               "Religiosity",
                               "Trust in Government",
                               "Political Interest",
                               "Employment Status",
                               "Border State",
                               "Urban",
                               "Treat: US Migration*Border State", "Treat: Word Migration*Border State",
                               "Treat: US Climate*Empathy","Border State: World Climate*Border State", 
                               "Treat: US Climate Migration*Border State", "Treat: World Climate Migration*Border State"))


wt_lm_climate_native <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+
                             # Age1825+ reference category	
                             Age2634+	Age3554+	Age5564+	Age65+
                             fp_orientation_index+soc_dom_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+
                             # HighSchool+	reference category
                             SomeCollege+	Bachelor+	PostBachelor+
                             +IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                             border_state_indicator +urban_indicator+
                             TREATMENT*NATIVE_BORN_num,
                           data=us_article, weights = wt)

wt_lm_climate_migration_native <- lm(climate_migration_index ~  TREATMENT+PARTISANSHIP6+
                                       # Age1825+ reference category	
                                       Age2634+	Age3554+	Age5564+	Age65+
                                       fp_orientation_index+soc_dom_index+empathy_index+
                                       NATIVE_BORN_num+GENDER_num+
                                       # HighSchool+	reference category
                                       SomeCollege+	Bachelor+	PostBachelor+
                                       +IDEOLOGY_num+
                                       RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                       border_state_indicator +urban_indicator+
                                       TREATMENT*NATIVE_BORN_num,
                                     data=us_article, weights = wt)

wt_lm_migration_native <- lm(migration_index ~  TREATMENT+PARTISANSHIP6+
                               # Age1825+ reference category	
                               Age2634+	Age3554+	Age5564+	Age65+
                               fp_orientation_index+soc_dom_index+empathy_index+
                               NATIVE_BORN_num+GENDER_num+
                               # HighSchool+	reference category
                               SomeCollege+	Bachelor+	PostBachelor+
                               +IDEOLOGY_num+
                               RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                               border_state_indicator +urban_indicator+
                               TREATMENT*NATIVE_BORN_num,
                             data=us_article, weights = wt)

stargazer(wt_lm_climate_native, wt_lm_migration_native, wt_lm_climate_migration_native,
          header=FALSE, 
          title = "Issue Importance: US Sample, Weighted, Interaction of Treatment with Native Born",
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
          notes = "Omitted reference categories are 18-25 for age and high school for education.",
          notes.append = T,
          covariate.labels = c("Treat: US Migration", "Treat: Word Migration", "Treat: US Climate",
                               "Treat: World Climate", "Treat: US Climate Migration", 
                               "Treat: World Climate Migration",
                               "Partisanship",
                               "Age Bin: 26-34", "Age Bin: 35-54", "Age Bin: 55-64", "Age Bin: 65+",
                               "Foreign Policy Orientation",
                               "Social Dominance",
                               "Empathy",
                               "Native Born",
                               "Gender",
                               "Ed. Bin: Some College", "Ed. Bin: Bachelor", "Ed. Bin: Post Bachelor",
                               "Ideology",
                               "Religiosity",
                               "Trust in Government",
                               "Political Interest",
                               "Employment Status",
                               "Border State",
                               "Urban",
                               "Treat: US Migration*Native Born", "Treat: Word Migration*Native Born",
                               "Treat: US Climate*Native Born","Treat: World Climate*Native Born", 
                               "Treat: US Climate Migration*Native Born", "Treat: World Climate Migration*Native Born"))


wt_lm_climate_part <- lm(climate_index ~ TREATMENT+PARTISANSHIP6+
                           # Age1825+ reference category	
                           Age2634+	Age3554+	Age5564+	Age65+
                           fp_orientation_index+soc_dom_index+empathy_index+
                           NATIVE_BORN_num+GENDER_num+
                           # HighSchool+	reference category
                           SomeCollege+	Bachelor+	PostBachelor+
                           +IDEOLOGY_num+
                           RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                           border_state_indicator +urban_indicator+
                           TREATMENT*PARTISANSHIP6,
                         data=us_article, weights = wt)

wt_lm_climate_migration_part <- lm(climate_migration_index ~  TREATMENT+PARTISANSHIP6+
                                     # Age1825+ reference category	
                                     Age2634+	Age3554+	Age5564+	Age65+
                                     fp_orientation_index+soc_dom_index+empathy_index+
                                     NATIVE_BORN_num+GENDER_num+
                                     # HighSchool+	reference category
                                     SomeCollege+	Bachelor+	PostBachelor+
                                     +IDEOLOGY_num+
                                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                                     border_state_indicator +urban_indicator+
                                     TREATMENT*PARTISANSHIP6,
                                   data=us_article, weights = wt)

wt_lm_migration_part <- lm(migration_index ~  TREATMENT+PARTISANSHIP6+
                             # Age1825+ reference category	
                             Age2634+	Age3554+	Age5564+	Age65+
                             fp_orientation_index+soc_dom_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+
                             # HighSchool+	reference category
                             SomeCollege+	Bachelor+	PostBachelor+
                             +IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+
                             border_state_indicator +urban_indicator+
                             TREATMENT*PARTISANSHIP6,
                           data=us_article, weights = wt)

stargazer(wt_lm_climate_part, wt_lm_migration_part, wt_lm_climate_migration_part,
          header=FALSE, 
          title = "Issue Importance: US Sample, Weighted, Interaction of Treatment with Partisanship",
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
          notes = "Omitted reference categories are 18-25 for age and high school for education.",
          notes.append = T,
          covariate.labels = c("Treat: US Migration", "Treat: Word Migration", "Treat: US Climate",
                               "Treat: World Climate", "Treat: US Climate Migration", 
                               "Treat: World Climate Migration",
                               "Partisanship",
                               "Age Bin: 26-34", "Age Bin: 35-54", "Age Bin: 55-64", "Age Bin: 65+",
                               "Foreign Policy Orientation",
                               "Social Dominance",
                               "Empathy",
                               "Native Born",
                               "Gender",
                               "Ed. Bin: Some College", "Ed. Bin: Bachelor", "Ed. Bin: Post Bachelor",
                               "Ideology",
                               "Religiosity",
                               "Trust in Government",
                               "Political Interest",
                               "Employment Status",
                               "Border State",
                               "Urban",
                               "Treat: US Migration*Partisanship", "Treat: Word Migration*Partisanship",
                               "Treat: US Climate*Partisanship","Treat: World Climate*Partisanship", 
                               "Treat: US Climate Migration*Partisanship", "Treat: World Climate Migration*Partisanship"))


######### Marginal Effects ######### 
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

us_climate_margins <- margins(lm_climate, variables = "empathy_index")
us_migration_margins <- margins(lm_climate_migration, variables = "empathy_index")
us_climate_migration_margins <- margins(lm_migration, variables = "empathy_index")
us_climate_margins_data <- as_tibble(summary(us_climate_margins))
us_climate_margins_data$outcome <- "Climate"
us_migration_margins_data <- as.tibble(summary(us_migration_margins))
us_migration_margins_data$outcome <- "Migration"
us_climate_migration_data <- as.tibble(summary(us_climate_migration_margins))
us_climate_migration_data$outcome <- "Climate Migration"
us_margins_data <- rbind(us_climate_margins_data, us_migration_margins_data, us_climate_migration_data) %>% 
  dplyr::select(AME, lower, upper, outcome)
us_margins_data$country <- "US"

us_margins <- ggplot(data= us_margins_data, aes(x=outcome, y=AME, ymin=lower, ymax=upper)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_pointrange() + coord_flip() +
  labs(x="", y="Average Marginal Effect of Empathy, US")

######### Subset to Republicans ######### 
us_article_repubs <- us_article[us_article$PARTISANSHIP_bin %in% 'R', ]

outcomes <- c("climate_index", "migration_index", "climate_migration_index")
tmat_repubs <- data.frame(matrix(NA,length(outcomes)*length(treats), 11))
colnames(tmat_repubs) <- c("treat_condition", "treat_id", "outcome", "t_pval", 
                           "control_mean", "treat_mean", "mean_diff", "ci_low", "ci_high",
                           "Location", "Prime")  


k <- c()
for(i in 1:length(outcomes)){
  a <- rep(outcomes[i], 6)
  k <- append(x = k, values = a)}

tmat_repubs[, 3] <- k
tmat_repubs[, 1] <- rep((1:6), 3)
tmat_repubs[, 2] <- rep(treats, 3)
tmat_repubs[, 10] <- rep(c("US", "World"), 9)
tmat_repubs[, 11] <- rep(c("Migration","Migration", "Climate",  "Climate","Climate Migration","Climate Migration"), 3)

counter <- 0
for (i in 1:length(outcomes)){
  for (j in 1:length(treat_conditions)){
    string <- paste('t_test <- t.test(us_article_repubs$', outcomes[i],
                    '[us_article_repubs$TREATMENT==', treat_conditions[j], '], us_article_repubs$', 
                    outcomes[i], '[us_article_repubs$TREATMENT=="Control"])', 
                    sep = "", collapse = "")
    eval(parse(text=string))
    tmat_repubs[(counter+j),4] <- round(t_test$p.value, digits = 3)
    tmat_repubs[(counter+j),5] <- round(t_test$estimate[2], digits = 3)
    tmat_repubs[(counter+j),6] <- round(t_test$estimate[1], digits = 3)
    tmat_repubs[(counter+j),7] <- tmat_repubs[(counter+j),6]-tmat_repubs[(counter+j),5]
    tmat_repubs[(counter+j),8] <- round(t_test$conf.int[1], digits = 3)
    tmat_repubs[(counter+j),9] <- round(t_test$conf.int[2], digits = 3)
  }
  counter <- counter+6
}

tmat_repubs[tmat_repubs$t_pval<.1, ]

######### Manipulation Checks ######### 
treats <- c("US Migration", "World Migration", "US Climate", "World Climate", "US Climate Migration", "World Climate Migration")

us_article$localization <- "US"
us_article$localization[us_article$TREATMENT %in% c(2, 4, 6)] <- "World" 

us_article$threat <- "Soccer"
us_article$threat[us_article$TREATMENT %in% c(1, 2)] <- "Migration" 
us_article$threat[us_article$TREATMENT %in% c(3, 4)] <- "Climate" 
us_article$threat[us_article$TREATMENT %in% c(5, 6)] <- "Climate Migration" 

#attention checks
table(us_article$threat, us_article$MANIP_CHECK_1)
table(us_article$localization, us_article$MANIP_CHECK_2)

# par(mfcol=c(2, 2))
# hist(us_article$manip_checks_4[us_article$threat == "Soccer"], main = "Soccer", xlab = "", ylab= "")
# hist(us_article$manip_checks_4[us_article$threat == "Migration"], main = "Migration", xlab = "", ylab= "")
# hist(us_article$manip_checks_4[us_article$threat == "Climate"], main = "Climate", xlab = "", ylab= "")
# hist(us_article$manip_checks_4[us_article$threat == "Climate Migration"], main = "Climate Migration", xlab = "", ylab= "")

us_article <- us_article %>% 
  dplyr::rename(
    manip_migration = manip_checks_1,
    manip_climate = manip_checks_4,
    manip_data_privacy = manip_checks_5,
    manip_climate_migration = manip_checks_6
  )


mean(na.omit(us_article$manip_migration[us_article$threat == "Migration"])) - mean(na.omit(us_article$manip_migration[us_article$threat == "Soccer"]))

manip_migration <- t.test(na.omit(us_article$manip_migration[us_article$threat == "Migration"]), 
                          na.omit(na.omit(us_article$manip_migration[us_article$threat == "Soccer"])))

mean(na.omit(us_article$manip_climate[us_article$threat == "Climate"])) - mean(na.omit(us_article$manip_climate[us_article$threat == "Soccer"]))

manip_climate <- t.test(na.omit(us_article$manip_climate[us_article$threat == "Climate"]), 
                        na.omit(us_article$manip_climate[us_article$threat == "Soccer"]))

mean(na.omit(us_article$manip_climate_migration[us_article$threat == "Climate Migration"])) - mean(na.omit(us_article$manip_climate_migration[us_article$threat == "Soccer"]))

manip_climate_migration <- t.test(na.omit(us_article$manip_climate_migration[us_article$threat == "Climate Migration"]), 
                                  na.omit(us_article$manip_climate_migration[us_article$threat == "Soccer"]))

manipmat <- data.frame(matrix(NA, 3, 4))
colnames(manipmat) <- c( "Treatment", "T-Test P val.", 
                         "Ctrl. Mean", "Treatment Mean") 

manipmat[, 1] <- c("Migration", "Climate", "Climate Migration")
manipmat[, 2] <- c(round(manip_migration$p.value, digits = 3),
                   round(manip_climate$p.value, digits = 3),
                   round(manip_climate_migration$p.value, digits = 3))
manipmat[, 3] <- c(round(manip_migration$estimate[2], digits = 3),
                   round(manip_climate$estimate[2], digits = 3),
                   round(manip_climate_migration$estimate[2], digits = 3))
manipmat[, 4] <- c(round(manip_migration$estimate[1], digits = 3),
                   round(manip_climate$estimate[1], digits = 3),
                   round(manip_climate_migration$estimate[1], digits = 3))

xtable(manipmat, 
       font.size = "small", caption = "Experiment 2 Manipulation Checks, US Sample")

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
migration_ger <- data.frame(ger_article[,c("MIGRATION_1", "MIGRATION_2", "MIGRATION_3", "MIGRATION_4", "MIGRATION_5", "MIGRATION_6")])
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

lm2 <- lm(MIG_LEVELS ~ TREATMENT+AGE+
            fp_orientation_index+empathy_index+
            NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
            RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
            east_indicator + urban_indicator,
          data=ger_article)

#interactions
lm_climate_emp <- lm(climate_index ~ TREATMENT+AGE+
                       fp_orientation_index+empathy_index+
                       NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                       RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                       east_indicator + urban_indicator+
                       TREATMENT*empathy_index,
                     data=ger_article)


lm_climate_migration_emp <- lm(climate_migration_index ~ TREATMENT+AGE+
                                 fp_orientation_index+empathy_index+
                                 NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                 RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                 east_indicator + urban_indicator+
                                 TREATMENT*empathy_index,
                               data=ger_article)
lm_migration_emp <- lm(migration_index ~ TREATMENT+AGE+
                         fp_orientation_index+empathy_index+
                         NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                         RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                         east_indicator + urban_indicator+
                         TREATMENT*empathy_index,
                       data=ger_article)

stargazer(lm_climate_emp, lm_migration_emp, lm_climate_migration_emp,
          header=FALSE, 
          title = "Issue Importance: German Sample, Unweighted, Interaction of Treatment with Empathy",
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
                               "Urban",
                               "Treat: GER Migration*Empathy", "Treat: Word Migration*Empathy",
                               "Treat: GER Climate*Empathy","Treat: World Climate*Empathy", 
                               "Treat: GER Climate Migration*Empathy", "Treat: World Climate Migration*Empathy"))

lm_climate_east <- lm(climate_index ~ TREATMENT+AGE+
                        fp_orientation_index+empathy_index+
                        NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                        RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                        east_indicator + urban_indicator+
                        TREATMENT*east_indicator,
                      data=ger_article)
lm_climate_migration_east <- lm(climate_migration_index ~ TREATMENT+AGE+
                                  fp_orientation_index+empathy_index+
                                  NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                  RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                  east_indicator + urban_indicator+
                                  TREATMENT*east_indicator,
                                data=ger_article)
lm_migration_east <- lm(migration_index ~ TREATMENT+AGE+
                          fp_orientation_index+empathy_index+
                          NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                          RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                          east_indicator + urban_indicator+
                          TREATMENT*east_indicator,
                        data=ger_article)

stargazer(lm_climate_east, lm_migration_east, lm_climate_migration_east,
          header=FALSE, 
          title = "Issue Importance: German Sample, Unweighted, Interaction of Treatment with Eastern State",
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
                               "Urban",
                               "Treat: GER Migration*East", "Treat: Word Migration*East",
                               "Treat: GER Climate*East","Treat: World Climate*East", 
                               "Treat: GER Climate Migration*East", "Treat: World Climate Migration*East"))



lm_climate_native <- lm(climate_index ~ TREATMENT+AGE+
                          fp_orientation_index+empathy_index+
                          NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                          RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                          east_indicator + urban_indicator+
                          TREATMENT*NATIVE_BORN_num,
                        data=ger_article)
lm_climate_migration_native <- lm(climate_migration_index ~ TREATMENT+AGE+
                                    fp_orientation_index+empathy_index+
                                    NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                    RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                    east_indicator + urban_indicator+
                                    TREATMENT*NATIVE_BORN_num,
                                  data=ger_article)
lm_migration_native <- lm(migration_index ~ TREATMENT+AGE+
                            fp_orientation_index+empathy_index+
                            NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                            RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                            east_indicator + urban_indicator+
                            TREATMENT*NATIVE_BORN_num,
                          data=ger_article)

stargazer(lm_climate_native, lm_migration_native, lm_climate_migration_native,
          header=FALSE, 
          title = "Issue Importance: German Sample, Unweighted, Interaction of Treatment with Native Born",
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
                               "Urban",
                               "Treat: GER Migration*Native Born", "Treat: Word Migration*Native Born",
                               "Treat: GER Climate*Native Born","Treat: World Climate*Native Born", 
                               "Treat: GER Climate Migration*Native Born", "Treat: World Climate Migration*Native Born"))


lm_climate_age <- lm(climate_index ~ TREATMENT+AGE+
                       fp_orientation_index+empathy_index+
                       NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                       RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                       east_indicator + urban_indicator+
                       TREATMENT*AGE,
                     data=ger_article)
lm_climate_migration_age <- lm(climate_migration_index ~ TREATMENT+AGE+
                                 fp_orientation_index+empathy_index+
                                 NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                 RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                 east_indicator + urban_indicator+
                                 TREATMENT*AGE,
                               data=ger_article)
lm_migration_age <- lm(migration_index ~ TREATMENT+AGE+
                         fp_orientation_index+empathy_index+
                         NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                         RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                         east_indicator + urban_indicator+
                         TREATMENT*AGE,
                       data=ger_article)

stargazer(lm_climate_age, lm_migration_age, lm_climate_migration_age,
          header=FALSE, 
          title = "Issue Importance: German Sample, Unweighted, Interaction of Treatment with Age",
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
                               "Urban",
                               "Treat: GER Migration*Age", "Treat: Word Migration*Age",
                               "Treat: GER Climate*Age","Treat: World Climate*Age", 
                               "Treat: GER Climate Migration*Age", "Treat: World Climate Migration*Age"))


lm_climate_border2 <- lm(climate_index ~ TREATMENT+AGE+
                           fp_orientation_index+empathy_index+
                           NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                           RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                           east_indicator2 + urban_indicator,
                         data=ger_article)
lm_climate_migration_border2 <- lm(climate_migration_index ~ TREATMENT+AGE+
                                     fp_orientation_index+empathy_index+
                                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                     east_indicator2 + urban_indicator,
                                   data=ger_article)
lm_migration_border2 <- lm(migration_index ~ TREATMENT+AGE+
                             fp_orientation_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                             east_indicator2 + urban_indicator,
                           data=ger_article)

lm_climate_border3 <- lm(climate_index ~ TREATMENT+AGE+
                           fp_orientation_index+empathy_index+
                           NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                           RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                           east_indicator3 + urban_indicator,
                         data=ger_article)
lm_climate_migration_border3 <- lm(climate_migration_index ~ TREATMENT+AGE+
                                     fp_orientation_index+empathy_index+
                                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                     east_indicator3 + urban_indicator,
                                   data=ger_article)
lm_migration_border3 <- lm(migration_index ~ TREATMENT+AGE+
                             fp_orientation_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                             east_indicator3 + urban_indicator,
                           data=ger_article)

lm_climate_border4 <- lm(climate_index ~ TREATMENT+AGE+
                           fp_orientation_index+empathy_index+
                           NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                           RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                           east_indicator4 + urban_indicator,
                         data=ger_article)
lm_climate_migration_border4 <- lm(climate_migration_index ~ TREATMENT+AGE+
                                     fp_orientation_index+empathy_index+
                                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                     east_indicator4 + urban_indicator,
                                   data=ger_article)
lm_migration_border4 <- lm(migration_index ~ TREATMENT+AGE+
                             fp_orientation_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                             east_indicator4 + urban_indicator,
                           data=ger_article)
lm_climate_border5 <- lm(climate_index ~ TREATMENT+AGE+
                           fp_orientation_index+empathy_index+
                           NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                           RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                           east_indicator5 + urban_indicator,
                         data=ger_article)
lm_climate_migration_border5 <- lm(climate_migration_index ~ TREATMENT+AGE+
                                     fp_orientation_index+empathy_index+
                                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                     east_indicator5 + urban_indicator,
                                   data=ger_article)
lm_migration_border5 <- lm(migration_index ~ TREATMENT+AGE+
                             fp_orientation_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                             east_indicator5 + urban_indicator,
                           data=ger_article)

lm_climate_border6 <- lm(climate_index ~ TREATMENT+AGE+
                           fp_orientation_index+empathy_index+
                           NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                           RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                           east_indicator6 + urban_indicator,
                         data=ger_article)
lm_climate_migration_border6 <- lm(climate_migration_index ~ TREATMENT+AGE+
                                     fp_orientation_index+empathy_index+
                                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                     east_indicator6 + urban_indicator,
                                   data=ger_article)
lm_migration_border6 <- lm(migration_index ~ TREATMENT+AGE+
                             fp_orientation_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                             east_indicator6 + urban_indicator,
                           data=ger_article)

summary(lm_climate)$coefficients[, 1] - summary(lm_climate_border6)$coefficients[, 1]
summary(lm_migration)$coefficients[, 1] - summary(lm_migration_border6)$coefficients[, 1]
summary(lm_climate_migration)$coefficients[, 1] -
  summary(lm_climate_migration_border6)$coefficients[, 1]

lm_climate_border7 <- lm(climate_index ~ TREATMENT+AGE+
                           fp_orientation_index+empathy_index+
                           NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                           RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + EMPLOYMENT_num +
                           east_indicator7 + urban_indicator,
                         data=ger_article)
lm_climate_migration_border7 <- lm(climate_migration_index ~ TREATMENT+AGE+
                                     fp_orientation_index+empathy_index+
                                     NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                     RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                                     east_indicator7 + urban_indicator,
                                   data=ger_article)
lm_migration_border7 <- lm(migration_index ~ TREATMENT+AGE+
                             fp_orientation_index+empathy_index+
                             NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                             RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ EMPLOYMENT_num +
                             east_indicator7 + urban_indicator,
                           data=ger_article)

summary(lm_climate)$coefficients[, 1] - summary(lm_climate_border7)$coefficients[, 1]
summary(lm_migration)$coefficients[, 1] - summary(lm_migration_border7)$coefficients[, 1]
summary(lm_climate_migration)$coefficients[, 1] -
  summary(lm_climate_migration_border7)$coefficients[, 1]



stargazer(lm_climate_border2, lm_climate_border3, 
          lm_climate_border4, lm_climate_border7, 
          header=FALSE, 
          title = "Issue Importance: Climate Change, German Sample, Unweighted, Alternate Specifications of Border State",
          dep.var.caption = "",
          # font.size = "tiny",
          model.numbers = F,
          dep.var.labels.include = F,
          model.names = F,
          # column.labels = c("Model 1", "Model", "Climate Migration"),
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
                               "Border State2",
                               "Border State3", 
                               "Border State4",
                               "Border State5",
                               "Urban")
)

stargazer(lm_migration_border2, lm_migration_border3, 
          lm_migration_border4, lm_migration_border7,
          header=FALSE, 
          title = "Issue Importance: Migration, German Sample, Unweighted",
          dep.var.caption = "",
          # font.size = "tiny",
          model.numbers = F,
          dep.var.labels.include = F,
          model.names = F,
          # column.labels = c("Model 1", "Model", "Climate Migration"),
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
                               "Border State2", "Border State3",
                               "Border State4", "Border State5",
                               "Urban")
)


stargazer(lm_climate_migration_border2, lm_climate_migration_border3,
          lm_climate_migration_border4, lm_climate_migration_border7,
          header=FALSE, 
          title = "Issue Importance: Climate Migration, German Sample, Unweighted",
          dep.var.caption = "",
          # font.size = "tiny",
          model.numbers = F,
          dep.var.labels.include = F,
          model.names = F,
          # column.labels = c("Model 1", "Model", "Climate Migration"),
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
                               "Border State2", "Border State3",
                               "Border State4", "Border State5",
                               "Urban")
)

######### Marginal Effects ######### 
lm_climate_ger <- lm(climate_index ~ TREATMENT+AGE+
                       fp_orientation_index+empathy_index+
                       NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                       RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num + 
                       east_indicator + urban_indicator,
                     data=ger_article)
lm_climate_migration_ger <- lm(climate_migration_index ~ TREATMENT+AGE+
                                 fp_orientation_index+empathy_index+
                                 NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                                 RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ 
                                 east_indicator + urban_indicator,
                               data=ger_article)
lm_migration_ger <- lm(migration_index ~ TREATMENT+AGE+
                         fp_orientation_index+empathy_index+
                         NATIVE_BORN_num+GENDER_num+EDUCATION_num+IDEOLOGY_num+
                         RELIGIOSITY_num+TRUST_GOVT_num+POL_INTEREST_num+ 
                         east_indicator + urban_indicator,
                       data=ger_article)

ger_climate_margins <- margins(lm_climate_ger, variables = "empathy_index")
ger_migration_margins <- margins(lm_climate_migration_ger, variables = "empathy_index")
ger_climate_migration_margins <- margins(lm_migration_ger, variables = "empathy_index")
ger_climate_margins_data <- as.tibble(summary(ger_climate_margins))
ger_climate_margins_data$outcome <- "Climate"
ger_migration_margins_data <- as.tibble(summary(ger_migration_margins))
ger_migration_margins_data$outcome <- "Migration"
ger_climate_migration_data <- as.tibble(summary(ger_climate_migration_margins))
ger_climate_migration_data$outcome <- "Climate Migration"
ger_margins_data <- rbind(ger_climate_margins_data, ger_migration_margins_data, ger_climate_migration_data) %>% 
  dplyr::select(AME, lower, upper, outcome)
ger_margins_data$country <- "GER"

ger_margins <- ggplot(data= ger_margins_data, aes(x=outcome, y=AME, ymin=lower, ymax=upper)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_pointrange() + coord_flip() +
  labs(x="", y="Average Marginal Effect of Empathy, GER")

# combined_margins_data <- rbind(us_margins_data, ger_margins_data)
# 
# ggplot(data= combined_margins_data, aes(x=outcome, y=AME, ymin=lower, ymax=upper)) +
#   geom_hline(yintercept=0, linetype="dashed") +
#   geom_pointrange() + coord_flip() +
#   facet_grid(country ~.)+
#   labs(x="", y="Average Marginal Effect of Empathy") 

######### Manipulation Checks ######### 
ger_article$threat <- "Soccer"
ger_article$threat[ger_article$TREATMENT %in% c(1, 2)] <- "Migration" 
ger_article$threat[ger_article$TREATMENT %in% c(3, 4)] <- "Climate" 
ger_article$threat[ger_article$TREATMENT %in% c(5, 6)] <- "Climate Migration" 


ger_article <- ger_article %>% 
  dplyr::mutate(
    manip_migration = as.numeric(manip_checks_1),
    manip_climate = as.numeric(manip_checks_4),
    manip_data_privacy = as.numeric(manip_checks_5),
    manip_climate_migration = as.numeric(manip_checks_6)
  )


mean(na.omit(ger_article$manip_migration[ger_article$threat == "Migration"])) - mean(na.omit(ger_article$manip_migration[ger_article$threat == "Soccer"]))

manip_migration <- t.test(na.omit(ger_article$manip_migration[ger_article$threat == "Migration"]), 
                          na.omit(na.omit(ger_article$manip_migration[ger_article$threat == "Soccer"])))


mean(na.omit(ger_article$manip_climate[ger_article$threat == "Climate"])) - mean(na.omit(ger_article$manip_climate[ger_article$threat == "Soccer"]))

manip_climate <- t.test(na.omit(ger_article$manip_climate[ger_article$threat == "Climate"]), 
                        na.omit(ger_article$manip_climate[ger_article$threat == "Soccer"]))


mean(na.omit(ger_article$manip_climate_migration[ger_article$threat == "Climate Migration"])) - mean(na.omit(ger_article$manip_climate_migration[ger_article$threat == "Soccer"]))

manip_climate_migration <- t.test(na.omit(ger_article$manip_climate_migration[ger_article$threat == "Climate Migration"]), 
                                  na.omit(ger_article$manip_climate_migration[ger_article$threat == "Soccer"]))

manipmat <- data.frame(matrix(NA, 3, 4))
colnames(manipmat) <- c( "Treatment", "T-Test P val.", 
                         "Ctrl. Mean", "Treatment Mean") 

manipmat[, 1] <- c("Migration", "Climate", "Climate Migration")
manipmat[, 2] <- c(round(manip_migration$p.value, digits = 3),
                   round(manip_climate$p.value, digits = 3),
                   round(manip_climate_migration$p.value, digits = 3))
manipmat[, 3] <- c(round(manip_migration$estimate[2], digits = 3),
                   round(manip_climate$estimate[2], digits = 3),
                   round(manip_climate_migration$estimate[2], digits = 3))
manipmat[, 4] <- c(round(manip_migration$estimate[1], digits = 3),
                   round(manip_climate$estimate[1], digits = 3),
                   round(manip_climate_migration$estimate[1], digits = 3))

xtable(manipmat, 
       font.size = "small", caption = "Experiment 2 Manipulation Checks, German Sample")

########################### Study 3: Follow up ########################### 
follow <- read.csv(file = 'Migration Follow-Up_August 1, 2020_12.19.csv', stringsAsFactors = T)

# 1 == labor, 2 == climate, 3 == refugee

crosstabs <- as_tibble(rbind(c(204, 97, 88), c(96, 166, 127), c(89, 126, 174)))
colnames(crosstabs) <- c("Labor", "Climate", "Refugee")

follow_plot <- as_tibble(cbind(c(follow$responsible_1, follow$responsible_2, follow$responsible_3),
                               c(rep(1, 389), rep(2, 389), rep(3, 389)),
                               c(rep("Labor", 389), rep("Climate", 389), rep("Refugee", 389))))

names(follow_plot) <- c("responsibility", "migrant", "Type")

ggplot(data=follow_plot, mapping = aes(x=responsibility, fill=as.factor(Type))) +
  geom_bar() +
  labs(fill = "Migrant Type", x="Rank", y="Num. Responses", 
       title="Ranking Migrant Responsibility")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

follow_plot$position <- as.numeric(follow_plot$responsibility)

t.test(follow_plot$position[follow_plot$Type %in% "Labor"], 
       follow_plot$position[follow_plot$Type %in% "Refugee"])

t.test(follow_plot$position[follow_plot$Type %in% "Labor"], 
       follow_plot$position[follow_plot$Type %in% "Climate"])

t.test(follow_plot$position[follow_plot$Type %in% "Climate"], 
       follow_plot$position[follow_plot$Type %in% "Refugee"])

