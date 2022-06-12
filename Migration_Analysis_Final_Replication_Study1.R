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
# 1. Study 1 (US): conjoint_design.dat
# 2. Study 1 (US): conjoint_design_internal.dat
# 3. Study 1 (US): Climate Migration 2_ Conjoint- USA_September 10, 2019_08.56.csv
# 4. Study 1 (Germany): conjoint_design_german2.dat
# 5. Study 1 (Germany): conjoint_design_german2_internal.dat
# 6. Study 1 (Germany): Climate Migration 2_ Conjoint- Germany_September 11, 2019_02.58.csv

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

save(cjt_us_data, file="cjt_US_data.Rdata")

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

names(cjt_ger_data)[23] <- "Occupation"
names(cjt_ger_data)[25] <- "Gender"
names(cjt_ger_data)[27] <- "Reason.for.migration"
names(cjt_ger_data)[29] <- "Origin"
names(cjt_ger_data)[31] <- "Religion"
names(cjt_ger_data)[34] <- "Language.Fluency"
names(cjt_ger_data)[36] <- "Vulnerability"

######### Conjoint main analysis ######### 

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

save(cjt_ger_data, file="cjt_ger_data.Rdata")

######### ADDITIONAL REPLICATION ######### 

# Load clean conjoint data
load("cjt_us_data.Rdata")
load("cjt_ger_data.Rdata")

# Translate German attributes
levels(cjt_ger_data$Gender) <- c("Female", "Male")
levels(cjt_ger_data$Language.Fluency) <- c("Fluent", "Broken", "None")
levels(cjt_ger_data$Occupation) <- c("Unemployed", "Doctor", "Teacher", "Cleaner")
levels(cjt_ger_data$Origin) <- c("Ethiopia", "Afghanistan", "Another region in your country", "Myanmar", "Ukraine")
levels(cjt_ger_data$Reason.for.migration) <- c("Flooding", "Drought", "Political/religious/ethnic persecution", "Wildfires", "Economic opportunity")
levels(cjt_ger_data$Religion) <- c("Agnostic", "Christian", "Muslim")
levels(cjt_ger_data$Vulnerability) <- c("Food insecurity", "Physically handicapped", "None", "No surviving family members", "Post Traumatic Stress Disorder (PTSD)")

# Combine US and German data

cjt_data <- merge(cjt_us_data, cjt_ger_data, all.x = T, all.y = T)
cjt_data$country <- NA
cjt_data$country[1:18966] <- "US"
cjt_data$country[18967:37828] <- "Germany"
cjt_data$Response.ID <- factor(cjt_data$Response.ID)

# Replicate Table 2 with the combined data

baselines <- list()
baselines$Vulnerability <- 'None'
baselines$Reason.for.migration <- 'Economic opportunity'
baselines$Occupation <- 'Unemployed'
baselines$Language.Fluency <- 'None'
baselines$Origin <- 'Another region in your country'

cjt_results <- cjoint::amce(selected ~ Gender + Language.Fluency + Occupation + Origin +
                              Reason.for.migration + Religion + Vulnerability, 
                            data = cjt_data,
                            cluster= T,
                            respondent.id = 'Response.ID',
                            design= cjt_us_design, 
                            baselines=baselines)

summary(cjt_results)$amce
xtable(summary(cjt_results)$amce[, c(1:4, 7)], digits=3, 
       font.size = "small", caption = "AMCE, US Sample (Compared to baseline levels")

levels.test<-list()
levels.test[["Gender"]]<-c("Female","Male")
levels.test[["Language Fluency"]]<-c('None', "Broken", "Fluent")
levels.test[["Occupation"]]<-c("Unemployed","Cleaner", "Doctor", "Teacher")
levels.test[["Origin"]]<-c("Same Country", "Afghanistan", "Ethiopia", "Myanmar", "Ukraine")
levels.test[["Reason.for.migration"]]<-c("Economic","Drought", "Flooding", "Persecution", "Wildfires")
levels.test[["Religion"]]<-c("Agnostic","Christian", "Muslim")
levels.test[["Vulnerability"]]<-c("None","Food insc.", "No family", "Physical handicap", "PTSD")

plot.amce(cjt_results, xlab="Expected Change in Migrant Profile Selection, US",
          main="", 
          level.names = levels.test,
          attribute.names = c("Gender","Language Fluency","Occupation",
                              "Origin", "Reason", "Religion", "Vulnerability"),
          # xlim=c(-0.07, .2),
          text.size=9)

### Compare the effects of migration reasons and other characteristics

# Combine reasons for migrations and origin 

cjt_data$Reason.for.migration3 <- cjt_data$Reason.for.migration
levels(cjt_data$Reason.for.migration3) <- c("Environment", "Economic opportunity", "Environment", "Persecution", "Environment")
cjt_data$Origin2 <- cjt_data$Origin
levels(cjt_data$Origin2) <- c("Foreign", "Native", "Foreign", "Foreign", "Foreign")

# Replicate the results with the combined attributes

m0 <- amce(cjt_data, selected ~ Reason.for.migration3 + Origin2 + Language.Fluency + Occupation, id = ~ Response.ID)
plot(m0)

### Compare effects and marginal means of migration reasons among anti-immigration and pro-immigration respondents 

# Define immigration preference based on average profile ranking
cjt_data_rank <- data.frame(cbind(rownames(tapply(cjt_data$rank_outcome, cjt_data$Response.ID, mean)), 
                                  tapply(cjt_data$rank_outcome, cjt_data$Response.ID, mean)))
names(cjt_data_rank) <- c("Response.ID", "rank_outcomeA")
cjt_data <- merge(cjt_data, cjt_data_rank, by = "Response.ID", all.x = T)
cjt_data$rank_outcomeA <- as.numeric(cjt_data$rank_outcomeA)
cjt_data$antiimmigration <- factor(ifelse(cjt_data$rank_outcomeA < 4, "anti-immigration", "pro-immigration"))

# Correlate with common predictors of immigration preferences to check the validity
cor(cjt_data[c("rank_outcomeA", "IDEOLOGY_num", "EDUCATION_num", "AGE")], use = "complete.obs")

# Define an alternative proxy for immigration preferences based on ideology, education, and age

cjt_data$antiimmigration2 <- factor(ifelse(cjt_data$IDEOLOGY_num > 4 & cjt_data$EDUCATION_num < 5, 
                                           "anti-immigration", "pro-immigration"))

# Estimate and plot subgroup marginal means by immigration preferences
mm_by <- cj(cjt_data, selected ~ Reason.for.migration3 + Origin2 + Language.Fluency + Occupation + Religion, id = ~Response.ID, estimate = "mm", by = ~antiimmigration)
plot(mm_by, group = "antiimmigration", vline = 0.5)

mm_by2 <- cj(cjt_data, selected ~ Reason.for.migration3 + Origin2 + Language.Fluency + Occupation + Religion, id = ~Response.ID, estimate = "mm", by = ~antiimmigration2)
plot(mm_by2, group = "antiimmigration2", vline = 0.5)

### Compare the effects of migration reasons to other characteristics 

# Test for differences between coefficients

m1 <- lm(selected ~ Reason.for.migration3 + Origin2 + Language.Fluency + Occupation + factor(Response.ID), data = cjt_data)
summary(m1)

library(car)
linearHypothesis(m1,"Reason.for.migration3Economic opportunity = Language.FluencyNone")
linearHypothesis(m1,"Reason.for.migration3Persecution = Language.FluencyFluent")

linearHypothesis(m1,"Reason.for.migration3Economic opportunity = OccupationUnemployed")
linearHypothesis(m1,"Reason.for.migration3Persecution = OccupationTeacher")
linearHypothesis(m1,"Reason.for.migration3Persecution = OccupationDoctor")

# Estimate and plot marginal means

mm1 <- mm(cjt_data, selected ~ Reason.for.migration3 + Origin2 + Language.Fluency + Occupation + Religion, id = ~Response.ID)
plot(mm1, vline = 0.5)

mm2 <- mm(cjt_data, rank_outcome ~ Reason.for.migration3 + Origin2 + Language.Fluency + Occupation + Religion, id = ~Response.ID)
plot(mm2)

# Compare marginal means of certain profiles

plot(mm(cjt_data[cjt_data$Language.Fluency == "Fluent" & cjt_data$Occupation == "Doctor",], 
        selected ~ Reason.for.migration3, id = ~Response.ID), vline = 0.5) # 0.55-0.64 for economic fluent and skilled migrants 
plot(mm(cjt_data[cjt_data$Reason.for.migration3 == "Environment",], 
        selected ~ Language.Fluency + Occupation, id = ~Response.ID), vline = 0.5) # 0.4-0.6 for environment migrants of various skills and fluency 
plot(mm(cjt_data[cjt_data$Reason.for.migration3 == "Persecution",], 
        selected ~ Language.Fluency + Occupation, id = ~Response.ID), vline = 0.5) # 0.45-0.65 for environment migrants of various skills and fluency 

plot(mm(cjt_data[cjt_data$Language.Fluency == "Fluent" & cjt_data$Occupation == "Doctor",], 
        rank_outcome ~ Reason.for.migration3, id = ~Response.ID)) # 4.6-4.9 for economic fluent and skilled migrants 
plot(mm(cjt_data[cjt_data$Reason.for.migration3 == "Environment",], 
        rank_outcome ~ Language.Fluency + Occupation, id = ~Response.ID)) # 4.1-4.8 for environment migrants of various skills and fluency 
plot(mm(cjt_data[cjt_data$Reason.for.migration3 == "Persecution",], 
        rank_outcome ~ Language.Fluency + Occupation, id = ~Response.ID)) # 4.4-4.9 for environment migrants of various skills and fluency 

### Replicate the results with the major interaction effects between migration reasons and other factors

m2 <- lm(selected ~ Reason.for.migration3 + Language.Fluency + Occupation + Religion + factor(country) + 
           Reason.for.migration3*Language.Fluency + Reason.for.migration3*Occupation + Reason.for.migration3*Religion, data = cjt_data)
summary(m2)
m3 <- lm(selected ~ Reason.for.migration3 + Origin2 + Occupation + factor(country), data = cjt_data[cjt_data$Language.Fluency == "Fluent",])
summary(m3)
# Persecution is less beneficial for favorability among migrants fluent in English
# Economic opportunity is more detrimental for favorability among migrants fluent in English