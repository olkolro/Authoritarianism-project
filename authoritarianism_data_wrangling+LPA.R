#set working directory

setwd("C:/Users/olgak/Desktop/masters project")

# load wave 7 dataset

library(haven)
aut_df <- read_sav("C19_wave7_all.sav")
View(aut_df) #view dataset

#recoding reverse authoritarianism items

library(car)
aut_df$R_aut_1 <- recode(aut_df$W7_Authoritarianism_1, "1=5; 2=4; 3=3; 4=2; 5=1")
aut_df$R_aut_8 <- recode(aut_df$W7_Authoritarianism_8, "1=5; 2=4; 3=3; 4=2; 5=1")
aut_df$R_aut_13 <- recode(aut_df$W7_Authoritarianism_13, "1=5; 2=4; 3=3; 4=2; 5=1")
aut_df$R_aut_23 <- recode(aut_df$W7_Authoritarianism_23, "1=5; 2=4; 3=3; 4=2; 5=1")
aut_df$R_aut_25 <- recode(aut_df$W7_Authoritarianism_25, "1=5; 2=4; 3=3; 4=2; 5=1")

#calculate scores for each authoritarianism subscale

aut_df$RWA <- aut_df$R_aut_1 + aut_df$W7_Authoritarianism_6 + aut_df$W7_Authoritarianism_11 + aut_df$W7_Authoritarianism_16 + aut_df$W7_Authoritarianism_21
aut_df$LWA <- aut_df$W7_Authoritarianism_2 + aut_df$W7_Authoritarianism_7 + aut_df$W7_Authoritarianism_12 + aut_df$W7_Authoritarianism_17 + aut_df$W7_Authoritarianism_22
aut_df$SA <- aut_df$W7_Authoritarianism_3 + aut_df$R_aut_8 + aut_df$R_aut_13 + aut_df$W7_Authoritarianism_18 + aut_df$R_aut_23
aut_df$COVID <- aut_df$W7_Authoritarianism_4 + aut_df$W7_Authoritarianism_9 + aut_df$W7_Authoritarianism_14 + aut_df$W7_Authoritarianism_19 + aut_df$W7_Authoritarianism_24
aut_df$JS <- aut_df$W7_Authoritarianism_5 + aut_df$W7_Authoritarianism_10 + aut_df$W7_Authoritarianism_15 + aut_df$W7_Authoritarianism_20 + aut_df$R_aut_25

#latent profile analysis

library(tidyLPA)
library(dplyr)

#get and compare models
aut_df %>%
  select(RWA, LWA, SA, COVID, JS) %>%
  single_imputation() %>%
  estimate_profiles(2:6) %>%
  compare_solutions(statistics = c("AIC", "BIC", "Entropy"))

#model estimates and plots

library(papaja)
library(ggplot2)

#2 profiles

m2 <- aut_df %>% select(RWA, LWA, SA, COVID, JS) %>% single_imputation() %>% 
  estimate_profiles(2)
#extract & sort LPA data
m2_data <- get_estimates(m2)
m2_data <- m2_data %>% subset(Category != "Variances") 
m2_data$Class <- factor(m2_data$Class, 
                        labels = c("Group 1", "Group 2"))
#plot
m2_data %>% ggplot(aes(x=Parameter, y=Estimate, group=Class, color=Class)) + 
  geom_point(size = 2.25) + 
  geom_line(size = 1.25) + 
  labs(x = NULL, y = "Mean Estimates") + 
  theme_apa() + scale_color_grey(start = 0.40, end = 0.80)

#3 profiles

m3 <- aut_df %>% select(RWA, LWA, SA, COVID, JS) %>% single_imputation() %>% 
  estimate_profiles(3)
#extract & sort LPA data
m3_data <- get_estimates(m3)
m3_data <- m3_data %>% subset(Category != "Variances") 
m3_data$Class <- factor(m3_data$Class, 
                        labels = c("Group 1", "Group 2", "Group 3"))
#plot
m3_data %>% ggplot(aes(x=Parameter, y=Estimate, group=Class, color=Class)) + 
  geom_point(size = 2.25) + 
  geom_line(size = 1.25) + 
  labs(x = NULL, y = "Mean Estimates") + 
  theme_apa() + scale_color_grey(start = 0.40, end = 0.80)

#4 profiles [best solution]

m4 <- aut_df %>% select(RWA, LWA, SA, COVID, JS) %>% single_imputation() %>% 
  estimate_profiles(4)
#extract & sort LPA data
m4_data <- get_estimates(m4)
m4_data <- m4_data %>% subset(Category != "Variances") 
m4_data$Class <- factor(m4_data$Class, 
                        labels = c("Group 1", "Group 2", "Group 3", "Group 4")); head(m4_data)
#plot
m4_data %>% ggplot(aes(x=Parameter, y=Estimate, group=Class, color=Class)) + 
  geom_point(size = 2.25) + 
  geom_line(size = 1.25) + 
  labs(x = NULL, y = "Mean Estimates") + 
  theme_apa() + scale_color_grey(start = 0.40, end = 0.80)

#5 profiles

m5 <- aut_df %>% select(RWA, LWA, SA, COVID, JS) %>% single_imputation() %>% 
  estimate_profiles(5)
#extract & sort LPA data
m5_data <- get_estimates(m5)
m5_data <- m5_data %>% subset(Category != "Variances") 
m5_data$Class <- factor(m5_data$Class, 
                        labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"))
#plot
m5_data %>% ggplot(aes(x=Parameter, y=Estimate, group=Class, color=Class)) + 
  geom_point(size = 2.25) + 
  geom_line(size = 1.25) + 
  labs(x = NULL, y = "Mean Estimates") + 
  theme_apa() + scale_color_grey(start = 0.40, end = 0.80)

#6 profiles
m6 <- aut_df %>% select(RWA, LWA, SA, COVID, JS) %>% single_imputation() %>% 
  estimate_profiles(6)
#extract & sort LPA data
m6_data <- get_estimates(m6)
m6_data <- m6_data %>% subset(Category != "Variances") 
m6_data$Class <- factor(m6_data$Class, 
                        labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6"))
#plot
m6_data %>% ggplot(aes(x=Parameter, y=Estimate, group=Class, color=Class)) + 
  geom_point(size = 2.25) + 
  geom_line(size = 1.25) + 
  labs(x = NULL, y = "Mean Estimates") + 
  theme_apa() + scale_color_grey(start = 0.40, end = 0.80)


#probabilities for 4 classes

aut_data <- get_data(m4)
View(aut_data) #also shows profile (class) for each participant

#merge aut_df with aut_data (for further analyses)

aut_all <- merge(aut_df, aut_data, by = c("RWA", "LWA", "SA", "COVID", "JS"))

#remove duplicate rows
aut_fin <- distinct(aut_all)
table(aut_fin$Class) #shows number of participants in each profile (class)

#prepare predictor variables

detach("package:tidyLPA", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:papaja", unload = TRUE)

#recode child- and parent-rearing items

aut_fin$child_rearing1 <- recode(aut_fin$W7_ChildRearing1, "1=0; 2=1")
aut_fin$child_rearing2 <- recode(aut_fin$W7_ChildRearing2, "1=0; 2=1")
aut_fin$child_rearing4 <- recode(aut_fin$W7_ChildRearing4, "1=0; 2=1")
aut_fin$child_rearing6 <- recode(aut_fin$W7_ChildReraing6, "1=0; 2=1")

aut_fin$child_rearing3 <- recode(aut_fin$W7_ChildRearing3, "1=1; 2=0")
aut_fin$child_rearing5 <- recode(aut_fin$W7_ChildRearing5, "1=1; 2=0")
aut_fin$child_rearing7 <- recode(aut_fin$W7_ChildRearing7, "1=1; 2=0")
aut_fin$child_rearing8 <- recode(aut_fin$W7_ChildRearing8, "1=1; 2=0")

aut_fin$parent_rearing1 <- recode(aut_fin$W7_Parent_Rearing1, "1=0; 2=1")
aut_fin$parent_rearing2 <- recode(aut_fin$W7_Parent_Rearing2, "1=0; 2=1")
aut_fin$parent_rearing4 <- recode(aut_fin$W7_Parent_Rearing4, "1=0; 2=1")
aut_fin$parent_rearing6 <- recode(aut_fin$W7_Parent_Rearing6, "1=0; 2=1")

aut_fin$parent_rearing3 <- recode(aut_fin$W7_Parent_Rearing3, "1=1; 2=0")
aut_fin$parent_rearing5 <- recode(aut_fin$W7_Parent_Rearing5, "1=1; 2=0")
aut_fin$parent_rearing7 <- recode(aut_fin$W7_Parent_Rearing7, "1=1; 2=0")
aut_fin$parent_rearing8 <- recode(aut_fin$W7_Parent_Rearing8, "1=1; 2=0")

#calculate total scores

aut_fin$child_rearing_TOTAL <- aut_fin$child_rearing1 + aut_fin$child_rearing2 + aut_fin$child_rearing3 + aut_fin$child_rearing4 + aut_fin$child_rearing5 + aut_fin$child_rearing6 + aut_fin$child_rearing7 + aut_fin$child_rearing8
aut_fin$parent_rearing_TOTAL <- aut_fin$parent_rearing1 + aut_fin$parent_rearing2 + aut_fin$parent_rearing3 + aut_fin$parent_rearing4 + aut_fin$parent_rearing5 + aut_fin$parent_rearing6 + aut_fin$parent_rearing7 + aut_fin$parent_rearing8

#recode bullying items

aut_fin$bully_child1 <- recode(aut_fin$W7_Bully_child_1, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child2 <- recode(aut_fin$W7_Bully_child_2, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child3 <- recode(aut_fin$W7_Bully_child_3, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child4 <- recode(aut_fin$W7_Bully_child_4, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child5 <- recode(aut_fin$W7_Bully_child_5, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child6 <- recode(aut_fin$W7_Bully_child_6, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child7 <- recode(aut_fin$W7_Bully_child_7, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child8 <- recode(aut_fin$W7_Bully_child_8, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")
aut_fin$bully_child9 <- recode(aut_fin$W7_Bully_child_9, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")

aut_fin$bully_adult1 <- recode(aut_fin$W7_Bully_adult_1, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult2 <- recode(aut_fin$W7_Bully_adult_2, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult3 <- recode(aut_fin$W7_Bully_adult_3, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult4 <- recode(aut_fin$W7_Bully_adult_4, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult5 <- recode(aut_fin$W7_Bully_adult_5, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult6 <- recode(aut_fin$W7_Bully_adult_6, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult7 <- recode(aut_fin$W7_Bully_adult_7, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult8 <- recode(aut_fin$W7_Bully_adult_8, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 
aut_fin$bully_adult9 <- recode(aut_fin$W7_Bully_adult_9, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0") 

aut_fin$bully_others <- recode(aut_fin$W7_Bully_others, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0")

#calculate totals

aut_fin$bully_child_TOTAL <- aut_fin$bully_child1 + aut_fin$bully_child2 + aut_fin$bully_child3 + aut_fin$bully_child4 + aut_fin$bully_child5 + aut_fin$bully_child6 + aut_fin$bully_child7 + aut_fin$bully_child8 + aut_fin$bully_child9
aut_fin$bully_adult_TOATL <- aut_fin$bully_adult1 + aut_fin$bully_adult2 + aut_fin$bully_adult3 + aut_fin$bully_adult4 + aut_fin$bully_adult5 + aut_fin$bully_adult6 + aut_fin$bully_adult7 + aut_fin$bully_adult8 + aut_fin$bully_adult9

#merge aut_fin with wave 1, 3, 4, 6 datasets including education binary variables

#read datasets

w1 <- read_sav("C19_wave1_data_needed.sav")
w3 <- read_sav("C19_wave3_data_needed.sav")
w4 <- read_sav("C19_wave4_data_needed.sav")
w6 <- read_sav("C19_wave6_data_needed.sav")

#merge by participant id

aut_fin1 <- merge(aut_fin, w1, by = "pid", all = TRUE)
aut_fin2 <- merge(aut_fin1, w3, by = "pid", all = TRUE)
aut_fin3 <- merge(aut_fin2, w4, by = "pid", all = TRUE)
aut_fin_w_edu <- merge(aut_fin3, w6, by = "pid", all = TRUE)

#merge all Education_binary variables into one column 

aut_fin_w_edu$Education_binary <- aut_fin_w_edu$W1_Education_binary
aut_fin_w_edu$Education_binary[!is.na(aut_fin_w_edu$W3_Education_binary)] <- aut_fin_w_edu$W3_Education_binary[!is.na(aut_fin_w_edu$W3_Education_binary)]
aut_fin_w_edu$Education_binary[!is.na(aut_fin_w_edu$W4_Education_binary)] <- aut_fin_w_edu$W4_Education_binary[!is.na(aut_fin_w_edu$W4_Education_binary)]
aut_fin_w_edu$Education_binary[!is.na(aut_fin_w_edu$W6_Education_binary)] <- aut_fin_w_edu$W6_Education_binary[!is.na(aut_fin_w_edu$W6_Education_binary)]

#remove non-wave 7 participants (NA in Class)

library(tidyverse)

aut_fin_w7only <- aut_fin_w_edu %>% drop_na(Class)
View(aut_fin_w7only) #view dataset

#clean dataset to leave variables of interest only

aut_data_clean <- select(aut_fin_w7only, c("pid", "RWA", "LWA", "SA", "COVID", "JS", "Class", "W7_Gender",
                         "W7_Age_year", "Education_binary", "W7_EU_Ref", 
                         "bully_child_TOTAL", "bully_adult_TOATL", "bully_others", 
                         "child_rearing_TOTAL", "parent_rearing_TOTAL"))

#save dataset in SPSS (.sav) format

write_sav(aut_data_clean, "aut_data_clean.sav")

#further analyses (multinomial logistic regressions) were carried out in SPSS through syntax
#see syntax file for the code


