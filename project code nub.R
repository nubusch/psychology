


if (!require("pacman")) install.packages("pacman");library(pacman)
pacman::p_load(tidyverse, janitor, ggplot2, dplyr, broom, fastDummies, haven, iotools, data.table, stringr, reshape2, psych, mlbench, lmerTest)


#import data
setwd("C:/Users/tingy/OneDrive/Bureaublad/Psych/Year 2/M7/project/data")
data = fread("data_project.csv", header = TRUE, sep = ',')[-1:-2,]

#getting the columns needed

data = select(data, all_of(c("Code", "Age", "Gender", "Screen time", "Time of day", 
                             "Apps", "Apps_4_TEXT", "Apps_reduce", "Apps_reduce_4_TEXT", "Barrier", "Barrier difficulty_1", "Q108_1", 
                             "Q108_2", "Q108_3", "Q108_4", "Q108_5", "Q108_6", "Q108_7", "Q108_8", "Q108_9", "Q108_10", "Q108_11", "Q108_12", 
                             "Q108_13", "Q108_14", "Q108_15", "Q108_16", "Q108_17", "Q108_18", "Q108_19", "Q108_20", "Q108_21", "Q108_22", 
                             "Q108_23", "Q108_24", "Q108_25", "Q108_26", "Q108_27", "Q108_28", "Q108_29", "Q108_30", "Q108_31", "Q108_32", 
                             "Q108_33", "Q108_34", "Q108_35", "Q108_36", "Q108_37", "Q108_38", "Q108_39", "Q108_40", "Q108_41", "Q108_42", 
                             "Q108_43", "Q108_44", "Q108_45", "Q108_46", "Q108_47", "Q108_48", "Q108_49", "Q108_50")))

data = data[!data$Q108_1=="",]


data$apps_to_reduce = str_c(data$Apps_reduce, sep = ",", data$Apps_reduce_4_TEXT)

#data = select(data, apps_to_reduce)


#personality traits

#creating reversed scoring
pt = select(data, all_of(c("Q108_1", "Q108_2", "Q108_3", "Q108_4", "Q108_5", "Q108_6", "Q108_7", "Q108_8", "Q108_9", "Q108_10", "Q108_11", "Q108_12", 
                              "Q108_13", "Q108_14", "Q108_15", "Q108_16", "Q108_17", "Q108_18", "Q108_19", "Q108_20", "Q108_21", "Q108_22", 
                              "Q108_23", "Q108_24", "Q108_25", "Q108_26", "Q108_27", "Q108_28", "Q108_29", "Q108_30", "Q108_31", "Q108_32", 
                              "Q108_33", "Q108_34", "Q108_35", "Q108_36", "Q108_37", "Q108_38", "Q108_39", "Q108_40", "Q108_41", "Q108_42", 
                              "Q108_43", "Q108_44", "Q108_45", "Q108_46", "Q108_47", "Q108_48", "Q108_49", "Q108_50")))

pt = sapply(pt, as.numeric)

keys = c(1,1,1,1,-1,1,-1,1,-1,1,1,1,-1,1,-1,1,1,1,-1,-1,1,-1,1,-1,1,1,1,-1,1,-1,-1,-1,1,1,1,1,1,-1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,-1)

pt = reverse.code(keys, pt, mini = 0, maxi = 5)


#adding traits to datasheet

#openness
data$openness = rowSums(pt[, c(1,6,11,16,21,26,31,36,41,46)], na.rm = TRUE)

#extraversion
data$extraversion = rowSums(pt[, c(2,7,12,17,22,27,32,37,42,47)], na.rm = TRUE)

#agreeableness
data$agreeableness = rowSums(pt[, c(3,8,13,18,23,28,33,38,43,48)], na.rm = TRUE)

#conscientiousness
data$conscientiousness = rowSums(pt[, c(4,9,14,19,24,29,34,39,44,49)], na.rm = TRUE)

#emotional stability
data$emotional_stability = rowSums(pt[, c(5,10,15,20,25,30,35,40,45,50)], na.rm = TRUE)


#remove personality trait scores from data
data = select(data, c("Code", "Age", "Gender", "Screen time", "Time of day", "Apps", "Apps_4_TEXT", "Apps_reduce", "Apps_reduce_4_TEXT", "Barrier", 
                      "Barrier difficulty_1", "apps_to_reduce", "openness", "extraversion", "agreeableness", "conscientiousness", "emotional_stability" ))


#making data long
#preparing data by splitting columns of apps participants use

ncols = max(str_count(data$apps_to_reduce, ",")) +1

colmn = paste("col", 1:ncols)

data = cbind(data, colsplit(data$apps_to_reduce, ",", names = colmn))


#pivot data longer and remove empty spaces
data = pivot_longer(data,
                    cols = c("col 1", "col 2", "col 3", "col 4", "col 5"),
                    names_to = "apps",
                    values_to = "apps_reduce")


data = filter(data, !is.na(apps_reduce) & apps_reduce!= "Other" & apps_reduce!= " please mention:" & apps_reduce!="") |> 
  select(-c("Apps_reduce", "Apps_reduce_4_TEXT", "apps_to_reduce", "apps", "Apps", "Apps_4_TEXT"))


#recode screentime
unique(data$`Screen time`)


data = mutate(data, group = case_when(
  group == 
))

data_numeric <- data2 %>% 
  mutate(group=case_when(
    group == "Hit" ~ 1,
    group == "Smash" ~ 2,
    group == "Control" ~ 3,
  ))

#recode time of day and make long





#check = select(data, "Code", "reduce")






















