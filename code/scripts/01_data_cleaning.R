#CLEANING RAW DATA----------------

#importing------------------------

#packages
library(tidyr)

#raw data
setwd("~/Documents/Data_Science/projects/medium/stats_101/descriptive_stats")
df_raw <- read.csv('../SF_Salary.csv', stringsAsFactors=FALSE)

head(df_raw)
str(df_raw)


#quick cleaning------------------------
df_raw[df_raw=='Not Provided'] <- NA

df_raw$BasePay <- as.numeric(df_raw$BasePay)
df_raw$OtherPay <- as.numeric(df_raw$OtherPay)
df_raw$OvertimePay <- as.numeric(df_raw$OvertimePay)
df_raw$OvertimePay <- as.numeric(df_raw$OtherPay)

df_raw$JobTitle <- toupper(df_raw$JobTitle)

#looking at NA's
na_count <- sapply(df_raw, function(y) length(which(is.na(y))))
na_count <- data.frame(na_count)
na_count$percent <- round(((na_count$na_count/nrow(df_raw))*100), 2)

#total pay, total pay benefits, and year didn't have any missing data
#employee name, job title, base bay, overtime pay, other pay all
#under 1% missing data --> delete observations for simplicity of project
df_raw <- df_raw[complete.cases(df_raw[ , 1:5]),]

#benefits have 12% missing data --> cut column for simplicity of project
df_raw <- df_raw[, -6]
names(df_raw)

#checking for missed NA values
sum(!complete.cases(df_raw)) #0

#checking for duplicated entries
df_raw <- df_raw[!duplicated(df_raw), ]

#saving clean df to directory
df_clean <- df_raw
write.csv(df_clean, file = '../SF_Salary_Clean.csv')
