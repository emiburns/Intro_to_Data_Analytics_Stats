#DESCRIPTIVE STATS-----------------------------------------------------

#importing------------------------
#packages
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(moments)
library(e1071)
source("01_descriptive_stats_functions.R")

#raw data
setwd("~/Documents/Data_Science/projects/medium/stats_101/descriptive_stats")
df_clean <- read.csv('../SF_Salary_Clean.csv', stringsAsFactors=FALSE)

head(df_clean)
str(df_clean)


#measures of dispersion------------------------
df_dispersion <- data.frame(matrix(nrow=5,ncol=5))
colnames(df_dispersion) <- c("BasePay", "OvertimePay", "OtherPay", 
                             "TotalPay", "TotalPayBenefits")
rownames(df_dispersion) <- c("minimum", "maximum", "variance",
                             "interquartile_range", "std")

df_short <- df_clean[,3:7]

i=1
for(j in 1:ncol(df_dispersion)){
    df_dispersion[i, j] <- min(df_short[,j], na.rm=T)
    df_dispersion[i+1, j] <- max(df_short[,j], na.rm=T)
    df_dispersion[i+2, j] <- var(df_short[,j], na.rm=T)
    df_dispersion[i+3, j] <- IQR(df_short[,j], na.rm=T)
    df_dispersion[i+4, j] <- sd(df_short[,j], na.rm=T)
    }

df_dispersion


#measures of central tendency------------------------
df_central <- data.frame(matrix(nrow=3,ncol=5))
colnames(df_central) <- c("BasePay", "OvertimePay", "OtherPay", 
                          "TotalPay", "TotalPayBenefits")
rownames(df_central) <- c("mean", "median", "mode")

getmode <- function(x) {
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
}

for(j in 1:ncol(df_central)){
    df_central[i, j] <- round(mean(df_short[,j], na.rm=T), 2)
    df_central[i+1, j] <- median(df_short[,j], na.rm=T)
    df_central[i+2, j] <- getmode(df_short[,j])
}

df_central


#measures of position------------------------
#quartile/percentile ranks
df_position <- data.frame(matrix(nrow=4,ncol=5))
colnames(df_position) <- c("BasePay", "OvertimePay", "OtherPay", 
                          "TotalPay", "TotalPayBenefits")
rownames(df_position) <- c("Q1_Quantile", "Q2_Quantile", "Q3_Quantile", "IQR")

for(j in 1:ncol(df_position)){
    df_position[i, j] <- quantile(df_short[,j], 0.25, names = F)
    df_position[i+1, j] <- quantile(df_short[,j], 0.50, names = F)
    df_position[i+2, j] <- quantile(df_short[,j], 0.75, names = F)
    df_position[i+3, j] <- quantile(df_short[,j], 0.75, names = F) - quantile(df_short[,j], 0.25, names = F)
}

df_position

#measures of symmetry------------------------
#outliers
df_outlier <- data.frame(matrix(nrow=1,ncol=5))
colnames(df_outlier) <- c("BasePay", "OvertimePay", "OtherPay", 
                           "TotalPay", "TotalPayBenefits")
rownames(df_outlier) <- c("Num_Outlier")

for(j in 1:ncol(df_outlier)){
    df_BPOut <- subset(df_short, df_short[,j] >= 1.5 * (quantile(df_short[,j], 0.75, names = F)) | 
                                  df_short[,j] <= 1.5*(quantile(df_short[,j], 0.25, names = F)))
    df_outlier[1,j] <- nrow(df_BPOut)
}

#boxplots 
df_clean$Year <- as.factor(df_clean$Year)

p1 <- ggplot(df_clean, aes(x=Year, y=TotalPay, color=Year)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size = 1)
p1

p2 <- ggplot(df_clean, aes(x=Year, y=TotalPayBenefits, color=Year)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size = 1)
p2

extreme_outliers <- df_clean %>% 
    dplyr::filter(TotalPay > 400000) %>% 
    dplyr::arrange(EmployeeName, JobTitle)
nrow(extreme_outliers) #22

#skew & kurtosis
pBasePay <- ggHistogram(df_clean$BasePay)
pBasePay

pOvertime <- ggHistogram(df_clean$OvertimePay)
pOvertime

pOtherPay <- ggHistogram(df_clean$OtherPay)
pOtherPay

pTotalPay <- ggHistogram(df_clean$TotalPay)
pTotalPay

pTotalPayBenefits <- ggHistogram(df_clean$TotalPayBenefits)
pTotalPayBenefits

df_symmetry <- data.frame(matrix(nrow=2,ncol=5))
colnames(df_symmetry) <- c("BasePay", "OvertimePay", "OtherPay", 
                           "TotalPay", "TotalPayBenefits")
rownames(df_symmetry) <- c("Skew", "Kurtosis")

for(j in 1:ncol(df_symmetry)){
    df_symmetry[i, j] <- round(skewness(df_short[,j]), 2)
    df_symmetry[i+1, j] <- kurtosis(df_short[,j])
}

#Most variables are normally distributed
#But OtherPay & OvertimePay have severe skew & kurtosis --> can use log10 transformation to correct


#measures of frequency (for categorical variables)------------------------
#count
length(unique(df_clean$JobTitle)) #1751
length(unique(df_clean$Year)) #8

#percent & frequency
#frequency of overtime pay for those with a base salary
sum(ifelse(df_clean$BasePay >0 & df_clean$OvertimePay >0, 1, 0)) / nrow(df_clean) #69%

#frequency & percent presence of job title 
df_freq <- as.data.frame(table(df_clean$JobTitle)) 
df_freq <- dplyr::rename(df_freq, JobTitle=Var1, JobTitleFreq=Freq)
df_merged <- merge(df_clean, df_freq, by = "JobTitle")
df_merged$JobTitlePercent <- round(100*(df_merged$JobTitleFreq/nrow(df_merged)), 2)
head(df_merged)

#plotting salary for the top 10 most frequent job's in dataset
df_max <- df_merged %>% dplyr::group_by(JobTitle) %>%
    dplyr::summarise(Avg_Pay = mean(TotalPayBenefits, na.rm=T), JobFreq = max(JobTitleFreq))
df_max <- df_max[order(-df_max$JobFreq),][1:10,]
df_max$Avg_Pay <- round(df_max$Avg_Pay)
    
p3 <- ggplot(data=df_max, aes(x=reorder(JobTitle, Avg_Pay), y=Avg_Pay)) +
    geom_bar(stat="identity", color="black", fill="aquamarine2") +
    geom_text(aes(label=Avg_Pay), vjust=-0.3, size=2.5) +
    xlab('Job Title') +
    ylab('Average Pay')+
    coord_flip() 
p3

#calculating % growth/decline in repeated people's salaries. plyr's count function for frequency
#covariance and correlation≥
