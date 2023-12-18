# Header ----
# Title: Sleep in MetaAIR
# Author: Tanya Alderete
# Date: Dec 2023

# Loading Packages ----
library(tidyverse); library(dplyr); library(lubridate); 
library(tibble); library(ggplot2); library(qwraps2);
library(magrittr); library(gridExtra)

# Create theme for graphs ---
mytheme <- theme(axis.line = element_line(size = 0.5, colour = "black"),
                 panel.background = element_rect(fill = "white"))

mytheme.2 <- theme(axis.line = element_line(size = 2, colour = "black"),
                   panel.background = element_rect(fill = "white"))

# Clear workspace ----
rm(list=ls())

# Set filepaths ----
meta <-"/Volumes/ADORLab/Lab Projects/MetaAIR/Master Metadata/"

# Importing Data ----
metaair <- read.csv(paste0(meta,"MetaAIRStudy_DATA_2021-05-28_1423.csv"))

# Dropping Variables ----
sub <- dplyr::select(metaair, id, id_lab, protocol_type, towncode, age, cage, male, heightcm, exer, weightkg, bodyfatpercent, bmi, gluc_bedsidefasting, gluc_bedside120min, ogtt1yn, waistcm, insulin_yn, insulin_fasting, insulin_30min, insulin_60min, insulin_90min, insulin_120min, insulin_fasting2, insulin_30min2, insulin_60min2, insulin_90min2, insulin_120min2, gluc_fasting, gluc_30min, gluc_60min, gluc_90min, gluc_120min, hba1c, wkday_bdtime, wkend_bdtime, wkday_wake, wkend_wake, sleep_trouble, sleep_stay, snore, race___1, race___2, race___3, race___4, race___5, race___6, race_other, hisp, occupation___1, occupation___2, occupation___3, occupation___4, occupation___5, occupation___6, occupation_other, occupation_type___1, occupation_type___10, occupation_type___11, occupation_type___12, occupation_type___13, occupation_type___14, occupation_type___15, occupation_type___16, occupation_type___17, occupation_type___2, occupation_type___3, occupation_type___4, occupation_type___5, occupation_type___6, occupation_type___7, occupation_type___8, occupation_type___9)
sub2 <- sub$id[!grepl("--1",sub$id) ] 
sub3 <- sub2[!grepl("--2",sub2) ]
sub.2 <- sub[ sub$id %in% sub3, ]

# Race/Ethnicity ----
## Recoding for Race ----
sub.2$racenew <- ifelse(sub.2$race___1 == 1 & sub.2$hisp == 0, 0, ifelse(sub.2$race___1 == 1 & sub.2$race___3 != 1 & sub.2$race___5 == 1, 0, ifelse(sub.2$hisp == 1, 1, ifelse(sub.2$race___2 == 1 & sub.2$race___6 == 0, 2, ifelse(sub.2$race___3 == 1 & sub.2$hisp == 0, 3, ifelse(sub.2$race___4 == 1 & sub.2$hisp == 0, 3, ifelse(sub.2$race___5 == 1 & sub.2$race___6 == 0, 4, ifelse(sub.2$race___1 == 1 & sub.2$race___3 == 1 & sub.2$race___5 == 1, 4, ifelse(sub.2$race___6 == 1 & sub.2$hisp == 0, 4, 5)))))))))

## Collapsing Race Categories ----
sub.2$race_eth <- ifelse(sub.2$racenew == 0, 0, ifelse(sub.2$racenew == 1, 1, ifelse(sub.2$racenew == 2 | sub.2$racenew == 3 | sub.2$racenew == 4, 2, 3)))
sub.2$hisp <- ifelse(sub.2$race_eth == 0, 0, ifelse(sub.2$race_eth == 1, 1, ifelse(sub.2$race_eth == 2, 0, 2)))

## Creating Race ----
#Creating Column that counts 1 as minority and 0 as white
sub.2$racenew2 <- ifelse(sub.2$racenew >=1, 1, 0) 
sub.2$racenew2 %<>% as.factor() 

# Time Variables ----
## Changing Time to Decimals ----
sub.2$wkday_bdtime_decimal <- sapply(strsplit(sub.2$wkday_bdtime,":"),
                                     function(x) {
                                       x <- as.numeric(x)
                                       x[1]+x[2]/60
                                     }
)

sub.2$wkend_bdtime_decimal <- sapply(strsplit(sub.2$wkend_bdtime,":"),
                                     function(x) {
                                       x <- as.numeric(x)
                                       x[1]+x[2]/60
                                     }
)

sub.2$wkday_wake_decimal <- sapply(strsplit(sub.2$wkday_wake,":"),
                                   function(x) {
                                     x <- as.numeric(x)
                                     x[1]+x[2]/60
                                   }
)

sub.2$wkend_wake_decimal <- sapply(strsplit(sub.2$wkend_wake,":"),
                                   function(x) {
                                     x <- as.numeric(x)
                                     x[1]+x[2]/60
                                   }
)


## Make Times Numeric ----
sub.2 <- sub.2 %>% 
  mutate(
    wkday_bdtime_decimal = as.numeric(wkday_bdtime_decimal),
    wkend_bdtime_decimal = as.numeric(wkend_bdtime_decimal),
    wkday_wake_decimal = as.numeric(wkday_wake_decimal),
    wkend_wake_decimal = as.numeric(wkend_wake_decimal)
  )

## Round Times ----
sub.2 <- sub.2 %>%
  mutate(across(
    .cols = c(wkday_bdtime_decimal, wkend_bdtime_decimal, wkday_wake_decimal, wkend_wake_decimal),
    .fns = ~ format(round(., 2), nsmall = 2)
  ))

# Make Times Numeric 
sub.2 <- sub.2 %>% 
  mutate(across(
    .cols = c(wkday_bdtime_decimal, wkend_bdtime_decimal, wkday_wake_decimal, wkend_wake_decimal),
    .fns = as.numeric
  ))

# Create Centered Sleep Variables ----
# Subtract 24 if greater than 1600
sub.2$wkday_bdtime_Center <- as.numeric(0)
sub.2$wkend_bdtime_Center <- as.numeric(0)
sub.2$wkday_wake_Center <- as.numeric(0)
sub.2$wkend_wake_Center <- as.numeric(0)

sub.2 <- sub.2 %>%
  mutate(
    wkday_bdtime_Center = ifelse(wkday_bdtime_decimal > 16.00, wkday_bdtime_decimal - 24.00, wkday_bdtime_decimal),
    wkend_bdtime_Center = ifelse(wkend_bdtime_decimal > 16.00, wkend_bdtime_decimal - 24.00, wkend_bdtime_decimal),
    wkday_wake_Center = ifelse(wkday_wake_decimal > 16.00, wkday_wake_decimal - 24.00, wkday_wake_decimal),
    wkend_wake_Center = ifelse(wkend_wake_decimal > 16.00, wkend_wake_decimal - 24.00, wkend_wake_decimal)
  )

#Test that centering worked
wkdaybed <- data.frame(sub.2$wkday_bdtime_decimal,sub.2$wkday_bdtime_Center)
wkdaybed[1:40, ]

# Sleep Duration ---- 
## Free and Work Days ----
sub.2$SleepWork <- sub.2$wkday_wake_Center - sub.2$wkday_bdtime_Center
sub.2$SleepFree <- sub.2$wkend_wake_Center - sub.2$wkend_bdtime_Center

# Average Sleep ----
sub.2$AverageSleep <- (sub.2$SleepWork + sub.2$SleepFree)/2

# Sleep Difference ----
#Difference in Sleep Work Days vs Work Free Days   #Abs(Sleep duration on work free days - sleep duration on work days)
sub.2$AbsDiffSleepWkendWkday <- abs(sub.2$SleepFree - sub.2$SleepWork)

# Sleep Timing ----
## MSF ----
#(midsleep on free days): sleep offset free days – 0.5sleep duration on free days
sub.2$MSF <- (sub.2$wkend_wake_Center - (0.5*sub.2$SleepFree))

## MSW ----
#(midsleep on work days): sleep offset work days – 0.5sleep duration on work days
sub.2$MSW <- (sub.2$wkday_wake_Center - (0.5*sub.2$SleepWork))

## Social Jetlag ----
#Abs(MSF-MSW)
sub.2$AbsMSF_MSW <- abs(sub.2$MSF - sub.2$MSW)

## Insomnia symptoms ----
#if Q3 in (2,3) or Q4 in (2,3) then insomnia_sympt=1; else insomnia_ sympt=0;
# 0=No, 1=Yes, 2=Sometimes

### Sleep_trouble ---- 
sub.2 <- sub.2 %>%
  mutate(insomnia_sympt_sleepTroblue = ifelse(sleep_trouble >= 2, 1, 0))

### Sleep_stay ----
sub.2 <- sub.2 %>%
  mutate(insomnia_sympt_sleepStay = ifelse(sleep_stay >= 2, 1, 0))

### Sum Sleep Trouble and Stay ----
sub.2$Sum_Insomnia <- sub.2$insomnia_sympt_sleepTroblue + sub.2$insomnia_sympt_sleepStay

# Insomnia Symptoms from Composite ----
# Now create variable from each composite   
sub.2 <- sub.2 %>%
  mutate(insomnia_sympt = ifelse(Sum_Insomnia >= 1, 1, 0))

# Snoring ----
# in (3,4,5) then snoring=1, else snoring=0
sub.2 <- sub.2 %>%
  mutate(snoring = ifelse(snore >= 3, 1, 0))

# Sleep Debt ----
# Sleep Debt and Short Sleep (yes/no; average sleep duration <6h)
sub.2 <- sub.2 %>%
  mutate(SleepDebtShort = ifelse(AverageSleep <= 6, 1, 0))

# Custom histogram function ----
create_histogram <- function(data, column, title, xlab) {
  ggplot(data, aes_string(x = column)) + 
    geom_histogram(binwidth = 1, fill = "red", colour = "black") +
    ggtitle(title) + 
    xlab(xlab) + 
    ylab("Count") + mytheme
}

# Histograms using function ----
wkday_bdtime_Center_Hist <- create_histogram(sub.2, "wkday_bdtime_Center", "Histogram for Weekday Bedtime (bin=1)", "wkday_bdtime_Subtract_24")
wkday_wake_Center_Hist <- create_histogram(sub.2, "wkday_wake_Center", "Histogram for Weekday Waketime (bin=1)", "wkday_waketime_Subtract_24")
wkend_bdtime_Center_Hist <- create_histogram(sub.2, "wkend_bdtime_Center", "Histogram for Weekend Bedtime (bin=1)", "wkend_bdtime_Subtract_24")
wkend_wake_Center_Hist <- create_histogram(sub.2, "wkend_wake_Center", "Histogram for Weekend Waketime (bin=1)", "wkend_wake_Subtract_24")

# Arrange the plots
g <- grid.arrange(wkday_bdtime_Center_Hist,
                  wkday_wake_Center_Hist,
                  wkend_bdtime_Center_Hist,
                  wkend_wake_Center_Hist, nrow = 2, ncol = 2)
g



# Custom another function for histograms ----
create_histogram <- function(data, column, binwidth, title, xlab, breaks_seq) {
  ggplot(data, aes_string(x = column), na.rm = TRUE) + 
    geom_histogram(binwidth = binwidth, fill = "red", colour = 'black') +
    ggtitle(title) + 
    xlab(xlab) + 
    ylab("Count") +
    scale_x_continuous(breaks = seq(breaks_seq[1], breaks_seq[2], breaks_seq[3])) +
    mytheme
}

# Custom function for creating bar plots with frequency labels ----
create_bar_plot <- function(data, column, title, xlab) {
  ggplot(data, aes_string(x = column)) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
    ggtitle(title) + 
    xlab(xlab) + 
    ylab("Count") +
    mytheme
}

# Create histograms
SleepWork_Hist <- create_histogram(sub.2, "SleepWork", 0.5, "Histogram for Weekday Sleep (bin=0.5)", "Average Hours of Sleep (Workday)", c(1, 15, 1))
SleepFree_Hist <- create_histogram(sub.2, "SleepFree", 1, "Histogram for Weekend Sleep (bin=1)", "Average Hours of Sleep (Weekend)", c(1, 15, 1))
AverageSleep_Hist <- create_histogram(sub.2, "AverageSleep", 0.5, "Histogram for Average Sleep (bin=0.5)", "Average Hours of Sleep (Wkend + Wkday)", c(1, 15, 1))
AbsDiffSleepWkendWkday_Hist <- create_histogram(sub.2, "AbsDiffSleepWkendWkday", 0.5, "Histogram for Abs Sleep Difference (bin=0.5)", "Abs Difference of (Sleep Wkend - Sleep Wkday)", c(1, 15, 1))
MSF_Hist <- create_histogram(sub.2, "MSF", 0.5, "Histogram for Mid-Sleep on Free Days (bin=0.5)", "Mid-Sleep on Free Days", c(1, 15, 1))
MSW_Hist <- create_histogram(sub.2, "MSW", 0.5, "Histogram for Mid-Sleep on Work Days (bin=0.5)", "Mid-Sleep on Work Days", c(1, 15, 1))
AbsMSF_MSW_Hist <- create_histogram(sub.2, "AbsMSF_MSW", 0.5, "Histogram for Social Jetlag (bin=0.5)", "Social Jetlag (MSF-MSW)", c(1, 15, 1))

# Create bar plots
SleepDebtShort_Freq <- create_bar_plot(sub.2, "SleepDebtShort", "Frequency of Sleep Debt/Short Sleep", "Frequency of Sleep Debt/Short Sleep")
insomnia_sympt_Freq <- create_bar_plot(sub.2, "insomnia_sympt", "Frequency of Insomnia Symptoms", "Frequency of Insomnia Symptoms")
snoring_Freq <- create_bar_plot(sub.2, "snoring", "Frequency of Snoring", "Frequency of Snoring")

# Arrange the plots
grid.arrange(SleepWork_Hist,
             SleepFree_Hist,
             AverageSleep_Hist,
             AverageSleep_Hist,
             AbsDiffSleepWkendWkday_Hist,
             MSF_Hist,
             MSW_Hist,
             AbsMSF_MSW_Hist, nrow = 4, ncol = 2)