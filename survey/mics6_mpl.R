### MICS6 data

library("tidyverse")
library("haven")
library("reshape2")
library("janitor")
library("ggplot2")
library("stringr")
library("ggpubr")
library("purrr")
library("readxl")


dir <- "/home/eldani/eldani/International LSA/MICS6_FLS/"
df <- readRDS(file.path(dir, "mics6.rds"))

# windex5	1=from family in the poorest wealth quintile; 5=from family in the richest wealth quintile
# sex HL4 male 1, female 2
# HH6	1=urban, 2=rural
# fsweight	Sampling weights

df <- mutate(df, 
             read = readskill, math = numbskill, 
             iso_code3 = iso_code,
             Sex = factor(HL4, 1:2, c("Male", "Female")), 
             Location = factor(HH6, 1:2, c("Urban", "Rural")), 
             Wealth = factor(windex5, 1:5, 1:5), 
             weight = fsweight)
### Aggregate
dvs <- c("math", "read")
ids <- c("iso_code3", "year", "grade")
groups <- c("Location", "Sex", "Wealth")
vars <- c(dvs, ids, groups, "weight")

df <- filter(df, grade %in% c(3, 6))
wide_data <- wide_bind(df, dvs, ids, groups)

wide_data <- wide_data %>% 
             mutate(rlevel2_no=n, mlevel2_no=n, survey = "MICS6") %>%
             rename(rlevel2_m = read, mlevel2_m = math) %>%
             select(-n)

ggplot(filter(wide_data, category=="Total"), 
       aes(x= reorder(iso_code3, mlevel2_m), y= mlevel2_m, fill= as.factor(grade))) +
  geom_bar(stat="identity", position = "dodge")

## Export

dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "mics6_mpl.csv"))

