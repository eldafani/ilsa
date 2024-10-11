### ilsa_summarize: MILO 2021

library("tidyverse")
library("haven")
library("reshape2")
library("intsvy") # version from github

# Data with country iso_codes
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
cnt <- read.csv(file.path(dir, "iso_code.csv"))

# MILO 2021
dir <- "/home/eldani/eldani/International LSA/MILO"
student <- read_spss(file=file.path(dir, "MILO_INT_Student_Final_DB_(2022.01.18).sav"))
school <- read_spss(file=file.path(dir, "MILO_INT_SCQ_Final_DB_(2022.01.18).sav"))
milo <- merge(student, school, by = c("CNT", "CNT_num", "Country", "MILO_SchID"), all=TRUE)

# Urban areas are defined as locations of more than 100,000 (SEA-PLM, ICILS)

milo <- milo %>% 
  mutate(iso_code3= CNT,
         country = Country,
         Sex = factor(S_Gender, levels = c(1:2), labels = c("Female", "Male")),
         Type = factor(SC03, levels = c(1:2), labels = c("Public", "Private")),
         Location = factor(SC04, levels = c(1:5), 
                           labels = c(rep("Rural", 3), rep("Urban", 2)))) %>%
  group_by(iso_code3) %>% 
  mutate(Wealth = cut(S_WEALTH, breaks=quantile(S_WEALTH, 
                                                probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5), include.lowest=TRUE))

groups <- c("Location", "Sex",  "Type", "Wealth")
read <- paste0("PL_PV", 1:5, "_MILO_r")
math <- paste0("PL_PV", 1:5, "_MILO_m")
id <- c("iso_code3", "country")

wide_data <- merge(wide_bind(milo, read, id, groups) %>% rename(rlevel2_m=value, rlevel2_no=n), 
             wide_bind(milo, math, id, groups) %>% rename(mlevel2_m=value, mlevel2_no=n), 
             by = c(id, "category", groups))

wide_data <- cbind(wide_data, survey= "MILO", grade=6, year = 2021)

## Export

dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "milo_mpl.csv"))
