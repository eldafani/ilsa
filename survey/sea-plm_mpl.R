### ilsa_summarize: Examples

library("tidyverse")
library("dplyr")
library("haven")
library("reshape2")
library("devtools")
#install_github("eldafani/intsvy")
library("intsvy") # version from github


# Read iso codes
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
cnt <- read.csv(file.path(dir, "iso_code.csv"))

# Read data
dir <- "/home/eldani/eldani/International LSA/SEA-PLM/Data/SEA-PLM_Regional_Students-sav"
student <- read_spss(file.path(dir, "SEA-PLM_Regional_Students.sav"))
dir <- "/home/eldani/eldani/International LSA/SEA-PLM/Data/SEA-PLM_Regional_Schools-sav"
school <- read_spss(file.path(dir, "SEA-PLM_Regional_Schools.sav"))

# Merge data
sea <- left_join(student, school, by = c("CNT", "SchID"))

# Create grouping variables
sea$COUNTRY <- sea$CNT

sea <- sea %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(SES)) != n() & 
           length(unique(quantile(SES, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(Wealth = cut(SES, breaks=quantile(SES, probs=seq(0,1, by=0.2), na.rm=TRUE),
                      labels = c(1:5),  include.lowest=TRUE))


sea$Sex <- factor(sea$Gender, 1:2, labels = c("Female", "Male"))

sea$Language <- factor(sea$S_LANG, 0:1, labels = c("No", "Yes"))

sea$Location <- factor(sea$SC09Q01, 1:5, labels = c("Rural", rep("Urban", 4)))


# Calculate MPLs

sea_r <- ilsa_sum(pvnames = "R", 
                   cutoff = c(304, 317),
                   config = sea_conf,
                   data = sea,
                   year = 2019,
                   level = "Primary",
                   grade = 5,
                   survey = "SEA-PLM",
                   prefix = "r",
                   pv1 = "PV1_R")


sea_m <- ilsa_sum(pvnames = "M", 
                   cutoff = c(295, 308, 321, 334),
                   config = sea_conf,
                   data = sea,
                   year = 2019,
                   level = "Primary",
                   grade = 5,
                   survey = "SEA-PLM",
                   prefix = "m",
                   pv1 = "PV1_M")


## Append datasets
datasets <- mget(grep("_r$|_m$|_s$", ls(), value = TRUE))
wide_data <- do.call(dplyr::bind_rows, datasets)

## Create country id
wide_data$iso_code3 <- as.character(wide_data$COUNTRY)
wide_data$COUNTRY <- cnt[match(wide_data$iso_code3, cnt$iso_code3), "country"]
wide_data$COUNTRY <- ifelse(is.na(wide_data$COUNTRY), wide_data$iso_code3, wide_data$COUNTRY)
wide_data$iso_num <- cnt[match(wide_data$COUNTRY, cnt$country), "iso_num"]

# remove iso_code3 with more than 3 characters
wide_data$iso_code3 <- ifelse(nchar(wide_data$iso_code3) >3, NA, wide_data$iso_code3)

## Descriptives
summary(wide_data)
cat.vars <- grep("_m|_se|_no", names(wide_data), invert = TRUE, value = TRUE)
sapply(wide_data[cat.vars], table)

## Export

# reorder columns for wide
vars <- c("category", "Sex",  "Location", "Wealth", "Language")
vars <- intersect(names(wide_data), vars)
num.vars <- setdiff(names(wide_data), cat.vars)
id.vars <- setdiff(cat.vars, vars)

wide_data <- wide_data[c(id.vars, vars, num.vars)]

dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "sea-plm_mpl.csv"))
