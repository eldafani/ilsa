library("tidyverse")
library("haven")
library("intsvy")
library("reshape2")

# Data with country iso_codes
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
cnt <- read.csv(file.path(dir, "iso_code.csv"))

# Read PISA-D data
dir <- "/home/eldani/eldani/International LSA/PISA-D/Data"
 
# pisa <- pisa.select.merge(folder = dir,
#                           student.file="CY1MDAI_STU_QQQ.sav",
#                           school.file="CY1MDAI_SCH_QQQ.sav",
#                           student= c("ESCS15", "ST004D01T", "ST021Q01TA", "RURALSTR"),
#                           school = c("SC001Q01TA"))

pisa <- read_sav(file.path(dir, "CY1MDAI_STU_QQQ.sav"))

# RURALSTR 1 urban, 2 rural
# ST004D01T 1 female, 2 male
# ST021Q01TA 1 language of the test, 2 other language


pisa <- pisa %>%
  group_by(CNT) %>%
  mutate(Wealth = cut(ESCS15, breaks=quantile(ESCS15, probs=seq(0,1, by=0.2), na.rm=TRUE),
                      labels = c(1:5),  include.lowest=TRUE))


pisa <- mutate(pisa, COUNTRY = CNT, Sex = factor(ST004D01T, labels = c("Female", "Male")), 
               Location = factor(RURALSTR, labels = c("Urban", "Rural")), 
               Language = factor(ST021Q01TA, labels = c("Yes", "No")))

# MPL calculate
pisa_r <- ilsa_sum(pvnames = "READ", 
                   cutoff = c(334.75, 407.47, 480.18, 552.89),
                   config = pisa_conf,
                   data = pisa,
                   year = NA,
                   level = "Upper secondary",
                   grade = NA,
                   survey = "PISA-D",
                   prefix = "r")



pisa_m <- ilsa_sum(pvnames = "MATH", 
                   cutoff = c(357.77, 420.07, 482.38, 544.68),
                   config = pisa_conf,
                   data = pisa,
                   year = NA,
                   level = "Upper secondary",
                   grade = NA,
                   survey = "PISA-D",
                   prefix = "m")

pisa_s <- ilsa_sum(pvnames = "SCIE",
                   cutoff = c(334.94, 409.54, 484.14, 558.73),
                   config
                   = pisa_conf,
                   data = pisa,
                   year = NA,
                   level = "Upper secondary",
                   grade = NA,
                   survey = "PISA-D",
                   prefix = "s")


## Append datasets
datasets <- mget(grep("_r$|_m$|_s$", ls(), value = TRUE))
wide_data <- do.call(dplyr::bind_rows, datasets)

## Create year variable
py <- tibble(CNT = c("ECU","GTM","HND","KHM", "PRY", "SEN", "ZMB"), YEAR =  c(2014, 2015, 2016, 2016, 2015, 2015, 2014))
wide_data <- mutate(wide_data, year = py$YEAR[match(wide_data$COUNTRY, py$CNT)])

## Create country id
names(wide_data)[1] <- "iso_code3"
wide_data$COUNTRY <- cnt[match(wide_data$iso_code3, cnt$iso_code3), "country"]
wide_data$COUNTRY <- ifelse(is.na(wide_data$COUNTRY), wide_data$iso_code3, wide_data$COUNTRY)
wide_data$iso_num <- cnt[match(wide_data$COUNTRY, cnt$country), "iso_num"]

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
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "pisad_mpl.csv"))
