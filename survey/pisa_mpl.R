### ilsa_summarize: Examples

library("tidyverse")
library("dplyr")
library("haven")
library("reshape2")
library("devtools")
#install_github("eldafani/intsvy")
library("intsvy") # version from github

# Data with country iso_codes
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
cnt <- read.csv(file.path(dir, "iso_code.csv"))
# country data is complete

## PISA 2015

dir <- "/home/eldani/eldani/International LSA/PISA/2015/Data"

# pisa.var.label(folder=dir, school.file= "CY6_MS_CMB_SCH_QQQ.sav", student.file="CY6_MS_CMB_STU_QQQ.sav", output = dir)
 

pisa <- pisa.select.merge(folder = dir,
                          student.file="CY6_MS_CMB_STU_QQQ.sav",
                          school.file="CY6_MS_CMB_SCH_QQQ.sav",
                          student= c("ESCS", "ST004D01T", "ST022Q01TA"),
                          school = c("SC001Q01TA"))

#pisa$COUNTRY <- cnt[match(pisa$CNT, cnt$iso_code3), "country"]
pisa$COUNTRY <- pisa$CNT

pisa$Sex <- factor(pisa$ST004D01T, labels = c("Female", "Male"))
pisa$Language <- factor(pisa$ST022Q01TA, labels = c("Yes", "No"))

pisa$Location <- factor(pisa$SC001Q01TA, labels = c("Rural", rep("Urban", 4)))

pisa <- pisa %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ESCS)) != n() & 
           length(unique(quantile(ESCS, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
    mutate(Wealth = cut(ESCS, breaks=quantile(ESCS, probs=seq(0,1, by=0.2), na.rm=TRUE),
        labels = c(1:5),  include.lowest=TRUE))

pisa15_r <- ilsa_sum(pvnames = "READ", 
                   cutoff = c(334.75, 407.47, 480.18, 552.89),
                   config = pisa_conf,
                   data = pisa,
                   year = 2015,
                   level = "Upper secondary",
                   grade = NA,
                   survey = "PISA",
                   prefix = "r")


pisa15_m <- ilsa_sum(pvnames = "MATH", 
                   cutoff = c(357.77, 420.07, 482.38, 544.68),
                   config = pisa_conf,
                   data = pisa,
                   year = 2015,
                   level = "Upper secondary",
                   grade = NA,
                   survey = "PISA",
                   prefix = "m")

pisa15_s <- ilsa_sum(pvnames = "SCIE",
                   cutoff = c(334.94, 409.54, 484.14, 558.73),
                   config = pisa_conf,
                   data = pisa,
                   year = 2015,
                   level = "Upper secondary",
                   grade = NA,
                   survey = "PISA",
                   prefix = "s")


## PISA 2012

dir <- "/home/eldani/eldani/International LSA/PISA/2012/Data"

# pisa.var.label(folder=dir, school.file="INT_SCQ12_DEC03.sav",
#                student.file="INT_STU12_DEC03.sav", output =dir)

pisa <- pisa.select.merge(folder=dir, school.file="INT_SCQ12_DEC03.sav",
                                student.file="INT_STU12_DEC03.sav",
                                student= c("ESCS", "ST04Q01", "ST25Q01"),
                                school = c("SC03Q01"))


pisa$COUNTRY <- pisa$CNT

pisa$Sex <- factor(pisa$ST04Q01, labels = c("Female", "Male"))
pisa$Language <- factor(pisa$ST25Q01, labels = c("Yes", "No"))

pisa$Location <- factor(pisa$SC03Q01, labels = c("Rural", rep("Urban", 4)))

pisa <- pisa %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ESCS)) != n() & 
           length(unique(quantile(ESCS, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(Wealth = cut(ESCS, breaks=quantile(ESCS, probs=seq(0,1, by=0.2), na.rm=TRUE),
                      labels = c(1:5),  include.lowest=TRUE))

pisa12_r <- ilsa_sum(pvnames = "READ", 
                     cutoff = c(334.75, 407.47, 480.18, 552.89),
                     config = pisa_conf,
                     data = pisa,
                     year = 2012,
                     level = "Upper secondary",
                     grade = NA,
                     survey = "PISA",
                     prefix = "r")


pisa12_m <- ilsa_sum(pvnames = "MATH", 
                     cutoff = c(357.77, 420.07, 482.38, 544.68),
                     config = pisa_conf,
                     data = pisa,
                     year = 2012,
                     level = "Upper secondary",
                     grade = NA,
                     survey = "PISA",
                     prefix = "m")

pisa12_s <- ilsa_sum(pvnames = "SCIE",
                     cutoff = c(334.94, 409.54, 484.14, 558.73),
                     config = pisa_conf,
                     data = pisa,
                     year = 2012,
                     level = "Upper secondary",
                     grade = NA,
                     survey = "PISA",
                     prefix = "s")

## Append datasets
datasets <- mget(grep("_r$|_m$|_s$", ls(), value = TRUE))
wide_data <- do.call(dplyr::bind_rows, datasets)

## Create country id
names(wide_data)[1] <- "iso_code3"
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
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "pisa_mpl.csv"))
