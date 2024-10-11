### ilsa_summarize: pasec 2014 and 19

library("tidyverse")
library("haven")
library("reshape2")
library("intsvy") # version from github

# Data with country iso_codes
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
cnt <- read.csv(file.path(dir, "iso_code.csv"))
# country data is complete


# PASEC 2014

## Read data
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/PASEC"
pasec2 <- read_dta(file.path(dir, "PASEC2014_GRADE2.dta"))
pasec6 <- read_dta(file.path(dir, "PASEC2014_GRADE6.dta"))

## Create grouping variables

### Grade 2
pasec2$COUNTRY <- cnt[match(pasec2$PAYS, cnt$pasec), "iso_code3"]

pasec2$Sex <- factor(pasec2$qe22, c(1,2), c("Male", "Female"))
pasec2$Language <- factor(pasec2$qe25, c(1:3), c(rep("Yes",2), "No"))
pasec2$Location <- factor(pasec2$qd24, c(1:4), c(rep("Urban",2), rep("Rural",2)))

### Grade 6
pasec6$COUNTRY <- cnt[match(pasec6$PAYS, cnt$pasec), "iso_code3"]
pasec6$Sex <- factor(pasec6$qe62, c(1,2), c("Male", "Female"))
pasec6$Language <- factor(pasec6$qe615, c(1:3), c(rep("Yes",2), "No"))
pasec6$Location <- factor(pasec6$qd24, c(1:4), c(rep("Urban",2), rep("Rural",2)))

pasec6 <- pasec6 %>%
  group_by(COUNTRY) %>%
  mutate(Wealth = cut(SES, breaks=quantile(SES, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5), include.lowest=TRUE))


pasec2_r<- ilsa_sum(pvnames = "LECT", 
                    cutoff = c(469.54, 539.96, 610.38),
                    config = pasec_conf,
                    data = pasec2,
                    year = 2014,
                    level = "Primary",
                    grade = 2,
                    survey = "PASEC",
                    prefix = "r") # 'r' for reading, 'm' for maths and 's' for science

pasec2_m<- ilsa_sum(pvnames = "MATHS", 
                    cutoff =  c(400.34, 489.03, 577.73), # 66.9
                    config = pasec_conf,
                    data = pasec2,
                    year = 2014,
                    level = "Primary",
                    grade = 2,
                    survey = "PASEC",
                    prefix = "m")


pasec6_r <- ilsa_sum(pvnames = "LECT", 
                     cutoff =  c(518.37, 595.05),
                     config = pasec_conf,
                     data = pasec6,
                     year = 2014,
                     level = "Primary",
                     grade = 6,
                     survey = "PASEC",
                     prefix = "r")


pasec6_m <- ilsa_sum(pvnames = "MATHS", 
                     cutoff =   c(521.46, 609.64), #68.1,
                     config = pasec_conf,
                     data = pasec6,
                     year = 2014,
                     level = "Primary",
                     grade = 6,
                     survey = "PASEC",
                     prefix = "m")


pasec14 <- dplyr::bind_rows(merge(pasec2_m, pasec2_r), merge(pasec6_m, pasec6_r))

# PASEC 2019

## Read data
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/PASEC"
pasec2 <- read_dta(file.path(dir, "PASEC2019_GRADE2.dta"))
pasec6 <- read_dta(file.path(dir, "PASEC2019_GRADE6.dta"))

## Recode weights
pasec2$JKREP[pasec2$JKREP==2] <- 1
pasec6$JKREP[pasec6$JKREP==2] <- 1

## Create grouping variables

#write.csv(do.call(rbind, sapply(pasec6, function(x) attr(x, "label"))), "pasec19.csv")

### Grade 2
pasec2$COUNTRY <- cnt[match(pasec2$PAYS, cnt$pasec), "iso_code3"]

pasec2$Sex <- factor(pasec2$qe23, c(1,2), c("Male", "Female"))
pasec2$Language <- factor(pasec2$qe28, c(1:4), c(rep("Yes",3), "No"))
pasec2$Location <- factor(pasec2$qd31, c(1:4), c(rep("Urban",2), rep("Rural",2)))

### Grade 6
pasec6$COUNTRY <- cnt[match(pasec6$PAYS, cnt$pasec), "iso_code3"]

pasec6$Sex <- factor(pasec6$qe63, c(1,2), c("Male", "Female"))
pasec6$Language <- factor(pasec6$qe620, c(1:4), c(rep("Yes",3), "No"))
pasec6$Location <- factor(pasec6$qd31, c(1:4), c(rep("Urban",2), rep("Rural",2)))

pasec6 <- pasec6 %>%
  group_by(COUNTRY) %>%
  mutate(Wealth = cut(ses, breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                  labels = c(1:5), include.lowest=TRUE))
  
pasec2_r<- ilsa_sum(pvnames = "LECT", 
         cutoff = c(469.54, 539.96, 610.38),
         config = pasec_conf,
         data = pasec2,
         year = 2019,
         level = "Primary",
         grade = 2,
         survey = "PASEC",
         prefix = "r") # 'r' for reading, 'm' for maths and 's' for science

pasec2_m<- ilsa_sum(pvnames = "MATHS", 
                    cutoff =  c(400.34, 489.03, 577.73), # 66.9
                    config = pasec_conf,
                    data = pasec2,
                    year = 2019,
                    level = "Primary",
                    grade = 2,
                    survey = "PASEC",
                    prefix = "m")


pasec6_r <- ilsa_sum(pvnames = "LECT", 
                       cutoff =  c(518.37, 595.05),
                       config = pasec_conf,
                       data = pasec6,
                       year = 2019,
                       level = "Primary",
                       grade = 6,
                       survey = "PASEC",
                       prefix = "r")


pasec6_m <- ilsa_sum(pvnames = "MATHS", 
                     cutoff =   c(521.46, 609.64), #68.1,
                     config = pasec_conf,
                     data = pasec6,
                     year = 2019,
                     level = "Primary",
                     grade = 6,
                     survey = "PASEC",
                     prefix = "m")


## Append datasets
# datasets <- mget(grep("_r$|_m$|_s$", ls(), value = TRUE))
# wide_data <- do.call(dplyr::bind_rows, datasets)

pasec19 <- dplyr::bind_rows(merge(pasec2_m, pasec2_r), merge(pasec6_m, pasec6_r))

wide_data <- dplyr::bind_rows(pasec14, pasec19)


## Create country id
names(wide_data)[1] <- "iso_code3"
wide_data$COUNTRY <- cnt[match(wide_data$iso_code3, cnt$iso_code3), "country"]
wide_data$COUNTRY <- ifelse(is.na(wide_data$COUNTRY), wide_data$iso_code3, wide_data$COUNTRY)
wide_data$iso_num <- cnt[match(wide_data$COUNTRY, cnt$country), "iso_num"]

# remove iso_code3 with more than 3 characters
wide_data$iso_code3 <- ifelse(nchar(as.character(wide_data$iso_code3)) >3, NA, 
                              as.character(wide_data$iso_code3))

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
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "pasec_mpl.csv"))
