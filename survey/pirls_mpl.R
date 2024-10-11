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

# PIRLS 2011

## Read data
dir <- "/home/eldani/eldani/International LSA/PIRLS/PIRLS 2011/Data"

#pirls.var.label(dir)

pirls <- pirls.select.merge(folder= dir,
         student= c("ITSEX", "ASBG05A", "ASBG05B", "ASBG05C", "ASBG05D", "ASBG05D",
                    "ASBG04", "ASBG03"), 
         home= c("ASBH14", "ASDHEDUP", "ASDHOCCP", "ASBH20A", "ASBH20B"),
         school= c("ACBG05B"))

# Grouping variables

pirls$IDCNTRYL <- cnt[match(pirls$IDCNTRY, cnt$iso_num), "country"]
pirls$COUNTRY <- cnt[match(pirls$IDCNTRY, cnt$iso_num), "iso_code3"]
pirls$COUNTRY <- ifelse(is.na(pirls$COUNTRY), pirls$IDCNTRYL, pirls$COUNTRY)


pirls <- droplevels(pirls[!is.na(pirls$COUNTRY), ])

pirls$Sex <- factor(pirls$ITSEX, c(1,2), c("Female", "Male"))

# ACBG05B
# 1: Urban–Densely populated; 2: Suburban–On fringe or outskirts of urban area; 
# 3: Medium size city or large town; 4: Small town or village; 5: Remote rural

pirls$Location <- factor(pirls$ACBG05B, c(1:4, 5), c(rep("Urban", 4), "Rural"))

pirls$Language <- factor(pirls$ASBG03, c(1:3,4), c(rep("Yes",3), "No"))

## Create Wealth quantles (Caro and Cortes, 2012)

### Parental education
pirls$pared <- pirls$ASDHEDUP
pirls$pared[pirls$pared==6] <-NA
pirls$pared <- 6-pirls$pared

### Parental occupation
# Father's ISEI
pirls$dadsei <- pirls$ASBH20A

pirls$dadsei[pirls$dadsei==1] <-22
pirls$dadsei[pirls$dadsei==2] <-57
pirls$dadsei[pirls$dadsei==3] <-49
pirls$dadsei[pirls$dadsei==4] <-45
pirls$dadsei[pirls$dadsei==5] <-31
pirls$dadsei[pirls$dadsei==6] <-37
pirls$dadsei[pirls$dadsei==7] <-33
pirls$dadsei[pirls$dadsei==8] <-24
pirls$dadsei[pirls$dadsei==9] <-67
pirls$dadsei[pirls$dadsei==10] <-73
pirls$dadsei[pirls$dadsei==11] <-52
pirls$dadsei[pirls$dadsei==12] <-NA

# Mother
pirls$momsei <- pirls$ASBH20B

pirls$momsei[pirls$momsei==1] <-22
pirls$momsei[pirls$momsei==2] <-57
pirls$momsei[pirls$momsei==3] <-49
pirls$momsei[pirls$momsei==4] <-45
pirls$momsei[pirls$momsei==5] <-31
pirls$momsei[pirls$momsei==6] <-37
pirls$momsei[pirls$momsei==7] <-33
pirls$momsei[pirls$momsei==8] <-24
pirls$momsei[pirls$momsei==9] <-67
pirls$momsei[pirls$momsei==10] <-73
pirls$momsei[pirls$momsei==11] <-52
pirls$momsei[pirls$momsei==12] <-NA

pirls$parsei <- with(pirls, pmax(dadsei, momsei, na.rm=T))

### Home possessions
pirls$pc <- pirls$ASBG05A
pirls$desk <- pirls$ASBG05B
pirls$room <- pirls$ASBG05C
pirls$internet <- pirls$ASBG05D

pos <- c("pc", "desk", "room", "internet")

pirls[pos][pirls[pos]==2] = 0

pirls$home <- apply(pirls[pos], 1, mean, na.rm=TRUE)


pirls$books <- pirls$ASBH14


### Create SES variable
ses.var <- c("pared", "parsei", "home", "books")

# SES with max values
ses.m <- princomp(na.omit(pirls[ses.var]), cor=T)
plot(ses.m,type="lines") # scree plot 
ses <- ses.m$scores[,1, drop=F]

# Match SES score with original data
pirls$ses <- ses[match(rownames(pirls), rownames(ses))] 

## Create Wealth quantiles

with(pirls, tapply(ses, COUNTRY, function(x) sum(is.na(x))/length(x)))

pirls <- pirls %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ses)) != n() & 
           length(unique(quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(ses_mis = sum(is.na(ses)) == n(), 
         Wealth = cut(ses, breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5),
                      include.lowest=TRUE))
# 
# pirls$Wealth <- with(pirls, cut(ses,
#                                 breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE),
#                                 labels = c(1:5),
#                                 include.lowest=TRUE))

# Reading performance
pirls11_r <- ilsa_sum(pvnames = "ASRREA", 
                    cutoff =   c(400, 475, 550),
                    config = pirls_conf,
                    data = pirls,
                    year = 2011,
                    level = "Primary",
                    grade = 4,
                    survey = "PIRLS",
                    prefix = "r")



## PIRLS 2006

dir <- "/home/eldani/eldani/International LSA/PIRLS/PIRLS 2006/Data"


pirls <- pirls.select.merge(folder= dir,
         student= c("ITSEX", "ASBGTA1", "ASBGTA2", "ASBGTA3", "ASBGTA4", 
                    "ASBGTA5", "ASBGTA6", "ASBGLNGH"), 
        home= c("ASBHBOOK", "ASDHEDUP", "ASDHOCCP", "ASBHMJF", "ASBHMJM"),
        school= c("ACBGCOMM"))

# Grouping variables

pirls$IDCNTRYL <- cnt[match(pirls$IDCNTRY, cnt$iso_num), "country"]
pirls$COUNTRY <- cnt[match(pirls$IDCNTRY, cnt$iso_num), "iso_code3"]
pirls$COUNTRY <- ifelse(is.na(pirls$COUNTRY), pirls$IDCNTRYL, pirls$COUNTRY)


pirls <- droplevels(pirls[!is.na(pirls$COUNTRY), ])

pirls$Sex <- factor(pirls$ITSEX, c(1,2), c("Female", "Male"))

# ACBG05B
# 1: Urban–Densely populated; 2: Suburban–On fringe or outskirts of urban area; 
# 3: Medium size city or large town; 4: Small town or village; 5: Remote rural

pirls$Location <- factor(pirls$ACBGCOMM, c(1:2, 3), c(rep("Urban", 2), "Rural"))

pirls$Language <- factor(pirls$ASBGLNGH, c(1:3), c(rep("Yes",2), "No"))

## Create Wealth quantles (Caro and Cortes, 2012)

### Parental education
pirls$pared <- pirls$ASDHEDUP
pirls$pared[pirls$pared==6] <-NA
pirls$pared <- 6-pirls$pared

### Parental occupation
# Father's ISEI
pirls$dadsei <- pirls$ASBHMJF

pirls$dadsei[pirls$dadsei==1] <-22
pirls$dadsei[pirls$dadsei==2] <-57
pirls$dadsei[pirls$dadsei==3] <-49
pirls$dadsei[pirls$dadsei==4] <-45
pirls$dadsei[pirls$dadsei==5] <-31
pirls$dadsei[pirls$dadsei==6] <-37
pirls$dadsei[pirls$dadsei==7] <-33
pirls$dadsei[pirls$dadsei==8] <-24
pirls$dadsei[pirls$dadsei==9] <-67
pirls$dadsei[pirls$dadsei==10] <-73
pirls$dadsei[pirls$dadsei==11] <-52
pirls$dadsei[pirls$dadsei==12] <-NA

# Mother
pirls$momsei <- pirls$ASBHMJM

pirls$momsei[pirls$momsei==1] <-22
pirls$momsei[pirls$momsei==2] <-57
pirls$momsei[pirls$momsei==3] <-49
pirls$momsei[pirls$momsei==4] <-45
pirls$momsei[pirls$momsei==5] <-31
pirls$momsei[pirls$momsei==6] <-37
pirls$momsei[pirls$momsei==7] <-33
pirls$momsei[pirls$momsei==8] <-24
pirls$momsei[pirls$momsei==9] <-67
pirls$momsei[pirls$momsei==10] <-73
pirls$momsei[pirls$momsei==11] <-52
pirls$momsei[pirls$momsei==12] <-NA

pirls$parsei <- with(pirls, pmax(dadsei, momsei, na.rm=T))

### Home possessions
pirls$pc <- pirls$ASBGTA1
pirls$desk <- pirls$ASBGTA2
pirls$room <- pirls$ASBGTA5
pirls$mobile <- pirls$ASBGTA6

pos <- c("pc", "desk", "room", "mobile")

pirls[pos][pirls[pos]==2] = 0

pirls$home <- apply(pirls[pos], 1, mean, na.rm=TRUE)


pirls$books <- pirls$ASBHBOOK


### Create SES variable
ses.var <- c("pared", "parsei", "home", "books")

# SES with max values
ses.m <- princomp(na.omit(pirls[ses.var]), cor=T)
plot(ses.m,type="lines") # scree plot 
ses <- ses.m$scores[,1, drop=F]

# Match SES score with original data
pirls$ses <- ses[match(rownames(pirls), rownames(ses))] 

## Create Wealth quantiles

with(pirls, tapply(ses, COUNTRY, function(x) sum(is.na(x))/length(x)))

pirls <- pirls %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ses)) != n() & 
           length(unique(quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(ses_mis = sum(is.na(ses)) == n(), 
         Wealth = cut(ses, breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5),
                      include.lowest=TRUE))
# 
# pirls$Wealth <- with(pirls, cut(ses,
#                                 breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE),
#                                 labels = c(1:5),
#                                 include.lowest=TRUE))



pirls06_r <- ilsa_sum(pvnames = "ASRREA", 
                      cutoff =   c(400, 475, 550),
                      config = pirls_conf,
                      data = pirls,
                      year = 2006,
                      level = "Primary",
                      grade = 4,
                      survey = "PIRLS",
                      prefix = "r")


## Append datasets
datasets <- mget(grep("_r$|_m$|_s$", ls(), value = TRUE))
wide_data <- do.call(dplyr::bind_rows, datasets)

names(wide_data)[7:15] <- c(paste0("rlevel", 2:4, "_m"), 
                          paste0("rlevel", 2:4, "_se"),
                          paste0("rlevel", 2:4, "_no"))


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
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "pirls_mpl.csv"))
