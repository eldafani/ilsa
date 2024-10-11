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

# TIMSS 2015 Grade 4

## Read data
dir <- "/home/eldani/eldani/International LSA/TIMSS/TIMSS 15/Grade 4/Data"

#timssg4.var.label(dir, output = dir)

timss <- timssg4.select.merge(folder= dir,
         student= c("ITSEX", "ASBG05A", "ASBG05B", "ASBG05C", "ASBG05D","ASBG05E",
                    "ASBG03"), 
         home= c("ASBH14", "ASDHEDUP", "ASDHOCCP", "ASBH23A", "ASBH23B"),
         school= c("ACBG05B"))


# Grouping variables
timss$IDCNTRYL <- cnt[match(timss$IDCNTRY, cnt$iso_num), "country"]
timss$COUNTRY <- cnt[match(timss$IDCNTRY, cnt$iso_num), "iso_code3"]
timss$COUNTRY <- ifelse(is.na(timss$COUNTRY), timss$IDCNTRYL, timss$COUNTRY)


timss <- droplevels(timss[!is.na(timss$COUNTRY), ])

timss$Sex <- factor(timss$ITSEX, c(1,2), c("Female", "Male"))

# ACBG05B
# 1: Urban–Densely populated; 2: Suburban–On fringe or outskirts of urban area; 
# 3: Medium size city or large town; 4: Small town or village; 5: Remote rural

timss$Location <- factor(timss$ACBG05B, c(1:4, 5), c(rep("Urban", 4), "Rural"))

timss$Language <- factor(timss$ASBG03, c(1:3,4), c(rep("Yes",3), "No"))

## Create Wealth quantles (Caro and Cortes, 2012)

### Parental education
timss$pared <- timss$ASDHEDUP
timss$pared[timss$pared==6] <-NA
timss$pared <- 6-timss$pared

### Parental occupation
# Father's ISEI
timss$dadsei <- timss$ASBH23A

timss$dadsei[timss$dadsei==1] <-22
timss$dadsei[timss$dadsei==2] <-57
timss$dadsei[timss$dadsei==3] <-49
timss$dadsei[timss$dadsei==4] <-45
timss$dadsei[timss$dadsei==5] <-31
timss$dadsei[timss$dadsei==6] <-37
timss$dadsei[timss$dadsei==7] <-33
timss$dadsei[timss$dadsei==8] <-24
timss$dadsei[timss$dadsei==9] <-67
timss$dadsei[timss$dadsei==10] <-73
timss$dadsei[timss$dadsei==11] <-52
timss$dadsei[timss$dadsei==12] <-NA

# Mother
timss$momsei <- timss$ASBH23B

timss$momsei[timss$momsei==1] <-22
timss$momsei[timss$momsei==2] <-57
timss$momsei[timss$momsei==3] <-49
timss$momsei[timss$momsei==4] <-45
timss$momsei[timss$momsei==5] <-31
timss$momsei[timss$momsei==6] <-37
timss$momsei[timss$momsei==7] <-33
timss$momsei[timss$momsei==8] <-24
timss$momsei[timss$momsei==9] <-67
timss$momsei[timss$momsei==10] <-73
timss$momsei[timss$momsei==11] <-52
timss$momsei[timss$momsei==12] <-NA

timss$parsei <- with(timss, pmax(dadsei, momsei, na.rm=T))

### Home possessions
timss$pc <- timss$ASBG05A
timss$desk <- timss$ASBG05B
timss$room <- timss$ASBG05C
timss$internet <- timss$ASBG05D

pos <- c("pc", "desk", "room", "internet")

timss[pos][timss[pos]==2] = 0

timss$home <- apply(timss[pos], 1, mean, na.rm=TRUE)


timss$books <- timss$ASBH14


### Create SES variable
ses.var <- c("pared", "parsei", "home", "books")

# SES with max values
ses.m <- princomp(na.omit(timss[ses.var]), cor=T)
plot(ses.m,type="lines") # scree plot 
ses <- ses.m$scores[,1, drop=F]

# Match SES score with original data
timss$ses <- ses[match(rownames(timss), rownames(ses))] 

## Create Wealth quantiles

with(timss, tapply(ses, COUNTRY, function(x) sum(is.na(x))/length(x)))

timss <- timss %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ses)) != n() & 
           length(unique(quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(ses_mis = sum(is.na(ses)) == n(), 
         Wealth = cut(ses, breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5),
                      include.lowest=TRUE))
# 
# timss$Wealth <- with(timss, cut(ses,
#                                 breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE),
#                                 labels = c(1:5),
#                                 include.lowest=TRUE))


# Math performance
timss4_15_m <- ilsa_sum(pvnames = "ASMMAT", 
                    cutoff =   c(400, 475, 550, 625),
                    config = timss4_conf,
                    data = timss,
                    year = 2015,
                    level = "Primary",
                    grade = 4,
                    survey = "TIMSS",
                    prefix = "m")


# Science performance
timss4_15_s <- ilsa_sum(pvnames = "ASSSCI", 
                        cutoff =   c(400, 475, 550, 625),
                        config = timss4_conf,
                        data = timss,
                        year = 2015,
                        level = "Primary",
                        grade = 4,
                        survey = "TIMSS",
                        prefix = "s")



## TIMSS 2015 Grade 8

dir <- "/home/eldani/eldani/International LSA/TIMSS/TIMSS 15/Grade 8/Data"


#timssg8.var.label(folder=dir, output=dir)


timss <- timssg8.select.merge(folder= dir,
                              student= c("ITSEX", "BSBG03", "BSBG04", "BSBG06A", "BSBG06B", 
                                         "BSBG06C", "BSBG06D", "BSBG06E", "BSDGEDUP"), 
                              school= c("BCBG05B"))

# Grouping variables

timss$IDCNTRYL <- cnt[match(timss$IDCNTRY, cnt$iso_num), "country"]
timss$COUNTRY <- cnt[match(timss$IDCNTRY, cnt$iso_num), "iso_code3"]
timss$COUNTRY <- ifelse(is.na(timss$COUNTRY), timss$IDCNTRYL, timss$COUNTRY)

timss <- droplevels(timss[!is.na(timss$COUNTRY), ])

timss$Sex <- factor(timss$ITSEX, c(1,2), c("Female", "Male"))

# ACBG05B
# 1: Urban–Densely populated; 2: Suburban–On fringe or outskirts of urban area; 
# 3: Medium size city or large town; 4: Small town or village; 5: Remote rural

timss$Location <- factor(timss$BCBG05B, c(1:4, 5), c(rep("Urban", 4), "Rural"))

timss$Language <- factor(timss$BSBG03, c(1:3,4), c(rep("Yes",3), "No"))

## Create Wealth quantles (Caro and Cortes, 2012)

### Parental education
timss$pared <- timss$BSDGEDUP
timss$pared[timss$pared==6] <-NA
timss$pared <- 6-timss$pared

### Home possessions
timss$pc <- timss$BSBG06A
timss$desk <- timss$BSBG06C
timss$room <- timss$BSBG06D
timss$internet <- timss$BSBG06E

pos <- c("pc", "desk", "room", "internet")

timss[pos][timss[pos]==2] = 0

timss$home <- apply(timss[pos], 1, mean, na.rm=TRUE)


timss$books <- timss$BSBG04


### Create SES variable
ses.var <- c("pared", "home", "books")

# SES with max values
ses.m <- princomp(na.omit(timss[ses.var]), cor=T)
plot(ses.m,type="lines") # scree plot 
ses <- ses.m$scores[,1, drop=F]

# Match SES score with original data
timss$ses <- ses[match(rownames(timss), rownames(ses))] 

## Create Wealth quantiles

with(timss, tapply(ses, COUNTRY, function(x) sum(is.na(x))/length(x)))

timss <- timss %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ses)) != n() & 
           length(unique(quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(ses_mis = sum(is.na(ses)) == n(), 
         Wealth = cut(ses, breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5),
                      include.lowest=TRUE))


# Math performance
timss8_15_m <- ilsa_sum(pvnames = "BSMMAT", 
                        cutoff =   c(400, 475, 550, 625),
                        config = timss8_conf,
                        data = timss,
                        year = 2015,
                        level = "Lower secondary",
                        grade = 8,
                        survey = "TIMSS",
                        prefix = "m")


# Science performance
timss8_15_s <- ilsa_sum(pvnames = "BSSSCI", 
                        cutoff =   c(400, 475, 550, 625),
                        config = timss8_conf,
                        data = timss,
                        year = 2015,
                        level = "Lower secondary",
                        grade = 8,
                        survey = "TIMSS",
                        prefix = "s")


#### TIMSS 2011

### TIMSS 2011 Grade 4

dir <- "/home/eldani/eldani/International LSA/TIMSS/TIMSS 11/Grade 4/Data"

#timssg4.var.label(dir, output = dir)


timss <- timssg4.select.merge(folder= dir,
                              student= c("ITSEX", "ASBG05A", "ASBG05B", "ASBG05C", "ASBG05D","ASBG05E",
                                         "ASBG03"), 
                              home= c("ASBH14", "ASDHEDUP", "ASDHOCCP", "ASBH20A", "ASBH20B"),
                              school= c("ACBG05B"))


# Grouping variables
timss$IDCNTRYL <- cnt[match(timss$IDCNTRY, cnt$iso_num), "country"]
timss$COUNTRY <- cnt[match(timss$IDCNTRY, cnt$iso_num), "iso_code3"]
timss$COUNTRY <- ifelse(is.na(timss$COUNTRY), timss$IDCNTRYL, timss$COUNTRY)


timss <- droplevels(timss[!is.na(timss$COUNTRY), ])

timss$Sex <- factor(timss$ITSEX, c(1,2), c("Female", "Male"))

# ACBG05B
# 1: Urban–Densely populated; 2: Suburban–On fringe or outskirts of urban area; 
# 3: Medium size city or large town; 4: Small town or village; 5: Remote rural

timss$Location <- factor(timss$ACBG05B, c(1:4, 5), c(rep("Urban", 4), "Rural"))

timss$Language <- factor(timss$ASBG03, c(1:3), c(rep("Yes",2), "No"))
## Create Wealth quantles (Caro and Cortes, 2012)

### Parental education
timss$pared <- timss$ASDHEDUP
timss$pared[timss$pared==6] <-NA
timss$pared <- 6-timss$pared

### Parental occupation
# Father's ISEI
timss$dadsei <- timss$ASBH20A

timss$dadsei[timss$dadsei==1] <-22
timss$dadsei[timss$dadsei==2] <-57
timss$dadsei[timss$dadsei==3] <-49
timss$dadsei[timss$dadsei==4] <-45
timss$dadsei[timss$dadsei==5] <-31
timss$dadsei[timss$dadsei==6] <-37
timss$dadsei[timss$dadsei==7] <-33
timss$dadsei[timss$dadsei==8] <-24
timss$dadsei[timss$dadsei==9] <-67
timss$dadsei[timss$dadsei==10] <-73
timss$dadsei[timss$dadsei==11] <-52
timss$dadsei[timss$dadsei==12] <-NA

# Mother
timss$momsei <- timss$ASBH20B

timss$momsei[timss$momsei==1] <-22
timss$momsei[timss$momsei==2] <-57
timss$momsei[timss$momsei==3] <-49
timss$momsei[timss$momsei==4] <-45
timss$momsei[timss$momsei==5] <-31
timss$momsei[timss$momsei==6] <-37
timss$momsei[timss$momsei==7] <-33
timss$momsei[timss$momsei==8] <-24
timss$momsei[timss$momsei==9] <-67
timss$momsei[timss$momsei==10] <-73
timss$momsei[timss$momsei==11] <-52
timss$momsei[timss$momsei==12] <-NA

timss$parsei <- with(timss, pmax(dadsei, momsei, na.rm=T))

### Home possessions
timss$pc <- timss$ASBG05A
timss$desk <- timss$ASBG05B
timss$room <- timss$ASBG05D
timss$internet <- timss$ASBG05E

pos <- c("pc", "desk", "room", "internet")

timss[pos][timss[pos]==2] = 0

timss$home <- apply(timss[pos], 1, mean, na.rm=TRUE)

timss$books <- timss$ASBH14

### Create SES variable
ses.var <- c("pared", "parsei", "home", "books")

# SES with max values
ses.m <- princomp(na.omit(timss[ses.var]), cor=T)
plot(ses.m,type="lines") # scree plot 
ses <- ses.m$scores[,1, drop=F]

# Match SES score with original data
timss$ses <- ses[match(rownames(timss), rownames(ses))] 

## Create Wealth quantiles

with(timss, tapply(ses, COUNTRY, function(x) sum(is.na(x))/length(x)))

timss <- timss %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ses)) != n() & 
           length(unique(quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(ses_mis = sum(is.na(ses)) == n(), 
         Wealth = cut(ses, breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5),
                      include.lowest=TRUE))
# Math performance
timss4_11_m <- ilsa_sum(pvnames = "ASMMAT", 
                        cutoff =   c(400, 475, 550, 625),
                        config = timss4_conf,
                        data = timss,
                        year = 2011,
                        level = "Primary",
                        grade = 4,
                        survey = "TIMSS",
                        prefix = "m")


# Science performance
timss4_11_s <- ilsa_sum(pvnames = "ASSSCI", 
                        cutoff =   c(400, 475, 550, 625),
                        config = timss4_conf,
                        data = timss,
                        year = 2011,
                        level = "Primary",
                        grade = 4,
                        survey = "TIMSS",
                        prefix = "s")



## TIMSS 2011 Grade 8

dir <- "/home/eldani/eldani/International LSA/TIMSS/TIMSS 11/Grade 8/Data"


#timssg8.var.label(folder=dir, output=dir)


timss <- timssg8.select.merge(folder= dir,
                              student= c("ITSEX", "BSBG03", "BSBG04", "BSBG05A", "BSBG05B", 
                                         "BSBG05C", "BSBG05D", "BSBG05E", "BSDGEDUP"), 
                              school= c("BCBG05B"))

# Grouping variables

timss$IDCNTRYL <- cnt[match(timss$IDCNTRY, cnt$iso_num), "country"]
timss$COUNTRY <- cnt[match(timss$IDCNTRY, cnt$iso_num), "iso_code3"]
timss$COUNTRY <- ifelse(is.na(timss$COUNTRY), timss$IDCNTRYL, timss$COUNTRY)

timss <- droplevels(timss[!is.na(timss$COUNTRY), ])

timss$Sex <- factor(timss$ITSEX, c(1,2), c("Female", "Male"))

# ACBG05B
# 1: Urban–Densely populated; 2: Suburban–On fringe or outskirts of urban area; 
# 3: Medium size city or large town; 4: Small town or village; 5: Remote rural

timss$Location <- factor(timss$BCBG05B, c(1:4, 5), c(rep("Urban", 4), "Rural"))

timss$Language <- factor(timss$BSBG03, c(1:3,4), c(rep("Yes",3), "No"))

## Create Wealth quantles (Caro and Cortes, 2012)

### Parental education
timss$pared <- timss$BSDGEDUP
timss$pared[timss$pared==6] <-NA
timss$pared <- 6-timss$pared

### Home possessions
timss$pc <- timss$BSBG05A
timss$desk <- timss$BSBG05B
timss$room <- timss$BSBG05D
timss$internet <- timss$BSBG05E

pos <- c("pc", "desk", "room", "internet")

timss[pos][timss[pos]==2] = 0

timss$home <- apply(timss[pos], 1, mean, na.rm=TRUE)


timss$books <- timss$BSBG04


### Create SES variable
ses.var <- c("pared", "home", "books")

# SES with max values
ses.m <- princomp(na.omit(timss[ses.var]), cor=T)
plot(ses.m,type="lines") # scree plot 
ses <- ses.m$scores[,1, drop=F]

# Match SES score with original data
timss$ses <- ses[match(rownames(timss), rownames(ses))] 

## Create Wealth quantiles

with(timss, tapply(ses, COUNTRY, function(x) sum(is.na(x))/length(x)))

timss <- timss %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ses)) != n() & 
           length(unique(quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(ses_mis = sum(is.na(ses)) == n(), 
         Wealth = cut(ses, breaks=quantile(ses, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                      labels = c(1:5),
                      include.lowest=TRUE))


# Math performance
timss8_11_m <- ilsa_sum(pvnames = "BSMMAT", 
                        cutoff =   c(400, 475, 550, 625),
                        config = timss8_conf,
                        data = timss,
                        year = 2011,
                        level = "Lower secondary",
                        grade = 8,
                        survey = "TIMSS",
                        prefix = "m")


# Science performance
timss8_11_s <- ilsa_sum(pvnames = "BSSSCI", 
                        cutoff =   c(400, 475, 550, 625),
                        config = timss8_conf,
                        data = timss,
                        year = 2011,
                        level = "Lower secondary",
                        grade = 8,
                        survey = "TIMSS",
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
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "timss_mpl.csv"))
