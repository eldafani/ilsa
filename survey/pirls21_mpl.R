### PIRLS 2021

library("tidyverse")
library("haven")
library("reshape2")
#library("devtools")
#install_github("eldafani/intsvy")
library("intsvy")

# Data with country iso_codes
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Data/"
cnt <- read.csv(file.path(dir, "wide_countries.csv"))
dir <- "/home/eldani/Dropbox/Work/intsvy/Data/"
iea <- read.csv(file.path(dir, "iea.country.csv"))

## Read data
dir <- "/home/eldani/eldani/International LSA/PIRLS/PIRLS 2021/3_International Database/1_SPSS Data/"
#pirls.var.label(dir)

pirls <- pirls.select.merge(folder= dir,
                            student= c("ITSEX", "ASBG03"),
                            home= c("ASBHSES", "ASDHSES"), 
                            school= c("ACBG05B"))

#(score <- pirls.mean.pv(pvlabel="ASRREA", by="IDCNTRYL", data=pirls))
#(mpl <- pirls.ben.pv(pvlabel = "ASRREA", cutoff = 400, by = "IDCNTRYL", data = pirls))

# consistent with int. report

## Select countries in WIDE
pirls$COUNTRY <- iea[match(pirls$IDCNTRY, iea$Code), "ISO"]
pirls <- filter(pirls, COUNTRY %in% unique(cnt$iso_code))

## Create grouping vars
pirls$Sex <- factor(pirls$ITSEX, c(1,2), c("Female", "Male"))

# ACBG05B
# 1: Urban–Densely populated; 2: Suburban–On fringe or outskirts of urban area; 
# 3: Medium size city or large town; 4: Small town or village; 5: Remote rural

pirls$Location <- factor(pirls$ACBG05B, c(1:4, 5), c(rep("Urban", 4), "Rural"))
pirls$Language <- factor(pirls$ASBG03, c(1:3,4), c(rep("Yes",3), "No"))

pirls <- pirls %>%
  group_by(COUNTRY) %>%
  mutate(ses_mis = sum(is.na(ASBHSES)) != n() & 
         length(unique(quantile(ASBHSES, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6, 
         Wealth = ifelse(!ses_mis, NA, cut(ASBHSES, 
                   breaks=quantile(ASBHSES, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                   labels = c(1:5),
                   include.lowest=TRUE)))


# Calculate
pirls_r <- ilsa_sum(pvnames = "ASRREA", 
                    cutoff =   c(400, 475, 550),
                    config = pirls_conf,
                    data = pirls,
                    year = 2021,
                    level = "Primary",
                    grade = 4,
                    survey = "PIRLS",
                    prefix = "r")

names(pirls_r)[7:15] <- c(paste0("rlevel", 2:4, "_m"), 
                          paste0("rlevel", 2:4, "_se"),
                          paste0("rlevel", 2:4, "_no"))


## Append datasets
wide_data <- pirls_r

## Create country id
names(wide_data)[1] <- "iso_code3"

## Export

# reorder columns for wide
cat.vars <- grep("_m|_se|_no", names(wide_data), invert = TRUE, value = TRUE)
vars <- c("category", "Sex",  "Location", "Wealth", "Language")
vars <- intersect(names(wide_data), vars)
num.vars <- setdiff(names(wide_data), cat.vars)
id.vars <- setdiff(cat.vars, vars)

wide_data <- wide_data[c(id.vars, vars, num.vars)]

dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "pirls21_mpl.csv"))
