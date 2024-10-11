### ilsa_summarize: Examples

library("tidyverse")
library("dplyr")
library("haven")
library("reshape2")
library("intsvy") 

# Data with country iso_codes
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
cnt <- read.csv(file.path(dir, "iso_code.csv"))
# country data is complete

# TERCE 2013

## Create pooled data

### Assessment data

dir <- "/home/eldani/eldani/International LSA/LLECE/TERCE/Logro-de-aprendizaje"
files <- grep(".dta$", list.files(dir), value = TRUE)

test <- lapply(files, function(x) {
  df = read_dta(file.path(dir, x));
  df$idgrade = as.numeric(substr(x, 3,3));
  df$area = substr(x, 2,2);
  df }
  )

vars <- Reduce(intersect, lapply(test, names))
test <- do.call(rbind, lapply(test, function(x) x[vars]))

### Student data
dir <- "/home/eldani/eldani/International LSA/LLECE/TERCE/Factores-asociados/Alumnos"
files <- grep("^QA", list.files(dir), value=TRUE)
student <- lapply(files, function(x) read_dta(file.path(dir, x)))
vars <- Reduce(intersect, lapply(student, names))
student <- do.call(rbind, lapply(student, function(x) x[vars]))

### Family data
dir <- "/home/eldani/eldani/International LSA/LLECE/TERCE/Factores-asociados/Familia"
files <- grep("^QF", list.files(dir), value=TRUE)
family <- lapply(files, function(x) read_dta(file.path(dir, x)))
vars <- Reduce(intersect, lapply(family, names))
family <- do.call(rbind, lapply(family, function(x) x[vars]))

### Merge files
terce <- left_join(test, student, by =c("idgrade", "country", "idschool", "idclass", "idstud"))
terce <- left_join(terce, family, by =c("idgrade", "country", "idschool", "idclass", "idstud"))


## Crete grouping variables

terce <- terce %>%
  mutate(COUNTRY =  recode(country,  `HON` =  "HND", `PAR` = "PRY", 
                                  `REP`= "DOM", `URU` = "URY"))


terce$Sex <- factor(terce$nina, c(0,1), c("Male", "Female"))
terce$Language <- factor(terce$DQFIT07, 1:6, c("Yes", rep("No", 5)))
terce$Location <- factor(terce$ruralidad, 1:2, c("Urban", "Rural"))

terce <- terce %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ISECF)) != n() & 
           length(unique(quantile(ISECF, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(Wealth = cut(ISECF, breaks=quantile(ISECF, probs=seq(0,1, by=0.2), na.rm=TRUE),
                      labels = c(1:5),  include.lowest=TRUE))


# Calculate MPL

## Reading


terce$wgt_sen <-terce$wgL_sen.x

terce3_r <- ilsa_sum(pvnames = "vp", 
                     cutoff = c(676, 729, 813),
                     config = llece_conf,
                     data = filter(terce, idgrade==3 & area =="L"),
                     year = 2013,
                     level = "Primary",
                     grade = 3,
                     survey = "TERCE",
                     prefix = "r")


terce6_r <- ilsa_sum(pvnames = "vp", 
                     cutoff = c(612, 754, 810),
                     config = llece_conf,
                     data = filter(terce, idgrade==6 & area =="L"),
                     year = 2013,
                     level = "Primary",
                     grade = 6,
                     survey = "TERCE",
                     prefix = "r")

## Maths

terce$wgt_sen <-terce$wgM_sen.x

terce3_m <- ilsa_sum(pvnames = "vp", 
                     cutoff = c(688, 750, 843),
                     config = llece_conf,
                     data = filter(terce, idgrade==3 & area =="M"),
                     year = 2013,
                     level = "Primary",
                     grade = 3,
                     survey = "TERCE",
                     prefix = "m")


terce6_m <- ilsa_sum(pvnames = "vp", 
                     cutoff = c(687, 789, 878),
                     config = llece_conf,
                     data = filter(terce, idgrade==6 & area =="M"),
                     year = 2013,
                     level = "Primary",
                     grade = 6,
                     survey = "TERCE",
                     prefix = "m")


## Science

terce$wgt_sen <-terce$wgC_sen

terce6_s <- ilsa_sum(pvnames = "vp", 
                     cutoff = c(669, 782, 862),
                     config = llece_conf,
                     data = filter(terce, idgrade==6 & area =="C"),
                     year = 2013,
                     level = "Primary",
                     grade = 6,
                     survey = "TERCE",
                     prefix = "s")


## Merge datasets
grade3 <- merge(terce3_m, terce3_r, all=TRUE)

names(grade3)[11:28] <- c(paste0("mlevel", 2:4, "_m"),
                             paste0("mlevel", 2:4, "_se"),
                             paste0("mlevel", 2:4, "_no"),
                             paste0("rlevel", 2:4, "_m"), 
                             paste0("rlevel", 2:4, "_se"),
                             paste0("rlevel", 2:4, "_no"))


grade6 <- merge(merge(terce6_m, terce6_r, all=TRUE), terce6_s, all=TRUE)
wide_data <- dplyr::bind_rows(grade3, grade6)

## Create country id
names(wide_data)[1] <- "iso_code3"
wide_data$iso_code3 <-as.character(wide_data$iso_code3)
wide_data$COUNTRY <- cnt[match(wide_data$iso_code3, cnt$iso_code3), "country"]
wide_data$COUNTRY <- ifelse(is.na(wide_data$COUNTRY), as.character(wide_data$iso_code3), wide_data$COUNTRY)
wide_data$iso_num <- cnt[match(wide_data$COUNTRY, cnt$country), "iso_num"]

# remove iso_code3 with more than 3 characters
wide_data$iso_code3 <- ifelse(nchar(as.character(wide_data$iso_code3)) >3, NA, wide_data$iso_code3)

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

# wide_data <- read.csv(file.path(dir, "llece_mpl.csv"))


dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "llece_mpl.csv"))

############# ERCE 2019

### Read data
dir <- "/home/eldani/eldani/International LSA/LLECE/ERCE 2019/Bases"
grade3s <- readRDS(file=file.path(dir, "ERCE_2019_QA3.rds"))
grade3p <- readRDS(file=file.path(dir, "ERCE_2019_QF3.rds"))
grade3 <- merge(grade3s, grade3p[grep("^W|ISECF", names(grade3p), value = TRUE, invert = TRUE)],
                by = c("COUNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD"), all.x = TRUE)

grade6 <- readRDS(file=file.path(dir, "ERCE_2019_QA6.rds"))


######## Subgroup variables

### Grade 3
attr(grade3$SEX, "labels")
attr(grade3$RURAL, "labels")
attr(grade3$FFIT08, "labels")

grade3$Sex <- factor(grade3$SEX, c(0,1), c("Male", "Female"))
grade3$Location <- factor(grade3$RURAL, c(0,1), c("Urban", "Rural"))
grade3$Language <- factor(grade3$FFIT08, 1:6,  c("Yes", rep("No", 5)))

grade3 <- grade3 %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ISECF)) != n() & 
           length(unique(quantile(ISECF, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(Wealth = cut(ISECF, breaks=quantile(ISECF, probs=seq(0,1, by=0.2), na.rm=TRUE),
                      labels = c(1:5),  include.lowest=TRUE))


#### Grade 6
grade6$Sex <- factor(grade6$SEX, c(0,1), c("Male", "Female"))
grade6$Location <- factor(grade6$RURAL, c(0,1), c("Urban", "Rural"))
grade6$Language <- factor(grade6$E6IT04, 1:6,  c("Yes", rep("No", 5)))

grade6 <- grade6 %>%
  group_by(COUNTRY) %>%
  filter(sum(is.na(ISECF)) != n() & 
           length(unique(quantile(ISECF, probs=seq(0,1, by=0.2), na.rm=TRUE)))==6) %>%
  mutate(Wealth = cut(ISECF, breaks=quantile(ISECF, probs=seq(0,1, by=0.2), na.rm=TRUE),
                      labels = c(1:5),  include.lowest=TRUE))


## Tweaking for intsvy

# Rename PVs
grade3[paste0("READ", 1:5)] <- grade3[paste0("LAN_", 1:5)]
grade3[paste0("MATH", 1:5)] <- grade3[paste0("MAT_", 1:5)]
grade6[paste0("READ", 1:5)] <- grade6[paste0("LAN_", 1:5)]
grade6[paste0("MATH", 1:5)] <- grade6[paste0("MAT_", 1:5)]

# Modify config
llece_conf <- list(variables = list(pvlabelpref = "vp",
                                    pvlabelsuff = "READ",
                                    weightFinal = "WT",
                                    weightBRR = "BRR"),
                   parameters = list(cutoffs = c(676, 729, 813),
                                     percentiles = c(5, 10, 25, 75, 90, 95),
                                     BRRreps = 100,
                                     weights = "BRR",
                                     replication_scheme = 'pisa')
)

erce3_r <- ilsa_sum(pvnames = "READ", 
                     cutoff = c(676, 729, 813),
                     config = llece_conf,
                     data = grade3,
                     year = 2019,
                     level = "Primary",
                     grade = 3,
                     survey = "LLECE",
                     prefix = "r")

erce3_m <- ilsa_sum(pvnames = "MATH", 
                     cutoff = c(688, 750, 843),
                     config = llece_conf,
                     data = grade3,
                     year = 2019,
                     level = "Primary",
                     grade = 3,
                     survey = "LLECE",
                     prefix = "m")


erce6_r <- ilsa_sum(pvnames = "READ", 
                     cutoff = c(612, 754, 810),
                     config = llece_conf,
                     data = grade6, 
                     year = 2019,
                     level = "Primary",
                     grade = 6,
                     survey = "LLECE",
                     prefix = "r")

erce6_m <- ilsa_sum(pvnames = "MATH", 
                     cutoff = c(687, 789, 878),
                     config = llece_conf,
                     data = grade6,
                     year = 2019,
                     level = "Primary",
                     grade = 6,
                     survey = "LLECE",
                     prefix = "m")

g3 <- merge(erce3_m, erce3_r, all=TRUE)

names(g3)[11:28] <- c(paste0("mlevel", 2:4, "_m"),
                          paste0("mlevel", 2:4, "_se"),
                          paste0("mlevel", 2:4, "_no"),
                          paste0("rlevel", 2:4, "_m"), 
                          paste0("rlevel", 2:4, "_se"),
                          paste0("rlevel", 2:4, "_no"))


g6 <- merge(erce6_m, erce6_r, all=TRUE)
erce <- dplyr::bind_rows(g3, g6)
names(erce)[1] <- "iso_code3"
erce$Wealth <- as.numeric(as.factor(erce$Wealth))

# Read TERCE data
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA/old/"
terce <- read.csv(file = file.path(dir, "llece_mpl.csv"))
terce$survey = "LLECE"

# bind_rows
vars <- intersect(names(erce), names(terce))
wide_data <- bind_rows(erce[vars], terce[vars])

dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
write.csv(wide_data, row.names = FALSE, na="", file = file.path(dir, "llece_mpl.csv"))

df <- read.csv(file = file.path(dir, "llece_mpl.csv"))

# df %>% filter(year==2019, category=="Sex", grade == 6) %>%
#   dplyr::select(iso_code3, Sex, mlevel2_m, rlevel2_m)
               
               
               

