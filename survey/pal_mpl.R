### pal data

library("tidyverse")
library("haven")
library("reshape2")
library("janitor")
library("ggplot2")
library("stringr")
library("ggpubr")
library("purrr")
library("readxl")

dir <- "/home/eldani/eldani/International LSA/PAL/"
df <- readRDS(file = file.path(dir, "pal_micro.rds")) %>% 
      filter(grade %in% c(3,6))


#### Aggregate data for WIDE 
dvs <- c("math", "read")
ids <- c("iso_code3", "year", "grade")
groups <- c("Sex", "Location")
vars <- c(dvs, ids, groups, "weight")

pal <- wide_bind(df, dvs, ids, groups)
pal <- filter(pal, !((iso_code3 == "PAK" & category == "Total")| 
                    (iso_code3=="IND" & category == "Total")))

############## Add aggregated

### India
dir <- "/home/eldani/eldani/International LSA/PAL/India/"
read <- read_excel(file.path(dir, "India.xlsx"), sheet = "read") %>%
  rename(read = Story) %>% dplyr::select(year, grade, read)
math <- read_excel(file.path(dir, "India.xlsx"), sheet = "math")  %>%
  rename(math = Division) %>% dplyr::select(year, grade, math)

india <- full_join(read, math, by = c("year", "grade")) %>% 
  filter(grade %in% c(1:8)) %>%
  mutate(iso_code3 = "IND", category = "Total", grade = as.numeric(grade)) %>%
  filter(year == 2022, grade %in% c(3,6))

#india <- bind_rows(india, cbind(select(india, !category), category = "Location", Location = "Rural"))


pal <- bind_rows(pal, cbind(select(india, !category), category = "Location", Location = "Rural"))


#### Pakistan
dir <- "/home/eldani/eldani/International LSA/PAL/Pakistan/"
read <- read_excel(file.path(dir, "Pakistan.xlsx"), sheet = "read") %>%
  mutate(read = story/100) %>% dplyr::select(year, grade, read)

math <- read_excel(file.path(dir, "Pakistan.xlsx"), sheet = "math") %>%
  mutate(math = division_2d/100) %>% dplyr::select(year, grade, math)

pak <- full_join(read, math, by = c("year", "grade")) %>% 
  mutate(iso_code3 = "PAK", category = "Total", grade = as.numeric(grade)) %>%
  filter(year >= 2016, grade %in% c(3,6))

# remove PAK from micro data
pal <- bind_rows(filter(pal, iso_code3 != "PAK"), 
                 cbind(select(pak, !category), category = "Location", Location = "Rural"))


## Rename for WIDE
pal <- pal %>%
  filter(iso_code3!= "MEX") %>%
  rename(rlevel2_m = read, mlevel2_m = math) %>%
  mutate(survey =  "ASER") %>%
  select(-n)

ggplot(filter(pal, category=="Total"), 
       aes(x= reorder(iso_code3, mlevel2_m), y= mlevel2_m, fill= as.factor(grade))) +
       geom_bar(stat="identity", position = "dodge")

## Export
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
write.csv(pal, row.names = FALSE, na="", file = file.path(dir, "pal_mpl.csv"))
