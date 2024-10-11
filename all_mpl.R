### WIDE all LEARNING: merge

library("tidyverse")
library("haven")
library("reshape2")

#### Read data
dir <- "/home/eldani/MEGA/Work/Projects/Ongoing/UNESCO/Analysis/ILSA"
files <- list.files(pattern="_mpl", dir, full.names = TRUE)
ilsa <- lapply(files, read_csv)

### Merge data
ilsa <- do.call(bind_rows, ilsa)
ilsa <- ilsa %>% drop_na(iso_code3)
summary(ilsa)

write.csv(ilsa, file = file.path(dir, "WIDE_learningMay2023.csv"), row.names=FALSE)

