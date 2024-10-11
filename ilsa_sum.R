### ilsa_sum: function to summarize ILSA indicators
### January 2021

ilsa_sum <- function(pvnames = "LECT",
                     cutoff = c(399.1, 469.5, 540, 610.4),
                     config = pasec_conf,
                     data = pasec2,
                     year = 2014,
                     level = "Primary",
                     grade = 2,
                     survey = "PASEC",
                     prefix = "r",
                     pv1= grep(pvnames, names(data), value = TRUE)[1]) {

# coerce to df
data <- as.data.frame(data)
data$COUNTRY <- as.factor(data$COUNTRY)  
# groups
vars <- c("Language", "Location", "Sex", "Wealth")
vars <- intersect(names(data), vars)
vars <- sort(vars)

# remove data with missing in DV
data <- data[complete.cases(data[, pv1]), ]

## binary level indicator based on pv1
ach_level <- paste0("level", 1:length(cutoff))

data[ach_level] <- as.data.frame(sapply(1:length(cutoff), function(x) 
  cut(data[[pv1]], breaks = c(-Inf, cutoff[x], + Inf), labels = FALSE) -1))

# % at or above levels

## by country
total_ach <- cbind(intsvy.ben.pv(pvnames = pvnames, 
                       cutoff= cutoff, by = "COUNTRY", 
                       data = data, config = config), "category" = "Total")

total_n <- cbind(melt(id.vars= "COUNTRY", 
                aggregate(data[ach_level], by=list(COUNTRY = data[["COUNTRY"]]), sum), 
                variable.name = "levels",
                value.name = "n"), "category" = "Total")


## by one group

one_ach <- lapply(vars, function(x) {
  
  mydata <- data[complete.cases(data[, x]), ];
  mymiss <- aggregate(list(n=mydata[["COUNTRY"]]), mydata[c("COUNTRY", x)], length);
  mydata <- merge(mydata, mymiss, all.x=TRUE);
  mydata <- mydata[mydata$n>5, ]; # remove intersections with less than 5 groups (for run)
  
  df = intsvy.ben.pv(pvnames = pvnames, 
                     cutoff = cutoff, by = c("COUNTRY", x), 
                     data = mydata, config = config);
  df$category = x;
  df})

one_ach <- do.call(dplyr::bind_rows, one_ach)

one_n <- lapply(vars, function(x) {
  df = aggregate(data[ach_level], by=data[, c("COUNTRY", x)], sum);
  df$category = x;
  df})

one_n <- melt(id.vars = c("COUNTRY", "category", vars), 
              do.call(dplyr::bind_rows, one_n), variable.name = "levels", value.name = "n")


# Two groups
two <- combn(vars, 2, simplify = FALSE)

two_ach <- lapply(two, function(x) {
  
  mydata <- data[complete.cases(data[, x]), ];
  mymiss <- aggregate(list(n=mydata[["COUNTRY"]]), mydata[c("COUNTRY", x)], length);
  mydata <- merge(mydata, mymiss, all.x=TRUE);
  mydata <- mydata[mydata$n>5, ]; # remove intersections with less than 5 groups (for run)
  
  ach <- intsvy.ben.pv(pvnames = pvnames, 
                       cutoff= cutoff, by = c("COUNTRY", x), 
                       data = mydata, config = config);
  ach$category <- paste(x, collapse = " & ")
  ach}
)

two_ach <- do.call(dplyr::bind_rows, two_ach)

two_n <- lapply(two, function(x) {
  df <- aggregate(data[ach_level], data[c("COUNTRY", x)], sum);
  df$category <- paste(x, collapse = " & ");
  df})

two_n <- melt(id.vars = c("COUNTRY", "category", vars), 
              do.call(dplyr::bind_rows, two_n), variable.name = "levels", value.name = "n")


# Three groups
three <- combn(vars, 3, simplify = FALSE)

three_ach <- lapply(three, function(x) {
  
  mydata <- data[complete.cases(data[, x]), ];
  mymiss <- aggregate(list(n=mydata[["COUNTRY"]]), mydata[c("COUNTRY", x)], length);
  mydata <- merge(mydata, mymiss, all.x=TRUE);
  mydata <- mydata[mydata$n>5, ]; # remove intersections with less than 5 groups (for run)
  
  ach <- intsvy.ben.pv(pvnames = pvnames, 
                       cutoff= cutoff, by = c("COUNTRY", x), 
                       data = mydata, config = config);
  ach$category <- paste(x, collapse = " & ")
  ach}
)

three_ach <- do.call(dplyr::bind_rows, three_ach)

three_n <- lapply(three, function(x) {
  df <- aggregate(data[ach_level], data[c("COUNTRY", x)], sum);
  df$category <- paste(x, collapse = " & ");
  df})

three_n <- melt(id.vars = c("COUNTRY", "category", vars), 
              do.call(dplyr::bind_rows, three_n), variable.name = "levels", value.name = "n")


## appended data
bench_ach <- dplyr::bind_rows(total_ach, one_ach, two_ach, three_ach)
bench_ach$levels <- factor(bench_ach$Benchmark, 
                       levels = sapply(seq_along(cutoff), function(c) 
                         grep(cutoff[c], levels(factor(bench_ach$Benchmark)), value = TRUE)),
                       labels = paste0("level", 1:length(cutoff)))
bench_ach$Wealth <- as.integer(bench_ach$Wealth)

bench_n <- dplyr::bind_rows(total_n, one_n, two_n, three_n)

bench_tot <- left_join(bench_ach, bench_n, by = c("COUNTRY", "category", vars, "levels"))


# output
myout <- pivot_wider(data = bench_tot, 
                     id_cols = c("COUNTRY", "category", all_of(vars)),
                     names_from= c("levels"), values_from =c("Percentage", "Std. err.", "n"))

## var names
perlev <- paste0(prefix, "level", 1:length(cutoff), "_m")
selev <- paste0(prefix, "level", 1:length(cutoff), "_se")
nolev <- paste0(prefix, "level", 1:length(cutoff), "_no")

names(myout) <- c("COUNTRY", "category", vars, perlev, selev, nolev)

## divide by 100
pervars <- grep("_m|_se", names(myout), value = TRUE)
myout[pervars] <- myout[pervars]/100

## add survey variables
myout <- cbind(myout, "year"=year, "grade"=grade, "level"=level, "survey" =  survey)
return(myout)
}
