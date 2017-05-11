###install necessary package

list.of.packages <- c("data.table", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(data.table)
library(dplyr) 


###Downloading dataset and set up for later analysis

###If the link does not work, please load dataset from local folder use the following code
###tangram <- read.csv("tangrams_data.csv")

tangram <- fread("http://statgames.tietronix.com/tangrams/webreporter.php?game=tangrams&GroupName=&winlose=both&random=false&rows=&type=csv")
tangram$HintsEnabled <- as.factor(tangram$HintsEnabled)
tangram$HintsUsed <- as.factor(tangram$HintsUsed)
tangram$Won <- as.factor(tangram$Won)
tangram$TimerHint <- as.factor(tangram$TimerHint)
tangram$TimerDisplay <- as.factor(tangram$TimerDisplay)
tangram$RequestedTime <- as.numeric(tangram$RequestedTime)
tangram$NumClicks <- as.numeric(tangram$NumClicks)
tangram$TimeUsed <- as.numeric(tangram$TimeUsed)
tangram$NumShapes <- as.numeric(tangram$NumShapes)

###Automated filter Useful informtaion(Gender, STEM major and Athletes )
### Gender information
gen = c("^gen","^sex")
gen_male = c("^m","^h")
gen_female = c("^f", "^mu")


### ismatch (helper function)
### @input: cond: a vector of key patterns (e.g. gen)
###         x, y, z: 3 columns to look for key
### @return: 0 if no match
###          index of the factor (1, 2, 3() if there is match
ismatch <- function(cond, x, y, z) {
  x <- grep(paste(cond, collapse = "|"), c(x, y, z), ignore.case = T, value = FALSE)
  return (ifelse(length(x), as.numeric(x), as.numeric(0)))
}

### level_gen_fun (helper function)
### @input: x is the col index (given by factor_gender)
###         y is the row index
### @return: -1 if no gender factor indicated
###          -2 if gender factor indicated but level_gen key patterns not matched (require future manual check)
###           M/F
### @note: this function is specific to gender only. Needs to think about how to generalize it. 
level_gen_fun <- function(x, y) {
  return (ifelse(x == 0, -1, 
                 ifelse(grepl(paste(gen_female, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                        "F", 
                        ifelse(grepl(paste(gen_male, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                               "M",-2))))
}
tangram <- as.data.table(tangram)
tangram[, factor_gender := ismatch(gen, Factor1, Factor2, Factor3), by = 1:nrow(tangram)]
tangram <- as.data.frame(tangram)
tangram$level_gender <- mapply(level_gen_fun, tangram$factor_gender, 1:nrow(tangram), SIMPLIFY = TRUE)


### STEM information

stem <- c("stem")
stem_Y <- c("^y")
stem_N <- c("^n")
tangram$factor_STEM <- mapply(ismatch, stem, tangram$Factor1, tangram$Factor2, tangram$Factor3, SIMPLIFY = TRUE)

level_stem_fun <- function(x, y) {
  return (ifelse(x == 0, -1, 
                 ifelse(grepl(paste(stem_Y, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                        "Y", 
                        ifelse(grepl(paste(stem_N, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                               "N",-2))))
}


tangram$level_stem <- mapply(level_stem_fun, tangram$factor_STEM, 1:nrow(tangram), SIMPLIFY = TRUE)

### athl information

athl <- c("athl")
athl_Y <- c("^y")
athl_N <- c("^n")
tangram$factor_athl <- mapply(ismatch, athl, tangram$Factor1, tangram$Factor2, tangram$Factor3, SIMPLIFY = TRUE)
level_athl_fun <- function(x, y) {
  return (ifelse(x == 0, -1, 
                 ifelse(grepl(paste(athl_Y, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                        "Y", 
                        ifelse(grepl(paste(athl_N, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                               "N",-2))))
}

tangram$level_athl <- mapply(level_athl_fun, tangram$factor_athl, 1:nrow(tangram), SIMPLIFY = TRUE)

### Secondary cataloging the puzzle name factor in the dataset

puzzlename <- c("A Nice Lighthouse", "Diamond", "House of Tangrams", "The Hook")
tangram$PuzzleName2 <- ifelse(tangram$PuzzleName %in% puzzlename, tangram$PuzzleName, "Others")
tangram$PuzzleName2 <- as.factor(tangram$PuzzleName2)