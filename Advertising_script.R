#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-05-08 11:05:23 x86_64-w64-mingw32 

# Rattle version 5.2.0 user 'Vidhya Shree'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-05-08 11:13:06 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///G:/svyasa/R/Datasets/Advertising.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 11:13:06 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=200 train=140 validate=30 test=30

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$numeric   <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:13:21 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=200 train=140 validate=30 test=30

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("TV", "radio", "newspaper")

crs$numeric   <- c("TV", "radio", "newspaper")

crs$categoric <- NULL

crs$target    <- "sales"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- "Sl"
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:13:29 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(sales ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.08 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 11:13:38 x86_64-w64-mingw32 

# Export regression model. 

# The 'pmml' package provides the 'pmml' function.

library(pmml, quietly=TRUE)
