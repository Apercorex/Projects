#cleaning and sorting

weighteddata <- RLVCZRAP
rawdata <- RLVCZRAP
library("devtools")
library("dplyr")
library("forcats")
library("ggplot2")
library("lubridate")
library("purrr")
library("readr")
library("stringr")
library("tibble")
library("tidyr")
library(roxygen2)
install.packages("berryFunctions")

####fixing columns
names(rawdata) <- make.names(names(rawdata), unique=TRUE)
rawdata = subset(rawdata, select = -c( TIME.PERIOD, MEDIA, CREATIVE))

####cleaning
##replaces physical NA values with 0
rawdata$FILM.FESTIVAL[rawdata$FILM.FESTIVAL == 'NA'] <- 0
rawdata$CRITICS.CIRCLE[rawdata$CRITICS.CIRCLE == 'NA'] <- 0
rawdata$GGLOBE.AND.AAWARD[rawdata$GGLOBE.AND.AAWARD == 'NA'] <- 0
rawdata$ROTTEN.TOMATOES[rawdata$ROTTEN.TOMATOES == 'NA'] <- 0
rawdata$CRITICS.REVIEWS[rawdata$CRITICS.REVIEWS == 'NA'] <- 0

##converts double to character when calling vector forms for sums
rawdata$FILM.FESTIVAL <- as.numeric(rawdata$FILM.FESTIVAL)
rawdata$CRITICS.CIRCLE <- as.numeric(rawdata$CRITICS.CIRCLE)
rawdata$GGLOBE.AND.AAWARD <- as.numeric(rawdata$GGLOBE.AND.AAWARD)
rawdata$ROTTEN.TOMATOES <- as.numeric(rawdata$ROTTEN.TOMATOES)
rawdata$CRITICS.REVIEWS <- as.numeric(rawdata$CRITICS.REVIEWS)

rawdata$FILM.FESTIVAL <- ifelse(is.na(as.numeric(rawdata$FILM.FESTIVAL)), 0, as.numeric(rawdata$FILM.FESTIVAL))
rawdata$CRITICS.CIRCLE <- ifelse(is.na(as.numeric(rawdata$CRITICS.CIRCLE)), 0, as.numeric(rawdata$CRITICS.CIRCLE))
rawdata$GGLOBE.AND.AAWARD <- ifelse(is.na(as.numeric(rawdata$GGLOBE.AND.AAWARD)), 0, as.numeric(rawdata$GGLOBE.AND.AAWARD))
rawdata$ROTTEN.TOMATOES <- ifelse(is.na(as.numeric(rawdata$ROTTEN.TOMATOES)), 0, as.numeric(rawdata$ROTTEN.TOMATOES))
rawdata$CRITICS.REVIEWS <- ifelse(is.na(as.numeric(rawdata$CRITICS.REVIEWS)), 0, as.numeric(rawdata$CRITICS.REVIEWS))

##RAW TOTALS
#########
##sums columns UNIT to CRITICS.REVIEWS

moviesum <- rawdata |>
  group_by(PRODUCT) |>
  summarise(across(everything(), sum))

#########
##creates a catalog of the last row of each movie (where unit totals are stored)
totalunits <- rawdata |>
  group_by(PRODUCT) |>
  slice_tail(n = 1) |>
  print(n = 155)

#########
##creating a new object with the spliced units and the column sums, and the
rawtotals <- cbind(totalunits[1:2], moviesum[3:7])


##UNIT WEIGHTED TOTALS
#########
##
names(weighteddata) <- make.names(names(weighteddata), unique=TRUE)
weighteddata = subset(weighteddata, select = -c( CREATIVE))

####cleaning
##replaces physical NA values with 0
weighteddata$FILM.FESTIVAL[weighteddata$FILM.FESTIVAL == 'NA'] <- 0
weighteddata$CRITICS.CIRCLE[weighteddata$CRITICS.CIRCLE == 'NA'] <- 0
weighteddata$GGLOBE.AND.AAWARD[weighteddata$GGLOBE.AND.AAWARD == 'NA'] <- 0
weighteddata$ROTTEN.TOMATOES[weighteddata$ROTTEN.TOMATOES == 'NA'] <- 0
weighteddata$CRITICS.REVIEWS[weighteddata$CRITICS.REVIEWS == 'NA'] <- 0

##converts double to character when calling vector forms for sums
weighteddata$FILM.FESTIVAL <- as.numeric(weighteddata$FILM.FESTIVAL)
weighteddata$CRITICS.CIRCLE <- as.numeric(weighteddata$CRITICS.CIRCLE)
weighteddata$GGLOBE.AND.AAWARD <- as.numeric(weighteddata$GGLOBE.AND.AAWARD)
weighteddata$ROTTEN.TOMATOES <- as.numeric(weighteddata$ROTTEN.TOMATOES)
weighteddata$CRITICS.REVIEWS <- as.numeric(weighteddata$CRITICS.REVIEWS)

## grouping by weeks and removing rows that are "grand total"
weeklymentionssum <- weighteddata |>
  group_by(PRODUCT, TIME.PERIOD, MEDIA) |>
  filter(TIME.PERIOD != "Grand Total") |>
  summarise(across(everything(), sum))

##TESTER
#removes rows where things like "Newspaper Total" appear

tester <- weighteddata[- grep("Total", weighteddata$MEDIA),]

#groups and filters to show the rows and cols we want to see, removes grand total rows and provides sums

testmentions <- tester |>
  group_by(PRODUCT, TIME.PERIOD, MEDIA) |>
  filter(TIME.PERIOD != "Grand Total") |>
  filter(MEDIA != (is.na=TRUE)) |>
  summarise(across(everything(), list(sum = ~sum(., na.rm = TRUE))))

testsums <- testmentions |>
  group_by(PRODUCT, TIME.PERIOD) |>
  select(!MEDIA) |>
  summarise(across(everything(), sum))

##multiply mentions by ad units
testproduct <- testsums |>
  mutate(
    FILMFESTprod = UNITS_sum * FILM.FESTIVAL_sum,
    CRITCIRCprod = UNITS_sum * CRITICS.CIRCLE_sum,
    GGAAprod = UNITS_sum * GGLOBE.AND.AAWARD_sum,
    ROTTOMprod = UNITS_sum * ROTTEN.TOMATOES_sum,
    CRITREVprod = UNITS_sum * CRITICS.REVIEWS_sum,
    .before = 3
  )

weightedtotals <- testproduct |>
  group_by(PRODUCT) |>
  select(!TIME.PERIOD) |>
  summarise(across(everything(), sum))


####combined
combinedtotals <- cbind(rawtotals[1:7], weightedtotals[2:6])

write.csv(combinedtotals, "summed reconciled files")
weightedtotals <- weightedtotals |>
  add_row(UNITS_sum=1, PRODUCT="A Murder In The Park" ,.after = 33)
