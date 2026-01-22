#Solar PV Informal Sensitivity

#Libraries
library(tidyverse)
library(here)

# Source solar PV function
source(here("R/solarpv.R"))

# read in R formatted data
load(here("Data/sierraczosolar_clean.rda"))

# notice very low power in the first year
# check input data
head(sierraczosolar)

# remove the first parital year from the dataset
sierraczosolar <- sierraczosolar %>%
  filter(year > 1944 & year < 1954)

# now run the model again
site1 <- solarpv(area = 0.1, eff = 0.6,
                 solar = sierraczosolar, clr = "green", eunit = "kW", g = FALSE)

# keep cleaned up data set for future use
save(sierraczosolar, file=here("Data/sierraczosolar_clean.rda"))

# read in R formatted data
load(here("Data/sierraczosolar_clean.rda"))


# run the model to check that everything works
site1 <- solarpv(area = 0.1, solar = sierraczosolar)

# consider a different PV array has a non standard energy threshold
site2 <- solarpv(area = 0.1, 
                 solar = sierraczosolar, g = FALSE, ethresh = 000)
site2$mean
site1$mean

# relative difference
paste((site2$mean-site1$mean)/site1$mean * 100, "%")

# relative difference in parameter
paste((50000-10000)/10000 * 100, "%")

# first come up with varying values for efficiency 
# if we don't know efficiency exactly , lets try 20 samples
ethresh <- rnorm(mean = , sd = 0.15, n = 20)

plot(ethresh)

# use map from purrr to run model for all values of ethresh
# notice how map adds the one parameter that is missing from the input list

site2 <- ethresh %>% map(~ solarpv(area = 0.1, 
                               solar = sierraczosolar, clr = "green", 
                               eunits = "kWhr", g = FALSE, etype = "direct", ethresh = .x)) # substitute the number of thresholds for each of the values within the eff dataframe

head(site2)

head(str(site2))
# this is pretty messy - but we can extract a useful data structure,lets say we want
# just the annual data (not the mean annual time series), and then reformat as a data frame with nice column names
tmp <- purrr::map_dfr(site2, `[`, c("annual"))

# `[` = function that extracts the first item on the list 

# convert from tibble to a data frame
site2df <- data.frame(year = tmp$annual$year, elect = tmp$annual$elect)
head(site2df)

ggplot(site2df, aes(year, elect, group = year)) +
  geom_boxplot() +
  labs(y = "Energy Generated in kW/hr")


