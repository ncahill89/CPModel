# Load packages
library(tidyverse)
library(rjags)
library(R2jags)

## load functions
devtools::load_all()

## Check crudata and cowdata as examples of datasets with no measurement errors
## Check NJ_CC or NC_CC as examples of datasets with vertical and horizontal measurement errors

## Read in data
dat<-read_csv("data/NJ_CC.csv")

## uncomment to de-trend the record (here using a GIA rate of 1.4 mm/yr)
#dat <- dat %>% mutate(y = (2010-year)*0.0014+y)

## indicate if you want to include horizontal (x) and vertical (y) errors: TRUE = yes, FALSE = no
include_errors = TRUE

## Plot the data with 1-sigma uncertainty
plot_data(dat,
          include_errors = include_errors)

## specify the number of change points (0,1,2,3 or 4)
## 0 = simple linear regression or EIV linear regression
num_change_points <- 1

## Run the model
RunCPModel(dat=dat,
           n_cp = num_change_points,
           include_errors = include_errors)

## Check convergence, if convergence is ok then a summary of the parameters will print
get_diagnostics(dat,
                n_cp = num_change_points)

## If convergence is not ok check the traceplots for flagged parameters e.g.,
PlotTrace("beta",
          n_cp = num_change_points)

## If diagnostics look ok, get estimates and plot the results
# get_ests will return mean estimate, sd and 95% uncertainty interval (l95 = 95% lower bound, u95 = 95% upper bound)
# beta = rate of change (slope)
# cp = change point
get_ests(dat,
         n_cp = num_change_points)

## Plot results, choose an axis label and a title
plot_res(dat,
         n_cp = num_change_points,
         yaxis.lab="Temperature anomoly",
         title="")




