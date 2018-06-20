# Set working directory 
setwd("~/Desktop/CPModel")

# Load packages
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(ggplot2)
library(rjags)
library(R2jags)

# Call functions
Rfiles <- list.files(file.path(getwd(), "R"))
Rfiles <- Rfiles[grepl(".R", Rfiles)]
sapply(paste0("R/", Rfiles), source)

##Read in data
dat<-read_csv("data/cowdata.csv")

##Plot the data
plot_data(dat)

##choose a model file
model.file="model/CP4_model.txt"

##Run the model
RunCPModel(dat=dat,
           model=model.file)

##Check convergence, if convergence is ok then a summary of the parameters will print
get_diagnostics(dat,
                model=model.file)

##If convergence is not ok check the traceplots for flagged parameters e.g.,
PlotTrace("beta[1]",
          model=model.file)

##If diagnostics look ok, get estimates and plot the results 
# get_ests will return mean estimate, sd and 95% uncertainty interval (l95 = 95% lower bound, u95 = 95% upper bound)
get_ests(dat,model=model.file)
# Plot results, choose an axis label and a title
plot_res(dat,
         model=model.file,
         yaxis.lab="Temperature anomoly",
         title="HadCRUT")




