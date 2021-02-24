# Load packages
library(tidyverse)
library(rjags)
library(R2jags)

# load functions
devtools::load_all()

##Read in data
dat<-read_csv("data/crudata.csv")

##Plot the data
plot_data(dat)

##choose a model file
model.file="model/CP2_model.txt"

##Run the model
RunCPModel(dat=dat,
           model=model.file)

##Check convergence, if convergence is ok then a summary of the parameters will print
get_diagnostics(dat,
                model=model.file)

##If convergence is not ok check the traceplots for flagged parameters e.g.,
PlotTrace("cp[2]",
          model=model.file)

##If diagnostics look ok, get estimates and plot the results
# get_ests will return mean estimate, sd and 95% uncertainty interval (l95 = 95% lower bound, u95 = 95% upper bound)
get_ests(dat,model=model.file)
# Plot results, choose an axis label and a title
plot_res(dat,
         model=model.file,
         yaxis.lab="Temperature anomoly",
         title="HadCRUT")




