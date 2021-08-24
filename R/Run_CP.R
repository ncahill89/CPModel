RunCPModel<-function(n_cp = 1,
                     dat,
                     include_errors = FALSE,
                     useriter=40000,
                     userburnin=15000,
                     userthin=15,
                     userchainnum=2){
  # Create a directory "fig" and "modeloutput" in current working directory
  dir.create("fig", showWarnings = FALSE)
  dir.create("output", showWarnings = FALSE)

  if(include_errors == TRUE & suppressWarnings(is.null(dat$y_error)))
    stop("If include_errors argument = TRUE then the dataset must include year_error and y_error")

  ####Data and prediction years
  year.j<-dat%>%pull(year)
  y.j<-dat%>%pull(y)
  year.t<-seq(min(year.j),max(year.j),by=1)
  n_years<-length(year.t)
  n_obs<-length(year.j)

  if(include_errors)
  {
    y_sd <- dat %>% pull(y_error)
    year_sd <- dat %>% pull(year_error)

    error_dat <- list(y_sd = y_sd,
                      year_sd = year_sd/1000,
                      year.j = year.j/1000)

  }
  if(!include_errors)
  {
    error_dat <- list()
  }
  #####Get some indexes
  #find the observation year indexes in the prediction years
  matchyears<-match(year.j,year.t)

  ##Run the model
  ##The required data
  jags_data <- c(list(y.j=y.j,
                 matchyears=matchyears,
                 year.t=year.t/1000,
                 n_years=n_years,
                 n_obs=n_obs,
                 year_min=min(year.t/1000),
                 year_max=max(year.t/1000)),
                 error_dat)

  ##Parameters to look at
  jags_pars <- c("s_star",
              "beta",
              "alpha",
              "sigma_err",
              "cp")

  if(n_cp == 0)
  {
    cpmodel <- 0
    model <- ifelse(include_errors == TRUE,"model/CP0_model_EIV.txt","model/CP0_model.txt")

    myinitial<-function(){list("alpha"=runif(1,0,10),
                               "beta"=rnorm(1,0,3),
                               "sigma_err"=runif(1,0,1) )}
  }

  if(n_cp == 1)
  {
    cpmodel <- 1
    model <- ifelse(include_errors == TRUE,"model/CP1_model_EIV.txt","model/CP1_model.txt")

        myinitial<-function(){list("alpha"=runif(1,0,10),
                               "beta"=c(rnorm(2,0,3)),
                               "cp"=runif(1,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }

  if(n_cp == 2)
  {
    cpmodel<-2
    model <- ifelse(include_errors == TRUE,"model/CP2_model_EIV.txt","model/CP2_model.txt")

    myinitial<-function(){list("alpha"=c(rnorm(2,0,3)),
                               "beta"=c(rnorm(1,0,3),NA,rnorm(1,0,3)),
                               "cp.temp"=runif(2,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }

  if(n_cp == 3)
  {
    cpmodel<-3
    model <- ifelse(include_errors == TRUE,"model/CP3_model_EIV.txt","model/CP3_model.txt")

    myinitial<-function(){list("alpha"=c(rnorm(3,0,3)),
                               "beta"=c(rnorm(1,0,3),NA,NA,rnorm(1,0,3)),
                               "cp.temp"=runif(3,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }

  if(n_cp == 4)
  {
    cpmodel<-4
    model <- ifelse(include_errors == TRUE,"model/CP4_model_EIV.txt","model/CP4_model.txt")

    myinitial<-function(){list("alpha"=c(rnorm(4,0,3)),
                               "beta"=c(rnorm(1,0,3),NA,NA,NA,rnorm(1,0,3)),
                               "cp.temp"=runif(4,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }

  ########Run the model########
    mod <- suppressWarnings(jags(data=jags_data,
                parameters.to.save=jags_pars,
                inits=myinitial,
                model.file=model,
                n.chains=userchainnum,
                n.iter=useriter,
                n.burnin=userburnin,
                n.thin=userthin))
    save(mod, file = file.path("output", paste0("jags_mod",cpmodel, ".Rdata")))

    save(jags_data,file="output/jags_data.Rdata")

    return(cat("MCMC output saved to output folder \n"))
}
