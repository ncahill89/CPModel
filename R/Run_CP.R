RunCPModel<-function(model="model/CPmodel.txt",
                     dat,
                     useriter=40000,
                     userburnin=15000,
                     userthin=15,
                     userchainnum=2){
  # Create a directory "fig" and "modeloutput" in current working directory
  dir.create("fig", showWarnings = FALSE)
  dir.create("output", showWarnings = FALSE)


  ####Data and prediction years
  year.j<-dat%>%extract2('year')
  y.j<-dat%>%extract2('y')
  year.t<-seq(min(year.j),max(year.j),by=1)
  n_years<-length(year.t)
  n_obs<-length(year.j)
  
  #####Get some indexes
  #find the observation year indexes in the prediction years
  matchyears<-match(year.j,year.t)
  
  ##Run the model
  ##The required data
  jags_data <- list(y.j=y.j,
                 matchyears=matchyears,
                 year.t=year.t/1000,
                 n_years=n_years,
                 n_obs=n_obs,
                 year_min=min(year.t/1000),
                 year_max=max(year.t/1000)) 
  
  ##Parameters to look at
  jags_pars <- c("s_star",
              "beta",
              "alpha",
              "sigma_err",
              "cp")  
  
  if(grepl("1",model))
  {
    cpmodel<-1
    myinitial<-function(){list("alpha"=runif(1,0,10),
                               "beta"=c(rnorm(2,0,3)),
                               "cp"=runif(1,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }
  
  if(grepl("2",model))
  {
    cpmodel<-2
    myinitial<-function(){list("alpha"=c(rnorm(1,0,3),NA),
                               "beta"=c(rnorm(3,0,3)),
                               "cp.temp"=runif(2,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }
  
  if(grepl("3",model))
  {
    cpmodel<-3
    myinitial<-function(){list("alpha"=c(rnorm(1,0,3),NA,NA),
                               "beta"=c(rnorm(4,0,3)),
                               "cp.temp"=runif(3,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }
  
  if(grepl("4",model))
  {
    cpmodel<-4
    myinitial<-function(){list("alpha"=c(rnorm(1,0,3),NA,NA,NA),
                               "beta"=c(rnorm(5,0,3)),
                               "cp.temp"=runif(4,min(year.t/1000)+0.01,max(year.t/1000)-0.01),
                               "sigma_err"=runif(1,0,1) )}
  }
  
  ########Run the model########
    mod <- jags(data=jags_data, 
                parameters.to.save=jags_pars,
                inits=myinitial,
                model.file=model,
                n.chains=userchainnum, 
                n.iter=useriter,
                n.burnin=userburnin,
                n.thin=userthin)
    save(mod, file = file.path("output", paste0("jags_mod",cpmodel, ".Rdata")))
    
    save(jags_data,file="output/jags_data.Rdata")
    
    return(cat("MCMC output saved to output folder \n"))
}  
