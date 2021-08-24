

get_diagnostics<-function(dat,
                          n_cp = 1)
{

  # load the MCMC output
  if(n_cp == 0)
  {
    load(paste0("output/jags_mod",0,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta","alpha","sigma_err")
  }


  # load the MCMC output
  if(n_cp == 1)
  {
    load(paste0("output/jags_mod",1,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","alpha[1]","cp","sigma_err")
  }

  if(n_cp == 2)
  {
    load(paste0("output/jags_mod",2,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","beta[3]","alpha[1]","alpha[2]","cp[1]","cp[2]","sigma_err")
  }

  if(n_cp == 3)
  {
    load(paste0("output/jags_mod",3,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","beta[3]","beta[4]","cp[1]","cp[2]","cp[3]")
  }

  if(n_cp == 4)
  {
    load(paste0("output/jags_mod",4,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","beta[3]","beta[4]","beta[5]","cp[1]","cp[2]","cp[3]","cp[4]")
  }

  # Get DIC
  DIC<-mod$BUGSoutput$DIC

  # Get gelman diagnostics (Rhat threshold = 1.1)
  # If gelman diagnostic fails then stop!
    gd<-gr_diag(mcmc.array,pars.check = pars.check)
    if(gd==-1)
    {
      cat("WARNING! Convergence issues, check trace plots \n")

    return(pars.check=pars.check)
    }
  # If gelman diagnostic passes then get other diagnostics
    if(gd==0)
    {
    eff_size(mcmc.array,pars.check = pars.check)
    mcse(mcmc.array,pars.check = pars.check)

    return(list(pars.checked=pars.check,DIC=DIC))
    }


}
