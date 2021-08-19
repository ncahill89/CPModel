gr_diag<-function(mcmc.array,
                  pars.check=c("beta[1]","beta[2]","cp")){
R <- rep(NA,length(pars.check))
p<-0
for (parname in pars.check){
  p <- p+1 # index
  mcmc.array.temp <- mcmc.array[,,parname]
  mcmc <- mcmc.list()

for (chain in 1:dim(mcmc.array.temp)[2]){
  mcmc[[chain]] <- as.mcmc(mcmc.array.temp[,chain])
}
  r<- gelman.diag(mcmc, autoburnin = FALSE, transform = F)$psrf
  R[p] <-r[,"Point est."]

}


names(R) <- pars.check


if (length(R[R>1.1])>0){
  cat(paste("Poor/no convergence for:", names(R[R>1.1]), "(R = ", round(R[R>1.1],3), ")", "\n"))
}
else
  cat(paste0("Rhat looks good, no  convergence issues indicated for checked parameters \n"))

if(length(R[R>1.1])>0)
  return(-1)
else
  return(0)

}


eff_size<-function(mcmc.array,
                   pars.check=c("beta[1]","beta[2]","cp")){
  ESS <- rep(NA,length(pars.check))
  p<-0
  for (parname in pars.check){
    p <- p+1 # index
    mcmc.array.temp <- mcmc.array[,,parname]
    mcmc <- mcmc.list()

    for (chain in 1:dim(mcmc.array.temp)[2]){
      mcmc[[chain]] <- as.mcmc(mcmc.array.temp[,chain])
    }
    es<- effectiveSize(mcmc)
    ESS[p] <-es/(dim(mcmc.array)[1]*dim(mcmc.array)[2])

  }
  names(ESS) <- pars.check
  if (length(ESS[ESS<0.1])>0){
    cat(paste0("Effective sample size is less than 10% of total iterations for parameter:"," ", names(ESS[ESS<0.1])," ", "(",round(ESS[ESS<0.1],3)*100,"%",")", "\n"))
    cat(paste0("Additional thinning may be required! \n"))
    }
  else
    cat(paste0("No apparent autocorrelation issues for checked parameters. \n"))


}

mcse<-function(mcmc.array,
               pars.check=c("beta[1]","beta[2]","cp")){
  MCSE <- rep(NA,length(pars.check))
  p<-0
  for (parname in pars.check){
    p <- p+1 # index
    mcmc.array.temp <- mcmc.array[,,parname]
    mcmc <- mcmc.list()

    for (chain in 1:dim(mcmc.array.temp)[2]){
      mcmc[[chain]] <- as.mcmc(mcmc.array.temp[,chain])
    }
    es<- effectiveSize(mcmc)
    MCSE[p] <-(sd(mcmc.array.temp)/es)/sd(mcmc.array.temp)

  }
  names(MCSE) <- pars.check
  if (length(MCSE[MCSE>0.1])>0){
    cat(paste0("The Monte Carlo standard error is greater than 10% of the posterior standard deviation for parameter:"," ", names(MCSE[MCSE>0.1])," ", "(",round(MCSE[MCSE>0.1],3)*100,"%",")", "\n"))
    cat(paste0("Sampling error variation appears too large! \n"))
  }
  else
    cat(paste0("The accuracy of the parameter estimation is adequate. \n"))
}


PlotTrace <- function(#Traceplot for one parameter
  ### Trace plot for one parameter and add loess smoother for each chain
  parname,
  n_cp = 1,
  n.chains= NULL, n.sim= NULL, main = NULL){

  # load the MCMC output
  if(n_cp == 1)
  {
    load(paste0("output/jags_mod",1,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
  }

  if(n_cp == 2)
  {
    load(paste0("output/jags_mod",2,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
  }

  if(n_cp == 3)
  {
    load(paste0("output/jags_mod",3,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
  }

  if(n_cp == 4)
  {
    load(paste0("output/jags_mod",4,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
  }

  if (is.null(main)) main <- parname
  if (is.null(n.sim)) n.sim <- dim(mcmc.array)[1]
  if (is.null(n.chains)) n.chains <- dim(mcmc.array)[2]
  plot(c(mcmc.array[,1,parname]), type = "l", ylab = parname,  main = main,
       ylim = c(min(mcmc.array[,,parname]),max(mcmc.array[,,parname])))
  for (chain in 1:n.chains){
    lines(c(mcmc.array[,chain,parname]), type = "l", col = chain)
  }
  for (chain in 1:n.chains){
    curve(predict(loess(c(mcmc.array[,chain,parname])~seq(1,n.sim)),x), lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
  }
}

