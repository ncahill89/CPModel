get_ests<-function(dat,
                   model="model/CP_model")
{
  # load the MCMC output  
  if(grepl("1",model))
  {
    load(paste0("output/jags_mod",1,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","cp")
  }
  
  if(grepl("2",model))
  {
    load(paste0("output/jags_mod",2,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","beta[3]","cp[1]","cp[2]")
  }
  
  if(grepl("3",model))
  {
    load(paste0("output/jags_mod",3,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","beta[3]","beta[4]","cp[1]","cp[2]","cp[3]")
  }
  
  if(grepl("4",model))
  {
    load(paste0("output/jags_mod",4,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    pars.check<-c("beta[1]","beta[2]","beta[3]","beta[4]","beta[5]","cp[1]","cp[2]","cp[3]","cp[4]")
  }
  
   # Get estimates and uncertainty
    pars.summary<-internalget_ests(mcmc.array,pars.check = pars.check)
    
    return(list(pars.summary=pars.summary))
  }
  
  



internalget_ests<-function(mcmc.array,
                   pars.check=c("beta[1]","beta[2]","cp")){
  mean<-sd<-l95<-u95<-rep(NA,length(pars.check))
  
  p<-0
  for (parname in pars.check){
    p <- p+1 # index
    
    if(grepl("beta",parname))
    mcmc.array.par<-mcmc.array[,,parname]/1000
    
    if(grepl("cp",parname))
      mcmc.array.par<-mcmc.array[,,parname]*1000
    
    
    mean[p] <- mean(mcmc.array.par)
    sd[p] <- sd(mcmc.array.par)
    l95[p]<-quantile(mcmc.array.par,probs=0.025)
    u95[p]<-quantile(mcmc.array.par,probs=0.975)
  }
  
  ests<-t(data.frame(mean,sd,l95,u95))
  colnames(ests)<-pars.check
  
  return(ests)
  
}