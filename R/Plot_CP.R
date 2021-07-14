plot_data<-function(dat,
                    title='')
{
  dir.create("fig", showWarnings = FALSE)

  p<-ggplot(dat, aes(y=y,x=year))+
    geom_point()+
    ylab('Y')+
    ggtitle(title)
  ggsave(p, file = 'fig/dat_plot.pdf', width = 10, height = 6)
  cat("Plot of raw data saved to figure folder \n")
  return(p)
}

plot_res<-function(dat,
                   model="model/CP_model.txt",
                   yaxis.lab="Y",
                   title="")
{

  # load the MCMC output
  if(grepl("1",model))
  {

    load(paste0("output/jags_mod",1,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP1_plot.pdf"
  }

  if(grepl("2",model))
  {
    load(paste0("output/jags_mod",2,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP2_plot.pdf"
  }

  if(grepl("3",model))
  {
    load(paste0("output/jags_mod",3,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP3_plot.pdf"
  }

  if(grepl("4",model))
  {
    load(paste0("output/jags_mod",4,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP4_plot.pdf"
  }

# Load jags data
load("output/jags_data.Rdata")
# Get posterior samples
sstar_s<-mod$BUGSoutput$sims.list$s_star
# Get estimates and uncertainty bounds
sstar_mean<-apply(sstar_s,2,mean)
sstar_upr<-apply(sstar_s,2,quantile,probs=0.025)
sstar_lwr<-apply(sstar_s,2,quantile,probs=0.975)

# Create data frame for plotting
df.s<-data.frame(sstar_mean,sstar_upr,sstar_lwr,jags_data$year.t*1000)
names(df.s)<-c("Y","upr","lwr","Year")

# Plot results
res<-ggplot()+
  geom_line(data=df.s,aes(x=Year,y=Y),colour="red")+
  geom_ribbon(data=df.s,aes(ymin=lwr,ymax=upr,x=Year,colour="red"),alpha=0.1,colour="red")+
  geom_point(data=dat,aes(y=y,x=year))+
  ylab(yaxis.lab)+
  ggtitle(title)+
  theme_minimal()
ggsave(res, file = paste0("fig/",saveas), width = 10, height = 6)

cat("Plot of estimates saved to figure folder \n")

return(res)
}
