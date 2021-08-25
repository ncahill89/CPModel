plot_data<-function(dat,
                    title='',
                    include_errors = FALSE)
{
  dir.create("fig", showWarnings = FALSE)

  if(include_errors == TRUE & suppressWarnings(is.null(dat$y_error)))
    stop("If include_errors argument = TRUE then the dataset must include year_error and y_error")

  p<-ggplot(dat, aes(y=y,x=year))+
    geom_point()+
    ylab('Y')+
    ggtitle(title) +
    theme_bw()

  if(include_errors == TRUE)
  {
    dat <- dat %>%
      mutate(y1_lwr = y - y_error,
             y2_lwr = y - y_error,
             y3_upr = y + y_error,
             y4_upr = y + y_error,
             x1_upr = year + year_error,
             x2_lwr = year - year_error,
             x3_lwr = year - year_error,
             x4_upr = year + year_error)

    get_bounds <- dat %>%
      dplyr::select(y1_lwr:x4_upr) %>%
      dplyr::mutate(obs_index = 1:n()) %>%
      tidyr::pivot_longer(cols = y1_lwr:x4_upr,
                   names_to = "bounds",
                   values_to = "value") %>%
      dplyr::mutate(bounds = replace(bounds, bounds %in% c("y1_lwr","y2_lwr","y3_upr","y4_upr"), "y"),
             bounds = replace(bounds, bounds %in% c("x1_upr","x2_lwr","x3_lwr","x4_upr"), "x"))

    x_bounds <- get_bounds %>%
      filter(bounds == "x")

    y_bounds <- get_bounds %>%
      filter(bounds == "y")

    data_to_plot <- tibble(obs_index = x_bounds$obs_index,
                           x = x_bounds$value,
                           y = y_bounds$value)


    p <- ggplot(data_to_plot, aes(x = x, y = y)) +
      geom_polygon(aes(group = obs_index),alpha = 0.3) +
      geom_point(data = dat, aes(x = year, y = y), alpha = 0.6, pch = 1) +
      ylab('Y') +
      ggtitle(title) +
      theme_bw()

  }
  ggsave(p, file = 'fig/dat_plot.pdf', width = 10, height = 6)
  cat("Plot of raw data saved to figure folder \n")
  return(p)
}

plot_res<-function(dat,
                   n_cp = 1,
                   yaxis.lab="Y",
                   title="")
{

  # load the MCMC output
  if(n_cp == 0)
  {

    load(paste0("output/jags_mod",0,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP1_plot.pdf"
  }

  # load the MCMC output
  if(n_cp == 1)
  {

    load(paste0("output/jags_mod",1,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP1_plot.pdf"
  }

  if(n_cp == 2)
  {
    load(paste0("output/jags_mod",2,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP2_plot.pdf"
  }

  if(n_cp == 3)
  {
    load(paste0("output/jags_mod",3,".Rdata"))
    mcmc.array<-mod$BUGSoutput$sims.array
    saveas<-"CP3_plot.pdf"
  }

  if(n_cp == 4)
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

# get ests
par_est <- get_ests(dat,
         n_cp = n_cp)


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

return(list(p_res=res, df.s = df.s))
}
