library(ncdf4)
Dbhat_func <- function(x){-2.6189-0.2436* sd(log(x))+0.6854*mean(log(x))} #r2 0.640

Dbhat_func_base10 <- function(x){-1.1374-0.2436* sd(log10(x))+0.6854*mean(log10(x))} #r2 0.640
Qbhat_func <- function(x) {-1.8699 + 1.8871*mean(log10(x))} #r2 0.89

#AMHG prediction model-------------------------------------------------------------
output <- data.frame()
for (k in 1:length(phase_files)) {
  S_obs <- c(0,1)
  file <- phase_files[k]
  data_in =nc_open(file)
  
  H_obs = rowMeans(ncvar_get(data_in, 'XS_Timeseries/H'))
  W_obs = ncvar_get(data_in, 'Reach_Timeseries/W')
  xs_bnds = ncvar_get(data_in, 'XS_Timeseries/X')
  for (i in 1:length(H_obs)) {
    S_obs[i] = abs((H_obs[i]-H_obs[i+1])/(xs_bnds[i+1]-xs_bnds[i]))
  }
  S_obs[S_obs==0]=1e-6
  S_obs[S_obs<0]=NA
  W_obs[W_obs<0]=NA
  
  Dbhat <- apply(W_obs, 1, Dbhat_func_base10)
  Qbhat <- apply(W_obs, 1, Qbhat_func)#ncvar_get(data_in, 'River_Info/QWBM')
  Dehat <- 19*10^(Dbhat)*S_obs
  
  regimeS <- 0.44*Dehat^(1.15)*Qbhat^(-0.46)
  regimeFit <- mean(log(W_obs))#summary(lm(regimeS~S_obs))$r.squared
  
  output <- rbind(output, regimeFit)
}
output$river <- phase_files