#Craig uses training 2 for pepsi1
#bsub -q long -n 8 -R rusage[mem=20000] -W 12:00 /home/cb63a/batchBAM.sh 

library(ncdf4)
library(bamr)
library(parallel)
library(stringr)
library(ggplot2)
library(rstan)
library(settings)

#function to run bam and set priors
batchbam = function(file,directory,errorflag, minxs, mintime){ 
  options(warn=-1)
  library(ncdf4)
  library(bamr)
  library(dplyr)
  library(settings)
  library(rstan)
  #functions for dA--------
  #' Calculate partial cross-section area from DAWG-formatted width and height matrices
  #' @param w Matrix of widths
  #' @param h Matrix of heights(FROM MARK)
  calcdA_mat <- function(w, h) {
    stopifnot(all(dim(w) == dim(h)))
    dA <- w
    for (i in 1:nrow(dA)) {
      dA[i, ] <- calcdA_vec(w[i, ], h[i, ])
    }
    dA
  }
  
  #' Calculate partial cross-section area from width and height vectors (time series)
  #' @param w vector of widths
  #' @param h vector of heights(FROM MARK)
  calcdA_vec <- function(w, h) {
    words <- order(w)
    warr <- w[words]
    harr <- h[words]
    delh <- c(0, diff(harr))
    delA <- cumsum(warr * delh)
    dA <- 1:length(w)
    dA[words] <- delA
    dA
  }
  #-----------------
  output_string= paste(directory,'BAM_Craig_',substr(file,1,(nchar(file)-2)),'csv',sep="")
  #output_string2= paste(directory,'BAM_Craig_switch_',substr(file,1,(nchar(file)-2)),'csv',sep="")
  
  data_in =nc_open(file)
  
  W_obs=ncvar_get(data_in,'Reach_Timeseries/W')
  H_obs=ncvar_get(data_in,'Reach_Timeseries/H')
  S_obs=ncvar_get(data_in,'Reach_Timeseries/S')
  Q_obs=ncvar_get(data_in,'Reach_Timeseries/Q')
  Q_prior=ncvar_get(data_in,'River_Info/QWBM')
  
  if (length(dim(W_obs))<2){
    output=rep(-9999,times=length(W_obs))
    write.csv(output,output_string)
    return(output)}
  
  
  good_reaches=ncvar_get(data_in,'River_Info/gdrch')
  
  W_obs=W_obs[good_reaches,]
  H_obs=H_obs[good_reaches,]
  S_obs=S_obs[good_reaches,]
  Q_obs=Q_obs[good_reaches,]
  
  S_obs[S_obs==0]=1e-6
  S_obs[S_obs<0]=NA
  W_obs[W_obs<0]=NA
  H_obs[H_obs<0]=NA
  Q_obs[Q_obs<0]=NA
  
  
  if (any(apply(S_obs,2,sum,na.rm=TRUE) ==0)){
    remove_index =  which((apply(S_obs,2,sum,na.rm=TRUE) ==0) ==TRUE)
    
    W_obs=W_obs[,-remove_index]
    H_obs=H_obs[,-remove_index]
    S_obs=S_obs[,-remove_index]
    Q_obs=Q_obs[,-remove_index]
    
  }
  
  if (any(apply(S_obs,1,sum,na.rm=TRUE) ==0)){
    remove_index =  which((apply(S_obs,1,sum,na.rm=TRUE) ==0) ==TRUE)
    
    W_obs=W_obs[-remove_index,]
    H_obs=H_obs[-remove_index,]
    S_obs=S_obs[-remove_index,]
    Q_obs=Q_obs[-remove_index,]
    
  }
  # 
  
  
  if (ncol(W_obs)<mintime){
    output=rep(-9999,times=ncol(W_obs))
    write.csv(output,output_string)
    return(output)
  }
  
  if (nrow(W_obs)<minxs){
    output=rep(-9999,times=ncol(W_obs))
    write.csv(output,output_string)
    return(output)
  }
  
  #according to BAM help, we need to calculate area above base area- 
  
  #min area at Hmin
  dA=calcdA_mat(W_obs,H_obs)
  
  #generate bam formatted data and priors
  bamdata=bamr::bam_data(w=W_obs,dA=dA,s=S_obs,Qhat=as.vector(Q_prior))
  
  #functions calculated in jupyter notebook and inputted manually here
  bhat_func <- function(x){0.0569+0.3822* sd(log(x))} #r2 0.726
  a0Hat_func <- function(x){-0.2918+-0.1887* sd(log(x))+1.6930*mean(log(x))} #r2 0.907
  Wbhat_func <- function(x){0.0037+1.0028*mean(log(x))} #r2 0.984
  Dbhat_func <- function(x){-2.6189-0.2436* sd(log(x))+0.6854*mean(log(x))} #r2 0.640
  rhat_func <- function(x){1.4241-1.9097* sd(log(x))+0.0420*mean(log(x))} #r2 0.421

  nhat_func <- function(x) {-0.1636+0.4077*log(mean(x))} #r2: 0.631 for entire river
  nhat_func_new <- function(x) {-0.1636+0.4077*mean(log(x))} #r2: 0.631
  #bhat_func_old <- function(x) {0.02161+0.4578*sd(log10(x))} #mark's b model
  
  #note this is log10
  Qbhat_func <- function(x) {-1.8699 + 1.8871*mean(log10(x))} #r2 0.89
  
  #hat variables
  a0hat <- apply(W_obs, 1, a0Hat_func)
  Wbhat <- apply(W_obs, 1, Wbhat_func)
  Dbhat <- apply(W_obs, 1, Dbhat_func)
  Qbhat <- apply(W_obs, 1, Qbhat_func)
  nhat <- nhat_func_new(S_obs)
  bhat <- apply(W_obs, 1, bhat_func)
  rhat <- apply(W_obs, 1, rhat_func)
  
  #AHG/AMHG Flag (in log base 10 NOT natural log like everyting else)
  H_obs = rowMeans(ncvar_get(data_in, 'XS_Timeseries/H'))
  xs_bnds = ncvar_get(data_in, 'XS_Timeseries/X')
  S_obs_model <- c(0,1)
  for (i in 1:length(H_obs)) {
    S_obs_model[i] = abs((H_obs[i]-H_obs[i+1])/(xs_bnds[i+1]-xs_bnds[i]))
  }
  Dehat <- 19*exp(Dbhat)*S_obs_model
  
  amhgFlag = 0
  regimeS <- 0.44*Dehat^(1.15)*Qbhat^(-0.46)
  regimeFit <- summary(lm(regimeS~S_obs_model))$r.squared
  if (regimeFit > 0.90) {amhgFlag = 1}
  else {amhgFlag = 0}
    
  priors <- bam_priors(bamdata = bamdata, logQ_sd = cv2sigma(1), 
                       b_hat = bhat,logn_hat = nhat, logA0_hat = a0hat, 
                       lowerbound_A0 = exp(A0[3,2]), upperbound_A0 = exp(A0[7,2]),
                       lowerbound_b = B[3,2], upperbound_b = B[7,2],
                       lowerbound_logn = log(0.01), upperbound_logn=log(0.05),
                       logn_sd=SDs[4,2],  b_sd=SDs[6,2], logA0_sd=SDs[5,2])
  
  
  #custom priors post Hagemann et al 2017
  priors$lowerbound_logQc=0.01
  priors$upperbound_logn=log(0.05)
  #braided river prior set
  #lower A0 for small rivers
  if (max(dA,na.rm=TRUE)<2000){
    priors$lowerbound_A0= 1
    priors$upperbound_A0= 1e3
  }
  #sigma man control
  
  #sigma AMHG control
  
  #Qbounds
  if (  priors$logWc_hat == -Inf){
    priors$logWc_hat=mean(log(W_obs),na.rm=TRUE) #in case of error
  }
  if (  priors$lowerbound_logQ == -Inf){
    priors$lowerbound_logQ=  priors$logQc_hat- (priors$upperbound_logQ-priors$logQc_hat) #in case of error
  }
  
  if (  priors$upperbound_logQ == -Inf){
    priors$upperbound_logQ=  priors$logQc_hat+ (priors$logQc_hat-priors$lowerbound_logQ) #in case of error
  }
  
  river_name= substr(file,1,nchar(file)-3)
  
  if (errorflag==1.5){
    
    
    
    priors$Werr_sd  =  1.5* error_table[error_table$X==river_name,3] #width in column 3
    priors$Serr_sd  =  1.5* error_table[error_table$X==river_name,5] #slope in m/m column 5
    priors$dAerr_sd =  1.5* error_table[error_table$X==river_name,6] #max da error in column 6 (rectangle)
    
    
  } else if (errorflag==0.5){
    priors$Werr_sd  =  0.5* error_table[error_table$X==river_name,3] #width in column 3
    priors$Serr_sd  =  0.5* error_table[error_table$X==river_name,5] #slope in m/m column 5
    priors$dAerr_sd =  0.5* error_table[error_table$X==river_name,6] #max da error in column 6 (rectangle)
    
    
  } else if (errorflag == 1.0) {
    
    
    priors$Werr_sd  =  1.0* error_table[error_table$X==river_name,3] #width in column 3
    priors$Serr_sd  =  1.0* error_table[error_table$X==river_name,5] #slope in m/m column 5
    priors$dAerr_sd =  1.0* error_table[error_table$X==river_name,6] #max da error in column 6 (rectangle)
    
  }  else if (errorflag == 0) {
    priors$Werr_sd  = 0.05
    priors$Serr_sd  = 1.0e-11
    priors$dAerr_sd = 0.05
    
  }
  
  
  
  #run it
  switch <- 0
  if (amhgFlag == 1){
    run_bam_Craig= bam_estimate(bamdata=bamdata, bampriors = priors, variant = 'manning_amhg')
    switch <- 1
  }
  else {
    run_bam_Craig= bam_estimate(bamdata=bamdata, bampriors = priors, variant = 'manning')
  }
  
  qobs=colMeans(Q_obs)
  
  bam_stats_Craig=bam_validate(run_bam_Craig, qobs, stats = c("RRMSE", "NRMSE", "NSE", "rBIAS"))
  
  #outpt error metrics
  statistics <- data.frame()
  statistics <- rbind(statistics, bam_stats_Craig$stats)
  colnames(statistics) <- c("RRMSE", "NRMSE", "NSE", "rBIAS")
  
  write.csv(statistics,output_string)
  print(switch)
  return(statistics)
  
}
#-------------------------
phase1='/home/cb63a/BAMtests/Pepsi2/Pepsi2'
phase2a='C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\NC_files(1)\\Pepsi1\\'
phase2b_1.5='D:/Box Sync/Pepsi 2/phase 2b/NetCDF-OneandHalf/'
phase2b_0.5='D:/Box Sync/Pepsi 2/phase 2b/NetCDFHalf/'
phase2b_1.0='D:/Box Sync/Pepsi 2/phase 2b/NetCDF/'
phase2c='D:/Box Sync/Pepsi 2/phase 2c/NetCDF-SWOTSampling/'

training1='D:/Box Sync/Pepsi 2/phase 2a/Pepsi1/7day/'
training2= '/home/cb63a/BAMtests/Tests_Data/'

allpepsi2=training2

minxs=3
mintime=3

for (phase in allpepsi2){
  setwd(phase)
  output_directory=paste('/home/cb63a/BAMtests/old_physics_new_priors_correctSD_correctN/')
  phase_files= list.files(pattern = "\\.nc$")
  # phase_files=phase_files[1]
  #output_string= paste(training1,'metricscv2.csv',sep="")
  
  if (phase==phase2b_1.5){
    errorflag=1.5 
  } else if (phase== phase2b_0.5){
    errorflag=0.5
  } else if (phase==phase2b_1.0 | phase==phase2c) {
    errorflag=1.0
  } else {
    errorflag=0
  }
}

#prior distributions
B <- read.csv('/home/cb63a/BAMtests/priorsB.csv')
N <- read.csv('/home/cb63a/BAMtests/priorsN.csv')
A0 <- read.csv('/home/cb63a/BAMtests/priorsA0.csv')
SDs <- read.csv('/home/cb63a/BAMtests/priorSDs.csv')

for (i in 1:(length(phase_files))) {
  statis <- batchbam(phase_files[i], output_directory, errorflag, minxs, mintime)
  print('river done')
}
