#Craig uses training 2 for pepsi1
#bsub -q long -n 8 -R rusage[mem=20000] -W 6:00 /home/cb63a/batchMark.sh 

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
  
  p_int <- as.matrix(read.table('/home/cb63a/BAMtests/p_int.txt', sep='\t', header=TRUE))

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

  priors <- bam_priors(bamdata = bamdata, logQ_sd = cv2sigma(1))
  
  
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
  
  
  
  ##run it
  p_int_river <- ifelse(file != 'Ohio.nc', as.numeric(p_int[which(p_int[,2]==file)]), -9998)
  p_int_river <- ifelse(file == 'Tanana.nc', 0, p_int_river)
  switch <- 0
  #mark switch
   if ((p_int_river > 0.15 & sd(log10(W_obs)) > 0.1)|p_int_river==-9998){
    run_bam_Craig= bam_estimate(bamdata=bamdata, variant = 'manning_amhg')
    switch <- 1
  }
  else {
    run_bam_Craig= bam_estimate(bamdata=bamdata, variant = 'manning')
  }
  
  qobs=colMeans(Q_obs)
  qPred <- bam_qpred(run_bam_Craig)

  hydrograph <- data.frame()
  hydrograph <- rbind(hydrograph, qPred)
  
  write.csv(hydrograph, paste(output_string, 'predQ_Craig_', substr(file,1,(nchar(file)-2)),'csv',sep=""))
  write.csv(qobs, paste(output_string, 'obsQ_', substr(file,1,(nchar(file)-2)),'csv',sep=""))  


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
training2='/home/cb63a/BAMtests/Tests_Data/'

allpepsi2=training2

minxs=3
mintime=3

for (phase in allpepsi2){
  setwd(phase)
  output_directory=paste('/home/cb63a/BAMtests/mark_switch_rBIAS/test/')
  phase_files= list.files(pattern = "\\.nc$")
  #phase_files=phase_files[3]
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

start_time= Sys.time()
num_cores <- 3 #detectCores() - 4
cluster= makeCluster(num_cores)

bam_hydrograph=parLapply(cluster,phase_files,batchbam,output_directory,errorflag,minxs,mintime)

stopCluster(cluster)
end_time= Sys.time()

elapsed_time=end_time-start_time
elapsed_time