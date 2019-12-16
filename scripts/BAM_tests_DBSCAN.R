#cbrinkerhoff uses training 2 for pepsi1


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
  a0Hat_func <- function(x){-0.2918-0.1887* sd(log(x))+1.6930*mean(log(x))} #r2 0.907
  Wbhat_func <- function(x){0.0037+1.0028*mean(log(x))} #r2 0.984
  Dbhat_func <- function(x){-2.6189-0.2436* sd(log(x))+0.6854*mean(log(x))} #r2 0.640
  rhat_func <- function(x){1.4241-1.9097* sd(log(x))+0.0420*mean(log(x))} #r2 0.421

  nhat_func <- function(x) {-0.1636+0.4077*log(x)} #r2: 0.631 for each Sobs
  nhat_func_new <- function(x) {-0.1636+0.4077*mean(log(x))} #r2: 0.631 for entire river
  #bhat_func_old <- function(x) {0.02161+0.4578*sd(log(x))} #mark's b model
  
  #note this is log10
  Qbhat_func <- function(x) {-1.8699 + 1.8871*mean(log10(x))} #r2 0.89
  
  #hat variables
 # a0hat <- apply(W_obs, 1, a0Hat_func)
#  Wbhat <- apply(W_obs, 1, Wbhat_func)
  Dbhat <- apply(W_obs, 1, Dbhat_func)
  Qbhat <- apply(W_obs, 1, Qbhat_func)
 # nhat <- apply(S_obs, 1, nhat_func_new) #nhat_func(S_obs)
  #bhat <- apply(W_obs, 1, bhat_func)
  #rhat <- apply(W_obs, 1, rhat_func)
  
  #AHG/AMHG Flag (in log base 10 NOT natural log like everything else)
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
  
  #Classify river for supervised framework
  #classify_func <- function(x) {
  #  temp <- filter(output, river == file)
  #  width <- temp$X5.20971786829646 #mean(log(x))
  #  maxWidth = 6.5
  #  classes <- widthsClass[,7] #median width of each river type
  #  output <- ifelse(width > maxWidth, 101, which.min(abs(classes-width)))
    
  #  return(output)
  #}
  
  rivClass <- ifelse(rivClass == 0, 101, rivClass) #101 for 'noise' rivers
  print(rivClass)
  
  #create variables
  rhat <- 0
  bhat <- 0
  nhat <- 0
  a0hat <- 0
  Wbhat <- 0
  Dbhat <- 0
  r_sd <- 0
  B_sd <- 0
  n_sd <- 0
  Wb_sd <- 0
  Db_sd <- 0
  a0_sd <- 0
  r_lower <- 0
  r_upper <- 0
  b_lower <- 0
  b_upper <- 0
  a0_upper <- 0
  a0_lower <- 0
  Db_upper <- 0
  Db_lower <- 0
  Wb_upper <- 0
  Wb_lower <- 0
  
    if (rivClass == 101){ #'noise' rivers
      rhat <- apply(W_obs, 1, rhat_func)
      bhat <- apply(W_obs, 1, bhat_func)
      nhat <- apply(S_obs, 1, nhat_func_new)
      Dbhat <- apply(W_obs, 1, Dbhat_func)
      Wbhat <- apply(W_obs, 1, Wbhat_func)
      a0hat <- apply(W_obs, 1, a0Hat_func)
      
      r_sd = SDs[3,2]
      n_sd = SDs[4,2]
      B_sd = SDs[6,2]
      a0_sd <- SDs[5,2]
      Wb_sd <- SDs[1,2]
      Db_sd <- SDs[2,2]
    
      r_lower = r_global[3,2]
      b_lower = b_global[3,2]
    
      r_upper = r_global[7,2]
      b_upper = b_global[7,2]
      
      a0_upper <- A0[7,2] #1000000 #set big for supervised
      a0_lower <- A0[3,2] #A0[4,2] #set to 25 percentile of global prior for supervised
      
      Wb_upper <- Wb[7,2] #10 #set big for supervised
      Wb_lower <- Wb[3,2] #Wb[4,2] #set to 25 percentile of global prior for supervised
      
      Db_upper <- Db[7,2] #5 #set big for supervised
      Db_lower <- Db[3,2] #Db[4,2] #set to 25 percentile of global prior for supervised
    }
  
  #DBSCAN CURRENT IMPLEMENTATION
   else if (rivClass != 100){ #'classified' rivers
     for (k in 1:nrow(W_obs)) {
        rhat[k] = r[rivClass, 7]
        nhat[k] = N[rivClass, 7]
        bhat[k] = B[rivClass, 7]
        a0hat[k] = A0class[rivClass, 7]
        Wbhat[k] = Wbclass[rivClass, 7]
        Dbhat[k] = Dbclass[rivClass, 7]
      
        r_upper <- r[rivClass,9]
        r_lower <- r[rivClass,5]
        r_sd <- r[rivClass,4]
    
        n_sd <- N[rivClass,4]
    
        b_upper <- B[rivClass,9]
        b_lower <- B[rivClass,5]
        B_sd <- B[rivClass,4]
      
        a0_upper <- A0class[rivClass,9]
        a0_lower <- A0class[rivClass,5]
        a0_sd <- A0class[rivClass,4]
      
        Wb_upper <- Wbclass[rivClass,9]
        Wb_lower <- Wbclass[rivClass,5]
        Wb_sd <- Wbclass[rivClass,4]
      
        Db_upper <- Dbclass[rivClass,9]
        Db_lower <- Dbclass[rivClass,5]
        Db_sd <- Dbclass[rivClass,4]
     }
    }
  
    #'braided' ,or high-width variability, river flag, using values from distribution of all x-sections where r < 1
    braidedFlag <- ifelse(sd(log(W_obs)) >= 0.45, 1, 0)
    if (braidedFlag == 1){
      rivClass = 102
      for (k in 1:nrow(W_obs)) {
        print('high width variability')
    
        #hardcoded priors for training data where r < 1
        rhat[k] <- -0.249
        bhat[k] <- 0.405
        nhat[k] <- -3.41
        Wbhat[k] <- 3.039
        Dbhat[k] <- -1.00
        a0hat[k] <- 4.394
    
        b_upper <- 0.77 
        b_lower <- 0.029
        B_sd <- 0.11
    
        r_upper <- 0
        r_lower <- -2.58
        r_sd <- 0.412
  
        n_sd <- 1.23
      
        Wb_upper <- 6.917
        Wb_lower <- -0.1227
        Wb_sd <- 1.284
    
        Db_upper <- 2.572
        Db_lower <- -3.02
        Db_sd <- 1.147
      
        a0_upper <- 11.55
        a0_lower <- 0.262
        a0_sd <- 2.285
      }
    }
  
  priors <- bam_priors(bamdata = bamdata, logQ_sd = cv2sigma(1), 
                       logr_hat = rhat, logWb_hat = Wbhat, logDb_hat = Dbhat, b_hat = bhat,logn_hat = nhat, logA0_hat = a0hat, 
                       lowerbound_A0 = exp(a0_lower), upperbound_A0 = exp(a0_upper),
                       lowerbound_logr = r_lower, upperbound_logr=r_upper,
                       lowerbound_logWb = Wb_lower, upperbound_logWb=Wb_upper,
                       lowerbound_logDb = Db_lower, upperbound_logDb=Db_upper,
                       lowerbound_b = b_lower, upperbound_b = b_upper,
                       lowerbound_logn = log(0.01), upperbound_logn=log(0.05), #set manually by cbrinkerhoff as part of 'new priors' and kept for geomorphic
                       logr_sd=r_sd, logWb_sd = Wb_sd, logDb_sd=Db_sd, logn_sd=n_sd,  b_sd=B_sd, logA0_sd = a0_sd)
  
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
  
  
  
  switch <- 0
  if (amhgFlag == 1){
    print('switch')
    run_bam_cbrinkerhoff= bam_estimate(bamdata=bamdata, bampriors = priors, stanmodel = cbrinkerhoff_model, variant = 'manning_amhg')
    switch <- 1
  }
  else {
    run_bam_cbrinkerhoff= bam_estimate(bamdata=bamdata, bampriors = priors, stanmodel = cbrinkerhoff_model, variant = 'manning')
  }
  
  qobs=colMeans(Q_obs)
  
  bam_stats_cbrinkerhoff=bam_validate(run_bam_cbrinkerhoff, qobs, stats = c("RRMSE", "NRMSE", "NSE", "rBIAS"))
  
  qPred <- bam_qpred(run_bam_cbrinkerhoff)
  
  hydrograph <- data.frame()
  hydrograph <- rbind(hydrograph, qPred)

  write.csv(hydrograph, paste(output_string, 'predQ_Craig_', substr(file,1,(nchar(file)-2)),'csv',sep=""))
  
  #outpt error metrics
  statistics <- data.frame()
  statistics <- rbind(statistics, bam_stats_cbrinkerhoff$stats)
  colnames(statistics) <- c("RRMSE", "NRMSE", "NSE", "rBIAS")
  
  write.csv(qobs, paste(output_string, 'obsQ_', substr(file,1,(nchar(file)-2)),'csv',sep=""))
  
  write.csv(statistics,output_string)
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
training2='C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Tests_Data\\'

allpepsi2=training2

minxs=3
mintime=3

for (phase in allpepsi2){
  setwd(phase)
  output_directory=paste('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//outputs//')#new_new_reachN_DBSCAN_20_Class_all_priors_regression_fin//hydrographs//')
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
B <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsBClassDBSCAN_20.csv')
b_global <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsB.csv')
N <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsNClassDBSCAN_20.csv')
A0 <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsA0.csv')
Db <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsDb.csv')
Wb <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsWb.csv')
A0class <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsA0ClassDBSCAN_20.csv')
Dbclass <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsDbClassDBSCAN_20.csv')
Wbclass <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsWbClassDBSCAN_20.csv')
r <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsRClassDBSCAN_20.csv')
r_global <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorsR.csv')
SDs <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//priorSDs.csv')
widthsClass <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priors//WidthsClass.csv')
#rivClasses <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//pepsiClassified_ep_20.csv') #random forest outputs

#remove noise class
B <- B[-1,]
N <- N[-1,]
A0class <- A0class[-1,]
Wbclass <- Wbclass[-1,]
Dbclass <- Dbclass[-1,]
r <- r[-1,]

#Compile and run my BAM stan model
cbrinkerhoff_model <- stan_model("C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//master.stan", model_name = 'cbrinkerhoff_model')

#cbrinkerhoff priors
test_priors <- options_manager(paramnames=c("lowerbound_logQ", "upperbound_logQ", "lowerbound_A0", "upperbound_A0", "lowerbound_logn",
                                            "upperbound_logn", "lowerbound_logQc", "upperbound_logQc", "lowerbound_logWc", "upperbound_logWc",
                                            "lowerbound_b", "upperbound_b", "lowerbound_logWb", 'upperbound_logWb', 'lowerbound_logDb',
                                            'upperbound_logDb', 'lowerbound_logr', 'upperbound_logr',
                                            "sigma_man", "sigma_amhg",
                                            "logQc_hat", "logWc_hat", "b_hat", "logA0_hat", "logn_hat",  'logWb_hat', 'logDb_hat', 'logr_hat',
                                            'logQ_sd', "logQc_sd", "logWc_sd", "b_sd",  "logA0_sd", "logn_sd", 'logWb_sd', 'logDb_sd', 'logr_sd',
                                            "Werr_sd", "Serr_sd", "dAerr_sd"),
                               lowerbound_logQ=bam_settings('lowerbound_logQ'), upperbound_logQ=bam_settings('upperbound_logQ'),
                               lowerbound_A0 = 30, upperbound_A0 = 1000000,
                               lowerbound_logn = -4.6, upperbound_logn = -1.5,
                               lowerbound_logQc = 0, upperbound_logQc=10,
                               lowerbound_logWc = 1, upperbound_logWc=8,
                               lowerbound_b = 0.01, upperbound_b = 0.8,
                               lowerbound_logWb = 0, upperbound_logWb = 0,
                               lowerbound_logDb = 0, upperbound_logDb = 0,
                               lowerbound_logr = 0, upperbound_logr = 0,
                               sigma_man=0.25, sigma_amhg = 0.22,
                               logQc_hat=bam_settings('logQc_hat'), logWc_hat=bam_settings('logWc_hat'),
                               b_hat=bam_settings('b_hat'), logA0_hat = bam_settings('logA0_hat'), logn_hat = bam_settings('b_hat'),
                               logWb_hat = 0, logDb_hat = 0, logr_hat = 0,
                               logQ_sd=0, logQc_sd = 0.8325546, logWc_sd=4.712493, b_sd=0.05, logA0_sd=0.5,
                               logn_sd=0.25, logWb_sd = 0, logDb_sd = 0, logr_sd = 0,
                               Werr_sd=10, Serr_sd=0.00001, dAerr_sd=10)

#for identifying big rivers------------------------------------
library(ncdf4)

output <- data.frame()
for (k in 1:length(phase_files)) {
  file <- phase_files[k]
  data_in =nc_open(file)
  
  W_obs = ncvar_get(data_in, 'Reach_Timeseries/W')

  temp <- sd(log(W_obs))
  
  output <- rbind(output, temp)
}
output$river <- phase_files

for (i in 1:(length(phase_files))) {
  print(phase_files[i])
  rivClass <- rivClasses$cluster[i]
  statis <- batchbam(phase_files[i], output_directory, errorflag, minxs, mintime)
  print(paste('river ', phase_files[i],  ' done', sep=''))
}
