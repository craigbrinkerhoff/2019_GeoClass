#Craig uses training 2 for pepsi1


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
  bhat_func <- function(x){0.0569+0.8801* sd(log10(x))} #r2 0.726
  ahat_func <- function(x){0.8223-1.9058*sd(log10(x))+0.6890*mean(log10(x))} #r2: 0.807
  a0Hat_func <- function(x){-0.1267+-0.1887* sd(log10(x))+1.6930*mean(log10(x))} #r2 0.907
  Wbhat_func <- function(x){0.0016+1.0028*mean(log10(x))} #r2 0.984
  Dbhat_func <- function(x){-1.1374-0.2436* sd(log10(x))+0.6854*mean(log10(x))} #r2 0.640
  rhat_func <- function(x){0.6185-1.9097* sd(log10(x))+0.0420*mean(log10(x))} #r2 0.421
  Qbhat_func <- function(x) {-1.8699 + 1.8871*mean(log10(x))} #r2 0.89
  
  #hat variables
  bhat <- apply(W_obs, 1, bhat_func)
  ahat <- apply(W_obs, 1, ahat_func)
  a0hat <- apply(W_obs, 1, a0Hat_func)
  Wbhat <- apply(W_obs, 1, Wbhat_func)
  Dbhat <- apply(W_obs, 1, Dbhat_func)
  rhat <- apply(W_obs, 1, rhat_func)
  Qbhat <- apply(W_obs, 1, Qbhat_func)

  #AHG/AMHG Flag
  H_obs = rowMeans(ncvar_get(data_in, 'XS_Timeseries/H'))
  xs_bnds = ncvar_get(data_in, 'XS_Timeseries/X')
  S_obs_model <- c(0,1)
  for (i in 1:length(H_obs)) {
    S_obs_model[i] = abs((H_obs[i]-H_obs[i+1])/(xs_bnds[i+1]-xs_bnds[i]))
  }
  Dehat <- 19*10^(Dbhat)*S_obs_model
  
  regimeS <- 0.44*Dehat^(1.15)*Qbhat^(-0.46)
  regimeFit <- summary(lm(regimeS~S_obs_model))$r.squared
  if (regimeFit < 0.95) {amhgFlag = 1}
  else {amhgFlag = 0}
  
  #assign river class to reach
  #classMedians = c(1.36, 1.51, 1.62, 1.69, 1.85, 1.95, 2.16, 2.52) #50TH PERCENTILE: c(1.15, 1.36, 1.48, 1.54, 1.66, 1.80, 2.00, 2.28)
  #reachClasses <- c(0,1)
  #for (i in 1:nrow(W_obs)) {
  #  meanW = log10(mean(W_obs[i,]))
  #  reachClasses[i] = which(abs(classMedians-meanW)==min(abs(classMedians-meanW)))
  #}
  
  priors <- bam_priors(bamdata = bamdata, logQ_sd = cv2sigma(1), 
                       b_hat=bhat, logWb_hat = Wbhat, logDb_hat = Dbhat, logr_hat = rhat, loga_hat = ahat, logA0_hat = a0hat, ahg_flag = amhgFlag)

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
  
  #rivers are big reaches= b sd should be up
  priors$b_sd=0.2
  
  #measurment error
  #error_table=read.csv('D:/Box Sync/Pepsi 2/Pepsi2Uncertainties.csv')
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
  run_bam_Craig= bam_estimate(bamdata=bamdata,bampriors=priors, stanmodel = craig_model, variant = 'manning_amhg')
  #run_bam_Craig= bam_estimate(bamdata=bamdata,bampriors=priors, variant = 'manning_amhg')
  #test=bam_hydrograph(run_bam_Craig,qobs=colMeans(Q_obs))
  
  #output=data.frame(test$data) %>%
  #  filter(stat=='mean')%>%
  #  group_by(time)%>%
  #  summarize(bamQ=mean(flow)) 
    

  qobs=colMeans(Q_obs)
  
  bam_stats_Craig=bam_validate(run_bam_Craig, qobs, stats = c("RRMSE", "NRMSE", "NSE"))

  #outpt error metrics
  statistics <- data.frame()
  statistics <- rbind(statistics, bam_stats_Craig$stats)
  #colnames(statistics) <- c("RRMSEcraig", "NRMSEcraig", "NSEcraig")
  colnames(statistics) <- c("RRMSEswitch", "NRMSEswitch", "NSEswitch")
  
  #return(c(file,round(bam_stats$stats,2)) )
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

#prior distributions
Wb <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priorsWb.csv')
Db <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priorsDb.csv')
r <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priorsR.csv')
a <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//priorsA.csv')

#Compile and run my BAM stan model
craig_model <- stan_model("C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//master.stan", model_name = 'craig_model')

#Craig priors
test_priors <- options_manager(paramnames=c("lowerbound_logQ", "upperbound_logQ", "lowerbound_A0", "upperbound_A0", "lowerbound_logn",
                                            "upperbound_logn", "lowerbound_logQc", "upperbound_logQc", "lowerbound_logWc", "upperbound_logWc",
                                            "lowerbound_b", "upperbound_b", "lowerbound_logWb", 'upperbound_logWb', 'lowerbound_logDb',
                                            'upperbound_logDb', 'lowerbound_logr', 'upperbound_logr', 'lowerbound_loga', 'upperbound_loga',
                                            "sigma_man", "sigma_amhg",
                                            "logQc_hat", "logWc_hat", "b_hat", "logA0_hat", "logn_hat",  'logWb_hat',
                                            'logDb_hat', 'logr_hat', 'loga_hat',
                                            'logQ_sd', "logQc_sd", "logWc_sd", "b_sd",  "logA0_sd", "logn_sd", 'logWb_sd', 'logDb_sd',
                                            'logr_sd', 'loga_sd',
                                            "Werr_sd", "Serr_sd", "dAerr_sd", "ahg_flag"),
                               lowerbound_logQ=bam_settings('lowerbound_logQ'), upperbound_logQ=bam_settings('upperbound_logQ'),
                               lowerbound_A0 = 30, upperbound_A0 = 1000000,
                               lowerbound_logn = -4.6, upperbound_logn = -1.5,
                               lowerbound_logQc = 0, upperbound_logQc=10,
                               lowerbound_logWc = 1, upperbound_logWc=8,
                               lowerbound_b = 0.01, upperbound_b = 0.8,
                               lowerbound_logWb = Wb[3,2], upperbound_logWb = Wb[7,2],
                               lowerbound_logDb = Db[3,2], upperbound_logDb = Db[7,2],
                               lowerbound_logr = r[3,2], upperbound_logr = r[7,2],
                               lowerbound_loga = a[3,2], upperbound_loga = a[7,2],
                               sigma_man=0.25, sigma_amhg = 0.22,
                               logQc_hat=bam_settings('logQc_hat'), logWc_hat=bam_settings('logWc_hat'),
                               b_hat=0, logA0_hat = bam_settings('logA0_hat'), logn_hat = -3.5,
                               logWb_hat = 0, logDb_hat = 0, logr_hat = 0, loga_hat = 0,
                               logQ_sd=0.8325546, logQc_sd = 0.8325546, logWc_sd=4.712493, b_sd=0.05, logA0_sd=0.5,
                               logn_sd=0.25, logWb_sd = Wb[2,2], logDb_sd = Db[2,2], logr_sd = r[2,2], loga_sd = a[2,2],
                               Werr_sd=10, Serr_sd=0.00001, dAerr_sd=10,
                               ahg_flag = 0)

allpepsi2=training2

minxs=3
mintime=3

for (phase in allpepsi2){
setwd(phase)
output_directory=paste('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//outputs//test_switch//')
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


for (i in 1:(length(phase_files))) {
  statis <- batchbam(phase_files[i], output_directory, errorflag, minxs, mintime)
  print('river done')
}

output_directory=paste('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//outputs//')

#join and plot
library(readr)
library(reshape2)
library(plyr)
myfiles = list.files(paste(output_directory, "Pepsi1_Bam_defaults/", sep=''), pattern="*.csv", full.names = TRUE)
bam_stats <- ldply(myfiles, read_csv)
bam_stats$river <- phase_files
myfiles = list.files(paste(output_directory, "Pepsi2a_testswitch//", sep=''), pattern="*.csv", full.names = TRUE)
temp <- ldply(myfiles, read_csv)
bam_stats <- merge(bam_stats, temp, by='river')#cbind(bam_stats, temp)

bam_stats <- bam_stats[, -1]
bam_stats <- bam_stats[, -11]

bam_stats$NSE_diff <- bam_stats$NSEswitch - bam_stats$NSE
bam_stats$NRMSE_diff <- bam_stats$NRMSEswitch - bam_stats$NRMSE
bam_stats$RMSE_diff <- bam_stats$RRMSEswitch - bam_stats$RRMSE

write.table(bam_stats, paste(output_directory, 'comparisonstats.txt', sep=''), sep='\t')

plot <- melt(bam_stats)

ggplot(filter(plot, (variable == 'NSE' | variable == 'NSEswitch')& river != 'Tanana.nc'), aes(x=river, y=value, fill=variable)) + 
  geom_bar(position="dodge", stat='identity') +
  scale_color_discrete() +
  theme(axis.text.x = element_text(angle = 90))









#start_time= Sys.time()
#num_cores <- 7 #detectCores() - 4
#cluster= makeCluster(num_cores)
#doParallel::registerDoParallel(cluster)
#clusterExport(cl=cluster, varlist = c("craig_model", "test_priors", "options_manager"))

#bam_metrics=parLapply(cluster,phase_files,batchbam,output_directory)
# bam_format=data.frame(t(simplify2array(bam_metrics)))
# names(bam_format)=c('River','rRMSE','nRMSE','NSE')
# write.csv(bam_format,output_string)
#bam_hydrograph=parLapply(cluster,phase_files, batchbam, output_directory,errorflag,minxs,mintime)

#temp <- data.frame()
#foreach(i = 1:length(phase_files), .combine = 'rbind') %dopar% {
#  temp = batchbam(phase_files[i], output_directory, errorflag, minxs, mintime)
#}

#registerDoSEQ()
#stopCluster(cluster)
#end_time= Sys.time()

#elapsed_time=end_time-start_time
#elapsed_time
