#Generates Figure S4 and tests normality of the resulting distribution

library(ggplot2)
library(dplyr)
library(cowplot)
theme_set(theme_cowplot())

#read in netcdfs------------------
setwd('C:\\Users\\craig\\Box Sync\\Datasets\\Pepsi1\\')
rivers= list.files(pattern = "\\.nc$")

#get widths--------------------------------
output <- data.frame('river'=NA, 'SD'=0)
for (file in rivers) {
  
  #Prepare netcdfs and river widths using filtering used in SWOT tests
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
  
  has_obs_Q <- which(colSums(is.na(Q_obs))==0)
  Q_obs = Q_obs[,has_obs_Q]
  W_obs = W_obs[,has_obs_Q]
  S_obs = S_obs[,has_obs_Q]
  H_obs = H_obs[,has_obs_Q]
  
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
  
  SD_w <- sd(log(W_obs))
  temp <- c(file, SD_w)
  output <- rbind(output, temp)
}

output <- output[-1,]
output$SD <- as.numeric(output$SD)


#plot results------------------
plot <- ggplot(output, aes(x=SD)) +
  geom_histogram(fill='#756bb1', color='black', size=1.25) +
  geom_vline(xintercept = 0.45, linetype='dashed', size=1.25) +
  ylab('Count') +
  xlab('Standard Deviation of log-River Width') +
  theme(axis.text=element_text(size=18),
       axis.title=element_text(size=20))
plot

ggsave('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\updates_review1\\sd_swot_rivers.png', plot, width = 10, height = 10)


#Shapiro-Wilk Test for normality-------------------------
#NOTE: null hyp is normal distribution. Thus if test is significant, it is non-normal.

#including all 19 rivers
shapiro.test(output$SD) #not normal

#Just rivers below our threshold
below_thresh <- filter(output, SD < 0.45)
shapiro.test(below_thresh$SD) #not normal
