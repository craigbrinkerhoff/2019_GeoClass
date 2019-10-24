training2='C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//Tests_Data//'
setwd(training2)
phase_files= list.files(pattern = "\\.nc$")

#for reverting big rivers------------------------------------
library(ncdf4)

classify_func <- function(x) {
  width <- median(log(x))
  maxWidth = 7.25 #from training data, approximately 1.5*IQR + median for class 8, so that abything considered an extreme value for class 8 is 'big river'
  classes <- widthsClass[,7] #median width of each river type
  output <- ifelse(width > maxWidth, 100, which.min(abs(classes-width)))
  
  return(output)
}
widthsClass <- read.csv('C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//WidthsClass.csv')
extrapolateClassModels <- read.csv('C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//extrapolateClassModels.csv')
r <- read.csv('C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//priorsRClass.csv')

output <- data.frame()
for (i in 1:length(phase_files)){
  file <- phase_files[i]
  data_in =nc_open(file)
  
  W_obs = ncvar_get(data_in, 'Reach_Timeseries/W')
  class <- apply(W_obs, 1, classify_func)
  
  logr_hat <- class
  logr_upper <- class
  logr_lower <- class
  logr_sd <- class
  reachNum <- class
  
  for(k in 1:nrow(W_obs)){
#big rivers
if (class[k] == 100) {
  width = mean(log(W_obs))
  
  logr_hat[k] =  extrapolateClassModels[1,2]+extrapolateClassModels[1,3]*width
  logr_sd[k] = extrapolateClassModels[7,2]+extrapolateClassModels[7,3]*width
  logr_lower[k] = extrapolateClassModels[13,2]+extrapolateClassModels[13,3]*width
  logr_upper[k] = extrapolateClassModels[19,2]+extrapolateClassModels[19,3]*width
}

#classified rivers
if (class[k] != 100) {
  logr_hat[k] = r[class[k], 7]
  logr_upper[k] <- r[class[k],9]
  logr_lower[k] <- r[class[k],5]
  logr_sd[k] <- r[class[k],4]
}

#braided rivers
if (file == 'Jamuna.nc' | file == 'Brahmaputra.nc') {
  class[k] <- 200
  logr_hat[k] <- -0.249
  logr_upper[k] <- 0
  logr_lower[k] <- -2.58
  logr_sd[k] <- 0.412
}
    reachNum[k] <- k
    
  }
  
  class <- cbind(reachNum, class, logr_lower, logr_upper, logr_hat, logr_sd, file)
  output <- rbind(output, class)
}

setwd('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//Kostas_rivclass//')

write.csv(output, file = "pepsi2_r_classified.csv")