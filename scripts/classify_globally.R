library(ggplot2)
library(cowplot)
library(dplyr)

merit <- read.table('C:\\Users\\cbrinkerhoff\\Box Sync\\MERIT_Hydro\\property_pfaf_07_riv_3sMERIT.csv', sep=',', header = TRUE)
widthsClass <- read.table('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\WidthsClass.csv', sep=',', header=TRUE)
logisticInts <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//logisticInts.csv', header=FALSE)
logisticCoefs <- read.csv('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//logisticCoefs.csv', header=FALSE)

maxWidth = 6.5 #from training data, approximately 1.5*IQR + median for class 8, so that abything considered an extreme value for class 8 is 'big river'
classes <- widthsClass[,7] #median width of each river type

classify_geomorph <- function(x){
  width <- log(x)
  if(width > maxWidth) {
    return(100)
  }
  else if (is.finite(width) == 0) {
    return(0)
  }
  else {
    return(which.min(abs(classes-width)))
  }
}

classify_kmeans_func <- function(x, y) {
  width <- log(x)
  slope <- log(y)
  maxWidth <- 6.5
  
  if (is.finite(width) == 0) {return(0)}
  
  class1Probs <- 1/(1+exp(-(logisticInts[1,]+logisticCoefs[1,1]*width+logisticCoefs[1,2]*slope)))
  class2Probs <- 1/(1+exp(-(logisticInts[2,]+logisticCoefs[2,1]*width+logisticCoefs[2,2]*slope)))
  class3Probs <- 1/(1+exp(-(logisticInts[3,]+logisticCoefs[3,1]*width+logisticCoefs[3,2]*slope)))
  class4Probs <- 1/(1+exp(-(logisticInts[4,]+logisticCoefs[4,1]*width+logisticCoefs[4,2]*slope)))
  class5Probs <- 1/(1+exp(-(logisticInts[5,]+logisticCoefs[5,1]*width+logisticCoefs[5,2]*slope)))
  class6Probs <- 1/(1+exp(-(logisticInts[6,]+logisticCoefs[6,1]*width+logisticCoefs[6,2]*slope)))
  class7Probs <- 1/(1+exp(-(logisticInts[7,]+logisticCoefs[7,1]*width+logisticCoefs[7,2]*slope)))
  class8Probs <- 1/(1+exp(-(logisticInts[8,]+logisticCoefs[8,1]*width+logisticCoefs[8,2]*slope)))

  classProbs <- c(class1Probs, class2Probs, class3Probs, class4Probs, class5Probs, class6Probs, class7Probs, class8Probs)
  output <- ifelse(width > maxWidth, 100, which.max(classProbs))
  return(output)
}

merit$geomorph_class <- apply(merit[,6, drop=F], 1, classify_geomorph)
merit$kmeans_class <- mapply(classify_kmeans_func, merit$w, merit$slope)

write.csv(merit, 'C:\\Users\\cbrinkerhoff\\Box Sync\\MERIT_Hydro\\MERIT_classified.csv', sep=',')

geomorphSize <- group_by(merit, geomorph_class) %>% summarise(size = n())
kmeansSize <- group_by(merit, kmeans_class) %>% summarise(size = n())

ggplot(merit, aes(x=factor(geomorph_class), y = log10(w))) +
  geom_boxplot(fill='deepskyblue4') +
  ggtitle('North American Rivers') +
  ylab('Log10 Mean River Width [m]') +
  xlab('Geomorphic Class')
