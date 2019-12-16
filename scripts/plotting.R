training2='C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Tests_Data\\'

setwd(training2)
phase_files= list.files(pattern = "\\.nc$")

output_directory=paste('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//outputs//final//')

#for reverting big rivers------------------------------------
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

#---------------------------------------------------------------------------------------------------
#join and plot
library(readr)
library(reshape2)
library(plyr)
library(tidyr)
library(cowplot)
library(RColorBrewer)
library(forcats)
library(dplyr)

mark_switch = list.files(paste(output_directory, "old_physics_old_priors_mark_switch//", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
mark_switch <- ldply(mark_switch, read_csv)
mark_switch$river <- phase_files
colnames(mark_switch) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
mark_switch$Type <- rep('Mark_Switch', nrow(mark_switch))
mark_switch$order <- rep(1, nrow(mark_switch))

old_old_regime = list.files(paste(output_directory, "old_physics_old_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
old_old_regime <- ldply(old_old_regime, read_csv)
old_old_regime$river <- phase_files
colnames(old_old_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
old_old_regime$Type <- rep('Old_old_regime', nrow(old_old_regime))
old_old_regime$order <- rep(2, nrow(old_old_regime))

old_new_regime = list.files(paste(output_directory, "old_physics_new_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
old_new_regime <- ldply(old_new_regime, read_csv)
old_new_regime$river <- phase_files
colnames(old_new_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
old_new_regime$Type <- rep('Old_new_regime', nrow(old_new_regime))
old_new_regime$order <- rep(3, nrow(old_new_regime))

new_old_regime = list.files(paste(output_directory, "new_physics_old_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_old_regime <- ldply(new_old_regime, read_csv)
new_old_regime$river <- phase_files
colnames(new_old_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_old_regime$Type <- rep('New_old_regime', nrow(new_old_regime))
new_old_regime$order <- rep(4, nrow(new_old_regime))

new_new_regime = list.files(paste(output_directory, "new_physics_new_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_regime <- ldply(new_new_regime, read_csv)
new_new_regime$river <- phase_files
colnames(new_new_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_regime$Type <- rep('New_new_regime', nrow(new_new_regime))
new_new_regime$order <- rep(5, nrow(new_new_regime))

new_new_KMeans = list.files(paste(output_directory, "new_new_reachN_KMeansClass_globalFunc_over_6_5_all_priors_wide_var//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_KMeans <- ldply(new_new_KMeans, read_csv)
new_new_KMeans$river <- phase_files
colnames(new_new_KMeans) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_KMeans$Type <- rep('new_new_KMeans', nrow(new_new_KMeans))
new_new_KMeans$order <- rep(5, nrow(new_new_KMeans))

new_new_geomorph = list.files(paste(output_directory, "new_new_reachN_rivClass_globalFunc_over_6_5_all_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_geomorph <- ldply(new_new_geomorph, read_csv)
new_new_geomorph$river <- phase_files
colnames(new_new_geomorph) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_geomorph$Type <- rep('new_new_Geomorph', nrow(new_new_geomorph))
new_new_geomorph$order <- rep(5, nrow(new_new_geomorph))

new_new_dbscan = list.files(paste(output_directory, "new_new_reachN_DBSCAN_20_Class_all_priors_regression_fin//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_dbscan <- ldply(new_new_dbscan, read_csv)
new_new_dbscan$river <- phase_files
colnames(new_new_dbscan) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_dbscan$Type <- rep('DBSCAN', nrow(new_new_dbscan))
new_new_dbscan$order <- rep(5, nrow(new_new_dbscan))

bam_stats <- rbind(mark_switch, old_new_regime, new_old_regime, new_new_regime, new_new_KMeans, new_new_geomorph, new_new_dbscan)
plot <- gather(bam_stats, 'metric', 'value', c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))
plot$Type <- factor(plot$Type, levels = c('Mark_Switch', 'New_old_regime', 'Old_new_regime', 'New_new_regime', 'new_new_KMeans', 'new_new_Geomorph', 'DBSCAN'))
plot$metric <- factor(plot$metric, levels=c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))

`%notin%` <- Negate(`%in%`)
#rivs <- c('StLawrenceDownstream.nc', 'StLawrenceUpstream.nc', 'MississippiDownstream.nc', 'OhioSection8.nc', 'OhioSection7.nc', 'Wabash.nc', 'MississippiUpstream.nc')  #poorly fit rivers for dbscan
#plot <- filter(plot, river %notin% rivs)

stats <- group_by(plot, Type, metric) %>% dplyr::summarize(mean = mean(value), median = median(value), IQR = IQR(value))

boxplots <- ggplot(plot, aes(x=metric, y = value)) +
  geom_boxplot(aes(fill=Type)) + 
  coord_cartesian(ylim = c(-1.25, 1.25))+
  #ggtitle('Performance Metrics For BAM Variants Across 32 Test Rivers') +
  #geom_hline(yintercept=0, linetype='dashed') +
  #geom_hline(yintercept=0.5, linetype='dashed') +
  #geom_hline(yintercept=1, linetype='dashed') +
  ylab('') +
  xlab('%') +
  theme(legend.position=c(0.04, 0.27)) +
  scale_fill_brewer(name = "", labels = c("Default Priors", "New Physics, Old Data", 'Old Physics, New Data', 'New Physics, New Data', 'Unsupervised: KMeans', 'Supervised', 'Unsupervised: DBSCAN'), palette = 'Dark2')
boxplots
#ggsave('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\boxplots_current_no_dash.tiff', boxplots, dpi = 400, width = 6.5, height = 6.5)
#ggsave('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\boxplots_current1.pdf', boxplots, dpi = 400, width = 6.5, height = 6.5)

#hydrographs
library(bamr)
obsQ <- read.csv('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final\\new_new_reachN_DBSCAN_20_Class_all_priors_regression_fin\\hydrographs\\obsQ_Seine.csv', header=TRUE, sep=",")
predQ <- read.csv('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final\\new_new_reachN_DBSCAN_20_Class_all_priors_regression_fin\\hydrographs\\predQ_Craig_Seine.csv', header=TRUE, sep=",")
predQ2 <- read.csv('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final\\old_physics_old_priors_mark_switch\\hydrographs\\predQ_Craig_Seine.csv', header=TRUE, sep=",")
#predQ3 <- read.csv('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final\\new_physics_new_priors\\hydrographs\\predQ_Craig_OhioSection2.csv', header=TRUE, sep=",")

predQ <- predQ$mean
predQ2 <- predQ2$mean
#predQ3 <- predQ3$mean

hydrograph <- cbind(predQ, obsQ, predQ2)
colnames(hydrograph) <- c('Unsupervised: DBSCAN', 'Time', 'Observed Discharge', 'Default Priors')
hydrograph <- gather(hydrograph, 'metric', 'value', c('Observed Discharge', 'Unsupervised: DBSCAN', 'Default Priors'))

#meanQ = mean(obsQ$x)

hydroplot <- ggplot(hydrograph, aes(Time, value, color = metric))+
  geom_line(size=1.2, aes(linetype = metric)) +
  ylab("Discharge (cms)") +
  xlab('Timestep') +
  theme(legend.position=c(0.65, 0.90))  +
  scale_color_manual(name="Seine River", values = c('#1b9e77', 'black', '#A6761D')) +
  scale_linetype_manual(name="Seine River" , values=c('solid', 'dotted', 'solid')) 
  #geom_line(aes(y=meanQ), size=1)
hydroplot
ggsave('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\Seine_hydrograph.tiff', hydroplot, dpi = 400, width = 10, height = 6)

new_new_dbscan$diff <- new_new_dbscan$NSE - mark_switch$NSE
new_new_dbscan$improvement <- ifelse(new_new_dbscan$diff > 0.1, "significant",
                                     ifelse(new_new_dbscan$diff > -0.1, "Similar", 'significant worse'))

#violins <- ggplot(plot, aes(x=metric, y = value)) +
#  geom_violin(aes(fill=Type), scale = 'width', trim = TRUE) + 
#  geom_boxplot(aes(group=interaction(Type,metric)), fill='azure', width = 0.20, outlier.colour=NA, position = position_dodge(width = 0.9), lwd=0.75, fatten = 3) +
#  coord_cartesian(ylim = c(-2.3, 1.5))+
#  ggtitle('Performance Metrics For BAM Variants Across 32 Rivers') +
#  scale_fill_brewer(palette="Dark2") +
#  geom_hline(yintercept=0, linetype='dashed') +
#  geom_hline(yintercept=0.5, linetype='dashed') +
#  geom_hline(yintercept=1, linetype='dashed')