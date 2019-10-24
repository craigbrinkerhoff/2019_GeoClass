training2='C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Tests_Data\\'

setwd(training2)
phase_files= list.files(pattern = "\\.nc$")

output_directory=paste('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//outputs//')

#for reverting big rivers------------------------------------
library(ncdf4)

output <- data.frame()
for (k in 1:length(phase_files)) {
  file <- phase_files[k]
  data_in =nc_open(file)
  
  W_obs = ncvar_get(data_in, 'Reach_Timeseries/W')

  temp <- median(log(W_obs))#summary(lm(regimeS~S_obs))$r.squared
  
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

mark_switch = list.files(paste(output_directory, "mark_switch_old_physics_old_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
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

new_new_varyingN = list.files(paste(output_directory, "new_physics_new_priors_varyingN//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_varyingN <- ldply(new_new_varyingN, read_csv)
new_new_varyingN$river <- phase_files
colnames(new_new_varyingN) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_varyingN$Type <- rep('new_new_varyingN', nrow(new_new_varyingN))
new_new_varyingN$order <- rep(5, nrow(new_new_varyingN))

new_new_varyingReachN = list.files(paste(output_directory, "new_physics_new_priors_varyingReachN//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_varyingReachN <- ldply(new_new_varyingReachN, read_csv)
new_new_varyingReachN$river <- phase_files
colnames(new_new_varyingReachN) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_varyingReachN$Type <- rep('new_new_varyingReachN', nrow(new_new_varyingReachN))
new_new_varyingReachN$order <- rep(5, nrow(new_new_varyingReachN))

new_new_geomorph = list.files(paste(output_directory, "new_new_rivN_geomorph_wideThresh7_5//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_geomorph <- ldply(new_new_geomorph, read_csv)
new_new_geomorph$river <- phase_files
colnames(new_new_geomorph) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_geomorph$Type <- rep('new_new_geomorph', nrow(new_new_geomorph))
new_new_geomorph$order <- rep(5, nrow(new_new_geomorph))

output2 <- filter(output, X5.19577569992289 < 7.5)
smaller_wdths_geo <- filter(new_new_geomorph, Type == 'new_new_geomorph') %>% filter(river %in% output2$river) %>% group_by(Type)
temp <- filter(new_new_regime, Type == 'new_new_regime') %>% filter(!river %in% output2$river)
smaller_wdths_geo <- rbind.fill(smaller_wdths_geo, temp)

#remove too big rivers and replace with their results from non-geomorphic classification
bam_stats <- rbind(mark_switch, old_old_regime, old_new_regime, new_old_regime, new_new_regime, smaller_wdths_geo)
plot <- gather(bam_stats, 'metric', 'value', c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))
plot$Type <- factor(plot$Type, levels = c('Mark_Switch', 'Old_old_regime', 'New_old_regime', 'Old_new_regime', 'New_new_regime', 'new_new_geomorph'))
plot$metric <- factor(plot$metric, levels=c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))

ggplot(plot, aes(x=metric, y = value, fill=Type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-2.3, 1))+
  ggtitle('Metrics For Regime-Switch BAM Variants Across 32 Rivers') +
  scale_fill_brewer(palette="Accent") +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_hline(yintercept=0.5, linetype='dashed') +
  geom_hline(yintercept=1, linetype='dashed')


#ggplot(filter(bam_stats, Type=='New_new_regime' | Type == 'new_new_geomorph'), aes(x=river, y=(NSE), fill=Type)) + 
#  geom_bar(position="dodge", stat='identity') +
#  scale_color_discrete() +
#  coord_cartesian(ylim = c(-1, 1))+
#  theme(axis.text.x = element_text(angle = 90)) +
#  scale_fill_brewer(palette='Dark2')


stats <- bam_stats %>% group_by(Type) %>% summarise(median = median(NSE))

new_new_geomorph$diffNSE <- new_new_geomorph$NSE - new_new_regime$NSE
new_new_geomorph <- merge(new_new_geomorph, output, by='river')
