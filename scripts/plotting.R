#---------------------------------------------------------------------------------------------------
training2='C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Tests_Data\\'

setwd(training2)
phase_files= list.files(pattern = "\\.nc$")

output_directory=paste('C://Users//cbrinkerhoff//Box Sync//Ongoing Projects//geomorph_class//outputs//')

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
colnames(mark_switch) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'river')
mark_switch$Type <- rep('Mark_Switch', nrow(mark_switch))
mark_switch$order <- rep(1, nrow(mark_switch))

old_old_regime = list.files(paste(output_directory, "regime_switch_old_physics_old_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
old_old_regime <- ldply(old_old_regime, read_csv)
old_old_regime$river <- phase_files
colnames(old_old_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'river')
old_old_regime$Type <- rep('Old_old_regime', nrow(old_old_regime))
old_old_regime$order <- rep(2, nrow(old_old_regime))

old_new_regime = list.files(paste(output_directory, "old_physics_new_priors_correctN_correctSD//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
old_new_regime <- ldply(old_new_regime, read_csv)
old_new_regime = old_new_regime[,-5]
old_new_regime$river <- phase_files
colnames(old_new_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'river')
old_new_regime$Type <- rep('Old_new_regime', nrow(old_new_regime))
old_new_regime$order <- rep(3, nrow(old_new_regime))

new_old_regime = list.files(paste(output_directory, "new_physics_new_priors_correctSD//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_old_regime <- ldply(new_old_regime, read_csv)
new_old_regime = new_old_regime[,-5]
new_old_regime$river <- phase_files
colnames(new_old_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'river')
new_old_regime$Type <- rep('New_old_regime', nrow(new_old_regime))
new_old_regime$order <- rep(4, nrow(new_old_regime))

new_new_regime = list.files(paste(output_directory, "new_physics_new_priors_correctN_correctSD//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_regime <- ldply(new_new_regime, read_csv)
new_new_regime = new_new_regime[,-5]
new_new_regime$river <- phase_files
colnames(new_new_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'river')
new_new_regime$Type <- rep('New_new_regime', nrow(new_new_regime))
new_new_regime$order <- rep(5, nrow(new_new_regime))

bam_stats <- rbind(mark_switch, old_old_regime, old_new_regime, new_old_regime, new_new_regime)
plot <- gather(bam_stats, 'metric', 'value', c('RRMSE', 'NRMSE', 'NSE'))
plot$Type <- factor(plot$Type, levels = c('Mark_Switch', 'Old_old_regime', 'New_old_regime', 'Old_new_regime', 'New_new_regime'))
plot$metric <- factor(plot$metric, levels=c('RRMSE', 'NRMSE', 'NSE'))

ggplot(plot, aes(x=metric, y = value, fill=Type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1))+
  ggtitle('Metrics For Regime-Switch BAM Variants Across 32 Rivers') +
  scale_fill_brewer(palette="Dark2")


ggplot(filter(plot, metric == 'NRMSE'), aes(x=river, y=(value), fill=Type)) + 
  geom_bar(position="dodge", stat='identity') +
  scale_color_discrete() +
 # coord_cartesian(ylim = c(-1, 1))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette='Dark2')


stats <- bam_stats %>% group_by(Type) %>% summarise(median = median(NSE))
