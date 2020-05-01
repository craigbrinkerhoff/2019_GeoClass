#Creator: Craig Brinkerhoff
#Description: Create figures for geoBAM paper

library(readr)
library(reshape2)
library(plyr)
library(tidyr)
library(cowplot)
library(RColorBrewer)
library(forcats)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(truncnorm)
library(tmap)
library(sf)
library(raster)
library(ncdf4)
library(ncdf4.helpers)
library(hydroGOF)
theme_set(theme_cowplot())

training2='C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Tests_Data\\'
setwd(training2)
phase_files= list.files(pattern = "\\.nc$")

output_directory=paste('C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//outputs//final_results//')

scaleFUN <- function(x) sprintf("%.2f", x)
scaleFUN3 <- function(x) sprintf("%.1f", x)

#SWOT Hydrographs figure-----------------------------------------------------------------------------------
files <- c('Connecticut.csv', 'Cumberland.csv', 'Ganges.csv', 'GaronneDownstream.csv', 'GaronneUpstream.csv',
           'Kanawha.csv', 'MississippiDownstream.csv', 'MississippiUpstream.csv',
           'Ohio.csv', 'Platte.csv', 'Po.csv', 'SacramentoDownstream.csv', 'SacramentoUpstream.csv',
           'Seine.csv', 'Severn.csv', 'StLawrenceDownstream.csv', 'StLawrenceUpstream.csv', 'Tanana.csv',
           'Wabash.csv')

pltList <- list()

for (i in 1:19){
  plot <- data.frame()
  file = files[i]
  river <- substr(file,1,nchar(file)-4)
  predQ <- read.csv(paste(output_directory, 'swot\\BAM_control\\hydrographs\\predQ_Craig_', file, sep=''), header=TRUE, sep=",")
  predQ <- predQ$mean
  
  predQsupervised <- read.csv(paste(output_directory, 'swot\\geoBAM_expert\\hydrographs\\predQ_Craig_', file, sep=''), header=TRUE, sep=",")
  predQsupervised <- predQsupervised$mean
  
  obsQ <- read.csv(paste(output_directory, 'swot\\BAM_control\\hydrographs\\observed\\obsQ_', file, sep=''), header=TRUE, sep=",")
  
  #normalize by max time
  maxTime <- max(obsQ$X)
  obsQ$X <- obsQ$X/maxTime
  
  plot <- data.frame(predQ, predQsupervised, obsQ)
  colnames(plot) <- c('Control BAM', 'geoBAM-Expert', 'Time', 'Observed Discharge')
  plot <- gather(plot, 'metric', 'value', c('Observed Discharge', 'Control BAM', 'geoBAM-Expert'))

  
  pltName <- paste('hydrograph_', file, sep='')
  
  pltList[[ pltName ]]  <- ggplot(plot, aes(Time, value, color = metric))+
    geom_point(data = filter(plot, metric!='Observed Discharge'), size=1.5) +
    geom_line(data = filter(plot, metric=='Observed Discharge'), size=1.2, linetype='dotted')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
    scale_color_manual(name="", values = c( '#6baed6', '#08519c','black')) +
    scale_y_continuous(labels = comma) +
    ggtitle(river)
  }

plotgrid <- plot_grid(pltList$hydrograph_Connecticut.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Cumberland.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Ganges.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_GaronneDownstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_GaronneUpstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Kanawha.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_MississippiDownstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_MississippiUpstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Ohio.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Platte.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Po.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_SacramentoDownstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_SacramentoUpstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Seine.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_Severn.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_StLawrenceDownstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList$hydrograph_StLawrenceUpstream.csv + theme(legend.position='none'),
                      pltList$hydrograph_Tanana.csv + theme(legend.position='none'),
                      pltList$hydrograph_Wabash.csv + theme(legend.position='none'),
                      ncol=3)

legend <- get_legend(
  # create some space to the left of the legend
  pltList$hydrograph_GaronneUpstream.csv + theme(legend.text=element_text(size=17))
)

plotHydrographs <- plotgrid + draw_grob(legend, 0.7, -0.42, 0.25, 1)

yTitleCombo <- textGrob(expression(Discharge~(cms)), gp=gpar(fontface="bold", col="black", fontsize=18), rot=90)
xTitleCombo <- textGrob(expression(Timestep/Max~Timestep), gp=gpar(fontface="bold", col="black", fontsize=18))

plotHydrographs <- grid.arrange(arrangeGrob(plotHydrographs, left = yTitleCombo, bottom = xTitleCombo))
ggsave('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\hydrographs_swot.png', plotHydrographs, width = 10, height = 11)




#Mackenzie 4-panel plot for NSE--------------------------------------------------------------------------------------------
mackenzie_geoBAM_expert <- read.csv("C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\mackenzie\\geoBAMr_expert_vic_hrr.csv", header = TRUE)
mackenzie_geoBAM_expert$Date <- as.Date(mackenzie_geoBAM_expert$Date, '%m/%d/%Y')

mackenzie_BAM <- read.csv("C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\mackenzie\\bam_vic_hrr.csv", header = TRUE)
mackenzie_BAM <- mackenzie_BAM[,-1]
mackenzie_BAM$Date <- as.Date(mackenzie_BAM$Date, '%m/%d/%Y')

mackenzie_geoBAM_unsupervised <- read.csv("C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\mackenzie\\geoBAMr_unsupervised_vic_hrr.csv", header = TRUE)
mackenzie_geoBAM_unsupervised$Date <- as.Date(mackenzie_geoBAM_unsupervised$Date, '%m/%d/%Y')

mackenzie_obs <- read.table("C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\mackenzie\\gauge_cal.txt", header = TRUE, check.names = FALSE)
mackenzie_obs$Date <- seq(as.Date("1984/1/1"), as.Date("2013/12/31"), "days")
mackenzie_obs <- gather(mackenzie_obs, key=key, value=value, c(-Date))
colnames(mackenzie_obs) <- c('Date', 'reach', 'discharge')
mackenzie_obs <- filter(mackenzie_obs, discharge >= 0) #filter out when gauges aren't measured

mackenzie <- merge(mackenzie_obs, mackenzie_geoBAM_expert, by=c("Date","reach")) # NA's match  #full_join(mackenzie_obs, mackenzie_geoBAM_expert, by=c('Date', 'reach'))
mackenzie <- merge(mackenzie, mackenzie_geoBAM_unsupervised, by=c("Date","reach"))
mackenzie <- merge(mackenzie, mackenzie_BAM, by=c("Date","reach"))

colnames(mackenzie) <- c('Date', 'reach', 'obs_Q', 'geoBAM_expert_mean', 'geoBAM_expert_sd','geoBAM_unsupervised_mean', 'geoBAM_unsupervised_sd', 'BAM_mean', 'BAM_sd') #'geoBAM_old_mean', 'geoBAM_old_sd',

stats <- group_by(mackenzie, reach) %>% 
  summarise(NSE_expert = NSE(geoBAM_expert_mean, obs_Q, na.rm=TRUE), 
           # NSE_oldgeoBAM = NSE(geoBAM_old_mean, obs_Q, na.rm=TRUE), 
            NSE_unsupervised = NSE(geoBAM_unsupervised_mean, obs_Q, na.rm=TRUE), 
            NSE_BAM = NSE(BAM_mean, obs_Q, na.rm=TRUE),
            n = n())

plot <- gather(stats, key = key, value=value, c('NSE_expert', 'NSE_unsupervised', 'NSE_BAM')) #, 'NSE_oldgeoBAM'

mackenzie_improvement <- data.frame('increaseNSE' = (stats$NSE_unsupervised - stats$NSE_BAM))

not_plottedNSE <- plot %>% group_by(key) %>%
  filter(value < -2.3) %>%
  summarise(num_outliers = n())

mackenzie_boxplots <- ggplot(plot, aes(x=key, y=value)) + 
  geom_boxplot(aes(fill=key), size=0.8) + 
  coord_cartesian(ylim=c(-2.3,1))+
  geom_text(data=not_plottedNSE,aes(y=-2.4,label=num_outliers), size=7,vjust=0,hjust=1.5) +
  ylab("NSE") + 
  xlab('') +
  theme(axis.ticks.x = element_blank())+
  scale_fill_manual(name = "", 
                    labels = c("Control BAM", 'geoBAM-Expert', 'geoBAM-Unsupervised'), 
                    values=c('#c7e9c0', '#74c476', '#006d2c')) +
  scale_x_discrete(name='',
                   labels=c('Control BAM', 'geoBAM-Expert', 'geoBAM-Unsupervised'))

mackenzie_CDFs <- ggplot(plot, aes(value)) + 
  stat_ecdf(aes(colour=key), size=1.5) +
  coord_cartesian(xlim=c(-2.5,1))+
  geom_segment(aes(x=-2.1, y=0.25, xend=-2.5, yend=0.25), arrow = arrow(), size=1) +
  ylab('') + 
  xlab('NSE') +
  scale_color_manual(name = "", 
                    labels = c("Control BAM", 'geoBAM-Expert','geoBAM_old', 'geoBAM-Unsupervised'), 
                    values=c('#c7e9c0', '#74c476', '#006d2c'))

mackenzie_PDF_improvement <- ggplot(mackenzie_improvement, aes(x=increaseNSE)) +
  geom_histogram(alpha = 0.5, fill='#7570B3', color='black', size=1, bins=30) + #(alpha=0.1, fill='#7570B3', color='#7570B3', size=0.8) +
  geom_vline(xintercept = 0, linetype='dashed', size=1.25) +
  scale_y_continuous(labels=scaleFUN)+
  xlab('Change in NSE') +
  ylab('') 

gridMackenzie <- plot_grid(mackenzie_CDFs + theme(legend.position = 'none'),
                           mackenzie_boxplots + theme(legend.position = 'none'),
                           mackenzie_PDF_improvement + theme(legend.position = 'none'),
                           labels = c('A', 'B', 'C'))

legend <- get_legend(mackenzie_boxplots + 
                       theme(legend.title = element_blank(), 
                             legend.text = element_text(size = 25)) +
                       guides(shape = guide_legend(override.aes = list(size = 25))))

plotMackenzie <- plot_grid(gridMackenzie)
plotMackenzie <- plotMackenzie + draw_grob(legend, 0.65, -0.3, 0.25, 1)
plotMackenzie
save_plot("C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\mackenzie.png", plotMackenzie, ncol=2, base_height = 8.5, base_width = 5.5)


#4-panel plot for SWOT--------------------------------------------------------------------------
output_directory=paste('C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//outputs//final_results//')

BAM_control = list.files(paste(output_directory, "swot\\BAM_control\\", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
BAM_control <- ldply(BAM_control, read_csv)
BAM_control$river <- files
colnames(BAM_control) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
BAM_control$Type <- rep('BAM_control', nrow(BAM_control))
BAM_control$order <- rep(1, nrow(BAM_control))

geoBAM_expert = list.files(paste(output_directory, "swot\\geoBAM_expert\\", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
geoBAM_expert <- ldply(geoBAM_expert, read_csv)
geoBAM_expert$river <- files
colnames(geoBAM_expert) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'AMHGflag', 'rivClass_1', 'rivClass_2',
                             'rivClass_3', 'rivClass_4', 'rivClass_5', 'rivClass_6', 'rivClass_7', 'rivClass_8',
                             'rivClass_9', 'rivClass_10', 'rivClass_11', 'rivClass_12', 'rivClass_13', 'rivClass_14',
                             'rivClass_15', 'rivClass_16', 'river')
geoBAM_expert$Type <- rep('geoBAM_expert', nrow(geoBAM_expert))
geoBAM_expert$order <- rep(1, nrow(geoBAM_expert))

geoBAM_unsupervised = list.files(paste(output_directory, "swot\\geoBAM_unsupervised\\", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
geoBAM_unsupervised <- ldply(geoBAM_unsupervised, read_csv)
geoBAM_unsupervised$river <- files
colnames(geoBAM_unsupervised) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'AMHGflag', 'rivClass_1', 'rivClass_2',
                             'rivClass_3', 'rivClass_4', 'rivClass_5', 'rivClass_6', 'rivClass_7', 'rivClass_8',
                             'rivClass_9', 'rivClass_10', 'rivClass_11', 'rivClass_12', 'rivClass_13', 'rivClass_14',
                             'rivClass_15', 'rivClass_16', 'river')
geoBAM_unsupervised$Type <- rep('geoBAM_unsupervised', nrow(geoBAM_unsupervised))
geoBAM_unsupervised$order <- rep(1, nrow(geoBAM_unsupervised))

plot <- data.frame('geoBAM_expert' = geoBAM_expert$NSE, 'BAM' = BAM_control$NSE, 'geoBAM_unsupervised' = geoBAM_unsupervised$NSE, 'river' = geoBAM_expert$river)

SWOT_improvement <- data.frame('increaseNSE' = (plot$geoBAM_unsupervised - plot$BAM))
SWOT <- gather(plot, 'key', 'value', c('geoBAM_expert', 'geoBAM_unsupervised', 'BAM'))

not_plottedNSE <- SWOT %>% group_by(key) %>%
  filter(value < -2.3) %>%
  summarise(num_outliers = n())

SWOT_boxplots <- ggplot(SWOT, aes(x=key, y=value)) + 
  geom_boxplot(aes(fill=key), size=0.8) + 
  coord_cartesian(ylim=c(-2.3,1))+
  geom_text(data=not_plottedNSE,aes(y=-2.4,label=num_outliers), size=7,vjust=0,hjust=0.7) +
  ylab("NSE") + 
  theme(axis.ticks.x = element_blank()) +
  scale_fill_manual(name = "", 
                    labels = c("Control BAM", 'geoBAM-Expert', 'geoBAM-Unsupervised'), 
                    values=c('#c6dbef', '#6baed6', '#08519c')) +
  scale_x_discrete(name ="", 
                   labels=c('Control BAM', 'geoBAM-Expert', 'geoBAM-Unsupervised'))

SWOT_CDFs <- ggplot(SWOT, aes(value)) + 
  stat_ecdf(aes(colour=key), size=1.5) +
  coord_cartesian(xlim=c(-2.5,1))+
  geom_segment(aes(x=-2.1, y=0.25, xend=-2.5, yend=0.25), arrow = arrow(), size=1) +
  ylab('') + 
  xlab('NSE') +
  scale_color_manual(name = "", 
                    labels = c("Control BAM", 'geoBAM-Expert'), 
                    values=c('#c6dbef', '#6baed6', '#08519c'))

SWOT_PDF_improvement <- ggplot(SWOT_improvement, aes(x=increaseNSE)) +
  geom_histogram(alpha = 0.5, fill='#7570B3', color='black', size=1, bins=30) +
  geom_vline(xintercept = 0, linetype='dashed', size=1) +
  scale_y_continuous(labels=scaleFUN)+
  xlab('Change in NSE') +
  ylab('')

gridSWOT <- plot_grid(SWOT_CDFs + theme(legend.position = 'none'),
                           SWOT_boxplots + theme(legend.position = 'none'),
                           SWOT_PDF_improvement + theme(legend.position = 'none'),
                           labels = c('A', 'B', 'C'))

legend <- get_legend(SWOT_boxplots + 
                       theme(legend.title = element_blank(), 
                             legend.text = element_text(size = 25)) +
                       guides(shape = guide_legend(override.aes = list(size = 25))))

plotSWOT <- plot_grid(gridSWOT)
plotSWOT <- plotSWOT + draw_grob(legend, 0.65, -0.3, 0.25, 1)
plotSWOT
save_plot("C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\swot.png", plotSWOT, ncol=2, base_height = 8.5, base_width = 5.5)




#Parametric distributions figure-----------------------------------------------------------------
training2='C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\priors\\'
setwd(training2)

#Palettes
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(17)
mycolors <- c(mycolors, 'black')
mycolors2 <- brewer.pal(7, "Spectral")
mycolors2 <- c(mycolors2, 'black')

#A0
A0parameters <- read.csv('priorsA0_geoBAMexpert.csv')
A0parameters <- A0parameters[order(A0parameters$clusterGeomorphIndex),]
globalA0 <- read.csv('priorsA0.csv')
A0parametersDBSCAN <- read.csv('priorsA0_geoBAMunsupervised.csv')
A0parametersDBSCAN <- A0parametersDBSCAN[2:8,]

A0_expert <- ggplot(data.frame(logA0 = c(globalA0[3,2], globalA0[7,2])), aes(x = logA0)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(A0parameters[1,5], A0parameters[1,9], A0parameters[1,7], A0parameters[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[2,5], b = A0parameters[2,9],A0parameters[2,7], A0parameters[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[3,5], b = A0parameters[3,9],A0parameters[3,7], A0parameters[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[4,5], b = A0parameters[4,9],A0parameters[4,7], A0parameters[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[5,5], b = A0parameters[5,9],A0parameters[5,7], A0parameters[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[6,5], b = A0parameters[6,9],A0parameters[6,7], A0parameters[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[7,5], b = A0parameters[7,9],A0parameters[7,7], A0parameters[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[8,5], b = A0parameters[8,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "8"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[9,5], b = A0parameters[10,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "9"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[10,5], b = A0parameters[10,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "10"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[11,5], b = A0parameters[11,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "11"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[12,5], b = A0parameters[12,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "12"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[13,5], b = A0parameters[13,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "13"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[14,5], b = A0parameters[14,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "14"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[15,5], b = A0parameters[15,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "15"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parameters[16,5], b = A0parameters[16,9], mean = A0parameters[8,7], sd = A0parameters[8,4]),
                aes(colour = "16"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalA0[3,2], globalA0[7,2], globalA0[5,2], globalA0[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors, breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'Global')) +
  theme(legend.title = element_blank())

A0_DBSCAN <- ggplot(data.frame(logA0 = c(globalA0[3,2], globalA0[7,2])), aes(x = logA0)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(A0parametersDBSCAN[1,5], A0parametersDBSCAN[1,9], A0parametersDBSCAN[1,7], A0parametersDBSCAN[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parametersDBSCAN[2,5], b = A0parametersDBSCAN[2,9],A0parametersDBSCAN[2,7], A0parametersDBSCAN[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parametersDBSCAN[3,5], b = A0parametersDBSCAN[3,9],A0parametersDBSCAN[3,7], A0parametersDBSCAN[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parametersDBSCAN[4,5], b = A0parametersDBSCAN[4,9],A0parametersDBSCAN[4,7], A0parametersDBSCAN[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parametersDBSCAN[5,5], b = A0parametersDBSCAN[5,9],A0parametersDBSCAN[5,7], A0parametersDBSCAN[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parametersDBSCAN[6,5], b = A0parametersDBSCAN[6,9],A0parametersDBSCAN[6,7], A0parametersDBSCAN[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = A0parametersDBSCAN[7,5], b = A0parametersDBSCAN[7,9],A0parametersDBSCAN[7,7], A0parametersDBSCAN[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalA0[3,2], globalA0[7,2], globalA0[5,2], globalA0[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors2, breaks = c('1', '2', '3', '4', '5', '6', '7', 'Global')) +
  theme(legend.title = element_blank())

#r
rparameters <- read.csv('priorsr_geoBAMexpert.csv')
rparameters <- rparameters[order(rparameters$clusterGeomorphIndex),]
globalr <- read.csv('priorsr.csv')
rparametersDBSCAN <- read.csv('priorsr_geoBAMunsupervised.csv')
rparametersDBSCAN <- rparametersDBSCAN[2:8,]

r_expert <- ggplot(data.frame(logr = c(globalr[3,2], globalr[7,2])), aes(x = logr)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(rparameters[1,5], rparameters[1,9], rparameters[1,7], rparameters[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[2,5], b = rparameters[2,9],rparameters[2,7], rparameters[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[3,5], b = rparameters[3,9],rparameters[3,7], rparameters[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[4,5], b = rparameters[4,9],rparameters[4,7], rparameters[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[5,5], b = rparameters[5,9],rparameters[5,7], rparameters[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[6,5], b = rparameters[6,9],rparameters[6,7], rparameters[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[7,5], b = rparameters[7,9],rparameters[7,7], rparameters[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[8,5], b = rparameters[8,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "8"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[9,5], b = rparameters[10,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "9"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[10,5], b = rparameters[10,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "10"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[11,5], b = rparameters[11,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "11"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[12,5], b = rparameters[12,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "12"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[13,5], b = rparameters[13,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "13"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[14,5], b = rparameters[14,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "14"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[15,5], b = rparameters[15,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "15"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparameters[16,5], b = rparameters[16,9], mean = rparameters[8,7], sd = rparameters[8,4]),
                aes(colour = "16"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalr[3,2], globalr[7,2], globalr[5,2], globalr[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors, breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'Global')) +
  theme(legend.title = element_blank())

r_DBSCAN <- ggplot(data.frame(logr = c(globalr[3,2], globalr[7,2])), aes(x = logr)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(rparametersDBSCAN[1,5], rparametersDBSCAN[1,9], rparametersDBSCAN[1,7], rparametersDBSCAN[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparametersDBSCAN[2,5], b = rparametersDBSCAN[2,9],rparametersDBSCAN[2,7], rparametersDBSCAN[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparametersDBSCAN[3,5], b = rparametersDBSCAN[3,9],rparametersDBSCAN[3,7], rparametersDBSCAN[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparametersDBSCAN[4,5], b = rparametersDBSCAN[4,9],rparametersDBSCAN[4,7], rparametersDBSCAN[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparametersDBSCAN[5,5], b = rparametersDBSCAN[5,9],rparametersDBSCAN[5,7], rparametersDBSCAN[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparametersDBSCAN[6,5], b = rparametersDBSCAN[6,9],rparametersDBSCAN[6,7], rparametersDBSCAN[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = rparametersDBSCAN[7,5], b = rparametersDBSCAN[7,9],rparametersDBSCAN[7,7], rparametersDBSCAN[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalr[3,2], globalr[7,2], globalr[5,2], globalr[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors2, breaks = c('1', '2', '3', '4', '5', '6', '7', 'Global')) +
  theme(legend.title = element_blank())

#Wb
Wbparameters <- read.csv('priorsWb_geoBAMexpert.csv')
Wbparameters <- Wbparameters[order(Wbparameters$clusterGeomorphIndex),]
globalWb <- read.csv('priorsWb.csv')
WbparametersDBSCAN <- read.csv('priorsWb_geoBAMunsupervised.csv')
WbparametersDBSCAN <- WbparametersDBSCAN[2:8,]

Wb_expert <- ggplot(data.frame(logWb = c(globalWb[3,2], globalWb[7,2])), aes(x = logWb)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(Wbparameters[1,5], Wbparameters[1,9], Wbparameters[1,7], Wbparameters[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[2,5], b = Wbparameters[2,9],Wbparameters[2,7], Wbparameters[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[3,5], b = Wbparameters[3,9],Wbparameters[3,7], Wbparameters[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[4,5], b = Wbparameters[4,9],Wbparameters[4,7], Wbparameters[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[5,5], b = Wbparameters[5,9],Wbparameters[5,7], Wbparameters[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[6,5], b = Wbparameters[6,9],Wbparameters[6,7], Wbparameters[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[7,5], b = Wbparameters[7,9],Wbparameters[7,7], Wbparameters[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[8,5], b = Wbparameters[8,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "8"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[9,5], b = Wbparameters[10,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "9"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[10,5], b = Wbparameters[10,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "10"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[11,5], b = Wbparameters[11,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "11"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[12,5], b = Wbparameters[12,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "12"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[13,5], b = Wbparameters[13,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "13"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[14,5], b = Wbparameters[14,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "14"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[15,5], b = Wbparameters[15,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "15"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Wbparameters[16,5], b = Wbparameters[16,9], mean = Wbparameters[8,7], sd = Wbparameters[8,4]),
                aes(colour = "16"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalWb[3,2], globalWb[7,2], globalWb[5,2], globalWb[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors, breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'Global')) +
  theme(legend.title = element_blank())

Wb_DBSCAN <- ggplot(data.frame(logWb = c(globalWb[3,2], globalWb[7,2])), aes(x = logWb)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(WbparametersDBSCAN[1,5], WbparametersDBSCAN[1,9], WbparametersDBSCAN[1,7], WbparametersDBSCAN[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = WbparametersDBSCAN[2,5], b = WbparametersDBSCAN[2,9],WbparametersDBSCAN[2,7], WbparametersDBSCAN[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = WbparametersDBSCAN[3,5], b = WbparametersDBSCAN[3,9],WbparametersDBSCAN[3,7], WbparametersDBSCAN[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = WbparametersDBSCAN[4,5], b = WbparametersDBSCAN[4,9],WbparametersDBSCAN[4,7], WbparametersDBSCAN[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = WbparametersDBSCAN[5,5], b = WbparametersDBSCAN[5,9],WbparametersDBSCAN[5,7], WbparametersDBSCAN[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = WbparametersDBSCAN[6,5], b = WbparametersDBSCAN[6,9],WbparametersDBSCAN[6,7], WbparametersDBSCAN[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = WbparametersDBSCAN[7,5], b = WbparametersDBSCAN[7,9],WbparametersDBSCAN[7,7], WbparametersDBSCAN[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalWb[3,2], globalWb[7,2], globalWb[5,2], globalWb[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors2, breaks = c('1', '2', '3', '4', '5', '6', '7', 'Global')) +
  theme(legend.title = element_blank())

#Db
Dbparameters <- read.csv('priorsDb_geoBAMexpert.csv')
Dbparameters <- Dbparameters[order(Dbparameters$clusterGeomorphIndex),]
globalDb <- read.csv('priorsDb.csv')
DbparametersDBSCAN <- read.csv('priorsDb_geoBAMunsupervised.csv')
DbparametersDBSCAN <- DbparametersDBSCAN[2:8,]

Db_expert <- ggplot(data.frame(logDb = c(globalDb[3,2], globalDb[7,2])), aes(x = logDb)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(Dbparameters[1,5], Dbparameters[1,9], Dbparameters[1,7], Dbparameters[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[2,5], b = Dbparameters[2,9],Dbparameters[2,7], Dbparameters[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[3,5], b = Dbparameters[3,9],Dbparameters[3,7], Dbparameters[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[4,5], b = Dbparameters[4,9],Dbparameters[4,7], Dbparameters[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[5,5], b = Dbparameters[5,9],Dbparameters[5,7], Dbparameters[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[6,5], b = Dbparameters[6,9],Dbparameters[6,7], Dbparameters[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[7,5], b = Dbparameters[7,9],Dbparameters[7,7], Dbparameters[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[8,5], b = Dbparameters[8,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "8"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[9,5], b = Dbparameters[10,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "9"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[10,5], b = Dbparameters[10,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "10"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[11,5], b = Dbparameters[11,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "11"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[12,5], b = Dbparameters[12,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "12"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[13,5], b = Dbparameters[13,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "13"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[14,5], b = Dbparameters[14,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "14"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[15,5], b = Dbparameters[15,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "15"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = Dbparameters[16,5], b = Dbparameters[16,9], mean = Dbparameters[8,7], sd = Dbparameters[8,4]),
                aes(colour = "16"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalDb[3,2], globalDb[7,2], globalDb[5,2], globalDb[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors, breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'Global')) +
  theme(legend.title = element_blank())

Db_DBSCAN <- ggplot(data.frame(logDb = c(globalDb[3,2], globalDb[7,2])), aes(x = logDb)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(DbparametersDBSCAN[1,5], DbparametersDBSCAN[1,9], DbparametersDBSCAN[1,7], DbparametersDBSCAN[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = DbparametersDBSCAN[2,5], b = DbparametersDBSCAN[2,9],DbparametersDBSCAN[2,7], DbparametersDBSCAN[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = DbparametersDBSCAN[3,5], b = DbparametersDBSCAN[3,9],DbparametersDBSCAN[3,7], DbparametersDBSCAN[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = DbparametersDBSCAN[4,5], b = DbparametersDBSCAN[4,9],DbparametersDBSCAN[4,7], DbparametersDBSCAN[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = DbparametersDBSCAN[5,5], b = DbparametersDBSCAN[5,9],DbparametersDBSCAN[5,7], DbparametersDBSCAN[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = DbparametersDBSCAN[6,5], b = DbparametersDBSCAN[6,9],DbparametersDBSCAN[6,7], DbparametersDBSCAN[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = DbparametersDBSCAN[7,5], b = DbparametersDBSCAN[7,9],DbparametersDBSCAN[7,7], DbparametersDBSCAN[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalDb[3,2], globalDb[7,2], globalDb[5,2], globalDb[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors2, breaks = c('1', '2', '3', '4', '5', '6', '7', 'Global')) +
  theme(legend.title = element_blank())

#n
nparameters <- read.csv('priorsn_geoBAMexpert.csv')
nparameters <- nparameters[order(nparameters$clusterGeomorphIndex),]
globaln <- read.csv('priorsn.csv')
nparametersDBSCAN <- read.csv('priorsn_geoBAMunsupervised.csv')
nparametersDBSCAN <- nparametersDBSCAN[2:8,]

n_expert <- ggplot(data.frame(logn = c(globaln[3,2], globaln[7,2])), aes(x = logn)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(nparameters[1,5], nparameters[1,9], nparameters[1,7], nparameters[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[2,5], b = nparameters[2,9],nparameters[2,7], nparameters[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[3,5], b = nparameters[3,9],nparameters[3,7], nparameters[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[4,5], b = nparameters[4,9],nparameters[4,7], nparameters[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[5,5], b = nparameters[5,9],nparameters[5,7], nparameters[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[6,5], b = nparameters[6,9],nparameters[6,7], nparameters[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[7,5], b = nparameters[7,9],nparameters[7,7], nparameters[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[8,5], b = nparameters[8,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "8"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[9,5], b = nparameters[10,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "9"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[10,5], b = nparameters[10,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "10"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[11,5], b = nparameters[11,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "11"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[12,5], b = nparameters[12,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "12"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[13,5], b = nparameters[13,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "13"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[14,5], b = nparameters[14,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "14"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[15,5], b = nparameters[15,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "15"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparameters[16,5], b = nparameters[16,9], mean = nparameters[8,7], sd = nparameters[8,4]),
                aes(colour = "16"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globaln[3,2], globaln[7,2], globaln[5,2], globaln[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors, breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'Global')) +
  theme(legend.title = element_blank())

n_DBSCAN <- ggplot(data.frame(logn = c(globaln[3,2], globaln[7,2])), aes(x = logn)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(nparametersDBSCAN[1,5], nparametersDBSCAN[1,9], nparametersDBSCAN[1,7], nparametersDBSCAN[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparametersDBSCAN[2,5], b = nparametersDBSCAN[2,9],nparametersDBSCAN[2,7], nparametersDBSCAN[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparametersDBSCAN[3,5], b = nparametersDBSCAN[3,9],nparametersDBSCAN[3,7], nparametersDBSCAN[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparametersDBSCAN[4,5], b = nparametersDBSCAN[4,9],nparametersDBSCAN[4,7], nparametersDBSCAN[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparametersDBSCAN[5,5], b = nparametersDBSCAN[5,9],nparametersDBSCAN[5,7], nparametersDBSCAN[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparametersDBSCAN[6,5], b = nparametersDBSCAN[6,9],nparametersDBSCAN[6,7], nparametersDBSCAN[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = nparametersDBSCAN[7,5], b = nparametersDBSCAN[7,9],nparametersDBSCAN[7,7], nparametersDBSCAN[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globaln[3,2], globaln[7,2], globaln[5,2], globaln[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors2, breaks = c('1', '2', '3', '4', '5', '6', '7', 'Global')) +
  theme(legend.title = element_blank())

#b
bparameters <- read.csv('priorsb_geoBAMexpert.csv')
bparameters <- bparameters[order(bparameters$clusterGeomorphIndex),]
globalb <- read.csv('priorsb.csv')
bparametersDBSCAN <- read.csv('priorsb_geoBAMunsupervised.csv')
bparametersDBSCAN <- bparametersDBSCAN[2:8,]

b_expert <- ggplot(data.frame(logb = c(globalb[3,2], globalb[7,2])), aes(x = logb)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(bparameters[1,5], bparameters[1,9], bparameters[1,7], bparameters[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[2,5], b = bparameters[2,9],bparameters[2,7], bparameters[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[3,5], b = bparameters[3,9],bparameters[3,7], bparameters[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[4,5], b = bparameters[4,9],bparameters[4,7], bparameters[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[5,5], b = bparameters[5,9],bparameters[5,7], bparameters[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[6,5], b = bparameters[6,9],bparameters[6,7], bparameters[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[7,5], b = bparameters[7,9],bparameters[7,7], bparameters[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[8,5], b = bparameters[8,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "8"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[9,5], b = bparameters[10,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "9"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[10,5], b = bparameters[10,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "10"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[11,5], b = bparameters[11,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "11"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[12,5], b = bparameters[12,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "12"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[13,5], b = bparameters[13,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "13"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[14,5], b = bparameters[14,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "14"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[15,5], b = bparameters[15,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "15"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparameters[16,5], b = bparameters[16,9], mean = bparameters[8,7], sd = bparameters[8,4]),
                aes(colour = "16"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalb[3,2], globalb[7,2], globalb[5,2], globalb[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors, breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'Global')) +
  theme(legend.title = element_blank())

b_DBSCAN <- ggplot(data.frame(logb = c(globalb[3,2], globalb[7,2])), aes(x = logb)) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(bparametersDBSCAN[1,5], bparametersDBSCAN[1,9], bparametersDBSCAN[1,7], bparametersDBSCAN[1,4]),
                aes(colour = "1"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparametersDBSCAN[2,5], b = bparametersDBSCAN[2,9],bparametersDBSCAN[2,7], bparametersDBSCAN[2,4]),
                aes(colour = "2"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparametersDBSCAN[3,5], b = bparametersDBSCAN[3,9],bparametersDBSCAN[3,7], bparametersDBSCAN[3,4]),
                aes(colour = "3"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparametersDBSCAN[4,5], b = bparametersDBSCAN[4,9],bparametersDBSCAN[4,7], bparametersDBSCAN[4,4]),
                aes(colour = "4"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparametersDBSCAN[5,5], b = bparametersDBSCAN[5,9],bparametersDBSCAN[5,7], bparametersDBSCAN[5,4]),
                aes(colour = "5"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparametersDBSCAN[6,5], b = bparametersDBSCAN[6,9],bparametersDBSCAN[6,7], bparametersDBSCAN[6,4]),
                aes(colour = "6"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(a = bparametersDBSCAN[7,5], b = bparametersDBSCAN[7,9],bparametersDBSCAN[7,7], bparametersDBSCAN[7,4]),
                aes(colour = "7"), size = 1.5) +
  stat_function(fun = dtruncnorm, geom="area", alpha=0.1, args = list(globalb[3,2], globalb[7,2], globalb[5,2], globalb[2,2]),
                aes(colour = "Global"), size = 1.5) +
  ylab('Frequency') +
  scale_colour_manual(values=mycolors2, breaks = c('1', '2', '3', '4', '5', '6', '7', 'Global')) +
  theme(legend.title = element_blank())

plotgrid <- plot_grid(A0_expert + theme(legend.position='none', axis.title.y = element_blank()),
                      A0_DBSCAN + theme(legend.position='none', axis.title.y = element_blank()),
                      Wb_expert + theme(legend.position='none', axis.title.y = element_blank()),
                      Wb_DBSCAN + theme(legend.position='none', axis.title.y = element_blank()),
                      Db_expert + theme(legend.position='none', axis.title.y = element_blank()),
                      Db_DBSCAN + theme(legend.position='none', axis.title.y = element_blank()),
                      r_expert + theme(legend.position='none', axis.title.y = element_blank()),
                      r_DBSCAN + theme(legend.position='none', axis.title.y = element_blank()),
                      n_expert + theme(legend.position='none', axis.title.y = element_blank()),
                      n_DBSCAN + theme(legend.position='none', axis.title.y = element_blank()),
                      b_expert + theme(legend.position='bottom', legend, axis.title.y = element_blank()),
                      b_DBSCAN + theme(legend.position='bottom', legend, axis.title.y = element_blank()),
                      ncol = 2)

yTitleCombo <- textGrob('Frequency', gp=gpar(fontface="bold", col="black", fontsize=18), rot=90)

plotgrid <- grid.arrange(arrangeGrob(plotgrid, left = yTitleCombo))

ggsave('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\prior_distributions.tiff', width = 12, height = 13)

#T-tests
A0parameters.aov <- summary(aov(X50. ~ clusterGeomorphIndex, data = A0parameters))[[1]][[1,"Pr(>F)"]]
A0parametersDBSCAN.aov <- summary(aov(X50. ~ cluster, data = A0parametersDBSCAN))[[1]][[1,"Pr(>F)"]]

Wbparameters.aov <- summary(aov(X50. ~ clusterGeomorphIndex, data = Wbparameters))[[1]][[1,"Pr(>F)"]]
WbparametersDBSCAN.aov <- summary(aov(X50. ~ cluster, data = WbparametersDBSCAN))[[1]][[1,"Pr(>F)"]]

Dbparameters.aov <- summary(aov(X50. ~ clusterGeomorphIndex, data = Dbparameters))[[1]][[1,"Pr(>F)"]]
DbparametersDBSCAN.aov <- summary(aov(X50. ~ cluster, data = DbparametersDBSCAN))[[1]][[1,"Pr(>F)"]]

rparameters.aov <- summary(aov(X50. ~ clusterGeomorphIndex, data = rparameters))[[1]][[1,"Pr(>F)"]]
rparametersDBSCAN.aov <- summary(aov(X50. ~ cluster, data = rparametersDBSCAN))[[1]][[1,"Pr(>F)"]]

nparameters.aov <- summary(aov(X50. ~ clusterGeomorphIndex, data = nparameters))[[1]][[1,"Pr(>F)"]]
nparametersDBSCAN.aov <- summary(aov(X50. ~ cluster, data = nparametersDBSCAN))[[1]][[1,"Pr(>F)"]]

bparameters.aov <- summary(aov(X50. ~ clusterGeomorphIndex, data = bparameters))[[1]][[1,"Pr(>F)"]]
bparametersDBSCAN.aov <- summary(aov(X50. ~ cluster, data = bparametersDBSCAN))[[1]][[1,"Pr(>F)"]]

anova_results <- data.frame('A0'=c(A0parameters.aov, A0parametersDBSCAN.aov), 
                            'Wb'=c(Wbparameters.aov, WbparametersDBSCAN.aov), 
                            'Db'=c(Dbparameters.aov, DbparametersDBSCAN.aov), 
                            'r'=c(rparameters.aov, rparametersDBSCAN.aov),
                            'b'=c(bparameters.aov, bparametersDBSCAN.aov), 
                            'n'=c(nparameters.aov, nparametersDBSCAN.aov))

rownames(anova_results) <- c('Expert', 'Unsupervised')

#MERIT Hydro map---------------------------------------------
widthsClass <- read.table('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\priors\\WidthsClass_geoBAMexpert.csv', sep=',', header=TRUE)
classes <- widthsClass$X50.
widthsMatrix <- load('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Mackenzie\\width_bam.Rdata')
widthsMatrix <- group_by(width_bam, reach, Date) %>% summarise(reachAvgWidth = mean(width))
widthsMatrix <- filter(widthsMatrix, reachAvgWidth > 0)
widthsMatrix2 <- group_by(widthsMatrix, reach) %>% summarise(logSD = sd(log(reachAvgWidth), na.rm=TRUE), mean = mean(log(reachAvgWidth), na.rm=TRUE), median = median(reachAvgWidth))
colnames(widthsMatrix2) <- c('COMID', 'logSD', 'logMean', 'median')

maxWidth = 6.5 #from training data,

classify_geomorph <- function(sd, mean){
  if (is.finite(mean) == 0 || is.na(mean)==1) {
    return(1)
  }
  else {
    index <- ifelse(mean > maxWidth, 17, which.min(abs(classes-mean))) #17 for big rivers
    index <- ifelse(is.na(sd) == 0 & sd >= 0.45, 16, index) 
    return(index) 
  }
}

classify_func_unsupervised <- function(width) {
  #One-vs-Rest Logistic regression model
  #test set accuracy rate of 87%
  p_noise <- 1 / (1.0 + exp(-(-3.11056777 + 0.00189261 * width)))
  p_1 <- 1 / (1.0 + exp(-(1.95763357 + -0.00114445 * width)))
  p_2 <- 1 / (1.0 + exp(-(-4.05274822 + 0.00156913 * width)))
  p_3<- 1 / (1.0 + exp(-(-4.43158034 + -0.00116211 * width)))
  p_4 <- 1 / (1.0 + exp(-(-3.99272449 + 0.00168791* width)))
  p_5 <- 1 / (1.0 + exp(-(-4.28204232 + -0.01493184 * width)))
  p_6 <- 1 / (1.0 + exp(-(-3.87408245 + -0.00175089 * width)))
  p_7 <- 1 / (1.0 + exp(-(-1.16209678 + -0.12664823 * width)))
  
  probs <- data.frame(p_noise, p_1, p_2, p_3, p_4, p_5, p_6, p_7)
  
  index <- which.max(probs)
  index <- ifelse(index == 1, 8, index-1) #101 for 'noisey' rivers
  return(index)
}

meritHYDRO <- st_read('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Mackenzie\\Mackenzie_rivers.shp')
lakes <- st_read('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Mackenzie\\clip_lakes.shp')
shield <- st_read('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Mackenzie\\CA_shield.shp')
shield <- st_transform(shield, crs(meritHYDRO))
basemap <- raster('C:\\Users\\craig\\Box Sync\\Datasets\\HYP_HR_SR_W\\HYP_HR_SR_W\\HYP_HR_SR_W.tif')

#temp <- meritHYDRO$width_mean
meritHYDRO <- left_join(meritHYDRO, widthsMatrix2, by='COMID')
output <- as.vector(1:nrow(meritHYDRO))
output2 <- as.vector(1:nrow(meritHYDRO))
for (i in 1:nrow(meritHYDRO)) {
  class <- classify_geomorph(meritHYDRO$logSD[i], log(meritHYDRO$width_mean[i]))
  output[i] <- class
  
  class <- classify_func_unsupervised(meritHYDRO$width_mean[i])
  output2[i] <- class
}

meritHYDRO$geomorph_class <- output
meritHYDRO$unsupervised_class <- output2

#Validation NSE
stats <- group_by(mackenzie, reach) %>% 
  summarise(NSE_expert = NSE(geoBAM_expert_mean, obs_Q, na.rm=TRUE), 
            NSE_unsupervised = NSE(geoBAM_unsupervised_mean, obs_Q, na.rm=TRUE), 
            NSE_BAM = NSE(BAM_mean, obs_Q, na.rm=TRUE),
            n = n())

#convert Dongmei IDs to COMIDs
load('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Mackenzie\\hrridMap.Rdata')
hrridMap <- as.data.frame(hrridMap)
hrridMap$COMID <- rownames(hrridMap)
colnames(hrridMap) <- c('reach', 'COMID')

stats$reach <- as.numeric(stats$reach)
stats <- left_join(stats, hrridMap, by='reach')
stats$COMID <- as.numeric(stats$COMID)

meritHYDRO <- left_join(meritHYDRO, stats, by = "COMID")
plotNetwork <- filter(meritHYDRO, is.na(NSE_BAM)==0)
plotNetwork$expert_imp <- (plotNetwork$NSE_expert - plotNetwork$NSE_BAM)
plotNetwork$unsupervised_imp <- (plotNetwork$NSE_unsupervised - plotNetwork$NSE_BAM)
plotNetwork <- plotNetwork[order(plotNetwork$expert_imp),] 

meritHYDRO <- filter(meritHYDRO, width_mean > 0)

hydroBox <- st_bbox(meritHYDRO)

#Palette with black for global
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(17)
mycolors <- c(mycolors, 'black')

map <- tm_shape(meritHYDRO, bbox = hydroBox) +
  tm_lines(col= 'geomorph_class', n =18, style='fixed', lwd=2, breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18, 19),
           labels=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', 'Global'),
           title.col='River Type',
           palette = mycolors) +
  tm_graticules()

mapLakes <- tm_shape(lakes, bbox = hydroBox) +
  tm_fill(col = 'deepskyblue4')

mapShield <- tm_shape(shield, bbox = hydroBox) +
  tm_borders(col = 'black', lwd=2)

bubbles <- tm_shape(plotNetwork, bbox = hydroBox) +
  tm_bubbles(col= 'expert_imp', size = 1, n = 5, style='fixed', breaks=c(min(plotNetwork$expert_imp), 0, 0.5, 1.5, 5, max(plotNetwork$expert_imp)),
             labels=c('-0.10-0', '0-0.50', '0.50-1.5', '1.5-5.0', '5.0+'),
          title.col=' NSE Increase',
          palette = 'PRGn')

base <- tm_shape(basemap, bbox=hydroBox) +
  tm_raster(alpha=0.75, legend.show = FALSE, palette = 'Greys')

mapfin <- base + mapShield + map + mapLakes + bubbles + tm_layout(legend.outside = TRUE)

tmap_save(mapfin, 'C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\mackenzie_map.png')

#NSE by stream order
plot <- ggplot(plotNetwork, aes(x=factor(order), y= expert_imp, fill=factor(order))) +
  geom_boxplot() +
#  geom_jitter(shape=16, position=position_jitter(0.2), size=1.5) +
  scale_fill_brewer(palette = 'Dark2', name='Stream Order') +
  ylab('Increase in NSE') +
  xlab('Stream Order') +
  geom_hline(yintercept =0, linetype='dashed') +
  geom_hline(yintercept =  1, linetype='dashed')
plot

#Mackenzie hydrographs---------------------------------------------------------
scaleFUN <- function(x) sprintf("%.2f", x)
scaleFUN3 <- function(x) sprintf("%.1f", x)

#Randomly grab 12 reaches for validation (3 from each NSE quartile)
stats$NSE_imp <- stats$NSE_expert - stats$NSE_BAM
quartiles <- quantile(stats$NSE_imp, c(0.25, 0.5, 0.75))
first_quart <- stats[stats$NSE_imp <= quartiles[1],]
sec_quart <- stats[stats$NSE_imp > quartiles[1] & stats$NSE_imp <= quartiles[2],]
third_quart <- stats[stats$NSE_imp > quartiles[2] & stats$NSE_imp <= quartiles[3],]
fourth_quart <- stats[stats$NSE_imp > quartiles[3],]

set.seed(13)
sample1 <- sample(first_quart$reach, 3)
sample2 <- sample(sec_quart$reach, 3)
sample3 <- sample(third_quart$reach, 3)
sample4 <- sample(fourth_quart$reach, 3)
sample <- c(sample1, sample2, sample3, sample4)

mack_hydrographs <- filter(mackenzie, reach %in% sample)
mack_hydrographs <- mack_hydrographs[,-c(5,6,7,9)]
colnames(mack_hydrographs) <- c('Date', 'reach', 'Observed Discharge', 'geoBAM-Expert', 'Control BAM')
mack_hydrographs$Date <- as.Date(mack_hydrographs$Date, '%m/%d/%Y')

pltList <- list()

for (i in 1:12){
  pltName <- paste('hydrograph_', sample[i], sep='')
  temp <- filter(mack_hydrographs, reach == sample[i])
  temp$Date <- 1:nrow(temp)
  maxTime <- nrow(temp)
  temp$Date <- temp$Date / maxTime
  temp <- gather(temp, 'key', 'value', c('Observed Discharge', 'geoBAM-Expert', 'Control BAM'))
  
  pltList[[ pltName ]] <- ggplot(temp, aes(x=Date, y=value, color = key))+
    geom_point(data = filter(temp, key!='Observed Discharge'), size=2) +
    geom_line(data = filter(temp, key=='Observed Discharge'), size=1.2, linetype='dotted')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
    scale_color_manual(name="", values = c('#a1d99b','#006d2c', 'black')) +
    scale_y_continuous(labels=comma) +
    ggtitle(sample[i])
}

plotgrid <- plot_grid(pltList[[1]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList[[2]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList[[3]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      textGrob('Litte Improvement', gp=gpar(fontface="bold", col="black", fontsize=18)),
                      pltList[[4]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList[[5]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList[[6]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      textGrob('Some Improvement', gp=gpar(fontface="bold", col="black", fontsize=18)),
                      pltList[[7]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList[[8]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      pltList[[9]] + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()),
                      textGrob('More Improvement', gp=gpar(fontface="bold", col="black", fontsize=18)),
                      pltList[[10]] + theme(legend.position='none'),
                      pltList[[11]] + theme(legend.position='none'),
                      pltList[[12]] + theme(legend.position='none'),
                      textGrob('Most Improvement', gp=gpar(fontface="bold", col="black", fontsize=18)),
                      ncol=4)

legend <- get_legend(
  # create some space to the left of the legend
  pltList[[1]] + theme(legend.text=element_text(size=17))
)

plotHydrographs <- plotgrid + draw_grob(legend, 0.8, -0.48, 0.2, 1)

yTitleCombo <- textGrob(expression(Discharge~(cms)), gp=gpar(fontface="bold", col="black", fontsize=18), rot=90)
xTitleCombo <- textGrob(expression(Timestep/Max~Timestep), gp=gpar(fontface="bold", col="black", fontsize=18))

plotHydrographs <- grid.arrange(arrangeGrob(plotHydrographs, left = yTitleCombo, bottom = xTitleCombo))
ggsave('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\hydrographs_mackenzie.png', plotHydrographs, width = 14, height = 9)

#Classifications compared---------------------------------------------
#SWOT
BAM_control = list.files(paste(output_directory, "swot\\BAM_control\\", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
BAM_control <- ldply(BAM_control, read_csv)
BAM_control$river <- files
colnames(BAM_control) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
BAM_control$Type <- rep('BAM_control', nrow(BAM_control))
BAM_control$order <- rep(1, nrow(BAM_control))

geoBAM_expert = list.files(paste(output_directory, "swot\\geoBAM_expert\\", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
geoBAM_expert <- ldply(geoBAM_expert, read_csv)
geoBAM_expert$river <- files
colnames(geoBAM_expert) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'AMHGflag', 'rivClass_1', 'rivClass_2',
                             'rivClass_3', 'rivClass_4', 'rivClass_5', 'rivClass_6', 'rivClass_7', 'rivClass_8',
                             'rivClass_9', 'rivClass_10', 'rivClass_11', 'rivClass_12', 'rivClass_13', 'rivClass_14',
                             'rivClass_15', 'rivClass_16', 'river')
geoBAM_expert$Type <- rep('geoBAM_expert', nrow(geoBAM_expert))
geoBAM_expert$order <- rep(1, nrow(geoBAM_expert))

geoBAM_unsupervised = list.files(paste(output_directory, "swot\\geoBAM_unsupervised\\", sep=''), pattern="*.csv", full.names = TRUE) #switch.x
geoBAM_unsupervised <- ldply(geoBAM_unsupervised, read_csv)
geoBAM_unsupervised$river <- files
colnames(geoBAM_unsupervised) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'AMHGflag', 'rivClass_1', 'rivClass_2',
                                   'rivClass_3', 'rivClass_4', 'rivClass_5', 'rivClass_6', 'rivClass_7', 'rivClass_8',
                                   'rivClass_9', 'rivClass_10', 'rivClass_11', 'rivClass_12', 'rivClass_13', 'rivClass_14',
                                   'rivClass_15', 'rivClass_16', 'river')
geoBAM_unsupervised$Type <- rep('geoBAM_unsupervised', nrow(geoBAM_unsupervised))
geoBAM_unsupervised$order <- rep(1, nrow(geoBAM_unsupervised))

write.csv(geoBAM_unsupervised, 'C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\geoBAM_unsupervised.csv') #save results
write.csv(geoBAM_expert, 'C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\geoBAM_expert.csv') #save results
write.csv(BAM_control, 'C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\BAM_control.csv') #save results

geoBAM_expert <- geoBAM_expert[,c(2:5,23,24)]
geoBAM_unsupervised <- geoBAM_unsupervised[,c(2:5,23,24)]
BAM_control <- BAM_control[,-1]

#Mackenzie
mackenzie_BAM <- group_by(mackenzie, reach) %>% 
  drop_na() %>%
  summarise(RRMSE =  sqrt(mean((BAM_mean - obs_Q)^2 / obs_Q^2)), 
            NRMSE = sqrt(mean((obs_Q - BAM_mean)^2)) / mean(obs_Q),
            NSE = NSE(BAM_mean, obs_Q, na.rm=TRUE),
            rBIAS =  mean(BAM_mean - obs_Q) / mean(obs_Q),
            Type = 'Mackenzie_control')
colnames(mackenzie_BAM) <- c('river', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'Type')

mackenzie_expert <- group_by(mackenzie, reach) %>% 
  drop_na() %>%
  summarise(RRMSE =  sqrt(mean((geoBAM_expert_mean - obs_Q)^2 / obs_Q^2)), 
            NRMSE = sqrt(mean((obs_Q - geoBAM_expert_mean)^2)) / mean(obs_Q),
            NSE = NSE(geoBAM_expert_mean, obs_Q, na.rm=TRUE),
            rBIAS =  mean(geoBAM_expert_mean - obs_Q) / mean(obs_Q),
            Type = 'Mackenzie_expert')
colnames(mackenzie_expert) <- c('river', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'Type')

mackenzie_unsupervised <- group_by(mackenzie, reach) %>% 
  drop_na() %>%
  summarise(RRMSE =  sqrt(mean((geoBAM_unsupervised_mean - obs_Q)^2 / obs_Q^2)), 
            NRMSE = sqrt(mean((obs_Q - geoBAM_unsupervised_mean)^2)) / mean(obs_Q),
            NSE = NSE(geoBAM_unsupervised_mean, obs_Q, na.rm=TRUE),
            rBIAS =  mean(geoBAM_unsupervised_mean - obs_Q) / mean(obs_Q),
            Type = 'Mackenzie_unsupervised')
colnames(mackenzie_unsupervised) <- c('river', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'Type')

#plot
bam_stats <- rbind(BAM_control, geoBAM_unsupervised, geoBAM_expert, mackenzie_BAM, mackenzie_unsupervised, mackenzie_expert)

plot <- gather(bam_stats, 'metric', 'value', c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))
plot$Type <- factor(plot$Type, levels = c('BAM_control', 'geoBAM_unsupervised', 'geoBAM_expert', 'Mackenzie_control', 'Mackenzie_unsupervised', 'Mackenzie_expert'))
plot$metric <- factor(plot$metric, levels=c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))

stats <- group_by(plot, Type, metric) %>% dplyr::summarize(mean = mean(value), median = median(value), IQR = IQR(value))

write.csv(stats, 'C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\outputs\\final_results\\summ_stats.csv') #save results

not_plottedNSE <- plot %>% group_by(Type, metric) %>%
  filter(metric == 'NSE') %>%
  summarise(num_outliers = sum(value < -2.3))

not_plottedOthers <- plot %>% group_by(Type, metric) %>%
  filter(metric != 'NSE') %>%
  summarise(num_outliers = sum(value > 2))

boxplots <- ggplot(plot, aes(x=metric, y = value, fill=Type)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-2.3, 2))+
 # geom_text(data=not_plottedNSE, aes(y=-2.46,label=num_outliers), size=3,hjust=0.5, position = position_dodge(width=0.85)) +
#  geom_text(data=not_plottedOthers,aes(y=2.18,label=num_outliers), size=3, hjust=0.7, position = position_dodge(width = 0.85)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_hline(yintercept=0.5, linetype='dashed') +
  geom_hline(yintercept=1, linetype='dashed') +
  ylab('') +
  xlab('%') +
  theme(legend.position= c(0.01, 0.27)) +
  scale_fill_manual(name = "", 
                    labels = c("SWOT-Control", 'SWOT-Unsupervised', 'SWOT-Expert', 'Mackenzie-Control', 'Mackenzie-Unsupervised', 'Mackenzie-Expert'), 
                    values=c('#6baed6', '#2171b5', '#08519c', '#74c476', '#238b45', '#006d2c'))
boxplots
ggsave('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\boxplots_compare.png', boxplots, dpi = 400, width = 6.5, height = 6.5)






#Space-varying vs Time-and-space varying Manning's n for supplement-----------------------
output_directory=paste('C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//outputs//')

new_new_regime = list.files(paste(output_directory, "old//new_physics_new_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_regime <- ldply(new_new_regime, read_csv)
new_new_regime$river <- phase_files
colnames(new_new_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_regime$Type <- rep('New_new_regime', nrow(new_new_regime))
new_new_regime$order <- rep(5, nrow(new_new_regime))
new_new_regime <- filter(new_new_regime, river %in% files)

varyingN = list.files(paste(output_directory, "old//new_physics_new_priors_varyingN//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
varyingN <- ldply(varyingN, read_csv)
varyingN$river <- phase_files
colnames(varyingN) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
varyingN$Type <- rep('Time-and-space varying n', nrow(varyingN))
varyingN$order <- rep(5, nrow(varyingN))

bam_stats <- rbind(new_new_regime, varyingN)
plot <- gather(bam_stats, 'metric', 'value', c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))
plot$Type <- factor(plot$Type, levels = c('New_new_regime', 'Time-and-space varying n'))

plot$metric <- factor(plot$metric, levels=c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))

#just pepsi 1
pepsi1 <- phase_files[c(2,3,4,7, 9,10,14, 23, 24, 25, 26, 27, 30, 31, 32, 33, 34)]
plot <- filter(plot, river %in% pepsi1)


cdf <- ggplot(filter(plot, metric=='NSE'), aes(x=value, color=Type)) +
  stat_ecdf(size=2) +
  scale_color_brewer(palette = 'Dark2', 
                    labels=c('Space-varying n', 'Space-and-time-varying n')) +
  geom_vline(xintercept=0, linetype='dashed', size=1) +
  xlab('NSE') +
  ylab('Density')
cdf  
ggsave('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\supp\\n_cdf.tiff', cdf, dpi = 400, width = 6.5, height = 6.5)



#---------------------------------------------------
#
#Pepsi boxplots---------------------------------------------------------------------------------------------------
output_directory=paste('C://Users//craig//Box Sync//Ongoing Projects//geomorph_class//outputs//')

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

new_old_regime = list.files(paste(output_directory, "new_old_fin_reachN//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_old_regime <- ldply(new_old_regime, read_csv)
new_old_regime$river <- phase_files
colnames(new_old_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_old_regime$Type <- rep('New_old_regime', nrow(new_old_regime))
new_old_regime$order <- rep(4, nrow(new_old_regime))

new_new_geomorph = list.files(paste(output_directory, "geoBAM//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_geomorph <- ldply(new_new_geomorph, read_csv)
new_new_geomorph$river <- phase_files
colnames(new_new_geomorph) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_geomorph$Type <- rep('new_new_Geomorph', nrow(new_new_geomorph))
new_new_geomorph$order <- rep(5, nrow(new_new_geomorph))

new_new_dbscan = list.files(paste(output_directory, "new_new_reachN_DBSCAN_20_Class_all_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_dbscan <- ldply(new_new_dbscan, read_csv)
new_new_dbscan$river <- phase_files
colnames(new_new_dbscan) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_dbscan$Type <- rep('DBSCAN', nrow(new_new_dbscan))
new_new_dbscan$order <- rep(5, nrow(new_new_dbscan))

new_new_regime = list.files(paste(output_directory, "old//new_physics_new_priors//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
new_new_regime <- ldply(new_new_regime, read_csv)
new_new_regime$river <- phase_files
colnames(new_new_regime) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
new_new_regime$Type <- rep('New_new_regime', nrow(new_new_regime))
new_new_regime$order <- rep(5, nrow(new_new_regime))
new_new_regime <- filter(new_new_regime, river %in% files)

varyingN = list.files(paste(output_directory, "old//new_physics_new_priors_varyingN//", sep=''), pattern="*.csv", full.names = TRUE) #switch.y
varyingN <- ldply(varyingN, read_csv)
varyingN$river <- phase_files
colnames(varyingN) <- c('order', 'RRMSE', 'NRMSE', 'NSE', 'rBIAS', 'river')
varyingN$Type <- rep('Time-and-space varying n', nrow(varyingN))
varyingN$order <- rep(5, nrow(varyingN))

bam_stats <- rbind(new_new_regime, varyingN)
#bam_stats <- rbind(mark_switch, new_new_dbscan, new_new_geomorph)
plot <- gather(bam_stats, 'metric', 'value', c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))
plot$Type <- factor(plot$Type, levels = c('New_new_regime', 'Time-and-space varying n'))
#plot$Type <- factor(plot$Type, levels = c('Mark_Switch', 'DBSCAN', 'new_new_Geomorph'))

plot$metric <- factor(plot$metric, levels=c('RRMSE', 'NRMSE', 'NSE', 'rBIAS'))

#just pepsi 1
pepsi1 <- phase_files[c(2,3,4,7, 9,10,14, 23, 24, 25, 26, 27, 30, 31, 32, 33, 34)]
plot <- filter(plot, river %in% pepsi1)
#plot <- filter(plot, metric == 'NSE')

stats <- group_by(plot, Type, metric) %>% dplyr::summarize(mean = mean(value), median = median(value), IQR = IQR(value))

not_plottedNSE <- plot %>% group_by(Type, metric) %>%
  filter(value < -2.15) %>%
  summarise(num_outliers = n())

not_plottedOthers <- plot %>% group_by(Type, metric) %>%
  filter(value > 1.4) %>%
  summarise(num_outliers = n())

boxplots <- ggplot(plot, aes(x=metric, y = value, fill=Type)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(-2, 1.25))+
  geom_text(data=not_plottedNSE,aes(y=-2*0.99,label=num_outliers), size=5,vjust=1.5,hjust=-0.5, position = position_dodge2(width=0.8)) +
  geom_text(data=not_plottedOthers,aes(y=1.25*0.99,label=num_outliers), size=5,vjust=1.5,hjust=-0.5, position = position_dodge2(width = 0.8)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_hline(yintercept=0.5, linetype='dashed') +
  geom_hline(yintercept=1, linetype='dashed') +
  ylab('') +
  xlab('%') +
  theme(legend.position= c(0.01, 0.27)) + 
  scale_fill_manual(name = "", 
                    labels = c("Default/Control", "New AMHG, Old Data", 'Old AMHG, New Data', 'New AMHG, New Data', 'Unsupervised', 'Expert'), 
                    values=c('#6baed6', '#bdbdbd', '#969696', '#737373', '#2171b5', '#08519c'))
boxplots
ggsave('C:\\Users\\craig\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\boxplots.tiff', boxplots, dpi = 400, width = 6.5, height = 6.5)


#-------------------------------------------------Scraps
#
#Scraps

plotHydrographs <- plot_grid(pltList$hydrograph_GaronneUpstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(labels=scaleFUN) + ggtitle('Garonne Upstream'),
                             pltList$hydrograph_Seine.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(labels=scaleFUN) + ggtitle('Seine'),
                             pltList$hydrograph_SacramentoUpstream.csv + theme(legend.position='none', axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(labels=scaleFUN) + ggtitle('Sacramento Upstream'),
                             NULL,
                             pltList$hydrograph_GaronneUpstream.csv + theme(legend.position='none') + scale_y_continuous(labels=scaleFUN),
                             pltList$hydrograph_Seine.csv + theme(legend.position='none') + scale_y_continuous(labels=scaleFUN),
                             pltList$hydrograph_SacramentoUpstream.csv + theme(legend.position='none') + scale_y_continuous(labels=scaleFUN),
                             NULL,
                             ncol=4)

legend <- get_legend(
  # create some space to the left of the legend
  pltList$hydrograph_GaronneUpstream.csv + theme(legend.text=element_text(size=17))
)
legend2 <- get_legend(
  # create some space to the left of the legend
  pltList$hydrograph_GaronneUpstream.csv + theme(legend.text=element_text(size=17))
)

text <- c('x better', 'x similar', 'x worse', 'y better', 'y similar', 'y worse')

plotHydrographs <- plotHydrographs + 
  draw_grob(legend, 0.75, 0.25, 0.25, 1) + 
  draw_grob(legend2, 0.75, -0.25, 0.25, 1) + 
  draw_text(text, x = c(0.15, 0.43, 0.7, 0.15, 0.43, 0.7), y = c(0.9, 0.9, 0.9, 0.45, 0.45, 0.45), hjust = 0)

yTitleCombo <- textGrob(expression(Discharge/(Mean~Observed~Discharge)), gp=gpar(fontface="bold", col="black", fontsize=18), rot=90)
xTitleCombo <- textGrob(expression(Timestep/Max~Timestep), gp=gpar(fontface="bold", col="black", fontsize=18))

plotHydrographs <- grid.arrange(arrangeGrob(plotHydrographs, left = yTitleCombo, bottom = xTitleCombo))
#ggsave('C:\\Users\\cbrinkerhoff\\Box Sync\\Ongoing Projects\\geomorph_class\\Figures\\paper_figures\\hydrographs_three.pdf', plotHydrographs, width = 14, height = 7)






