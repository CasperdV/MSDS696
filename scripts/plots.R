#Load the required libraries and clear the environment
library(readr)
library(dplyr)
library(parallel)
library(ggplot2)
library(ggrepel)
library(ggalluvial)
library(reshape2)
rm(list=ls())

#Create custom functions
get.data <- function(file,delimiter,time_variable,id_variable,periods) {
  'Load an ordered subset of raw data'
  data <- data.frame(read_delim(file,delimiter,col_types = cols(),progress = FALSE))
  data <- data[data[,time_variable] %in% periods,]
  data <- data[order(data[,id_variable],data[,time_variable]),]
  rownames(data) <- NULL
  return(data)
}

eda.canvas <- function(data,xvar,yvar,gvar,agg,xlims,ylims,xbreaks) {
  'Create a ggplot canvas with the default plotting theme and colors.
  Also aggregate the data for easier plotting.'
  colors <- c('#2d6077','#82c7b7','#b84840','#f5b66e')
  data <- aggregate(list(y = data[,yvar]),list(x = data[,xvar],g = data[,gvar]),agg)
  canvas <- ggplot(data) +
    coord_cartesian(xlim = xlims,ylim = ylims,expand = FALSE,clip = 'off') +
    scale_x_continuous(breaks = xbreaks,labels = scales::comma) +
    scale_color_manual(values = colors) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key.height = unit(0.1,'cm'),
      legend.key.width = unit(0.25,'cm'),
      legend.title = element_blank(),
      legend.text = element_text(color = 'grey40', size = 8),
      legend.position = 'none',
      plot.background = element_blank(),
      plot.title = element_text(face = 'bold', color = 'grey20', size = 15),
      plot.subtitle = element_text(color = 'grey40', size = 8),
      axis.line.x = element_line(colour = 'grey70', size = 0.1),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(colour = 'grey50'),
      axis.text.y = element_blank(),
      plot.margin=unit(c(1.9,2,0.6,3.2),'cm'))
  return(canvas)
}

eda.lines <- function(canvas,set.data=NULL,xvar=NULL,yvar=NULL,gvar=NULL,agg=NULL,size=NULL,linetype=NULL) {
  'Add lines to the ggplot canvas'
  if (is.null(set.data)) {
    canvas <- canvas + geom_line(aes(x = x,y = y,color = g,size = g,linetype = g))
  } else {
    set.data <- aggregate(list(y = set.data[,yvar]),list(x = set.data[,xvar],g = set.data[,gvar]),agg)
    canvas <- canvas + geom_line(data = set.data, aes(x = x,y = y,color = g,size = g,linetype = g))
  }
  return(canvas)
}

eda.points <- function(canvas,xpoints,set.data=NULL,xvar=NULL,yvar=NULL,gvar=NULL,agg=NULL) {
  'Add points to the ggplot canvas'
  if (is.null(set.data)) {
    data.points <- canvas$data[canvas$data$x %in% xpoints,]
  } else {
    set.data <- aggregate(list(y = set.data[,yvar]),list(x = set.data[,xvar],g = set.data[,gvar]),agg)
    data.points <- set.data[set.data$x %in% xpoints,]
  }
  canvas <- canvas + geom_point(data = data.points,aes(x = data.points$x,y = data.points$y,color = data.points$g),size = 2)
  return(canvas)
}

eda.labels <- function(canvas,gvar,nudge = 0,set.data=NULL,xvar=NULL,yvar=NULL,agg=NULL,label=gvar) {
  'Add labels to the start and end of the lines in the ggplot canvas'
  if (is.null(set.data)) {
    data.labels.min <- canvas$data[canvas$data$x == min(canvas$coordinates$limits$x),]
    data.labels.max <- canvas$data[canvas$data$x == max(canvas$coordinates$limits$x),]
  } else {
    set.data <- aggregate(list(y = set.data[,yvar]),list(x = set.data[,xvar],g = set.data[,gvar]),agg)
    data.labels.min <- set.data[set.data$x == min(canvas$coordinates$limits$x),]
    data.labels.max <- set.data[set.data$x == max(canvas$coordinates$limits$x),]
  }
  canvas <- canvas +
    geom_text(data = data.labels.min,aes(x = data.labels.min$x - 1 - nudge,y = data.labels.min$y,color = data.labels.min$g,label = paste0(label,' ',data.labels.min$g,': ',format(round(data.labels.min$y,2),big.mark = ','))),hjust = 1,vjust = 0.5,size = 3,fontface = 'bold') +
    geom_text(data = data.labels.max,aes(x = data.labels.max$x + 1 + nudge,y = data.labels.max$y,color = data.labels.max$g,label = format(round(data.labels.max$y,2),big.mark = ',')),hjust = 0,vjust = 0.5,size = 3,fontface = 'bold')
  return(canvas)
}

eda.titles <- function(canvas,title,subtitle,xtitle,nudge = 0) {
  'Add main,sub and x-axis titles to the ggplot canvas'
  xmin <- min(canvas$coordinates$limits$x)
  xmax <- max(canvas$coordinates$limits$x)
  ymin <- min(canvas$coordinates$limits$y)
  ymax <- max(canvas$coordinates$limits$y)
  canvas <- canvas + 
    geom_text(aes(x = xmin - 0.18 * (xmax - xmin) + nudge,y = ymax + 0.192 * (ymax - ymin),label = title),fontface = 'bold',color = 'grey20',size = 5,hjust = 0) +
    geom_text(aes(x = xmin - 0.18 * (xmax - xmin) + nudge,y = ymax + 0.13 * (ymax - ymin),label = subtitle),color = 'grey50',size = 3,hjust = 0,vjust = 1) +
    geom_text(aes(x = xmin - 0.005 * (xmax - xmin),y = ymin - 0.092 * (ymax - ymin),label = xtitle),color = 'grey50',size = 3,hjust = 0)
  return(canvas)
}

#Set the parameters
data.file <- 'data/beneficiaries.txt'                                       # path to data file
data.delimiter <- '|'                                                       # column delimiter used in data file
beneficiary.id <- 'Beneficiary'                                             # column name of the column containing the beneficiary id
time <- 'Period'                                                            # column name of the column containing the time variable
states <- c('Plan','Income','Type','Chronic')                               # column name of the column containing the current membership state
categorical.predictors <- c('Gender')                                       # all variables that should be one-hot encoded (excluding state variables - these are added automatically)
continuous.predictors <- c('Age','Duration','Adults','Children')            # all variables that should be scaled and centered
ageing.variables <- c('Age','Duration')                                     # variables that increase with time (deterministic)
training.periods <- 1:36                                                    # periods for which data is "known" and used for training
prediction.periods <- 37:48                                                 # months for which data is "unknown" and not used for training
base.period <- 36                                                           # last "known" period from which predictions will be made

#Load the raw beneficiary data
data <- get.data(data.file,data.delimiter,time,beneficiary.id,min(training.periods):max(prediction.periods))
data <- data[c(time,beneficiary.id,categorical.predictors,continuous.predictors,states)]
data$Exposure <- 1
data[c(categorical.predictors,states)] <- mclapply(data[c(categorical.predictors,states)],factor)
data <- cbind(data,data.frame(mclapply(ageing.variables,function(x) {data[paste0('Int.',x)] <- floor(data[x]);return(data[paste0('Int.',x)])})))

#Plot beneficiaries by plan
(eda.canvas(data,'Period','Exposure','Plan','sum',c(1,48),c(0,130000),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.labels('Plan') %>% 
    eda.titles(
      'Membership by plan',
      'Net membership growth per period is 0.8% for Plan 1, 1.4% for Plan 2 and 0.3% for Plan 3. Growth spikes in the first\nperiod of each year for Plan 1 and Plan 2, but decreases for Plan 3.',
      'Period')) %>%
  ggsave('images/eda1.png',.,'png',width=7.1,height=4)

#Plot beneficiaries by income
(eda.canvas(data[data$Plan == 3,],'Period','Exposure','Income','sum',c(1,48),c(0,9000),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.labels('Income') %>% 
    eda.titles(
      'Membership by income',
      'The number of beneficiaries in income bands 2 and 3 decrease in the first period of each year followed by an increase\nin the sixth period. The number of beneficiaries in income band 1 increase in the first period followed by a decrease\nin period six.',
      'Period',
      -0.85)) %>%
  ggsave('images/eda2.png',.,'png',width=7.1,height=4)

#Plot beneficiaries by type
(eda.canvas(data,'Period','Exposure','Type','sum',c(1,48),c(0,90000),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.labels('Type') %>% 
    eda.titles(
      'Membership by type',
      'The number of beneficiaries increases by between 0.83% and 0.94% per period. Type 2 beneficiaries have the highest\ngrowth rate and type 3 beneficiaries have the lowest growth rate.',
      'Period',
      -0.2)) %>%
  ggsave('images/eda3.png',.,'png',width=7.1,height=4)

#Plot beneficiaries by chronic status
(eda.canvas(data,'Period','Exposure','Chronic','sum',c(1,48),c(0,180000),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.labels('Chronic') %>% 
    eda.titles(
      'Membership by chronic status',
      'The number of beneficiaries with a chronic condition (chronic status 2) increased by 75% from period 1 to period 48,\nbut the number of non-chronic beneficiaries increased by only 47%. A large number of non-chronic beneficiaries joins\nthe scheme at the beginning of each year.',
      'Period',
      -2.35)) %>%
  ggsave('images/eda4.png',.,'png',width=7.1,height=4)

#Plot avg age by plan
(eda.canvas(data,'Period','Age','Plan','mean',c(1,48),c(26.5,29),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.labels('Plan') %>% 
    eda.titles(
      'Average age by plan',
      'The average age of principal members (type 1) has remained relatively constant from period 1 to 48. The average age\nof adult dependants (type 2) decreases consistently while that of child dependants decreased initially followed by an\nincrease from period 12 to 48.',
      'Period',
      0.8)) %>%
  ggsave('images/eda5.png',.,'png',width=7.1,height=4)

#Plot avg age by income
(eda.canvas(data[data$Plan == 3,],'Period','Age','Income','mean',c(1,48),c(26,30),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.labels('Income') %>% 
    eda.titles(
      'Average age by income',
      'The average beneficiary age by income band is eratic but tends to increase or decrease in response to an increase or\ndecrease in membership. The average age in bads 1 and 3 tend to increase in the middle of the year while the average\nage in band 2 tends to increase at the start of the year.',
      'Period',
      -0.75)) %>%
  ggsave('images/eda6.png',.,'png',width=7.1,height=4)

#Plot avg age by type
data.labels.right <- aggregate(list(y = data[data$Period == 48,'Age']),list(x = data[data$Period == 48,'Period'],g = data[data$Period == 48,'Type']),mean)
data.labels.left <- aggregate(list(y = data[data$Period == 1,'Age']),list(x = data[data$Period == 1,'Period'],g = data[data$Period == 1,'Type']),mean)
(eda.canvas(data,'Period','Age','Type','mean',c(1,48),c(0,50),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.titles(
      'Average age by type',
      'The average age by beneficiary type has remained relatively constant from period 1 to 48. There was an unusual\ndecrease in the average age of type 2 and type 3 beneficiaries in period 14. This is likely due to a change in scheme\nrules which resulted in older child dependants being reclassified as adult dependants.',
      'Period',
      0.7) +
    geom_text(data = data.labels.left,aes(x = data.labels.left$x - 1,y = data.labels.left$y + ifelse(data.labels.right$g == 1,-0.5,0) + ifelse(data.labels.right$g == 2,0.5,0),color = data.labels.left$g,label = paste0('Type ',data.labels.left$g,': ',format(round(data.labels.left$y,2),big.mark = ','))),hjust = 1,vjust = 0.5,size = 3,fontface = 'bold') +
    geom_text(data = data.labels.right,aes(x = data.labels.right$x + 1,y = data.labels.right$y + ifelse(data.labels.right$g == 1,1,0) + ifelse(data.labels.right$g == 2,-1,0),color = data.labels.right$g,label = format(round(data.labels.right$y,2),big.mark = ',')),hjust = 0,vjust = 0.5,size = 3,fontface = 'bold')) %>%
  ggsave('images/eda7.png',.,'png',width=7.1,height=4)
rm(data.labels.right,data.labels.left)

#Plot avg age by chronic status
(eda.canvas(data,'Period','Age','Chronic','mean',c(1,48),c(20,55),c(1,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36,48)) %>% 
    eda.labels('Chronic') %>% 
    eda.titles(
      'Average age by chronic status',
      'The average age of chronic beneficiaries (chronic status 2) is significantly higher than the average age of non-chronics.\nHowever, the decreasing trend in the average age of chronic beneficiaries indicates that first-time diagnosis of\nchronic conditions is happening at younger ages each year.',
      'Period',
      -1.1)) %>%
  ggsave('images/eda8.png',.,'png',width=7.1,height=4)

#Extract data for new beneficiaries
data.new <- data[is.na(left_join(data,data.frame(data['Beneficiary'],data['Period'] + 1,Indicator = 'Remove'),by = c("Period", "Beneficiary"))$Indicator),]
data.new <- data.new[data.new$Period != 1,]

#Plot new beneficiaries by plan
(eda.canvas(data.new,'Period','Exposure','Plan','sum',c(2,48),c(0,12000),c(2,12,24,36,48)) %>% 
    eda.lines() %>% 
    eda.points(c(12,24,36,48)) +
    scale_y_continuous(breaks = seq(0,12000,2000),labels = scales::comma) +
    geom_text(aes(x = -1.4,y = 14800,label = 'New entrants by plan'),fontface = 'bold',color = 'grey20',size = 5,hjust = 0) +
    geom_text(aes(x = 1.8,y = -1100,label = 'Period'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = -1.4,y = 12800,label = 'Number of lives'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=3)) +
    scale_color_manual(values = c('#2d6077','#82c7b7','#b84840'), labels = c('Plan 1','Plan 2','Plan 3')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(2.2,1.5,0.6,1.5),'cm'),
      legend.position = c(0.074,1.175))) %>%
  ggsave('images/eda9.png',.,'png',width=7.1,height=4)

#Plot average age of new beneficiaries by plan
(eda.canvas(data.new,'Period','Age','Plan','mean',c(2,48),c(17,28),c(2,12,24,36,48)) %>% 
    eda.lines() %>%
    eda.points(c(1)) +
    scale_y_continuous(breaks = seq(17,28,1),labels = scales::comma) +
    geom_text(aes(x = 0.6,y = 30.5,label = 'Avg age of new entrants by plan'),fontface = 'bold',color = 'grey20',size = 5,hjust = 0) +
    geom_text(aes(x = 1.8,y = 16,label = 'Period'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = 0.7,y = 28.7,label = 'Age'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=3)) +
    scale_color_manual(values = c('#2d6077','#82c7b7','#b84840'), labels = c('Plan 1','Plan 2','Plan 3')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(2.2,1.5,0.6,1.5),'cm'),
      legend.position = c(0.11,1.16))) %>%
  ggsave('images/eda10.png',.,'png',width=7.1,height=4)
rm(data.new)

#Extract data for new beneficiaries
data.exit <- data[is.na(left_join(data,data.frame(data['Beneficiary'],data['Period'] - 1,Indicator = 'Remove'),by = c("Period", "Beneficiary"))$Indicator),]
data.exit <- data.exit[data.exit$Period != 48,]

#Plot resignations by plan
(eda.canvas(data.exit,'Period','Exposure','Plan','sum',c(1,47),c(0,3000),c(1,12,24,36,47)) %>% 
    eda.lines() %>% 
    eda.points(c(1,12,24,36)) +
    scale_y_continuous(breaks = seq(0,3000,500),labels = scales::comma) +
    geom_text(aes(x = -1.85,y = 3700,label = 'Resignations by plan'),fontface = 'bold',color = 'grey20',size = 5,hjust = 0) +
    geom_text(aes(x = 0.8,y = -280,label = 'Period'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = -1.85,y = 3200,label = 'Number of exits'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=3)) +
    scale_color_manual(values = c('#2d6077','#82c7b7','#b84840'), labels = c('Plan 1','Plan 2','Plan 3')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(2.2,1.5,0.6,1.5),'cm'),
      legend.position = c(0.085,1.165))) %>%
  ggsave('images/eda11.png',.,'png',width=7.1,height=4)

#Plot average age of resignations by plan
(eda.canvas(data.exit,'Period','Age','Plan','mean',c(1,47),c(24,34),c(1,12,24,36,47)) %>% 
    eda.lines() %>%
    eda.points(c(48)) +
    scale_y_continuous(breaks = seq(24,34,1),labels = scales::comma) +
    geom_text(aes(x = -0.3,y = 36.4,label = 'Avg age of resignations by plan'),fontface = 'bold',color = 'grey20',size = 5,hjust = 0) +
    geom_text(aes(x = 0.8,y = 23,label = 'Period'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = -0.3,y = 34.7,label = 'Age'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=3)) +
    scale_color_manual(values = c('#2d6077','#82c7b7','#b84840'), labels = c('Plan 1','Plan 2','Plan 3')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(2.2,1.5,0.6,1.5),'cm'),
      legend.position = c(0.11,1.18))) %>%
  ggsave('images/eda12.png',.,'png',width=7.1,height=4)
rm(data.exit)

#Plot option changes in period 25
data.move <- data[(data$Type == 1) & (data$Period %in% c(24,25)),c('Period','Beneficiary','Plan','Exposure')]
data.move$Period <- as.factor(data.move$Period)
data.move <- dcast(data.move,Beneficiary~Period,value.var = 'Plan',fill=0)
data.move <- data.move[!is.na(data.move$`24`) & !is.na(data.move$`25`) & (data.move$`24` != data.move$`25`),]
paths <- unique(data.move[,-1])
rownames(paths) <- NULL
paths$Path <- rownames(paths)
data.move <- left_join(data.move,paths)
data.move <- melt(data.move,id = c('Beneficiary','Path'))
rm(paths)
names(data.move) <- c('Beneficiary','Path','Period','Plan')
data.move$Plan <- as.factor(data.move$Plan)
levels(data.move$Plan) <- c('Plan 1','Plan 2','Plan 3')
levels(data.move$Period) <- c('Period 24','Period 25')
data.move <- aggregate(data.move['Beneficiary'],by=list(Period = data.move$Period,Plan = data.move$Plan,Path = data.move$Path),FUN=length)
names(data.move) <- c('Period','Plan','Path','Exposure')

(ggplot(data.move,aes(x = Period, y = Exposure, stratum = Plan, alluvium = Path,fill = Plan)) +
  coord_cartesian(ylim=c(0,1600),clip = 'off') +
  scale_x_discrete(name='Period',breaks=c('Period 24','Period 25'),expand=c(0.09,0.09)) +
  scale_y_continuous(name='Exposure',breaks=seq(0,1600,100),expand=c(0,0),labels = scales::comma) +
  scale_fill_manual(values = c('#b84840','#82c7b7','#2d6077')) +
  geom_flow(alpha = 0.7, width = 0.2) +
  geom_stratum(size = 0,width = 0.2, alpha = 0.8,color='grey50') +
  geom_text(stat = "stratum", label.strata = TRUE,fontface = 'bold',color = 'white',size = 3.5) +
  labs(title='Plan changes in period 25',
       subtitle='Families') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour = 'grey70', size = 0.1),
    axis.line.y = element_line(colour = 'grey70', size = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
    axis.text.x = element_text(colour = 'grey50',face='bold',size=9),
    axis.text.y = element_text(colour = 'grey50'),
    plot.title = element_text(face = 'bold', color = 'grey20', size = 15, hjust = -0.14, vjust = 7),
    plot.subtitle = element_text(face='bold',color = 'grey50', size = 9, hjust = -0.076,vjust = 2.8),
    legend.position = 'off',
    plot.margin=unit(c(1.2,2,0.6,1.5),'cm'))) %>%
  ggsave('images/eda13.png',.,'png',width=7.1,height=4)
rm(data.move)

#Plot option changes in period 37
data.move <- data[(data$Type == 1) & (data$Period %in% c(36,37)),c('Period','Beneficiary','Plan','Exposure')]
data.move$Period <- as.factor(data.move$Period)
data.move <- dcast(data.move,Beneficiary~Period,value.var = 'Plan',fill=0)
data.move <- data.move[!is.na(data.move$`36`) & !is.na(data.move$`37`) & (data.move$`36` != data.move$`37`),]
paths <- unique(data.move[,-1])
rownames(paths) <- NULL
paths$Path <- rownames(paths)
data.move <- left_join(data.move,paths)
data.move <- melt(data.move,id = c('Beneficiary','Path'))
rm(paths)
names(data.move) <- c('Beneficiary','Path','Period','Plan')
data.move$Plan <- as.factor(data.move$Plan)
levels(data.move$Plan) <- c('Plan 1','Plan 2','Plan 3')
levels(data.move$Period) <- c('Period 36','Period 37')
data.move <- aggregate(data.move['Beneficiary'],by=list(Period = data.move$Period,Plan = data.move$Plan,Path = data.move$Path),FUN=length)
names(data.move) <- c('Period','Plan','Path','Exposure')

(ggplot(data.move,aes(x = Period, y = Exposure, stratum = Plan, alluvium = Path,fill = Plan)) +
    coord_cartesian(ylim=c(0,1600),clip = 'off') +
    scale_x_discrete(name='Period',breaks=c('Period 36','Period 37'),expand=c(0.09,0.09)) +
    scale_y_continuous(name='Exposure',breaks=seq(0,1600,100),expand=c(0,0),labels = scales::comma) +
    scale_fill_manual(values = c('#b84840','#82c7b7','#2d6077')) +
    geom_flow(alpha = 0.7, width = 0.2) +
    geom_stratum(size = 0,width = 0.2, alpha = 0.8,color='grey50') +
    geom_text(stat = "stratum", label.strata = TRUE,fontface = 'bold',color = 'white',size = 3.5) +
    labs(title='Plan changes in period 37',
         subtitle='Families') +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(colour = 'grey70', size = 0.1),
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.x = element_text(colour = 'grey50',face='bold',size=9),
      axis.text.y = element_text(colour = 'grey50'),
      plot.title = element_text(face = 'bold', color = 'grey20', size = 15, hjust = -0.14, vjust = 7),
      plot.subtitle = element_text(face='bold',color = 'grey50', size = 9, hjust = -0.076,vjust = 2.8),
      legend.position = 'off',
      plot.margin=unit(c(1.2,2,0.6,1.5),'cm'))) %>%
  ggsave('images/eda14.png',.,'png',width=7.1,height=4)
rm(data.move)

#Plot beneficiaries by age in period 12,24,36,48
data.age <- data[data$Period %in% seq(12,48,12),]
data.age$Period <- as.factor(data.age$Period)
(eda.canvas(data.age,'Int.Age','Exposure','Period','sum',c(0,100),c(0,5000),seq(0,100,5)) %>% 
    eda.lines() %>% 
    eda.points(c(110)) +
    scale_y_continuous(breaks = seq(0,5000,1000),labels = scales::comma) +
    geom_text(aes(x = -6,y = 6200,label = 'Beneficiaries by age'),fontface = 'bold',color = 'grey20',size = 5,hjust = 0) +
    geom_text(aes(x = -0.5,y = -500,label = 'Age'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = -6,y = 5300,label = 'Beneficiaries'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=4)) +
    scale_color_manual(values = c('#2d6077','#82c7b7','#b84840','#f5b66e'), labels = c('Year 1','Year 2','Year 3','Year 4')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(2.2,1.5,0.6,1),'cm'),
      legend.position = c(0.135,1.18))) %>%
  ggsave('images/eda15.png',.,'png',width=7.1,height=4)
rm(data.age)

#Plot beneficiaries by duration in period 12,24,36,48
data.duration <- data[data$Period %in% seq(12,48,12),]
data.duration$Period <- as.factor(data.duration$Period)
(eda.canvas(data.duration,'Int.Duration','Exposure','Period','sum',c(0,13),c(0,35000),seq(0,13,1)) %>% 
    eda.lines() %>% 
    eda.points(c(14)) +
    scale_y_continuous(breaks = seq(0,35000,5000),labels = scales::comma) +
    geom_text(aes(x = -0.9,y = 43500,label = 'Beneficiaries by duration'),fontface = 'bold',color = 'grey20',size = 5,hjust = 0) +
    geom_text(aes(x = -0.05,y = -3500,label = 'Duration'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = -0.9,y = 37000,label = 'Beneficiaries'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=4)) +
    scale_color_manual(values = c('#2d6077','#82c7b7','#b84840','#f5b66e'), labels = c('Year 1','Year 2','Year 3','Year 4')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(2.2,1.5,0.6,1),'cm'),
      legend.position = c(0.125,1.18))) %>%
  ggsave('images/eda16.png',.,'png',width=7.1,height=4)
rm(data.duration)

rm(data)

#Load the multi-class classifier training history
history <- read_delim("data/history_beneficiaries.txt", "|", escape_double = FALSE, trim_ws = TRUE)
history$epoch <- as.numeric(rownames(history))
history <- rbind(
  data.frame(Epoch = history$epoch,Set = 'Train',Metric = 'Loss',Value = history$loss),
  data.frame(Epoch = history$epoch,Set = 'Train',Metric = 'Accuracy',Value = history$acc),
  data.frame(Epoch = history$epoch,Set = 'Validate',Metric = 'Loss',Value = history$val_loss),
  data.frame(Epoch = history$epoch,Set = 'Validate',Metric = 'Accuracy',Value = history$val_acc))

#Plot the MCC training metrics
(eda.canvas(history[history$Metric == 'Loss',],'Epoch','Value','Set','sum',c(1,147),c(0.34,0.45),c(1,seq(10,140,10),147)) %>% 
    eda.lines() %>% 
    eda.points(c(1,147)) %>% 
    eda.labels('',2) %>% 
    eda.titles(
      'Training and validation loss',
      '',
      'Epoch',
      3) +
    theme(plot.margin=unit(c(2.2,1.5,0.6,2.5),'cm'))) %>%
  ggsave('images/mcc1.png',.,'png',width=7.1,height=4,bg = "transparent")

#Plot the MCC training metrics
(eda.canvas(history[history$Metric == 'Accuracy',],'Epoch','Value','Set','sum',c(1,147),c(0.885,0.91),c(1,seq(10,140,10),147)) %>% 
    eda.lines() %>% 
    eda.points(c(1,147)) %>% 
    eda.titles(
      'Training and validation accuracy',
      '',
      'Epoch',
      3) +
    theme(plot.margin=unit(c(2.2,1.5,0.6,2.5),'cm')) +
    geom_text(data = history[(history$Metric == 'Accuracy') & (history$Epoch == 1),],aes(x = Epoch - 3,y = Value,color = Set,label = paste0(Set,': ',format(round(Value,2),big.mark = ','))),hjust = 1,vjust = 0.5,size = 3,fontface = 'bold') +
    geom_text(data = history[(history$Metric == 'Accuracy') & (history$Epoch == 147),],aes(x = Epoch + 3,y = Value + ifelse(Set == 'Train',0.0005,-0.0005),color = Set,label = format(round(Value,2),big.mark = ',')),hjust = 0,vjust = 0.5,size = 3,fontface = 'bold')) %>%
  ggsave('images/mcc2.png',.,'png',width=7.1,height=4,bg = "transparent")
rm(history)

#Load the multi-class classifier prediction summary
data <- read_delim('data/summary_beneficiaries.txt',delim='|',escape_double = FALSE,trim_ws = TRUE)
data <- data.frame(data[(data$Plan != 0) & (data$Period >= 36),])
data$Output <- as.factor(data$Output)

#Plot actual vs predicted membership by plan over time
mape <- dcast(aggregate(data[data$Period != 36,'Exposure'],by=list(Period = data[data$Period != 36,]$Period,Plan = data[data$Period != 36,]$Plan,Output = data[data$Period != 36,]$Output),FUN=sum),Period+Plan~Output,value.var='x')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Plan=mape$Plan),FUN=sum),aggregate(mape['Period'],by=list(Plan=mape$Plan),FUN=length)['Period'])
mape <- data.frame(mape['Plan'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data,'Period','Exposure','Output','sum',c(36,48),c(0,120000),36:48) %>% 
    eda.lines(data[data$Plan == 1,],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.lines(data[data$Plan == 2,],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.lines(data[data$Plan == 3,],'Period','Exposure','Output','sum','Output','Output') %>%
    eda.points(c(36,48),data[(data$Plan == 1) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.points(c(36,48),data[(data$Plan == 2) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.points(c(36,48),data[(data$Plan == 3) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>%
    eda.labels('Output',-0.8,data[(data$Plan == 1) & (data$Output == 'Actual'),],'Period','Exposure','sum','Plan 1') %>%
    eda.labels('Output',-0.8,data[(data$Plan == 2) & (data$Output == 'Actual'),],'Period','Exposure','sum','Plan 2') %>%
    eda.labels('Output',-0.8,data[(data$Plan == 3) & (data$Output == 'Actual'),],'Period','Exposure','sum','Plan 3') %>%
    eda.titles(
      'Membership by plan',
      paste('The MAPE is',mape$mape[1],'for plan 1,',mape$mape[2],'for plan 2 and',mape$mape[3],'for plan 3'),
      'Period',-1.2) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,3.7),'cm'),
      legend.position = c(-0.15,1.065))) %>%
  ggsave('images/mcc3.png',.,'png',width=7.1,height=4)
rm(mape)

#Plot actual vs predicted membership by income over time
mape <- dcast(aggregate(data[data$Period != 36,'Exposure'],by=list(Period = data[data$Period != 36,]$Period,Income = data[data$Period != 36,]$Income,Output = data[data$Period != 36,]$Output),FUN=sum),Period+Income~Output,value.var='x')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Income=mape$Income),FUN=sum),aggregate(mape['Period'],by=list(Income=mape$Income),FUN=length)['Period'])
mape <- data.frame(mape['Income'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data,'Period','Exposure','Output','sum',c(36,48),c(0,8000),36:48) %>% 
    eda.lines(data[(data$Plan == 3) & (data$Income == 1),],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.lines(data[(data$Plan == 3) & (data$Income == 2),],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.lines(data[(data$Plan == 3) & (data$Income == 3),],'Period','Exposure','Output','sum','Output','Output') %>%
    eda.points(c(36,48),data[(data$Plan == 3) & (data$Income == 1) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.points(c(36,48),data[(data$Plan == 3) & (data$Income == 2) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.points(c(36,48),data[(data$Plan == 3) & (data$Income == 3) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>%
    eda.labels('Output',-0.8,data[(data$Plan == 3) & (data$Income == 1) & (data$Output == 'Actual'),],'Period','Exposure','sum','Income 1') %>%
    eda.labels('Output',-0.8,data[(data$Plan == 3) & (data$Income == 2) & (data$Output == 'Actual'),],'Period','Exposure','sum','Income 2') %>%
    eda.labels('Output',-0.8,data[(data$Plan == 3) & (data$Income == 3) & (data$Output == 'Actual'),],'Period','Exposure','sum','Income 3') %>%
    eda.titles(
      'Membership by income',
      paste('The MAPE is',mape$mape[1],'for income 1,',mape$mape[2],'for income 2 and',mape$mape[3],'for income 3'),
      'Period',
      -1.2) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,3.7),'cm'),
      legend.position = c(-0.15,1.065))) %>%
  ggsave('images/mcc4.png',.,'png',width=7.1,height=4)
rm(mape)

#Plot actual vs predicted membership by type over time
mape <- dcast(aggregate(data[data$Period != 36,'Exposure'],by=list(Period = data[data$Period != 36,]$Period,Type = data[data$Period != 36,]$Type,Output = data[data$Period != 36,]$Output),FUN=sum),Period+Type~Output,value.var='x')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Type=mape$Type),FUN=sum),aggregate(mape['Period'],by=list(Type=mape$Type),FUN=length)['Period'])
mape <- data.frame(mape['Type'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data,'Period','Exposure','Output','sum',c(36,48),c(0,70000),36:48) %>% 
    eda.lines(data[(data$Type == 1),],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.lines(data[(data$Type == 2),],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.lines(data[(data$Type == 3),],'Period','Exposure','Output','sum','Output','Output') %>%
    eda.points(c(36,48),data[(data$Type == 1) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.points(c(36,48),data[(data$Type == 2) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.points(c(36,48),data[(data$Type == 3) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>%
    eda.labels('Output',-0.8,data[(data$Type == 1) & (data$Output == 'Actual'),],'Period','Exposure','sum','Type 1') %>%
    eda.labels('Output',-0.8,data[(data$Type == 2) & (data$Output == 'Actual'),],'Period','Exposure','sum','Type 2') %>%
    eda.labels('Output',-0.8,data[(data$Type == 3) & (data$Output == 'Actual'),],'Period','Exposure','sum','Type 3') %>%
    eda.titles(
      'Membership by type',
      paste('The MAPE is',mape$mape[1],'for type 1,',mape$mape[2],'for type 2 and',mape$mape[3],'for type 3'),
      'Period',
      -1) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,3.7),'cm'),
      legend.position = c(-0.135,1.065))) %>%
  ggsave('images/mcc5.png',.,'png',width=7.1,height=4)
rm(mape)

#Plot actual vs predicted membership by chronic status over time
mape <- dcast(aggregate(data[data$Period != 36,'Exposure'],by=list(Period = data[data$Period != 36,]$Period,Chronic = data[data$Period != 36,]$Chronic,Output = data[data$Period != 36,]$Output),FUN=sum),Period+Chronic~Output,value.var='x')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Chronic=mape$Chronic),FUN=sum),aggregate(mape['Period'],by=list(Chronic=mape$Chronic),FUN=length)['Period'])
mape <- data.frame(mape['Chronic'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data,'Period','Exposure','Output','sum',c(36,48),c(0,150000),36:48) %>% 
    eda.lines(data[(data$Chronic == 1),],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.lines(data[(data$Chronic == 2),],'Period','Exposure','Output','sum','Output','Output') %>% 
    eda.points(c(36,48),data[(data$Chronic == 1) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.points(c(36,48),data[(data$Chronic == 2) & (data$Output == 'Actual'),],'Period','Exposure','Output','sum') %>% 
    eda.labels('Output',-0.8,data[(data$Chronic == 1) & (data$Output == 'Actual'),],'Period','Exposure','sum','Chronic status 1') %>%
    eda.labels('Output',-0.8,data[(data$Chronic == 2) & (data$Output == 'Actual'),],'Period','Exposure','sum','Chronic status 2') %>%
    eda.titles(
      'Membership by chronic status',
      paste('The MAPE is',mape$mape[1],'for chronic status 1 and',mape$mape[2],'for chronic status 2'),
      'Period',
      -3.1) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,5),'cm'),
      legend.position = c(-0.295,1.065))) %>%
  ggsave('images/mcc6.png',.,'png',width=7.1,height=4)
rm(mape)

#Plot actual vs predicted age by plan over time
data.age <- aggregate(data[c('Age','Exposure')],by=list(Period = data$Period,Plan = data$Plan,Output = data$Output),FUN=sum)
data.age$Age <- data.age$Age / data.age$Exposure

mape <- dcast(data.age,Period+Plan~Output,value.var='Age')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Plan=mape$Plan),FUN=sum),aggregate(mape['Period'],by=list(Plan=mape$Plan),FUN=length)['Period'])
mape <- data.frame(mape['Plan'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data.age,'Period','Age','Output','mean',c(36,48),c(25,30),36:48) %>% 
    eda.lines(data.age[data.age$Plan == 1,],'Period','Age','Output','sum','Output','Output') %>% 
    eda.lines(data.age[data.age$Plan == 2,],'Period','Age','Output','sum','Output','Output') %>% 
    eda.lines(data.age[data.age$Plan == 3,],'Period','Age','Output','sum','Output','Output') %>%
    eda.points(c(36,48),data.age[(data.age$Plan == 1) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.points(c(36,48),data.age[(data.age$Plan == 2) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.points(c(36,48),data.age[(data.age$Plan == 3) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Plan == 1) & (data.age$Output == 'Actual'),],'Period','Age','sum','Plan 1') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Plan == 2) & (data.age$Output == 'Actual'),],'Period','Age','sum','Plan 2') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Plan == 3) & (data.age$Output == 'Actual'),],'Period','Age','sum','Plan 3') %>%
    eda.titles(
      'Average age by plan',
      paste('The MAPE is',mape$mape[1],'for plan 1,',mape$mape[2],'for plan 2 and',mape$mape[3],'for plan 3'),
      'Period',-1.2) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,3.7),'cm'),
      legend.position = c(-0.15,1.065))) %>%
  ggsave('images/mcc7.png',.,'png',width=7.1,height=4)
rm(mape)

#Plot actual vs predicted age by income over time
data.age <- aggregate(data[c('Age','Exposure')],by=list(Period = data$Period,Income = data$Income,Output = data$Output,Plan = data$Plan),FUN=sum)
data.age <- data.age[data.age$Plan == 3,]
data.age$Age <- data.age$Age / data.age$Exposure

mape <- dcast(data.age,Period+Income~Output,value.var='Age')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Income=mape$Income),FUN=sum),aggregate(mape['Period'],by=list(Income=mape$Income),FUN=length)['Period'])
mape <- data.frame(mape['Income'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data.age,'Period','Age','Output','mean',c(36,48),c(25,33),36:48) %>% 
    eda.lines(data.age[(data.age$Plan == 3) & (data.age$Income == 1),],'Period','Age','Output','sum','Output','Output') %>% 
    eda.lines(data.age[(data.age$Plan == 3) & (data.age$Income == 2),],'Period','Age','Output','sum','Output','Output') %>% 
    eda.lines(data.age[(data.age$Plan == 3) & (data.age$Income == 3),],'Period','Age','Output','sum','Output','Output') %>%
    eda.points(c(36,48),data.age[(data.age$Plan == 3) & (data.age$Income == 1) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.points(c(36,48),data.age[(data.age$Plan == 3) & (data.age$Income == 2) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.points(c(36,48),data.age[(data.age$Plan == 3) & (data.age$Income == 3) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Plan == 3) & (data.age$Income == 1) & (data.age$Output == 'Actual'),],'Period','Age','sum','Income 1') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Plan == 3) & (data.age$Income == 2) & (data.age$Output == 'Actual'),],'Period','Age','sum','Income 2') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Plan == 3) & (data.age$Income == 3) & (data.age$Output == 'Actual'),],'Period','Age','sum','Income 3') %>%
    eda.titles(
      'Average age by income',
      paste('The MAPE is',mape$mape[1],'for income 1,',mape$mape[2],'for income 2 and',mape$mape[3],'for income 3'),
      'Period',-1.2) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,3.7),'cm'),
      legend.position = c(-0.15,1.065))) %>%
  ggsave('images/mcc8.png',.,'png',width=7.1,height=4)
rm(mape)

#Plot actual vs predicted age by type over time
data.age <- aggregate(data[c('Age','Exposure')],by=list(Period = data$Period,Type = data$Type,Output = data$Output),FUN=sum)
data.age$Age <- data.age$Age / data.age$Exposure

mape <- dcast(data.age,Period+Type~Output,value.var='Age')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Type=mape$Type),FUN=sum),aggregate(mape['Period'],by=list(Type=mape$Type),FUN=length)['Period'])
mape <- data.frame(mape['Type'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data.age,'Period','Age','Output','mean',c(36,48),c(0,50),36:48) %>% 
    eda.lines(data.age[data.age$Type == 1,],'Period','Age','Output','sum','Output','Output') %>% 
    eda.lines(data.age[data.age$Type == 2,],'Period','Age','Output','sum','Output','Output') %>% 
    eda.lines(data.age[data.age$Type == 3,],'Period','Age','Output','sum','Output','Output') %>%
    eda.points(c(36,48),data.age[(data.age$Type == 1) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.points(c(36,48),data.age[(data.age$Type == 2) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.points(c(36,48),data.age[(data.age$Type == 3) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Type == 3) & (data.age$Output == 'Actual'),],'Period','Age','sum','Type 3') %>%
    eda.titles(
      'Average age by type',
      paste('The MAPE is',mape$mape[1],'for type 1,',mape$mape[2],'for type 2 and',mape$mape[3],'for type 3'),
      'Period',-1.2) +
    geom_text(data = data.age[(data.age$Type == 1) & (data.age$Output == 'Actual') & (data.age$Period == 36),],aes(x = 35.8,y = Age + 1,color = Output,label = paste0('Type 1 ',Output,': ',format(round(Age,2),big.mark = ','))),hjust = 1,vjust = 0.5,size = 3,fontface = 'bold') +
    geom_text(data = data.age[(data.age$Type == 2) & (data.age$Output == 'Actual') & (data.age$Period == 36),],aes(x = 35.8,y = Age - 1,color = Output,label = paste0('Type 2 ',Output,': ',format(round(Age,2),big.mark = ','))),hjust = 1,vjust = 0.5,size = 3,fontface = 'bold') +
    geom_text(data = data.age[(data.age$Type == 1) & (data.age$Output == 'Actual') & (data.age$Period == 48),],aes(x = 48.2,y = Age + 0.8,color = Output,label = format(round(Age,2),big.mark = ',')),hjust = 0,vjust = 0.5,size = 3,fontface = 'bold') +
    geom_text(data = data.age[(data.age$Type == 2) & (data.age$Output == 'Actual') & (data.age$Period == 48),],aes(x = 48.2,y = Age - 0.8,color = Output,label = format(round(Age,2),big.mark = ',')),hjust = 0,vjust = 0.5,size = 3,fontface = 'bold') +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,3.7),'cm'),
      legend.position = c(-0.15,1.065))) %>%
  ggsave('images/mcc9.png',.,'png',width=7.1,height=4)
rm(mape)

#Plot actual vs predicted age by chronic status over time
data.age <- aggregate(data[c('Age','Exposure')],by=list(Period = data$Period,Chronic = data$Chronic,Output = data$Output),FUN=sum)
data.age$Age <- data.age$Age / data.age$Exposure

mape <- dcast(data.age,Period+Chronic~Output,value.var='Age')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- cbind(aggregate(mape['mape'],by=list(Chronic=mape$Chronic),FUN=sum),aggregate(mape['Period'],by=list(Chronic=mape$Chronic),FUN=length)['Period'])
mape <- data.frame(mape['Chronic'],mape = sprintf("%1.2f%%",mape$mape / mape$Period),stringsAsFactors = FALSE)

(eda.canvas(data.age,'Period','Age','Output','mean',c(36,48),c(20,50),36:48) %>% 
    eda.lines(data.age[data.age$Chronic == 1,],'Period','Age','Output','sum','Output','Output') %>% 
    eda.lines(data.age[data.age$Chronic == 2,],'Period','Age','Output','sum','Output','Output') %>% 
    eda.points(c(36,48),data.age[(data.age$Chronic == 1) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.points(c(36,48),data.age[(data.age$Chronic == 2) & (data.age$Output == 'Actual'),],'Period','Age','Output','sum') %>% 
    eda.labels('Output',-0.8,data.age[(data.age$Chronic == 1) & (data.age$Output == 'Actual'),],'Period','Age','sum','Chronic status 1') %>%
    eda.labels('Output',-0.8,data.age[(data.age$Chronic == 2) & (data.age$Output == 'Actual'),],'Period','Age','sum','Chronic status 2') %>%
    eda.titles(
      'Average age by chronic status',
      paste('The MAPE is',mape$mape[1],'for chronic status 1 and',mape$mape[2],'for chronic status 2'),
      'Period',
      -2.6) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      plot.margin=unit(c(1.9,2,0.6,4.7),'cm'),
      legend.position = c(-0.25,1.065))) %>%
  ggsave('images/mcc10.png',.,'png',width=7.1,height=4)
rm(mape,data.age)

#Plot beneficiaries by age in period 36 and 48
data.age <- aggregate(list(Exposure = data[data$Period %in% c(36,48),'Exposure']),by=list(Age = data[data$Period %in% c(36,48),]$AAge,Period = data[data$Period %in% c(36,48),]$Period,Output = data[data$Period %in% c(36,48),]$Output),FUN=sum)

((eda.canvas(data.age,'Age','Exposure','Output','sum',c(0,100),c(0,4000),seq(0,100,10)) +
  geom_line(data = data.age[(data.age$Period == 36) & (data.age$Output == 'Actual'),],aes(x = Age,y = Exposure),color = 'grey90',size = 1)) %>% 
  eda.lines(data.age[data.age$Period == 48,],'Age','Exposure','Output','sum','Output','Output') %>%
  eda.points(c(110),data.age[data.age$Period == 48,],'Age','Exposure','Output','sum') %>% 
  eda.titles('Beneficiaries by age in period 48','','Period',12) +
  geom_text(aes(x = -6,y = 4250,label = 'Beneficiaries'),color = 'grey50',size = 3,hjust = 0) +
  scale_y_continuous(breaks = seq(0,5000,1000),labels = scales::comma) +
  guides(color = guide_legend(ncol=2)) +
  scale_color_manual(values = c('#2d6077','#f5b66e')) + 
  scale_size_manual(values=c(1,0.9)) +
  scale_linetype_manual(values=c('solid','dashed')) +
  theme(
    axis.line.y = element_line(colour = 'grey70', size = 0.1),
    axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
    axis.text.y = element_text(colour = 'grey50'),
    plot.margin=unit(c(1.9,1.5,0.6,1),'cm'),
    legend.position = c(0.045,1.14))) %>%
  ggsave('images/mcc11.png',.,'png',width=7.1,height=4)
rm(data.age)

#Plot beneficiaries by duration in period 36 and 48
data <- read_delim("data/output_beneficiaries.txt","|", escape_double = FALSE, trim_ws = TRUE)
data$Duration.Int <- floor(data$Duration)
data.duration <- aggregate(list(Exposure = data[data$Period %in% c(36,48),'Exposure']),by=list(Duration = data[data$Period %in% c(36,48),]$Duration.Int,Period = data[data$Period %in% c(36,48),]$Period,Output = data[data$Period %in% c(36,48),]$Output),FUN=sum)

((eda.canvas(data.duration,'Duration','Exposure','Output','sum',c(0,13),c(0,30000),seq(0,13,1)) +
    geom_line(data = data.duration[(data.duration$Period == 36) & (data.duration$Output == 'Actual'),],aes(x = Duration,y = Exposure),color = 'grey90',size = 1)) %>% 
    eda.lines(data.duration[data.duration$Period == 48,],'Duration','Exposure','Output','sum','Output','Output') %>%
    eda.points(c(14),data.duration[data.duration$Period == 48,],'Duration','Exposure','Output','sum') %>% 
    eda.titles('Beneficiaries by duration in period 48','','Period',1.4) +
    geom_text(aes(x = -0.95,y = 31500,label = 'Beneficiaries'),color = 'grey50',size = 3,hjust = 0) +
    scale_y_continuous(breaks = seq(0,30000,5000),labels = scales::comma) +
    guides(color = guide_legend(ncol=2)) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(1.9,1.5,0.6,1),'cm'),
      legend.position = c(0.035,1.14))) %>%
  ggsave('images/mcc12.png',.,'png',width=7.1,height=4)
rm(data.duration)

#Plot predicted option changes in period 37
data.move <- data[(data$Type == 1) & (data$Output == 'Actual') & (data$Period == 36),c('Period','Beneficiary','Plan','Exposure')]
data.move <- rbind(data.move,data[(data$Type == 1) & (data$Output == 'Predicted') & (data$Period == 37),c('Period','Beneficiary','Plan','Exposure')])
data.move$Period <- as.factor(data.move$Period)
data.move <- dcast(data.move,Beneficiary + Plan~Period,value.var = 'Exposure',fill=0,fun.aggregate = sum)
data.curr <- data.frame(Beneficiary = data.move$Beneficiary,Plan.Current = data.move$Plan * data.move$`36`)
data.curr <- data.curr[data.curr$Plan.Current != 0,]
data.move <- left_join(data.move,data.curr,by='Beneficiary')
rm(data.curr)
data.move <- data.move[!is.na(data.move$Plan.Current) & (data.move$Plan.Current != data.move$Plan),]
paths <- unique(data.move[,c('Plan.Current','Plan')])
rownames(paths) <- NULL
paths$Path <- rownames(paths)
data.move <- left_join(data.move,paths,by=c('Plan','Plan.Current'))
data.move <- data.move[,c('Beneficiary','Path','Plan.Current','Plan','37')]
rm(paths)
names(data.move) <- c('Beneficiary','Path','Period 36','Period 37','Exposure')
data.move <- melt(data.move,id = c('Beneficiary','Path','Exposure'))
names(data.move) <- c('Beneficiary','Path','Exposure','Period','Plan')
data.move$Plan <- as.factor(data.move$Plan)
levels(data.move$Plan) <- c('Plan 1','Plan 2','Plan 3')
data.move <- aggregate(data.move$Exposure,by=list(Period = data.move$Period,Plan = data.move$Plan,Path = data.move$Path),FUN=sum)
names(data.move) <- c('Period','Plan','Path','Exposure')

(ggplot(data.move,aes(x = Period, y = Exposure, stratum = Plan, alluvium = Path,fill = Plan)) +
    coord_cartesian(ylim=c(0,1600),clip = 'off') +
    scale_x_discrete(name='Period',breaks=c('Period 36','Period 37'),expand=c(0.09,0.09)) +
    scale_y_continuous(name='Exposure',breaks=seq(0,1600,100),expand=c(0,0),labels = scales::comma) +
    scale_fill_manual(values = c('#b84840','#82c7b7','#2d6077')) +
    geom_flow(alpha = 0.7, width = 0.2) +
    geom_stratum(size = 0,width = 0.2, alpha = 0.8,color='grey50') +
    geom_text(stat = "stratum", label.strata = TRUE,fontface = 'bold',color = 'white',size = 3.5) +
    labs(title='Predicted plan changes in period 37',
         subtitle='Families') +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(colour = 'grey70', size = 0.1),
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.x = element_text(colour = 'grey50',face='bold',size=9),
      axis.text.y = element_text(colour = 'grey50'),
      plot.title = element_text(face = 'bold', color = 'grey20', size = 15, hjust = -0.21, vjust = 7),
      plot.subtitle = element_text(face='bold',color = 'grey50', size = 9, hjust = -0.076,vjust = 2.8),
      legend.position = 'off',
      plot.margin=unit(c(1.2,2,0.6,1.5),'cm'))) %>%
  ggsave('images/mcc13.png',.,'png',width=7.1,height=4)
rm(data.move)

#Load the regression model training history
history <- read_delim("data/history_claims.txt", "|", escape_double = FALSE, trim_ws = TRUE)
history <- data.frame(history[!is.na(history$value),])
names(history) <- c('Epoch','Value','Metric','Set')


#Plot the regression training metrics
(eda.canvas(history[history$Metric == 'loss',],'Epoch','Value','Set','sum',c(1,42),c(460000,740000),c(1,5,10,15,20,25,30,35,40)) %>% 
    eda.lines() %>% 
    eda.points(c(1,42)) %>% 
    eda.labels('',0) %>% 
    eda.titles(
      'Training and validation loss',
      '',
      'Epoch',
      -3) +
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(plot.margin=unit(c(2.2,2,0.6,3.5),'cm'))) %>%
  ggsave('images/reg1.png',.,'png',width=7.1,height=4)

#Plot the regression training metrics
(eda.canvas(history[history$Metric == 'mean_absolute_error',],'Epoch','Value','Set','sum',c(1,42),c(70,130),c(1,5,10,15,20,25,30,35,40)) %>% 
    eda.lines() %>% 
    eda.points(c(1,42)) %>% 
    eda.labels('',0) %>% 
    eda.titles(
      'Training and validation MAE',
      '',
      'Epoch',
      -2) +
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(plot.margin=unit(c(2.2,2,0.6,3.5),'cm'))) %>%
  ggsave('images/reg2.png',.,'png',width=7.1,height=4)
rm(history)

#Load and summarize the claims predictions
data <- read_delim("data/output_claims.txt","|", escape_double = FALSE, trim_ws = TRUE)
data <- left_join(data,data.frame(data[(data$Period == 36) & (data$Output == 'Actual'),'Beneficiary'],Keep = 'Keep'),by=c('Beneficiary'))
data <- data[!is.na(data$Keep),
             c('Period','Age','Duration','Plan','Income','Type','Chronic','Output','Exposure','ClaimTotal')]
data$Age <- floor(data$Age)
data$Duration <- floor(data$Duration)
data$ClaimTotal <- data$ClaimTotal * data$Exposure
data <- aggregate(list(Exposure = data$Exposure,Claims = data$ClaimTotal),
                  by=list(Period = data$Period,Age = data$Age,Duration = data$Duration,Plan = data$Plan,Income = data$Income,Type = data$Type,Chronic = data$Chronic,Output = data$Output),
                  FUN=sum)
data <- data[data$Period %in% 37:48,]

#Plot predicted and actual claims
data.pb <- aggregate(list(Exposure = data$Exposure,Claims = data$Claims),by=list(Period = data$Period,Output = data$Output),FUN=sum)
data.pb$Claims.pb <- data.pb$Claims / data.pb$Exposure

mape <- dcast(data.pb,Period~Output,value.var='Claims.pb')
mape$mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)

mape2 <- aggregate(list(Claims = data.pb$Claims,Exposure = data.pb$Exposure),by=list(Output = data.pb$Output),FUN=sum)
mape2$Claims.pb <- mape2$Claims / mape2$Exposure
mape2 <- abs((mape2[mape2$Output == 'Actual','Claims.pb'] - mape2[mape2$Output == 'Predicted','Claims.pb']) / mape2[mape2$Output == 'Actual','Claims.pb'] * 100)
mape2 <- round(mape2,2)

(eda.canvas(data.pb,'Period','Claims.pb','Output','sum',c(37,48),c(0,100),36:48) %>% 
    eda.lines(data.pb,'Period','Claims.pb','Output','sum','Output','Output') %>%
    eda.points(c(36)) +
    geom_text(aes(x = 36.6,y = 140,label = 'Average claim per beneficiary over time'),fontface = 'bold', color = 'grey20',size = 6,hjust = 0) +
    geom_text(aes(x = 36.6,y = 130,label = paste0('The average MAPE per month is ',round(mean(mape$mape),2),'%. The overall MAPE for the prediction period is ',mape2,'%')),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = 36.6,y = 107,label = 'Claim per beneficiary'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = 36.88,y = -11,label = 'Period'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=2)) +
    scale_y_continuous(breaks = seq(0,100,10),labels = scales::comma) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(3,1,0.6,1),'cm'),
      legend.position = c(0.065,1.24))) %>%
  ggsave('images/reg3.png',.,'png',width=7.1,height=4)
rm(mape,mape2,data.pb)

#Plot the predicted an actual claims by plan over time
data.plan <- aggregate(data[c('Claims','Exposure')],by=list(Period = data$Period,Plan = data$Plan,Output = data$Output),FUN=sum)
data.plan$Claims.pb <- data.plan$Claims / data.plan$Exposure
data.plan <- data.plan[data.plan$Plan != 0,c('Period','Plan','Output','Claims.pb','Claims','Exposure')]

mape <- aggregate(list(Claims = data.plan$Claims,Exposure = data.plan$Exposure),by=list(Plan = data.plan$Plan,Output = data.plan$Output),FUN=sum)
mape$Claims.pb <- mape$Claims / mape$Exposure
mape <- dcast(mape,Plan~Output,value.var='Claims.pb')
mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- round(mape,2)

(eda.canvas(data.plan,'Period','Claims.pb','Output','mean',c(37,48),c(0,100),37:48) %>% 
    eda.lines(data.plan[data.plan$Plan == 1,],'Period','Claims.pb','Output','sum','Output','Output') %>% 
    eda.lines(data.plan[data.plan$Plan == 2,],'Period','Claims.pb','Output','sum','Output','Output') %>% 
    eda.lines(data.plan[data.plan$Plan == 3,],'Period','Claims.pb','Output','sum','Output','Output') %>%
    eda.points(c(36)) +
    geom_text(aes(x = 36.6,y = 140,label = 'Average claim per beneficiary by plan over time'),fontface = 'bold', color = 'grey20',size = 6,hjust = 0) +
    geom_text(aes(x = 36.6,y = 130,label = paste0('The overall average MAPE is ',mape[1],'% for plan 1, ',mape[2],'% for plan 2 and ',mape[3],'% for plan 3')),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = 36.6,y = 107,label = 'Claim per beneficiary'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = 36.88,y = -11,label = 'Period'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=2)) +
    scale_y_continuous(breaks = seq(0,100,10),labels = scales::comma) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(3,1,0.6,1),'cm'),
      legend.position = c(0.065,1.24))) %>%
  ggsave('images/reg4.png',.,'png',width=7.1,height=4)
rm(mape,data.plan)

#Plot the predicted an actual claims by type over time
data.type <- aggregate(data[c('Claims','Exposure')],by=list(Period = data$Period,Plan = data$Plan,Type = data$Type,Output = data$Output),FUN=sum)
data.type$Claims.pb <- data.type$Claims / data.type$Exposure
data.type <- data.type[data.type$Plan != 0,c('Period','Type','Output','Claims.pb','Claims','Exposure')]
data.type <- aggregate(data.type[c('Claims','Exposure')],by=list(Period = data.type$Period,Type = data.type$Type,Output = data.type$Output),FUN=sum)
data.type$Claims.pb <- data.type$Claims / data.type$Exposure

mape <- aggregate(list(Claims = data.type$Claims,Exposure = data.type$Exposure),by=list(Type = data.type$Type,Output = data.type$Output),FUN=sum)
mape$Claims.pb <- mape$Claims / mape$Exposure
mape <- dcast(mape,Type~Output,value.var='Claims.pb')
mape <- abs((mape$Actual - mape$Predicted) / mape$Actual * 100)
mape <- round(mape,2)

(eda.canvas(data.type,'Period','Claims.pb','Output','mean',c(37,48),c(0,150),37:48) %>% 
    eda.lines(data.type[data.type$Type == 1,],'Period','Claims.pb','Output','sum','Output','Output') %>% 
    eda.lines(data.type[data.type$Type == 2,],'Period','Claims.pb','Output','sum','Output','Output') %>% 
    eda.lines(data.type[data.type$Type == 3,],'Period','Claims.pb','Output','sum','Output','Output') %>%
    eda.points(c(36)) +
    geom_text(aes(x = 36.6,y = 210,label = 'Average claim per beneficiary by type over time'),fontface = 'bold', color = 'grey20',size = 6,hjust = 0) +
    geom_text(aes(x = 36.6,y = 195,label = paste0('The overall average MAPE is ',mape[1],'% for type 1, ',mape[2],'% for type 2 and ',mape[3],'% for type 3')),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = 36.6,y = 161,label = 'Claim per beneficiary'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = 36.88,y = -16.5,label = 'Period'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=2)) +
    scale_y_continuous(breaks = seq(0,150,25),labels = scales::comma) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(3,1,0.6,1),'cm'),
      legend.position = c(0.065,1.24))) %>%
  ggsave('images/reg5.png',.,'png',width=7.1,height=4)
rm(mape,data.type)

#Plot predicted and actual claims by age
data.pb <- data[data$Plan != 0,]
data.pb <- aggregate(list(Exposure = data.pb$Exposure,Claims = data.pb$Claims),by=list(Age = data.pb$Age,Output = data.pb$Output),FUN=sum)
data.pb$Claims.pb <- data.pb$Claims / data.pb$Exposure
data.pb <- data.pb[data.pb$Age <= 90,]

(eda.canvas(data.pb,'Age','Claims.pb','Output','sum',c(0,90),c(0,500),seq(0,90,10)) %>% 
    eda.lines(data.pb,'Age','Claims.pb','Output','sum','Output','Output') %>%
    eda.points(c(91)) +
    geom_text(aes(x = -3.5,y = 600,label = 'Average claim per beneficiary by age'),fontface = 'bold', color = 'grey20',size = 6,hjust = 0) +
    geom_text(aes(x = -3.5,y = 530,label = 'Claim per beneficiary'),color = 'grey50',size = 3,hjust = 0) +
    geom_text(aes(x = -0.25,y = -45,label = 'Age'),color = 'grey50',size = 3,hjust = 0) +
    guides(color = guide_legend(ncol=2)) +
    scale_y_continuous(breaks = seq(0,500,50),labels = scales::comma) +
    scale_color_manual(values = c('#2d6077','#f5b66e')) + 
    scale_size_manual(values=c(1,0.9)) +
    scale_linetype_manual(values=c('solid','dashed')) +
    theme(
      axis.line.y = element_line(colour = 'grey70', size = 0.1),
      axis.ticks.y = element_line(colour = 'grey70', size = 0.1),
      axis.text.y = element_text(colour = 'grey50'),
      plot.margin=unit(c(2,1,0.6,1),'cm'),
      legend.position = c(0.065,1.135))) %>%
  ggsave('images/reg6.png',.,'png',width=7.1,height=4)
rm(data.pb)
rm(list=ls())
