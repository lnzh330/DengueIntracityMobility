library(openxlsx)
library(EpiEstim)
library(incidence)
library(ggplot2)
data.day <- read.xlsx('EpiCurve.xlsx', sheet = 'daily')
str(data.day)
data.day$Date <- as.Date(data.day$Date, origin = '1899-12-30')
###################Imported-local acquired-4years###################
###data
dates_onset.imp <- data.day$Date[unlist(lapply(1:nrow(data.day), function(i) 
  rep(i, data.day$Imported[i])))]
location.imp <- rep('Imported',length(dates_onset.imp))

dates_onset.loc <- data.day$Date[unlist(lapply(1:nrow(data.day), function(i) 
  rep(i, data.day$Local[i])))]
location.loc <- rep('Local',length(dates_onset.loc))

dates_onset <- c(dates_onset.imp, dates_onset.loc)
location <- c(location.imp, location.loc)
## get incidence per group (location)
incid <- incidence(dates_onset, groups = location)
plot(incid)

#check the dates
incid$dates
##time 
T = length(incid$dates)
tstart = seq(57,T-13) 
tend = tstart + 13
## Estimate R with assumptions on serial interval
res_with_imports <- estimate_R(incid, method = "parametric_si",
                               config = make_config(list(
                                 mean_si = 18.2, std_si = 4,
                                 t_start = tstart, t_end = tend)))
#> Default config will estimate R on weekly sliding windows.
#>     To change this change the t_start and t_end arguments.
plot(res_with_imports, add_imported_cases=TRUE)

############################################################################
###################Imported-local acquired-daily######################################
######2019######
###data
data.day2 <- data.day[1:365,]
dates_onset.imp <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Imported[i])))]
location.imp <- rep('Imported',length(dates_onset.imp))

dates_onset.loc <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Local[i])))]
location.loc <- rep('Local',length(dates_onset.loc))

dates_onset <- c(dates_onset.imp, dates_onset.loc)
location <- c(location.imp, location.loc)
## get incidence per group (location)
incid <- incidence(dates_onset, groups = location)
plot(incid)
#check the dates
incid$dates
case19 <- as.data.frame(incid$counts)
case19$dates <- incid$dates
##time 
T = length(incid$dates)
tstart = seq(93,T-13) 
tend = tstart + 13
## Estimate R with assumptions on serial interval
res_with_imports <- estimate_R(incid, method = "parametric_si",
                               config = make_config(list(
                                 mean_si = 18.2, std_si = 4,
                                 t_start = tstart, t_end = tend)))
#> Default config will estimate R on weekly sliding windows.
#>     To change this change the t_start and t_end arguments.
plot(res_with_imports, add_imported_cases=TRUE)
R19 <- res_with_imports$R
R19$date.start <- incid$dates[tstart]
R19$date.end <- incid$dates[tend]
R19$date <- R19$date.start + 6
#plot
case19.plot <- data.frame(time = rep(case19$dates,2),
                          type = c(rep('Imported',nrow(case19)), rep('local',nrow(case19))),
                          values = c(case19$Imported, case19$Local))
p1 <- ggplot() +
  geom_histogram(case19.plot, mapping = aes(time, values, fill = type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values = c('#F48453', '#4682B4')) +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2019-07-01'),as.Date('2020-01-01')),
               date_labels = '%b')+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,30),breaks = seq(0,30,10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p1

p2 <- ggplot(R19, mapping = aes(date, `Mean(R)`)) +
  geom_line(color = '#4682B4') +
  geom_ribbon(mapping = aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`),alpha=0.3, fill = '#4682B4') +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2019-07-01'),as.Date('2020-01-01')),
               date_labels = '%b')+
  scale_y_continuous(limits = c(0,12),breaks = seq(0,12,2)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p2
######2020######
###data
data.day2 <- data.day[366:731,]
dates_onset.imp <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Imported[i])))]
location.imp <- rep('Imported',length(dates_onset.imp))

dates_onset.loc <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Local[i])))]
location.loc <- rep('Local',length(dates_onset.loc))

dates_onset <- c(dates_onset.imp, dates_onset.loc)
location <- c(location.imp, location.loc)
## get incidence per group (location)
incid <- incidence(dates_onset, groups = location)
plot(incid)
incid
#check the dates
incid$dates
case20 <- as.data.frame(incid$counts)
case20$dates <- incid$dates
##time 
T = length(incid$dates)
tstart = seq(208,T - 13) 
tend = tstart + 13
## Estimate R with assumptions on serial interval
res_with_imports <- estimate_R(incid, method = "parametric_si",
                               config = make_config(list(
                                 mean_si = 18.2, std_si = 4,
                                 t_start = tstart, t_end = tend)))
#> Default config will estimate R on weekly sliding windows.
#>     To change this change the t_start and t_end arguments.
plot(res_with_imports, add_imported_cases=TRUE,"R")
R20 <- res_with_imports$R
R20$date.start <- incid$dates[tstart]
R20$date.end <- incid$dates[tend]
R20$date <- R20$date.start+6
#plot
case20.plot <- data.frame(time = rep(case20$dates,2),
                          type = c(rep('Imported',nrow(case20)), rep('local',nrow(case20))),
                          values = c(case20$Imported, case20$Local))
p1 <- ggplot() +
  geom_histogram(case20.plot, mapping = aes(time, values, fill = type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values = c('#F48453', '#4682B4')) +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2020-07-01'),as.Date('2021-01-01')),
               date_labels = '%b')+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,30),breaks = seq(0,30,10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p1

p2 <- ggplot(R20, mapping = aes(date, `Mean(R)`)) +
  geom_line(color = '#4682B4') +
  geom_ribbon(mapping = aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`),alpha=0.3, fill = '#4682B4') +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2020-07-01'),as.Date('2021-01-01')),
               date_labels = '%b')+
  scale_y_continuous(limits = c(0,12),breaks = seq(0,12,2)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p2
######2021######
###data
data.day2 <- data.day[732:1096,]
dates_onset.imp <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Imported[i])))]
location.imp <- rep('Imported',length(dates_onset.imp))

dates_onset.loc <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Local[i])))]
location.loc <- rep('Local',length(dates_onset.loc))

dates_onset <- c(dates_onset.imp, dates_onset.loc)
location <- c(location.imp, location.loc)
## get incidence per group (location)
incid <- incidence(dates_onset, groups = location)
plot(incid)
incid
#check the dates
incid$dates
case21 <- as.data.frame(incid$counts)
case21$dates <- incid$dates
##time 
T = length(incid$dates)
tstart = seq(11,T-13) #High season 9.30-11.13
tend = tstart + 13
## Estimate R with assumptions on serial interval
res_with_imports <- estimate_R(incid, method = "parametric_si",
                               config = make_config(list(
                                 mean_si = 18.2, std_si = 4,
                                 t_start = tstart, t_end = tend)))
#> Default config will estimate R on weekly sliding windows.
#>     To change this change the t_start and t_end arguments.
plot(res_with_imports, add_imported_cases=TRUE)
R21 <- res_with_imports$R
R21$date.start <- incid$dates[tstart]
R21$date.end <- incid$dates[tend]
R21$date <- R21$date.start + 6
#plot
case21.plot <- data.frame(time = rep(case21$dates,2),
                          type = c(rep('Imported',nrow(case21)), rep('local',nrow(case21))),
                          values = c(case21$Imported, case21$Local))
p1 <- ggplot() +
  geom_histogram(case21.plot, mapping = aes(time, values, fill = type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values = c('#F48453', '#4682B4')) +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2021-07-01'),as.Date('2022-01-01')),
               date_labels = '%b')+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,30),breaks = seq(0,30,10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p1

p2 <- ggplot(R21, mapping = aes(date.end, `Mean(R)`)) +
  geom_line(color = '#4682B4') +
  geom_ribbon(mapping = aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`),alpha=0.3, fill = '#4682B4') +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2021-07-01'),as.Date('2022-01-01')),
               date_labels = '%b')+
  scale_y_continuous(limits = c(0,12),breaks = seq(0,12,2)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p2
######2022######
###data
data.day2 <- data.day[1097:1461,]
dates_onset.imp <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Imported[i])))]
location.imp <- rep('Imported',length(dates_onset.imp))

dates_onset.loc <- data.day2$Date[unlist(lapply(1:nrow(data.day2), function(i) 
  rep(i, data.day2$Local[i])))]
location.loc <- rep('Local',length(dates_onset.loc))

dates_onset <- c(dates_onset.imp, dates_onset.loc)
location <- c(location.imp, location.loc)
## get incidence per group (location)
incid <- incidence(dates_onset, groups = location)
plot(incid)
#check the dates
incid$dates
case22 <- as.data.frame(incid$counts)
case22$dates <- incid$dates
##time 
T = length(incid$dates)
tstart = seq(11,T - 13) 
tend = tstart + 13
## Estimate R with assumptions on serial interval
res_with_imports <- estimate_R(incid, method = "parametric_si",
                               config = make_config(list(
                                 mean_si = 18.2, std_si = 4,
                                 t_start = tstart, t_end = tend)))
#> Default config will estimate R on weekly sliding windows.
#>     To change this change the t_start and t_end arguments.
plot(res_with_imports, add_imported_cases=TRUE,'R')
R22 <- res_with_imports$R
R22$date.start <- incid$dates[tstart]
R22$date.end <- incid$dates[tend]
R22$date <- R22$date.start + 6
#plot
case22.plot <- data.frame(time = rep(case22$dates),
                          type = c(rep('local',nrow(case22))),
                          values = c(case22$Local))
p1 <- ggplot() +
  geom_histogram(case22.plot, mapping = aes(time, values, fill = type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values = c('#4682B4')) +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2022-07-01'),as.Date('2023-01-01')),
               date_labels = '%b')+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,30),breaks = seq(0,30,10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p1

p2 <- ggplot(R22, mapping = aes(date, `Mean(R)`)) +
  geom_line(color = '#4682B4') +
  geom_ribbon(mapping = aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`),alpha=0.3, fill = '#4682B4') +
  scale_x_date(breaks = '1 month',
               limits = c(as.Date('2022-07-01'),as.Date('2023-01-01')),
               date_labels = '%b')+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.text = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title = element_text(family = 'sans',size = 11, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        legend.position = '') +
  xlab('') + ylab('Num. of Cases')
p2

R.df <- rbind(R19, R20, R21, R22)
write.csv(R.df,'Rtdaily.csv')
