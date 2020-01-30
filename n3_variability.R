library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

observing_files <- read_csv('18a_plotting.csv')

obs_dates_times <- observing_files %>% 
  separate(Obs_Data_Starts, into=c('Date_Start', 'Time_Start'), sep=' ') %>% 
  separate(Obs_Data_Stops, into=c('Date_Stop', 'Time_Stop'), sep=' ') %>% 
  subset(select = c(Archive_File, Date_Start, Time_Start, Date_Stop, Time_Stop)) %>% 
  mutate(Time = 30, Observation_Number = row_number()) 

obs_dates_times$Date_Start <- as.Date(obs_dates_times$Date_Start, format = '%m/%d/%y')
obs_dates_times$Date_Stop <- as.Date(obs_dates_times$Date_Stop, format = '%m/%d/%y')

# Integrated Flux
observation_number = list(2, 4, 8)
clow_intflux = c(0.0451613, 0.0498291, 0.0500832, 0.0510663, 0.0509634, 0.0480291, 0.0491896, 0.0482797)
chigh_intflux = c(0.0535411, 0.0582872, 0.0520912, 0.0600491, 0.057424, 0.0586122, 0.0597528, 0.0573686)
xlow_intflux = c(0.0497911, 0.0596587, 0.0610392, 0.0600477, 0.0589672, 0.0585276, 0.057754, 0.0586517)
xhigh_intflux = c(0.0387159, 0.0523965, 0.0541612, 0.0543891, 0.0487898, 0.0510072, 0.0513736, 0.0532303)

# Peak Flux
clow_peakflux = c(0.0360373, 0.0386983, 0.0443241, 0.0445673, 0.0403476, 0.0381243, 0.0369907, 0.0374759)
chigh_peakflux = c(0.0497081, 0.0540879, 0.0507876, 0.0557015, 0.050774, 0.0536447, 0.052652,	0.0529501)
xlow_peakflux = c(0.0456914, 0.054581, 0.059105, 0.0574829, 0.0536616, 0.0530232, 0.0513428, 0.052519)
xhigh_peakflux = c(0.0380855, 0.0498107, 0.0519624,  0.0479987, 0.0448202, 0.0481937, 0.0458688, 0.0481993)

obs_dates_times <- obs_dates_times %>% 
  mutate(C_Low_Integrated_Flux = clow_intflux, C_Low_Peak_Flux = clow_peakflux, 
         C_High_Integrated_Flux = chigh_intflux, C_High_Peak_Flux = chigh_peakflux,
         X_Low_Integrated_Flux = xlow_intflux, X_Low_Peak_Flux = xlow_peakflux,
         X_High_Integrated_Flux = xhigh_intflux, X_High_Peak_Flux = xhigh_peakflux,
         C_Error = mean(x= c((clow_intflux-clow_peakflux), (chigh_intflux-chigh_peakflux)), trim = 0, na.rm = TRUE), 
         X_Error = mean(x= c((xlow_intflux-xlow_peakflux), (xhigh_intflux-xhigh_peakflux)), trim = 0, na.rm = TRUE))

clow_plot <- ggplot(data = obs_dates_times, aes(Date_Start, C_Low_Integrated_Flux)) +
  geom_point() +
  geom_errorbar(aes(ymin = C_Low_Integrated_Flux - C_Error, ymax = C_Low_Integrated_Flux + C_Error)) +
  labs(y = 'Integrated Flux (Jy)', x = 'Observation Date',
       title = 'Integrated Flux Variability',
       subtitle = 'Low C band') +
  theme_bw()
ggsave('clow_plot_w_obs1.png')

chigh_plot <- ggplot(data = obs_dates_times, aes(Date_Start, C_High_Integrated_Flux)) +
  geom_point() +
  geom_errorbar(aes(ymin = C_High_Integrated_Flux - C_Error, ymax = C_High_Integrated_Flux + C_Error)) +
  labs(y = 'Integrated Flux (Jy)', x = 'Observation Date',
       title = 'Integrated Flux Variability',
       subtitle = 'High C band') +
  theme_bw()
ggsave('chigh_plot_w_obs1.png')

xlow_plot <- ggplot(data = obs_dates_times, aes(Date_Start, X_Low_Integrated_Flux)) +
  geom_point() +
  geom_errorbar(aes(ymin = X_Low_Integrated_Flux - X_Error, ymax = X_Low_Integrated_Flux + X_Error)) +
  labs(y = 'Integrated Flux (Jy)', x = 'Observation Date',
       title = 'Integrated Flux Variability',
       subtitle = 'Low X band') +
  theme_bw()
ggsave('xlow_plot_w_obs1.png')

xhigh_plot <- ggplot(data = obs_dates_times, aes(Date_Start, X_High_Integrated_Flux)) +
  geom_point() +
  geom_errorbar(aes(ymin = X_High_Integrated_Flux - X_Error, ymax = X_High_Integrated_Flux + X_Error)) +
  labs(y = 'Integrated Flux (Jy)', x = 'Observation Date',
       title = 'Integrated Flux Variability',
       subtitle = 'High X band') +
  theme_bw()
ggsave('xhigh_plot_w_obs1.png')

print(clow_plot)
print(chigh_plot)
print(xlow_plot)
print(xhigh_plot)

#flux_table = data.frame(observation_number, clow_intflux, clow_peakflux, chigh_intflux, chigh_peakflux,
#                        xlow_intflux, xlow_peakflux, xhigh_intflux, xhigh_peakflux)

