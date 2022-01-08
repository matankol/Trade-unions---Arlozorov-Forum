library(ggplot2)
library(dplyr)
library(data.table)
library(janitor)
library(readxl)

'%ni%' <- Negate('%in%')
###############################################################################
setwd("C:/Users/97254/Dropbox/Unions Arlozerov")

unions_density <- fread("Unions density.csv") %>% 
  clean_names() %>% 
  mutate(time=ifelse(time==2016 & country=="France",2017,time)) %>% 
  filter(time ==2017) %>%
  select(location,time,country,value) %>% 
  rename("state"="location","year"="time","density"="value")


ratio_90_10 <- fread("ratio 90-10.csv") %>% 
  clean_names() %>% 
  select(location, time, value) %>% 
  rename("state"="location","year"="time","ratio90_10"="value")


df <- unions_density %>% 
  left_join(ratio_90_10) %>% 
  filter(!is.na(ratio90_10)) %>% 
  filter(country %ni% c("Costa Rica","Chile","Colombia","Mexico","OECD - Total"))

# Graph 3
df %>% 
  ggplot(aes(x=density,y=ratio90_10))+
  geom_point()+
  geom_text(aes(label=state),hjust=-0.2, vjust=-0.2)+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Density")+
  ylab("90-10 Ratio")+
  ggtitle("Earnings Inequality (Full-Time Emplyees) and Union Density, 2017")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20))
  

data_gini <- read_excel("data gini.xlsx") %>% 
  clean_names() 

data_gini <- data_gini%>% 
  mutate(gini_after=gini_after %>% as.numeric(),
         gini_before=gini_before %>% as.numeric(),
         redistribution=1-gini_after/gini_before) %>% 
  select(year, country,redistribution) %>% 
  mutate(year=year %>% as.numeric())

df <- unions_density %>% 
  left_join(data_gini) %>% 
  filter(!is.na(redistribution)) %>% 
  filter(country %ni% c("Costa Rica","Chile","Colombia","Mexico","OECD - Total"))


# Graph 4
df %>% 
  filter(country %ni% c("Sweden","Finland","Norway","Denmark","Iceland")) %>%
  ggplot(aes(x=density,y=redistribution))+
  geom_point()+
  geom_text(aes(label=state),hjust=-0.2, vjust=-0.2)+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Density")+
  ylab("Redistribution")+
  ggtitle("Redistribution and Union Density, 2017")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20))
