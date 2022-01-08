library(ggplot2)
library(dplyr)
library(data.table)
library(janitor)
library(readxl)
library(tidyr)
library(zoo)
library(gridExtra)

tmp <- data %>% 
  filter(country=="Israel") %>% 
  relocate(coord,level)


'%ni%' <- Negate('%in%')
###############################################################################
setwd("C:/Users/97254/Dropbox/Unions Arlozerov")

data <- fread("Unions Density Inequality/OECD-AIAS-ICTWSS-CSV.csv") %>% 
  clean_names()


data <- data %>% 
  select(country,year,ud,unadj_cov,unadj_cov_s) %>%
  mutate(cov=case_when(
    unadj_cov>0 & unadj_cov_s>0 ~ (unadj_cov_s+unadj_cov)/2,
    unadj_cov>0 & unadj_cov_s<0 ~ unadj_cov,
    unadj_cov<0 & unadj_cov_s>0 ~ unadj_cov_s,
    unadj_cov<0 & unadj_cov_s<0 ~ (-1),
  )) %>% 
  filter(ud>0 | cov>0) %>% 
  mutate(country=ifelse(country=="United States","USA",country))

israel_ud <- data.frame(country="Israel",year=c(1982,1988,1992),ud=c(84,73,70))

ud_data <- data %>% 
  filter(ud>0) %>% 
  rbind(israel_ud,fill=TRUE)

l <- list()
i=1
for (country1 in (ud_data$country %>% unique())) {
  country_data <- ud_data %>% 
    filter(country==country1) %>% 
    mutate(original_ud=T)
  
  country_data <- data.frame(year=min(country_data$year):max(country_data$year),country=country_data$country[[1]]) %>% 
    left_join(country_data) %>% 
    mutate(ud = na.approx(ud, rule = 2),
           original_ud=ifelse(is.na(original_ud),F,T))
  
  l[[i]] <- country_data
  i=i+1
}

ud_data <- l %>% 
  bind_rows() %>% 
  select(year,country,ud,original_ud)
################################################################################
l <- list()
i=1


cov_data <- data %>% 
  filter(cov>0) %>% 
  filter(country!="Denmark")

unions_coverage_OECD <- fread("Unions coverage OECD.csv") %>% 
  clean_names() %>% 
  select(country,year,value) %>% 
  rename("cov"="value") %>% 
  filter(country %in% c("Denmark","Ireland","Israel","Italy"))

cov_data <- cov_data %>% 
  rbind(unions_coverage_OECD,fill=T) %>% 
  rbind(data.frame(country="Israel",year=c(1982,1988,1992),cov=c(84,73,70)),fill=T)


for (country1 in (cov_data$country %>% unique())) {
  country_data <- cov_data %>% 
    filter(country==country1) %>% 
    mutate(original_cov=T)
  
  country_data <- data.frame(year=min(country_data$year):max(country_data$year),country=country_data$country[[1]]) %>% 
    left_join(country_data) %>% 
    mutate(cov = na.approx(cov, rule = 2),
           original_cov=ifelse(is.na(original_cov),F,T))
  
  l[[i]] <- country_data
  i=i+1
}

cov_data <- l %>% 
  bind_rows() %>% 
  select(year,country,cov,original_cov)

################################################################################
################################################################################
WID_data <- read_excel("Unions Density Inequality/WID data gini.xlsx") %>% 
  clean_names() 

WID_data <- WID_data %>% 
  select(country,year,gini) %>% 
  filter(!is.na(gini)) %>% 
  mutate(original_wid=T)


gini_israel <- fread("Unions Density Inequality/gini israel.csv") %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(country="Israel",
         original_wid=T)

gini_israel<- gini_israel[2:43,]

names(gini_israel)[1:2] <- c("year","gini")

gini_israel <- gini_israel %>% 
  filter(!is.na(gini)) %>% 
  mutate(year=year %>% as.numeric(),
         gini=as.numeric(gini)/100)

WID_data <- WID_data %>% 
  rbind(gini_israel)


country1 <- WID_data$country[[1]]

l <- list()
i=1
for (country1 in (WID_data$country %>% unique())) {
  country_data <- WID_data %>% 
    filter(country==country1)
  
  country_data <- data.frame(year=min(country_data$year):max(country_data$year),country=country_data$country[[1]]) %>% 
    left_join(country_data) %>% 
    mutate(gini = na.approx(gini, rule = 2),
           original_wid=ifelse(is.na(original_wid),F,T))
  
  l[[i]] <- country_data
  i=i+1
}

WID_data <- l %>% 
  bind_rows()

df <- WID_data %>% 
  left_join(ud_data) %>% 
  filter(!is.na(ud)) %>% 
  arrange(country)

rel_countries<- df %>% 
  filter(year<1990) %>% 
  .$country %>% 
  unique()

df <- df %>% 
  filter(country %in% rel_countries) %>% 
  left_join(cov_data)

df_long <- melt(df %>% select(year,country,gini, ud, cov), id.var=c("year","country")) %>% 
  mutate(variable=case_when(
    variable=="ud" ~ "Union Density",
    variable=="gini" ~ "Gini Coefficent",
    variable=="cov" ~"Union Coverage")) %>% 
  filter(!is.na(value)) %>% 
  as.data.table() 




plot_list <- list()
i=1
for (country1 in df_long$country %>% unique()) {
  df_long_country <- df_long %>% 
    filter(country==country1)
  
  plot_list[[i]]<- ggplot(df_long_country, aes(x = year, y = value)) + 
    geom_line(aes(color = variable)) + 
    facet_grid(variable ~ ., scales = "free_y") + 
    theme(legend.position = "none")+
    theme_bw()+
    ggtitle(country1)+
    theme(legend.position="none",plot.title = element_text(hjust = 0.5))+
    ylab("")
  i=i+1
}
# Graph 5

do.call("grid.arrange", c(plot_list[1:10], ncol = 5))
do.call("grid.arrange", c(plot_list[11:19], ncol = 5))