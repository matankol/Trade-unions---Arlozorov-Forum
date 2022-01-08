library(ggplot2)
library(dplyr)
library(data.table)
library(janitor)
library(readxl)
library(haven)
library(fixest)
library(gridExtra)
library(forcats)
library(scales)
library(vtable)


'%ni%' <- Negate('%in%')
################################################################################
setwd("C:/Users/97254/Dropbox/Unions Arlozerov")

wikipedia_iso_country_codes <- fread("wikipedia-iso-country-codes.csv") %>% 
  clean_names()

lis_income <-  fread("lis_income.csv") 
lis_income <- lis_income%>% 
  mutate(lincome=log(income_approx),
         country_name=ifelse(country_name=="Russia","Russian Federation",country_name))
################################################################################
data_list <- list()
i=1
gc()
for (i in 1:9) {
  files<- list.files(path = paste0("ESS data/ESS",i," full"))
  dta_file <- files[grepl(pattern = "dta",files)]
  data <- read_dta(paste0("ESS data/ESS",i," full/",dta_file)) %>%
    as.data.table()
  
  try(data <- data %>% 
        mutate(inwyys=inwyr),silent = T)
  
  try(data <- data %>% 
        mutate(occ_ESS=iscoco),silent = T)
  
  try(data <- data %>% 
        mutate(occ_ESS=isco08),silent = T)
  
  try(data <- data %>% 
        mutate(ind_ESS=nacer1),silent = T)
  
  try(data <- data %>% 
        mutate(ind_ESS=nacer11),silent = T)
  
  try(data <- data %>% 
        mutate(ind_ESS=nacer2),silent = T)
  
  try(data <- data %>% 
        mutate(income_ESS=hinctnt),silent = T)
  
  try(data <- data %>% 
        mutate(income_ESS=hinctnta),silent = T)
  
  try(data <- data %>% 
        mutate(hh_size=hhmmb),silent = T)
  
  try(data <- data %>% 
        mutate(partner_educ=edulvlpa,
               partner_educ_labels=as_factor(edulvlpa, levels = "labels") %>% as.character()),silent = T)
  
  
  
  try(data <- data %>% 
        mutate(partner_educ=as.numeric(edulvlpb)%/%100,
               partner_educ=ifelse(partner_educ>5 & partner_educ<9,"5",as.character(partner_educ)),
               partner_educ_labels=as_factor(edulvlpb, levels = "labels") %>% as.character()),silent = T)
  
  try(data <- data %>% 
        mutate(main_activity=mnactic %>% as.numeric()))
  
  
  data <- data %>% 
    select(inwyys,occ_ESS,ind_ESS,income_ESS,hh_size,eduyrs,rlgdnm,gndr,agea,yrbrn,
           mbtru,cntry,pspwght,partner_educ,partner_educ_labels,yrbrn2,gndr2,main_activity) %>% 
    left_join(wikipedia_iso_country_codes,by=c("cntry"="country_code")) 
    
  
  data <- data %>% 
    mutate(partner_educ=ifelse(partner_educ_labels=="Not applicable","6",partner_educ %>% as.character()),
           yrbrn2_label=as_factor(yrbrn2, levels = "labels") %>% as.character(),
           gndr2_label=as_factor(gndr2, levels = "labels") %>% as.character(),
           patner_age=inwyys-yrbrn2,
           yrbrn2_label=ifelse(is.na(yrbrn2_label),"reg",yrbrn2_label),
           mbtru_label=as_factor(mbtru, levels = "labels") %>% as.character(),
           patner_age_group=case_when(
             patner_age<65 & patner_age>18 ~ patner_age%/%5,
             yrbrn2_label=="Not applicable" |patner_age <18 ~0,
             patner_age>=65~14),
           patner_gender=case_when(
             gndr2==1 ~ "male",
             gndr2==2 ~ "female",
             gndr2_label=="Not applicable"~"none"))
  
  data <- data %>% 
    mutate(
      educ=case_when(
        eduyrs<12~1,
        eduyrs==12~2,
        eduyrs>=13 & eduyrs<=15 ~3,
        eduyrs>=16~4),
      eduyrs2=eduyrs*eduyrs,
      religion=ifelse(is.na(rlgdnm),99,rlgdnm),
      hh_size=hh_size %>% as.character() %>% as.numeric(),
      sex=gndr,
      age=agea,
      age=ifelse(is.na(age),inwyys-yrbrn,age),
      age2=age*age,
      age3=age*age*age,
      union=(mbtru==1),
      occ=occ_ESS%/%1000,
      occ=ifelse(is.na(occ),999,occ),
      ind1=ind_ESS%/%10,
      ind1=ifelse(is.na(ind1),999,ind1),
      ind2=ind_ESS,
      income_dec=paste0("d",income_ESS),
      year=2000+2*i,
      employed=(main_activity==1)|(main_activity==1)) %>% 
    filter(age>=18 & age<=65) %>% 
    left_join(lis_income %>% select(year,country_name,income_dec,lincome))
  
  data_list[[i]]<- data
}


unique_countries <- lapply(data_list, function(x) x$country_name %>% unique()) %>% 
  unlist() %>% 
  unique() %>% 
  sort()

# unique_countries <- unique_countries[unique_countries %in% (lis_income$country_name %>% unique())]

plot_country <- function(country) {
  print(country)  
  
  il_list <- list()
  ud_list <- list()
  for (i in 1:9) {
     data_country <- data_list[[i]] %>% 
      filter(country_name==country)
    
     
    if(nrow(data_country)!=0 & sum(!is.na(data_country$lincome))>0 & 
       sum(!is.na(data_country$mbtru))>0 & sum(!is.na(data_country$partner_educ))>0 ){


      # m1<- feols(fml ="lincome ~ union+sex+hh_size+age+age2 |religion+educ"%>% as.formula(),data_country,weights = ~pspwght,se="hetero")
      # m2<- feols(fml ="lincome ~ union+sex+hh_size+age+age2+eduyrs+eduyrs2 |religion"%>% as.formula(),data_country,weights = ~pspwght,se="hetero")
      # m3<- feols(fml ="lincome ~ union+sex+hh_size+age+age2 |religion+educ+occ"%>% as.formula(),data_country,weights = ~pspwght,se="hetero")
      # m4<- feols(fml ="lincome ~ union+sex+hh_size+age+age2+eduyrs+eduyrs2 |religion+occ"%>% as.formula(),data_country,weights = ~pspwght,se="hetero")
      # m5<- feols(fml ="lincome ~ union+sex+hh_size+age+age2 |religion+educ+occ+ind1"%>% as.formula(),data_country,weights = ~pspwght,se="hetero")
      m6<- feols(fml ="lincome ~ union+sex+hh_size+age+age2|
                 eduyrs+partner_educ+patner_age_group+patner_gender+religion+occ+ind1"%>% as.formula(),
                 data_country,
                 weights = ~pspwght,
                 se="hetero")

      row <- m6$coeftable %>%
        as.data.frame() %>%
        head(1) %>% 
        mutate(year=2000+2*i) %>% 
        rename("sd"="Std. Error")
      
      il_list[[i]] <-row
    }
     
    try(sd <- feols("union~1" %>% as.formula(),se="hetero",data_country) %>% .$coeftable %>% as.data.frame() %>% .$'Std. Error',silent = T)
     
    try(
     ud_list[[i]] <- data.frame(union_density=
                                  sum(data_country %>% 
                                        filter(age>18 & age<65) %>% 
                                        .$union,na.rm = T)/
                                  sum(!is.na(data_country%>% 
                                               filter(age>18 & age<65) %>% 
                                               .$union)),
                            sd=sd,
                            year=2000+i*2),silent = T)
  }
  
  df <- ud_list %>% 
    bind_rows() %>% 
    filter(!is.na(union_density))
  
  p1<- ggplot(df,aes(x=year,y=union_density))+
    geom_line(col="red")+
    geom_point(col="red")+
    geom_errorbar(data =df, aes(ymin=union_density-sd*1.96, ymax=union_density+sd*1.96), col="red", 
                  position = position_dodge(0.5), width = 1)+
    geom_hline(yintercept = 0, color = "black", size=1)+
    ggtitle(label = country)+
    ylab(label = "Union Density")+
    xlab(label = "Year")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    xlim(2001, 2019)
    
  
  if(nrow(df)<3){
    p1=NULL
  }
  
  df <- il_list %>% 
    bind_rows()
  
  p2<- ggplot(df,aes(x=year,y=Estimate))+
    geom_point(col=hue_pal()(3)[[2]],shape=2)+
    geom_errorbar(data =df, aes(ymin=Estimate-sd*1.96, ymax=Estimate+sd*1.96), col=hue_pal()(3)[[2]], 
                  position = position_dodge(0.5), width = 1)+
    geom_hline(yintercept = 0,linetype="dashed", color = "red", size=1)+
    ggtitle(label = country)+
    ylab(label = "Union Premium")+
    xlab(label = "Year")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
  if(nrow(df)<3){
    p2=NULL
  }
  
  return(list(p1,p2))
}
plots_list <- lapply(unique_countries, plot_country)
p1_list <- lapply(plots_list, function(x) x[[1]])
p2_list <- lapply(plots_list, function(x) x[[2]])

# Graph 9
p1_list<- Filter(Negate(is.null), p1_list)
do.call("grid.arrange", c(p1_list, ncol = 5))


# Graph 10
p2_list<- Filter(Negate(is.null), p2_list)
do.call("grid.arrange", c(p2_list, ncol = 5))


data2 <- data_list %>% 
  bind_rows()

data2 %>% select(main_activity,union) %>% table()

nrow(data2)/(data2 %>% mutate(state_year=paste0(country_name,year)) %>% .$state_year %>% unique() %>% length())

f <- "lincome ~ union+sex+hh_size+age+age2|eduyrs+partner_educ+patner_gender+patner_age_group+religion+occ+ind1+country_name"%>% as.formula()


feols(fml =f,
      data2,
      weights = ~pspwght,
      se="hetero")

data2 %>% filter(union==1 & country_name=="Israel") %>% .$age %>% mean()


data2 <- data2 %>% 
  mutate(income_ESS_labels=as_factor(income_ESS, levels = "labels") %>% as.character())

data2 %>% filter(union==1) %>% .$age %>% mean()

unique_countries <- unique_countries[unique_countries %in% (lis_income$country_name %>% unique())]

f <- "lincome ~ union+sex+hh_size+age+age2|year+eduyrs+partner_educ+patner_gender+patner_age_group+religion+occ+ind1"%>% as.formula()

data2$patner_age_group %>% unique() %>% length()
(is.na(data2$patner_age_group) %>% sum())/(nrow(data2))
(!is.na(data2$patner_age_group)) %>% sum()

countries_effect<- lapply(unique_countries, function(x) 
  feols(fml =f,
        data2 %>%
          filter(country_name==x),
        weights = ~pspwght,
        cluster="year") %>% 
    .$coeftable %>% 
    as.data.frame() %>% 
    head(1) %>% 
    mutate(country=x)) %>% 
  bind_rows() %>% 
  arrange(desc(Estimate)) %>% 
  mutate(country = fct_reorder(country, Estimate))
  


row.names(countries_effect) <- 1:nrow(countries_effect)
names(countries_effect)[2] <- "sd"

# Graph 7
ggplot(countries_effect,aes(x=country,y=Estimate))+
  geom_point(col=hue_pal()(3)[[2]],shape=2)+
  geom_errorbar(data =countries_effect, aes(ymin=Estimate-sd*1.96, ymax=Estimate+sd*1.96), col=hue_pal()(3)[[2]], 
                position = position_dodge(0.5), width = 1)+
  geom_hline(yintercept = 0,linetype="dashed", color = "red", size=1)+
  ylab(label = "Union Premium")+
  xlab(label = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15))


countries_effect_male<- lapply(unique_countries, function(x) 
  feols(fml =f,
        data2 %>%
          filter(country_name==x & sex==1),
        weights = ~pspwght,
        se="hetero") %>% 
    .$coeftable %>% 
    as.data.frame() %>% 
    head(1) %>% 
    mutate(country=x)) %>% 
  bind_rows() %>% 
  arrange(desc(Estimate)) %>% 
  mutate(country = fct_reorder(country, Estimate)) %>% 
  mutate(sex="male")

row.names(countries_effect_male) <- 1:nrow(countries_effect_male)
names(countries_effect_male)[2] <- "sd"

countries_effect_female<- lapply(unique_countries, function(x) 
  feols(fml =f,
        data2 %>%
          filter(country_name==x & sex==2),
        weights = ~pspwght,
        se="hetero") %>% 
    .$coeftable %>% 
    as.data.frame() %>% 
    head(1) %>% 
    mutate(country=x)) %>% 
  bind_rows() %>% 
  arrange(desc(Estimate)) %>% 
  mutate(country = fct_reorder(country, Estimate)) %>% 
  mutate(sex="female")

row.names(countries_effect_female) <- 1:nrow(countries_effect_female)
names(countries_effect_female)[2] <- "sd"


countries_effect_sex <- rbind(countries_effect_male,countries_effect_female)

# Decomposing the effect by gender, wasn't included in the paper
ggplot(countries_effect_sex,aes(x=country,y=Estimate,col=sex))+
  geom_point(position = position_dodge(0.5), width = 1)+
  geom_errorbar(data =countries_effect_sex, aes(ymin=Estimate-sd*1.96, ymax=Estimate+sd*1.96,col=sex), 
                position = position_dodge(0.5), width = 1)+
  geom_hline(yintercept = 0,linetype="dashed", color = "red", size=1)+
  ylab(label = "Union Premium Female")+
  xlab(label = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15))