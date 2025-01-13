# Remittances data
# Oscar Monterroso

rm(list=ls())

#Libraries
library(reshape2)
library(lares)
library(ggplot2)
library(ggfortify)
library(foreign)
library(xtable)
library(tidyverse)
library(dplyr)
library(readxl)
library(sjPlot)
library(stargazer)
library(data.table)
library(gtsummary)
library(RColorBrewer)
library(spatstat)
library(stringr)
library(scales)
library(tinytex)
library(writexl)
library(fredr)
library(zoo)
library(lubridate)
library(tidyr)
library(broom)
library(lubridate)
library(tidyr)
library(xlsx)
library(readxl)
library(openxlsx)
library(tis)
library(tseries)
library(aTSA)
library(vars)
library(urca)



# SET WORKING DIRECTORY

setwd("C:\\Users\\ADMIN\\Desktop\\RESEARCH\\Global remmitances")

# LOADING THE DATA

DATA_1 <- read_xlsx("Remittances data.xlsx",1) # Data for remittances received, current US$
DATA_2 <- read_xlsx("Remittances data.xlsx",2) # Data for remittances, % of GDP
DATA_3 <- read_xlsx("Remittances data.xlsx",3) # Data for remittances paid, current US$
DATA_4 <- read_xlsx("Remittances data.xlsx",4) # Data for remittances received, current US$ (Quarterly data)

# DATA CLEANING 

  # Remittances received, current US$
Remittances_rec <- mutate_all(DATA_1[,-1], function(x) as.numeric(as.character(x))) #Converting all data into numeric format, except for the first column
Remittances_rec[] <- lapply(Remittances_rec, function(x) x/tail(x,1) * 100) # Getting the percent structure for each year
Remittances_rec<- cbind(DATA_1[,1],Remittances_rec) # Appending the column of the country names to the data 

  # Remittances, % of GDP
Remittances_GDP <- mutate_all(DATA_2[,-1], function(x) as.numeric(as.character(x))) #Converting all data into numeric format, except for the first column
Remittances_GDP<- cbind(DATA_2[,1],Remittances_GDP) # Appending the column of the country names to the data 

  # Remittances paid, current US$
Remittances_paid <- mutate_all(DATA_3[,-1], function(x) as.numeric(as.character(x))) #Converting all data into numeric format, except for the first column
Remittances_paid[] <- lapply(Remittances_paid, function(x) x/tail(x,1) * 100) # Getting the percent structure for each year
Remittances_paid<- cbind(DATA_3[,1],Remittances_paid) # Appending the column of the country names to the data 


# TABLES. AVERAGE FOR THE PERIOD 2013-2023
  
  # Remittances received, current US$
Table_rec <- Remittances_rec %>%
  select(1,45:55) 
Table_rec <- data.frame(Table_rec[,1], Mean=rowMeans(Table_rec[,-1]))

  # Remittances, % of GDP
Table_GDP <- Remittances_GDP %>%
  select(1,45:55)
Table_GDP <- data.frame(Table_GDP[,1], Mean=rowMeans(Table_GDP[,-1]))

# EXPORT DATAFRAMES TO EXCEL

dataframe_names <- list("Sheet1"=Table_rec, "Sheet2"=Table_GDP)
openxlsx::write.xlsx(dataframe_names,file="Tables remittances.xlsx")

# PLOTS OF Y-O-Y GROWTH RATES AND NBER RECESSION DATES

  # Function to calculate growth rate annual: 
  growth_rate <- function(x)(x/lag(x)-1)*100 
  
  # Function to calculate growth rate quarterly: 
  growth_rate_q <- function(x)(x/lag(x,4)-1)*100 
  
  # Function to plot NBER recession dates. Source: Scheler (2020)
  add_rec_shade<-function(st_date,ed_date,shade_color="darkgray")
  {
    library(fredr)
    library(ecm)
    library(ggplot2)
    
    fredr_set_key("ff21fd944b1f16b5c0f7c11c86321e3d")
    
    st_date<-as.Date("1979-01-01")
    ed_date<-as.Date(Sys.Date())
    
    recession<-fredr(series_id = "USRECD",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))
    
    recession$diff<-recession$value-lagpad(recession$value,k=1)
    recession<-recession[!is.na(recession$diff),]
    recession.start<-recession[recession$diff==1,]$date
    recession.end<-recession[recession$diff==(-1),]$date
    
    if(length(recession.start)>length(recession.end))
    {recession.end<-c(recession.end,Sys.Date())}
    if(length(recession.end)>length(recession.start))
    {recession.start<-c(min(recession$date),recession.start)}
    
    recs<-as.data.frame(cbind(recession.start,recession.end))
    recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
    recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
    if(nrow(recs)>0)
    {
      rec_shade<-geom_rect(data=recs, inherit.aes=F, 
                           aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), 
                           fill=shade_color, alpha=0.5)
      return(rec_shade)
    }
  }
  
  add_rec_shade2<-function(st_date,ed_date,shade_color="darkgray")
  {
    library(fredr)
    library(ecm)
    library(ggplot2)
    
    fredr_set_key("ff21fd944b1f16b5c0f7c11c86321e3d")
    
    st_date<-as.Date("1999-01-01")
    ed_date<-as.Date(Sys.Date())
    
    recession<-fredr(series_id = "USRECD",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))
    
    recession$diff<-recession$value-lagpad(recession$value,k=1)
    recession<-recession[!is.na(recession$diff),]
    recession.start<-recession[recession$diff==1,]$date
    recession.end<-recession[recession$diff==(-1),]$date
    
    if(length(recession.start)>length(recession.end))
    {recession.end<-c(recession.end,Sys.Date())}
    if(length(recession.end)>length(recession.start))
    {recession.start<-c(min(recession$date),recession.start)}
    
    recs<-as.data.frame(cbind(recession.start,recession.end))
    recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
    recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
    if(nrow(recs)>0)
    {
      rec_shade<-geom_rect(data=recs, inherit.aes=F, 
                           aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), 
                           fill=shade_color, alpha=0.5)
      return(rec_shade)
    }
  }
  ######################## ANNUAL DATA######################## 
  # India
  remit_grate_india<- growth_rate(na.omit(as.numeric((t(DATA_1[c(90),]))))) 
  Remit_GRate_India <- cbind(data.frame(seq(as.Date("1976/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_india))
  names(Remit_GRate_India)[names(Remit_GRate_India) == 'seq.as.Date..1976.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_India)[names(Remit_GRate_India) == 'na.omit(remit_grate_india)'] <- 'Annual growth rate'
  
  India_plot<-
    ggplot(Remit_GRate_India, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="India's annual growth rate of remittances and NBER Recessions",x ="")
  
  India_plot
  
  # Mexico
  remit_grate_mexico<- growth_rate(na.omit(as.numeric((t(DATA_1[c(128),]))))) 
  Remit_GRate_Mexico <- cbind(data.frame(seq(as.Date("1980/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_mexico))
  names(Remit_GRate_Mexico)[names(Remit_GRate_Mexico) == 'seq.as.Date..1980.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Mexico)[names(Remit_GRate_Mexico) == 'na.omit(remit_grate_mexico)'] <- 'Annual growth rate'
  
  Mexico_plot<-
    ggplot(Remit_GRate_Mexico, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-25,60)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Mexico's annual growth rate of remittances and NBER Recessions",x ="")

  Mexico_plot  
  
  # Philippines
  remit_grate_phil<- growth_rate(na.omit(as.numeric((t(DATA_1[c(156),]))))) 
  Remit_GRate_Phil <- cbind(data.frame(seq(as.Date("1978/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_phil))
  names(Remit_GRate_Phil)[names(Remit_GRate_Phil) == 'seq.as.Date..1978.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Phil)[names(Remit_GRate_Phil) == 'na.omit(remit_grate_phil)'] <- 'Annual growth rate'
  
  Philippines_plot<-
    ggplot(Remit_GRate_Phil, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Philippines' annual growth rate of remittances and NBER Recessions",x ="")
  
  Philippines_plot  

  # Egypt
  remit_grate_egypt<- growth_rate(na.omit(as.numeric((t(DATA_1[c(59),]))))) 
  Remit_GRate_Egypt <- cbind(data.frame(seq(as.Date("1978/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_egypt))
  names(Remit_GRate_Egypt)[names(Remit_GRate_Egypt) == 'seq.as.Date..1978.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Egypt)[names(Remit_GRate_Egypt) == 'na.omit(remit_grate_egypt)'] <- 'Annual growth rate'  

  Egypt_plot<-
    ggplot(Remit_GRate_Egypt, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Egypt's annual growth rate of remittances and NBER Recessions",x ="")
  
  Egypt_plot
  
  # Pakistan
  remit_grate_pakistan<- growth_rate(na.omit(as.numeric((t(DATA_1[c(150),]))))) 
  Remit_GRate_Pakistan <- cbind(data.frame(seq(as.Date("1977/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_pakistan))
  names(Remit_GRate_Pakistan)[names(Remit_GRate_Pakistan) == 'seq.as.Date..1977.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Pakistan)[names(Remit_GRate_Pakistan) == 'na.omit(remit_grate_pakistan)'] <- 'Annual growth rate' 
  
  Pakistan_plot<-
    ggplot(Remit_GRate_Pakistan, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Pakistan's annual growth rate of remittances and NBER Recessions",x ="")
  
  Pakistan_plot
  
  # Nigeria
  remit_grate_nigeria<- growth_rate(na.omit(as.numeric((t(DATA_1[c(145),]))))) 
  Remit_GRate_Nigeria <- cbind(data.frame(seq(as.Date("1978/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_nigeria))
  names(Remit_GRate_Nigeria)[names(Remit_GRate_Nigeria) == 'seq.as.Date..1978.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Nigeria)[names(Remit_GRate_Nigeria) == 'na.omit(remit_grate_nigeria)'] <- 'Annual growth rate'   
  
  Nigeria_plot<-
    ggplot(Remit_GRate_Nigeria, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-100, 600)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Nigeria's annual growth rate of remittances and NBER Recessions",x ="")
  
  Nigeria_plot
  
  # Bangladesh
  remit_grate_bangladesh<- growth_rate(na.omit(as.numeric((t(DATA_1[c(16),]))))) 
  Remit_GRate_Bangladesh <- cbind(data.frame(seq(as.Date("1977/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_bangladesh))
  names(Remit_GRate_Bangladesh)[names(Remit_GRate_Bangladesh) == 'seq.as.Date..1977.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Bangladesh)[names(Remit_GRate_Bangladesh) == 'na.omit(remit_grate_bangladesh)'] <- 'Annual growth rate'
  
  Bangladesh_plot<-
    ggplot(Remit_GRate_Bangladesh, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-30, 125)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Bangladesh's annual growth rate of remittances and NBER Recessions",x ="")
  
  Bangladesh_plot
  
  # Guatemala
  remit_grate_guatemala<- growth_rate(na.omit(as.numeric((t(DATA_1[c(81),]))))) 
  Remit_GRate_Guatemala <- cbind(data.frame(seq(as.Date("1978/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_guatemala))
  names(Remit_GRate_Guatemala)[names(Remit_GRate_Guatemala) == 'seq.as.Date..1978.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Guatemala)[names(Remit_GRate_Guatemala) == 'na.omit(remit_grate_guatemala)'] <- 'Annual growth rate'   
 
  Guatemala_plot<-
    ggplot(Remit_GRate_Guatemala, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-100, 200)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Guatemala's annual growth rate of remittances and NBER Recessions",x ="")
  
  Guatemala_plot 
  
  # Morocco
  remit_grate_morocco<- growth_rate(na.omit(as.numeric((t(DATA_1[c(134),]))))) 
  Remit_GRate_Morocco <- cbind(data.frame(seq(as.Date("1976/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_morocco))
  names(Remit_GRate_Morocco)[names(Remit_GRate_Morocco) == 'seq.as.Date..1976.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Morocco)[names(Remit_GRate_Morocco) == 'na.omit(remit_grate_morocco)'] <- 'Annual growth rate'   
  
  Morocco_plot<-
    ggplot(Remit_GRate_Morocco, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Morocco's annual growth rate of remittances and NBER Recessions",x ="")
  
  Morocco_plot 
  
  # Dominican Republic
  remit_grate_dominican_republic<- growth_rate(na.omit(as.numeric((t(DATA_1[c(57),]))))) 
  Remit_GRate_Dominican_Republic <- cbind(data.frame(seq(as.Date("1971/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_dominican_republic))
  names(Remit_GRate_Dominican_Republic)[names(Remit_GRate_Dominican_Republic) == 'seq.as.Date..1971.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Dominican_Republic)[names(Remit_GRate_Dominican_Republic) == 'na.omit(remit_grate_dominican_republic)'] <- 'Annual growth rate'  
  
  Dominican_Republic_plot<-
    ggplot(Remit_GRate_Dominican_Republic, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-50, 125)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Dominican Republic's annual growth rate of remittances and NBER Recessions",x ="")
  
  Dominican_Republic_plot
  
  # Sri Lanka
  remit_grate_sri_lanka<- growth_rate(na.omit(as.numeric((t(DATA_1[c(181),]))))) 
  Remit_GRate_Sri_Lanka <- cbind(data.frame(seq(as.Date("1976/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_sri_lanka))
  names(Remit_GRate_Sri_Lanka)[names(Remit_GRate_Sri_Lanka) == 'seq.as.Date..1976.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Sri_Lanka)[names(Remit_GRate_Sri_Lanka) == 'na.omit(remit_grate_sri_lanka)'] <- 'Annual growth rate'  
 
  Sri_Lanka_plot<-
    ggplot(Remit_GRate_Sri_Lanka, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Sri Lanka's annual growth rate of remittances and NBER Recessions",x ="")
  
  Sri_Lanka_plot 
  
  # El Salvador
  remit_grate_el_salvador<- growth_rate(na.omit(as.numeric((t(DATA_1[c(60),]))))) 
  Remit_GRate_El_Salvador <- cbind(data.frame(seq(as.Date("1977/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_el_salvador))
  names(Remit_GRate_El_Salvador)[names(Remit_GRate_El_Salvador) == 'seq.as.Date..1977.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_El_Salvador)[names(Remit_GRate_El_Salvador) == 'na.omit(remit_grate_el_salvador)'] <- 'Annual growth rate'  
  
  El_Salvador_plot<-
    ggplot(Remit_GRate_El_Salvador, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-60, 75)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="El Salvador's annual growth rate of remittances and NBER Recessions",x ="")
  
  El_Salvador_plot 
  
  # Honduras
  remit_grate_honduras<- growth_rate(na.omit(as.numeric((t(DATA_1[c(86),]))))) 
  Remit_GRate_Honduras <- cbind(data.frame(seq(as.Date("1975/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_honduras))
  names(Remit_GRate_Honduras)[names(Remit_GRate_Honduras) == 'seq.as.Date..1975.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Honduras)[names(Remit_GRate_Honduras) == 'na.omit(remit_grate_honduras)'] <- 'Annual growth rate'   
  
  Honduras_plot<-
    ggplot(Remit_GRate_Honduras, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-50, 75)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Honduras' annual growth rate of remittances and NBER Recessions",x ="")
  
  Honduras_plot 
  
  # Jordan  
  remit_grate_jordan<- growth_rate(na.omit(as.numeric((t(DATA_1[c(100),]))))) 
  Remit_GRate_Jordan <- cbind(data.frame(seq(as.Date("1973/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_jordan))
  names(Remit_GRate_Jordan)[names(Remit_GRate_Jordan) == 'seq.as.Date..1973.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Jordan)[names(Remit_GRate_Jordan) == 'na.omit(remit_grate_jordan)'] <- 'Annual growth rate' 
  
  Jordan_plot<-
    ggplot(Remit_GRate_Jordan, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-50, 150)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Jordan's annual growth rate of remittances and NBER Recessions",x ="")
  
  Jordan_plot 
  
  # Ghana
  remit_grate_ghana<- growth_rate(na.omit(as.numeric((t(DATA_1[c(75),]))))) 
  Remit_GRate_Ghana <- cbind(data.frame(seq(as.Date("1980/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_ghana))
  names(Remit_GRate_Ghana)[names(Remit_GRate_Ghana) == 'seq.as.Date..1980.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Ghana)[names(Remit_GRate_Ghana) == 'na.omit(remit_grate_ghana)'] <- 'Annual growth rate'   

  Ghana_plot<-
    ggplot(Remit_GRate_Ghana, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-100, 200)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Ghana's annual growth rate of remittances and NBER Recessions",x ="")
  
  Ghana_plot
  
  # Jamaica
  
  remit_grate_jamaica<- growth_rate(na.omit(as.numeric((t(DATA_1[c(98),]))))) 
  Remit_GRate_Jamaica <- cbind(data.frame(seq(as.Date("1977/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_jamaica))
  names(Remit_GRate_Jamaica)[names(Remit_GRate_Jamaica) == 'seq.as.Date..1977.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Jamaica)[names(Remit_GRate_Jamaica) == 'na.omit(remit_grate_jamaica)'] <- 'Annual growth rate'   
  
  Jamaica_plot<-
    ggplot(Remit_GRate_Jamaica, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Jamaica's annual growth rate of remittances and NBER Recessions",x ="")
  
  Jamaica_plot
  
  # Senegal
  
  remit_grate_senegal<- growth_rate(na.omit(as.numeric((t(DATA_1[c(168),]))))) 
  Remit_GRate_Senegal <- cbind(data.frame(seq(as.Date("1975/12/31"), as.Date("2023/12/31"), "years")), na.omit(remit_grate_senegal))
  names(Remit_GRate_Senegal)[names(Remit_GRate_Senegal) == 'seq.as.Date..1975.12.31....as.Date..2023.12.31.....years..'] <- 'Date'
  names(Remit_GRate_Senegal)[names(Remit_GRate_Senegal) == 'na.omit(remit_grate_senegal)'] <- 'Annual growth rate'
  
  Senegal_plot<-
    ggplot(Remit_GRate_Senegal, aes(x=Date)) +
    add_rec_shade(as.Date("1970-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-40, 60)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Senegal's annual growth rate of remittances and NBER Recessions",x ="")
  
  Senegal_plot
  
######################## QUARTERLY DATA ########################
  
  # India
  remit_grate_q_india <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(1),]))))) 
  Remit_GRate_Q_India <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_india))
  names(Remit_GRate_Q_India)[names(Remit_GRate_Q_India) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_India)[names(Remit_GRate_Q_India) == 'na.omit(remit_grate_q_india)'] <- 'Annual growth rate'
  
  India_Q_plot<-
    ggplot(Remit_GRate_Q_India, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="India's annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  India_Q_plot
  
  # Mexico
  remit_grate_q_mexico <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(2),]))))) 
  Remit_GRate_Q_Mexico <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_mexico))
  names(Remit_GRate_Q_Mexico)[names(Remit_GRate_Q_Mexico) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Mexico)[names(Remit_GRate_Q_Mexico) == 'na.omit(remit_grate_q_mexico)'] <- 'Annual growth rate'
  
 Mexico_Q_plot<-
    ggplot(Remit_GRate_Q_Mexico, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Mexico's annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Mexico_Q_plot
  
  # Philippines
  remit_grate_q_philippines <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(3),]))))) 
  Remit_GRate_Q_Philippines <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_philippines))
  names(Remit_GRate_Q_Philippines)[names(Remit_GRate_Q_Philippines) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Philippines)[names(Remit_GRate_Q_Philippines) == 'na.omit(remit_grate_q_philippines)'] <- 'Annual growth rate'
  
  Philippines_Q_plot<-
    ggplot(Remit_GRate_Q_Philippines, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Philippines' annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Philippines_Q_plot
  
  # Bangladesh
  remit_grate_q_bangladesh <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(4),]))))) 
  Remit_GRate_Q_Bangladesh <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_bangladesh))
  names(Remit_GRate_Q_Bangladesh)[names(Remit_GRate_Q_Bangladesh) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Bangladesh)[names(Remit_GRate_Q_Bangladesh) == 'na.omit(remit_grate_q_bangladesh)'] <- 'Annual growth rate'
  
  Bangladesh_Q_plot<-
    ggplot(Remit_GRate_Q_Bangladesh, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Bangladesh' annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Bangladesh_Q_plot
  
  # Guatemala
  remit_grate_q_guatemala <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(5),]))))) 
  Remit_GRate_Q_Guatemala <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_guatemala))
  names(Remit_GRate_Q_Guatemala)[names(Remit_GRate_Q_Guatemala) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Guatemala)[names(Remit_GRate_Q_Guatemala) == 'na.omit(remit_grate_q_guatemala)'] <- 'Annual growth rate'
  
  Guatemala_Q_plot<-
    ggplot(Remit_GRate_Q_Guatemala, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    coord_cartesian(ylim=c(-50, 100)) + 
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Guatemala's annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Guatemala_Q_plot
  
  # Sri Lanka
  remit_grate_q_sri_lanka <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(6),]))))) 
  Remit_GRate_Q_Sri_Lanka <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_sri_lanka))
  names(Remit_GRate_Q_Sri_Lanka)[names(Remit_GRate_Q_Sri_Lanka) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Sri_Lanka)[names(Remit_GRate_Q_Sri_Lanka) == 'na.omit(remit_grate_q_sri_lanka)'] <- 'Annual growth rate'
  
  Sri_Lanka_Q_plot<-
    ggplot(Remit_GRate_Q_Sri_Lanka, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Sri Lanka's annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Sri_Lanka_Q_plot
  
  # El Salvador
  remit_grate_q_el_salvador <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(7),]))))) 
  Remit_GRate_Q_El_Salvador <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_el_salvador))
  names(Remit_GRate_Q_El_Salvador)[names(Remit_GRate_Q_El_Salvador) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_El_Salvador)[names(Remit_GRate_Q_El_Salvador) == 'na.omit(remit_grate_q_el_salvador)'] <- 'Annual growth rate'
  
  El_Salvador_Q_plot<-
    ggplot(Remit_GRate_Q_El_Salvador, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="El Salvador's annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  El_Salvador_Q_plot
  
  # Jordan
  remit_grate_q_jordan <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(8),]))))) 
  Remit_GRate_Q_Jordan <- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_jordan))
  names(Remit_GRate_Q_Jordan)[names(Remit_GRate_Q_Jordan) == 'seq.as.Date..2001.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Jordan)[names(Remit_GRate_Q_Jordan) == 'na.omit(remit_grate_q_jordan)'] <- 'Annual growth rate'
  
  Jordan_Q_plot<-
    ggplot(Remit_GRate_Q_Jordan, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Jordan's annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Jordan_Q_plot
  
  # Honduras
  remit_grate_q_honduras <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(9),]))))) 
  Remit_GRate_Q_Honduras <- cbind(data.frame(seq(as.Date("2005/3/31"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_honduras))
  names(Remit_GRate_Q_Honduras)[names(Remit_GRate_Q_Honduras) == 'seq.as.Date..2005.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Honduras)[names(Remit_GRate_Q_Honduras) == 'na.omit(remit_grate_q_honduras)'] <- 'Annual growth rate'

  Honduras_Q_plot<-
    ggplot(Remit_GRate_Q_Honduras, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Honduras' annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Honduras_Q_plot
  
  # Pakistan
  
  remit_grate_q_pakistan <- growth_rate_q(na.omit(as.numeric((t(DATA_4[c(10),]))))) 
  Remit_GRate_Q_Pakistan <- cbind(data.frame(seq(as.Date("2003/9/30"), as.Date("2022/12/31"), "quarters")), na.omit(remit_grate_q_pakistan))
  names(Remit_GRate_Q_Pakistan)[names(Remit_GRate_Q_Pakistan) == 'seq.as.Date..2003.9.30....as.Date..2022.12.31.....quarters..'] <- 'Date'
  names(Remit_GRate_Q_Pakistan)[names(Remit_GRate_Q_Pakistan) == 'na.omit(remit_grate_q_pakistan)'] <- 'Annual growth rate'
  
  Pakistan_Q_plot<-
    ggplot(Remit_GRate_Q_Pakistan, aes(x=Date)) +
    add_rec_shade2(as.Date("1990-01-01"),as.Date("2024-01-01"))+
    geom_line(aes(y=`Annual growth rate`),size = 0.8,color="#dd0400") +
    scale_y_continuous(name="Growth rate (%)") +
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
    labs(title="Pakistan's annual growth rate of remittances and NBER Recessions",x ="", subtitle = "Quarterly data")
  
  Pakistan_Q_plot
  

###################### VAR MODELS ######################
  
  ################ FOR ALL COUNTRIES ###################
  
    # Obtain the relevant time series
  fredr_set_key("ff21fd944b1f16b5c0f7c11c86321e3d")                                  
  Fedfr_1 <- fredr(series_id="BOGZ1FL072052006Q", observation_start = as.Date("2001-01-01"),observation_end = as.Date("2022-12-01"))
  US_outputgap_1 <- read_xls("fredgraph.xls", skip=10) 
  
    # US output gap
  plot(VAR_US_outputgap_Mexico)
  tseries::adf.test(VAR_US_outputgap_Mexico) # Visually, series does not seem stationary. Confirmed by ADF test. 
  
  plot(diff(VAR_US_outputgap_Mexico)) 
  tseries::adf.test(diff(VAR_US_outputgap_Mexico)) # After taking first difference, data looks stationary. Confirmed by ADF test
  
    # Fed funds rate
  plot(VAR_Fedfr_Mexico)
  tseries::adf.test(VAR_Fedfr_Mexico) # Visually, series does not seem stationary. Confirmed by ADF test
  
  plot(diff(VAR_Fedfr_Mexico)) 
  tseries::adf.test(diff(VAR_Fedfr_Mexico)) # After taking first difference, the series still is non-stationary. Confirmed by ADF test
  
  plot(diff(log(VAR_Fedfr_Mexico)))
  tseries::adf.test(diff(log(VAR_Fedfr_Mexico))) # After taking log-difference, the series still is non-stationary. Confirmed by ADF test
  
  plot(diff(diff(log(VAR_Fedfr_Mexico))))
  tseries::adf.test(diff(diff(log(VAR_Fedfr_Mexico)))) # After taking the difference of the log-difference, series looks more stationary. Confirmed by ADF test
  
    # Matrix for Cholesky decomposition
  amat <- diag(3)
  amat[2,1] <- NA
  amat[3,1] <- NA
  amat[3,2] <- NA
  
    ################ MEXICO ###################
  
    # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Mexico <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Mexico <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Mexico <- ts(Remit_GRate_Q_Mexico$`Annual growth rate`, start=c(2001,1), end=c(2022,4), frequency = 4) 
 
    # Stationarity of the series
    
      # Remittances
  plot(VAR_Remit_Mexico) 
  tseries::adf.test(VAR_Remit_Mexico) # Visually, data does not seem stationary. Confirmed by ADF test. 
  
  plot(diff(VAR_Remit_Mexico))
  tseries::adf.test(diff(VAR_Remit_Mexico)) # After taking first difference, data looks stationary. Confirmed by ADF test

  #  Lag selection and VAR estimation
  
  VAR_Mexico <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), diff(VAR_Remit_Mexico)), start=c(2001,3), end=c(2022,4))
  colnames(VAR_Mexico) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Mexico)
  
  info.var.mexico <- VARselect(VAR_Mexico, lag.max = 12, type = "none")
  info.var.mexico$selection # Using the information criteria, VAR(1) is the most appropriate
  
  VAR_Mexico_est <- VAR(VAR_Mexico, p=1, type = "none")
  summary(VAR_Mexico_est)
  
  # Structural VAR and IRFs

  SVAR_Mexico_est <- SVAR(VAR_Mexico_est , Amat = amat, max.iter = 1000)
  
  irf_gap_remit_mex <- irf(SVAR_Mexico_est, impulse = "US.output.gap", response = "Remittances")
  plot(irf_gap_remit_mex) # US output gap shock on remittances
  
  irf_ffr_remit_mex <- irf(SVAR_Mexico_est, impulse = "Fed.funds.rate", response = "Remittances")
  plot(irf_ffr_remit_mex) # FED funds rate shock on remittances

  ################ PHILIPPINES ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Philippines <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Philippines <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Philippines <- ts(Remit_GRate_Q_Philippines$`Annual growth rate`, start=c(2001,1), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
  # Remittances
  plot(VAR_Remit_Philippines) 
  tseries::adf.test(VAR_Remit_Philippines) # Visually, data seems somewhat stationary. Confirmed by ADF test. 
  
  #  Lag selection and VAR estimation
  
  VAR_Philippines <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), VAR_Remit_Philippines), start=c(2001,3), end=c(2022,4))
  colnames(VAR_Philippines) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Philippines)
  
  info.var.philippines <- VARselect(VAR_Philippines, lag.max = 12, type = "none")
  info.var.philippines$selection # Using the information criteria, VAR(1) is the most appropriate
  
  VAR_Philippines_est <- VAR(VAR_Philippines, p=1, type = "none")
  summary(VAR_Philippines_est)
  
  # Structural VAR and IRFs
  
  SVAR_Philippines_est <- SVAR(VAR_Philippines_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_phil <- irf(SVAR_Philippines_est, impulse = "US.output.gap", response = "Remittances")
  plot(irf_gap_remit_phil ) # US output gap shock on remittances
  
  irf_ffr_remit_phil <- irf(SVAR_Philippines_est, impulse = "Fed.funds.rate", response = "Remittances")
  plot(irf_ffr_remit_phil) # FED funds rate shock on remittances
  
  ################ BANGLADESH ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Bangladesh <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Bangladesh <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Bangladesh <- ts(Remit_GRate_Q_Bangladesh$`Annual growth rate`, start=c(2001,1), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
    # Remittances
  plot(VAR_Remit_Bangladesh) 
  tseries::adf.test(VAR_Remit_Bangladesh) # Visually, data seems somewhat stationary. ADF test says it is non-stationary.
  
  # Lag selection and VAR estimation
  
  VAR_Bangladesh <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), diff(VAR_Remit_Bangladesh)), start=c(2001,3), end=c(2022,4))
  colnames(VAR_Bangladesh) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Bangladesh)
  
  info.var.bangladesh <- VARselect(VAR_Bangladesh, lag.max = 12, type = "none")
  info.var.bangladesh$selection # Using the information criteria, VAR(4) is the most appropriate
  
  VAR_Bangladesh_est <- VAR(VAR_Bangladesh, p=4, type = "none")
  summary(VAR_Bangladesh_est)
  
  # Structural VAR and IRFs
  
  SVAR_Bangladesh_est <- SVAR(VAR_Bangladesh_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_bang <- irf(SVAR_Bangladesh_est, impulse = "US.output.gap", response = "Remittances", n.ahead=20)
  plot(irf_gap_remit_bang) # US output gap shock on remittances
  
  irf_ffr_remit_bang <- irf(SVAR_Bangladesh_est, impulse = "Fed.funds.rate", response = "Remittances", n.ahead=20)
  plot(irf_ffr_remit_bang) # FED funds rate shock on remittances
  
  ################ GUATEMALA ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Guatemala <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Guatemala <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Guatemala <- ts(Remit_GRate_Q_Guatemala$`Annual growth rate`, start=c(2001,1), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
    # Remittances
  plot(VAR_Remit_Guatemala) 
  tseries::adf.test(VAR_Remit_Guatemala) # Visually, data seems somewhat stationary. ADF test says it is non-stationary.
  
  # Lag selection and VAR estimation
  
  VAR_Guatemala <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), diff(VAR_Remit_Guatemala)), start=c(2001,3), end=c(2022,4))
  colnames(VAR_Guatemala) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Guatemala)
  
  info.var.guatemala <- VARselect(VAR_Guatemala, lag.max = 12, type = "none")
  info.var.guatemala$selection # Using the information criteria, VAR(1) is the most appropriate
  
  VAR_Guatemala_est <- VAR(VAR_Guatemala, p=1, type = "none")
  summary(VAR_Guatemala_est)
  
  # Structural VAR and IRFs
  
  SVAR_Guatemala_est <- SVAR(VAR_Guatemala_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_gua <- irf(SVAR_Guatemala_est, impulse = "US.output.gap", response = "Remittances", n.ahead=10)
  plot(irf_gap_remit_gua) # US output gap shock on remittances
  
  irf_ffr_remit_gua <- irf(SVAR_Guatemala_est, impulse = "Fed.funds.rate", response = "Remittances", n.ahead=10)
  plot(irf_ffr_remit_gua) # FED funds rate shock on remittances
  
  ################ SRI LANKA ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Sri_Lanka <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Sri_Lanka <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Sri_Lanka <- ts(Remit_GRate_Q_Sri_Lanka$`Annual growth rate`, start=c(2001,1), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
  # Remittances
  plot(VAR_Remit_Sri_Lanka) 
  tseries::adf.test(VAR_Remit_Sri_Lanka) # Visually, data seems somewhat stationary. Confirmed by ADF test.
  
  # Lag selection and VAR estimation
  
  VAR_Sri_Lanka <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), VAR_Remit_Sri_Lanka), start=c(2001,3), end=c(2022,4))
  colnames(VAR_Sri_Lanka) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Sri_Lanka)
  
  info.var.sri.lanka <- VARselect(VAR_Sri_Lanka, lag.max = 12, type = "none")
  info.var.sri.lanka$selection # Using the information criteria, VAR(1) is the most appropriate
  
  VAR_Sri_Lanka_est <- VAR(VAR_Sri_Lanka, p=1, type = "none")
  summary(VAR_Sri_Lanka_est)
  
  # Structural VAR and IRFs
  
  SVAR_Sri_Lanka_est <- SVAR(VAR_Sri_Lanka_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_srilan <- irf(SVAR_Sri_Lanka_est, impulse = "US.output.gap", response = "Remittances", n.ahead=10)
  plot(irf_gap_remit_srilan) # US output gap shock on remittances
  
  irf_ffr_remit_srilan <- irf(SVAR_Sri_Lanka_est, impulse = "Fed.funds.rate", response = "Remittances", n.ahead=10)
  plot(irf_ffr_remit_srilan) # FED funds rate shock on remittances
  
  ################ EL SALVADOR ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_El_Salvador <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_El_Salvador <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_El_Salvador <- ts(Remit_GRate_Q_El_Salvador$`Annual growth rate`, start=c(2001,1), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
    # Remittances
  plot(VAR_Remit_El_Salvador) 
  tseries::adf.test(VAR_Remit_El_Salvador) # Visually, data seems somewhat stationary. ADF test says it is non-stationary
  
  # Lag selection and VAR estimation
  
  VAR_El_Salvador <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), diff(VAR_Remit_El_Salvador)), start=c(2001,3), end=c(2022,4))
  colnames(VAR_El_Salvador) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_El_Salvador)
  
  info.var.el.salvador <- VARselect(VAR_El_Salvador, lag.max = 12, type = "none")
  info.var.el.salvador$selection # Using the information criteria, VAR(4) is the most appropriate
  
  VAR_El_Salvador_est <- VAR(VAR_El_Salvador, p=4, type = "none")
  summary(VAR_El_Salvador_est)
  
  # Structural VAR and IRFs
  
  SVAR_El_Salvador_est <- SVAR(VAR_El_Salvador_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_elsal <- irf(SVAR_El_Salvador_est, impulse = "US.output.gap", response = "Remittances", n.ahead=20)
  plot(irf_gap_remit_elsal) # US output gap shock on remittances
  
  irf_ffr_remit_elsal <- irf(SVAR_El_Salvador_est, impulse = "Fed.funds.rate", response = "Remittances", n.ahead=20)
  plot(irf_ffr_remit_elsal) # FED funds rate shock on remittances
  
  ################ JORDAN ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Jordan <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Jordan <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Jordan <- ts(Remit_GRate_Q_Jordan$`Annual growth rate`, start=c(2001,1), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
    # Remittances
  plot(VAR_Remit_Jordan) 
  tseries::adf.test(VAR_Remit_Jordan) # Visually, data seems somewhat stationary. Confirmed by ADF test
  
  # Lag selection and VAR estimation
  
  VAR_Jordan <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), VAR_Remit_Jordan), start=c(2001,3), end=c(2022,4))
  colnames(VAR_Jordan) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Jordan)
  
  info.var.jordan <- VARselect(VAR_Jordan, lag.max = 12, type = "none")
  info.var.jordan$selection # Using the information criteria, VAR(1) is the most appropriate
  
  VAR_Jordan_est <- VAR(VAR_Jordan, p=1, type = "none")
  summary(VAR_Jordan_est)
  
  # Structural VAR and IRFs
  
  SVAR_Jordan_est <- SVAR(VAR_Jordan_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_jord <- irf(SVAR_Jordan_est, impulse = "US.output.gap", response = "Remittances", n.ahead=20)
  plot(irf_gap_remit_jord) # US output gap shock on remittances
  
  irf_ffr_remit_jord <- irf(SVAR_Jordan_est, impulse = "Fed.funds.rate", response = "Remittances", n.ahead=20)
  plot(irf_ffr_remit_jord) # FED funds rate shock on remittances
  
  ################ HONDURAS ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Honduras <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Honduras <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Honduras <- ts(Remit_GRate_Q_Honduras$`Annual growth rate`, start=c(2005,1), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
    # Remittances
  plot(VAR_Remit_Honduras) 
  tseries::adf.test(VAR_Remit_Honduras) # Visually, data seems somewhat stationary. Confirmed by ADF test
  
  # Lag selection and VAR estimation
  
  VAR_Honduras <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), VAR_Remit_Honduras), start=c(2005,1), end=c(2022,4))
  colnames(VAR_Honduras) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Honduras)
  
  info.var.honduras <- VARselect(VAR_Honduras, lag.max = 12, type = "none")
  info.var.honduras$selection # Using the information criteria, VAR(1) is the most appropriate
  
  VAR_Honduras_est <- VAR(VAR_Honduras, p=1, type = "none")
  summary(VAR_Honduras_est)
  
  # Structural VAR and IRFs
  
  SVAR_Honduras_est <- SVAR(VAR_Honduras_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_hond <- irf(SVAR_Honduras_est, impulse = "US.output.gap", response = "Remittances", n.ahead=20)
  plot(irf_gap_remit_hond) # US output gap shock on remittances
  
  irf_ffr_remit_hond <- irf(SVAR_Honduras_est, impulse = "Fed.funds.rate", response = "Remittances", n.ahead=20)
  plot(irf_ffr_remit_hond) # FED funds rate shock on remittances
  
  ################ PAKISTAN ###################
  
  # Convert series to ts objects and merge them in a single data frame
  VAR_US_outputgap_Pakistan <- ts(US_outputgap_1$GDPC1_GDPPOT, start=c(2001,1), end=c(2022,4), frequency = 4) 
  VAR_Fedfr_Pakistan <- ts(Fedfr_1$value, start=c(2001,1), end=c(2022,4), frequency = 4)
  VAR_Remit_Pakistan <- ts(Remit_GRate_Q_Pakistan$`Annual growth rate`, start=c(2003,3), end=c(2022,4), frequency = 4) 
  
  # Stationarity of the series
  
    # Remittances
  plot(VAR_Remit_Pakistan) 
  tseries::adf.test(VAR_Remit_Pakistan) # Visually, data seems somewhat stationary. ADF test says it is non-stationary
  
  # Lag selection and VAR estimation
  
  VAR_Pakistan <- window(ts.union(diff(VAR_US_outputgap_Mexico), diff(diff(log(VAR_Fedfr_Mexico))), diff(VAR_Remit_Pakistan)), start=c(2003,4), end=c(2022,4))
  colnames(VAR_Pakistan) <- c("US output gap", "Fed funds rate", "Remittances")
  plot.ts(VAR_Pakistan)
  
  info.var.pakistan <- VARselect(VAR_Pakistan, lag.max = 12, type = "none")
  info.var.pakistan$selection # Using the information criteria, VAR(1) is the most appropriate
  
  VAR_Pakistan_est <- VAR(VAR_Pakistan, p=1, type = "none")
  summary(VAR_Pakistan_est)
  
  # Structural VAR and IRFs
  
  SVAR_Pakistan_est <- SVAR(VAR_Pakistan_est, Amat = amat, max.iter = 1000)
  
  irf_gap_remit_pakis <- irf(SVAR_Pakistan_est, impulse = "US.output.gap", response = "Remittances", n.ahead=10)
  plot(irf_gap_remit_pakis) # US output gap shock on remittances
  
  irf_ffr_remit_pakis <- irf(SVAR_Pakistan_est, impulse = "Fed.funds.rate", response = "Remittances", n.ahead=10)
  plot(irf_ffr_remit_pakis) # FED funds rate shock on remittances
  