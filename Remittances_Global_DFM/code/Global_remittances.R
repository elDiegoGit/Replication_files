# Remittances data
# Oscar Monterroso

rm(list=ls())

#Libraries
library(ggplot2)
library(xtable)
library(dplyr)
library(readxl)
library(scales)
library(writexl)
library(fredr)
library(zoo)
library(lubridate)
library(tidyr)
library(xlsx)
library(tis)
library(gridExtra)
library(cowplot)
library(vars)
library(svars)
library(grid)
library(gridBase)
library(gridExtra)
library(MARSS)
library(data.table)

# SET WORKING DIRECTORY

setwd("C:\\Users\\ADMIN\\Desktop\\RESEARCH\\Global remmitances")

# Function to calculate growth rate quarterly data 
growth_rate_q <- function(x)(x/dplyr::lag(x,4)-1)*100

# Function to plot NBER recession dates. Source: Scheler (2020)
add_rec_shade2<-function(st_date,ed_date,shade_color="darkgray")
{
  library(fredr)
  library(ecm)
  library(ggplot2)
  
  fredr_set_key("ff21fd944b1f16b5c0f7c11c86321e3d")
  
  st_date<-as.Date("1976-01-01")
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

# ---------------------- GRAPHS ----------------------#

# EL SALVADOR

DATA_El_Salvador <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",1))) # Load data for remittances
DATA_El_Salvador <- as.data.frame(as.numeric(DATA_El_Salvador[-1,])) # Remove first row and convert data to numeric format

Remittances_El_Salvador <- cbind(data.frame(seq(as.Date("1999/3/31"), as.Date("2024/3/31"), "quarters")), DATA_El_Salvador) # Create dataframe with dates and values of remittances

names(Remittances_El_Salvador)[names(Remittances_El_Salvador) == 'seq.as.Date..1999.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_El_Salvador)[names(Remittances_El_Salvador) == 'as.numeric(DATA_El_Salvador[-1, ])'] <- 'Remittances'

Remittances_El_Salvador$Growth_rate <- growth_rate_q(Remittances_El_Salvador$Remittances) # Add column with growth rates

EL_Salvador_plot<-
  ggplot(Remittances_El_Salvador, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2000-03-31','2024-03-31')),
               breaks = seq(as.Date("2000-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-20,60, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-20,60, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="El Salvador", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-20, 60))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

EL_Salvador_plot

# GUATEMALA

DATA_Guatemala <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",2))) # Load data for remittances
DATA_Guatemala<- as.data.frame(as.numeric(DATA_Guatemala[-1,])) # Remove first row and convert data to numeric format

Remittances_Guatemala <- cbind(data.frame(seq(as.Date("1988/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Guatemala) # Create dataframe with dates and values of remittances

names(Remittances_Guatemala)[names(Remittances_Guatemala) == 'seq.as.Date..1988.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Guatemala)[names(Remittances_Guatemala) == 'as.numeric(DATA_Guatemala[-1, ])'] <- 'Remittances'

Remittances_Guatemala$Growth_rate <- growth_rate_q(Remittances_Guatemala$Remittances) # Add column with growth rates

Guatemala_plot<-
  ggplot(Remittances_Guatemala, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-50,250, by=50), sec.axis = sec_axis(~.,name="", breaks = seq(-50,250, by=50)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Guatemala", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-50, 250))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Guatemala_plot

# HONDURAS

DATA_Honduras <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",3))) # Load data for remittances
DATA_Honduras<- as.data.frame(as.numeric(DATA_Honduras[-1,])) # Remove first row and convert data to numeric format

Remittances_Honduras <- cbind(data.frame(seq(as.Date("2004/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Honduras) # Create dataframe with dates and values of remittances

names(Remittances_Honduras)[names(Remittances_Honduras) == 'seq.as.Date..2004.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Honduras)[names(Remittances_Honduras) == 'as.numeric(DATA_Honduras[-1, ])'] <- 'Remittances'

Remittances_Honduras$Growth_rate <- growth_rate_q(Remittances_Honduras$Remittances) # Add column with growth rates

Honduras_plot<-
  ggplot(Remittances_Honduras, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2005-03-31','2024-03-31')),
               breaks = seq(as.Date("2005-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-20,70, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-20,70, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Honduras", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-20, 70))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Honduras_plot

# MÉXICO

DATA_Mexico <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",4))) # Load data for remittances
DATA_Mexico <- as.data.frame(as.numeric(DATA_Mexico [-1,])) # Remove first row and convert data to numeric format

Remittances_Mexico <- cbind(data.frame(seq(as.Date("1980/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Mexico) # Create dataframe with dates and values of remittances

names(Remittances_Mexico)[names(Remittances_Mexico) == 'seq.as.Date..1980.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Mexico)[names(Remittances_Mexico) == 'as.numeric(DATA_Mexico[-1, ])'] <- 'Remittances'

Remittances_Mexico$Growth_rate <- growth_rate_q(Remittances_Mexico$Remittances) # Add column with growth rates

Mexico_plot<-
  ggplot(Remittances_Mexico, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-30,70, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-30,70, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Mexico", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-30, 70))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Mexico_plot

# ECUADOR 

DATA_Ecuador <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",5))) # Load data for remittances
DATA_Ecuador <- as.data.frame(as.numeric(DATA_Ecuador[-1,])) # Remove first row and convert data to numeric format

Remittances_Ecuador <- cbind(data.frame(seq(as.Date("1993/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Ecuador) # Create dataframe with dates and values of remittances

names(Remittances_Ecuador)[names(Remittances_Ecuador) == 'seq.as.Date..1993.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Ecuador)[names(Remittances_Ecuador) == 'as.numeric(DATA_Ecuador[-1, ])'] <- 'Remittances'

Remittances_Ecuador$Growth_rate <- growth_rate_q(Remittances_Ecuador$Remittances) # Add column with growth rates

Ecuador_plot<-
  ggplot(Remittances_Ecuador, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-30,70, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-30,70, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Ecuador", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-30, 70))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Ecuador_plot

# COLOMBIA

DATA_Colombia <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",6))) # Load data for remittances
DATA_Colombia <- as.data.frame(as.numeric(DATA_Colombia[-1,])) # Remove first row and convert data to numeric format

Remittances_Colombia <- cbind(data.frame(seq(as.Date("1996/3/31"), as.Date("2024/07/01"), "quarters")), DATA_Colombia) # Create dataframe with dates and values of remittances

names(Remittances_Colombia)[names(Remittances_Colombia) == 'seq.as.Date..1996.3.31....as.Date..2024.07.01.....quarters..'] <- 'Date' # Change column names
names(Remittances_Colombia)[names(Remittances_Colombia) == 'as.numeric(DATA_Colombia[-1, ])'] <- 'Remittances'

Remittances_Colombia$Growth_rate <- growth_rate_q(Remittances_Colombia$Remittances) # Add column with growth rates

Colombia_plot<-
  ggplot(Remittances_Colombia, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-50,150, by=25), sec.axis = sec_axis(~.,name="", breaks = seq(-50,150, by=25)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Colombia", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-50, 150))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Colombia_plot

# PHILIPPPINES

DATA_Philippines <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",7))) # Load data for remittances
DATA_Philippines  <- as.data.frame(as.numeric(DATA_Philippines[-1,])) # Remove first row and convert data to numeric format

Remittances_Philippines <- cbind(data.frame(seq(as.Date("1999/3/31"), as.Date("2024/07/01"), "quarters")), DATA_Philippines) # Create dataframe with dates and values of remittances

names(Remittances_Philippines)[names(Remittances_Philippines) == 'seq.as.Date..1999.3.31....as.Date..2024.07.01.....quarters..'] <- 'Date' # Change column names
names(Remittances_Philippines)[names(Remittances_Philippines) == 'as.numeric(DATA_Philippines[-1, ])'] <- 'Remittances'

Remittances_Philippines$Growth_rate <- growth_rate_q(Remittances_Philippines$Remittances) # Add column with growth rates

Philippines_plot<-
  ggplot(Remittances_Philippines, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2000-03-31','2024-03-31')),
               breaks = seq(as.Date("2000-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-20,50, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-20,50, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Philippines", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-20, 50))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Philippines_plot

# PAPUA NEW GUINEA

DATA_Papua_New_Guinea <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",8))) # Load data for remittances
DATA_Papua_New_Guinea <- as.data.frame(as.numeric(DATA_Papua_New_Guinea[-1,])) # Remove first row and convert data to numeric format

Remittances_Papua_New_Guinea <- cbind(data.frame(seq(as.Date("1999/3/31"), as.Date("2023/12/31"), "quarters")), DATA_Papua_New_Guinea) # Create dataframe with dates and values of remittances

names(Remittances_Papua_New_Guinea)[names(Remittances_Papua_New_Guinea) == 'seq.as.Date..1999.3.31....as.Date..2023.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Papua_New_Guinea)[names(Remittances_Papua_New_Guinea) == 'as.numeric(DATA_Papua_New_Guinea[-1, ])'] <- 'Remittances'

Remittances_Papua_New_Guinea$Growth_rate <- growth_rate_q(Remittances_Papua_New_Guinea$Remittances) # Add column with growth rates

Papua_New_Guinea_plot<-
  ggplot(Remittances_Papua_New_Guinea, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('2000-03-31','2023-12-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Papua New Guinea", x ="")

Papua_New_Guinea_plot

# BANGLADESH

DATA_Bangladesh <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",9))) # Load data for remittances
DATA_Bangladesh <- as.data.frame(as.numeric(DATA_Bangladesh[-1,])) # Remove first row and convert data to numeric format

Remittances_Bangladesh <- cbind(data.frame(seq(as.Date("1988/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Bangladesh) # Create dataframe with dates and values of remittances

names(Remittances_Bangladesh)[names(Remittances_Bangladesh) == 'seq.as.Date..1988.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Bangladesh)[names(Remittances_Bangladesh) == 'as.numeric(DATA_Bangladesh[-1, ])'] <- 'Remittances'

Remittances_Bangladesh$Growth_rate <- growth_rate_q(Remittances_Bangladesh$Remittances) # Add column with growth rates

Bangladesh_plot<-
  ggplot(Remittances_Bangladesh, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-30,60, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-30,60, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Bangladesh", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-30, 60))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Bangladesh_plot

# INDIA

DATA_India <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",10))) # Load data for remittances
DATA_India <- as.data.frame(as.numeric(DATA_India[-1,])) # Remove first row and convert data to numeric format

Remittances_India <- cbind(data.frame(seq(as.Date("1998/3/31"), as.Date("2024/3/31"), "quarters")), DATA_India) # Create dataframe with dates and values of remittances

names(Remittances_India)[names(Remittances_India) == 'seq.as.Date..1998.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_India)[names(Remittances_India) == 'as.numeric(DATA_India[-1, ])'] <- 'Remittances'

Remittances_India$Growth_rate <- growth_rate_q(Remittances_India$Remittances) # Add column with growth rates

India_plot<-
  ggplot(Remittances_India, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1999-03-31','2024-03-31')),
               breaks = seq(as.Date("1999-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-50,70, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-50,70, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="India", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-50, 70))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

India_plot

# THAILAND

DATA_Thailand <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",11))) # Load data for remittances
DATA_Thailand <- as.data.frame(as.numeric(DATA_Thailand[-1,])) # Remove first row and convert data to numeric format

Remittances_Thailand<- cbind(data.frame(seq(as.Date("2005/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Thailand) # Create dataframe with dates and values of remittances

names(Remittances_Thailand)[names(Remittances_Thailand) == 'seq.as.Date..2005.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Thailand)[names(Remittances_Thailand) == 'as.numeric(DATA_Thailand[-1, ])'] <- 'Remittances'

Remittances_Thailand$Growth_rate <- growth_rate_q(Remittances_Thailand$Remittances) # Add column with growth rates

Thailand_plot<-
  ggplot(Remittances_Thailand, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2006-03-31','2024-03-31')),
               breaks = seq(as.Date("2006-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-100, 300, by=50), sec.axis = sec_axis(~.,name="", breaks = seq(-100, 300, by=50)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Thailand", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-100, 300))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Thailand_plot

# VIETNAM

DATA_Vietnam <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",12))) # Load data for remittances
DATA_Vietnam <- as.data.frame(as.numeric(DATA_Vietnam[-1,])) # Remove first row and convert data to numeric format

Remittances_Vietnam<- cbind(data.frame(seq(as.Date("1996/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Vietnam) # Create dataframe with dates and values of remittances

names(Remittances_Vietnam)[names(Remittances_Vietnam) == 'seq.as.Date..1996.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Vietnam)[names(Remittances_Vietnam) == 'as.numeric(DATA_Vietnam[-1, ])'] <- 'Remittances'

Remittances_Vietnam$Growth_rate <- growth_rate_q(Remittances_Vietnam$Remittances) # Add column with growth rates

Vietnam_plot<-
  ggplot(Remittances_Vietnam, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-100, 300, by=50), sec.axis = sec_axis(~.,name="", breaks = seq(-100, 300, by=50)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Vietnam", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-100, 300))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Vietnam_plot

# FIJI

DATA_Fiji <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",13))) # Load data for remittances
DATA_Fiji <- as.data.frame(as.numeric(DATA_Fiji[-1,])) # Remove first row and convert data to numeric format

Remittances_Fiji<- cbind(data.frame(seq(as.Date("2000/3/31"), as.Date("2023/07/01"), "quarters")), DATA_Fiji) # Create dataframe with dates and values of remittances

names(Remittances_Fiji)[names(Remittances_Fiji) == 'seq.as.Date..2000.3.31....as.Date..2023.07.01.....quarters..'] <- 'Date' # Change column names
names(Remittances_Fiji)[names(Remittances_Fiji) == 'as.numeric(DATA_Fiji[-1, ])'] <- 'Remittances'

Remittances_Fiji$Growth_rate <- growth_rate_q(Remittances_Fiji$Remittances) # Add column with growth rates

Fiji_plot<-
  ggplot(Remittances_Fiji, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('2001-03-31','2023-07-01')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Fiji", x ="")

Fiji_plot

# SUDAN

DATA_Sudan <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",14))) # Load data for remittances
DATA_Sudan <- as.data.frame(as.numeric(DATA_Sudan[-1,])) # Remove first row and convert data to numeric format

Remittances_Sudan<- cbind(data.frame(seq(as.Date("1977/3/31"), as.Date("2022/12/31"), "quarters")), DATA_Sudan) # Create dataframe with dates and values of remittances

names(Remittances_Sudan)[names(Remittances_Sudan) == 'seq.as.Date..1977.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Sudan)[names(Remittances_Sudan) == 'as.numeric(DATA_Sudan[-1, ])'] <- 'Remittances'

Remittances_Sudan$Growth_rate <- growth_rate_q(Remittances_Sudan$Remittances) # Add column with growth rates

Sudan_plot<-
  ggplot(Remittances_Sudan, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  coord_cartesian(ylim=c(-500, 500)) +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('1978-03-31','2022-12-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Sudan", x ="")

Sudan_plot

# ETHIOPIA

DATA_Ethiopia <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",15))) # Load data for remittances
DATA_Ethiopia <- as.data.frame(as.numeric(DATA_Ethiopia[-1,])) # Remove first row and convert data to numeric format

Remittances_Ethiopia<- cbind(data.frame(seq(as.Date("1997/3/31"), as.Date("2023/12/31"), "quarters")), DATA_Ethiopia) # Create dataframe with dates and values of remittances

names(Remittances_Ethiopia)[names(Remittances_Ethiopia) == 'seq.as.Date..1997.3.31....as.Date..2023.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Ethiopia)[names(Remittances_Ethiopia) == 'as.numeric(DATA_Ethiopia[-1, ])'] <- 'Remittances'

Remittances_Ethiopia$Growth_rate <- growth_rate_q(Remittances_Ethiopia$Remittances) # Add column with growth rates

Ethiopia_plot<-
  ggplot(Remittances_Ethiopia, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  coord_cartesian(ylim=c(-200, 200)) +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('1998-03-31','2023-12-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Ethiopia", x ="")

Ethiopia_plot

# MADAGASCAR

DATA_Madagascar <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",16))) # Load data for remittances
DATA_Madagascar <- as.data.frame(as.numeric(DATA_Madagascar[-1,])) # Remove first row and convert data to numeric format

Remittances_Madagascar<- cbind(data.frame(seq(as.Date("2003/3/31"), as.Date("2022/12/31"), "quarters")), DATA_Madagascar) # Create dataframe with dates and values of remittances

names(Remittances_Madagascar)[names(Remittances_Madagascar) == 'seq.as.Date..2003.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Madagascar)[names(Remittances_Madagascar) == 'as.numeric(DATA_Madagascar[-1, ])'] <- 'Remittances'

Remittances_Madagascar$Growth_rate <- growth_rate_q(Remittances_Madagascar$Remittances) # Add column with growth rates

Madagascar_plot<-
  ggplot(Remittances_Madagascar, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  coord_cartesian(ylim=c(-200, 300))+
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('2004-03-31','2022-12-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Madagascar", x ="")

Madagascar_plot

# LESOTHO

DATA_Lesotho <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",17))) # Load data for remittances
DATA_Lesotho <- as.data.frame(as.numeric(DATA_Lesotho[-1,])) # Remove first row and convert data to numeric format

Remittances_Lesotho<- cbind(data.frame(seq(as.Date("1982/3/31"), as.Date("2024/07/01"), "quarters")), DATA_Lesotho) # Create dataframe with dates and values of remittances

names(Remittances_Lesotho)[names(Remittances_Lesotho) == 'seq.as.Date..1982.3.31....as.Date..2024.07.01.....quarters..'] <- 'Date' # Change column names
names(Remittances_Lesotho)[names(Remittances_Lesotho) == 'as.numeric(DATA_Lesotho[-1, ])'] <- 'Remittances'

Remittances_Lesotho$Growth_rate <- growth_rate_q(Remittances_Lesotho$Remittances) # Add column with growth rates

Lesotho_plot<-
  ggplot(Remittances_Lesotho, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-50, 125, by=25), sec.axis = sec_axis(~.,name="", breaks = seq(-50, 125, by=25)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Lesotho", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-50, 125))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Lesotho_plot

# UGANDA

DATA_Uganda <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",18))) # Load data for remittances
DATA_Uganda <- as.data.frame(as.numeric(DATA_Uganda[-1,])) # Remove first row and convert data to numeric format

Remittances_Uganda<- cbind(data.frame(seq(as.Date("2001/3/31"), as.Date("2023/10/01"), "quarters")), DATA_Uganda) # Create dataframe with dates and values of remittances

names(Remittances_Uganda)[names(Remittances_Uganda) == 'seq.as.Date..2001.3.31....as.Date..2023.10.01.....quarters..'] <- 'Date' # Change column names
names(Remittances_Uganda)[names(Remittances_Uganda) == 'as.numeric(DATA_Uganda[-1, ])'] <- 'Remittances'

Remittances_Uganda$Growth_rate <- growth_rate_q(Remittances_Uganda$Remittances) # Add column with growth rates

Uganda_plot<-
  ggplot(Remittances_Uganda, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2005-03-31','2024-03-31')),
               breaks = seq(as.Date("2005-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-75, 200, by=25), sec.axis = sec_axis(~.,name="", breaks = seq(-75, 200, by=25)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Uganda", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-75, 200))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Uganda_plot

# GHANA

DATA_Ghana <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",19))) # Load data for remittances
DATA_Ghana <- as.data.frame(as.numeric(DATA_Ghana[-1,])) # Remove first row and convert data to numeric format

Remittances_Ghana<- cbind(data.frame(seq(as.Date("2011/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Ghana) # Create dataframe with dates and values of remittances

names(Remittances_Ghana)[names(Remittances_Ghana) == 'seq.as.Date..2011.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Ghana)[names(Remittances_Ghana) == 'as.numeric(DATA_Ghana[-1, ])'] <- 'Remittances'

Remittances_Ghana$Growth_rate <- growth_rate_q(Remittances_Ghana$Remittances) # Add column with growth rates

Ghana_plot<-
  ggplot(Remittances_Ghana, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('2012-03-31','2024-03-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Ghana", x ="")

Ghana_plot

# JORDAN

DATA_Jordan <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",20))) # Load data for remittances
DATA_Jordan <- as.data.frame(as.numeric(DATA_Jordan[-1,])) # Remove first row and convert data to numeric format

Remittances_Jordan<- cbind(data.frame(seq(as.Date("1977/3/31"), as.Date("2022/12/31"), "quarters")), DATA_Jordan) # Create dataframe with dates and values of remittances

names(Remittances_Jordan)[names(Remittances_Jordan) == 'seq.as.Date..1977.3.31....as.Date..2022.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Jordan)[names(Remittances_Jordan) == 'as.numeric(DATA_Jordan[-1, ])'] <- 'Remittances'

Remittances_Jordan$Growth_rate <- growth_rate_q(Remittances_Jordan$Remittances) # Add column with growth rates

Jordan_plot<-
  ggplot(Remittances_Jordan, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-30, 40, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-30, 40, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Jordan", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-30, 40))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Jordan_plot

# LEBANON

DATA_Lebanon <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",21))) # Load data for remittances
DATA_Lebanon <- as.data.frame(as.numeric(DATA_Lebanon[-1,])) # Remove first row and convert data to numeric format

Remittances_Lebanon<- cbind(data.frame(seq(as.Date("2002/3/31"), as.Date("2023/12/31"), "quarters")), DATA_Lebanon) # Create dataframe with dates and values of remittances

names(Remittances_Lebanon)[names(Remittances_Lebanon) == 'seq.as.Date..2002.3.31....as.Date..2023.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Lebanon)[names(Remittances_Lebanon) == 'as.numeric(DATA_Lebanon[-1, ])'] <- 'Remittances'

Remittances_Lebanon$Growth_rate <- growth_rate_q(Remittances_Lebanon$Remittances) # Add column with growth rates

Lebanon_plot<-
  ggplot(Remittances_Lebanon, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2007-03-31','2024-03-31')),
               breaks = seq(as.Date("2007-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-100, 350, by=50), sec.axis = sec_axis(~.,name="", breaks = seq(-100, 350, by=50)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Lebanon", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-100, 350))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Lebanon_plot

# SAUDI ARABIA

DATA_Saudi_Arabia <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",22))) # Load data for remittances
DATA_Saudi_Arabia <- as.data.frame(as.numeric(DATA_Saudi_Arabia[-1,])) # Remove first row and convert data to numeric format

Remittances_Saudi_Arabia<- cbind(data.frame(seq(as.Date("2006/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Saudi_Arabia) # Create dataframe with dates and values of remittances

names(Remittances_Saudi_Arabia)[names(Remittances_Saudi_Arabia) == 'seq.as.Date..2006.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Saudi_Arabia)[names(Remittances_Saudi_Arabia) == 'as.numeric(DATA_Saudi_Arabia[-1, ])'] <- 'Remittances'

Remittances_Saudi_Arabia$Growth_rate <- growth_rate_q(Remittances_Saudi_Arabia$Remittances) # Add column with growth rates

Saudi_Arabia_plot<-
  ggplot(Remittances_Saudi_Arabia, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('2007-03-31','2024-03-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Saudi Arabia", x ="")

Saudi_Arabia_plot

# PAKISTAN

DATA_Pakistan <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",23))) # Load data for remittances
DATA_Pakistan <- as.data.frame(as.numeric(DATA_Pakistan[-1,])) # Remove first row and convert data to numeric format

Remittances_Pakistan<- cbind(data.frame(seq(as.Date("1976/3/31"), as.Date("2013/12/31"), "quarters")), DATA_Pakistan) # Create dataframe with dates and values of remittances

names(Remittances_Pakistan)[names(Remittances_Pakistan) == 'seq.as.Date..1976.3.31....as.Date..2013.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Pakistan)[names(Remittances_Pakistan) == 'as.numeric(DATA_Pakistan[-1, ])'] <- 'Remittances'

Remittances_Pakistan$Growth_rate <- growth_rate_q(Remittances_Pakistan$Remittances) # Add column with growth rates

Pakistan_plot<-
  ggplot(Remittances_Pakistan, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('1977-03-31','2013-12-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Pakistan", x ="")

Pakistan_plot

# SRI LANKA

DATA_Sri_Lanka <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",24))) # Load data for remittances
DATA_Sri_Lanka <- as.data.frame(as.numeric(DATA_Sri_Lanka[-1,])) # Remove first row and convert data to numeric format

Remittances_Sri_Lanka<- cbind(data.frame(seq(as.Date("1977/3/31"), as.Date("2023/12/31"), "quarters")), DATA_Sri_Lanka) # Create dataframe with dates and values of remittances

names(Remittances_Sri_Lanka)[names(Remittances_Sri_Lanka) == 'seq.as.Date..1977.3.31....as.Date..2023.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Sri_Lanka)[names(Remittances_Sri_Lanka) == 'as.numeric(DATA_Sri_Lanka[-1, ])'] <- 'Remittances'

Remittances_Sri_Lanka$Growth_rate <- growth_rate_q(Remittances_Sri_Lanka$Remittances) # Add column with growth rates

Sri_Lanka_plot<-
  ggplot(Remittances_Sri_Lanka, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-75, 100, by=25), sec.axis = sec_axis(~.,name="", breaks = seq(-75, 100, by=25)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Sri Lanka", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-75, 100))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Sri_Lanka_plot

# INDONESIA

DATA_Indonesia <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",25))) # Load data for remittances
DATA_Indonesia <- as.data.frame(as.numeric(DATA_Indonesia[-1,])) # Remove first row and convert data to numeric format

Remittances_Indonesia<- cbind(data.frame(seq(as.Date("1981/3/31"), as.Date("2024/07/01"), "quarters")), DATA_Indonesia) # Create dataframe with dates and values of remittances

names(Remittances_Indonesia)[names(Remittances_Indonesia) == 'seq.as.Date..1981.3.31....as.Date..2024.07.01.....quarters..'] <- 'Date' # Change column names
names(Remittances_Indonesia)[names(Remittances_Indonesia) == 'as.numeric(DATA_Indonesia[-1, ])'] <- 'Remittances'

Remittances_Indonesia$Growth_rate <- growth_rate_q(Remittances_Indonesia$Remittances) # Add column with growth rates

Indonesia_plot<-
  ggplot(Remittances_Indonesia, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-100, 800, by=100), sec.axis = sec_axis(~.,name="", breaks = seq(-100, 800, by=100)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Indonesia", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-100, 800))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Indonesia_plot

# EGYPT

DATA_Egypt <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",26))) # Load data for remittances
DATA_Egypt <- as.data.frame(as.numeric(DATA_Egypt[-1,])) # Remove first row and convert data to numeric format

Remittances_Egypt<- cbind(data.frame(seq(as.Date("2011/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Egypt) # Create dataframe with dates and values of remittances

names(Remittances_Egypt)[names(Remittances_Egypt) == 'seq.as.Date..2011.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Egypt)[names(Remittances_Egypt) == 'as.numeric(DATA_Egypt[-1, ])'] <- 'Remittances'

Remittances_Egypt$Growth_rate <- growth_rate_q(Remittances_Egypt$Remittances) # Add column with growth rates

Egypt_plot<-
  ggplot(Remittances_Egypt, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2012-03-31','2024-03-31')),
               breaks = seq(as.Date("2012-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-50, 100, by=25), sec.axis = sec_axis(~.,name="", breaks = seq(-50, 100, by=25)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Egypt", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-50, 100))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Egypt_plot

# MOROCCO

DATA_Morocco <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",27))) # Load data for remittances
DATA_Morocco <- as.data.frame(as.numeric(DATA_Morocco[-1,])) # Remove first row and convert data to numeric format

Remittances_Morocco<- cbind(data.frame(seq(as.Date("2003/3/31"), as.Date("2015/12/31"), "quarters")), DATA_Morocco) # Create dataframe with dates and values of remittances

names(Remittances_Morocco)[names(Remittances_Morocco) == 'seq.as.Date..2003.3.31....as.Date..2015.12.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Morocco)[names(Remittances_Morocco) == 'as.numeric(DATA_Morocco[-1, ])'] <- 'Remittances'

Remittances_Morocco$Growth_rate <- growth_rate_q(Remittances_Morocco$Remittances) # Add column with growth rates

Morocco_plot<-
  ggplot(Remittances_Morocco, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2004-03-31','2016-03-31')),
               breaks = seq(as.Date("2004-03-31"), as.Date("2016-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-30, 50, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-30, 50, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Morocco", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-30, 50))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Morocco_plot


# NIGERIA

DATA_Nigeria <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",28))) # Load data for remittances
DATA_Nigeria <- as.data.frame(as.numeric(DATA_Nigeria[-1,])) # Remove first row and convert data to numeric format

Remittances_Nigeria<- cbind(data.frame(seq(as.Date("2014/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Nigeria) # Create dataframe with dates and values of remittances

names(Remittances_Nigeria)[names(Remittances_Nigeria) == 'seq.as.Date..2014.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Nigeria)[names(Remittances_Nigeria) == 'as.numeric(DATA_Nigeria[-1, ])'] <- 'Remittances'

Remittances_Nigeria$Growth_rate <- growth_rate_q(Remittances_Nigeria$Remittances) # Add column with growth rates

Nigeria_plot<-
  ggplot(Remittances_Nigeria, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`, colour = "Remittances"),size = 0.8) +
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2015-03-31','2024-03-31')),
               breaks = seq(as.Date("2015-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-50, 50, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-50, 50, by=10)))+
  scale_color_manual(name="", values = c("Remittances"="#dd0400"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Nigeria", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-50, 50))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key = element_blank())

Nigeria_plot

# UKRAINE

DATA_Ukraine <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",29))) # Load data for remittances
DATA_Ukraine <- as.data.frame(as.numeric(DATA_Ukraine[-1,])) # Remove first row and convert data to numeric format

Remittances_Ukraine<- cbind(data.frame(seq(as.Date("2000/3/31"), as.Date("2024/3/31"), "quarters")), DATA_Ukraine) # Create dataframe with dates and values of remittances

names(Remittances_Ukraine)[names(Remittances_Ukraine) == 'seq.as.Date..2000.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Remittances_Ukraine)[names(Remittances_Ukraine) == 'as.numeric(DATA_Ukraine[-1, ])'] <- 'Remittances'

Remittances_Ukraine$Growth_rate <- growth_rate_q(Remittances_Ukraine$Remittances) # Add column with growth rates

Ukraine_plot<-
  ggplot(Remittances_Ukraine, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Growth_rate`),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Growth rate (%)") +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('2001-03-31','2024-03-31')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Ukraine", x ="")

Ukraine_plot

#---------------------- SVAR MODELS ----------------------#

# Obtain the relevant time series
fredr_set_key("ff21fd944b1f16b5c0f7c11c86321e3d")                                  
DATA_US_outputgap <- read_xls("fredgraph.xls", skip=10) 

# Matrix for Cholesky decomposition
amat <- diag(2)
amat[2,1] <- NA

# El Salvador

VAR_US_Outputgap_El_Salvador <- ts(DATA_US_outputgap[-c(1:92),]$GDPC1_GDPPOT, start=c(2000,1), end=c(2024,1), frequency = 4) 
VAR_Remit_El_Salvador <- ts(na.omit(Remittances_El_Salvador$Growth_rate), start=c(2000,1), end=c(2024,1), frequency = 4)

plot(VAR_US_Outputgap_El_Salvador)
tseries::adf.test(VAR_US_Outputgap_El_Salvador)
tseries::adf.test(diff(VAR_US_Outputgap_El_Salvador)) # Series is stationary after taking first differences

plot(VAR_Remit_El_Salvador)
tseries::adf.test(VAR_Remit_El_Salvador)
tseries::adf.test(diff(VAR_Remit_El_Salvador)) # Series is stationary after taking first differences

VAR_El_Salvador <- window(ts.union(diff(VAR_US_Outputgap_El_Salvador), diff(VAR_Remit_El_Salvador)), start=c(2000,2), end=c(2024,1))
colnames(VAR_El_Salvador ) <- c("US output gap", "Remittances")
plot.ts(VAR_El_Salvador) # Merge time series objects and plot them in a single graph

info.var.el.salvador<- VARselect(VAR_El_Salvador, lag.max = 12, type = "none")
info.var.el.salvador$selection # Using the information criteria, VAR(4) is the most appropriate

VAR_El_Salvador_est <- VAR(VAR_El_Salvador, p=4, type = "none")
summary(VAR_El_Salvador_est) # Estimation of reduced-form VAR

SVAR_El_Salvador_est <- SVAR(VAR_El_Salvador_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_elsal <- irf(SVAR_El_Salvador_est, impulse = "US.output.gap", response = "Remittances", n.ahead=15)
plot(irf_gap_remit_elsal) # US output gap shock on remittances

# GUATEMALA

VAR_US_Outputgap_Guatemala <- ts(DATA_US_outputgap[-c(1:48),]$GDPC1_GDPPOT, start=c(1989,1), end=c(2024,1), frequency = 4) 
VAR_Remit_Guatemala <- ts(na.omit(Remittances_Guatemala$Growth_rate), start=c(1989,1), end=c(2024,1), frequency = 4)

plot(VAR_US_Outputgap_Guatemala)
tseries::adf.test(VAR_US_Outputgap_Guatemala)
tseries::adf.test(diff(VAR_US_Outputgap_Guatemala)) # Series is stationary after taking first differences

plot(VAR_Remit_Guatemala)
tseries::adf.test(VAR_Remit_Guatemala) # Series is stationary without any transformation

VAR_Guatemala <- window(ts.union(diff(VAR_US_Outputgap_Guatemala), VAR_Remit_Guatemala), start=c(1989,2), end=c(2024,1))
colnames(VAR_Guatemala) <- c("US output gap", "Remittances")
plot.ts(VAR_Guatemala) # Merge time series objects and plot them in a single graph

info.var.guatemala<- VARselect(VAR_Guatemala, lag.max = 12, type = "none")
info.var.guatemala$selection # Using the information criteria, VAR(1) is the most appropriate

VAR_Guatemala_est <- VAR(VAR_Guatemala, p=1, type = "none")
summary(VAR_Guatemala_est) # Estimation of reduced-form VAR

SVAR_Guatemala_est <- SVAR(VAR_Guatemala_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_gua <- irf(SVAR_Guatemala_est, impulse = "US.output.gap", response = "Remittances", n.ahead=10)
plot(irf_gap_remit_gua) # US output gap shock on remittances

# MEXICO

VAR_US_Outputgap_Mexico<- ts(DATA_US_outputgap[-c(1:16),]$GDPC1_GDPPOT, start=c(1981,1), end=c(2024,1), frequency = 4) 
VAR_Remit_Mexico <- ts(na.omit(Remittances_Mexico$Growth_rate), start=c(1981,1), end=c(2024,1), frequency = 4)

plot(VAR_US_Outputgap_Mexico)
tseries::adf.test(VAR_US_Outputgap_Mexico) # Series is stationary without any transformation

plot(VAR_Remit_Mexico)
tseries::adf.test(VAR_Remit_Mexico) # Series is stationary without any transformation

VAR_Mexico <- window(ts.union(VAR_US_Outputgap_Mexico, VAR_Remit_Mexico), start=c(1981,1), end=c(2024,1))
colnames(VAR_Mexico) <- c("US output gap", "Remittances")
plot.ts(VAR_Mexico) # Merge time series objects and plot them in a single graph

info.var.mexico<- VARselect(VAR_Mexico, lag.max = 12, type = "none")
info.var.mexico$selection # Using the information criteria, VAR(1) is the most appropriate

VAR_Mexico_est <- VAR(VAR_Mexico, p=1, type = "none")
summary(VAR_Mexico_est) # Estimation of reduced-form VAR

SVAR_Mexico_est <- SVAR(VAR_Mexico_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_mex <- irf(SVAR_Mexico_est, impulse = "US.output.gap", response = "Remittances", n.ahead=20)
plot(irf_gap_remit_mex) # US output gap shock on remittances

# ECUADOR 

VAR_US_Outputgap_Ecuador<- ts(DATA_US_outputgap[-c(1:68),]$GDPC1_GDPPOT, start=c(1994,1), end=c(2024,1), frequency = 4) 
VAR_Remit_Ecuador <- ts(na.omit(Remittances_Ecuador$Growth_rate), start=c(1994,1), end=c(2024,1), frequency = 4)

plot(VAR_US_Outputgap_Ecuador)
tseries::adf.test(VAR_US_Outputgap_Ecuador)
tseries::adf.test(diff(VAR_US_Outputgap_Ecuador)) # Series is stationary after taking first differences

plot(VAR_Remit_Ecuador)
tseries::adf.test(VAR_Remit_Ecuador)
tseries::adf.test(diff(VAR_Remit_Ecuador)) # Series is stationary after taking first differences

VAR_Ecuador <- window(ts.union(diff(VAR_US_Outputgap_Ecuador), diff(VAR_Remit_Ecuador)), start=c(1994,2), end=c(2024,1))
colnames(VAR_Ecuador) <- c("US output gap", "Remittances")
plot.ts(VAR_Ecuador) # Merge time series objects and plot them in a single graph

info.var.ecuador<- VARselect(VAR_Ecuador, lag.max = 12, type = "none")
info.var.ecuador$selection # Using the information criteria, VAR(4) is the most appropriate

VAR_Ecuador_est <- VAR(VAR_Ecuador, p=4, type = "none")
summary(VAR_Ecuador_est) # Estimation of reduced-form VAR

SVAR_Ecuador_est <- SVAR(VAR_Ecuador_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_ecua <- irf(SVAR_Ecuador_est, impulse = "US.output.gap", response = "Remittances", n.ahead=15)
plot(irf_gap_remit_ecua) # US output gap shock on remittances

# BANGLADESH

VAR_US_Outputgap_Bangladesh<- ts(DATA_US_outputgap[-c(1:48),]$GDPC1_GDPPOT, start=c(1989,1), end=c(2024,1), frequency = 4) 
VAR_Remit_Bangladesh <- ts(na.omit(Remittances_Bangladesh$Growth_rate), start=c(1989,1), end=c(2024,1), frequency = 4)

plot(VAR_US_Outputgap_Bangladesh)
tseries::adf.test(VAR_US_Outputgap_Bangladesh)
tseries::adf.test(diff(VAR_US_Outputgap_Bangladesh)) # Series is stationary after taking first differences

plot(VAR_Remit_Bangladesh)
tseries::adf.test(VAR_Remit_Bangladesh) # Series is stationary without any transformation

VAR_Bangladesh <- window(ts.union(diff(VAR_US_Outputgap_Bangladesh), VAR_Remit_Bangladesh), start=c(1989,2), end=c(2024,1))
colnames(VAR_Bangladesh) <- c("US output gap", "Remittances")
plot.ts(VAR_Bangladesh) # Merge time series objects and plot them in a single graph

info.var.bangladesh<- VARselect(VAR_Bangladesh, lag.max = 12, type = "none")
info.var.bangladesh$selection # Using the information criteria, VAR(6) is the most appropriate

VAR_Bangladesh_est <- VAR(VAR_Bangladesh, p=6, type = "none")
summary(VAR_Bangladesh_est) # Estimation of reduced-form VAR

SVAR_Bangladesh_est <- SVAR(VAR_Bangladesh_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_bang <- irf(SVAR_Bangladesh_est, impulse = "US.output.gap", response = "Remittances", n.ahead=15)
plot(irf_gap_remit_bang) # US output gap shock on remittances

# INDIA

VAR_US_Outputgap_India<- ts(DATA_US_outputgap[-c(1:88),]$GDPC1_GDPPOT, start=c(1999,1), end=c(2024,1), frequency = 4) 
VAR_Remit_India <- ts(na.omit(Remittances_India$Growth_rate), start=c(1999,1), end=c(2024,1), frequency = 4)

plot(VAR_US_Outputgap_India)
tseries::adf.test(VAR_US_Outputgap_India)
tseries::adf.test(diff(VAR_US_Outputgap_India)) # Series is stationary after taking first differences

plot(VAR_Remit_India)
tseries::adf.test(VAR_Remit_India) # Series is stationary without any transformation

VAR_India <- window(ts.union(diff(VAR_US_Outputgap_India), VAR_Remit_India), start=c(1999,2), end=c(2024,1))
colnames(VAR_India) <- c("US output gap", "Remittances")
plot.ts(VAR_India) # Merge time series objects and plot them in a single graph

info.var.india<- VARselect(VAR_India, lag.max = 12, type = "none")
info.var.india$selection # Using the information criteria, VAR(1) is the most appropriate

VAR_India_est <- VAR(VAR_India, p=1, type = "none")
summary(VAR_India_est) # Estimation of reduced-form VAR

SVAR_India_est <- SVAR(VAR_India_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_ind <- irf(SVAR_India_est, impulse = "US.output.gap", response = "Remittances", n.ahead=10)
plot(irf_gap_remit_ind) # US output gap shock on remittances

# VIETNAM

VAR_US_Outputgap_Vietnam<- ts(DATA_US_outputgap[-c(1:80),]$GDPC1_GDPPOT, start=c(1997,1), end=c(2024,1), frequency = 4) 
VAR_Remit_Vietnam <- ts(na.omit(Remittances_Vietnam$Growth_rate), start=c(1997,1), end=c(2024,1), frequency = 4)

plot(VAR_US_Outputgap_Vietnam)
tseries::adf.test(VAR_US_Outputgap_Vietnam)
tseries::adf.test(diff(VAR_US_Outputgap_Vietnam)) # Series is stationary after taking first differences

plot(VAR_Remit_Vietnam)
tseries::adf.test(VAR_Remit_Vietnam) # Series is stationary without any transformation

VAR_Vietnam <- window(ts.union(diff(VAR_US_Outputgap_Vietnam), VAR_Remit_Vietnam), start=c(1997,2), end=c(2024,1))
colnames(VAR_Vietnam) <- c("US output gap", "Remittances")
plot.ts(VAR_Vietnam) # Merge time series objects and plot them in a single graph

info.var.vietnam<- VARselect(VAR_Vietnam, lag.max = 12, type = "none")
info.var.vietnam$selection # Using the information criteria, VAR(1) is the most appropriate

VAR_Vietnam_est <- VAR(VAR_Vietnam, p=1, type = "none")
summary(VAR_Vietnam_est) # Estimation of reduced-form VAR

SVAR_Vietnam_est <- SVAR(VAR_Vietnam_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_viet <- irf(SVAR_Vietnam_est, impulse = "US.output.gap", response = "Remittances", n.ahead=10)
plot(irf_gap_remit_viet) # US output gap shock on remittances

# LESOTHO

VAR_US_Outputgap_Lesotho<- ts(DATA_US_outputgap[-c(1:24),]$GDPC1_GDPPOT, start=c(1983,1), end=c(2024,2), frequency = 4) 
VAR_Remit_Lesotho <- ts(na.omit(Remittances_Lesotho$Growth_rate), start=c(1983,1), end=c(2024,2), frequency = 4)

plot(VAR_US_Outputgap_Lesotho)
tseries::adf.test(VAR_US_Outputgap_Lesotho)
tseries::adf.test(diff(VAR_US_Outputgap_Lesotho)) # Series is stationary after taking first differences

plot(VAR_Remit_Lesotho)
tseries::adf.test(VAR_Remit_Lesotho) # Series is stationary without any transformation

VAR_Lesotho <- window(ts.union(diff(VAR_US_Outputgap_Lesotho), VAR_Remit_Lesotho), start=c(1983,2), end=c(2024,2))
colnames(VAR_Lesotho) <- c("US output gap", "Remittances")
plot.ts(VAR_Lesotho) # Merge time series objects and plot them in a single graph

info.var.lesotho<- VARselect(VAR_Lesotho, lag.max = 12, type = "none")
info.var.lesotho$selection # Using the information criteria, VAR(1) is the most appropriate

VAR_Lesotho_est <- VAR(VAR_Lesotho, p=1, type = "none")
summary(VAR_Lesotho_est) # Estimation of reduced-form VAR

SVAR_Lesotho_est <- SVAR(VAR_Lesotho_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_lesot <- irf(SVAR_Lesotho_est, impulse = "US.output.gap", response = "Remittances", n.ahead=10)
plot(irf_gap_remit_lesot) # US output gap shock on remittances

# JORDAN

VAR_US_Outputgap_Jordan<- ts(DATA_US_outputgap[-c(1:4),]$GDPC1_GDPPOT, start=c(1978,1), end=c(2022,4), frequency = 4) 
VAR_Remit_Jordan <- ts(na.omit(Remittances_Jordan$Growth_rate), start=c(1978,1), end=c(2022,4), frequency = 4)

plot(VAR_US_Outputgap_Jordan)
tseries::adf.test(VAR_US_Outputgap_Jordan) # Series is stationary without any transformation

plot(VAR_Remit_Jordan)
tseries::adf.test(VAR_Remit_Jordan) # Series is stationary without any transformation

VAR_Jordan <- window(ts.union(VAR_US_Outputgap_Jordan, VAR_Remit_Jordan), start=c(1978,1), end=c(2022,4))
colnames(VAR_Jordan) <- c("US output gap", "Remittances")
plot.ts(VAR_Jordan) # Merge time series objects and plot them in a single graph

info.var.jordan<- VARselect(VAR_Jordan, lag.max = 12, type = "none")
info.var.jordan$selection # Using the information criteria, VAR(1) is the most appropriate

VAR_Jordan_est <- VAR(VAR_Jordan, p=1, type = "none")
summary(VAR_Jordan_est) # Estimation of reduced-form VAR

SVAR_Jordan_est <- SVAR(VAR_Jordan_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_jord <- irf(SVAR_Jordan_est, impulse = "US.output.gap", response = "Remittances", n.ahead=15)
plot(irf_gap_remit_jord) # US output gap shock on remittances

# COLOMBIA

VAR_US_Outputgap_Colombia<- ts(DATA_US_outputgap[-c(1:80),]$GDPC1_GDPPOT, start=c(1997,1), end=c(2024,2), frequency = 4) 
VAR_Remit_Colombia <- ts(na.omit(Remittances_Colombia$Growth_rate), start=c(1997,1), end=c(2024,2), frequency = 4)

plot(VAR_US_Outputgap_Colombia)
tseries::adf.test(VAR_US_Outputgap_Colombia) 
tseries::adf.test(diff(VAR_US_Outputgap_Colombia)) # Series is stationary after taking first differences

plot(VAR_Remit_Colombia)
tseries::adf.test(VAR_Remit_Colombia) # Series is stationary without any transformation

VAR_Colombia <- window(ts.union(diff(VAR_US_Outputgap_Colombia), VAR_Remit_Colombia), start=c(1997,2), end=c(2024,2))
colnames(VAR_Colombia) <- c("US output gap", "Remittances")
plot.ts(VAR_Colombia) # Merge time series objects and plot them in a single graph

info.var.colombia<- VARselect(VAR_Colombia, lag.max = 12, type = "none")
info.var.colombia$selection # Using the information criteria, VAR(5) is the most appropriate

VAR_Colombia_est <- VAR(VAR_Colombia, p=5, type = "none")
summary(VAR_Colombia_est) # Estimation of reduced-form VAR

SVAR_Colombia_est <- SVAR(VAR_Colombia_est, Amat = amat, max.iter = 1000) # Estimate SVAR with restrictions

irf_gap_remit_col <- irf(SVAR_Colombia_est, impulse = "US.output.gap", response = "Remittances", n.ahead=15)
plot(irf_gap_remit_col) # US output gap shock on remittances

#---------------------- DFM ESTIMATION ----------------------#

  # Use relevant countries and merge them into a single time series object (Growth rates)
DFM_El_Salvador <- ts(data = na.omit(Remittances_El_Salvador$Growth_rate), start = c(2000,1), frequency = 4)

DFM_Guatemala <- ts(data = na.omit(Remittances_Guatemala$Growth_rate), start = c(1989,1), frequency = 4)
DFM_Guatemala <- window(DFM_Guatemala, start=c(2003,2), frequency=4) # Use only data starting from 2003Q2

DFM_Mexico <- ts(data = na.omit(Remittances_Mexico$Growth_rate), start = c(1981,1), frequency = 4)
DFM_Ecuador <- ts(data = na.omit(Remittances_Ecuador$Growth_rate), start = c(1994,1), frequency = 4)
DFM_Bangladesh <- ts(data = na.omit(Remittances_Bangladesh$Growth_rate), start = c(1989,1), frequency = 4)
DFM_India <- ts(data = na.omit(Remittances_India$Growth_rate), start = c(1999,1), frequency = 4)

DFM_Vietnam <- ts(data = na.omit(Remittances_Vietnam$Growth_rate), start = c(1997,1), frequency = 4)
DFM_Vietnam <- window(DFM_Vietnam, end=c(2023,1), frequency=4) # Use only data until 2023Q1

DFM_Lesotho <- ts(data = na.omit(Remittances_Lesotho$Growth_rate), start = c(1983,1), frequency = 4)
DFM_Jordan <- ts(data = na.omit(Remittances_Jordan$Growth_rate), start = c(1978,1), frequency = 4)

DFM_Colombia <- ts(data = na.omit(Remittances_Colombia$Growth_rate), start = c(1997,1), frequency = 4)
DFM_Colombia <- window(DFM_Colombia, start=c(2000,1), frequency=4) # Use only data starting from 2000Q1

DFM_Egypt <- ts(data = na.omit(Remittances_Egypt$Growth_rate), start = c(2012,1), frequency = 4)
DFM_Egypt <- window(DFM_Egypt, start=c(2013,1), frequency=4) # Use only data starting from 2013Q1

DFM_Indonesia <- ts(data = na.omit(Remittances_Indonesia$Growth_rate), start = c(1982,1), frequency = 4)
DFM_Indonesia <- window(DFM_Indonesia, start=c(2004,2), frequency=4) # Use only data starting from 2004Q2

DFM_Sri_Lanka <- ts(data = na.omit(Remittances_Sri_Lanka$Growth_rate), start = c(1978,1), frequency = 4)
DFM_Morocco <- ts(data = na.omit(Remittances_Morocco$Growth_rate), start = c(2004,1), frequency = 4)
DFM_Nigeria <- ts(data = na.omit(Remittances_Nigeria$Growth_rate), start = c(2015,1), frequency = 4)
DFM_Philippines <- ts(data = na.omit(Remittances_Philippines$Growth_rate), start = c(2000,1), frequency = 4)
DFM_Honduras <- ts(data = na.omit(Remittances_Honduras$Growth_rate), start = c(2005,1), frequency = 4)
DFM_Thailand <- ts(data = na.omit(Remittances_Thailand$Growth_rate), start = c(2006,1), frequency = 4)

DFM_Lebanon <- ts(data = na.omit(Remittances_Lebanon$Growth_rate), start = c(2003,1), frequency = 4)
DFM_Lebanon <- window(DFM_Lebanon, start=c(2011,1), frequency=4) # Use only data starting from 2011Q1

DFM_Uganda <- ts(data = na.omit(Remittances_Uganda$Growth_rate), start = c(2002,1), frequency = 4)
DFM_Uganda <- window(DFM_Uganda, start=c(2011,1), frequency=4) # Use only data starting from 2011Q1

  # Use relevant countries and merge them into a single time series object (Levels)
Levels_El_Salvador <- ts(data = na.omit(Remittances_El_Salvador$Remittances), start = c(1999,1), frequency = 4)
Levels_Guatemala <- ts(data = na.omit(Remittances_Guatemala$Remittances), start = c(1988,1), frequency = 4)
Levels_Mexico <- ts(data = na.omit(Remittances_Mexico$Remittances), start = c(1980,1), frequency = 4)
Levels_Ecuador <- ts(data = na.omit(Remittances_Ecuador$Remittances), start = c(1993,1), frequency = 4)
Levels_Bangladesh <- ts(data = na.omit(Remittances_Bangladesh$Remittances), start = c(1988,1), frequency = 4)
Levels_India <- ts(data = na.omit(Remittances_India$Remittances), start = c(1998,1), frequency = 4)
Levels_Vietnam <- ts(data = na.omit(Remittances_Vietnam$Remittances), start = c(1996,1), frequency = 4)
Levels_Lesotho <- ts(data = na.omit(Remittances_Lesotho$Remittances), start = c(1982,1), frequency = 4)
Levels_Jordan <- ts(data = na.omit(Remittances_Jordan$Remittances), start = c(1977,1), frequency = 4)
Levels_Colombia <- ts(data = na.omit(Remittances_Colombia$Remittances), start = c(1996,1), frequency = 4)
Levels_Egypt <- ts(data = na.omit(Remittances_Egypt$Remittances), start = c(2011,1), frequency = 4)
Levels_Indonesia <- ts(data = na.omit(Remittances_Indonesia$Remittances), start = c(1981,1), frequency = 4)
Levels_Sri_Lanka <- ts(data = na.omit(Remittances_Sri_Lanka$Remittances), start = c(1977,1), frequency = 4)
Levels_Morocco <- ts(data = na.omit(Remittances_Morocco$Remittances), start = c(2003,1), frequency = 4)
Levels_Nigeria <- ts(data = na.omit(Remittances_Nigeria$Remittances), start = c(2014,1), frequency = 4)
Levels_Philippines <- ts(data = na.omit(Remittances_Philippines$Remittances), start = c(1999,1), frequency = 4)
Levels_Honduras <- ts(data = na.omit(Remittances_Honduras$Remittances), start = c(2004,1), frequency = 4)
Levels_Thailand <- ts(data = na.omit(Remittances_Thailand$Remittances), start = c(2005,1), frequency = 4)

Levels_Lebanon <- ts(data = na.omit(Remittances_Lebanon$Remittances), start = c(2002,1), frequency = 4)
Levels_Lebanon <- window(Levels_Lebanon, start=c(2007,1), frequency=4) # Use only data starting from 2007Q1

Levels_Uganda <- ts(data = na.omit(Remittances_Uganda$Remittances), start = c(2001,1), frequency = 4)
Levels_Uganda <- window(Levels_Uganda, start=c(2007,1), frequency=4) # Use only data starting from 2007Q1

 # Relevant data sets

Data_DFM <- cbind(DFM_El_Salvador, DFM_Guatemala, DFM_Mexico, DFM_Ecuador, DFM_Bangladesh,  # 18 countries
                      DFM_India, DFM_Vietnam, DFM_Lesotho, DFM_Jordan, DFM_Colombia,
                  DFM_Egypt, DFM_Indonesia, DFM_Sri_Lanka, DFM_Morocco, DFM_Nigeria,
                  DFM_Philippines, DFM_Honduras, DFM_Thailand)

Data_DFM_sample <- window(Data_DFM, start=c(1990,1), end=c(2024,1), frequency=4)

Data_DFM_2 <- cbind(DFM_El_Salvador, DFM_Guatemala, DFM_Mexico, DFM_Ecuador, DFM_Bangladesh,  # 20 countries
                  DFM_India, DFM_Vietnam, DFM_Lesotho, DFM_Jordan, DFM_Colombia,
                  DFM_Egypt, DFM_Indonesia, DFM_Sri_Lanka, DFM_Morocco, DFM_Nigeria,
                  DFM_Philippines, DFM_Honduras, DFM_Thailand, DFM_Lebanon, DFM_Uganda)

Data_DFM_sample_2 <- window(Data_DFM_2, start=c(1990,1), end=c(2024,1), frequency=4) # Sample: 1990Q1-2024Q1
Data_DFM_sample_3 <- window(Data_DFM_2, start=c(1998,1), end=c(2024,1), frequency=4) # Sample: 1998Q1-2024Q1
scaled_Data_DFM_sample_3 <- scale(Data_DFM_sample_3) # Sample: 1998Q1-2024Q1. Scaled data


Data_DFM_Levels <- cbind(Levels_El_Salvador, Levels_Guatemala, Levels_Mexico, Levels_Ecuador, Levels_Bangladesh,  # Levels 20 countries
                    Levels_India, Levels_Vietnam, Levels_Lesotho, Levels_Jordan, Levels_Colombia,
                    Levels_Egypt, Levels_Indonesia, Levels_Sri_Lanka, Levels_Morocco, Levels_Nigeria,
                    Levels_Philippines, Levels_Honduras, Levels_Thailand, Levels_Lebanon, Levels_Uganda)

Data_DFM_Levels <- window(Data_DFM_Levels, start=c(1990,1), end=c(2024,1), frequency=4) # Sample: 1990Q1-2024Q1
Data_DFM_Levels_2 <- window(Data_DFM_Levels, start=c(1998,1), end=c(2024,1), frequency=4) # Sample: 1998Q1-2024Q1

  # dfms package (Not necessary)

ic <- ICr(Data_DFM_sample)
print(ic)

screeplot(ic)

info.var.dfm <- VARselect(ic$F_pca[,1:1])
info.var.dfm$selection

DFM_Remittances_mod1 <- DFM(Data_DFM_sample, r=1, p=5) # Estimate the model for CPI inflation with 1 factor and 5 lags 
print(DFM_Remittances_mod1)
summary(DFM_Remittances_mod1)

DFM_Remittances_mod2 <- DFM(Data_DFM_sample, r=1, p=9) # Estimate the model for CPI inflation with 1 factor and 9 lags 
print(DFM_Remittances_mod2)
summary(DFM_Remittances_mod2)

F_DFM_Remittances_mod1 <- ts(data = DFM_Remittances_mod1$F_qml, start = c(1990,1), frequency = 4) # Extract the factor and convert it to time series object (1 factor and 5 lags)
F_DFM_Remittances_mod2 <- ts(data = DFM_Remittances_mod2$F_qml, start = c(1990,1), frequency = 4) # Extract the factor and convert it to time series object (1 factor and 9 lags)

Global_factor_remittances_mod1 <- as.data.frame(F_DFM_Remittances_mod1) 
names(Global_factor_remittances_mod1)[names(Global_factor_remittances_mod1) == 'f1'] <- 'Global factor model 1'

Global_factor_remittances_mod2 <- as.data.frame(F_DFM_Remittances_mod2)
names(Global_factor_remittances_mod2)[names(Global_factor_remittances_mod2) == 'f1'] <- 'Global factor model 2'

  # MARSS package (Relevant)

# Specify the relevant matrices

model.spec <- list() # Model with 1 lag and 20 countries
model.spec$B <- matrix(
  list("b11"), 1, 1)
model.spec$Z <- matrix(list(0), 20, 1)
model.spec$Z[,1] <-paste0("z", 1:20)
model.spec$Q <- "diagonal and equal"
model.spec$R <- "diagonal and unequal"
model.spec$A <- "zero"
model.spec$U <- "zero"
control_lag1 = list(maxit = 20000, abstol=1e-4)

model.spec.2 <- list() # Model with 2 lags and 20 countries
model.spec.2$B <- matrix(
  list("b1", 1, "b2", 0), 2, 2)
model.spec.2$Z <- matrix(list(0),20,2)
model.spec.2$Z[,1] <-paste0("z", 1:20)
model.spec.2$Q <- "diagonal and equal"
model.spec.2$R <- "diagonal and unequal"
model.spec.2$A <- "zero"
model.spec.2$U <- "zero"
control_lag2 = list(maxit = 20000, abstol=1e-4, safe=TRUE, conv.test.slope.tol = 0.1)

model.spec.scaled <- list() # Model with 1 lag and 20 countries. Scaled
model.spec.scaled$B <- matrix(
  list("b11"), 1, 1)
model.spec.scaled$Z <- matrix(list(0), 20, 1)
model.spec.scaled$Z[,1] <-paste0("z", 1:20)
model.spec.scaled$Q <- "diagonal and equal"
model.spec.scaled$R <- "diagonal and unequal"
model.spec.scaled$A <- "zero"
model.spec.scaled$U <- "zero"
control_lag1_scaled = list(maxit = 20000, abstol=1e-4)

model.spec.scaled.2 <- list() # Model with 2 lags and 20 countries. Scaled
model.spec.scaled.2$B <- matrix(
  list("b1", 1, "b2", 0), 2, 2)
model.spec.scaled.2$Z <- matrix(list(0),20,2)
model.spec.scaled.2$Z[,1] <-paste0("z", 1:20)
model.spec.scaled.2$Q <- "diagonal and equal"
model.spec.scaled.2$R <- "diagonal and unequal"
model.spec.scaled.2$A <- "zero"
model.spec.scaled.2$U <- "zero"
control_lag2_scaled = list(maxit = 20000, abstol=1e-4, safe=TRUE, conv.test.slope.tol = 0.1)

model.spec.scaled.2.1 <- list() # Model with 2 lags and 20 countries. Scaled. Common component with lag
model.spec.scaled.2.1$B <- matrix(
  list("b1", 1, "b2", 0), 2, 2)
model.spec.scaled.2.1$Z <- matrix(list(0),20,2)
model.spec.scaled.2.1$Z[,1] <-paste0("z", 1:20)
model.spec.scaled.2.1$Z[,2] <-paste0("z", 21:40)
model.spec.scaled.2.1$Q <- "diagonal and equal"
model.spec.scaled.2.1$R <- "diagonal and unequal"
model.spec.scaled.2.1$A <- "zero"
model.spec.scaled.2.1$U <- "zero"
control_lag2_1_scaled = list(maxit = 20000, abstol=1e-4, safe=TRUE, conv.test.slope.tol = 0.1)

model.spec.scaled.3 <- list() # Model with 3 lags and 20 countries. Scaled. 
model.spec.scaled.3$B <- matrix(
  list("b1", 1, 0,
       "b2", 0, 1,
       "b3", 0, 0), 3, 3)
model.spec.scaled.3$Z <- matrix(list(0),20,3)
model.spec.scaled.3$Z[,1] <-paste0("z", 1:20)
model.spec.scaled.3$Q <- "diagonal and equal"
model.spec.scaled.3$R <- "diagonal and unequal"
model.spec.scaled.3$A <- "zero"
model.spec.scaled.3$U <- "zero"
control_lag3_scaled = list(maxit = 20000, abstol=1e-4, safe=TRUE, conv.test.slope.tol = 0.1)

# Estimate DFM using the EM Algorithm

# 1 lag. 20 countries. 1998Q1-2024Q1
model.em <- MARSS(
  Data_DFM_sample_3, 
  model  = model.spec,
  inits  = list(x0 = 0), 
  silent = TRUE,
  control = control_lag1)

model.em
summary(model.em)

Z_mat <- matrix(c(model.em$par$Z),20,1) # Compute the matrix with the common components. Sample: 1998Q1-2024Q1
x_mat <- matrix(model.em$states,1,105)
chi_mat <- Z_mat %*% x_mat

# 2 lags. 20 countries. 1998Q1-2024Q1
model.em.2 <- MARSS(
  Data_DFM_sample_3, 
  model  = model.spec.2,
  inits  = list(x0 = 0),
  silent = FALSE,
  control = control_lag2
)

model.em.2
summary(model.em.2)

Z_mat_2 <- matrix(c(model.em.2$par$Z),20,1) # Compute the matrix with the common components. Sample: 1998Q1-2024Q1
x_mat_2 <- matrix(c(model.em.2$states[1,]),1,105)
chi_mat_2 <- Z_mat_2 %*% x_mat_2

# 1 lag. 20 countries. Scaled data. 1998Q1-2024Q1
model.em.scaled <- MARSS(
  scaled_Data_DFM_sample_3, 
  model  = model.spec.scaled,
  inits  = list(x0 = 0), 
  silent = TRUE,
  control = control_lag1_scaled)

model.em.scaled
summary(model.em.scaled)
 
Z_mat_scaled <- matrix(c(model.em.scaled$par$Z),20,1) # Compute the matrix with the common components. Sample: 1998Q1-2024Q1
x_mat_scaled <- matrix(model.em.scaled$states,1,105)
chi_mat_scaled <- Z_mat_scaled %*% x_mat_scaled

# 2 lags. 20 countries. Scaled data.1998Q1-2024Q1
model.em.scaled.2 <- MARSS(
  scaled_Data_DFM_sample_3, 
  model  = model.spec.scaled.2,
  inits  = list(x0 = 0),
  silent = FALSE,
  control = control_lag2_scaled
)

model.em.scaled.2
summary(model.em.scaled.2)

KF_mod<- MARSSkf(model.em.scaled.2)
KF_mod$xtT # Kalman smoother output
KF_mod$xtt # Kalman filter output

Z_mat_scaled_2 <- matrix(c(model.em.scaled.2$par$Z),20,1) # Compute the matrix with the common components. Sample: 1998Q1-2024Q1
x_mat_scaled_2 <- matrix(c(model.em.scaled.2$states[1,]),1,105)
chi_mat_scaled_2 <- Z_mat_scaled_2 %*% x_mat_scaled_2

x_mat_scaled_2_filter <- matrix(c(KF_mod$xtt[1,]),1,105) # Compute the matrix with the common components. Sample: 1998Q1-2024Q1. Kalman filter (not smoother) output
chi_mat_scaled_2_filter <- Z_mat_scaled_2 %*% x_mat_scaled_2_filter 

# 2 lags. 20 countries. Scaled data. Common component with lag. 1998Q1-2024Q1
model.em.scaled.2.1 <- MARSS(
  scaled_Data_DFM_sample_3, 
  model  = model.spec.scaled.2.1,
  inits  = list(x0 = 0),
  silent = FALSE,
  control = control_lag2_1_scaled
)

model.em.scaled.2.1
summary(model.em.scaled.2.1)

Z_mat_scaled_2_1 <- matrix(c(model.em.scaled.2.1$par$Z),20,2) # Compute the matrix with the common components. Sample: 1998Q1-2024Q1
x_mat_scaled_2_1 <- matrix(c(model.em.scaled.2$states[1,]),2,105)
chi_mat_scaled_2_1 <- Z_mat_scaled_2_1 %*% x_mat_scaled_2_1

# Data frame with country weights. 1998Q1-2024Q1
Weights <- as.data.frame(t(Data_DFM_Levels_2))
Weights <- Weights/colSums(Weights, na.rm = TRUE)[col(Weights)]

# Compute world remittances growth as a weighted average using the country weights
remit_growth <- as.data.frame(t(scaled_Data_DFM_sample_3)*Weights) 
Global_remit <- ts(data= colSums(remit_growth, na.rm = TRUE), start = c(1998,1), frequency = 4)

# Compute global components as a weighted average using the country weights
Common_comp_remit_lag1 <- as.data.frame(chi_mat_scaled * Weights)
Global_factor_remit_lag1 <- ts(data= colSums(Common_comp_remit_lag1, na.rm = TRUE), start = c(1998,1), frequency = 4)

Common_comp_remit_lag2 <- as.data.frame(chi_mat_scaled_2 * Weights)
Global_factor_remit_lag2 <- ts(data= colSums(Common_comp_remit_lag2, na.rm = TRUE), start = c(1998,1), frequency = 4)

Common_comp_remit_lag2_filter <- as.data.frame(chi_mat_scaled_2_filter * Weights)
Global_factor_remit_lag2_filter <- ts(data= colSums(Common_comp_remit_lag2_filter, na.rm = TRUE), start = c(1998,1), frequency = 4)

#---------------------- PLOTS OF FACTORS ----------------------#

Global_remittances_cycle <- cbind(data.frame(seq(as.Date("1998/03/31"), as.Date("2024/03/31"), "quarters")), Global_remit , Global_factor_remit_lag1, Global_factor_remit_lag2, Global_factor_remit_lag2_filter)
names(Global_remittances_cycle)[names(Global_remittances_cycle) == 'seq.as.Date..1998.03.31....as.Date..2024.03.31.....quarters..'] <- 'Date'
names(Global_remittances_cycle)[names(Global_remittances_cycle) == 'Global_remit'] <- 'Global remittances'
names(Global_remittances_cycle)[names(Global_remittances_cycle) == 'Global_factor_remit_lag1'] <- 'Global factor remittances 1 lag'
names(Global_remittances_cycle)[names(Global_remittances_cycle) == 'Global_factor_remit_lag2'] <- 'Global factor remittances 2 lags'
names(Global_remittances_cycle)[names(Global_remittances_cycle) == 'Global_factor_remit_lag2_filter'] <- 'Global factor remittances 2 lags no smoother'
rownames(Global_remittances_cycle) <- NULL

Global_remittances_cycle_plot<-
  ggplot(Global_remittances_cycle, aes(x=Date)) +
  add_rec_shade2(as.Date("1977-01-01"),as.Date("2024-01-01"))+
  geom_line(aes(y=`Global remittances`, colour="Global remittances"),size = 0.7)+
  geom_line(aes(y=`Global factor remittances 1 lag`, colour="Global remittances factor 1 lag"),size = 0.7)+
  geom_line(aes(y=`Global factor remittances 2 lags`, colour="Global remittances factor 2 lags"),size = 0.7)+
  geom_line(aes(y=`Global factor remittances 2 lags and 1 lag comm. component`, colour="Global remittances factor 2 lags and 1 lag comm. component"),size=0.7)+
  scale_y_continuous(name="") +
  scale_color_manual(name="Global measures", values = c("Global remittances"="blue", "Global remittances factor 1 lag"="red",
                                                        "Global remittances factor 2 lags"="darkcyan", "Global remittances factor 2 lags and 1 lag comm. component"="orange"))+
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c('1998-03-31','2024-07-01')))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"))+
  labs(title="Global remittances cycle", x ="")

Global_remittances_cycle_plot

# Global remittances cycle plot

Global_remittances_cycle_2_plot<-
  ggplot(Global_remittances_cycle, aes(x=Date)) +
  geom_line(aes(y=`Global remittances`, colour="Global remittances"),size = 0.7)+
  geom_line(aes(y=`Global factor remittances 2 lags`, colour="Global remittances factor"),size = 0.7)+
  geom_hline(yintercept=0)+
  scale_y_continuous(name="", labels = NULL, n.breaks = 10, sec.axis = sec_axis(~.,name="", breaks = seq(-2,2, by=0.5))) +
  scale_color_manual(name="", values = c("Global remittances"="blue3", "Global remittances factor"="#dd0400", "Global remittances factor no smoother"="orange"))+
  scale_x_date(labels = date_format("Q1-%Y"), limits = as.Date(c('1998-03-31','2024-03-31')),
               breaks = seq(as.Date("1998-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Global remittances factor", subtitle = "Standard deviations from 4-quarter percent change mean", x="")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = c(0.65, 0.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=10))
  
Global_remittances_cycle_2_plot

# Remittances growth selected countries plot

Remittances_growth_plot <- ggplot() +
  geom_line(Remittances_Guatemala, mapping=aes(y=`Growth_rate`, x=`Date`, colour = "Guatemala"),size = 0.8)+
  geom_line(Remittances_Mexico, mapping=aes(y=`Growth_rate`, x=`Date`, colour = "Mexico"),size = 0.8)+
  geom_line(Remittances_Colombia, mapping=aes(y=`Growth_rate`, x=`Date`, colour = "Colombia"),size = 0.8, linetype="dashed")+
  geom_line(Remittances_India, mapping=aes(y=`Growth_rate`, x=`Date`, colour = "India"),size = 0.8, linetype="dashed")+
  geom_line(Remittances_Philippines, mapping=aes(y=`Growth_rate`, x=`Date`, colour = "Philippines"),size = 0.8, linetype="dashed")+
  geom_line(Remittances_Indonesia, mapping=aes(y=`Growth_rate`, x=`Date`, colour = "Indonesia"),size = 0.8, linetype="dashed")+
  geom_line(Remittances_Nigeria, mapping=aes(y=`Growth_rate`, x=`Date`, colour = "Nigeria"),size = 0.8, linetype="dashed")+
  geom_hline(yintercept=0)+
  scale_x_date(labels = date_format("%YQ1"), limits = as.Date(c('2014-03-31','2024-03-31')),
               breaks = seq(as.Date("2014-03-31"), as.Date("2024-03-31"), by = "8 quarters"))+
  scale_y_continuous(name="", labels = NULL, breaks = seq(-40, 60, by=10), sec.axis = sec_axis(~.,name="", breaks = seq(-40, 60, by=10)))+
  scale_color_manual(name="", values = c("Guatemala"="red2", "Mexico"="purple2", "Colombia"="green3", "India"="blue2", "Philippines"="orange2", "Indonesia"="brown", "Nigeria"="pink"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="Remittances growth", subtitle = "4-quarter percent change", x="")+
  coord_cartesian(ylim=c(-40, 60))+
  guides(color=guide_legend(nrow=2))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = c(0.25,0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=10))

Remittances_growth_plot

# Remittances world 

DATA_World <- as.data.frame(t(read_xlsx("Remittances_Haver.xlsx",30))) # Load data for remittances
DATA_World <- as.data.frame(DATA_World[-1,]) # Remove first row and convert data to numeric format
rownames(DATA_World) <- NULL

DATA_World<- cbind(data.frame(seq(as.Date("1980-01-01"), as.Date("2023-01-01"), "year")), DATA_World) # Create dataframe with dates and values of remittances

names(DATA_World)[names(DATA_World) == 'seq.as.Date..1980.01.01....as.Date..2023.01.01.....year..'] <- 'Date' # Change column names
names(DATA_World)[names(DATA_World) == 'V1'] <- 'Remittances'
names(DATA_World)[names(DATA_World) == 'V2'] <- 'Remittances GDP'

DATA_World$Remittances <- as.numeric(DATA_World$Remittances)
DATA_World$`Remittances GDP`<-as.numeric(DATA_World$`Remittances GDP`)

scaling_factor <- max(DATA_World$Remittances) / max(DATA_World$`Remittances GDP`)

Remittances_world_plot <- ggplot(DATA_World, aes(x=Date))+
  geom_line(aes(y=Remittances, color="Remittances"), size=0.8)+
  geom_line(aes(y=`Remittances GDP`*scaling_factor, color="Remittances (% of GDP)"), size=0.8)+
  scale_y_continuous(name="Billions of dollars", sec.axis = sec_axis(~./scaling_factor,name="% of GDP"))+
  scale_x_date(labels = date_format("%Y"), limits = as.Date(c('1980-01-01','2024-01-01')),
               breaks = seq(as.Date("1980-01-01"), as.Date("2024-01-01"), by = "4 years"))+
  scale_color_manual(name="", values=c("Remittances"="red3", "Remittances (% of GDP)"="blue3"))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  labs(title="World remittances", x="")+
  guides(color=guide_legend(nrow=2))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = c(0.50,0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.title.y=element_text(size=8),
        legend.text = element_text(size=10))

Remittances_world_plot

#---------------------- GLOBAL DRIVERS OF REMITTANCES: SVAR ----------------------#

# Global GDP growth

World_GDP <- as.data.frame(read_xlsx("Inflation data OECD.xlsx",3))
World_GDP <- t(World_GDP[23,-1])
Data_World_GDP <- cbind(data.frame(seq(as.Date("1961/3/31"), as.Date("2024/3/31"), "quarters")), World_GDP)
rownames(Data_World_GDP) <- NULL
names(Data_World_GDP)[names(Data_World_GDP) == 'seq.as.Date..1961.3.31....as.Date..2024.3.31.....quarters..'] <- 'Date' # Change column names
names(Data_World_GDP)[names(Data_World_GDP) == '23'] <- 'Level'
Data_World_GDP$Growth_rate <- growth_rate_q(Data_World_GDP$Level)

Global_GDP_Growth <- ts(na.omit(Data_World_GDP$Growth_rate), start = c(1962,1), frequency = 4)
Global_GDP_Growth <- window(Global_GDP_Growth, start = c(2004,1), end= c(2024,1))

# Baumeister's Monthly World Industrial production index

WIP <- as.data.frame(read_xlsx("OECD_plus6_industrial_production.xlsx",1))
names(WIP)[names(WIP) == '...1'] <- 'Date' # Change column names
names(WIP)[names(WIP) == 'OECD+6NME industrial production'] <- 'WIP Index'
WIP <- WIP[-(1:540),]
rownames(WIP) <- NULL
WIP <- WIP[-(256:260),]

zoo_WIP <-zoo(WIP$`WIP Index`, WIP$Date)
quartely_zoo_WIP <- aggregate(zoo_WIP, as.yearqtr, mean)
WIP_quarterly <- data.frame(WIP_Index=coredata(quartely_zoo_WIP))
WIP_quarterly$Growth_rate <- growth_rate_q(WIP_quarterly$WIP_Index)

WIP_Index_Growth <- ts(na.omit(WIP_quarterly$Growth_rate), start = c(2004,1), frequency = 4)

plot(WIP_Index_Growth)

plot(Global_GDP_Growth)
tseries::adf.test(Global_GDP_Growth)  # Series is stationary. No transformation needed
tseries::adf.test(diff(Global_GDP_Growth))

# Shortages index

SI <- as.data.frame(read_xlsx("Shortages_Index.xlsx",1))
names(SI)[names(SI) == 'date'] <- 'Date' # Change column names
SI <- SI[,-(3:6)]
SI <- SI[-(1:1236),]
rownames(SI) <- NULL
SI <- SI[-(256:263),]
SI$Date <- seq(from=as.Date("2003-01-01"), to= as.Date("2024-03-01"), by="month")

zoo_SI <-zoo(SI$index_all_shortage, SI$Date)
quartely_zoo_SI <- aggregate(zoo_SI, as.yearqtr, mean)
SI_quarterly <- data.frame(SI_Index=coredata(quartely_zoo_SI))
SI_quarterly$Growth_rate <- growth_rate_q(SI_quarterly$SI_Index)

SI_Index <- ts(na.omit(SI_quarterly$SI_Index), start = c(2003,1), frequency = 4)
plot(SI_Index)

# Office of Financial Research (OFR) Financial Stress Index (FSI)

FSI <- as.data.frame(read_xlsx("OFR_FSI.xlsx",1))

zoo_FSI <- zoo(FSI$Value, FSI$Date)
quarterly_zoo_FSI <- aggregate(zoo_FSI, as.yearqtr, mean)
FSI_quarterly <- data.frame(FSI_Index=coredata(quarterly_zoo_FSI))
FSI_quarterly$Growth_rate <- growth_rate_q(FSI_quarterly$FSI_Index)

FSI_Index <- ts(na.omit(FSI_quarterly$FSI_Index), start = c(2001,1), end=c(2024,1), frequency = 4)

# US quarterly GDP growth 
w <- fredr(series_id = "USAGDPRQPSMEI", frequency = "q", units = "", observation_start = as.Date("2004-01-01"), observation_end = as.Date("2024-01-01") )
US_RGDP <- ts(na.omit(w$value), start=c(2004,1), frequency = 4)

# BAA spread
x <- fredr(series_id = "BAA10Y", frequency = "q", units="", observation_start = as.Date("2003-01-01"), observation_end = as.Date("2024-01-01"))
x$growth_rate <- growth_rate_q(x$value)
BAA_spread <- ts(na.omit(x$growth_rate), start = c(2004,1), frequency = 4)

# VIX
x2 <- fredr(series_id = "VIXCLS", frequency = "q", units="", observation_start = as.Date("2003-01-01"), observation_end = as.Date("2024-01-01"))
x2$growth_rate <- growth_rate_q(x2$value)
VIX <- ts(na.omit(x2$growth_rate), start = c(2004,1), frequency = 4)

plot(BAA_spread)
tseries::adf.test(BAA_spread) # Series is stationary. No transformation needed

# Global price index of all commodities
y <- fredr(series_id = "PALLFNFINDEXQ", frequency = "q", units="", observation_start = as.Date("2003-01-01"), observation_end = as.Date("2024-01-01"))
y$growth_rate <- growth_rate_q(y$value)
comm_prices <- ts(na.omit(y$growth_rate), start = c(2004,1), frequency = 4)

plot(comm_prices)
tseries::adf.test(comm_prices)
tseries::adf.test(diff(comm_prices)) # Series is nonstationary. Take first differences

plot(Global_remittances_cycle$`Global factor remittances 1 lag`)
tseries::adf.test(Global_remittances_cycle$`Global factor remittances 1 lag`)
tseries::adf.test(diff(Global_remittances_cycle$`Global factor remittances 1 lag`)) # Series is nonstationary. Take first differences

# VAR estimation
VAR_Drivers <- window(ts.union(WIP_Index_Growth, comm_prices, FSI_Index, Global_remittances_cycle$`Global factor remittances 2 lags`), start=c(2004,1), end=c(2024,1))
colnames(VAR_Drivers) <- c("WIP growth", "Commodity prices growth", "FSI","Global remittances factor")
plot(VAR_Drivers)

VAR_drivers_df <- as.data.frame(VAR_Drivers)
write_xlsx(VAR_drivers_df, "VAR drivers data.xlsx")

info.var.drivers<- VARselect(VAR_Drivers, lag.max = 12, type = "const")
info.var.drivers$selection # Using the information criteria, VAR(2) is the most appropriate

VAR_Drivers_est <- VAR(VAR_Drivers, p=2, type = "const")
summary(VAR_Drivers_est) # Estimation of reduced-form VAR

# SVAR estimation

SVAR_Drivers_est <- id.chol(VAR_Drivers_est, order_k = c(1,2,3,4))
summary(SVAR_Drivers_est)

# Historical decomposition

SVAR_Drivers_est_hd <- hd(SVAR_Drivers_est,4)
plot(SVAR_Drivers_est_hd)

Shocks_SVAR <- as.data.frame(SVAR_Drivers_est_hd$hidec)
Shocks_SVAR <- cbind(as.data.frame(seq(as.Date("2004/03/31"), as.Date("2024/03/31"), "quarters"))[-(1:2),], Shocks_SVAR) 
names(Shocks_SVAR)[names(Shocks_SVAR) == 'as.data.frame(seq(as.Date(\"2004/03/31\"), as.Date(\"2024/03/31\"), '] <- 'Date' # Change column names
names(Shocks_SVAR)[names(Shocks_SVAR) == 'Cumulative effect of  WIP.growth shock on  Global.remittances.factor'] <- 'WIP growth shocks'
names(Shocks_SVAR)[names(Shocks_SVAR) == 'Cumulative effect of  Commodity.prices.growth shock on  Global.remittances.factor'] <- 'Commodity prices growth shocks' 
names(Shocks_SVAR)[names(Shocks_SVAR) == 'Cumulative effect of  FSI shock on  Global.remittances.factor'] <- 'Global financial stress shocks' 
names(Shocks_SVAR)[names(Shocks_SVAR) == 'Cumulative effect of  Global.remittances.factor shock on  Global.remittances.factor'] <- 'Global remittances shocks'

Shocks_SVAR <- Shocks_SVAR %>%
  mutate(Global_activity_shocks = `WIP growth shocks`+`Commodity prices growth shocks`) %>%
  mutate(Global_shocks = Global_activity_shocks+`Global financial stress shocks`+`Global remittances shocks`)

Global_remittance_Shocks_SVAR <- as.data.frame(Global_remittances_cycle$`Global remittances`)
rownames(Global_remittance_Shocks_SVAR) <- NULL
Global_remittance_Shocks_SVAR<- Global_remittance_Shocks_SVAR[-(1:26),]

Shocks_SVAR <- cbind(Shocks_SVAR, Global_remittance_Shocks_SVAR)
names(Shocks_SVAR)[names(Shocks_SVAR) == 'Global_remittance_Shocks_SVAR'] <- 'Global remittances' # Change column names

Shocks_SVAR <- Shocks_SVAR %>%
  mutate(Idiosyncratic_shocks = `Global remittances`- Global_shocks)

Shocks_SVAR_long <- reshape2::melt(Shocks_SVAR, id.vars='Date', variable.name = "Variable", value.name="Value")
Global_remittances_cycle_long <- reshape2::melt(Global_remittances_cycle, id.vars='Date', variable.name = "Variable", value.name="Value")

Shocks_SVAR_long_filtered <- Shocks_SVAR_long %>%
  filter(Variable != "Demeaned series  Global.remittances.factor") %>%
  filter(Variable != "Constructed series  Global.remittances.factor") %>%
  filter(Variable != "WIP growth shocks") %>%
  filter(Variable != "Commodity prices growth shocks")%>%
  filter(Variable != "Global_shocks") %>%
  filter(Variable != "Global remittances") %>%
  mutate(Variable = factor(Variable, levels=c("Global_activity_shocks", "Global financial stress shocks", "Global remittances shocks", "Idiosyncratic_shocks")))
  
Global_remittances_cycle_filtered <- Global_remittances_cycle_long %>%
  filter(Variable == "Global remittances")

custom_colors <- c("Global_activity_shocks" = "coral1",
                   "Global financial stress shocks" = "chartreuse2",
                   "Global remittances shocks" = "darkturquoise",
                   "Idiosyncratic_shocks"= "darkorchid2")

custom_labels <- c("Global_activity_shocks" = "Global activity shocks",
                   "Global financial stress shocks" = "Global financial shocks",
                   "Global commodity prices shocks" = "Global commodity prices shocks",
                   "Global remittances shocks" = "Global remittances shocks",
                   "Idiosyncratic_shocks"= "Idiosyncratic shocks")

drivers_1 <- ggplot(Shocks_SVAR_long_filtered, aes(x=Date, y=Value, fill=Variable))+
  geom_bar(stat="identity")+
  geom_line(data=Global_remittances_cycle_filtered, aes(x=Date, y=Value, group=1),
            inherit.aes=FALSE, color="black", size=0.8)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name="", labels = NULL, n.breaks = 10, sec.axis = sec_axis(~.,name="", breaks = seq(-2,2, by=0.5))) +
  scale_x_date(labels = date_format("Q1-%Y"), limits = as.Date(c('2005/03/31','2013/12/31')),
               breaks = seq(as.Date("2005/03/31"), as.Date("2024/12/31"), by = "8 quarters"))+
  labs(title="2005-2013", subtitle = "Standard deviations from 4-quarter percent change mean", x="", y="")+
  scale_fill_manual(values = custom_colors, labels = custom_labels, guide = guide_legend(title = NULL))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = c(0.75, 0.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=10))

drivers_1

drivers_2 <- ggplot(Shocks_SVAR_long_filtered, aes(x=Date, y=Value, fill=Variable))+
  geom_bar(stat="identity")+
  geom_line(data=Global_remittances_cycle_filtered, aes(x=Date, y=Value, group=1),
            inherit.aes=FALSE, color="black", size=0.8)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name="", labels = NULL, n.breaks = 10, sec.axis = sec_axis(~.,name="", breaks = seq(-2,2, by=0.5))) +
  scale_x_date(labels = date_format("Q1-%Y"), limits = as.Date(c('2017/03/31','2024/03/31')),
               breaks = seq(as.Date("2017/07/31"), as.Date("2024/03/31"), by = "8 quarters"))+
  labs(title="2017-2024", subtitle = "Standard deviations from 4-quarter percent change mean", x="", y="")+
  scale_fill_manual(values = custom_colors, labels = custom_labels, guide = guide_legend(title = NULL))+
  theme(plot.title = element_text(color="black", size=14, face="bold",family="Calibri Light"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size = 8),
        plot.subtitle = element_text(hjust=1,size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        legend.position = c(0.25, 0.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=10))

drivers_2

