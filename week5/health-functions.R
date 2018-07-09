library(dplyr)
library(DataExplorer)
library(ggplot2)
library(plotly)
library(data.table)
library(zipcode)
library(tidyverse)
library(stringr)
library(maps)
library(kableExtra)
library(RcppRoll)
library(plotly)
library(GGally)


### Parsing only numbers from drug definition ###

parse_num <- function(df){
  df1 <- df %>%
    mutate(DRG_Num=substr(x = DRG_Def, start=1, stop=3))
 
  return (df1)
}


### Adding group_class ###

group_class<- function(df)
{
 df1 <- df %>% 
    group_by(DRG_Def) %>%
    mutate(n_row_group=n())%>%
    ungroup()%>%
    mutate(group_class= cut(n_row_group, breaks = 3, labels = c("1_to_945", "946_to_1890", "1890_to_2837")))  
 return(df1)
}


### Adding Standardized Total Payment ###
add_standard_payment <- function(df){
  df1 <- df %>%
    group_by(DRG_Num, Pro_State) %>%
    mutate(Mu_DRG_State_Total_Payment=mean(Avg_Tot_Payments), 
           Sd_DRG_State_Total_Payment=sd(Avg_Tot_Payments),
           Std_DRG_State_Total_Payment=(Avg_Tot_Payments-Mu_DRG_State_Total_Payment)/Sd_DRG_State_Total_Payment) %>%
    ungroup()
  
  return(df1)
}


### Adding Standardized Difference ###
add_standard<-function(df){
  df1 <- df %>%
    mutate(Diff_MedCov_MedPay=Avg_Cov_Charges-Avg_Med_Payments) %>%
    group_by(DRG_Num) %>%
    mutate(Mu_Diff=mean(Diff_MedCov_MedPay), Sd_Diff=sd(Diff_MedCov_MedPay)) %>%
    mutate(Standard_Diff=(Diff_MedCov_MedPay-Mu_Diff)/Sd_Diff)%>%
    ungroup()
  return(df1)
}



### Adding Standardized Difference ###
add_standard2 <- function(df){
  df1 <- df %>%
    group_by(DRG_Num, Pro_State)%>%
    mutate(Mean_Discharge=mean(Tot_Discharges), 
           Stdv_Discharges=sd(Tot_Discharges),
           Standardized_Discharges= (Tot_Discharges-Mean_Discharge)/Stdv_Discharges )%>%
    ungroup()
  
  return(df1)
}












