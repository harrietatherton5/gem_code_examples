
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            HERBACEOUS/GRASS NPP                          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. Information                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#  Example dataset and protocol provided by Katherine Gordon         
#  <katherine@schematech.co.za>; This one is also compatible with GEM
#  protocol                                                      

#  R script written by Huanyuan Zhang hyzhang1996@gmail.com
#
#  2021 August 22 as part of the African data workshop



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                2. Meta data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ???How does this script work?  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read comments first and come back here to write a paragraph (in French if you
# prefer) to summaries how was NPP calculated


##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ???Necessary input  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~





##~~~~~~~~~~~~~~~~~~~~~~~
##  ???important note----
##~~~~~~~~~~~~~~~~~~~~~~~


#...............................................................................
#                                                                              .
#  if there is no grass in a given quadrat, you should still record it with    .
#  dry mass as 0, if you lost data of a month, you need to put NA as dry mass  .
#  and put in correct date. So that the calculation of daysbetween would be    .
#  correct                                                                     .
#                                                                              .
#  If you say FALSE to Days_interval_recorded, you must run one plot at a      .
#  time, if you say TRUE, you can run all plot in one go                       .
#                                                                              .
#  You should mannually check daysBetween, make sure there are no value like   .
#  1 (1 day between collection is not possible) or 378 (in case you stop your  .
#  experiment for a year)                                                      .
#                                                                              .
#...............................................................................



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             3. data preparation                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~
##???quality check  ----
##~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(lubridate)
library(ggplot2)

#library(here)
#setwd(here::here())
source('functions.r')
setwd("F:/Side_project/african_data_workshop/General/Dataset examples/")
census <- read.csv('Herb_npp_20210822_XXX(example).csv',
                   sep = ",",
                   header = T)
str(census) # Use this to check input data, compare with metadata above
#View(census)
#summary(census)

census<-census%>%
  filter(plot_code=='ABC-01')
# One plot at a time for code simplicity
census$plot_code <- as.factor(census$plot_code)
census$sub_plot <- as.factor(census$sub_plot)
census$year <- as.numeric(census$year)
census$month <- as.numeric(census$month)
census$day <- as.numeric(census$day)
census$is_stock <- as.factor(census$is_stock)
census$dry_mass_g <- as.numeric(census$dry_mass_g)
census$Treatment_code <- as.factor(census$Treatment_code)
census$quadrat_num <- as.factor(census$quadrat_num)
census$Plants_type <- as.factor(census$Plants_type)
census$quadrat_area_m2 <- as.numeric(census$quadrat_area_m2)

##~~~~~~~~~~~~~~~~~~~~~
##???your options  ----
##~~~~~~~~~~~~~~~~~~~~~



Days_interval_recorded = FALSE

#  say True, if you have a column census$DaysBetween, unit in days,
#  say FALSE if you want this script to calculate DaysBetween based on your
#  year month day


#  we try to calculate days interval for each collection by looking
#  for the date of previous collection. if it is
#  not too much trouble, it is good to do this manually in excel because
#  NPP is calculated as roots_weight/days_interval


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      4. Calculate days_between and NPP                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



if (!Days_interval_recorded) {

  All_npp<-census%>%
    mutate(date=parse_date_time(paste(year,month,day),"ymd"))%>%
    # make a date series
   mutate(DaysBetween=get_time_diffs2(date))%>%
    filter(is_stock=='N')%>%
    mutate(npp_g_m2_per_day=dry_mass_g/DaysBetween/quadrat_area_m2)
   #Then, we could get a days interval
   # pls check that it got days between correctly
   message('I got days_between from your date, Pls check this:')
   distinct(All_npp[,c('DaysBetween','date')])
}else{
  All_npp<-census%>%
    filter(is_stock=='N')%>%
    mutate(date=parse_date_time(paste(year,month,day),"ymd"))%>%
    mutate(npp_g_m2_per_day=dry_mass_g/DaysBetween/quadrat_area_m2)
  }

All_stock<-census%>%
  filter(is_stock=='Y')%>%
  mutate(date=parse_date_time(paste(year,month,day),"ymd"))%>%
  mutate(npp_g_m2=dry_mass_g/quadrat_area_m2)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              5.unit conversion                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# Unit conversion from npp_g_m2_per_day
# to Mg C ha-1 year-1



# Carbon content
cc <- 0.5


# Divided by A --> per m2
# Multiplied by 10000 --> per ha
# Multiplied by 365 --> per year
# Divided by 10^6 --> from g to Mg
# Multiplied by carbon content --> From biomass to carbon

unit_correct <-  10000 * 365 / (10 ^ 6) * cc



All_stock$stock_MgC_ha = All_stock$npp_g_m2 * unit_correct/365
# /365 because No need to correct day unit, not applicable to stock
All_npp$npp_MgC_ha_year = All_npp$npp_g_m2_per_day * unit_correct


# Well, let's save it first
plot_name=All_stock$plot_code[1]
write.csv(All_stock, file = paste0(plot_name,"_herb_stock_finest.csv"))
write.csv(All_npp, file = paste0(plot_name,"_herb_NPP_finest.csv"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            7. data visualization                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_unique_quadrat_id=c("sub_plot","quadrat_num","date","Treatment_code","DaysBetween")
Per_quadrat_npp <- All_npp %>%
  group_by_at((my_unique_quadrat_id) )%>%
  summarise(npp_MgC_ha_year_sum = sum(npp_MgC_ha_year,na.rm=T))
# The above sum will add all plant types together, basically sum everthing we have in each quadrats
ggplot(Per_quadrat_npp, aes(x=Treatment_code, y=npp_MgC_ha_year_sum)) +
  geom_boxplot(alpha=0.3) +
  theme_bw()

ggsave(filename =paste0(plot_name,"_box_plot.png") )

Per_date_npp <- All_npp %>%
  group_by_at((my_unique_quadrat_id) )%>%
  summarise(npp_MgC_ha_year_sum = sum(npp_MgC_ha_year,na.rm=T))%>%
  ungroup()%>%
  group_by(date,DaysBetween,Treatment_code)%>%
  summarise(npp_MgC_ha_year_mean=mean(npp_MgC_ha_year_sum,na.rm=T),
            ste=standard_error_calc(npp_MgC_ha_year_sum,na.rm=T))%>%
  ungroup()

# You will need weighted average if you want to average across date
# You will need weighted average if you want to average across date
# You will need weighted average if you want to average across date

p <- ggplot(Per_date_npp, aes(x=factor(date), y=npp_MgC_ha_year_mean, fill=factor(Treatment_code))) +
  geom_bar(stat="identity", position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=npp_MgC_ha_year_mean-ste, ymax=npp_MgC_ha_year_mean+ste), width=.2,
                position=position_dodge(.9))
# Finished bar plot
p+theme_classic() +
  scale_fill_manual(values=c('#999999','#E69F00'))
ggsave(filename =paste0(plot_name,"_bar_plot.png") )
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           8. Archived information                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


