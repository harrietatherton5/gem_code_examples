
# This is for reading EGM4 data, and then filling in soil respiration partitioning code.
# This code will also extract subplot and date and plot code from file name, following this format:
# ANK-03 CUE2 1,6,12,11,14,19,24 16022017.dat
# plotcode, space, anyinfo, space, subplot codes with comma, space, date

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    usage                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/Thanos/Desktop/Carbon_analysis_2023/Bernice_partitioning/ANK PARTIONING/PARTIONING/2017/SOIL PARTITIONING/2.FEBUARY")

#  The EGM-5 soil respiration measurement is in a different format to EGM-4,    
#  this file was used to import and correct column name of EGM-5                
library(tidyverse)
library(openxlsx)
treatmeant_code=c('control_normal_litter',
                  'control_no_litter',
                  'no_root_no_mycor_no_window',
                  'mycor_window_mesh',
                  'mineral_soil_Ghana_only')

myfilelist<-list.files(path = getwd(),pattern = ".dat")

for (i in 1:length(myfilelist)) {
  
mydate<-str_extract(myfilelist[i], "[0-9]{8}")
myplot_code<-str_extract(myfilelist[i], "[A-Z]{3}-[0-9]{2}")
subplot_code<-str_extract(myfilelist[i], "([[:space:]][0-9]{1,2}.*[[:space:]])")
my_subplot_code<-str_split_1(subplot_code,',')
# read the txt file
#setwd('F:/Side_project/african_data_workshop/not_to_share_with_student/Mackline/EGM-5 Data ALL')
EGM4_raw<-read.csv(myfilelist[i],header = T,sep='\t',skip=2)%>%
  filter(!is.na(RecNo)) # remove the useless last row
# The correct column header
column_header=c('egm_measurement',
                'rec_num',
                'random_original_day',
                'random_original_month',
                'hour',
                'minute',
                'co2ref',
                'mb.Ref',
                'mbR.Temp',
                'InputA_PAR',
                'InputB_Relative_humidity',
                'InputC_Temperature',
                'InputD',
                'time',
                'InputF',
                'InputG',
                'InputH',
                'atmp',
                'probe_type')


colnames(EGM4_raw)<-column_header

rollingnum<-unique(EGM4_raw$egm_measurement)
if(length(rollingnum)==5*length(my_subplot_code)){
  sub_plot_code<-rep(my_subplot_code, times = 1, length.out = NA, each = 5)
  treatment_code_partitioning<-rep(treatmeant_code, times = length(my_subplot_code), length.out = NA, each = 1)
  newdf<-data.frame(sub_plot_code,treatment_code_partitioning,egm_measurement=unique(EGM4_raw$egm_measurement))
  EGM4_raw<-left_join(EGM4_raw,newdf,by='egm_measurement')
  }else{
  EGM4_raw<-cbind(treatment_code_partitioning = NA, EGM4_raw)
  EGM4_raw<-cbind(sub_plot_code = NA, EGM4_raw)
  warning(paste0('egm_measurement is not five times as subplot in  ',myfilelist[i]))
}

EGM4_raw<-cbind(year = substr(mydate,5,8), EGM4_raw)
EGM4_raw<-cbind(month = substr(mydate,3,4), EGM4_raw)
EGM4_raw<-cbind(day = substr(mydate,1,2), EGM4_raw)

EGM4_raw<-cbind(plot_code = myplot_code, EGM4_raw)

#  # Now you should save EGM5_clean, however, there might be strange rows at    
#  the beginning or the end, you should check mannually                         

openxlsx::write.xlsx(EGM4_raw,file = paste0(myfilelist[i],'.xlsx'),overwrite = T)
}
