# Read metedata of the Sonde dataset  (excel file)
# Loading
  library("readxl")
  library("tidyverse")
  
  
# xls files
  file_path <- "C:/Users/Zhong.Zhang/Documents/Monitoring Analysis/Work/2022/Sonde/Data"
  metadata_Sonde <- read_excel(paste0(file_path,"/Metadata_Sonde.xlsx"))
  camrose_knock <- read_excel(paste0(file_path,"/Camrose_Knock_Brook.xlsx"))
  pelcomb <- read_excel(paste0(file_path,"/Pelcomb_Brook.xlsx"))
  
# add df_Name as column in metadate dataframe
  
  metadata_Sonde$df_Name <- c("camrose_knock","pelcomb")

# add Id and sample name column in the dataframe
  
  camrose_knock <- mutate(camrose_knock, ID = 85006, Name = "Camrose/knock Brook at Cutty B",.before = `Date (MM/DD/YYYY)`)
  pelcomb <- mutate(pelcomb, ID = 85007, Name = "Pelcomb Brook at Crowhill Brid", .before =`Date (MM/DD/YYYY)`)
  
  
# rename column "Time (Fract. Sec)" as Time for camrose_knock
  
  names(camrose_knock)[names(camrose_knock) == "Time (Fract. Sec)"] <- "Time"
 

# join the sample data from the two sites. Please note the columns in each sites
# are not the exact same alghough they both have 20 columns. 
# For camrose_knowck it has a column of "Tss mg/l" while pelcomb has a column of "Pressure psi a"
 
  join_camrose_pelcomb <- bind_rows(camrose_knock,pelcomb)
  
  
# remove backsplash from the name column
  
  join_camrose_pelcomb$Name[join_camrose_pelcomb$Name=="Camrose/knock Brook at Cutty B"] <- "Camrose_knock Brook at Cutty B"
  metadata_Sonde$Name[metadata_Sonde$Name=="Camrose/Knock Brook at Cutty B"] <- "Camrose_knock Brook at Cutty B"
  
  
  
   # Reoder the columns and make sure that ID and sample name in front of the dataframe
  # camrose_knock <- camrose_knock[,c(19:20,1:18)]
  # pelcomb <- pelcomb[,c(19:20,1:18)]
  
  
  