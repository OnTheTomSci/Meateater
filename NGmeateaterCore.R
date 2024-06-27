# install dependancies 
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)

M15 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 1)
M12 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 2)
M11 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 3)
M10 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 4)
M1 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 5)
HC25 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 6)
HC22 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 7)
HC20 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 8)
HC14 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 9)
HC13 <- read_xls("E:/MECFS_Biomarkers/MECFS_Nglycans/MS2ScanRawMeat.xls", sheet = 10)

print(max(HC13$MZ))

# remove scans with a chare state of 0
HC13_chag <- HC13 %>% filter(Charge != "0")
HC14_chag <- HC14 %>% filter(Charge != "0")
HC20_chag <- HC20 %>% filter(Charge != "0")
HC22_chag <- HC22 %>% filter(Charge != "0")
HC25_chag <- HC25 %>% filter(Charge != "0")
M1_chag <- M1 %>% filter(Charge != "0")
M10_chag <- M10 %>% filter(Charge != "0")
M11_chag <- M11 %>% filter(Charge != "0")
M12_chag <- M12 %>% filter(Charge != "0")
M15_chag <- M15 %>% filter(Charge != "0")

# round MZ to 1 decimal place 
HC13_chag$MZ <- round(HC13_chag$MZ, digits = 1)
HC14_chag$MZ <- round(HC14_chag$MZ, digits = 1)
HC20_chag$MZ <- round(HC20_chag$MZ, digits = 1)
HC22_chag$MZ <- round(HC22_chag$MZ, digits = 1)
HC25_chag$MZ <- round(HC25_chag$MZ, digits = 1)
M1_chag$MZ <- round(M1_chag$MZ, digits = 1)
M10_chag$MZ <- round(M10_chag$MZ, digits = 1)
M11_chag$MZ <- round(M11_chag$MZ, digits = 1)
M12_chag$MZ <- round(M12_chag$MZ, digits = 1)
M15_chag$MZ <- round(M15_chag$MZ, digits = 1)

# remove rows with duplicated MZ values 
# may need to get rid of this section as some redundant MZ will be at different 
# retention times that correspond to different isomers of the same glycan
HC13_chag <-distinct(HC13_chag, MZ, .keep_all = TRUE)
HC14_chag <-distinct(HC14_chag, MZ, .keep_all = TRUE)
HC20_chag <-distinct(HC20_chag, MZ, .keep_all = TRUE)
HC22_chag <-distinct(HC22_chag, MZ, .keep_all = TRUE)
HC25_chag <-distinct(HC25_chag, MZ, .keep_all = TRUE)
M1_chag <-distinct(M1_chag, MZ, .keep_all = TRUE)
M10_chag <-distinct(M10_chag, MZ, .keep_all = TRUE)
M11_chag <-distinct(M11_chag, MZ, .keep_all = TRUE)
M12_chag <-distinct(M12_chag, MZ, .keep_all = TRUE)
M15_chag <-distinct(M15_chag, MZ, .keep_all = TRUE)

# calculate charged monoisotopic mass

for ( i in HC13_chag)
  
{
 redglymass <- c(HC13_chag$MZ *  HC13_chag$Charge +  HC13_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
HC13_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in HC14_chag)
  
{
  redglymass <- c(HC14_chag$MZ *  HC14_chag$Charge +  HC14_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
HC15_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in HC20_chag)
  
{
  redglymass <- c(HC20_chag$MZ *  HC20_chag$Charge +  HC20_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
HC20_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in HC22_chag)
  
{
  redglymass <- c(HC22_chag$MZ *  HC22_chag$Charge +  HC22_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
HC22_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in HC25_chag)
  
{
  redglymass <- c(HC25_chag$MZ *  HC25_chag$Charge +  HC25_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
HC25_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in M1_chag)
  
{
  redglymass <- c(M1_chag$MZ *  M1_chag$Charge +  M1_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
M1_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in M10_chag)
  
{
  redglymass <- c(M10_chag$MZ *  M10_chag$Charge +  M10_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
M10_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in M11_chag)
  
{
  redglymass <- c(M11_chag$MZ *  M11_chag$Charge +  M11_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
M11_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in M12_chag)
  
{
  redglymass <- c(M12_chag$MZ *  M12_chag$Charge +  M12_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
M12_chag$redglymass <-redglymass

# calculate charged monoisotopic mass

for ( i in M15_chag)
  
{
  redglymass <- c(M15_chag$MZ *  M15_chag$Charge +  M15_chag$Charge - 1)
}
# append reduced monoisotopic calulated glycan mass to data frame 
M15_chag$redglymass <-redglymass

# write to csv files 
write.csv(HC13_chag, "HC13_NgPeaks.csv")
write.csv(HC14_chag, "HC14_NgPeaks.csv")
write.csv(HC20_chag, "HC20_NgPeaks.csv")
write.csv(HC22_chag, "HC22_NgPeaks.csv")
write.csv(HC25_chag, "HC25_NgPeaks.csv")
write.csv(M1_chag, "M1_NgPeaks.csv")
write.csv(M10_chag, "M10_NgPeaks.csv")
write.csv(M11_chag, "M11_NgPeaks.csv")
write.csv(M12_chag, "M12_NgPeaks.csv")
write.csv(M15_chag, "M15_NgPeaks.csv")

#remove extra column before combining data
M1_chag <- select(M1_chag, -M)

# combine glycan lists from all replicates together 
Total_NG <- rbind(HC13_chag, HC14_chag, HC20_chag, HC22_chag, HC25_chag, M1_chag, M10_chag, M11_chag, M12_chag, M15_chag)

# remove rows with duplicates of reduced monoisotopic glycan masses 
Total_NG <-distinct(Total_NG, MZ, .keep_all = TRUE)
write.csv(Total_NG, "Total_NG_Peaks.csv")

# remove the rest of the coloums and export to a glycan mass list for searching in glycomod 
gmod_masses <- Total_NG %>% select(MZ)
write.csv(gmod_masses, "gmod_Mass_list.csv")
