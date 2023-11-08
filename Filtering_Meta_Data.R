library(stringr)
library(tidyr)
library(dplyr)
library(writexl)


maindir<- "C:/Users/dimonr/OneDrive - DPIE/R/projects/Filtering Meta Data/"
setwd(maindir)

DatabaseFile <- read.csv(paste0(maindir,"/Database_File.csv"), header=T)
TargetSamples <- read.csv(paste0(maindir,"/Target_Samples.csv"), header=T)
ReplaceList <- read.csv(paste0(maindir,"/Replace_List.csv"), header=T)


metafile <- c()


for (i in 1:length(TargetSamples$NSWNumbers)){

   samplenum <- which(DatabaseFile$nswNumber==TargetSamples$NSWNumbers[i])


  
  if (length(samplenum)==0){
    print(paste0(TargetSamples$NSWNumbers[i], " Not Found in Database File!"))
    metafile$sample[i] <- c(paste0("Missing Metadata for ", TargetSamples$NSWNumbers[i]))
  } else{
    
  metafile$sample[i] <- DatabaseFile$nswNumber[samplenum]
  metafile$site[i] <- paste0(na.omit(DatabaseFile$locality[samplenum]), " ",
                             na.omit(DatabaseFile$nationalPark[samplenum]), " ",
                             na.omit(DatabaseFile$natureReserve[samplenum]), " ", 
                             na.omit(DatabaseFile$stateConservationArea[samplenum]), " ",
                             na.omit(DatabaseFile$stateForest[samplenum]), " ",
                             na.omit(DatabaseFile$otherTenure[samplenum]))
  metafile$lat[i] <- DatabaseFile$decimalLatitude[samplenum]
  metafile$long[i] <- DatabaseFile$decimalLongitude[samplenum]
  metafile$Species[i] <- DatabaseFile$acceptedName[samplenum]
  metafile$Collection_Date[i]<- DatabaseFile$sampleDate[samplenum]
  metafile$Collection_Notes[i] <- DatabaseFile$collectionNotes[samplenum]
  metafile$General_Notes[i] <- DatabaseFile$generalNotes[samplenum]
  metafile$Hybrid_Status[i] <- paste0(DatabaseFile$hybridStatus[samplenum]," ", DatabaseFile$hybridStatusNotes[samplenum])
  metafile$Species_ID_Confidence[i] <- paste0(DatabaseFile$speciesIDConfidence[samplenum]," ",DatabaseFile$speciesIDNotes[samplenum])
  metafile$Population_Notes[i] <- DatabaseFile$populationNotes[samplenum]
  metafile$People_Present[i] <-DatabaseFile$peoplePresent[samplenum]
  metafile$Verbatim_Locality_Description[i] <- na.omit(DatabaseFile$locality[samplenum])
  
  # Remove duplicate values from site column
  d <- unlist(strsplit(metafile$site[i], split=" "))
  
  metafile$site[i] <- paste(unique(d), collapse = ' ')  
  
  if(!metafile$site[i]==""){

  # Remove any numbers which are greater than 5 or less than -5 (i.e. postcodes or most street numbers) while keeping site numbers and alphanumeric characters, or numbers with a comma e.g. Bulburin site 1, 2, and 3))
  d <- unlist(strsplit(metafile$site[i], split=" "))
 
   tfd <- grepl("\\d", d)
  
  for (x in 1:length(d)){
    if((tfd[x])==TRUE){
      if (is.na(as.numeric(d[x]))==FALSE){
        if((as.numeric(d[x])>5)==TRUE | (as.numeric(d[x])< -5)==TRUE){
          d <- gsub(d[x],"",d)
        }
      }
    }
  }
  metafile$site[i] <- paste(unique(d), collapse = ' ')  

  
  # Alert if lat/long values don't seem correct for Oceania region
  if (is.na(metafile$lat[i])==TRUE){
    print("Latitude NA value")
  } else if (metafile$lat[i] > 0){
    print("latitude values are non-negative. Check GPS data from file!")
  } else if (metafile$long[i] > 200 | metafile$long[i] < 100){
    print("Longitude values don't match oceania. Check GPS data from file!")
  } 
  
  } 
  }
}     


replace(metafile, is.na(metafile), "")


for(z in 1:length(ReplaceList$InitialString)) {
  metafile$site <- gsub(c(ReplaceList$InitialString[z]), c(ReplaceList$ReplacedString[z]), metafile$site, ignore.case = TRUE)
}



#run the number removal script again to remove any additonal numbers after the replace list has passed through the site column
for (i in 1:length(TargetSamples$NSWNumbers)){
  
  samplenum <- which(DatabaseFile$nswNumber==TargetSamples$NSWNumbers[i])
  
  if(!metafile$site[i]==""&&is.na(metafile$site[i])==FALSE){
    
d <- unlist(strsplit(metafile$site[i], split=" "))

tfd <- grepl("\\d", d)

for (x in 1:length(d)){
  if((tfd[x])==TRUE){
    if (is.na(as.numeric(d[x]))==FALSE){
      if((as.numeric(d[x])>5)==TRUE | (as.numeric(d[x])< -5)==TRUE){
        d <- gsub(d[x],"",d)
      }
    }
  }
}
metafile$site[i] <- paste(unique(d), collapse = ' ')  

  }
  
}



commonspecies <- tail(names(sort(table(metafile$Species))), 1)

metafile[] <- lapply(metafile, function(x) gsub(c("NA"), "", x))

outputdir<- "C:/Users/dimonr/OneDrive - DPIE/R/projects/Filtering Meta Data/Filtered_Outputs/"


write_xlsx(data.frame(metafile), paste0(outputdir, commonspecies, " filtered metadata file.xlsx"))


