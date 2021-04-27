#----------------------------------------------------------------------------------------
# Program name: abakus_tests
# Authors: Azzurra Spagnesi
# Date: April 2021
# Objective: Import data for abakus, conductivity, draw wire and flowmeter from 
#            a selected folder, manage statistics and plot the results
#----------------------------------------------------------------------------------------
#FIRST PART: DATA IMPORT AND MANAGEMENT
#----------------------------------------------------------------------------------------
#clean the virtual work folder
rm(list = ls()) 

library(tidyverse)
library(lubridate)
library(nycflights13)
library(plyr)
library(readr)
library(dplyr) #necessary to create vector of data frame names from a list of data frames
library(purrr) #necessary to create vector of data frame names from a list of data frames

#set main wd
setwd("C:/Users/azzur/Dropbox/Il mio PC (LAPTOP-AUDRMQN9)/Desktop/CFA_data/CFA_HPLC")

#set locations and select all files with "*.txt" suffix 
#from the current working directories (creating them as arrays 
#for the next steps),then import them all at once as entries 
#in a list and repeat the same operations for each device

setwd("C:/Users/azzur/Dropbox/Il mio PC (LAPTOP-AUDRMQN9)/Desktop/CFA_data/CFA_HPLC/abakus_data")
abakus_files <- as.array(list.files(pattern = ".txt"))
abakus_datalist <- lapply(abakus_files, 
                          read.table, 
                          header=FALSE, 
                          fill = TRUE,
                          sep="\t", skip = 6)

setwd("C:/Users/azzur/Dropbox/Il mio PC (LAPTOP-AUDRMQN9)/Desktop/CFA_data/CFA_HPLC/cond_data")
conductivity_files <- as.array(list.files(pattern = ".txt"))
conductivity_datalist <- lapply(conductivity_files, function(x) {
  tryCatch(read.table(x, header = FALSE, sep = ' '), error=function(e) NULL)
})

setwd("C:/Users/azzur/Dropbox/Il mio PC (LAPTOP-AUDRMQN9)/Desktop/CFA_data/CFA_HPLC/estensimetro_data")
drawwire_files <- as.array(list.files(pattern="*_height.txt"))
drawwire_datalist <- lapply(drawwire_files, 
                            tryCatch(read.table, 
                                     header=FALSE, 
                                     fill = TRUE,
                                     sep="/t", skip = 6))

setwd("C:/Users/azzur/Dropbox/Il mio PC (LAPTOP-AUDRMQN9)/Desktop/CFA_data/CFA_HPLC/flow_data")
flowmeter_files <- as.array(list.files(pattern=".txt"))
flowmeter_datalist <- lapply(flowmeter_files, 
                             tryCatch(read.table, 
                                      header=FALSE, 
                                      fill = TRUE,
                                      sep="\t", skip = 6))

setwd("C:/Users/azzur/Dropbox/Il mio PC (LAPTOP-AUDRMQN9)/Desktop/CFA_data/CFA_HPLC/logs")
log_files <- as.array(list.files(pattern="*_logbook.txt"))
log_datalist <- lapply(log_files, 
                       read.table, 
                       header=FALSE, 
                       fill = TRUE,
                       sep="\t")

#apply correspondent and reducted file names to each list.
#The reduction of file names would be fundamental for the next steps
abakus_files <- substr(abakus_files, start = 1, stop = 17) 
conductivity_files <- substr(conductivity_files, start = 1, stop = 17) 
drawwire_files <- substr(drawwire_files, start = 1, stop = 17) 
flowmeter_files <- substr(flowmeter_files, start = 1, stop = 17)
log_files <- substr(log_files, start=1, stop=17)

abakus_datalist <- setNames(abakus_datalist, abakus_files)
conductivity_datalist <- setNames(conductivity_datalist, conductivity_files)
drawwire_datalist <- setNames(drawwire_datalist, drawwire_files)
flowmeter_datalist <- setNames(flowmeter_datalist, flowmeter_files)
log_datalist <- setNames(log_datalist, log_files)

rm(abakus_files, conductivity_files, drawwire_files, flowmeter_files, log_files)

#MANAGE ABAKUS DATA
#delete dataframes with less than 34 columns
abakus_datalist <- Filter(function(x) ncol(x) == 34, abakus_datalist)
#SET HEADER for each file of abakus_datalist 
abakus_colnames <- c("Index","Duration (s)","0,8","0,9","1,0","1,1","1,2","1,3",
                     "1,4","1,6","1,8","2,0","2,2","2,4","2,6","2,9","3,2","3,5",
                     "3,9","4,3","4,8","5,3","5,8","6,4","7,1","7,8","8,6","9,5",
                     "10,5","11,6","12,8","14,1","15,5","80,0")
abakus_datalist <-  lapply(abakus_datalist, setNames, abakus_colnames)
#delete the first row from each df in the list using numeric index 
abakus_datalist <-  lapply(abakus_datalist, function(x) {x <- x[-1, ]})
#delete files with less than 200sec of acquisition (failed tests) from abakus list 
abakus_datalist <- abakus_datalist[sapply(abakus_datalist, function(x) dim(x)[1]) > 200]

#MANAGE CONDUCTIVITY DATA
#delete unnecessary columns
dropList <- c("V4", "V5", "V6", "V7", "V8", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V22")
conductivity_datalist <- lapply(conductivity_datalist, function(x) { x[dropList] <- NULL; x })
#delete dataframes with NULL dimension
conductivity_datalist <- conductivity_datalist[lapply(conductivity_datalist,length)>0] 
#delete files with less than 200sec of acquisition (failed tests) from conductivity list 
conductivity_datalist <- conductivity_datalist[sapply(conductivity_datalist, function(x) dim(x)[1]) > 200]
#rename columns for all df in the list 
col_names <- c("Date","Time_%H:%M:%S", "SENSOR1(mS)", "SENSOR2(mS)", "SENSOR4(mS)")
conductivity_datalist <-  lapply(conductivity_datalist, setNames, col_names)

#MANAGE DRAW WIRE DATA
#rename columns for all df in the list 
#delete files with less than 200sec of acquisition (failed tests) from drawwire list 
drawwire_datalist <- drawwire_datalist[sapply(drawwire_datalist, function(x) dim(x)[1]) > 200]
col_names <- c("Date","Time_%H:%M:%S", "Ice Height (mm)", "Melt speed (mm/sec)")
drawwire_datalist <-  lapply(drawwire_datalist, setNames, col_names)

#MANAGE FLOWMETER DATA
col_names <- c("Date","Time_%H:%M:%S", "Flow (ul/min)")
flowmeter_datalist <-  lapply(flowmeter_datalist, setNames, col_names)

rm(abakus_colnames, col_names, dropList)

#SELECT sample files from each list 
#extract the dfs of interest (sample) from abakus list to manage them singularly
#NB_original file '2021-04-14_130504.txt' has header to be removed 
#to fix error given by chr type
abakus_df_130504 <- as.data.frame(abakus_datalist[['2021-04-14_130504']])
abakus_df_134809 <- as.data.frame(abakus_datalist[['2021-04-14_134809']])
abakus_df_142819 <- as.data.frame(abakus_datalist[['2021-04-14_142819']])
#remove empty rows from these dfs 
abakus_df_130504 <- abakus_df_130504[-c(2506:2549), ]
abakus_df_134809 <- abakus_df_134809[-c(2339:2360),]
#remove the first row (line 0) from the 134809 and 142819 df
abakus_df_134809 <- abakus_df_134809[-1,]
abakus_df_142819 <- abakus_df_142819[-1,]
#create a new col in each dataframe for time, incrementing its value per row
#by 1 sec
start_time1 <- ymd_hms("2021-04-14 15:05:14")
start_time2 <- ymd_hms("2021-04-14 15:48:21")
start_time3 <- ymd_hms("2021-04-14 16:28:27")

abakus_df_130504$Time <- start_time1 + 1 * (seq_len(nrow(abakus_df_130504))-1) 
abakus_df_134809$Time <- start_time2 + 1 * (seq_len(nrow(abakus_df_134809))-1) 
abakus_df_142819$Time <- start_time3 + 1 * (seq_len(nrow(abakus_df_142819))-1) 
#subset all the df, keep only specific timeframes 
#130504: keep data from 15:24:03 (index 1130) to the end of df (15:46:58)
file_130504 <- abakus_df_130504[-c(1:1129), ]
#134809: keep data from the begin (15:48:21) to 15:55:35 (index 435)
file_134809_first <- abakus_df_134809[-c(436:2337), ]
#134809: keep data from 16:03:45 (index 925) to the end of df (16:27:17)
file_134809_second <- abakus_df_134809[-c(1:924), ]
#142819: keep data from the begin (16:28:27) to 16:42:30 (index 844)
file_142819 <- abakus_df_142819[-c(845:1197),]

#BAG1: 130504 + 134809 (first) df subset
bag1_files <- list(file_130504,file_134809_first)
BAG1_abakus <- do.call("rbind", bag1_files)
rm(bag1_files)
#BAG2: 134809 (second) + 142819 (first) df subset
bag2_files <- list(file_134809_second, file_142819)
BAG2_abakus <- do.call("rbind", bag2_files)
rm(bag2_files)
rm(abakus_df_130504, abakus_df_134809, abakus_df_142819, file_130504, file_134809_first,
   file_134809_second, file_142819, start_time1, start_time2, start_time3)

conductivity_sample <- conductivity_datalist[names(conductivity_datalist) %in% c("2021-04-14_130504", 
                                                                           "2021-04-14_134809",
                                                                           "2021-04-14_142819")]
estens_sample <- drawwire_datalist[names(drawwire_datalist) %in% c("2021-04-14_130504", 
                                                                       "2021-04-14_134809",
                                                                       "2021-04-14_142819")]
flow_sample <- flowmeter_datalist[names(flowmeter_datalist) %in% c("2021-04-14_130504", 
                                                                  "2021-04-14_134809",
                                                                  "2021-04-14_142819")]

#CONDUCTIVITY, DRAWWIRE AND FLOWMETER DATA: apply the strp time function 

conductivity_sample <-lapply(conductivity_sample, transform, 
                            `Time_%H:%M:%S` = as.POSIXct(strptime(`Time_%H:%M:%S`, 
                             "%H:%M:%S")))
estens_sample <-lapply(estens_sample, transform, 
                             `Time_%H:%M:%S` = as.POSIXct(strptime(`Time_%H:%M:%S`, 
                                                                   "%H:%M:%S")))
flow_sample <-lapply(flow_sample, transform, 
                             `Time_%H:%M:%S` = as.POSIXct(strptime(`Time_%H:%M:%S`, 
                                                                   "%H:%M:%S")))

#The "datalists" contain all the data, but we need to manage only a subset of files
rm(abakus_datalist, conductivity_datalist, drawwire_datalist, flowmeter_datalist)

#----------------------------------------------------------------------------------------
#SECOND PART: STATISTICS AND PLOTS
#----------------------------------------------------------------------------------------
#ABAKUS DATA: ~1 acq. per sec
#---------------------------------
BAG1_Index <- c(1:1811)
BAG1_abakus[["Index"]] <- BAG1_Index
rm(BAG1_Index)

BAG2_Index <- c(1:2257)
BAG2_abakus[["Index"]] <- BAG2_Index
rm(BAG2_Index)

#settings----------------------------------------------------------------------
BAG1_start_row_index <- 1 # first line of interest
BAG1_last_row_index <- length(BAG1_abakus$Index) # last line of interest
BAG1_start_column_index <- 3 # first channel of interest
BAG1_last_column_index <- 34 #last channel of interest

BAG2_start_row_index <- 1 # first line of interest
BAG2_last_row_index <- length(BAG2_abakus$Index) # last line of interest
BAG2_start_column_index <- 3 # first channel of interest
BAG2_last_column_index <- 34 #last channel of interest

#BAG1--------------------------------------------------------------------------
#Tot counts per channel
BAG1_distr <- colSums(BAG1_abakus[BAG1_start_row_index:BAG1_last_row_index,BAG1_start_column_index:BAG1_last_column_index])
#Define size classes
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,
                  3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,
                  14.1,15.5,80.0)
barplot(BAG1_distr,
        names.arg = size_classes,
        space = 0,
        border = NA,
        main = "BAG1_Tot counts per channel",
        ylab = "Counts",
        xlab = expression(paste("Diameter (", mu, "m)")))

#Compute cumulative distribution
BAG1_abakus <- BAG1_abakus[1:(length(BAG1_abakus)-1)]
BAG1_counts <- apply(BAG1_abakus[3:ncol(BAG1_abakus)],2,sum)
BAG1_cumDistr <- cumsum(BAG1_counts)

#Display cumulative distribution 
plot(BAG1_cumDistr, type= "l", col="blue",
        main = "BAG1_Cumulative distribution",
        ylab = "Counts",
        xlab = "Channels")

#Compute differential distribution weighted by volume
B1_counts <- apply(BAG1_abakus[3:ncol(BAG1_abakus)],2,sum)
vec_diameters_corr <- c(rep(0,31))

for (i in seq(1,length(size_classes)-1)) {
  vec_diameters_corr[i] <- mean(c(size_classes[i],size_classes[i+1])) 
}

BAG1_diffDistr_Vol <- (4/3*pi*(vec_diameters_corr/2)^3)*diff(BAG1_cumDistr)/(diff(log(size_classes)))

par(mfrow=c(2,1))
par(mar=c(4,4.5,2.5,4))
barplot(BAG1_diffDistr_Vol, col=rgb(1, 0, 0, .5),
     main = "BAG1_Differential distribution (Vol)",
     ylab = "dV/dlog(d)",
     xlab = expression(paste("Diameter (", mu, "m)")))
     box()
     
#Compute differential distribution (Counts)
BAG1_CountsDiffDistr <-  diff(BAG1_cumDistr)/diff(log(size_classes))
barplot(BAG1_CountsDiffDistr, col=rgb(0, 1, 0, .5),
     main = "BAG1_Differential distribution (Counts)",
     ylab = "dN/dlog(d)",
     xlab = expression(paste("Diameter (", mu, "m)")))
     box()
dev.off()
     
#BAG2--------------------------------------------------------------------------
#Tot counts per channel
BAG2_distr <- colSums(BAG2_abakus[BAG2_start_row_index:BAG2_last_row_index,BAG2_start_column_index:BAG2_last_column_index])
#Define size classes
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,
                       3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,
                       14.1,15.5,80.0)
barplot(BAG2_distr,
  names.arg = size_classes,
  space = 0,
  border = NA,
  main = "BAG2_Tot counts per channel",
  ylab = "Counts",
  xlab = expression(paste("Diameter (", mu, "m)")))
     
#Compute cumulative distribution
BAG2_abakus <- BAG2_abakus[1:(length(BAG2_abakus)-1)]
BAG2_counts <- apply(BAG2_abakus[3:ncol(BAG2_abakus)],2,sum)
BAG2_cumDistr <- cumsum(BAG2_counts)
     
#Display cumulative distribution 
plot(BAG2_cumDistr, type= "l", col="blue",
main = "BAG2_Cumulative distribution",
ylab = "Counts",
xlab = "Channels")
     
#Compute differential distribution weighted by volume
B2_counts <- apply(BAG2_abakus[3:ncol(BAG2_abakus)],2,sum)
vec_diameters_corr <- c(rep(0,31))
     
for (i in seq(1,length(size_classes)-1)) {
 vec_diameters_corr[i] <- mean(c(size_classes[i],size_classes[i+1])) 
 }
     
BAG2_diffDistr_Vol <- (4/3*pi*(vec_diameters_corr/2)^3)*diff(BAG2_cumDistr)/(diff(log(size_classes)))
     
par(mfrow=c(2,1))
par(mar=c(4,4.5,2.5,4))
barplot(BAG2_diffDistr_Vol, col=rgb(1, 0, 0, .8),
  main = "BAG2_Differential distribution (Vol)",
  ylab = "dV/dlog(d)",
  xlab = expression(paste("Diameter (", mu, "m)")))
  box()
     
#Compute differential distribution (Counts)
BAG2_CountsDiffDistr <-  diff(BAG2_cumDistr)/diff(log(size_classes))
barplot(BAG2_CountsDiffDistr, col=rgb(0, 1, 0, .8),
 main = "BAG2_Differential distribution (Counts)",
 ylab = "dN/dlog(d)",
 xlab = expression(paste("Diameter (", mu, "m)")))
box()
     
#--------------------PLOTS_COMPARISON-------------------------------------------
#CUMULATIVE DISTRIBUTIONS
par(mar = c(4,4.5,2.5,4))      
plot(BAG1_cumDistr, type= "l", col="blue",
     main = "Cumulative distribution",
     ylab = "Counts",
     xlab = "Channels")
par(new=TRUE)
plot(BAG2_cumDistr, type= "l", col="green",
     axes = FALSE, xlab = "", ylab = "")
     legend("bottomright", inset= .02, legend=c("BAG1", "BAG2"),
     col=c("blue","green"), lty=1:1, lwd=2, cex=0.8)

#DIFFERENTIAL DISTRIBUTIONS(Vol)
plot(BAG1_diffDistr_Vol, type="l", col="blue",
        main = "Differential distribution (Vol)",
        ylab = "dV/dlog(d)",
        xlab = "Channels")
par(new=TRUE)
plot(BAG2_diffDistr_Vol, type="l", col="green",
        axes = FALSE, xlab = "", ylab = "")
        axis(side = 4, at = pretty(range(BAG2_diffDistr_Vol)))     
        legend("topleft", inset= .02, legend=c("BAG1", "BAG2"),
        col=c("blue","green"), lty=1:1, lwd=2, cex=0.8)
        
#DIFFERENTIAL DISTRIBUTIONS(Counts)
plot(BAG1_CountsDiffDistr, type="l", col="blue",
     main = "Differential distribution (Counts)",
     ylab = "dN/dlog(d)",
     xlab = "Channels")
par(new=TRUE)
plot(BAG2_CountsDiffDistr, type="l", col="green",
     axes = FALSE, xlab = "", ylab = "")
     axis(side = 4, at = pretty(range(BAG2_CountsDiffDistr)))     
     legend("topright", inset= .02, legend=c("BAG1", "BAG2"),
     col=c("blue","green"), lty=1:1, lwd=2, cex=0.8)
        
        
