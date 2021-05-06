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

library(ampd)        #An Algorithm for Automatic Peak Detection in Noisy Periodic and
#Quasi-Periodic Signals
library(zoo)
library(Bolstad2)
library(changepoint)
library(DescTools)
library(tidyverse)
library(lubridate)
library(nycflights13)
library(plyr)
library(readr)
library(dplyr) #necessary to create vector of data frame names from a list of data frames
library(purrr) #necessary to create vector of data frame names from a list of data frames
library(xlsx)

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
                                     fill = TRUE, dec=",",
                                     sep="\t", skip = 6))

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
attach(mtcars)
par(mfrow=c(2,2))

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

#BAG2--------------------------------------------------------------------------
#Tot counts per channel
BAG2_distr <- colSums(BAG2_abakus[BAG2_start_row_index:BAG2_last_row_index,BAG2_start_column_index:BAG2_last_column_index])
#Define size classes
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,
                  3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,
                  14.1,15.5,80.0)
attach(mtcars)
par(mfrow=c(2,2))
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
par(mfrow=c(1,1))
#Define a function for scientific notation 
sciNotation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sciNotation(x[1]), sciNotation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  as.expression(substitute(base %*% 10^exponent, 
                           list(base = base, exponent = exponent)))
}

#CUMULATIVE DISTRIBUTIONS
par(mar = c(4,4.5,2.5,4))      
plot(BAG1_cumDistr, type= "l", col="blue",
     main = "Cumulative distribution",
     ylab = "Counts",
     xlab = "Channels")
par(new=TRUE)
plot(BAG2_cumDistr, type= "l", col="green",
     axes = FALSE, xlab = "", ylab = "")
axis(4, at = axTicks(4), label = sciNotation(axTicks(4), 4))
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
axis(4, at = axTicks(4), label = sciNotation(axTicks(4), 4))
#axis(side = 4, at = pretty(range(BAG2_CountsDiffDistr)))     
legend("topright", inset= .02, legend=c("BAG1", "BAG2"),
       col=c("blue","green"), lty=1:1, lwd=2, cex=0.8)

rm(B1_counts, B2_counts,BAG1_counts, BAG1_CountsDiffDistr, BAG1_cumDistr, 
   BAG1_diffDistr_Vol, BAG1_distr, BAG1_last_column_index, BAG1_last_row_index, 
   BAG1_start_column_index,BAG1_start_row_index, BAG2_counts, BAG2_cumDistr, 
   BAG2_distr,BAG2_CountsDiffDistr, BAG2_diffDistr_Vol, BAG2_last_column_index, 
   BAG2_last_row_index, BAG2_start_column_index,BAG2_start_row_index, i, 
   size_classes, vec_diameters_corr)        

#-------------------------------------------------------------------------------
#CONDUCTIVITY DATA: ~1 acq. per sec
#-------------------------------------------------------------------------------
#extract the dfs of interest (sample) from conductivity list to manage them singularly
cond_df_130504 <- as.data.frame(conductivity_sample[['2021-04-14_130504']])
cond_df_134809 <- as.data.frame(conductivity_sample[['2021-04-14_134809']])
cond_df_142819 <- as.data.frame(conductivity_sample[['2021-04-14_142819']])
#subset all the df, keep only specific timeframes 
#130504: keep data from 15:24:03 (row 1116) to the end of df (15:47:39)
file_130504 <- cond_df_130504[-c(1:1115), ]
#134809: keep data from the begin (15:48:19) to 15:55:35 (row 431)
file_134809_first <- cond_df_134809[-c(432:2327), ]
#134809: keep data from 16:03:45 (row 914) to the end of df (16:27:38)
file_134809_second <- cond_df_134809[-c(1:913), ]

#BAG1: 130504 + 134809 (first) df subset
bag1_files <- list(file_130504,file_134809_first)
BAG1_cond <- do.call("rbind", bag1_files)
rm(bag1_files)

#BAG2: 134809 (second) + 142819 (all!) df subset
bag2_files <- list(file_134809_second, cond_df_142819)
BAG2_cond <- do.call("rbind", bag2_files)
rm(bag2_files)
rm(cond_df_130504, cond_df_134809, cond_df_142819, conductivity_sample,
   file_130504, file_134809_first, file_134809_second)

#add a column index for simplicity
BAG1_Index <- c(1:1828)
BAG1_cond[["Index"]] <- BAG1_Index
rm(BAG1_Index)

BAG2_Index <- c(1:2595)  
BAG2_cond[["Index"]] <- BAG2_Index
rm(BAG2_Index)

#ONLY sensor1 has a spike that need to be removed (not revealed by the other sensors!)
BAG1_cond <- BAG1_cond[-c(1282), ]
BAG2_cond <- BAG2_cond[-c(881), ]

#SMOOTH THE DATA AND FIND PEAKS, THEN COMPUTE THE DISTANCE AMONG SENSORS FOR EACH RUN. 
#data smoothing via local polynomial regression (LOESS) 
tmp.span <- 0.1
B1s1 <- unlist(BAG1_cond[,3])
B1s2 <- unlist(BAG1_cond[,4])
B1s4 <- unlist(BAG1_cond[,5])
#
B2s1 <- unlist(BAG2_cond[,3])
B2s2 <- unlist(BAG2_cond[,4])
B2s4 <- unlist(BAG2_cond[,5])

x1 <- BAG1_cond$Index
x2 <- BAG2_cond$Index

#BAG1 sensors: DATA SMOOTHING---------------------------------------------------
BAG1_cond1 <- loess(B1s1 ~ x1, span=tmp.span)$fitted
BAG1_cond2 <- loess(B1s2 ~ x1, span=tmp.span)$fitted
BAG1_cond4 <- loess(B1s4 ~ x1, span=tmp.span)$fitted

#PLOTS: BAG1 with sensor1, sensor2, sensor4
plot(BAG1_cond$SENSOR1.mS. ~ BAG1_cond$Index , ylim = c(0,0.04), 
     type="l", main="BAG1: CONDUCTIVITY DATA", xlab="Time(sec)", ylab="conductivity (ms)")
lines(BAG1_cond$SENSOR2.mS. ~ BAG1_cond$Index, col="red")
lines(BAG1_cond$SENSOR4.mS. ~ BAG1_cond$Index, col="blue")
legend("topleft", inset=.01, legend=c("RAW_Sensor1: start", 
                                      "RAW_Sensor2: HPLC", "RAW_Sensor4: FC_dust"),
       col=c("black","red","blue"), lty=1, lwd=1, bty = "n", cex=0.6)

par(new=TRUE) 

plot(x1, BAG1_cond1, type = 'l', lty=2, ylim = c(0,0.04), col="gray", lwd=3,
     axes = FALSE, xlab = "", ylab = "")
lines(x1, BAG1_cond2, type = 'l', lty=2, col="tomato", lwd=3)
lines(x1, BAG1_cond4, type = 'l', lty=2, col="deepskyblue", lwd=3)
legend("topright", inset=c(-0.3,0), legend=c("SMOOTHED_Sensor1: start", "SMOOTHED_Sensor2: HPLC",
                                             "SMOOTHED_Sensor4: FC_dust"),
       col=c("gray","tomato","deepskyblue"), 
       lty=2, lwd=2, bty = "n", cex=0.6)

#BAG2 sensors: DATA SMOOTHING---------------------------------------------------
BAG2_cond1 <- loess(B2s1 ~ x2, span=tmp.span)$fitted
BAG2_cond2 <- loess(B2s2 ~ x2, span=tmp.span)$fitted
BAG2_cond4 <- loess(B2s4 ~ x2, span=tmp.span)$fitted
#PLOTS: BAG2 with sensor1, sensor2, sensor4
plot(BAG2_cond$SENSOR1.mS. ~ BAG2_cond$Index, ylim = c(0,0.04), 
     type="l", main="BAG2: CONDUCTIVITY DATA", xlab="Time(sec)", ylab="conductivity (ms)")
lines(BAG2_cond$SENSOR2.mS. ~ BAG2_cond$Index, col="red")
lines(BAG2_cond$SENSOR4.mS. ~ BAG2_cond$Index, col="blue")
legend("topleft", inset=.01, legend=c("RAW_Sensor1: start", 
                                      "RAW_Sensor2: HPLC", "RAW_Sensor4: FC_dust"),
       col=c("black","red","blue"), lty=1, lwd=1, bty = "n", cex=0.6)

par(new=TRUE) 

plot(x2, BAG2_cond1, type = 'l', lty=2, ylim = c(0,0.04), col="gray", lwd=3,
     axes = FALSE, xlab = "", ylab = "")
lines(x2, BAG2_cond2, type = 'l', lty=2, col="tomato", lwd=3)
lines(x2, BAG2_cond4, type = 'l', lty=2, col="deepskyblue", lwd=3)
legend("topright", inset=c(-0.3,0), legend=c("SMOOTHED_Sensor1: start", "SMOOTHED_Sensor2: HPLC",
                                             "SMOOTHED_Sensor4: FC_dust"),
       col=c("gray","tomato","deepskyblue"), 
       lty=2, lwd=2, bty = "n", cex=0.6)

#AREA UNDER THE CURVE-BAG2------------------------------------------------------
#consider only BAG2 !!!
#SENSOR1-------------------
#transform conductivity vectors for each sensor of BAG2 in df
BAG2_cond1_smooth <- as.data.frame(BAG2_cond1)
BAG2_cond2_smooth <- as.data.frame(BAG2_cond2)
BAG2_cond4_smooth <- as.data.frame(BAG2_cond4)

#add index to df for BAG2:sens1 smoothed data   
BAG2_Index <- c(1:2594)
BAG2_cond1_smooth[["Index"]] <- BAG2_Index
#
BAG2_cond2_smooth[["Index"]] <- BAG2_Index
#
BAG2_cond4_smooth[["Index"]] <- BAG2_Index

#normalize smoothed values (BAG2)
bag2_s1_norm <- (BAG2_cond1_smooth$BAG2_cond1 - min(BAG2_cond1_smooth$BAG2_cond1)) / (max(BAG2_cond1_smooth$BAG2_cond1) - min(BAG2_cond1_smooth$BAG2_cond1))
bag2_s2_norm <- (BAG2_cond2_smooth$BAG2_cond2 - min(BAG2_cond2_smooth$BAG2_cond2)) / (max(BAG2_cond2_smooth$BAG2_cond2) - min(BAG2_cond2_smooth$BAG2_cond2))
bag2_s4_norm <- (BAG2_cond4_smooth$BAG2_cond4 - min(BAG2_cond4_smooth$BAG2_cond4)) / (max(BAG2_cond4_smooth$BAG2_cond4) - min(BAG2_cond4_smooth$BAG2_cond4))

#find change points with cpt.mean function
bag2_s1_norm_cpt = cpt.mean(bag2_s1_norm, penalty = "Manual", method = "PELT", pen.value = 1 )
bag2_s2_norm_cpt = cpt.mean(bag2_s2_norm, penalty = "Manual", method = "PELT", pen.value = 1 )
bag2_s4_norm_cpt = cpt.mean(bag2_s4_norm, penalty = "Manual", method = "PELT", pen.value = 1 )

#transform normalized smoothed values in df (cannot do it before, otherwise they are
#saved as formal classes -if you do it all in one step- and change points function
#does not work)
bag2_s1_norm <- as.data.frame(bag2_s1_norm)
bag2_s2_norm <- as.data.frame(bag2_s2_norm)
bag2_s4_norm <- as.data.frame(bag2_s4_norm)
#Add indexes
norm_Index <- c(1:2594)
bag2_s1_norm[["Index"]] <- norm_Index
bag2_s2_norm[["Index"]] <- norm_Index
bag2_s4_norm[["Index"]] <- norm_Index

#Display graphs
plot(bag2_s1_norm$Index, bag2_s1_norm$bag2_s1_norm, type = 'l', lty=1, ylim = c(0,1), col="gray", lwd=3,
     main="BAG2: NORMALIZED-SMOOTHED DATA", xlab="Time(sec)", ylab="conductivity (ms)")
lines(bag2_s2_norm$Index, bag2_s2_norm$bag2_s2_norm, type = 'l', lty=1, col="tomato", lwd=3)
lines(bag2_s4_norm$Index, bag2_s4_norm$bag2_s4_norm, type = 'l', lty=1, col="deepskyblue", lwd=3)
legend("topleft", legend=c("SMOOTHED_Sensor1: start", 
                           "SMOOTHED_Sensor2: HPLC",
                           "SMOOTHED_Sensor4: FC_dust"),
       col=c("gray","tomato","deepskyblue"), 
       lty=1, lwd=3, bty = "n", cex=0.6)

rm(B1s1, B1s2, B1s4, B2s1, B2s2, B2s4, BAG1_cond1,BAG1_cond2,BAG1_cond4,
   BAG2_cond1,BAG2_cond2,BAG2_cond4,BAG2_Index,norm_Index,tmp.span, x1,x2)

#set start points and end points for sensor1 (data smoothed from BAG2)
bag2_s1_start_1 <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[2]]
bag2_s1_end_1   <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[3]]

bag2_s1_start_2 <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[4]]
bag2_s1_end_2   <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[5]]

bag2_s1_start_3 <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[6]]
bag2_s1_end_3   <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[7.5]]

bag2_s1_start_4 <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[8]]
bag2_s1_end_4   <- bag2_s1_norm$Index[bag2_s1_norm_cpt@cpts[9]]

dumb_smooth <- 30
bag2_sub_peak1 <- bag2_s1_norm[which(bag2_s1_norm$Index >= (bag2_s1_start_1 - dumb_smooth) & bag2_s1_norm$Index <= (bag2_s1_end_1 + dumb_smooth)),]
bag2_sub_peak2 <- bag2_s1_norm[which(bag2_s1_norm$Index >= (bag2_s1_start_2 - dumb_smooth) & bag2_s1_norm$Index <= (bag2_s1_end_2 + dumb_smooth)),]
bag2_sub_peak3 <- bag2_s1_norm[which(bag2_s1_norm$Index >= (bag2_s1_start_3 - dumb_smooth) & bag2_s1_norm$Index <= (bag2_s1_end_3 + dumb_smooth)),]
bag2_sub_peak4 <- bag2_s1_norm[which(bag2_s1_norm$Index >= (bag2_s1_start_4 - dumb_smooth) & bag2_s1_norm$Index <= (bag2_s1_end_4 + dumb_smooth)),]

polygon(x = c(bag2_sub_peak1$Index, rev(bag2_sub_peak1$Index)), y = c(bag2_sub_peak1$bag2_s1_norm, rep(0,length(bag2_sub_peak1$bag2_s1_norm))), col="grey")
polygon(x = c(bag2_sub_peak2$Index, rev(bag2_sub_peak2$Index)), y = c(bag2_sub_peak2$bag2_s1_norm, rep(0,length(bag2_sub_peak2$bag2_s1_norm))), col="grey")
polygon(x = c(bag2_sub_peak3$Index, rev(bag2_sub_peak3$Index)), y = c(bag2_sub_peak3$bag2_s1_norm, rep(0,length(bag2_sub_peak3$bag2_s1_norm))), col="grey")
polygon(x = c(bag2_sub_peak4$Index, rev(bag2_sub_peak4$Index)), y = c(bag2_sub_peak4$bag2_s1_norm, rep(0,length(bag2_sub_peak4$bag2_s1_norm))), col="grey")

#
#areas under the Curve for each of the selected peaks
sub_peak1_area <- AUC(bag2_sub_peak1$Index, bag2_sub_peak1$bag2_s1_norm)
sub_peak2_area <- AUC(bag2_sub_peak2$Index, bag2_sub_peak2$bag2_s1_norm)
sub_peak3_area <- AUC(bag2_sub_peak3$Index, bag2_sub_peak3$bag2_s1_norm)
sub_peak4_area <- AUC(bag2_sub_peak4$Index, bag2_sub_peak4$bag2_s1_norm)

#-------------------------------------------------------------------------------
#SENSOR2-------------------
#set start points and end points for sensor2 (data smoothed from BAG2)
bag2_s2_start_1 <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[1]]
bag2_s2_end_1   <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[3.5]]

bag2_s2_start_2 <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[4]]
bag2_s2_end_2   <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[5]]

bag2_s2_start_3 <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[6]]
bag2_s2_end_3   <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[7]]

bag2_s2_start_4 <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[8]]
bag2_s2_end_4   <- bag2_s2_norm$Index[bag2_s2_norm_cpt@cpts[10]]

bag2_sub_peak1 <- bag2_s2_norm[which(bag2_s2_norm$Index >= (bag2_s2_start_1 - dumb_smooth) & bag2_s2_norm$Index <= (bag2_s2_end_1 + dumb_smooth)),]
bag2_sub_peak2 <- bag2_s2_norm[which(bag2_s2_norm$Index >= (bag2_s2_start_2 - dumb_smooth) & bag2_s2_norm$Index <= (bag2_s2_end_2 + dumb_smooth)),]
bag2_sub_peak3 <- bag2_s2_norm[which(bag2_s2_norm$Index >= (bag2_s2_start_3 - dumb_smooth) & bag2_s2_norm$Index <= (bag2_s2_end_3 + dumb_smooth)),]
bag2_sub_peak4 <- bag2_s2_norm[which(bag2_s2_norm$Index >= (bag2_s2_start_4 - dumb_smooth) & bag2_s2_norm$Index <= (bag2_s2_end_4 + dumb_smooth)),]

polygon(x = c(bag2_sub_peak1$Index, rev(bag2_sub_peak1$Index)), y = c(bag2_sub_peak1$bag2_s2_norm, rep(0,length(bag2_sub_peak1$bag2_s2_norm))), col="tomato")
polygon(x = c(bag2_sub_peak2$Index, rev(bag2_sub_peak2$Index)), y = c(bag2_sub_peak2$bag2_s2_norm, rep(0,length(bag2_sub_peak2$bag2_s2_norm))), col="tomato")
polygon(x = c(bag2_sub_peak3$Index, rev(bag2_sub_peak3$Index)), y = c(bag2_sub_peak3$bag2_s2_norm, rep(0,length(bag2_sub_peak3$bag2_s2_norm))), col="tomato")
polygon(x = c(bag2_sub_peak4$Index, rev(bag2_sub_peak4$Index)), y = c(bag2_sub_peak4$bag2_s2_norm, rep(0,length(bag2_sub_peak4$bag2_s2_norm))), col="tomato")

#
#areas under the Curve for each of the selected peaks
sub2_peak1_area <- AUC(bag2_sub_peak1$Index, bag2_sub_peak1$bag2_s2_norm)
sub2_peak2_area <- AUC(bag2_sub_peak2$Index, bag2_sub_peak2$bag2_s2_norm)
sub2_peak3_area <- AUC(bag2_sub_peak3$Index, bag2_sub_peak3$bag2_s2_norm)
sub2_peak4_area <- AUC(bag2_sub_peak4$Index, bag2_sub_peak4$bag2_s2_norm)

#-------------------------------------------------------------------------------
#SENSOR4-------------------
#set start points and end points for sensor4 (data smoothed from BAG2)
bag2_s4_start_1 <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[2]]
bag2_s4_end_1   <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[3]]

bag2_s4_start_2 <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[4]]
bag2_s4_end_2   <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[5]]

bag2_s4_start_3 <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[6]]
bag2_s4_end_3   <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[7]]

bag2_s4_start_4 <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[8]]
bag2_s4_end_4   <- bag2_s4_norm$Index[bag2_s4_norm_cpt@cpts[9]]

bag2_sub_peak1 <- bag2_s4_norm[which(bag2_s4_norm$Index >= (bag2_s4_start_1 - dumb_smooth) & bag2_s4_norm$Index <= (bag2_s4_end_1 + dumb_smooth)),]
bag2_sub_peak2 <- bag2_s4_norm[which(bag2_s4_norm$Index >= (bag2_s4_start_2 - dumb_smooth) & bag2_s4_norm$Index <= (bag2_s4_end_2 + dumb_smooth)),]
bag2_sub_peak3 <- bag2_s4_norm[which(bag2_s4_norm$Index >= (bag2_s4_start_3 - dumb_smooth) & bag2_s4_norm$Index <= (bag2_s4_end_3 + dumb_smooth)),]
bag2_sub_peak4 <- bag2_s4_norm[which(bag2_s4_norm$Index >= (bag2_s4_start_4 - dumb_smooth) & bag2_s4_norm$Index <= (bag2_s4_end_4 + dumb_smooth)),]

polygon(x = c(bag2_sub_peak1$Index, rev(bag2_sub_peak1$Index)), y = c(bag2_sub_peak1$bag2_s4_norm, rep(0,length(bag2_sub_peak1$bag2_s4_norm))), col="deepskyblue")
polygon(x = c(bag2_sub_peak2$Index, rev(bag2_sub_peak2$Index)), y = c(bag2_sub_peak2$bag2_s4_norm, rep(0,length(bag2_sub_peak2$bag2_s4_norm))), col="deepskyblue")
polygon(x = c(bag2_sub_peak3$Index, rev(bag2_sub_peak3$Index)), y = c(bag2_sub_peak3$bag2_s4_norm, rep(0,length(bag2_sub_peak3$bag2_s4_norm))), col="deepskyblue")
polygon(x = c(bag2_sub_peak4$Index, rev(bag2_sub_peak4$Index)), y = c(bag2_sub_peak4$bag2_s4_norm, rep(0,length(bag2_sub_peak4$bag2_s4_norm))), col="deepskyblue")

#
#areas under the Curve for each of the selected peaks
sub4_peak1_area <- AUC(bag2_sub_peak1$Index, bag2_sub_peak1$bag2_s4_norm)
sub4_peak2_area <- AUC(bag2_sub_peak2$Index, bag2_sub_peak2$bag2_s4_norm)
sub4_peak3_area <- AUC(bag2_sub_peak3$Index, bag2_sub_peak3$bag2_s4_norm)
sub4_peak4_area <- AUC(bag2_sub_peak4$Index, bag2_sub_peak4$bag2_s4_norm)

#CREATE A UNIQUE DF FOR RESULTS (3x4)
peak1 <- c(sub_peak1_area,sub2_peak1_area,sub4_peak1_area)
peak2 <- c(sub_peak2_area,sub2_peak2_area,sub4_peak2_area)
peak3 <- c(sub_peak3_area,sub2_peak3_area,sub4_peak3_area)
peak4 <- c(sub_peak4_area,sub2_peak4_area,sub4_peak4_area)

#Create a unique df for peaks' areas
Cond_NormSmooth_peakArea <- cbind(peak1, peak2, peak3, peak4)
Cond_NormSmooth_peakArea <- as.data.frame(Cond_NormSmooth_peakArea)

conductivity <- c("sens1:start", "sens2:HPLC", "sens4:FC_dust")
Cond_NormSmooth_peakArea[["conductivity"]] <- conductivity

#compute mean, std.dev, rsd% and add col to df
Means <- rowMeans(Cond_NormSmooth_peakArea[1:3, 1:4])
Cond_NormSmooth_peakArea[["Mean"]] <- Means
Cond_NormSmooth_peakArea <- Cond_NormSmooth_peakArea %>% mutate(stDev = apply(.[(1:4)],1,sd))
Cond_NormSmooth_peakArea <- transform(Cond_NormSmooth_peakArea, RSD = (stDev / Means)*100)

#create a unique df for peaks values (normalized and smoothed)
Cond_NormSmooth_peaks <- cbind(bag2_s1_norm$Index ,bag2_s1_norm$bag2_s1_norm,bag2_s2_norm$bag2_s2_norm,bag2_s4_norm$bag2_s4_norm)
Cond_NormSmooth_peaks <- as.data.frame(Cond_NormSmooth_peaks)
colname_cond <- c("Index","s1","s2","s4")
Cond_NormSmooth_peaks <- setNames(Cond_NormSmooth_peaks, colname_cond)

rm(bag2_s1_end_1,bag2_s1_end_2,bag2_s1_end_3,bag2_s1_end_4,
   bag2_s2_end_1,bag2_s2_end_2,bag2_s2_end_3,bag2_s2_end_4,
   bag2_s4_end_1,bag2_s4_end_2,bag2_s4_end_3,bag2_s4_end_4,
   bag2_s1_start_1,bag2_s1_start_2,bag2_s1_start_3,bag2_s1_start_4,
   bag2_s2_start_1,bag2_s2_start_2,bag2_s2_start_3,bag2_s2_start_4,
   bag2_s4_start_1,bag2_s4_start_2,bag2_s4_start_3,bag2_s4_start_4,
   colname_cond, conductivity, dumb_smooth, Means, peak1, peak2,peak3,peak4,
   bag2_s1_norm, bag2_s2_norm,bag2_s4_norm)

#FIND PEAKS HEIGHTS, applying a local filter to data (s1,s2,s4) 
#Cond_NormSmooth_peaks: smoothed and normalized data

# Parameters:
# v:     a numeric vector where to search peaks.
# delta: numeric of length one; defining the local threshold for peak detection.
# x:     a numeric vector the same length as v containing corresponding x-values
#        for v.
# Value:
# A list containing two data frames maxtab and mintab containing maxima and
# minima. Data frames contain two columns with indices in v (or corresponding
# values in x if provided) and values in v.
peakdet <- function(v, delta, x = NULL)
{
  maxtab <- NULL
  mintab <- NULL
  
  if (is.null(x))
  {
    x <- seq_along(v)
  }
  if (length(v) != length(x))
  {
    stop("Input vectors v and x must have the same length")
  }
  if (!is.numeric(delta))
  {
    stop("Input argument delta must be numeric")
  }
  if (delta <= 0)
  {
    stop("Input argument delta must be positive")
  }
  mn <- Inf
  mx <- -Inf
  mnpos <- NA
  mxpos <- NA
  lookformax <- TRUE
  for(i in seq_along(v))
  {
    this <- v[i]
    if (this > mx)
    {
      mx <- this
      mxpos <- x[i]
    }
    if (this < mn)
    {
      mn <- this
      mnpos <- x[i]
    }
    if (lookformax)
    {
      if (this < mx - delta)
      {
        maxtab <- rbind(maxtab, data.frame(pos = mxpos, val = mx))
        mn <- this
        mnpos <- x[i]
        
        lookformax <- FALSE
      }
    }
    else
    {
      if (this > mn + delta)
      {
        mintab <- rbind(mintab, data.frame(pos = mnpos, val = mn))
        mx <- this
        mxpos <- x[i]
        
        lookformax <- TRUE
      }
    }
  }
  list(maxtab = maxtab, mintab = mintab)
}

s1 <- Cond_NormSmooth_peaks$s1
vals <- sin(s1)
H1 <- peakdet(vals, .15, Cond_NormSmooth_peaks$Index)

s2 <- Cond_NormSmooth_peaks$s2
vals <- sin(s2)
H2 <-peakdet(vals, .1, Cond_NormSmooth_peaks$Index)

s4 <- Cond_NormSmooth_peaks$s4
vals <- sin(s4)
H4 <-peakdet(vals, .1, Cond_NormSmooth_peaks$Index)

peak_heights <- cbind(H1$maxtab,H2$maxtab,H4$maxtab)
namesheights <- c("START:Time(sec)","s1 peaks","HPLC:Time(sec)","s2 peaks","FC:Time(sec)","s4 peaks")
peak_heights <- setNames(peak_heights,namesheights)
peak_heights$DELAY_hplc <- (peak_heights$`HPLC:Time(sec)` - peak_heights$`START:Time(sec)`)
peak_heights$DELAY_fc <- (peak_heights$`FC:Time(sec)` - peak_heights$`START:Time(sec)`)
delay_hplc <- peak_heights$DELAY_hplc 
delay_fc <- peak_heights$DELAY_fc
#statistics
delay_hplc_mean <- mean(delay_hplc)
delay_fc_mean <- mean(delay_fc)
delay_hplc_stdev <- sd(delay_hplc)
delay_fc_stdev <- sd(delay_fc)
delay_hplc_rsd <- (delay_hplc_stdev/delay_hplc_mean)*100
delay_fc_rsd <- (delay_fc_stdev/delay_fc_mean)*100

DELAYS_HPLC <- c(delay_hplc_mean,delay_hplc_stdev,delay_hplc_rsd)
DELAYS_FC <- c(delay_fc_mean,delay_fc_stdev,delay_fc_rsd)
DELAYS <- as.data.frame(cbind(DELAYS_HPLC,DELAYS_FC))
stats <-c("mean","stdev","rsd")
DELAYS[["stats"]] <- stats
COLNAME <- c("DELAYS:start-hplc (sec)", "DELAYS:start-fc (sec)","stats")
DELAYS <- setNames(DELAYS, COLNAME)


rm(s1,s2,s4,vals, H1,H2,H4, delay_hplc, delay_fc, namesheights,DELAYS_HPLC,DELAYS_FC,
   stats, COLNAME,delay_fc_mean,delay_fc_rsd,delay_fc_stdev,delay_hplc_mean,
   delay_hplc_rsd,delay_hplc_stdev)

rm(sub_peak1_area,sub_peak2_area,sub_peak3_area,sub_peak4_area,
   sub2_peak1_area,sub2_peak2_area,sub2_peak3_area,sub2_peak4_area,
   sub4_peak1_area,sub4_peak2_area,sub4_peak3_area,sub4_peak4_area,
   BAG2_cond1_smooth,BAG2_cond2_smooth,BAG2_cond4_smooth,bag2_s1_norm_cpt,
   bag2_s2_norm_cpt,bag2_s4_norm_cpt,bag2_sub_peak1,bag2_sub_peak2,bag2_sub_peak3,
   bag2_sub_peak4)
#-------------------------------------------------------------------------------
#DRAW WIRE DATA: ~1 acq. per 50 msec
#-------------------------------------------------------------------------------
#extract the dfs of interest (sample) from estens_sample list to manage them singularly
DW_df_130504 <- as.data.frame(estens_sample[['2021-04-14_130504']])
DW_df_134809 <- as.data.frame(estens_sample[['2021-04-14_134809']])
DW_df_142819 <- as.data.frame(estens_sample[['2021-04-14_142819']])

#extract last 8 string of column (time)
DW_df_130504$time <- str_sub(DW_df_130504$Time_.H..M..S,-8) 
DW_df_134809$time <- str_sub(DW_df_134809$Time_.H..M..S,-8) 
DW_df_142819$time <- str_sub(DW_df_142819$Time_.H..M..S,-8) 

#transform IceHeights and meltspeed from chr to numeric, then
#compute the average ice heights and meltspeeds per seconds with aggregate function:
DW_df_130504$Ice.Height..mm. <- as.numeric(gsub(",", ".", DW_df_130504$Ice.Height..mm.))
DW_df_130504$Melt.speed..mm.sec. <- as.numeric(gsub(",", ".", DW_df_130504$Melt.speed..mm.sec.))
#
DW_df_134809$Ice.Height..mm. <- as.numeric(gsub(",", ".", DW_df_134809$Ice.Height..mm.)) 
DW_df_134809$Melt.speed..mm.sec. <- as.numeric(gsub(",", ".", DW_df_134809$Melt.speed..mm.sec.)) 
#
DW_df_142819$Ice.Height..mm. <- as.numeric(gsub(",", ".", DW_df_142819$Ice.Height..mm.))
DW_df_142819$Melt.speed..mm.sec. <- as.numeric(gsub(",", ".", DW_df_142819$Melt.speed..mm.sec.))

#COMPUTE THE AVERAGE VALUES (Ice Heights and Melting speed) PER SECONDS 
#IceHeights per sec
file130504_MeanIceHEIGHT <- aggregate(DW_df_130504$Ice.Height..mm. ~ DW_df_130504$time, DW_df_130504, mean)
file134809_MeanIceHEIGHT <- aggregate(DW_df_134809$Ice.Height..mm. ~ DW_df_134809$time, DW_df_134809, mean)
file142819_MeanIceHEIGHT <- aggregate(DW_df_142819$Ice.Height..mm. ~ DW_df_142819$time, DW_df_142819, mean)
#Meltspeeds per sec
file130504_MeanMeltSPEED <- aggregate(DW_df_130504$Melt.speed..mm.sec. ~ DW_df_130504$time, DW_df_130504, mean)
file134809_MeanMeltSPEED <- aggregate(DW_df_134809$Melt.speed..mm.sec. ~ DW_df_134809$time, DW_df_134809, mean)
file142819_MeanMeltSPEED <- aggregate(DW_df_142819$Melt.speed..mm.sec. ~ DW_df_142819$time, DW_df_142819, mean)

#ADD time index (for next plots) to each df
Time_130504 <- c(1:2552)
Time_134809 <- c(1:2363)
Time_142819 <- c(1:1201)
#
file130504_MeanIceHEIGHT$index <- Time_130504
file134809_MeanIceHEIGHT$index <- Time_134809
file142819_MeanIceHEIGHT$index <- Time_142819
#
file130504_MeanMeltSPEED$index <- Time_130504
file134809_MeanMeltSPEED$index <- Time_134809
file142819_MeanMeltSPEED$index <- Time_142819

#rename cols
colname1 <- c("time","mean Ice Height (mm)","index")
file130504_MeanIceHEIGHT <-setNames(file130504_MeanIceHEIGHT, colname1)
file134809_MeanIceHEIGHT <-setNames(file134809_MeanIceHEIGHT, colname1)
file142819_MeanIceHEIGHT <-setNames(file142819_MeanIceHEIGHT, colname1)
#
colname2 <- c("time","mean melt speed (mm/sec)","index")
file130504_MeanMeltSPEED <-setNames(file130504_MeanMeltSPEED, colname2)
file134809_MeanMeltSPEED <-setNames(file134809_MeanMeltSPEED, colname2)
file142819_MeanMeltSPEED <-setNames(file142819_MeanMeltSPEED, colname2)

#subset the MeanIceHEIGHT df, keep only specific timeframes 
#134809: keep data from the begin (13:48:16) to 14:02:59 (row 884)
MIH_134809_first <- file134809_MeanIceHEIGHT[-c(885:2363), ]
#134809: keep data from 14:03:00 (row 885) to the end of df (14:27:38)
MIH_134809_second <- file134809_MeanIceHEIGHT[-c(1:884), ]
#
#subset the MeltSPEED df, keep only specific timeframes 
#134809: keep data from the begin (13:48:16) to 14:02:59 (row 884)
MS_134809_first <- file134809_MeanMeltSPEED[-c(885:2363), ]
#134809: keep data from 14:03:00 (row ) to the end of df (14:27:38)
MS_134809_second <- file134809_MeanMeltSPEED[-c(1:884), ]

#MIH_BAG1: 130504 + 134809 (first) df subset
MIH_BAG1 <- list(file130504_MeanIceHEIGHT,MIH_134809_first)
BAG1_IceHeight <- do.call("rbind", MIH_BAG1)
rm(MIH_BAG1)
#remove data from the beginning until the 13:20:27 (index 919)
BAG1_IceHeight <- BAG1_IceHeight[-c(1:919), ]
#adjust index and add it to the df 
v <- c(0:2516)
BAG1_IceHeight$index <- v 
#remove data tail from 14:02:34 (index 2491)
BAG1_IceHeight <- BAG1_IceHeight[-c(2491:2517), ]

#MS_BAG1: 130504 + 134809 (first) df subset
MS_BAG1 <- list(file130504_MeanMeltSPEED,MS_134809_first)
BAG1_MeltSpeed <- do.call("rbind", MS_BAG1)
rm(MS_BAG1)
#remove data from the beginning until the 13:20:27 (index 919)
BAG1_MeltSpeed <- BAG1_MeltSpeed[-c(1:919), ]
#adjust index and add it to the df 
v <- c(0:2516)
BAG1_MeltSpeed$index <- v 
#remove data tail from 14:02:34 (index 2491)
BAG1_MeltSpeed <- BAG1_MeltSpeed[-c(2491:2517), ]
#add new column for conversion mm/sec to cm/min
MeltSpeed_cm.min<- (BAG1_MeltSpeed$`mean melt speed (mm/sec)`)*6.089
BAG1_MeltSpeed <- cbind(BAG1_MeltSpeed,MeltSpeed_cm.min)
#
#MIH_BAG2: 134809 (second) + 142819 (all!) df subset
MIH_BAG2 <- list(MIH_134809_second, file142819_MeanIceHEIGHT)
BAG2_IceHeight <- do.call("rbind", MIH_BAG2)
rm(MIH_BAG2)
#adjust index and add it to the df 
v <- c(0:2679)
BAG2_IceHeight$index <- v 
#remove data tail from 14:44:24 (index 2440)
BAG2_IceHeight <- BAG2_IceHeight[-c(2440:2680), ]
#
#MS_BAG2: 134809 (second) + 142819 (all!) df subset
MS_BAG2 <- list(MS_134809_second, file142819_MeanMeltSPEED)
BAG2_MeltSpeed <- do.call("rbind", MS_BAG2)
rm(MS_BAG2)
#adjust index and add it to the df 
v <- c(0:2679)
BAG2_MeltSpeed$index <- v 
#remove data tail from 14:44:24 (index 2440)
BAG2_MeltSpeed <- BAG2_MeltSpeed[-c(2440:2680), ]
#add new column for conversion mm/sec to cm/min
MeltSpeed_cm.min<- (BAG2_MeltSpeed$`mean melt speed (mm/sec)`)*6.089
BAG2_MeltSpeed <- cbind(BAG2_MeltSpeed,MeltSpeed_cm.min)

rm(DW_df_130504,DW_df_134809,DW_df_142819,colname1,colname2, Time_130504,Time_134809,
   Time_142819, file130504_MeanIceHEIGHT,file130504_MeanMeltSPEED,file134809_MeanIceHEIGHT,
   file134809_MeanMeltSPEED,file142819_MeanIceHEIGHT,file142819_MeanMeltSPEED,
   MS_134809_first,MS_134809_second, MIH_134809_first, MIH_134809_second, estens_sample)

#REMOVE SPIKES with filter 
#first, identify the outliers and save them in a vector
outliers_bag1 <- boxplot(BAG1_MeltSpeed$MeltSpeed_cm.min, plot=FALSE)$out
outliers_bag2 <- boxplot(BAG2_MeltSpeed$MeltSpeed_cm.min, plot=FALSE)$out

#This vector is to be
#excluded from our dataset. The which() function tells us the rows in which the
#outliers exist, these rows are to be removed from our data set. However, before
#removing them, I store "warpbreaks" in a variable, suppose x, to ensure that I
#don't destroy the dataset.
x <- BAG1_MeltSpeed 
x <- x[-which(x$MeltSpeed_cm.min %in% outliers_bag1),]

y <- BAG2_MeltSpeed
y <- y[-which(y$MeltSpeed_cm.min %in% outliers_bag2),]

#PLOTS--------------------------------------------------------------------
#BAG1 plot
attach(mtcars)
par(mfrow=c(2,1))
plot(x$MeltSpeed_cm.min ~ x$index, type="l",
     main="BAG1:Melt Speed (cm/min)", 
     ylab="Melt Speed (cm/min)", xlab="Time (sec)")
plot(BAG1_IceHeight$`mean Ice Height (mm)`~BAG1_IceHeight$index, type="l",
     main="BAG1:Ice Height(mm)",
     ylab="Ice Height (mm)", xlab="Time (sec)")

#BAG2 plot
plot(y$MeltSpeed_cm.min ~ y$index, type="l",
     main="BAG2:Melt Speed (cm/min)",
     ylab="Melt Speed (cm/min)", xlab="Time (sec)")
plot(BAG2_IceHeight$`mean Ice Height (mm)`~BAG2_IceHeight$index, type="l",
     main="BAG2:Ice Height(mm)",
     ylab="Ice Height (mm)", xlab="Time (sec)")

#BAG1 STATS (cm/min)
x <- as.array(x$MeltSpeed_cm.min)
Mean_melt1 <- mean(x)
stDev_melt1 <- sd(x)
rsd_melt1 <- (stDev_melt1/Mean_melt1)*100
BAG1_MeltStat <- as.data.frame(cbind(Mean_melt1,stDev_melt1,rsd_melt1))
meltnames <- c("Mean melt speed (cm/min)","stDev","rsd %")
BAG1_MeltStat <- setNames(BAG1_MeltStat, meltnames)

#BAG2 STATS (cm/min)
y <- as.array(y$MeltSpeed_cm.min)
Mean_melt2 <- mean(y)
stDev_melt2 <- sd(y)
rsd_melt2 <- (stDev_melt2/Mean_melt2)*100
BAG2_MeltStat <- as.data.frame(cbind(Mean_melt2,stDev_melt2,rsd_melt2))
BAG2_MeltStat <- setNames(BAG2_MeltStat, meltnames)

rm(Mean_melt1,Mean_melt2,meltnames,MeltSpeed_cm.min,outliers_bag1,
   outliers_bag2,rsd_melt1,rsd_melt2,stDev_melt1,stDev_melt2,v,x,y)

#-------------------------------------------------------------------------------
#FLOWMETER DATA: ~1 acq. per 50 msec
#-------------------------------------------------------------------------------
#extract the dfs of interest (sample) from flow_sample list to manage them singularly
flow_df_130504 <- as.data.frame(flow_sample[['2021-04-14_130504']])
flow_df_134809 <- as.data.frame(flow_sample[['2021-04-14_134809']])
flow_df_142819 <- as.data.frame(flow_sample[['2021-04-14_142819']])

#extract last 8 string of column (time)
flow_df_130504$time <- str_sub(flow_df_130504$Time_.H..M..S,-8) 
flow_df_134809$time <- str_sub(flow_df_134809$Time_.H..M..S,-8) 
flow_df_142819$time <- str_sub(flow_df_142819$Time_.H..M..S,-8) 

#transform Flows from chr to numeric, then
#compute the average values per seconds with aggregate function:
flow_df_130504$Flow..ul.min. <- as.numeric(gsub(",", ".", flow_df_130504$Flow..ul.min.))
flow_df_134809$Flow..ul.min. <- as.numeric(gsub(",", ".", flow_df_134809$Flow..ul.min.)) 
flow_df_142819$Flow..ul.min. <- as.numeric(gsub(",", ".", flow_df_142819$Flow..ul.min.))

#COMPUTE THE AVERAGE VALUES PER SECONDS 
#IceHeights per sec
file130504_flow <- aggregate(flow_df_130504$Flow..ul.min. ~ flow_df_130504$time, flow_df_130504, mean)
file134809_flow <- aggregate(flow_df_134809$Flow..ul.min. ~ flow_df_134809$time, flow_df_134809, mean)
file142819_flow <- aggregate(flow_df_142819$Flow..ul.min. ~ flow_df_142819$time, flow_df_142819, mean)

#ADD time index (for next plots) to each df
Time_130504 <- c(1:2551)
Time_134809 <- c(1:2362)
Time_142819 <- c(1:1201)
#
file130504_flow$index <- Time_130504
file134809_flow$index <- Time_134809
file142819_flow$index <- Time_142819

#rename cols
colname1 <- c("time","flow (ul/min)","index")
file130504_flow <-setNames(file130504_flow, colname1)
file134809_flow <-setNames(file134809_flow, colname1)
file142819_flow <-setNames(file142819_flow, colname1)

rm(flow_df_130504,flow_df_134809,flow_df_142819,flow_sample)

#keep only specific timeframes 
#134809: keep data from the begin (13:48:16) to 14:02:59 (row 884)
flow_134809_first <- file134809_flow[-c(885:2363), ]
#134809: keep data from 14:03:00 (row 885) to the end of df (14:27:38)
flow_134809_second <- file134809_flow[-c(1:884), ]

#flow_BAG1: 130504 + 134809 (first) df subset
flow_BAG1 <- list(file130504_flow,flow_134809_first)
BAG1_flow <- do.call("rbind", flow_BAG1)
rm(flow_BAG1)
#remove data from the beginning until the 13:20:27 (index 919)
BAG1_flow <- BAG1_flow[-c(1:919), ]
#adjust index and add it to the df 
v <- c(0:2515)
BAG1_flow$index <- v 
#remove data tail from 14:02:34 (index 2491)
BAG1_flow <- BAG1_flow[-c(2491:2517), ]
#
#flow_BAG2: 134809 (second) + 142819 (all!) df subset
flow_BAG2 <- list(flow_134809_second, file142819_flow)
BAG2_flow <- do.call("rbind", flow_BAG2)
rm(flow_BAG2)
#adjust index and add it to the df 
v <- c(0:2678)
BAG2_flow$index <- v 
#remove data tail from 14:44:24 (index 2440)
BAG2_flow <- BAG2_flow[-c(2440:2680), ]

#DATA SMOOTHING:SETTINGS---------------------------------------------------
tmp.span <- 0.1
flow1 <- unlist(BAG1_flow[,2])
flow2 <- unlist(BAG2_flow[,2])

x1 <- BAG1_flow$index
x2 <- BAG2_flow$index

#DATA SMOOTHING-----------------------------------------------------------------
BAG1_flow1 <- loess(flow1 ~ x1, span=tmp.span)$fitted
BAG2_flow2 <- loess(flow2 ~ x2, span=tmp.span)$fitted
#-------------------------------------------------------------------------------
#PLOTS
par(mfrow=c(1,1))
#BAG1
plot(BAG1_flow$`flow (ul/min)` ~ BAG1_flow$index , ylim = c(-2500,3000), 
     type="l", main="BAG1: flow rate (ul/min)", xlab="Time(sec)", ylab="Flow rate (ul/min)")
par(new=TRUE) 
plot(x1, BAG1_flow1, type = 'l', lty=1, ylim = c(-2500,3000), col="red", lwd=3,
     axes = FALSE, xlab = "", ylab = "")
#BAG2
plot(BAG2_flow$`flow (ul/min)` ~ BAG2_flow$index , ylim = c(-2500,3000), 
     type="l", main="BAG2: flow rate (ul/min)", xlab="Time(sec)", ylab="Flow rate (ul/min)")
par(new=TRUE) 
plot(x2, BAG2_flow2, type = 'l', lty=1, ylim = c(-2500,3000), col="red", lwd=3,
     axes = FALSE, xlab = "", ylab = "")

rm(file130504_flow,file134809_flow,file142819_flow,flow_134809_first,flow_134809_second)

#BAG1 STATS (ul/min)
flow_bag1 <- as.array(BAG1_flow$`flow (ul/min)`)
flow_bag1 <- (flow_bag1)/1000 #ml/min
Mean_flow1 <- mean(flow_bag1)
stDev_flow1 <- sd(flow_bag1)
rsd_flow1 <- (stDev_flow1/Mean_flow1)*100
BAG1_flowstat <- as.data.frame(cbind(Mean_flow1,stDev_flow1,rsd_flow1))
flownames <- c("Mean flow (ml/min)","stDev","rsd %")
BAG1_flowstat <- setNames(BAG1_flowstat, flownames)

#BAG2 STATS (ul/min)
flow_bag2 <- as.array(BAG2_flow$`flow (ul/min)`)
flow_bag2 <- (flow_bag2)/1000 #ml/min
Mean_flow2 <- mean(flow_bag2)
stDev_flow2 <- sd(flow_bag2)
rsd_flow2 <- (stDev_flow2/Mean_flow2)*100
BAG2_flowstat <- as.data.frame(cbind(Mean_flow2,stDev_flow2,rsd_flow2))
BAG2_flowstat <- setNames(BAG2_flowstat, flownames)

rm(BAG1_flow1,BAG2_flow2,colname1,flow1,flow2,flownames, 
   Mean_flow1,Mean_flow2,rsd_flow1,rsd_flow2,stDev_flow1,stDev_flow2,Time_130504,
   Time_134809,Time_142819,tmp.span,v,x1,x2, flow_bag1,flow_bag2)

#group data 
#BAG1----------------------------------------------------------------------------
BAG1 <- list(BAG1_abakus,BAG1_cond,BAG1_flow,BAG1_flowstat,BAG1_IceHeight,BAG1_MeltSpeed,
             BAG1_MeltStat)
renamebag1 <- c("dust","conductivity","flow","flow statistics","Ice Height","Melt speed",
                "Melt statitics")
BAG1 <- setNames(BAG1, renamebag1)
#BAG2---------------------------------------------------------------------------
BAG2 <- list(BAG2_abakus,BAG2_cond,BAG2_flow,BAG2_flowstat,BAG2_IceHeight,BAG2_MeltSpeed,
             BAG2_MeltStat,Cond_NormSmooth_peakArea,Cond_NormSmooth_peaks,DELAYS)
renamebag2 <- c("dust","conductivity","flow","flow statistics","Ice Height","Melt speed",
                "Melt statitics","Peaks:Area","Peaks","Conductivity Sensors:delays")
BAG2 <- setNames(BAG2, renamebag2)


samples <- list(BAG1,BAG2)
samplename <- c("BAG1","BAG2")
samples <- setNames(samples,samplename)
rm(samplename, BAG1,BAG2)

SAMPLE <- list(samples)
SAMPLEN <- c("samples")
SAMPLE <- setNames(SAMPLE,SAMPLEN)
rm(SAMPLEN)

rm(log_datalist,file,renamebag1,renamebag2,samples)
