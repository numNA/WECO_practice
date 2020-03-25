## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.


##############################################################
#  Error in weco.rule8(x, sdx = sdx, mux = mux, ...) :       #
#  NA/NaN/Inf in foreign function call (arg 4)               #   
#  에러 발생시 길이가 0인 데이터 프레임이 들어오는 것이므로  #
#  원본 csv 파일과 변수 개수 등을 확인하여 빈칸을 읽어오고   #
#  있진 않은지 확인해야 함                                   #
##############################################################

# #################################################################################################################
# WECO Rules
# 
# This package considers the following eight WECO rules:
#   
#   Rule 1
# 1 data point is greater than 3 standard deviation from the center line. 
# (This rule is to identify single data point that is out of the acceptable range.)
# 
# Rule 2
# 9 data points in a row on the same side of the center line. 
# (The ideal stable process is assumed to be up and down around the center line. 
#   A large block of data points on the same side of the center line indicates a process mean is shifted.)
# 
# Rule 3
# 6 data points in a row, all increasing or decreasing. 
# (This rule is also an indicator of possible mean shift.)
# 
# Rule 4
# 16 data points in a row, alternating up and down. 
# (When data points are routinely alternating up and down, 
#   it shows a high negative correlation between neighboring observations, 
#   which is abnormal for a stable process. For an in-control-process, 
#   it is not expected to observe correlation between neighboring data points.)
# 
# Rule 5
# 2 out of 3 data points on the same side are greater than 2 standard deviations from the center line. 
# (For a normally distributed in-control-process, about 95 deviation. The chance to violate this rule is 0.00306. 
#   This rule is used to detect increase in process variation.)
# 
# Rule 6
# 4 out of 5 data points on the same side are greater than 1 standard deviation from the center line. 
# (For a normally distributed in-control-process, about 62 deviation. This chance to violate this rule is 0.00553. 
#   This rule is also used to detect increase in process variation.)
# 
# Rule 7
# 15 data points in a row within 1 standard deviation of the center line. 
# (Too many data points are within 1 standard deviation indicates the decrease in process variation.)
# 
# Rule 8
# 8 data points in a row are greater than 1 standard deviation of the center line. 
# (This is another rule to detect increase in process variance.)

# weco.rule(rule, x, sdx = sd(x), mux = mean(x), ...)
# <Arguments>
# 
# rule	
# WECO rule number from 1 to 8
# x	
# A vector of continuously observed data from some process
# sdx	
# Standard deviation of the observed data
# mux	
# Mean of the observed data
# ...	
# Specifications for individual rule
# k
# Number of "abnormal" data points
# l
# Number of standard deviations
# n
# Number of data points prior to the current point (including the current point) to be evaluated
# #################################################################################################################


# Set Library -------------------------------------------------------------

library(weco)
library(stringr)

#rm(list=ls()) <- 개체 모두 지우기


# 현재 분석중인 측정값 이름을 추출하는 함수
Mnm <- function() {
  numOfMeas <- length(names(SensorData[-1:-2]))
  a=0
  for (i in 1:numOfMeas) {
    nm <-  names(SensorData)[i+2]
    nml <- str_locate_all(nm,"\\.")
    lop <- nml[[1]][,1][length(nml[[1]][,1])]+1 
    a = a+nchar(nm)-lop
  }
  b=(a/numOfMeas)-0.5
  return(substr(nm,nchar(nm)-b+1,nchar(nm)))
}

#0 제외하는 것이 T 일때 호출할 함수
Newsdx <- function(x) {
  for (k in 1:length(x)) {
    ifelse(x[k]==0,x[k] <- NA,x[k] <- x[k])
  }
  ifelse(is.na(sd(x,na.rm = TRUE)),return(0),return(sd(x,na.rm = TRUE)))
}

#파일이름 등에 사용할 Rule name#
rnm = paste('Rule','8')       ##
#rule , 시그마 변경           ##
sigma = 3                     ##
wrule = 8                     ##
################################

# WECO_A ------------------------------------------------------------------

WECO_A <- function(D=SensorData,rule=wrule){
  numOfMeas <- length(names(D[-1:-2]))
  for (i in 1:numOfMeas) {
    
    
    colAn <- D[,i+2][complete.cases(D[,i+2])]
    
    rst <-weco.rule(rule,colAn,l=sigma)  
    rstP <- weco.rl(rst)
    
    for (k in 1:length(rstP)) {
      ifelse(k==1,rstP[k] <- rstP[k]+1,rstP[k] <- rstP[k-1]+rstP[k]+1)
    }
    
    
    if(i==1) {
      
      if(length(rstP)==0){
        rstP <- NA
        rst_rule <- data.frame("a" <- rstP,"b" <- rstP,"c" <- rstP)
        
        names(rst_rule)[i] <- paste(rnm,names(D)[i+2],'순번')
        names(rst_rule)[i+1] <- paste(rnm,names(D)[i+2],'날짜')
        names(rst_rule)[i+2] <- paste(rnm,names(D)[i+2],'값')
      }
      
      else{
        rst_rule <- data.frame("a" <- rstP,"b" <- rstP,"c" <- rstP)
        rst_rule[,i] <- rstP
        rst_rule[,i+1] <- D[,2][rstP]
        rst_rule[,i+2] <- D[,i+2][rstP]
        
        
        names(rst_rule)[i] <- paste(rnm,names(D)[i+2],'순번')
        names(rst_rule)[i+1] <- paste(rnm,names(D)[i+2],'날짜')
        names(rst_rule)[i+2] <- paste(rnm,names(D)[i+2],'값')
      }
      
    }
    
    
    if(i>1){
      if(length(rst_rule[,3*(i-1)])<length(rstP)) {
        
        rst_rule[length(rstP),] <- NA
        
      }
      if(length(rst_rule[,3*(i-1)])>length(rstP)) {
        rstP[(length(rstP)+1):length(rst_rule[,3*(i-1)])] <- NA
      }
      
      
      
      
      rst_rule[,3*i-2] <- rstP
      ifelse(sum(is.na(rstP))==length(rstP),rst_rule[,3*i-1] <- rstP,rst_rule[,3*i-1] <- D[,2][rstP])
      ifelse(sum(is.na(rstP))==length(rstP),rst_rule[,3*i] <- rstP, rst_rule[,3*i] <- D[,i+2][rstP])
      
      names(rst_rule)[3*i-2] <- paste(rnm,names(D)[i+2],'순번')
      names(rst_rule)[3*i-1] <- paste(rnm,names(D)[i+2],'날짜')
      names(rst_rule)[3*i] <- paste(rnm,names(D)[i+2],'값')
    }
    
  }
  filenm <- paste0(substr(D[1,2],1,10),rnm, Mnm(),'Results.csv')
  write.csv(rst_rule,filenm)
  
}


# StEndList ---------------------------------------------------------------

StEnList <- function(otlr,d_otlr){
  
  counter <- 2
  rang_ot <- list('1' <- c(0,0))
  
  
  for(n in 1:length(d_otlr)){
    if(n==length(d_otlr)&&length(rang_ot)==1){
      rang_ot[[length(rang_ot)+1]] <- c(otlr[1],otlr[n])
    }
    
    
    else if(n==1){
      if (d_otlr[n]<180) next()
      else {
        rang_ot[[length(rang_ot)+1]] <- c(otlr[n],otlr[n])
        counter <- n+1
      }
    }
    
    
    else if(n==length(d_otlr)){
      rang_ot[[length(rang_ot)+1]] <- c(otlr[counter],otlr[n])
    }
    
    else if(sum(d_otlr[counter:n])>1000){
      rang_ot[[length(rang_ot)+1]] <- c(otlr[counter],otlr[n])
      counter <- n+1
    }
  }
  
  
  return(rang_ot)
}


# WECO_P --------------------------------------------------------------------

WECO_P <- function(D=SensorData){
  #file name
  f_nor <- paste0(substr(D[1,2],1,10),rnm, Mnm(),'Results.csv')
  forPlot <- read.csv(f_nor)
  
  
  RangeOT <- list('1' <- NULL)
  for (l in 1:((length(forPlot)-1)/3)) {
    otlr<- forPlot[,3*l-1][complete.cases(forPlot[,3*l-1])]
    
    
    if(isFALSE(l<((length(forPlot)-1)/3)||exists('d_otlr'))){
      Print("No Data")
      break
    }
    
    else if(length(otlr)==0) next() 
    
    else {
      d_otlr <- otlr[2:(length(otlr))]-otlr[1:(length(otlr)-1)]
      RangeOT[[l]] <- StEnList(otlr,d_otlr)
    }
  }  
  
  for (o in 1:length(RangeOT)) {
    if(!is.null(RangeOT[[o]])){
      for (p in 2:length(RangeOT[[o]])) {
        
        rst <- weco.rule(wrule,D[,o+2][complete.cases(D[,o+2])],l=sigma)  
        jpeg(paste0(substr(as.character(D[RangeOT[[o]][[p]][1],2]),1,10),'_',
                    rnm,'_',substr(names(D)[o+2],7,16),'_',Mnm(),'_(',p-1,')','.jpg'))
        
        #Plot
        if(p==2){
          if((RangeOT[[o]][[p]][1]-100)<1) plot(rst,start=1,end=RangeOT[[o]][[p]][2]+100)
          else plot(rst,start=RangeOT[[o]][[p]][1]-100,end=RangeOT[[o]][[p]][2]+100)
        }else plot(rst,start=RangeOT[[o]][[p]][1]-100,end=RangeOT[[o]][[p]][2]+100)
        
        #Title
        if(p==2){
          if(RangeOT[[o]][[p]][1]-100<0){
            
            IntT <- format(as.numeric(as.POSIXct(SensorData[RangeOT[[o]][[p]][2]+100,2])
                                      -as.POSIXct(SensorData[1,2]),units='mins'),nsmall=3)
            IntO <- ifelse(RangeOT[[o]][[p]][1]==RangeOT[[o]][[p]][2],format(8/60,nsmall = 3),format(as.numeric(as.POSIXct(SensorData[RangeOT[[o]][[p]][2],2])
                                                                                                                -as.POSIXct(SensorData[RangeOT[[o]][[p]][1]-9,2]),units='mins'),nsmall=3))
            
            
            mtext(
              paste0('Time interval: ', SensorData[1,2],'~', 
                     substr(as.character(SensorData[RangeOT[[o]][[p]][2]+100,2]),6,19),' (',IntT,' mins)',
                     '\n','Rule out interval: ', SensorData[RangeOT[[o]][[p]][1]-9,2],'~', 
                     substr(as.character(SensorData[RangeOT[[o]][[p]][2],2]),6,19),' (',IntO,' mins)'), side = 3,font=0.5)
          } else {
            
            IntT <- format(as.numeric(as.POSIXct(SensorData[RangeOT[[o]][[p]][2],2])
                                      -as.POSIXct(SensorData[RangeOT[[o]][[p]][1]-100,2]),units='mins'),nsmall=3)
            IntO <- ifelse(RangeOT[[o]][[p]][1]==RangeOT[[o]][[p]][2],format(8/60,nsmall = 3),format(as.numeric(as.POSIXct(SensorData[RangeOT[[o]][[p]][2],2])
                                                                                                                -as.POSIXct(SensorData[RangeOT[[o]][[p]][1]-9,2]),units='mins'),nsmall=3))
            
            mtext(
              paste0('Time interval: ', SensorData[RangeOT[[o]][[p]][1]-100,2],'~', 
                     substr(as.character(SensorData[RangeOT[[o]][[p]][2],2]),6,19),' (',IntT,' mins)',
                     '\n','Rule out interval: ', SensorData[RangeOT[[o]][[p]][1]-9,2],'~', 
                     substr(as.character(SensorData[RangeOT[[o]][[p]][2],2]),6,19),' (',IntO,' mins)'), side = 3,font=0.5)
          } 
          
        } else {
          
          IntT <- format(as.numeric(as.POSIXct(SensorData[RangeOT[[o]][[p]][2]+100,2])
                                    -as.POSIXct(SensorData[RangeOT[[o]][[p]][1]-100,2]),units='mins'),nsmall=3)
          
          IntO <- ifelse(RangeOT[[o]][[p]][1]==RangeOT[[o]][[p]][2],format(8/60,nsmall = 3),format(as.numeric(as.POSIXct(SensorData[RangeOT[[o]][[p]][2],2])
                                                                                                              -as.POSIXct(SensorData[RangeOT[[o]][[p]][1]-9,2]),units='mins'),nsmall=3))
          
          mtext(
            paste0('Time interval: ', SensorData[RangeOT[[o]][[p]][1]-100,2],'~', 
                   substr(as.character(SensorData[RangeOT[[o]][[p]][2]+100,2]),6,19),' (',IntT,' mins)',
                   '\n','Rule out interval: ', SensorData[RangeOT[[o]][[p]][1]-9,2],'~', 
                   substr(as.character(SensorData[RangeOT[[o]][[p]][2],2]),6,19),' (',IntO,' mins)'), side = 3,font=0.5)
        }
        dev.off()
      }
    } else next()
  }
}


# WECO_T ------------------------------------------------------------------

WECO_T <- function(D=SensorData){
  #file name
  f_nor <- paste0(substr(D[1,2],1,10),rnm, Mnm(),'Results.csv')
  forPlot <- read.csv(f_nor)
  
  RangeOT <- list('1' <- NULL)
  for (l in 1:((length(forPlot)-1)/3)) {
    otlr<- forPlot[,3*l-1][complete.cases(forPlot[,3*l-1])]
    
    
    if(isFALSE(l<((length(forPlot)-1)/3)||exists('d_otlr'))){
      Print("No Data")
      break
    }
    
    else if(length(otlr)==0) next() 
    
    else {
      d_otlr <- otlr[2:(length(otlr))]-otlr[1:(length(otlr)-1)]
      RangeOT[[l]] <- StEnList(otlr,d_otlr)
    }
  }  
  
  EventDate <- data.frame(
    Machine = NA,
    Start = NA,
    End = NA,
    Period = NA
  )
  
  
  for (o in 1:length(RangeOT)) {
    if(!is.null(RangeOT[[o]])){
      for (p in 2:length(RangeOT[[o]])) {
        
        
        #시작점
        StartTime<- as.POSIXlt(D[(RangeOT[[o]][[p]][1]),2])
        StartTime$zone <- NULL
        #끝점
        EndTime<- as.POSIXlt(D[(RangeOT[[o]][[p]][2]),2])
        EndTime$zone <- NULL
        
        
        EventDate <- rbind(EventDate,c(substr(names(D)[o+2],7,17),
                                       toString(StartTime),
                                       toString(EndTime),
                                       ifelse(EndTime==StartTime,
                                              format(8/60,nsmall = 3),
                                              format(as.numeric(EndTime-StartTime,units="mins"),nsmall = 3))))
        
      } 
    }else next()
  }
  fnm<- paste0(substr(as.character(EventDate[2,2]),1,10),'_',rnm,' Out_',Mnm(),'.csv')
  write.csv(EventDate,fnm)
  
}


