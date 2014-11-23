#-Find monitor name 
getmonitor_no<-function(id){
  if (id>=1 && id<10){ filename<-paste("00",id,".csv",sep="")
     } else if (id>=10 && id<100){filename<-paste("0",id,".csv",sep="")
     } else{ filename<-paste(id,".csv",sep="")
     }
}  

#--------------------------------------
#Calculate column mean of each monitor
colmean <-function(y, k, removeNA=TRUE){
    nc<-ncol(y)
    means<-numeric(nc)
    for(i in 2:nc){
       means[i]<- mean(y[,i],na.rm=removeNA) 
      }
   #means
   result<-list(s=means[2],n=means[3])
  }
#------------------------------------------------------------------
pollutantmean <- function(directory,pollutant,id,removeNA=TRUE) {
  # 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  defaultD<-paste("C:/Users/Hp/Documents/GitHub/Deepani/ProgrammingAssignment2/",directory,sep="")
  setwd(defaultD)
  monitor<-matrix(nrow=332,ncol=2)

  for (t in id){
      filename1<-getmonitor_no(t)
      y<-read.csv(filename1)
      z <- colmean(y,t)
      monitor[t,1]<-z$s
      monitor[t,2]<-z$n
   }
nc1<-ncol(monitor)
meansA<-numeric(nc1)

if (pollutant=="sulfate"){ meansA<- mean(monitor[,1],na.rm=removeNA) }
else if(pollutant=="nitrate"){ meansA<- mean(monitor[,2],na.rm=removeNA)}
else { print ("check spelling")}
meansA
}
 
pollutantmean("specdata","nitrate",70:72)
pollutantmean("specdata","sulfate",1:10)
pollutantmean("specdata","sulfate",23)

