# CAISO-oasisAPI-operations.R
#
# Contributors: Peter Alstone
#
# Version 0.1
#
# These R functions can be used to access CAISO operations data, an alternative to using the "Oasis" system
# get R: cran.r-project.org


# FUNCTIONS ------------------------------------------------

# fetch CAISO LMP data from OASIS (see "Interface Specification for OASIS API)
getCAISOlmp <- function(startdate=20110101, enddate=20110102, market_run_id="DAM",node="ALL",
                        onlyDo=NA,allAtOnce=TRUE){

# # DON"T use node by node , only use ALL nodes -- doesn't append data!
#  this function simply grabs the csv data from caiso and dumps it into a data frame with the same structure as the csv.
# onlyDo means "only do the first X nodes in the list"
# allAtOnce means to use the "all apnode" command to download instead of node by node crawling -- use anything else at your peril.
  
  # convert CAISO format to POSIXct dates
  start <- strptime(startdate,"%Y%m%d")
  end <- strptime(enddate,"%Y%m%d")
  # Initialize data frame with starting day
  activeDay <- start
  
  #define base URL
  baseURL <- "http://oasis.caiso.com/mrtu-oasis/SingleZip?"
  
  if(node!="ALL"){ #if there is only one node...just do that.
    
    while(activeDay < end){
        activeNode <- node  
        # assemble url for LMP
        getURL <- paste(baseURL,"resultformat=6&queryname=PRC_LMP&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),"&market_run_id=",market_run_id,"&node=",activeNode,sep="")
        temp <- tempfile() #create temp file
        download.file(getURL,temp) # get data into temp
        data <- read.csv(unzip(temp)) # unzip and read csv, dump into data
        unlink(temp)
        
      #end for
      activeDay <- activeDay + 86400 # add one day (in seconds)
      #end while
    }
    return(data)
    
    
  }else{ #...or get all of the nodes...
  
if(allAtOnce){ #get all nodes day by day.  
    
  # First day -- initialize data output frame.
    getURL <- paste(baseURL,"resultformat=6&queryname=PRC_LMP&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),"&market_run_id=",market_run_id,"&grp_type=ALL_APNODES",sep="")
    temp <- tempfile() #create temp file
    try(download.file(getURL,temp)) # get data into temp
    try(data <- read.csv(unzip(temp))) # unzip and read csv, dump into data
    unlink(temp)
    activeDay <- activeDay + 86400 #increment one day
               
    while(activeDay < end){
        # assemble url for LMP
        getURL <- paste(baseURL,"resultformat=6&queryname=PRC_LMP&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),"&market_run_id=",market_run_id,"&grp_type=ALL_APNODES",sep="")
        temp <- tempfile() #create temp file
        try(download.file(getURL,temp)) # get data into temp
        try(newdata <- read.csv(unzip(temp))) # unzip and read csv, dump into data
        unlink(temp)
        try(data <- rbind(data,newdata))#append new data to existing
        activeDay <- activeDay + 86400 # add one day (in seconds)
    }    
    
}else{    # go through pnodes one by one (very slow)
  #get list of all Pnodes (this may or may not be all...just take the start date)
  pnodeURL <- paste(baseURL,"resultformat=6&queryname=ATL_PNODE&Pnode_type=ALL&startdate=",strftime(start,"%Y%m%d"),sep="")
  temp <- tempfile() #create temp file
  download.file(pnodeURL,temp) # get data into temp
  pnode.desc <- read.csv(unzip(temp)) # unzip and read csv, dump into data
  unlink(temp)
  
  if(is.na(onlyDo)){numberNodes <- length(pnode.desc$PNODE_ID)}else{numberNodes <- onlyDo}
  
  
  activeNode <- pnode.desc$PNODE_ID[1]
  
  # assemble url for LMP and download.
  getURL <- paste(baseURL,"resultformat=6&queryname=PRC_LMP&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),"&market_run_id=",market_run_id,"&node=",activeNode,sep="")
  temp <- tempfile() #create temp file
  download.file(getURL,temp) # get data into temp
  data <- read.csv(unzip(temp)) # unzip and read csv, dump into data (THE MAIN OUTPUT)
  unlink(temp)
  
while(activeDay < end){
  
  for(i in 2:numberNodes){
  activeNode <- pnode.desc$PNODE_ID[i]  
  # assemble url for LMP
  getURL <- paste(baseURL,"resultformat=6&queryname=PRC_LMP&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),"&market_run_id=",market_run_id,"&node=",activeNode,sep="")
  temp <- tempfile() #create temp file
  try(download.file(getURL,temp)) # get data into temp
  try(newdata <- read.csv(unzip(temp))) # unzip and read csv, dump into data
  unlink(temp)
  try(data <- rbind(data,newdata))#append new data to existing DF
  }
  
  #end for
  activeDay <- activeDay + 86400 # add one day (in seconds)

} #end while
} #end else
  return(data)
} #end else
} #end FUNCTION ... getCAISOlmp

# fetch CAISO energy clearing data from OASIS (see "Interface Specification for OASIS API)
getCAISOsysenergy <- function(startdate=20110101, enddate=20110102, 
                              market_run_id=c("DAM","RTM","RUC","HASP")){
  
  # this function simply grabs the csv data from caiso and dumps it into a data frame with the same structure as the csv.
  # use a set of market run IDs to get all of them using c(oncatenate)...otherwise specify "DAM" "RTM" "RUC" or "HASP"
  
  # convert CAISO format to POSIXct dates
  start <- strptime(startdate,"%Y%m%d")
  end <- strptime(enddate,"%Y%m%d")
  # Initialize data frame with starting day
  activeDay <- start
  
  #define base URL
  baseURL <- "http://oasis.caiso.com/mrtu-oasis/SingleZip?"
      
      # First day -- initialize data output frame with first data.
      dummy <- 1
      for(i in market_run_id){
      getURL <- paste(baseURL,"resultformat=6&queryname=ENE_SLRS&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),"&market_run_id=",i,"&tac_zone_name=ALL&schedule=ALL",sep="")
      temp <- tempfile() #create temp file
      try(download.file(getURL,temp)) # get data into temp
      if(dummy==1){try(data <- read.csv(unzip(temp)))
      dummy=dummy+1
        }else{
        try(newdata <- read.csv(unzip(temp)))
        try(data <- rbind(data,newdata))#append new data to existing
      } # unzip and read csv, dump into data
      unlink(temp)
      } #end for loop for first day
      activeDay <- activeDay + 86400 # add one day (in seconds)

      # Subsequent days -- download and append
      while(activeDay < end){      
        for(i in market_run_id){ #loop through market types
        getURL <- paste(baseURL,"resultformat=6&queryname=ENE_SLRS&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),"&market_run_id=",i,"&tac_zone_name=ALL&schedule=ALL",sep="")
        temp <- tempfile() #create temp file
        try(download.file(getURL,temp)) # get data into temp
        try(newdata <- read.csv(unzip(temp))) # unzip and read csv, dump into data
        unlink(temp)
        try(data <- rbind(data,newdata))#append new data to existing
        }
        activeDay <- activeDay + 86400 # add one day (in seconds)
      } #end while loop for moving through time...    
    return(data) # these are the data you are looking for...
} #end FUNCTION ... getCAISOsysenergy

# fetch CAISO load data from OASIS (see "Interface Specification for OASIS API)
getCAISOload <- function(startdate=20110101, enddate=20110102){
  
  # this function simply grabs the csv data from caiso and dumps it into a data frame with the same structure as the csv.
  
  # convert CAISO format to POSIXct dates
  start <- strptime(startdate,"%Y%m%d")
  end <- strptime(enddate,"%Y%m%d")
  # Initialize data frame with starting day
  activeDay <- start
  
  #define base URL
  baseURL <- "http://oasis.caiso.com/mrtu-oasis/SingleZip?"
  
  # First day -- initialize data output frame with first data.
    getURL <- paste(baseURL,"resultformat=6&queryname=SLD_FCST&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),sep="")
    temp <- tempfile() #create temp file
    try(download.file(getURL,temp)) # get data into temp
    try(data <- read.csv(unzip(temp)))
    unlink(temp)
  activeDay <- activeDay + 86400 # add one day (in seconds)
  
  # Subsequent days -- download and append
  while(activeDay < end){      
      getURL <- paste(baseURL,"resultformat=6&queryname=SLD_FCST&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),sep="")
      temp <- tempfile() #create temp file
      try(download.file(getURL,temp)) # get data into temp
      try(newdata <- read.csv(unzip(temp))) # unzip and read csv, dump into data
      unlink(temp)
      try(data <- rbind(data,newdata))#append new data to existing
    activeDay <- activeDay + 86400 # add one day (in seconds)
  } #end while loop for moving through time...    
  return(data) # these are the data you are looking for...
} #end FUNCTION ... getCAISOload

# Fetch CAISO public bid data (be careful, these are big files)
getCAISObids <- function(startdate=20110101, enddate=20110102, market="DAM"){
  #market is DAM or RTM
  
  # this function simply grabs the csv data from caiso and dumps it into a data frame with the same structure as the csv.
  
  # convert CAISO format to POSIXct dates
  start <- strptime(startdate,"%Y%m%d")
  end <- strptime(enddate,"%Y%m%d")
  # Initialize data frame with starting day
  activeDay <- start
  
  #define base URL
  baseURL <- "http://oasis.caiso.com/mrtu-oasis/GroupZip?" #note this is a grouped file
  
  reportname<-paste("PUB_",market,"_GRP",sep="")
  
  # First day -- initialize data output frame with first data.
  getURL <- paste(baseURL,"resultformat=6&groupid=",reportname,"&startdate=",strftime(activeDay,"%Y%m%d"),sep="")
  temp <- tempfile() #create temp file
  try(download.file(getURL,temp)) # get data into temp
  try(data <- read.csv(unzip(temp)))
  unlink(temp)
  activeDay <- activeDay + 86400 # add one day (in seconds)
  
  # Subsequent days -- download and append
  while(activeDay < end){      
    getURL <- paste(baseURL,"resultformat=6&groupid=",reportname,"&startdate=",strftime(activeDay,"%Y%m%d"),sep="")
    temp <- tempfile() #create temp file
    try(download.file(getURL,temp)) # get data into temp
    try(newdata <- read.csv(unzip(temp))) # unzip and read csv, dump into data
    unlink(temp)
    try(data <- rbind(data,newdata))#append new data to existing
    activeDay <- activeDay + 86400 # add one day (in seconds)
  } #end while loop for moving through time...    
  return(data) # these are the data you are looking for...
} #end FUNCTION ... getCAISObids


# Fetch CAISO public bid data (be careful, these are big files)
getCAISOopres <- function(startdate=20110101, enddate=20110102){
  #market is DAM or RTM
  
  # this function simply grabs the csv data from caiso and dumps it into a data frame with the same structure as the csv.
  
  # convert CAISO format to POSIXct dates
  start <- strptime(startdate,"%Y%m%d")
  end <- strptime(enddate,"%Y%m%d")
  # Initialize data frame with starting day
  activeDay <- start
  
  #define base URL
  baseURL <- "http://oasis.caiso.com/mrtu-oasis/SingleZip?" #note this is a grouped file

  
  # First day -- initialize data output frame with first data.
  getURL <- paste(baseURL,"resultformat=6&queryname=AS_OP_RSRV&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),sep="")
  temp <- tempfile() #create temp file
  try(download.file(getURL,temp)) # get data into temp
  try(data <- read.csv(unzip(temp)))
  unlink(temp)
  activeDay <- activeDay + 86400 # add one day (in seconds)
  
  # Subsequent days -- download and append
  while(activeDay < end){      
    getURL <- paste(baseURL,"resultformat=6&queryname=AS_OP_RSRV&startdate=",strftime(activeDay,"%Y%m%d"),"&enddate=",strftime(activeDay,"%Y%m%d"),sep="")
    temp <- tempfile() #create temp file
    try(download.file(getURL,temp)) # get data into temp
    try(newdata <- read.csv(unzip(temp))) # unzip and read csv, dump into data
    unlink(temp)
    try(data <- rbind(data,newdata))#append new data to existing
    activeDay <- activeDay + 86400 # add one day (in seconds)
  } #end while loop for moving through time...    
  return(data) # these are the data you are looking for...
} #end FUNCTION ... getCAISOopres

# add date info to CAISO data frames
addDatesCAISO <- function(caisodata,date.format=c("%Y%m%d"),date.column="OPR_DT"){
caisodata$posixlt <- as.POSIXlt(caisodata[[date.column]])
caisodata$year <- caisodata$posixlt$year+1900
caisodata$mon <- strftime(caisodata$posixlt,"%b")
caisodata$monnum <-strftime(caisodata$posixlt,"%m")
caisodata$mday <- caisodata$posixlt$mday 
caisodata$wday <- strftime(caisodata$posixlt,"%a") #week day
caisodata$date <- strftime(caisodata$posixlt,date.format)
caisodata$yhr <- caisodata$posixlt$yday*24+caisodata$posixlt$hour+1 #hour ending 1-8760 over the year
caisodata$yday <- caisodata$posixlt$yday

return(caisodata)
}

# melt CAISO data into a mono-value data frame for analysis.
meltCAISO <- function(caisodata,preserve.na=FALSE){
hourLabels <- c("HE01","HE02","HE03","HE04","HE05","HE06","HE07","HE08","HE09","HE10","HE11","HE12","HE13","HE14","HE15","HE16","HE17","HE18","HE19","HE20","HE21","HE22","HE23","HE24")
data <- melt(caisodata,id=which(!names(caisodata) %in% hourLabels),measured=which(names(caisodata) %in% hourLabels,na.rm=preserve.na))
return(data)
}

# Load duration curves with classic R look saved to pdf
makeLDC<-function(data,value.column,file="mypdf.pdf",
                  h=5,w=10,
                  xlab="Exceedance (unitless)",ylab="value",
                  main="Load Duration Curve",sub="CAISO total service area",type="p"){
  data<-arrange(data,desc(data[[value.column]]))
  pdf(file,width=w,height=h)
  cumdist <- 1:length(data[[value.column]]) / length(data[[value.column]])
  myplot<-plot(cumdist,data[[value.column]],xlab=xlab,ylab=ylab,main=main,sub=sub,type=type)
  print(myplot)
  dev.off()}

# Load duration curves with classic R look ready to send to a graphics device.
viewLDC<-function(data,value.column,
                  color, #use same number of colors as value column!
                  h=5,w=10,
                  xlab="Exceedance (unitless)",ylab="value",
                  main="Load Duration Curve",sub="CAISO total service area",type="p"){
  if(length(value.column)==1){
  # for first value column
  data<-arrange(data,desc(data[[value.column]]))
  cumdist <- 1:length(data[[value.column]]) / length(data[[value.column]])
  myplot<-plot(cumdist,data[[value.column]],col=color[1],xlab=xlab,ylab=ylab,main=main,sub=sub,type=type)
  return(myplot)
  }else{
    # for first value column initialize plot
    active.data<-arrange(data,desc(data[[value.column[1]]]))
    cumdist <- 1:length(active.data[[value.column[1]]]) / length(active.data[[value.column[1]]])
    myplot<-plot(cumdist,active.data[[value.column[1]]],col=color[1],xlab=xlab,ylab=ylab,main=main,sub=sub,type=type)
    for(i in 2:length(value.column)){
      active.data<-arrange(data,desc(data[[value.column[i]]]))
      cumdist <- 1:length(active.data[[value.column[i]]]) / length(active.data[[value.column[i]]])
      points(cumdist,active.data[[value.column[i]]],col=color[i])
    }
    return(myplot)
  }}#end function view LDC


# add temporal categories (useful for slicing up data by season / peak / weekend)
addTimeCats <- function(data,time.column="posixlt",hour.column="dhr"){
  begin.summer <- 5 #first month of summer
  end.summer <- 9 #last month of summer
  begin.peak.sum <- 13 # first hour on peak
  end.peak.sum <- 19 # last hour on peak
  begin.peak.wint <- 17
  end.peak.wint <- 20
  
  for(i in 1:length(data[[time.column]])){
    month <- data[[time.column]][["mon"]][i]+1
    hour <- data[[hour.column]][i]
    day <- data[[time.column]][["wday"]][i]
    season <- if(month <= begin.summer | month >= end.summer){"winter"}else{"summer"}
    daytype <- if(day==0 | day==6){"weekend"}else{"weekday"}
    period <- if(daytype=="weekend"){"off.peak"}else{if(season == "summer" & begin.peak.sum <= hour & hour <= end.peak.sum){"peak"}else{if(season == "winter" & begin.peak.wint <= hour & hour <= end.peak.wint){"peak"}else{"off.peak"}}}
    data$season[i] <- season
    data$period[i] <- period
    data$daytype[i] <- daytype
  }
  return(data)
  }
  
  


# Arrange ggplots (appropriated from http://gettinggeneticsdone.blogspot.com/2010/03/arrange-multiple-ggplot2-plots-in-same.html)

vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row	
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}

# Calculate DR payment (NOTE: Electricity savings potential ests are broken somehow...too high in some random cases...)

dr.payment <- function(LMP,nbt.threshold,potential,load.shape,retail.elec,only.during.nbt=FALSE){
  # LMP is a vector of LMP for the time period
  # nbt.threshold is a vector of nbt threshold times for the period (or a single value)
  # mw.potential is the maximum savings potential for the measure (a single value)
  # load shape is a vector of relative availability for the measure--fractions of peak (in time)
  # retail.elec is either a vector of prices or a single value
  
  
  timesteps <- length(LMP)
  if(length(nbt.threshold==1)){nbt.threshold <- matrix(nbt.threshold,nrow=timesteps)}
  if(length(retail.elec==1)){retail.elec <- matrix(retail.elec,nrow=timesteps)}
  if(length(load.shape)!=timesteps){stop("load shape different length than lmp")}
  
  potential <- potential*load.shape
  
  # make dr payment matrix with zeroes if < nbt and lMP if > nbt.
  dr.payment <- matrix(data=0,nrow=timesteps)
  pass.nbt <- which(LMP>=nbt.threshold)
  fail.nbt <- which(LMP<nbt.threshold)
  dr.payment[pass.nbt]<-LMP[pass.nbt]
  
  if(only.during.nbt){retail.elec[fail.nbt]<-0}
  
  dr.rev <- dr.payment*potential
  e.save <- retail.elec*potential
  tot <- dr.rev + e.save

  out<-data.frame(dr.rev=dr.rev,e.save=e.save,tot=tot)
} #end function dr.payment
