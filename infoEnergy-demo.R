#infoEnergy Demo -- CAISO Data

# 2014 Feb 28  Peter Alstone

# STEP 1: Load Packages

require(ggplot2)  # for nice plots
require(plyr)     # for data processing
require(reshape)  # for melting and casting data 
#(and you actually have to use reshape not reshape2...melting issues...)

source("CAISO-oasisAPI-operations.R")

# STEP 2: Grab Data

# LOADS ~~~~~~~
load2011 <- getCAISOload(startdate=20110101,enddate=20120101)
# ...Add dates and melt...
load2011 <- addDatesCAISO(load2011)
load2011m <- meltCAISO(load2011,preserve.na=TRUE)

# ...Add more date stuff
load2011m$dhr<-as.numeric(substr(load2011m$variable,3,4))
load2011m$yhr <- load2011m$yday*24+load2011m$dhr


# STEP 3: Explore Data

# DATAFRAME 1: sys8760------------------------------------------------
sys8760 <- subset(load2011m,TAC_AREA_NAME=="CA ISO-TAC" & MARKET_RUN_ID =="ACTUAL" & XML_DATA_ITEM=="SYS_FCST_ACT_MW")
sys8760 <- arrange(sys8760,yhr)
sys8760$sys.load <- sys8760$value #move values to dedicated sys column
# clean up superfluous columns
sys8760$value <- NULL
sys8760$HE25 <- NULL
sys8760$variable <- NULL
sys8760$LABEL <- NULL
sys8760$XML_DATA_ITEM <- NULL

sys8760 <- addTimeCats(sys8760) #add categories for slicing...

# PLot the Load

casys <- ggplot(sys8760, aes(x=dhr, y=sys.load))

quartz()

casys + geom_point()

casys + geom_point() + geom_smooth()

casys + geom_jitter() + geom_smooth()

casys + geom_jitter(aes(color=season)) + geom_smooth()

casys + geom_jitter() + geom_smooth() + facet_grid(wday ~ monnum)

