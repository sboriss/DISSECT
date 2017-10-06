#####################################
### code for the DISSECTION paper ###
#####################################
# join forecasts from DFM from SK12 paper
# and real-time squared forecasts from BS(2012)
#
# > msfeARM   <- fnMSFE(tsErroARM); msfeARM: [1] 0.1366939 with opt$dateBegEstn <- c(1992,1)


# load the general library
library(RPostgreSQL)
library(xtable)
library(elasticnet)
library(bcp)
library(lubridate)
library(Hmisc)
library(tfplot)
library(forecast)


cat("\014")  # clear console
rm(list=ls(all=TRUE))

set.seed(1707)

beginTime    = proc.time()
beginSysTime = Sys.time()

### set global dates
date <- list()

date$dataBeg        <- as.Date("1992-03-1")
date$dataEnd        <- as.Date("2020-12-1")
date$vintageVJAinit <- as.Date("2004-12-1")
date$vintageVJAlast <- as.Date("2016-06-1")

path <- list()
path$main    <- "Z:/NotesKOF/NowCasting/FMinRealTime/"
path$data    <- paste0(path$main,"Data/") 
path$plot    <- paste0(path$main,"Plot/") 
path$text    <- paste0(path$main,"Text/") 

### set global tables for SQL
csv <- list()
csv$GDP       <- "GDP_Vintage_2004Q4-2015Q2.csv" 
csv$DFMexAnte <- "GLOBAL_DB_GDP_FCST_FCTRQ_20150828.csv"
csv$DFMexPost <- "DFM_FCST_2005Q1-2009Q4_R7SUB1.csv"

csv$FAE <- "GDP_FirstAvailableActual_2005Q1-2015Q2.csv"   

vintageVJA <- seq(date$vintageVJAinit %m+% months(0), date$vintageVJAlast, by="3 month"); vintageVJA
#vintageVJA <- seq(date$vintageVJAinit %m+% months(0), by="3 month", length.out=27); vintageVJA

opt <- list()
opt$data <- "GDP_QOQ_" 
opt$dateBegEstn <- c(1992,1) ### estimation start of DFM c(2000,4)
opt$makePlot    <- FALSE
opt$saveCSV     <- FALSE # TRUE
opt$nameOutput  <- paste0(path$text,"TEXT4DISSECTION.txt") 

sink(opt$nameOutput, append = FALSE)
cat("\nTEXT4DISSECTION: ")
sink()

### functions ##########################################################
fnQOQPCT <- function(x){ return (diff(x,lag = 1)/lag(x,-1)*100)}

fnYOYPCT <- function(x){ return (diff(x,lag = 4)/lag(x,-4)*100)}

fnGetDateSQL <- function(x){
        ### create dates in SQL(hstore) format  
        # input: a ts object x
        period <- "month"
        beg.x  <- start(x)
        if(frequency(x) == 4){ period <- "3 months"; beg.x[2] <- (beg.x[2]-1)*3+1;}
        return(seq(as.Date(sprintf("%i-%i-1", beg.x[1], beg.x[2])), by=period, length.out=length(x)))
}
fnGetHstore <- function(x){
        ### create hstore object for SQL  
        # input: a ts object x
        dates  <- fnGetDateSQL(x); dates
        hstore <- paste(dates, x, sep="=>", collapse=",")
        hstore <- gsub("NA","NULL",hstore); 
        return(hstore)
}
fnRMSFE  <- function(x){ return (sqrt(sum(   (na.omit(x))^2) / length(na.omit(x))))}
fnMSFE  <- function(x){ return       (sum(   (na.omit(x))^2) / length(na.omit(x)))}
fnMAFE  <- function(x){ return       (mean(abs(na.omit(x))  ))}
fnMinorTicks <- function(n){
        range <- par("usr")[1:2]
        tick.pos <- par("xaxp")
        distance.between.minor <- (tick.pos[2] - tick.pos[1])/tick.pos[3]/n
        possible.minors        <- tick.pos[1] - (0:100) * distance.between.minor
        low.minor <- min(possible.minors[possible.minors >= range[1]])
        if (is.na(low.minor)) low.minor <- tick.pos[1]
        possible.minors <- tick.pos[2] + (0:100) * distance.between.minor
        hi.minor <- max(possible.minors[possible.minors <= range[2]])
        return(list(low.minor = low.minor, hi.minor = hi.minor, distance.between.minor = distance.between.minor))
}
fnPlotBCP <- function(y, sTitle, sPlaceLegend){
  pdf(file=paste(path$plot,"PlotBCP_",sTitle,".pdf",sep=""),paper="special",width=12,height=8)
  op1 <- par(mfrow=c(2,1),col.lab="black",col.main="black")
  
      y  <- c(y0)
      oBCP <- bcp(y)
      mBCP <- cbind(y, summary(oBCP))
      if(any(is.na(y0))) mBCP <- rbind(matrix(NA, nrow = h, ncol = 3),mBCP)
      tsBCP <- ts(mBCP, start = start(y0), freq = 4) 
      
      op2 <- par(mar=c(0,2,2,2), cex.axis=0.75) 
          post = tsBCP[,"X1"]
          actl = tsBCP[,"y"]
          plot(post, type = "l", lwd = 2, mgp = c(3, 1, 0), ylim = c(min(na.omit(actl)),max(na.omit(actl))),xaxt="n", ylab = "", main = sTitle)
          lines(actl,type = "p", pch = 16, col = "red")
          abline(h = 0)
          tfplot:::tfXaxis(post)
          legend(sPlaceLegend,c("CSSFED","Post.mean"), col = c("red","black"), lwd = 2, bty = "n")
          par(op2)
          op2 <- par(mar=c(2,2,0,2), cex.axis=0.75) 
          prob = tsBCP[,"Probability"]
          plot(prob, type = "l", lwd = 2, mgp = c(3, 1, 0),xaxt="n", ylab = "")
          #legend(start(prob)[1],0.9,c("Post. prob."), col = c("black"),bty = "n", lwd = 2)
          tfplot:::tfXaxis(prob)
      par(op2)
  
  par(op1)  
  dev.off()
}
fnPlotActlFcst <- function(actl, fcst, sTitle){
  pdf(file=paste(path$plot,"Plot_",sTitle,".pdf",sep=""),paper="special",width=12,height=8)
  ymax = max(cbind(actl,fcst))
  ymin = min(cbind(actl,fcst))
  
  plot (actl, type = "b", lwd = 2, xaxt="n", ylab = "", ylim = c(ymin,ymax), main = sTitle, pch = 16)
  lines(fcst, type = "b", col = "red", lwd = 2) 
  legend("topright",c("Actual","Forecast"), col = c("black","red"), lwd = 2, bty = "n")
  tfplot:::tfXaxis(actl)
  dev.off()
}
########################################################################

### load GDP QOQ vintages
gdp <- read.csv(paste0(path$data,csv$GDP), sep = ";"); head(gdp)
cdateBegGDP <- as.numeric(gdp[1,c("year","period")]); cdateBegGDP
tsGDP <- ts(gdp, start = cdateBegGDP, freq = 4)



### load DFM-forecasts (ex ante)
dateNcst2010 <- c("2010.03.02","2010.06.02","2010.09.02","2010.12.02")
dateNcst2011 <- c("2011.03.01","2011.06.05","2011.09.06","2011.12.01")
dateNcst2012 <- c("2012.03.02","2012.06.01","2012.09.04","2012.11.29")
dateNcst2013 <- c("2013.03.01","2013.06.03","2013.09.03","2013.11.28")
dateNcst2014 <- c("2014.03.04","2014.05.29","2014.09.02","2014.12.03")
dateNcst2015 <- c("2015.03.03","2015.05.29")

dateDFMexAnte <- c(dateNcst2010,dateNcst2011,dateNcst2012,dateNcst2013,dateNcst2014,dateNcst2015)

dfmexante     <- read.csv(paste0(path$data,csv$DFMexAnte), na.strings = "#N/A"); head(dfmexante)
dateBeg <- as.numeric(unlist(strsplit(as.character(dfmexante$X[1]),"-")))

### collect nowcasts (ex ante)
cdateBegDFMexAnte <- unlist(strsplit(dateDFMexAnte[1],".", fixed = TRUE)); cdateBegDFMexAnte
sdateBegDFMexAnte <- as.Date(sprintf("%s-%s-01",cdateBegDFMexAnte[1],cdateBegDFMexAnte[2]))

vDFMexAnte <- sapply(dateDFMexAnte, function(idate){
    cdate  <- unlist(strsplit(idate,".", fixed = TRUE))
    sdate  <- as.Date(sprintf("%s-%s-01",cdate[1],cdate[2]))
    
    idxFitFcst <- grep(paste0("FitFcst_",idate),colnames(dfmexante))
    
    if(idate == "2014.09.02") idxFitFcst <- idxFitFcst[2]
    
    tsFitFcst <- ts(dfmexante[,colnames(dfmexante)[idxFitFcst]], start = dateBeg, freq = 4); tsFitFcst
    ncst      <- window(tsFitFcst, start = c(year(sdate), quarter(sdate)), end = c(year(sdate), quarter(sdate)))
    ncst
})

tsDFMexAnte <- ts(vDFMexAnte, start = c(year(sdateBegDFMexAnte), quarter(sdateBegDFMexAnte)), freq = 4); tsDFMexAnte
sdateEndDFMexAnte <- as.Date(sprintf("%s-%s-01",end(tsDFMexAnte)[1],end(tsDFMexAnte)[2] * 3))

### collect nowcasts (ex post)
dfmexpost         <- read.csv(paste0(path$data,csv$DFMexPost), sep = ";"); head(dfmexpost)
cdateBegDFMexPost <- as.numeric(dfmexpost[1, c("year","period")]); cdateBegDFMexPost
sdateBegDFMexPost <- as.Date(sprintf("%s-%s-01",cdateBegDFMexPost[1],cdateBegDFMexPost[2]))
tsDFMexPost       <- ts(dfmexpost[,"R7SUB1"], start = cdateBegDFMexPost, freq = 4); tsDFMexPost

### merge ex post and ex ante forecasts
tsDFM <- ts(NA, start = start(tsDFMexPost), end = end(tsDFMexAnte), freq = 4)
window(tsDFM, start = start(tsDFMexPost), end = end(tsDFMexPost)) <- tsDFMexPost
window(tsDFM, start = start(tsDFMexAnte), end = end(tsDFMexAnte)) <- tsDFMexAnte
tsDFM

dateDFM <- seq(sdateBegDFMexPost %m+% months(0), sdateEndDFMexAnte, by="3 month");

plot(tsDFM, type = "b")

### load First-Available GDP estimates (Q-on-Q)
fae   <- read.csv(paste0(path$data,csv$FAE), sep = ";"); head(fae)
tsFAE <- ts(fae[,"h.0"], start = as.numeric(fae[1,c("year","period")]), freq = 4) 
tsFAE <- window(tsFAE, start = start(tsDFM), end = end(tsDFM))


tsErroDFM <- tsFAE - tsDFM
msfeDFM   <- fnMSFE(tsErroDFM); msfeDFM


### make Historical average (HAM) and autoregressive (ARM) forecasts

mBnch <- sapply(dateDFM, function(idate){ #   idate <- dateDFM[2]; idate 
  
    sdatePrev <- idate - months(3); sdatePrev
    nameGDP <- paste0(opt$data,year(sdatePrev),"Q",quarter(sdatePrev))
    
    itsGDP  <- window(tsGDP[,nameGDP], start = opt$dateBegEstn, end = c(year(sdatePrev), quarter(sdatePrev))); itsGDP
    
    ### fit AR(1)
    gdp.ar   <- ar(itsGDP, aic = FALSE, order.max = 1, method = "ols")
    gdp.pred <- predict(gdp.ar, n.ahead = 1)
    
    fsctHAM <- mean(itsGDP); 
    
    c(fsctHAM,gdp.pred$pred)
})
mBnch
tsBnch <- ts(t(mBnch), start = start(tsDFM), freq = 4)
tsHAM <- tsBnch[,1]
tsARM <- tsBnch[,2]

tsErroHAM <- tsFAE - tsHAM
msfeHAM   <- fnMSFE(tsErroHAM); msfeHAM

tsErroARM <- tsFAE - tsARM
msfeARM   <- fnMSFE(tsErroARM); msfeARM


R2OS_HAM_DFM <- (msfeHAM - msfeDFM) / msfeHAM * 100; R2OS_HAM_DFM
R2OS_HAM_ARM <- (msfeHAM - msfeARM) / msfeHAM * 100; R2OS_HAM_ARM
R2OS_ARM_DFM <- (msfeARM - msfeDFM) / msfeARM * 100; R2OS_ARM_DFM

### compute SFED
sfed_HAM_DFM   <- tsErroHAM^2 - tsErroDFM^2; sfed_HAM_DFM
cssfed_HAM_DFM <- ts(cumsum(sfed_HAM_DFM), start = start(sfed_HAM_DFM), freq = 4); cssfed_HAM_DFM

sfed_HAM_ARM   <- tsErroHAM^2 - tsErroARM^2; sfed_HAM_ARM
cssfed_HAM_ARM <- ts(cumsum(sfed_HAM_ARM), start = start(sfed_HAM_ARM), freq = 4); cssfed_HAM_ARM

sfed_ARM_DFM   <- tsErroARM^2 - tsErroDFM^2; sfed_ARM_DFM
cssfed_ARM_DFM <- ts(cumsum(sfed_ARM_DFM), start = start(sfed_ARM_DFM), freq = 4); cssfed_ARM_DFM

sfed   <- cbind(sfed_HAM_DFM,sfed_ARM_DFM)
cssfed <- cbind(cssfed_HAM_DFM,cssfed_ARM_DFM)

plot( sfed_HAM_DFM, col = "red", ylim = c(min(sfed), max(sfed)))
lines(sfed_ARM_DFM, col = "blue")
abline(h = 0)

plot( cssfed_HAM_DFM, col = "red", ylim = c(min(cssfed), max(cssfed)))
lines(cssfed_ARM_DFM, col = "blue")
lines(cssfed_HAM_ARM, col = "green")
abline(h = 0)

### do DM test
dm.test(tsErroARM,tsErroDFM,h=1)

### plot the squared forecast errors
vy <- tsErroDFM^2
vx <- tsErroARM^2
sTitle <- "Squared forecast errors: DFM and ARM"
sTitleSave <- "SquaredForecastErrors_DFMvsARM"

# pdf(file=paste(path$plot,"Plot_",sTitleSave,".pdf",sep=""),paper="special",width=12,height=8)

ymax = max(cbind(vy,vx))
ymin = min(cbind(vy,vx))

plot (vy, type = "b", lwd = 2, xaxt="n", ylab = "", ylim = c(ymin,ymax), main = sTitle, pch = 16)
lines(vx, type = "b", col = "red", lwd = 2) 
legend("topright",c("DFM","ARM"), col = c("black","red"), lwd = 2, bty = "n", pch = c(16,1) )
tfplot:::tfXaxis(vy)

# dev.off()

dfmA <- fnMSFE( window( tsErroDFM, start = c(2006,2), end = c(2008,3) ) ); dfmA
armA <- fnMSFE( window( tsErroARM, start = c(2006,2), end = c(2008,3) ) ); armA

(armA - dfmA) / armA * 100

dfmB <- fnMSFE( window( tsErroDFM, start = c(2009,2), end = c(2011,4) ) ); dfmB
armB <- fnMSFE( window( tsErroARM, start = c(2009,2), end = c(2011,4) ) ); armB

(armB - dfmB) / armB * 100

dfmC <- fnMSFE( window( tsErroDFM, start = c(2012,3), end = c(2014,4) ) ); dfmC 
armC <- fnMSFE( window( tsErroARM, start = c(2012,3), end = c(2014,4) ) ); armC

(armC - dfmC) / armC * 100



### plot CSSFED
# y0 <- cssfed_HAM_DFM; sTitle <- "DFM vs HAM"      
# fnPlotBCP(y0,sTitle,"topleft")
# 
# y0 <- cssfed_HAM_ARM; sTitle <- "ARM vs HAM"      
# fnPlotBCP(y0,sTitle,"topright")
# 
# y0 <- cssfed_ARM_DFM; sTitle <- "DFM vs ARM"      
# fnPlotBCP(y0,sTitle,"topleft")
# 
# 
# ### plot Actual and Forecast values 
# 
# actl <- tsFAE
# fcst <- tsDFM
# fnPlotActlFcst(actl, fcst, "DFM")
# 
# actl <- tsFAE
# fcst <- tsARM
# fnPlotActlFcst(actl, fcst, "ARM")
# 
# actl <- tsFAE
# fcst <- tsHAM
# fnPlotActlFcst(actl, fcst, "HAM")

