#####################################
### code for the DISSECTION paper ###
#####################################

# load the general library
library(RPostgreSQL)
library(xtable)
library(elasticnet)
library(bcp)
library(lubridate)
library(Hmisc)
library(tfplot)


cat("\014")  # clear console
rm(list=ls(all=TRUE))

set.seed(1707)

beginTime    = proc.time()
beginSysTime = Sys.time()

### set global dates
date <- list()

date$dataBeg        <- as.Date("1992-03-1")
date$dataEnd        <- as.Date("2020-12-1")
date$vintageVJAinit <- as.Date("2009-12-1")
date$vintageVJAlast <- as.Date("2015-06-1")

path <- list()
path$main    <- "Z:/NotesKOF/NowCasting/FMinRealTime/"
path$data    <- paste0(path$main,"Data/") 
path$plot    <- paste0(path$main,"Plot/") 
path$text    <- paste0(path$main,"Text/") 

### set global tables for SQL
csv <- list()
csv$GDP <- "GDP_Vintage_2015Q2.csv" 
csv$DFM <- "GLOBAL_DB_GDP_FCST_FCTRQ_20150828.csv"
csv$FAE <- "GDP_FirstAvailableActual.csv"   

vintageVJA <- seq(date$vintageVJAinit %m+% months(0), date$vintageVJAlast, by="3 month"); vintageVJA
#vintageVJA <- seq(date$vintageVJAinit %m+% months(0), by="3 month", length.out=27); vintageVJA

opt <- list()
opt$data <- "GDP_QOQ_" 
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
          legend(2007.25,0.9,c("Post. prob."), col = c("black"),bty = "n", lwd = 2)
          tfplot:::tfXaxis(prob)
      par(op2)
  
  par(op1)  
  dev.off()
}
fnPlotActlFcst <- function(actl, fcst, sTitle){
  pdf(file=paste(path$plot,"Plot_",sTitle,".pdf",sep=""),paper="special",width=12,height=8)
  ymax = max(cbind(actl,fcst))
  ymin = min(cbind(actl,fcst))
  
  plot (actl, type = "b", lwd = 2, xaxt="n", ylab = "", ylim = c(ymin,ymax), main = sTitle)
  lines(fcst, type = "b", col = "red", lwd = 2) 
  legend("topright",c("Actual","Forecast"), col = c("black","red"), lwd = 2, bty = "n")
  tfplot:::tfXaxis(tsFAE4Ncst)
  dev.off()
}
########################################################################

dateNcst2010 <- c("2010.03.02","2010.06.02","2010.09.02","2010.12.02")
dateNcst2011 <- c("2011.03.01","2011.06.05","2011.09.06","2011.12.01")
dateNcst2012 <- c("2012.03.02","2012.06.01","2012.09.04","2012.11.29")
dateNcst2013 <- c("2013.03.01","2013.06.03","2013.09.03","2013.11.28")
dateNcst2014 <- c("2014.03.04","2014.05.29","2014.09.02","2014.12.03")
dateNcst2015 <- c("2015.03.03","2015.05.29")

dateNcst <- c(dateNcst2010,dateNcst2011,dateNcst2012,dateNcst2013,dateNcst2014,dateNcst2015)

### load DFM-forecasts
dfm     <- read.csv(paste0(path$data,csv$DFM), na.strings = "#N/A"); head(dfm)
dateBeg <- as.numeric(unlist(strsplit(as.character(dfm$X[1]),"-")))

### load First-Available GDP estimates (Q-on-Q)
fae   <- read.csv(paste0(path$data,csv$FAE)); head(fae)
tsFAE <- ts(fae[,"h.0"], start = as.numeric(fae[1,c("year","period")]), freq = 4) 

### collect nowcasts
cdateBegNcst <- unlist(strsplit(dateNcst[1],".", fixed = TRUE)); cdateBegNcst
sdateBegNcst <- as.Date(sprintf("%s-%s-01",cdateBegNcst[1],cdateBegNcst[2]))

vNcst <- sapply(dateNcst, function(idateNcst){
    cdateNcst  <- unlist(strsplit(idateNcst,".", fixed = TRUE))
    sdateNcst  <- as.Date(sprintf("%s-%s-01",cdateNcst[1],cdateNcst[2]))
    
    idxFitFcst <- grep(paste0("FitFcst_",idateNcst),colnames(dfm))
    
    if(idateNcst == "2014.09.02") idxFitFcst <- idxFitFcst[2]
    
    tsFitFcst <- ts(dfm[,colnames(dfm)[idxFitFcst]], start = dateBeg, freq = 4); tsFitFcst
    ncst      <- window(tsFitFcst, start = c(year(sdateNcst), quarter(sdateNcst)), end = c(year(sdateNcst), quarter(sdateNcst)))
    ncst
})

tsNcts     <- ts(vNcst, start = c(year(sdateBegNcst), quarter(sdateBegNcst)), freq = 4)
tsFAE4Ncst <- window(tsFAE, start = start(tsNcts), end = end(tsNcts))
tsErroNcst <- tsFAE4Ncst - tsNcts
msfeNcst   <- fnMSFE(tsErroNcst); msfeNcst
mafeNcst   <- fnMAFE(tsErroNcst); mafeNcst

### make Historical average forecasts
dateBegHist <- c(1992,1)
mBnch <- sapply(dateNcst, function(idateNcst){ #    idateNcst <- dateNcst[1]
    
    cdateNcst <- unlist(strsplit(idateNcst,".", fixed = TRUE))
    sdateNcst <- as.Date(sprintf("%s-%s-01",cdateNcst[1],cdateNcst[2]))
    sdatePrev <- sdateNcst - months(3)
    
    sDateGDP  <- paste0(year(sdatePrev)-2000,quarter(sdatePrev)); sDateGDP 
    sDateGDP  <- ifelse(nchar(sDateGDP) == 2, paste0(0,sDateGDP), sDateGDP)
    sGDP      <- paste0(".",sDateGDP,".qoq")
    sGDP      <- ifelse(sdatePrev < as.Date("2014-06-30"), paste0("ts41808001",sGDP), paste0("ts61808001",sGDP))
    
    tsGDP     <- na.omit(window(ts(dfm[,sGDP], start = dateBeg, freq = 4), start = dateBegHist)); tsGDP
    
    ### fit AR(1)
    gdp.ar   <- ar(tsGDP, aic = FALSE, order.max = 1, method = "ols")
    gdp.pred <- predict(gdp.ar, n.ahead = 1)
    
    fsctHist <- mean(tsGDP); 
    
    c(fsctHist,gdp.pred$pred)
})
mBnch
tsBnch <- ts(t(mBnch), start = c(year(sdateBegNcst), quarter(sdateBegNcst)), freq = 4)
tsHist <- tsBnch[,1]
tsAR1M <- tsBnch[,2]

tsCMBN <- (tsNcts + tsAR1M) / 2

tsErroHist <- tsFAE4Ncst - tsHist
msfeHist   <- fnMSFE(tsErroHist); msfeHist
mafeHist   <- fnMAFE(tsErroHist); mafeHist

tsErroAR1M <- tsFAE4Ncst - tsAR1M
msfeAR1M   <- fnMSFE(tsErroAR1M); msfeAR1M
mafeAR1M   <- fnMAFE(tsErroAR1M); mafeAR1M

tsErroCMBN <- tsFAE4Ncst - tsCMBN
msfeCMBN   <- fnMSFE(tsErroCMBN); msfeCMBN
mafeCMBN   <- fnMAFE(tsErroCMBN); mafeCMBN

R2OS_DFM     <- (msfeHist - msfeNcst) / msfeHist * 100; R2OS_DFM
R2OS_AR1     <- (msfeHist - msfeAR1M) / msfeHist * 100; R2OS_AR1
R2OS_CMB     <- (msfeHist - msfeCMBN) / msfeHist * 100; R2OS_CMB
R2OS_AR1_DFM <- (msfeAR1M - msfeNcst) / msfeAR1M * 100; R2OS_AR1_DFM

### compute SFED
sfed_DFM   <- tsErroHist^2 - tsErroNcst^2; sfed_DFM
cssfed_DFM <- ts(cumsum(sfed_DFM), start = start(sfed_DFM), freq = 4)
afed_DFM   <- abs(tsErroHist) - abs(tsErroNcst); afed_DFM
csafed_DFM <- ts(cumsum(afed_DFM), start = start(sfed_DFM), freq = 4); csafed_DFM

sfed_AR1   <- tsErroHist^2 - tsErroAR1M^2; sfed_AR1
cssfed_AR1 <- ts(cumsum(sfed_AR1), start = start(sfed_AR1), freq = 4)

sfed_CMB   <- tsErroHist^2 - tsErroCMBN^2; sfed_CMB
cssfed_CMB <- ts(cumsum(sfed_CMB), start = start(sfed_CMB), freq = 4); cssfed_CMB

sfed_AR1_DFM   <- tsErroAR1M^2 - tsErroNcst^2; sfed_AR1_DFM
cssfed_AR1_DFM <- ts(cumsum(sfed_AR1_DFM), start = start(sfed_AR1_DFM), freq = 4); cssfed_AR1_DFM

sfed   <- c(  sfed_DFM,  sfed_AR1)
cssfed <- c(cssfed_DFM,cssfed_AR1)

plot( sfed_DFM, col = "red", ylim = c(min(sfed), max(sfed)))
lines(sfed_AR1, col = "blue")
abline(h = 0)

plot( cssfed_DFM, col = "red", ylim = c(min(cssfed), max(cssfed)))
lines(cssfed_AR1, col = "blue")
lines(cssfed_CMB, col = "green")
abline(h = 0)

### plot CSSFED against HM
y0 <- cssfed_DFM; sTitle <- "DFM vs HM"      
fnPlotBCP(y0,sTitle,"topright")

y0 <- csafed_DFM; sTitle <- "DFM vs HM CSAFED"      
fnPlotBCP(y0,sTitle,"topright")

y0 <- cssfed_AR1; sTitle <- "AR1 vs HM"      
fnPlotBCP(y0,sTitle,"topright")

y0 <- cssfed_CMB; sTitle <- "CMB vs HM"      
fnPlotBCP(y0,sTitle,"topright")

y0 <- cssfed_AR1_DFM; sTitle <- "DFM vs AR1"      
fnPlotBCP(y0,sTitle,"topleft")


### plot Actual and Forecast values 

actl <- tsFAE4Ncst
fcst <- tsNcts
fnPlotActlFcst(actl, fcst, "DFM")

actl <- tsFAE4Ncst
fcst <- tsAR1M
fnPlotActlFcst(actl, fcst, "AR1")

actl <- tsFAE4Ncst
fcst <- tsHist
fnPlotActlFcst(actl, fcst, "HM")

