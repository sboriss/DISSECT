### draw timing of fiscal forecasts for Switzerland

library(IDPmisc)
library(grid)
library(shape)
library(diagram)

############# beg ### figure for timing #################################
par(mai=c(.5,0.5,0.5,0.5))
par(oma=c(0.1,0.1,0.1,0.1))
plot(c(0,9), c(0,1), type="n",yaxt = "n", xaxt = "n",xlab = "",ylab = "")
yaxis <- 0.4
axis(1,pos = yaxis, outer = TRUE, tck=0.05, at = seq(0,9,1), labels = FALSE)
axis(1,pos = yaxis, outer = TRUE, tck=0.1, at = seq(0,9,3), labels = FALSE)

### set coordinates for forecasts
x.point <- c(2.1,5.1,8.1)
y.point <- c(yaxis,yaxis,yaxis)


points(x.point,y.point, pch=16, col="black")
points(seq(2.25,5.25,0.25)-0.15,rep(y.point[1],13)-0.01, pch=2, col="blue")


text(x.point[1],y.point[1]+0.5,substitute(GDP[2012*"Q"*2]),cex = 1.5)
text(x.point[2],y.point[2]+0.5,substitute(GDP[2012*"Q"*3]),cex = 1.5)
text(x.point[3],y.point[3]+0.5,substitute(GDP[2012*"Q"*4]),cex = 1.5)

text( 0.5,yaxis-0.05,substitute(VII) ,cex = 1.5)
text( 1.5,yaxis-0.05,substitute(VIII),cex = 1.5)
text( 2.5,yaxis-0.05,substitute(IX)  ,cex = 1.5)
text( 3.5,yaxis-0.05,substitute(X)   ,cex = 1.5)
text( 4.5,yaxis-0.05,substitute(XI)  ,cex = 1.5)
text( 5.5,yaxis-0.05,substitute(XII) ,cex = 1.5)
text( 6.5,yaxis-0.05,substitute(I)   ,cex = 1.5)
text( 7.5,yaxis-0.05,substitute(II)  ,cex = 1.5)
text( 8.5,yaxis-0.05,substitute(III) ,cex = 1.5)

text(x.point[1]-0.6,y.point[1]-0.2,substitute(2012*"Q"*3),cex = 2)
text(x.point[2]-0.6,y.point[2]-0.2,substitute(2012*"Q"*4),cex = 2,col="red")
text(x.point[3]-0.6,y.point[3]-0.2,substitute(2013*"Q"*1),cex = 2)

straightarrow(from=c(x.point[1],y.point[1]+0.45),to=c(x.point[1],yaxis+0.02),lty=1,lcol=1,lwd=1, arr.pos = 1)
straightarrow(from=c(x.point[2],y.point[1]+0.45),to=c(x.point[2],yaxis+0.02),lty=1,lcol=1,lwd=1, arr.pos = 1)
straightarrow(from=c(x.point[3],y.point[1]+0.45),to=c(x.point[3],yaxis+0.02),lty=1,lcol=1,lwd=1, arr.pos = 1)

############# end ### figure for timing #################################
# 
# ############# beg ### figure for period t #################################
# par(mai=c(.5,0.5,0.5,0.5))
# par(oma=c(0.1,0.1,0.1,0.1))
# plot(c(0,3), c(0,1), type="n",yaxt = "n", xaxt = "n",xlab = "",ylab = "", main="Information set in period t")
# yaxis <- 0.1
# axis(1,pos = yaxis, outer = TRUE, tck=0.05, at = seq(0,3,1), labels = FALSE)
# 
# text( 0.5,yaxis-0.05,substitute(t-1),cex = 1.5)
# text( 1.5,yaxis-0.05,substitute(t  ),cex = 1.5)
# text( 2.5,yaxis-0.05,substitute(t+1),cex = 1.5)
# 
# ### set coordinates for forecasts
# fx <- c(0.5, 1.5, 2.5)
# fy <- c(0.25, 0.3, 0.5)
# 
# text( fx[1]+0.1,fy[1]-0.05,substitute(R[t-1*"|"*t-2]^f),cex = 1.5)
# text( fx[2]-0.1,fy[2]-0.05,substitute(R[t*"|"*t-1  ]^f),cex = 1.5)
# text( fx[3]+0.1,fy[3]-0.05,substitute(R[t+1*"|"*t]^f),cex = 1.5)
# 
# points(fx, fy, pch=16, col="black")
# 
# ### set coordinates for outturn
# ax <- c(0.5)
# ay <- c(0.5)
# 
# text( ax[1]+0.1,ay[1]-0.05,substitute(R[t-1*"|"*t]^a),cex = 1.5)
# 
# points(ax, ay, pch=16, col="red")
# ############# end ### figure for period t #################################
# 
# ############# beg ### figure for period t+2 #################################
# par(mai=c(.5,0.5,0.5,0.5))
# par(oma=c(0.1,0.1,0.1,0.1))
# plot(c(0,3), c(0,1), type="n",yaxt = "n", xaxt = "n",xlab = "",ylab = "", main="Information set in period t+2")
# yaxis <- 0.1
# axis(1,pos = yaxis, outer = TRUE, tck=0.05, at = seq(0,3,1), labels = FALSE)
# 
# text( 0.5,yaxis-0.05,substitute(t-1),cex = 1.5)
# text( 1.5,yaxis-0.05,substitute(t  ),cex = 1.5)
# text( 2.5,yaxis-0.05,substitute(t+1),cex = 1.5)
# 
# ### set coordinates for forecasts
# fx <- c(0.5, 1.5, 2.5)
# fy <- c(0.25, 0.3, 0.5)
# 
# text( fx[1]+0.1,fy[1]-0.05,substitute(R[t-1*"|"*t-2]^f),cex = 1.5)
# text( fx[2]-0.1,fy[2]-0.05,substitute(R[t*"|"*t-1  ]^f),cex = 1.5)
# text( fx[3]+0.1,fy[3]-0.05,substitute(R[t+1*"|"*t]^f),cex = 1.5)
# 
# points(fx, fy, pch=16, col="black")
# 
# ### set coordinates for outturn
# ax <- c(0.5,1.5,2.5)
# ay <- c(0.5,0.85,0.65)
# 
# text( ax[1]+0.1,ay[1]-0.05,substitute(R[t-1*"|"*t]^a),cex = 1.5)
# text( ax[2]-0.1,ay[2]-0.05,substitute(R[t*"|"*t+1]^a),cex = 1.5)
# text( ax[3]+0.1,ay[3]-0.05,substitute(R[t+1*"|"*t+2]^a),cex = 1.5)
# points(ax, ay, pch=16, col="red")
# 
#  lines(ax[2:3], ay[2:3], col="red"  ,lwd=2)
#  lines(fx[2:3], fy[2:3], col="black",lwd = 2)
#  
#  lines(c(ax[3],fx[3]),c(ay[3],fy[3]), col="blue",lwd = 2)
# 
# ############# end ### figure for period t+2 #################################




