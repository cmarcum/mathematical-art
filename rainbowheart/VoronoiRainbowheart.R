#This is effectively devel code but I added it to master anyway in case folks wanted to see the devel process without making a pull request. 
#Voronoi code modified from https://www.codeproject.com/Articles/1189287/Generating-Random-Voronoi-Diagrams
# by Anatol Voevudko.
# 9 March 2021 by @cmarcum (CSM)

#CSM - TO DO:
# 1) pipe initial voronoi diagram output directly to R object
# 2) revise the voronoi color function parameter to be tuneable
# 3) pass default args via ellipsis to plot in voronoi function

#  Helper Functions, unused by CSM - needs to be revised to be tunable
## HF#1 Random hex color returned as "#RRGGBB".
randHclr <- function() {
  m=255;r=g=b=0;
  r <- sample(0:m, 1, replace=TRUE);
  g <- sample(0:m, 1, replace=TRUE);
  b <- sample(0:m, 1, replace=TRUE);
  return(rgb(r,g,b,maxColorValue=m));
}
## HF#2 Return the value of a metric: Euclidean, Manhattan or Minkovski
Metric <- function(x, y, mt) {
  if(mt==1) {return(sqrt(x*x + y*y))}
  if(mt==2) {return(abs(x) + abs(y))}
  if(mt==3) {return((abs(x)^3 + abs(y)^3)^0.33333)}
}

## Generating and plotting Voronoi diagram.
## ns - number of sites, fn - file name, ttl - plot title.
## mt - type of metric: 1 - Euclidean, 2 - Manhattan, 3 - Minkovski.
pVoronoiD <- function(ns, fn="", ttl="",mt=1) {
  cat(" *** START VD:", date(), "\n");
  if(mt<1||mt>3) {mt=1}; mts=""; if(mt>1) {mts=paste0(", mt - ",mt)};
  m=640; i=j=k=m1=m-2; x=y=d=dm=0;
  if(fn=="") {pf=paste0("VDR", mt, ns, ".png")} else {pf=paste0(fn, ".png")};
  if(ttl=="") {ttl=paste0("Voronoi diagram, sites - ", ns, mts)};
  cat(" *** Plot file -", pf, "title:", ttl, "\n");
  par(mar=c(0,0,0,0))
  plot(NA, xlim=c(0,m), ylim=c(0,m), xlab="", ylab="", main="",xaxt="n",yaxt="n");
  # Building 3 arrays for requested n random sites (located on canvas)
  # and linked to them random colors.
  X=numeric(ns); Y=numeric(ns); C=numeric(ns);
  for(i in 1:ns) {
    X[i]=sample(0:m1, 1, replace=TRUE);
    Y[i]=sample(0:m1, 1, replace=TRUE);
    #C[i]=randHclr();
    C[i]=rainbow(ns)[i];
  }
  X<-sort(X)
  Y<-sort(Y)
  # Plotting sites and applying selected mt metric and linked colors.
  for(i in 0:m1) {
    for(j in 0:m1) {
      dm=Metric(m1,m1,mt); k=-1;
      for(n in 1:ns) {
        d=Metric(X[n]-j,Y[n]-i, mt);
        if(d<dm) {dm=d; k=n;}
      }
      clr=C[k]; segments(j, i, j, i, col=clr);
    }
  }
  # Plotting site points (in black).
  #points(X, Y, pch = 19, col = "transparent", bg = "white")
  dev.copy(png, filename=pf, width=m, height=m);
  dev.off(); graphics.off();
  cat(" *** END VD:",date(),"\n");
}
## Executing:
#pVoronoiD(10)           ## Euclidean metric
#pVoronoiD(10,"","",2)   ## Manhattan metric
#pVoronoiD(10,"","",3)   ## Minkovski metric
#pVoronoiD(150)          ## Euclidean metric

pVoronoiD(100)

stop()

library(e1071)
library(gganimate)
library(tidyverse)
library(grid)
library(png)

#Polar function for Cardioid with implicit approach from Wolfram 
# http://mathworld.wolfram.com/HeartCurve.html
heart.dat<- data.frame(t=seq(0, 2*pi, by=.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
heart.dat$x=xhrt(heart.dat$t)
heart.dat$y=yhrt(heart.dat$t)

#Examine initial cardioid
#plot(heart.dat[,2:3])

#pars
nt<-20
time<-rep(1:nt,nrow(heart.dat))
sc<-c(8.5,6.5)

#Use random bridges to walk around with scale
x1<-c(rbind(heart.dat$x,sweep(replicate(nrow(heart.dat),rbridge(frequency=nt-1)*sc[1]),2,heart.dat$x,FUN="+")))
y1<-c(rbind(heart.dat$y,sweep(replicate(nrow(heart.dat),rbridge(frequency=nt-1)*sc[2]),2,heart.dat$y,FUN="+")))

vng <- readPNG("VDR1100.png")

#Put it in a tibble so gg works nicely
heart.walk<-tibble(Time=time,x=x1,y=y1,col=sort(rep(rainbow(nrow(heart.dat)),nt)))

draw.heart <- heart.walk %>%
  ggplot(aes(
    x = x, 
    y = y)) +
  annotation_custom(rasterGrob(vng, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(
    show.legend = FALSE,
    size = 5,alpha=.5,color=heart.walk$col) + ease_aes('linear') + theme_void()


anim.heart <- draw.heart + transition_time(time = Time) + shadow_wake(wake_length = .1,exclude_phase=c("enter","exit"))

anim.heart %>% animate(detail=5,type="cairo",nframes=100, start_pause=4,end_pause=2)

anim_save("vheart.gif")
