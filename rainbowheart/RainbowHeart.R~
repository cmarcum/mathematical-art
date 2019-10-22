#################################################################
#This script was inspired by a post by
# Dr. DJ Navarro @djnavarro on github and twitter
# Where she used Brownian bridges to resolve a heart-shape
# https://twitter.com/djnavarro/status/1185370781183705089
# Her original gif is much smoother than the result of this
# rendering, which was created to force myself to figure out
# how it was done without peaking at her code.
#
#Author: Chris Marcum <cmarcum@uci.edu>
#Last Modified: 21 October 2019
#################################################################

library(e1071)
library(gganimate)
library(tidyverse)

#Polar function for Cardioid with implicit approach from Wolfram 
# http://mathworld.wolfram.com/HeartCurve.html
heart.dat<- data.frame(t=seq(0, 2*pi, by=.1) )
 xhrt <- function(t) 16*sin(t)^3
 yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
 heart.dat$x=xhrt(heart.dat$t)
 heart.dat$y=yhrt(heart.dat$t)

#Examine initial cardioid
plot(heart.dat[,2:3])

#pars
nt<-20
time<-rep(1:nt,nrow(heart.dat))
sc<-c(8.5,6.5)

#Use random bridges to walk around with scale
x1<-c(rbind(heart.dat$x,sweep(replicate(nrow(heart.dat),rbridge(frequency=nt-1)*sc[1]),2,heart.dat$x,FUN="+")))
y1<-c(rbind(heart.dat$y,sweep(replicate(nrow(heart.dat),rbridge(frequency=nt-1)*sc[2]),2,heart.dat$y,FUN="+")))

#Put it in a tibble so gg works nicely
heart.walk<-tibble(Time=time,x=x1,y=y1,col=sort(rep(rainbow(nrow(heart.dat)),nt)))

draw.heart <- heart.walk %>%
  ggplot(aes(
    x = x, 
    y = y)) + 
  geom_point(
    show.legend = FALSE,
    size = 5,alpha=.5,color=heart.walk$col) + ease_aes('linear') + theme_void()
 

anim.heart <- draw.heart + transition_time(time = Time) + shadow_wake(wake_length = .1,exclude_phase=c("enter","exit"))
anim.heart %>% animate(detail=5,type="cairo",nframes=100, start_pause=4,end_pause=2)

anim_save("rainbowheart.gif")
