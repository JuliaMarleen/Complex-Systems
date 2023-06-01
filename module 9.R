

getwd()
setwd("D:/Documents auf D/00Tilburg/00TiU/blok1/ComplexSystems/portfolio/lecture2time")
d0=read.csv("ESMdata.csv")

plot(d0$mood_relaxed,type = "l")

plot(walking_data$`LeftAnkleJoint_AnteroPosterior(mm)`
     [1:length(walking_data$`LeftAnkleJoint_AnteroPosterior(mm)`)-1],
     diff(walking_data$`LeftAnkleJoint_AnteroPosterior(mm)`),pch=20, xlab='x(t)', ylab='vel x(t)')

plot(d0$mood_relaxed[1:length(d0$mood_relaxed)-1],diff(d0$mood_relaxed),xlab='x(t)',ylab = 'vel x(t)')

names(d0)



    

plot(walking_data$`RightHipJoint_Vertical(mm)`[1:length(walking_data$`RightHipJoint_Vertical(mm)`)-1],
     diff(walking_data$`RightHipJoint_Vertical(mm)`),xlab='x(t)',ylab = 'vel x(t)')




##########     #############     #############     ##############         #########

getwd()
setwd()

# sine and cosinus wave in R

time<- seq(0*pi,2*pi,pi/100)
A<-1
freq<-2
d<-0
phi<-0
sine.wave<-A*sin(freq*time+phi)+d
plot(time,sine.wave,type = "l",ylab = 'y(t)',xlab = 't',main = 'simple sine wave and doubble cosinus')
freq<-4
cos.wave<-A*cos(freq*time+phi)+d
lines(time,cos.wave)

#phase portrait 

plot(sine.wave[1:(length(sine.wave)-1)],diff(sine.wave),xlab = 'x(t)',ylab = 'vel x(t)',ylim = c(-.15,.15),type = 'l' )
lines(cos.wave[1:(length(cos.wave)-1)],diff(cos.wave),col='blue')

#Hilbert Transform

require(hht)
ht.sine.wave<-HilbertTransform(sine.wave)
ht.cos.wave<- HilbertTransform(cos.wave)

plot(time,sine.wave,main = "Hilbert Transform of Sine Wave",type = "l",lwd=4)
lines(time,Re(ht.sine.wave),col="green") #real components of Hilbert transform
lines(time,Im(ht.sine.wave),col="red")
legend("topright",col=c("black","green","red"),lty=c(1,1,1),legend = c("Signal","Real","Imaginary"))


plot(time,cos.wave,main = "Hilbert Transform of Cosinus Wave",type = "l",lwd=4)
lines(time,Re(ht.cos.wave),col="green") #real components of Hilbert transform
lines(time,Im(ht.cos.wave),col="red")
legend("topright",col=c("black","green","red"),lty=c(1,1,1),legend = c("Signal","Real","Imaginary"))

#Phase Angles 
phase.angle.1<-pracma::rad2deg(atan2(Im(ht.sine.wave),Re(ht.sine.wave)))
phase.angle.2<-pracma::rad2deg(atan2(Im(ht.cos.wave),Re(ht.cos.wave)))

plot(time,phase.angle.1,type = 'l')
lines(time,phase.angle.2,col='blue')
legend("bottomleft",col = c("black","blue"), lty = c(1,1),legend = c("Sinus","Cosinus freq"))

#phase differences
rel.phase<-as.data.frame(cbind(time,abs(phase.angle.1-phase.angle.2)))
names(rel.phase)<-c("time","relative.phase")
#correction of discontinuous values (Ippersiel et al 2021)
rel.phase$relative.phase[rel.phase$relative.phase>180]<-(360-rel.phase$relative.phase[rel.phase$relative.phase>180])

plot(rel.phase$time,rel.phase$relative.phase,type = 'l',xlab = 'time',
     ylab = 'Continuous Relative Phase')


###########            ##############         ##############         ##############
# different data set
library(readxl)
walking_data <- read_excel("D:/Documents auf D/00Tilburg/00TiU/blok1/ComplexSystems/portfolio/0lectureCoordination/AC68TP07_treadmill_walking_60hz_4.4kmh.xlsx")
View(walking_data)   # cite Carpinella, I., Crenna, P., Rabuffetti, M., & Ferrarin, M. (2010). 
 
wd<-walking_data   


plot(wd$'Time(s)',wd$'LeftAnkleJoint_MedioLateral(mm)',type='l',xlab = "t",ylab = "y(t)")
lines(wd$'Time(s)',wd$'LeftKneeJoint_MedioLateral(mm)',col = 'blue')
legend("topright",col=c("black","blue"),lty=c(1,1),legend = c("ankle","knee"))

#phase portrait

plot(wd$'LeftAnkleJoint_MedioLateral(mm)'[1:(length(wd$'LeftAnkleJoint_MedioLateral(mm)')-1)],diff(wd$'LeftAnkleJoint_MedioLateral(mm)'),type = 'l',,xlab = 'x(t)',ylab = 'vel x(t)')
lines(wd$`LeftKneeJoint_MedioLateral(mm)`[1:(length(wd$'LeftKneeJoint_MedioLateral(mm)')-1)],diff(wd$'LeftKneeJoint_MedioLateral(mm)'),col='blue')
legend("bottomright",,col=c("black","blue"),lty=c(1,1),legend = c("ankle","knee"))

plot(sine.wave[1:(length(sine.wave)-1)],diff(sine.wave),xlab = 'x(t)',ylab = 'vel x(t)',ylim = c(-0.07,0.07),type = 'l' )
lines(cos.wave[1:(length(cos.wave)-1)],diff(cos.wave),col='red')


#rename
knee.left<-wd$'LeftKneeJoint_MedioLateral(mm)'
ankle.left<-wd$'LeftAnkleJoint_MedioLateral(mm)'
time <- walking_data$`Time(s)`

#centering
knee.left.cen<- knee.left-min(knee.left)-(max(knee.left)-min(knee.left))/2
ankle.left.cen<- ankle.left-min(ankle.left)-(max(ankle.left)-min(ankle.left))/2


#Hilbert Transform

require(hht)
ht.knee.left<-HilbertTransform(knee.left.cen)
ht.ankle.left<- HilbertTransform(ankle.left.cen)

plot(time,ht.knee.left,main = "Hilbert Transform of knee left",type = "l",lwd=4,ylim = c(-70,70))
lines(time,Re(ht.knee.left),col="green") #real components of Hilbert transform
lines(time,Im(ht.knee.left),col="red")
legend("bottom",col=c("black","green","red"),lty=c(1,1,1),legend = c("Signal","Real","Imaginary"))


plot(time,ht.ankle.left,main = "Hilbert Transform of ankle left",type = "l",lwd=4,ylim = c(-70,70))
lines(time,Re(ht.ankle.left),col="green") #real components of Hilbert transform
lines(time,Im(ht.ankle.left),col="red")
legend("bottom",col=c("black","green","red"),lty=c(1,1,1),legend = c("Signal","Real","Imaginary"))

#Phase Angles 
phase.angle.1<-pracma::rad2deg(atan2(Im(ht.ankle.left),Re(ht.ankle.left)))
phase.angle.2<-pracma::rad2deg(atan2(Im(ht.knee.left),Re(ht.knee.left)))

plot(time,phase.angle.1,type = 'l',ylab = 'phase angles')
lines(time,phase.angle.2,col='blue')
legend("bottomleft",col = c("black","blue"), lty = c(1,1),legend = c("Ankle","Knee"))

#phase differences
rel.phase<-as.data.frame(cbind(time,abs(phase.angle.1-phase.angle.2)))
names(rel.phase)<-c("time","relative.phase")
#correction of discontinuous values (Ippersiel et al 2021)
rel.phase$relative.phase[rel.phase$relative.phase>180]<-(360-rel.phase$relative.phase[rel.phase$relative.phase>180])

plot(rel.phase$time,rel.phase$relative.phase,type = 'l',xlab = 'time',
     ylab = 'Continuous Relative Phase')



###########     #############   #############  ###############    ##########

# esm data 
getwd()

d0=read.csv("D:/Documents auf D/00Tilburg/00TiU/blok1/ComplexSystems/portfolio/0lectureCoordination/ESMdata.csv") 

plot(d0$mood_relaxed,type="l",ylab = "mood scale",main ="item: mood relaxed")
plot(d0$phy_hungry,type = 'l',clo='blue')
# ------------ centering ------------------

cmrelax=d0$mood_relaxed - min(d0$mood_relaxed)- (max(d0$mood_relaxed)-min(d0$mood_relaxed))/2
cphunger=d0$phy_hungry  - min(d0$phy_hungry,na.rm = TRUE)  - (max(d0$phy_hungry,na.rm = T)  -min(d0$phy_hungry,na.rm = T))/2
# !!! NAs removed without further explaination

plot(cmrelax,type = 'l',ylab = "scale")
lines(cphunger,type = 'l',col='blue')
legend("bottomleft",col = c("black","blue"), lty = c(1,1),legend = c("relaxed","hungry"))

# ------------  phase portrait  ------------------------
plot(d0$mood_relaxed[1:length(d0$mood_relaxed)-1],diff(d0$mood_relaxed),type = 'l',xlab = "x(t)",ylab = "vel x(t)")
# what do I see? 
#human movement plotted against itself, compared to a sinus wave this got more jumps
#what makes it a question of imagination to see the circle back from other movement. 
# still there is to see how changes in mood/movement is connected, with a finer scale probably
#other movement could be detected too
#lets try another variable
#plot(d0$phy_hungry,type = 'l',)
lines(d0$phy_hungry[1:length(d0$phy_hungry)-1],diff(d0$phy_hungry),col='blue',type = 'l',xlab = "x(t)",ylab = "vel x(t)")
#difference between phsych and phys feeling??
legend("topright",col = c("black","blue"), lty = c(1,1),legend = c("relaxed","hungry"))



# ------------- hilbert transform 
index<-1:length(cmrelax)
require(hht)
ht.cmrelax<-HilbertTransform(cmrelax)

plot(cmrelax,type = 'l')
lines(index,ht.cmrelax,main = "Hilbert Transform mood relax",type = "l",col='blue')
#imaginary part gekickt
lines(index,Re(ht.cmrelax),type = 'l') #real components of Hilbert transform
lines(index,Im(ht.cmrelax),col="red")
legend("bottomleft",col=c("black",'red'),lty=c(1,1),legend=c("Real","Imaginary"))

cphunger<-na.omit(cphunger)#how to remove missings in hilbert internet R 
ht.cphunger<- HilbertTransform(cphunger)
plot(1:length(ht.cphunger),ht.cphunger,type = 'l',xlab='time',ylab='hungry scale')# warning iaginary discarded
lines(1:length(ht.cphunger),Re(ht.cphunger),type = 'l')
lines(1:length(ht.cphunger),Im(ht.cphunger),type = 'l',col='red')
legend("bottomleft",col=c("black",'red'),lty=c(1,1),legend=c("Real","Imaginary"))
# ---------------  Phase Angles  -----------
phase.angle.1<-pracma::rad2deg(atan2(Im(ht.cmrelax),Re(ht.cmrelax)))
phase.angle.2<-pracma::rad2deg(atan2(Im(ht.cphunger),Re(ht.cphunger)))


plot(1:length(ht.cphunger),phase.angle.2,col='blue',type = 'l',xlab = 'index',ylab = "phase angles")
lines(index,phase.angle.1,type = 'l')
legend("bottomleft",col = c("black","blue"), lty = c(1,1),legend = c("relaxed","hungry"))

#-----------------   phase differences ------------------------
#phase differences
length(phase.angle.1)
index=index[1:1471]
phase.angle.1=phase.angle.1[1:1471]
phase.angle.2=phase.angle.2[1:1471]
rel.phase<-as.data.frame(cbind(index[1:1471],abs(phase.angle.1[1:1471]-phase.angle.2[1:1471])))
names(rel.phase)<-c("index","relative.phase")

#correction of discontinuous values (Ippersiel et al 2021)
rel.phase$relative.phase[rel.phase$relative.phase>180]<-(360-rel.phase$relative.phase[rel.phase$relative.phase>180])

plot(rel.phase$index,rel.phase$relative.phase,type = 'l',xlab = 'index',
     ylab = 'Continuous Relative Phase')



