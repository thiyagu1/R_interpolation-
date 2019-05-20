
# Clear all the connection and Memory
closeAllConnections()
rm(list=ls()) 
graphics.off()

library(ggplot2)

#Data Original Import
mydata = read.csv("C:\\Users\\varjo\\Desktop\\Final_all_dataset.csv", header = TRUE)
mydata <- mydata[ -c(1) ]
names(mydata)<-c("Light_intensity","Pixel_intensity")
mydata$group<- rep(1, each=60)
ggplot(mydata, aes(x=Light_intensity, y=Pixel_intensity)) + geom_point()



#Data Cleaning
#plot(Light_intensity,Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")
newdata <- mydata[order(Pixel_intensity),] 
Final<-data.frame(newdata$Light_intensity,newdata$Pixel_intensity)
#Final <- Final[-c(53,54,55,56,57,58,59,60), ]
Final <- Final[-c(55,56,57,58,59,60,1,2,3,4,5,6,7,8,9,10,11,12,13,14), ]
names(Final)<-c("Light_intensity","Pixel_intensity")
rownames(Final) = 1:dim(Final)[1] 
# #Add Additional Points to Fit best
Final$Pixel_intensity[39]<-249
Final$Pixel_intensity[40]<-253
Final$Light_intensity[39]<-135
Final$Light_intensity[40]<-140
#Final$Pixel_intensity[53]<-249
#Final$Pixel_intensity[54]<-253
#Final$Light_intensity[53]<-135
#Final$Light_intensity[54]<-140
#plot(Final$Light_intensity,Final$Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")


# Adding Origin (0,0)
LI<-c(0)
PI<-c(0)
# LI<-c(0.0007275, 0.001455, 0.00291,0.00582)
# PI<-c(0.325, 0.65, 1.3, 2.6)
Final1<-data.frame(LI,PI)
names(Final1)<-c("Light_intensity","Pixel_intensity")
Final <- rbind(Final1, Final)
group<- rep(2, each=41)
Final <- data.frame(cbind(Final,group))


plot(Final$Light_intensity,Final$Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity", col="Red")
fit <-lm(Final$Pixel_intensity~poly(Final$Light_intensity,9,raw=TRUE))
lines(Final$Light_intensity, predict(fit, data.frame(x=Final$Light_intensity)), col="skyblue")
coeff<-fit$coefficients
Res<-data.frame(0,0,0,0,0,0,0,0,0,0)

Res<-rbind(Res,coeff)
Result <- Res[-c(1), ]
rownames(Result) = 1:length(s) 
names(Result)<-c("Intercept","P_order_1","P_order_2","P_order_3","P_order_4","P_order_5","P_order_6","P_order_7","P_order_8","P_order_9")

x= Final$Light_intensity
y = 0.5990317 + 13.90099*x - 0.7364667*x^2 + 0.02571748*x^3 - 0.0005556208*x^4 + 7.487023e-06*x^5 - 6.261863e-08*x^6 + 3.141575e-10*x^7 - 8.616575e-13*x^8 + 9.890992e-16*x^9
#y = -0.0132644955334661 + 14.5542392141813*x - 0.869114454181935*x^2 + 0.0363548342887775*x^3 - 0.000988367980569141*x^4 + 0.0000175260397817941*x^5 - 0.000000202921694941043*x^6 + 0.00000000151224948878638*x^7 - 0.00000000000695252225068811*x^8 + 0.0000000000000178478249501752*x^9 -0.0000000000000000194796472972162*x^10


Final$prediction<-y
Final$Prediction_Difference<-Final$prediction-Final$Pixel_intensity
summary(Final$Prediction_Difference)
options(digits = 10)
p<-seq(1,255,1)
Real = vector(,255)




for (i in p){
  print(i)
  r<- polyroot(c(0.5990317-i, 13.90099, -0.7364667, 0.02571748, -0.0005556208, 7.487023e-06, -6.261863e-08, 3.141575e-10, -8.616575e-13, 9.890992e-16))
  r
  #r<-polyroot(c(-0.0132644955334661-i, 14.5542392141813, -0.869114454181935, 0.0363548342887775, -0.000988367980569141, 0.0000175260397817941, -0.000000202921694941043, 0.00000000151224948878638, -0.00000000000695252225068811, 0.0000000000000178478249501752, -0.0000000000000000194796472972162))
  position<-which(round(Im(r),7)== 0)
  position
  lt = vector(,length(position))
  for (j in 1:length(position)){
                  lt[j]<-Re(r[position[j]])
  }
  Real[i]<-min(lt)
}

x=	Real
y = 0.5990317 + 13.90099*x - 0.7364667*x^2 + 0.02571748*x^3 - 0.0005556208*x^4 + 7.487023e-06*x^5 - 6.261863e-08*x^6 + 3.141575e-10*x^7 - 8.616575e-13*x^8 + 9.890992e-16*x^9
#y = -0.0132644955334661 + 14.5542392141813*x - 0.869114454181935*x^2 + 0.0363548342887775*x^3 - 0.000988367980569141*x^4 + 0.0000175260397817941*x^5 - 0.000000202921694941043*x^6 + 0.00000000151224948878638*x^7 - 0.00000000000695252225068811*x^8 + 0.0000000000000178478249501752*x^9 -0.0000000000000000194796472972162*x^10


x
u<-y
#u<-format(round(y, 8), nsmall = 8)

#u
options(scipen=0)
group3<- rep(3, each=255)
x<-(x - min(x))/(max(x) - min(x))
x_f<-formatC(x, format = "e", digits = 8)

x_f<-paste0(x_f, "f")
Ultimate<-data.frame(x_f,u,group3)
names(Ultimate)<-c( "Light_intensity", "Pixel_intensity", "group")
ggplot(Ultimate, aes(x=Ultimate$Light_intensity, y=Ultimate$Pixel_intensity)) + geom_point(size=1) 
Ville_final_x<-paste0(x_f, collapse=",")
Ville_final_y<-paste0(u, collapse=",")

# Write CSV in R
write.csv(Ultimate, file = "C:\\Users\\varjo\\Desktop\\Ultimate_Pixel_to_Light_Intensity.csv")

# Plot Actual Data vs Cleaned Data
Allplot<- rbind(Allplot,Ultimate)
# Make color depend on yval
ggplot(Allplot, aes(x=Allplot$Light_intensity, y=Allplot$Pixel_intensity, colour=Allplot$group)) + geom_point(size=1) 




