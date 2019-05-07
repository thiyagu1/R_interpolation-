
# Clear all the connection and Memory
closeAllConnections()
rm(list=ls()) 
graphics.off()

#Data Import
mydata = read.csv("C:\\Users\\varjo\\Desktop\\Final_all_dataset.csv", header = TRUE)
names(mydata)<-c("s_no","Light_intensity","Pixel_intensity")
attach(mydata)
plot(Light_intensity,Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")
newdata <- mydata[order(Pixel_intensity),] 
Final<-data.frame(newdata$Light_intensity,newdata$Pixel_intensity)
#Final <- Final[-c(53,54,55,56,57,58,59,60), ]
Final <- Final[-c(55,56,57,58,59,60), ]
names(Final)<-c("Light_intensity","Pixel_intensity")
rownames(Final) = 1:dim(Final)[1] 

# #Add Additional Points to Fit best
 Final$Pixel_intensity[53]<-249
 Final$Pixel_intensity[54]<-253
 Final$Light_intensity[53]<-135
 Final$Light_intensity[54]<-140
plot(Final$Light_intensity,Final$Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")








# Write CSV in R
write.csv(Final, file = "C:\\Users\\varjo\\Desktop\\Final_interpolation_Refined.csv")
#Segment generation

diff<-260
seg<-seq(0,260,diff)
Rlength<-length(seg)

for (i in 1:Rlength){
  if(i<Rlength){
    Final$Condition1[Final$Pixel_intensity>seg[i] & Final$Pixel_intensity<=seg[i+1]]<-seg[i+1]
  }
  else if(i==Rlength)
    Final$Condition1[Final$Pixel_intensity>seg[i] & Final$Pixel_intensity<=seg[i]]<-seg[i] 
}

s<-split(Final, Final$Condition1)
len<-length(s)
Res<-data.frame(0,0,0,0,0,0,0,0,0,0)
names(Res)<-c("I","o1","o2","o3","o4","o5","o6","o7","o8","o9")
for (i in 1:len){
  temp<-s[[i]]
  print(temp)
  filename="C:\\Users\\varjo\\Desktop\\Gamma\\"
  mypath <- file.path(paste(filename, i, ".jpg", sep = ""))
  jpeg(mypath, width = 1920, height = 1080)
  plot(temp$Light_intensity,temp$Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")
  fit <-lm(temp$Pixel_intensity~poly(temp$Light_intensity,9,raw=TRUE))
  lines(temp$Light_intensity, predict(fit, data.frame(x=temp$Light_intensity)), col="skyblue")
  dev.copy(jpeg);
  dev.off ();
  coeff<-fit$coefficients
  names(coeff)<-c("I","o1","o2","o3","o4","o5","o6","o7","o8","o9")
  Res<-rbind(Res,coeff)
  #fit[i]$coefficients
}

Result <- Res[-c(1), ]
rownames(Result) = 1:length(s) 
names(Result)<-c("Intercept","P_order_1","P_order_2","P_order_3","P_order_4","P_order_5","P_order_6","P_order_7","P_order_8","P_order_9")
split_segment<-names(s)
Result<-cbind(split_segment,Result)
graphics.off()
plot(Final$Light_intensity,Final$Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")

options(scipen=999)
#x<-Final$Light_intensity

#y = 8.728135 + 11.75104*x - 0.526867*x^2 + 0.01564272*x^3 - 0.0002866002*x^4 + 3.262234e-06*x^5 - 2.292673e-08*x^6 + 9.611848e-11*x^7 - 2.193436e-13*x^8 + 2.089698e-16*x^9 
#y = 8.242772 + 12.58992*x - 0.7143854*x^2 + 0.03103062*x^3 - 0.0009069477*x^4 + 1.717111e-05*x^5 - 2.050664e-07*x^6 + 1.481243e-09*x^7 - 5.886026e-12*x^8 + 9.852119e-15*x^9
#y = 8.440657 + 12.19154*x - 0.6148812*x^2 + 0.02206417*x^3 - 0.0005157655*x^4 + 0.000007797485*x^5 - 0.00000007528866*x^6 + 0.0000000004475405*x^7 - 0.000000000001492447*x^8 + 0.000000000000002138769*x^9


x=Final$Light_intensity
y = 8.440657 + 12.19154*x - 0.6148812*x^2 + 0.02206417*x^3 - 0.0005157655*x^4 + 0.000007797485*x^5 - 0.00000007528866*x^6 + 0.0000000004475405*x^7 - 0.000000000001492447*x^8 + 0.000000000000002138769*x^9

Final$prediction<-y
Final$Prediction_Difference<-Final$prediction-Final$Pixel_intensity
summary(Final$Prediction_Difference)

#Interpolating 
list = spline(Final$Light_intensity, Final$prediction, n = 12*length(Final$Light_intensity), method = "fmm",
              xmin = min(Final$Light_intensity), xmax = max(Final$Light_intensity), ties = mean)

list$x
list$y
x_spline = list$x
pop_spline = list$y

cool<-data.frame(x_spline,pop_spline)
names(cool)<-c("Light_Intensity", "Pixel_Intensity")

#Plotting new Interpolated Data
plot(cool$Light_Intensity,cool$Pixel_Intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")
min(cool$Pixel_Intensity)

#Normalized
cool$Normalized_Light_Intensity<-(cool$Light_Intensity - min(cool$Light_Intensity))/(max(cool$Light_Intensity) - min(cool$Light_Intensity))
plot(cool$Normalized_Light_Intensity,cool$Pixel_Intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")

write.csv(cool, file = "C:\\Users\\varjo\\Desktop\\Interpolated_Curve.csv")



#Finding x based on swapping
#y=Final$Pixel_intensity
#x = 8.440657 + 12.19154*y - 0.6148812*y^2 + 0.02206417*y^3 - 0.0005157655*y^4 + 0.000007797485*y^5 - 0.00000007528866*y^6 + 0.0000000004475405*y^7 - 0.000000000001492447*y^8 + 0.000000000000002138769*y^9





