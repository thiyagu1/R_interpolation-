
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
names(Final)<-c("Light_intensity","Pixel_intensity")

#Segment generation

diff<-250
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
Res<-data.frame(0,0,0,0)
names(Res)<-c("I","o1","o2",o3)
for (i in 1:len){
temp<-s[[i]]
print(temp)
filename="C:\\Users\\varjo\\Desktop\\Gamma\\"
mypath <- file.path(paste(filename, i, ".jpg", sep = ""))
jpeg(mypath, width = 1920, height = 1080)
plot(temp$Light_intensity,temp$Pixel_intensity, xlab = "Light_intensity",ylab = "Pixel_intensity")
fit <-lm(temp$Pixel_intensity~poly(temp$Light_intensity,3,raw=TRUE))
lines(temp$Light_intensity, predict(fit, data.frame(x=temp$Light_intensity)), col="skyblue")
dev.copy(jpeg);
dev.off ();
coeff<-fit$coefficients
names(coeff)<-c("I","o1","o2","o3")
Res<-rbind(Res,coeff)
#fit[i]$coefficients
}

Result <- Res[-c(1), ]
rownames(Result) = 1:length(s) 
names(Result)<-c("Intercept","P_order_1","P_order_2","P_order_3")
split_segment<-names(s)
Result<-cbind(split_segment,Result)
graphics.off()