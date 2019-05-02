#*******************************************************************************************
#                                           Power Function
#********************************************************************************************

# Clear all the connection and Memory
closeAllConnections()
rm(list=ls()) 
graphics.off()

Nomi <- read.csv("C:\\Users\\varjo\\Desktop\\NOMI_Data_O.csv", header = TRUE)
names(Nomi)<-c("Luminance_1","Y_1", "Luminance_2","Y_2", "Luminance_3","Y_3")
attach(Nomi)

a <- 41.8189
g <- 0.3595858

# Case 1 : Dataset 1
ISO<-100
Exposure<-1/125
#par(mfrow = c(3,1))
x <- Luminance_1*ISO*Exposure
plot(x,Y_1, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y<- (a*x^g)
points(x,pixel_intensity_y, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )
legend("bottomright", c("Nomicam_Y1", "POW_EQ_Y1"), col=c("red", "blue"), lty=1, cex=0.8)
title("Mapping Luminance to Luma Pixel intensity ")

# 0000000000000000000000000000000000000000000000000000000000000000Case 2 : Dataset 2
ISO_2<-100 * 4.0
Exposure_2<-1/125
x_2 <- Luminance_2*ISO_2*Exposure_2
plot(x_2,Y_2, xlab = "X_2 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y_2<- (a*x_2^g) #* 1.7
points(x_2,pixel_intensity_y_2, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )
legend("bottomright", c("Nomicam_Y2", "POW_EQ_Y2"), col=c("red", "blue"), lty=1, cex=0.8)
title("Mapping Luminance to Luma Pixel intensity ")

# Case 2 : Dataset 3
ISO_3<-100 * 8.0
Exposure_3<-1/125
x_3 <- Luminance_3*ISO_3*Exposure_3
plot(x_3,Y_3, xlab = "X_3 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y_3<- (a*x_3^g) #* 1.3
points(x_3,pixel_intensity_y_3, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )
legend("bottomright", c("Nomicam_Y3", "POW_EQ_Y3"), col=c("red", "blue"), lty=1, cex=0.8)
title("Mapping Luminance to Luma Pixel intensity ")

graphics.off()
# Case 4 : All Data in one plot
plot(x,Y_1, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
legend("bottomright", c("Nomicam_Y1", "Nomicam_Y2", "Nomicam_Y3", "POW_EQ_Y1", "POW_EQ_Y2", "POW_EQ_Y3"), col=c("red", "blue", "green", "orange","skyblue", "black"), lty=1, cex=0.8)
points(x_2,Y_2, xlab = "X_2 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "blue") #, type="l", lty=2, lwd=3, col )
points(x_3,Y_3, xlab = "X_3 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "green") #, type="l", lty=2, lwd=3, col )
points(x,pixel_intensity_y, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "orange") #, type="l", lty=2, lwd=3 )
points(x_2,pixel_intensity_y_2, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "skyblue") #, type="l", lty=2, lwd=3 )
points(x_3,pixel_intensity_y_3, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "black") #, type="l", lty=2, lwd=3 )

D1<-pixel_intensity_y - Y_1
D2<-pixel_intensity_y_2 - Y_2
D3<-pixel_intensity_y_3 - Y_3

RMSQ1 <- sqrt(mean(pixel_intensity_y - Y_1)^2)
RMSQ2 <- sqrt(mean(pixel_intensity_y_2 - Y_2)^2)
RMSQ3 <- sqrt(mean(pixel_intensity_y_3 - Y_3)^2)

Final<-data.frame("Y1" = Y_1, "P1" = pixel_intensity_y,"Diff_1" = D1, "Y2" = Y_2,"P2" = pixel_intensity_y_2, "Diff_2" = D2, "Y3" = Y_3,"P3"=pixel_intensity_y_3, "Diff_3" = D3)



#Other Optional 
attach(Final)
summary(Diff_1)
summary(Diff_2)
summary(Diff_3)
Var1<-var(Diff_1)
Var2<-var(Diff_2)
Var3<-var(Diff_3)
SD1<-sqrt(sum((Diff_1-mean(Diff_1))^2/(length(Diff_1)-1)))
SD2<-sqrt(sum((Diff_2-mean(Diff_2))^2/(length(Diff_2)-1)))
SD3<-sqrt(sum((Diff_3-mean(Diff_3))^2/(length(Diff_3)-1)))
#print (c(Var1,SD1,var2,SD2,Var3,SD3))


#Print Results
SD1
SD2
SD3
