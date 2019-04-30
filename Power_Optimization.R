

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
g <- 0.3295858

# Case 1 : Dataset 1

ISO<-100
Exposure<-1/125
par(mfrow = c(3,1))
x <- Luminance_1*ISO*Exposure
plot(x,Y_1, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y<- (a*x^g)
points(x,pixel_intensity_y, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )



# Case 2 : Dataset 2

ISO_2<-100 * 4.0
Exposure_2<-1/125
x_2 <- Luminance_2*ISO_2*Exposure_2
plot(x_2,Y_2, xlab = "X_2 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y_2<- (a*x_2^g) #* 1.7
points(x_2,pixel_intensity_y_2, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )



# Case 2 : Dataset 3

ISO_3<-100 * 16.0
Exposure_3<-1/125
x_3 <- Luminance_3*ISO_3*Exposure_3
plot(x_3,Y_3, xlab = "X_3 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y_3<- (a*x_3^g) #* 1.3
points(x_3,pixel_intensity_y_3, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )

graphics.off()


# Case 4 : All Data in one plot

plot(x,Y_1, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
points(x_2,Y_2, xlab = "X_2 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "blue") #, type="l", lty=2, lwd=3, col )
points(x_3,Y_3, xlab = "X_3 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "green") #, type="l", lty=2, lwd=3, col )
points(x,pixel_intensity_y, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "orange") #, type="l", lty=2, lwd=3 )
points(x_2,pixel_intensity_y_2, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "skyblue") #, type="l", lty=2, lwd=3 )
points(x_3,pixel_intensity_y_3, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "black") #, type="l", lty=2, lwd=3 )

Final<-data.frame("P1" = pixel_intensity_y, "P2" = pixel_intensity_y_2, "P3"=pixel_intensity_y_3)

# Add a legend
#legend(295, 95, legend=c("Y1", "Y2", "Y3", "Predicted_Y1", "Predicted_Y2", "Predicted_Y3"), lty=2:4, lwd=3, cex=0.8)
legend("bottomright", c("Y1", "Y2", "Y3", "Predicted_Y1", "Predicted_Y2", "Predicted_Y3"), col=c("red", "blue", "green", "orange","skyblue", "black"), lty=1, cex=0.8)
title("Mapping Luminance to Luma Pixel intensity ")

