
# Clear all the connection and Memory
closeAllConnections()
rm(list=ls()) 
graphics.off()

#Data Import
mydata = read.csv("C:\\Users\\varjo\\Desktop\\Data_O.csv", header = TRUE)
names(mydata)<-c("EV","LUX")
attach(mydata)


#Normalized the Data if Needed
#normalized = (LUX-min(LUX))/(max(LUX)-min(LUX))

# Original Plot for EV to Lux chart
normalized <- LUX
par(mfrow = c(2,1))
plot(EV,normalized, xlab = "EV",ylab = "LUX")
title("Standard EV to LUX chart")

#Actual Interpolation Function

list = spline(EV, normalized, n = 4*length(EV), method = "fmm",
              xmin = min(EV), xmax = max(EV), ties = mean)

x_spline = list$EV
pop_spline = list$normalized

#Plotting new Interpolated Data
attach(list)
plot(x,y, xlab = "EV",ylab = "LUX")
title("Interpolated  EV to LUX chart ")

#format(list[["y"]], scientific = FALSE)
options(scipen=999) # 0 - Enable Scientific Notation or 999 - Disable scientific Notation 
Final_EV<-list[["x"]]
Final_Lux<-list[["y"]]

# Making a Dataframe object for the EV and the LUX value
Final<-data.frame("EV" = Final_EV, "LUX" = Final_Lux)

# Write CSV in R
write.csv(Final, file = "C:\\Users\\varjo\\Desktop\\Final_interpolation.csv")

#*******************************************************************************************************

# Clear all the connection and Memory
closeAllConnections()
rm(list=ls()) 
graphics.off()

Nomi <- read.csv("C:\\Users\\varjo\\Desktop\\NOMI_Data_O.csv", header = TRUE)
names(Nomi)<-c("Luminance_1","Y_1", "Luminance_2","Y_2", "Luminance_3","Y_3")
write.csv(Nomi, file = "C:\\Users\\varjo\\Desktop\\Final.xlsx")
#Luminance_2<-(Luminance_2*370*1/90)
#Luminance_3<-(Luminance_3*720*1/90)
attach(Nomi)
#par(mfrow = c(3,1))
plot(Luminance_1,Y_1, xlab = "Luminance * Exp * ISO (cd/m2)",ylab = "Y (pixel)") #, type="l", lty=2, lwd=3 )
#lines(Luminance_2,Y_2, xlab = "Luminance (cd/m2)",ylab = "Y (pixel)", type="l", lty=3, lwd=3 )
#lines(Luminance_3,Y_3, xlab = "Luminance (cd/m2)",ylab = "Y (pixel)", type="l", lty=4, lwd=3 )

#case1: Poly fit
fit <-lm(Y_1~poly(Luminance_1,2,raw=TRUE))

#case2: Polynomial Fit
#fit <-lm(Y_1 ~ 1 + I(Luminance_1) + I(Luminance_1^2) + I(Luminance_1^3) + I(Luminance_1^4) + I(Luminance_1^5) + I(Luminance_1^6)) # +  I(Luminance_1^7) +  I(Luminance_1^8) + I(Luminance_1^9)

#case3: Power Function Fit
#fit <-nls(Y_1 ~ a * I(Luminance_1^g),start=list(a = 1, g=-0.45), trace = T)
#coef(fit)

#case4: exponential Fit
#fit <-nls(Y_1 ~ a*exp(b*Luminance_1)+C, start=list(a=-1, b=-0.5, c= 1), trace = T)                       #(Luminance_1)^(g),start=list(a=1, g=-0.55), trace = T)

lines(Luminance_1, predict(fit, data.frame(x=Luminance_1)), col="red")
title("Mapping Luminance to Luma Pixel intensity ")
summary(fit)
# Add a legend
#legend(295, 95, legend=c("Data1", "Data2", "Data3"), lty=2:4, lwd=3, cex=0.8)
# some model evaluation, residual sum of square and R²
RSS <- sum(residuals(fit)^2)
TSS <- sum((Y_1 - mean(Y_1))^2)
R.square <- 1 - (RSS/TSS)

#------------------














# Clear all the connection and Memory
closeAllConnections()
rm(list=ls()) 
graphics.off()

Nomi <- read.csv("C:\\Users\\varjo\\Desktop\\NOMI_Data_O.csv", header = TRUE)
names(Nomi)<-c("Luminance_1","Y_1", "Luminance_2","Y_2", "Luminance_3","Y_3")
Luminance_2<-(Luminance_2*370*1/125)
Luminance_3<-(Luminance_3*720*1/125)
attach(Nomi)
#par(mfrow = c(3,1))
plot(Luminance_1,Y_1, xlab = "Luminance * Exp * ISO (cd/m2)",ylab = "Y (pixel)") #, type="l", lty=2, lwd=3 )
#lines(Luminance_2,Y_2, xlab = "Luminance (cd/m2)",ylab = "Y (pixel)") #, type="l", lty=3, lwd=3 )
#lines(Luminance_3,Y_3, xlab = "Luminance (cd/m2)",ylab = "Y (pixel)") #, type="l", lty=4, lwd=3 )


#case1: Poly fit
#fit <-lm(Y_1~poly(Luminance_1,7,raw=TRUE))

#case2: Polynomial Fit
#fit <-lm(Y_1 ~ 1 + I(Luminance_1) + I(Luminance_1^2) + I(Luminance_1^3) + I(Luminance_1^4) + I(Luminance_1^5)) #+ I(Luminance_1^6)) # +  I(Luminance_1^7) +  I(Luminance_1^8) + I(Luminance_1^9)

#case3: Power Function Fit
#fit <-nls(Y_1 ~ a * I(Luminance_1^g),start=list(a = 1, g=-0.45), trace = T)
#coef(fit)
fit = 40.8189*Luminance_1^0.3495858 #fit = 40.64189*Luminance_1^0.3495858 

#case4: exponential Fit -Halflife
#fit <-nls(Y_1 ~ a + (b/2^(Luminance_1/c)),start=list(a = 1, b=1, c=1), trace = T)

# theta.0 <- min(Y_1) * 0.5
# model.0 <- lm(log(Y_1 - theta.0) ~ Luminance_1)
# alpha.0 <- exp(coef(model.0)[1])
# beta.0 <- coef(model.0)[2]
# start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
# #start <- list(alpha = 1, beta = 1e4, theta = 1)
# start
# fit <- nls(Y_1 ~ I(alpha * exp(beta * Luminance_1) + theta) ,  algorithm="plinear", start = start)
# #fit <-nls(Y_1 ~ a * exp(b * Luminance_1),start=list(a=1, b=1), trace = T)
# 
#y = 20.21859 - (-4.500743/0.01996319)*(1 - exp(-0.01996319*Luminance_1))
lines(Luminance_1,fit, col="skyblue")
#lines(Luminance_1, predict(fit, data.frame(x=Luminance_1)), col="skyblue")
title("Mapping Luminance to Luma Pixel intensity ")
summary(fit)
# Add a legend
#legend(295, 95, legend=c("Data1", "Data2", "Data3"), lty=2:4, lwd=3, cex=0.8)
# some model evaluation, residual sum of square and R²
RSS <- sum(residuals(fit)^2)
TSS <- sum((Y_1 - mean(Y_1))^2)
R.square <- 1 - (RSS/TSS)

summary(fit)
# Exponential- HalfLife -  y = 245.6707 - 225.4521/2^(x/34.72126)
# Exponential - Propotion rate growth y = 20.21859 - (-4.500743/0.01996319)*(1 - e^(-0.01996319*x))
# Poly 5th order - y = 14.9973 + 5.715775*x - 0.07690625*x^2 + 0.0005343721*x^3 - 0.000001719373*x^4 + 2.027115e-9*x^5




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


# Case 1 : Dataset 1

ISO<-100
Exposure<-1/125
par(mfrow = c(3,1))
x <- Luminance_1*ISO*Exposure
plot(x,Y_1, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y<- 40.8189*x^0.3495858
points(x,pixel_intensity_y, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )



# Case 2 : Dataset 2

ISO_2<-100
Exposure_2<-1/125
x_2 <- Luminance_2*ISO_2*Exposure_2
plot(x_2,Y_2, xlab = "X_2 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y_2<- (40.8189*x_2^0.3495858) * 1.5
points(x_2,pixel_intensity_y_2, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )



# Case 2 : Dataset 3

ISO_3<-100
Exposure_3<-1/125
x_3 <- Luminance_2*ISO_3*Exposure_3
plot(x_3,Y_3, xlab = "X_3 (Lux*ISO*Exposure)",ylab = "Y_2 (pixel)", col = "red") #, type="l", lty=2, lwd=3, col )
title("Mapping Luminance to Luma Pixel intensity ")
pixel_intensity_y_3<- (40.8189*x_2^0.3495858) * 1.5
points(x_3,pixel_intensity_y_3, xlab = "X (Lux*ISO*Exposure)",ylab = "Y (pixel)",col = "blue") #, type="l", lty=2, lwd=3 )




# Case 2 : Dataset 3