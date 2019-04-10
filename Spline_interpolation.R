
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




