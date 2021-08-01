#install.packages( "dplyr" )      
#install.packages( "pander" )   
#install.packages( "stargazer" )  
#install.packages( "scales" )  
#install.packages("stargazer")

#load the packages
pacman::p_load(data.table, ggplot2,psych,dplyr)
library( "scales" )        
library( "stargazer" )    
library( "dplyr" )         
library( "pander" )        
library( "Wats" )         
library(stargazer)


# load the data
#data including the first intervention
li<- fread("C:/working/MAP PROJECT/DATA/li48894114 - 1st change.csv",
                     data.table = T,
                     stringsAsFactors = F)


#data including the second intervention
li2<- fread("C:/working/MAP PROJECT/DATA/li48894114 - 2nd change.csv",
           data.table = T,
           stringsAsFactors = F)


#plot the cumulative cpa
plot( li$T, li$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 30), xlim=c(0,85),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA" )


# Line marking the interventions
#first intervention --> day 14
abline( v=14, col="firebrick", lty=3 )
text( 14, 30, "1st Trader Intervention", col="firebrick", cex=1, pos=4 )
abline(h=10.15, col="black", lty=3)

#second intervention --> day 42
abline( v=42, col="darkgreen", lty=3 )
text( 42, 30, "2nd Trader Intervention", col="darkgreen", cex=1, pos=4 )
abline(h=7.12, col="black", lty=3)

#add the last day --> day 82
abline( v=81, col="orange", lty=3 )
text(68, 25, "Last Day", col="orange", cex=1, pos=4 )

#abline(h=7.12, col="black", lty=3)


# Add the regression line
ts <- lm( li$Y ~ li$T + li$D + li$P, data=li )
lines( li$T, ts$fitted.values, col="steelblue", lwd=2 )

#run the regression and provide the regression summary
regli = lm ( Y ~ T + D + P, data=li) 

stargazer( regli, 
           type = "html", 
           dep.var.labels = ("Cumulative CPA"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out="models.html")


#find the predicted value at day 15
data1 <- as.data.frame( cbind( T = 15, D = 1, P = 1 )) 

y1 <- predict( regli, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( li$T,li$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

# We add a point showing the cumulative cpa at day 15
points( 15, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 15, y1, labels = "t = 15", pos = 4, cex = 1 )

# Line marking the intervention
abline( v=14, col="red", lty=2 )


#find the predicted value at day 19
data2 <- as.data.frame( cbind( T = 19, D = 1, P = 5 )) 

y2 <- predict( regli, data2 ) 

# We plot our initial observations, the column Y in our dataset
plot( li$T,li$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

# We add a point showing the cumulative cpa at day 15
points(15, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the cumulative cpa at day 19
points(19, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(15, y1, labels = "t = 15", pos = 4, cex = 1)

text(19, y2, labels = "t = 19", pos = 4, cex = 1)

# Line marking the interruption
abline( v=14, col="red", lty=2 )



#find the counterfactual value at day 19
data3 <- as.data.frame(cbind( T= 19, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regli, data3) 

# We plot our initial observations, the column Y in our dataset
plot( li$T,li$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

# We add a  point showing the predicted cumulative cpa at day 19
points(19, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual at day 19
points(19, y3, col = "darkorange2", pch = 19, bg = "red",cex = 2)

# Label for our predicted outcome
text(19, y2, labels = "Y at t = 19", pos = 4, cex = 1)

#Label for the counterfactual 
text(19, y3, labels = "C at t = 19", pos = 4, cex = 1)

# Line marking the intervention
abline( v=13, col="red", lty=2 )
text( 13, 30, "1st Trader Intervention", col="firebrick", cex=1, pos=4 )

# Add the regression line
#ts <- lm( li$Y ~ li$T + li$D + li$p, data=li )
#lines( li$T, ts$fitted.values, col="steelblue", lwd=2 )

#find all predictedand counterfactual values at any point
pred1 <- predict(regli, li) 

# To estimate all predicted values of Y, we just use our dataset
datanew <- as.data.frame(cbind(T = rep(1 : 81), D = rep(0), P = rep(0))) 

# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.
pred2 <- predict(regli, datanew) 
# Predict the counterfactuals

plot( li$T,li$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

#plot the lines
lines( rep(1:14), pred1[1:14], col="dodgerblue4", lwd = 3 )
lines( rep(14:81), pred1[14:81], col="dodgerblue4", lwd = 3 )
lines( rep(14:81), pred2[14:81], col="darkorange2", lwd = 3, lty = 5 ) 

#anottate the lines
text(0, 17, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue4")
text(22, 27, labels = "Counterfactual values", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=13, col="darkorange2", lty=2 )
text( 14, 30, "1st Trader Intervention", col="firebrick", cex=1, pos=4 )





#SECOND INTERVENTION

plot( li2$T, li2$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 30), xlim=c(0,60),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA" )


# Line marking the interruption
abline( v=42, col="darkgreen", lty=3 )
text( 42, 30, "2nd Trader Intervention", col="darkgreen", cex=1, pos=4 )


#abline( v=42, col="darkgreen", lty=3 )
#text( 42, 30, "2nd Trader Intervention", col="darkgreen", cex=1, pos=4 )

# Add the regression line
ts2 <- lm( li2$Y ~ li2$T + li2$D + li2$P, data=li2 )
lines( li2$T, ts2$fitted.values, col="brown", lwd=2 )


#second regression equation
regli2 = lm ( Y ~ T + D + P, data=li2)  # Our time series model

stargazer( regli2, 
           type = "html", 
           dep.var.labels = ("Cumulative CPA"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out="models.html")


# find the predicted value at day 43
data1 <- as.data.frame( cbind( T = 43, D = 1, P = 1 )) 


y1 <- predict( regli2, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( li2$T,li2$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

# at the predicted value
points( 43, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 43, y1, labels = "t = 43", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=42, col="red", lty=2 )


#find the predicted value at day 47
data2 <- as.data.frame( cbind( T = 47, D = 1, P = 5 )) # New data

y2 <- predict( regli2, data2 ) 

# We plot our initial observations, the column Y in our dataset
plot( li2$T,li2$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

# plot the predicted value at day 43
points(43, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# plot the predicted value at day 47
points(47, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(43, y1, labels = "t = 43", pos = 4, cex = 1)

text(47, y2, labels = "t = 47", pos = 4, cex = 1)

# Line marking the interruption
abline( v=42, col="red", lty=2 )



#find the counterfactual value at day 47
data3 <- as.data.frame(cbind( T= 47, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regli2, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( li2$T,li2$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

#plot the predicted value at day 47
points(47, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# plot the counterfactual value at day 47
points(47, y3, col = "darkorange2", pch = 19, bg = "red",cex = 2)

# Label for our predicted outcome
text(47, y2, labels = "Y at t = 19", pos = 4, cex = 1)

#Label for the counterfactual 
text(19, y3, labels = "C at t = 19", pos = 4, cex = 1)

# Line marking the interruption
abline( v=42, col="darkgreen", lty=2 )
text(42, 30, "2nd Trader Intervention", col="darkgreen", cex=1, pos=4 )

# Add the regression line
#ts <- lm( li$Y ~ li$T + li$D + li$p, data=li )
#lines( li$T, ts$fitted.values, col="steelblue", lwd=2 )


#calculate predicted and counterfactual values at any point in time
pred1 <- predict(regli2, li2) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 81), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regli2, datanew) 
# Predict the counterfactuals

plot( li2$T,li2$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 60), 
      ylim = c(0, 30),
      xlab = "Time (days)", 
      ylab = "Cumulative CPA")

#plot the lines
lines( rep(1:42), pred1[1:42], col="brown", lwd = 3 )
lines( rep(42:81), pred1[42:81], col="brown", lwd = 3 )
lines( rep(42:81), pred2[42:81], col="darkorange2", lwd = 3, lty = 5 ) 

#anottated
text(0, 17, labels = "Predicted values", pos = 4, cex = 1, col = "brown")
text(43,2, labels = "Counterfactual values", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=42, col="darkgreen", lty=2 )
text( 42, 30, "2nd Trader Intervention", col="darkgreen", cex=1, pos=4 )

