# FINAL EDA

#Load Libraries
library(ggplot2)
library(skimr)
library(GGally)
library(RColorBrewer)

#Load data set - make not present because it was removed from the modelling
cars_eda = read.csv(file="/Users/benkight/OneDrive - Durham University/Masters/Dissertation/Python - Scraping/Engineered Dataset.csv", sep=",")[,-c(1:2)]
View(cars_eda) 

#Convert to factors
cars_eda$Indicated.Value <- as.factor(cars_eda$Indicated.Value)
cars_eda$Transmission <- as.factor(cars_eda$Transmission)
cars_eda$Fuel.Type <- as.factor(cars_eda$Fuel.Type)

#Split dataset into categorical or continuous
cars_continuous <- cars_eda[,c(1:3,5,7)]

#GGpairs plot of continuous variables
ggpairs(cars_continuous, aes(alpha =0.1))
pairs(cars_continuous)

#Density Plots of above
par(mfrow = c(2,2))
d_mmp <- density(cars_eda$Mean.Model.Price....)
d_year <- density(cars_eda$Year)
d_mileage <- density(cars_eda$Mileage)
d_eng <- density(cars_eda$Engine.Size..L.)
d_acp <- density(cars_eda$Actual.Car.Price....)
plot(d_mmp, main = "Mean Model Price Density Plot", xlab = "Price (£)", col="navy", cex.main = 1.5, cex.axis = 1.5, cex.lab=1.5)
plot(d_mileage, main = "Mileage Density Plot", xlab= "Mileage", col="navy", cex.main = 1.5, cex.axis = 1.5, cex.lab=1.5)
plot(d_eng, main = "Engine Size Density Plot", xlab = "Engine Size (L)", col="navy", cex.main = 1.5, cex.axis = 1.5, cex.lab=1.5)
plot(d_acp, main = "Actual Car Price Density Plot", xlab = "Actual Car Price (£)", col="navy", cex.main = 1.5, cex.axis = 1.5, cex.lab=1.5)

par(mfrow = c(2,1))
plot(d_year, main = "Year Density Plot", xlab = "Year", col = "navy")
years <- table(cars_eda$Year)
barplot(years, main = "Barplot of Year", xlab = "Year", ylab = "Quantity of Cars" , col="navy", ylim = c(0,1000))

#Individual plots from pairs
plot(cars_continuous[,1],cars_continuous[,5], pch=20, cex = 0.75, col = "navy", main = "Actual Car Price vs Mean Model Price", ylab = "Actual Price (£)", xlab ="Mean Model Price (£)")
plot(cars_continuous[,2],cars_continuous[,5], pch=20, cex=0.75, col = "navy", main ="Actual Car Price vs Year", ylab = "Actual Price (£)", xlab = "Year")
plot(cars_continuous[,3],cars_continuous[,5], pch=20, cex=0.75, col = "navy", main ="Actual Car Price vs Mileage", ylab = "Actual Price (£)", xlab = "Mileage", xlim = c(0,500000))
plot(cars_continuous[,2],cars_continuous[,3], pch=20, cex=0.75, col = "navy", main ="Mileage vs Year", ylab = "Mileage", xlab = "Year")
plot(cars_continuous[,2],cars_continuous[,4], pch=20, cex=0.75, col = "navy", main ="Engine Size vs Year", ylab = "Engine Size (L)", xlab = "Year")
plot(cars_continuous[,4],cars_continuous[,5], pch=20, cex=0.75, col = "navy", main ="Actual Car Price vs Engine Size", ylab = "Actual Price (£)", xlab = "Engine Size (L)") #not as correlated as we thought it would be
plot(cars_continuous[,3],cars_continuous[,4], pch=20, cex=0.75, col = "navy", main ="Engine Size vs Mileage", ylab = "Engine Size (L)", xlab = "Mileage", xlim = c(0,500000))

# Categorical
#indicated values
par(mfrow = c(1,1))
i_value <- table(cars_eda$Indicated.Value)
x <- barplot(rev(i_value), col=rev(brewer.pal(5, "RdYlGn")), ylim = c(0,5000),main = "Indicated Values", ylab = "Quantity of Cars", xlab = "Indicated Value")
y <- as.matrix(i_value)
text(x,rev(y+150), labels = as.character(rev(y)))

#Transmission
transmission <- table(cars_eda$Transmission)
x <- barplot(transmission, main = "Transmission Types", ylab="Quantity of Cars", xlab = "Transmission",col=rev(topo.colors(2)), ylim = c(0,14000))
y <- as.matrix(table(cars_eda$Transmission))
text(x,y+350, labels = as.character(y))

#Fuel Type
fuel <- table(cars_eda$Fuel.Type)
x <- barplot(fuel,main = "Fuel Types", ylab = "Quantity of Cars", xlab= "Fuel Type", col = brewer.pal(4, "Set1"), ylim = c(0,10000))
y <-  as.matrix(table(cars_eda$Fuel.Type))
text(x,y+250, labels = as.character(y))


# Categorical vs Continuous Variables
#all indicated value plots
ordered_iv <- ordered(cars_eda$Indicated.Value, levels = c("Lower", "Great", "Good", "Fair", "Higher")) #reorder so in ascending
#ACP vs IV
boxplot(Actual.Car.Price....~ordered_iv, data=cars_eda, col=rev(brewer.pal(5, "RdYlGn")), main = "Actual Price by Indicated Value", ylab = "Actual Price (£)", xlab = "Indicated Value")
#year by indicated value
boxplot(Year~ordered_iv, data=cars_eda, col=rev(brewer.pal(5, "RdYlGn")), main = "Indicated Value by Year", ylab = "Year", xlab = "Indicated Value")
#Indicated Value by Year
cars_eda$Indicated.Value <- factor(cars_eda$Indicated.Value, levels = c("Higher", "Fair", "Good", "Great", "Lower"))
plot(Indicated.Value ~ Year, cars_eda, col=rev(brewer.pal(5, "RdYlGn")), main="Indicated Values vs Year", ylab = "Indicated Values", xlab = "Year")
#engine size by indicated value
par(mfrow=c(1,1))
boxplot(Engine.Size..L.~ordered_iv, data=cars_eda, col=rev(brewer.pal(5, "RdYlGn")), main = "Engine Size by Indicated Value", xlab = "Indicated Value", ylab="Engine Size (L)")
#mileage by indicated value
par(mfrow=c(1,2))
boxplot(Mileage~ordered_iv, data=cars_eda, col=rev(brewer.pal(5, "RdYlGn")), main = "Mileage vs Indicated Values", ylab = "Mileage", xlab = "Indicated Value")
#^^^above without outlier&&&
boxplot(Mileage~ordered_iv, data=cars_eda, col=rev(brewer.pal(5, "RdYlGn")), main = "Mileage vs Indicated Values", ylab = "Mileage", xlab = "Indicated Value", plot=F)$out #find the outlier
boxplot(Mileage[-c(9339)]~ordered_iv[-c(9339)], data=cars_eda, col=rev(brewer.pal(5, "RdYlGn")), main = "Mileage (no major outlier) vs Indicated Values", ylab = "Mileage", xlab = "Indicated Value")

#actual car price plots
boxplot(Actual.Car.Price....~Transmission, data=cars_eda, col=rev(topo.colors(2)), main = "Actual Price by Transmission", ylab = "Actual Price (£)", xlab = "Transmission") #proves what was said in the lit review
boxplot(Actual.Car.Price....~Fuel.Type, data=cars_eda, col = brewer.pal(4, "Set1"), main= "Actual Price by Fuel Type", ylab = "Actual Price (£)", xlab = "Fuel Type")

#Transmission by Year
plot(cars_eda$Transmission ~ cars_eda$Year, cars_eda, col=topo.colors(2), main="Transmission vs Year", ylab = "Transmission", xlab = "Year")

#rename columns and provide head and str of data set
names(cars_eda)[names(cars_eda) == "Mean.Model.Price...."] <- "Mean.Model.Price"
names(cars_eda)[names(cars_eda) == "Engine.Size..L."] <- "Engine.Size"
names(cars_eda)[names(cars_eda) == "Actual.Car.Price...."] <- "Actual.Price"

head(cars_eda)
str(cars_eda)



