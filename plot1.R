##Create data folder and download appropriate files
if(!file.exists("data")) {dir.create("data")}

file_URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(file_URL, destfile = "./data/PM2.5.zip", method = "curl")
unzip("./data/PM2.5.zip")

## Assign data to appropriate variables
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Install necessary packages
install.packages("dplyr")
library(dplyr)

## Separate data into independent data frames based on year
OneK99 <- filter(NEI, year == "1999")
TwoK02 <- filter(NEI, year =="2002")
TwoK05 <- filter(NEI, year == "2005")
TwoK08 <- filter(NEI, year == "2008")

## Calculate total emissions for each year and generate regression equation
totals <- c(sum(OneK99$Emissions),sum(TwoK02$Emissions),sum(TwoK05$Emissions),
            sum(TwoK08$Emissions))/1000000
years <- c("1999","2002","2005","2008")
model <- lm(totals ~ as.numeric(years))

## Plot data and add regression line
plot(years, totals, pch = 4, 
     xlim = c(1998, 2010),
     ylim = c(0,10),
     xlab = "Year", 
     ylab ="Total PM2.5 Emissions [megatonnes]", 
     lwd = 3,
     main = "Total PM2.5 Emissions in the USA for 1999, 2002, 2005 & 2008")

abline(model, col = 'red', lwd = 1, lty = 2)
legend("topright", 
       legend = "y = 792.6711 - 0.3929x",
       lty = 2,
       col = "red",
       title = "Linear Regression Equation")

dev.copy(png, file = "plot1.png", width = 480, height = 480)
dev.off()


 
