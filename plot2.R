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

## Filter out data exclusive to Baltimore City, Maryland
baltimore <- filter(NEI, fips == "24510")

## Filter data by appropriate year
baltimore_1999 <- filter(baltimore, year == "1999")
baltimore_2002 <- filter(baltimore, year == "2002")
baltimore_2005 <- filter(baltimore, year == "2005")
baltimore_2008 <- filter(baltimore, year == "2008")

## Sum total emissions for each year
baltimore_totals <- c(sum(baltimore_1999$Emissions),
                      sum(baltimore_2002$Emissions),
                      sum(baltimore_2005$Emissions),
                      sum(baltimore_2008$Emissions))

## Create years vector
years <- c(1999,2002,2005,2008)

## Assemble plot and fit linear regression equation
model <- lm(baltimore_totals ~ as.numeric(years))

plot(years, baltimore_totals, pch = 4, 
     xlim = c(1996,2010),
     ylim = c(0,5000),
     xlab = "Year",
     ylab = "Total PM2.5 Emissions [tonnes]",
     lwd = 3,
     main = "Total PM2.5 Emissions for Baltimore, Maryland in 1999, 2002, 2005 and 2008")
abline(model, lty = 2, lwd = 1, col = "red")
legend("topright", 
       legend = "y = 242974.0 - 119.9x",
       lty = 2,
       col = "red",
       title = "Linear Regression Equation")

## Create PNG
dev.copy(png, file = "plot2.png", width = 600, height = 600)
dev.off()



