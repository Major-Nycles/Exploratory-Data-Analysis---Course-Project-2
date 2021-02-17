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

## Filter mobile and road relevant codes. This should capture motor vehicle data.
SCC_mobile <- SCC[grep("Mobile", SCC$EI.Sector),]
SCC_mobile <- SCC_mobile[grep("Road", SCC_mobile$EI.Sector),]
index = SCC_mobile[,1]

## Filter Baltimore data to only include motor vehicles
baltimore_vehicles <- filter(baltimore, SCC %in% index)

## Summarise by year and sum total emissions per year
summary <- baltimore_vehicles %>%
        group_by(year) %>%
        summarise(total = sum(Emissions))

## Plot results and regression line
model <- lm(total~year, data = summary)
plot(summary$year, summary$total,
     pch = 4,
     lwd = 2,
     xlim = c(1996,2010),
     ylim = c(0,500),
     xlab = "Year",
     ylab = "Total PM2.5 Emiisions [tonnes]",
     main = "Total PM2.5 Emissions from Motor Vehicles in Baltimore, Maryland (1999 - 2008)")
abline(model, lwd = 1, lty = 2, col = "red")
legend("topright",
       lty = 2,
       col = "red",
       legend = "53975.71 - 26.82x",
       title = "Linear Regression Equation")

## create png
dev.copy(png, file = "plot5.png", width = 650, height = 650)
dev.off()





