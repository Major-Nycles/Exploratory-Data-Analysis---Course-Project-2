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

## Filter out data exclusive to Baltimore City, Maryland & LA County, Calif
baltimore <- filter(NEI, fips == "24510")
los_angeles <- filter(NEI, fips == "06037")

## Filter mobile and road relevant codes. This should capture motor vehicle data.
SCC_mobile <- SCC[grep("Mobile", SCC$EI.Sector),]
SCC_mobile <- SCC_mobile[grep("Road", SCC_mobile$EI.Sector),]
index = SCC_mobile[,1]

## Filter data to only include motor vehicles
baltimore_vehicles <- filter(baltimore, SCC %in% index)
los_angeles_vehicles <- filter(los_angeles, SCC %in% index)

## Summarise by year and sum total emissions per year
summary_baltimore <- baltimore_vehicles %>%
        group_by(year) %>%
        summarise(total = sum(Emissions))

summary_LA <- los_angeles_vehicles %>%
        group_by(year) %>%
        summarise(total = sum(Emissions))

## Add city ident and combine into final data frame
summary_baltimore[,3] = "Baltimore"
summary_LA[,3] = "Los Angeles"
summary_both = rbind(summary_baltimore, summary_LA)
names(summary_both)[3] = "city"

## Build plot
qplot(year, total, data = summary_both, facets = .~city, 
      geom = c("point","smooth"), method = lm, xlab = "Year",
      ylab = "PM2.5 Emissions [tonnes]", 
      main = "PM2.5 Emissions for Baltimore, MA and Los Angeles, CA (1999-2008)")

## create png
dev.copy(png, file = "plot6.png", width = 650, height = 650)
dev.off()