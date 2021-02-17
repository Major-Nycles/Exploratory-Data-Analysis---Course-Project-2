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

## Summarise data by summing pollution source for each year and type
baltimore_1999 <- baltimore_1999 %>%
        group_by(type) %>%
        summarise(total = sum(Emissions))
baltimore_2002 <- baltimore_2002 %>%
        group_by(type) %>%
        summarise(total = sum(Emissions))
baltimore_2005 <- baltimore_2005 %>%
        group_by(type) %>%
        summarise(total = sum(Emissions))
baltimore_2008 <- baltimore_2008 %>%
        group_by(type) %>%
        summarise(total = sum(Emissions))

## Install ggplot
install.packages("ggplot2")
library(ggplot2)

## Reassign years to data and assemble singluar tidy data frame

baltimore_1999[,3] = 1999
baltimore_2002[,3] = 2002
baltimore_2005[,3] = 2005
baltimore_2008[,3] = 2008

baltimore_all <-
        bind_rows(baltimore_1999, 
                  baltimore_2002, 
                  baltimore_2005, 
                  baltimore_2008)

names(baltimore_all)[3] = "year"

## Begin plot assembly with ggplot2
g <- ggplot(baltimore_all, aes(year, total))
g <- g + geom_point()  
g <- g + facet_grid(type ~ .) 
g <- g + geom_smooth(method = "lm", se = FALSE, lty = 2, col = "red")
g <- g + ggtitle("Total PM2.5 Emissions for Baltimore 1999 to 2008")
g <- g + xlab("Year")
g <- g + ylab("PM2.5 Emissions [tonnes]")
print(g)

## Create PNG
dev.copy(png, file = "plot3.png", width = 600, height = 600)
dev.off()

