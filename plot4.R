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

## Filter SCC to only include sources that mention combustion and coal

combustion <- SCC[grep("Combustion", SCC$SCC.Level.One),]
coal_and_combustion <- combustion[grep("Coal", combustion$EI.Sector),]

## filter data based on indices found and assemble summary table by year
index <- coal_and_combustion[,1]
edited_data_2 <- filter(NEI, SCC %in% index)
summary <- edited_data %>%
        group_by(year) %>%
        summarise(total = sum(Emissions))

## assemble plot
model <- lm(total ~ year, data = summary)
plot(summary$year, summary$total, 
     pch = 4,
     lty = 2,
     lwd = 2,
     xlab = "Year",
     ylab = "PM2.5 Emissions [tonnes]",
     main = "PM2.5 Emissions from Coal Combustion sources in the USA (1999-2008)",
     xlim = c(1996,2010),
     ylim = c(0,7000))
abline(model, lty = 2, col = "red", lwd = 1)
legend("topright", 
       legend = "423216.9 - 209.7x",
       lty = 2, 
       col = "red",
       title = "Linear Regression Equation")

## create png
dev.copy(png, file = "plot4.png", width = 650, height = 650)
dev.off()