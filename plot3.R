## NB: IF YOU RUN THIS CODE WITH THE source() COMMAND
## TO GET THE FIGURE TO OUTPUT CORRECTLY TO PNG USE:
## source('plot3.R', print.eval=TRUE)

# call ggplot2 library into environment
library(ggplot2)

# retrieve file if it doesn't exist in current working directory
file.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if (!file.exists("summarySCC_PM25.rds")) {
    if (.Platform$OS.type == "unix") {
        download.file(file.url, "epa_data.zip", method="curl")
    } else {
        download.file(file.url, "epa_data.zip")
    }
    unzip("epa_data.zip")
}

# Read R data file
pm25 <- readRDS("summarySCC_PM25.rds")

# filter to just Baltimore City, fips == 24510
pm25 <- pm25[pm25$fips=="24510",]

# totals, Baltimore, by year, by type - Q3
# aggregate sums by year and type
sums <- aggregate(pm25$Emissions,
               by=list(pm25$year, pm25$type),
               FUN=sum
               )
# change names of sums, to something legible
# instead of c('Group.1', 'Group.2', 'x')
names(sums) <- c("year", "type", "Emissions")

# output figure to png
png("plot3.png", height=1000, width=750, type="cairo")

# plot multi-faceted plot, add aesthetics, labels, and theme.
qplot(year, Emissions, data=sums, geom="bar", stat="identity") +
    aes(fill=year)+facet_grid(type~.) +
    labs(title="Total Emissions by Type (tons PM2.5)
         Baltimore City, MD") +
    labs(y="Emissions (tons)") +
    theme_bw()

# close graphical device
dev.off()
