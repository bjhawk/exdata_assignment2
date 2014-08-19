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

# calculate yearly total emissions for Baltimore City, MD
# fips == 24510
pm25 <- pm25[pm25$fips=="24510",]
yearly.totals <- sapply(split(pm25, pm25$year),
                        function(year){sum(year$Emissions)}
)
#output chart to PNG
png("plot2.png", height=500, width=500, type="cairo")

# Plot barplot, no axes or labels. Barplot natively returns the x-coordinates
# of the midpoints for each bar - we'll use those to apply labels.
labels.x <- barplot(yearly.totals, axes=F,
                    xlab="Year",
                    main="Total Emissions (tons PM2.5)
                        Baltimore City, MD")

# apply labels - Sum of each year rounded to nearest ton
# use x-axis position from return value of barplot
# Calculate y-axis position of the labels by placing them slightly beneith the
# top of the axis, using the smallest value in the vector.
text(labels.x, # label positions on the x axis
     yearly.totals-(min(yearly.totals)*.25), # label positions on y axis
     labels=round(yearly.totals, 0) # label values
)

# close graphical device
dev.off()
