# retrieve file if it doesn't exist in current working directory
file.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if (!file.exists("summarySCC_PM25.rds"
                 | !file.exists("Source_Classification_Code.rds")) {
    if (.Platform$OS.type == "unix") {
        download.file(file.url, "epa_data.zip", method="curl")
    } else {
        download.file(file.url, "epa_data.zip")
    }
    unzip("epa_data.zip")
}

# Read R data files
pm25 <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

#filter to just EI Sectors related to coal combustion
coal.sectors <- grep("coal", scc$EI.Sector, ignore.case=T, value=T)
scc <- scc[scc$EI.Sector %in% coal.sectors,]

# calculate yearly total emissions from coal combustion sources
pm25 <- pm25[pm25$SCC %in% scc[,'SCC'],]
yearly.totals <- sapply(split(pm25, pm25$year),
                        function(year){sum(year$Emissions)}
)
#output chart to PNG
png("plot4.png", height=500, width=500, type="cairo")

# Plot barplot, no axes or labels. Barplot natively returns the x-coordinates
# of the midpoints for each bar - we'll use those to apply labels.
labels.x <- barplot(yearly.totals, axes=F,
                    xlab="Year",
                    main="Total Emissions (tons PM2.5),
                            US - Coal Burning Sources")

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
