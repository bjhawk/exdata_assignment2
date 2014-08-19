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

###
#     'SCCs starting with 22010 define the light duty gasoline vehicles including
#     motorcycles, with the exception of SCCs starting with 220107, which define
#     the heavy duty gasoline vehicles. SCCs starting with 22300 define the light
#     duty diesel vehicles, with the exception of SCCs starting with 223007 that
#     define the heavy duty diesel vehicles.'
# from the 2008 NEI Technical Support Document v3 - Sept 2013
# http://www.epa.gov/ttn/chief/net/2008neiv3/2008_neiv3_tsd_draft.pdf
###
mv.codes <- append(grep('^22010*', levels(scc$SCC), ignore.case=T, value=T),
                   grep('^22300*', levels(scc$SCC), ignore.case=T, value=T))

# Baltimore Data
pm25.baltimore <- pm25[pm25$fips=='24510' & pm25$SCC %in% mv.codes,]
yearly.totals.baltimore <- sapply(split(pm25.baltimore, pm25.baltimore$year),
                             function(year){sum(year$Emissions)}
)

pm25.la <- pm25[pm25$fips=='06037' & pm25$SCC %in% mv.codes,]
yearly.totals.la <- sapply(split(pm25.la, pm25.la$year),
                                  function(year){sum(year$Emissions)}
)
# save figure as png
png('plot6.png', height=500, width=500, type="cairo")

# Show these two graphs next to each other, vertically
par(mfrow=c(2,1))
# Plot barplot, no axes or labels. Barplot natively returns the x-coordinates
# of the midpoints for each bar - we'll use those to apply labels.
labels.x.baltimore <- barplot(yearly.totals.baltimore,
                    axes=F, xlab="Year",
                    main="Total Emissions (tons PM2.5)
                    Baltimore City, MD - Motor Vehicles")

# apply labels - Sum of each year rounded to nearest ton
# use x-axis position from return value of barplot
# Calculate y-axis position of the labels by placing them slightly beneith the
# top of the axis, using the smallest value in the vector.
text(labels.x.baltimore, # label positions on the x axis
     yearly.totals.baltimore-(min(yearly.totals.baltimore)*.25), # label positions on y axis
     labels=round(yearly.totals.baltimore, 0) # label values
)

# Follow the above procedure with yearly data for LA
labels.x.la <- barplot(yearly.totals.la,
                      axes=F, xlab="Year",
                      main="Total Emissions (tons PM2.5)
                      Los Angeles, CA - Motor Vehicles")

# Follow the above procedure with the labels for LA
text(labels.x.la,
     yearly.totals.la-(min(yearly.totals.la)*.25),
     labels=round(yearly.totals.la, 0)
)

# close graphical device
dev.off()
