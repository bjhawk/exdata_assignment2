file <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'

if (!file.exists('summarySCC_PM25.rds')) {
    download.file(file, 'epa_data.zip', method='curl')
    unzip('epa_data.zip')
}

scc <- readRDS('Source_Classification_Code.rds')
pm25 <- readRDS('summarySCC_PM25.rds')

pm25 <- transform(pm25, year=as.factor(year))
pm25 <- transform(pm25, type=as.factor(type))

#yearly total emissions - Q1
yearly.totals <- sapply(split(pm25, pm25$year), function(z){sum(z$Emissions)})
labels.x <- barplot(yearly.totals, axes=F, main="Total Emissions (tons PM2.5)")
text(labels.x, yearly.totals-(min(yearly.totals)*.25), labels=round(yearly.totals, 0))

#yearly total emissions, Baltimore City, MD (fips=24510) - Q2
pm25.Baltimore <- pm25[pm25$fips=="24510",]
yearly.totals.Baltimore <- sapply(split(pm25.Baltimore, pm25.Baltimore$year),
                        function(z){sum(z$Emissions)}
                    )
labels.x <- barplot(yearly.totals.Baltimore, axes=F,
                    main="Total Emissions, Baltimore City, MD (tons PM2.5)"
                )
text(labels.x, yearly.totals.Baltimore-(min(yearly.totals.Baltimore)*.25),
                    labels=round(yearly.totals.Baltimore, 0)
                )


#totals, Baltimore, by year, by type - Q3
p <- aggregate(pm25.Baltimore$Emissions, by=list(pm25.Baltimore$year, pm25.Baltimore$type), FUN=sum)
setnames(p, c('year', 'type', 'Emissions')) # instead of c('Group.1', 'Group.2', 'x')
qplot(year, Emissions, data=p, geom='bar', stat='identity') +
    aes(fill=year)+facet_grid(type~.) +
    labs(title="Total Emissions by type, Baltimore City, MD") +
    labs(y='Emissions (tons)') +
    theme_bw()


# use 'Coal' %in% scc$SCC.Level.Three - Q4
coal.codes <- scc['Coal' %in% scc$SCC.Level.Three,'SCC']
yearly.totals.coal <- sapply(split(pm25[pm25$SCC %in% scc.codes,], pm25$year), function(z){sum(z$Emissions)})
labels.x <- barplot(yearly.totals.coal, axes=F, main="Total Emissions From Coal-related Sources (tons PM2.5)")
text(labels.x, yearly.totals.coal-(min(yearly.totals.coal)*.25), labels=round(yearly.totals.coal, 0))

# Q5
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
pm25.b.cars <- pm25[pm25$fips=='24510' & pm25$SCC %in% mv.codes, ]
yearly.totals.cars <- sapply(split(pm25.b.cars, pm25.b.cars$year), function(z){sum(z$Emissions)})
labels.x <- barplot(yearly.totals.cars, axes=F, main='yearly total cars baltimore')
text(labels.x, yearly.totals.cars-(min(yearly.totals.cars)*.25), labels=round(yearly.totals.cars, 0))

#Q6
mv.codes <- append(grep('^22010*', levels(scc$SCC), ignore.case=T, value=T),
                   grep('^22300*', levels(scc$SCC), ignore.case=T, value=T))
pm25.b.cars <- pm25[pm25$fips=='24510' & pm25$SCC %in% mv.codes, ]
pm25.la.cars <- pm25[pm25$fips=='06037' & pm25$SCC %in% mv.codes, ]
yearly.totals.cars.b <- sapply(split(pm25.b.cars, pm25.b.cars$year), function(z){sum(z$Emissions)})
yearly.totals.cars.la <- sapply(split(pm25.la.cars, pm25.la.cars$year), function(z){sum(z$Emissions)})

par(mfcol=c(2,1))
labels.x.b <- barplot(yearly.totals.cars.b, axes=F, main='yearly total cars baltimore')
text(labels.x.b, yearly.totals.cars.b-(min(yearly.totals.cars.b)*.5), labels=round(yearly.totals.cars.b, 0))

labels.x.la <- barplot(yearly.totals.cars.la, axes=F, main='yearly total cars LA')
text(labels.x.la, yearly.totals.cars.la-(min(yearly.totals.cars.la)*.5), labels=round(yearly.totals.cars.la, 0))
