#!/usr/local/bin/Rscript

#' read latest csse_covid_19 daily report csv and print counts for
#' confirmed, deaths and recovered. 
#' 
#' based on data here:
#' https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#' see also interactive map https://coronavirus.jhu.edu/map.html .
#' 
rm(list=ls())
suppressMessages(library(data.table))
suppressMessages(library(tidyr))

#' customize these constants.
kDataRoot <- "~/data/covid-19/COVID-19"
kDataDir <- "csse_covid_19_data/csse_covid_19_daily_reports"
kTop <- 25

inDir <- file.path(kDataRoot, kDataDir)
setwd(inDir)
reports <- list.files(pattern="*.csv")
if (length(reports) == 0) {
    stop("no reports found")
}
latestReport <- reports[length(reports)]
targetReport <- latestReport

args <- commandArgs(trailingOnly=T)
if (length(args) > 0 && grepl('[0-9]{8}', args[1])) {
    d = as.Date(args[1], "%Y%m%d")
    if (is.na(d)) {
        stop("specify date as YYYYMMDD")
    }
    reportDate <- paste0(format(d, "%m-%d-%Y"), ".csv")
    if (!reportDate %in% reports) {
        stop(reportDate," not found")
    }
    targetReport = reportDate
}

inPath <- file.path(kDataRoot, kDataDir, targetReport)
if (!file.exists(inPath)) {
    stop(inPath,' not found')
}

dt = fread(inPath)
names(dt) = tolower(names(dt))
dtl <- data.table(gather(dt, key=type, value=count, names(dt)[8:10]))
ct <- xtabs(count ~ province_state+type, data=dtl[country_region=='US'])
ct <- data.table(state=row.names(ct), c = ct[,1], d=ct[,2], r=ct[,3])
dr = round(ct$d * 100 / ct$c,3)
ct[,dr_pct := dr]

cat("top",kTop,"\n")
ct[order(-c)][1:kTop]

cs <- colSums(ct[,2:4])
cat("US totals for", sub(".csv", "", targetReport), "\n")
for (t in names(cs)) {
    cat(sprintf("%s   %9s\n",t,format(cs[t],big.mark=",")))
}
cat(sprintf("d/c %9s%%\n", round(cs["d"] * 100 / cs["c"], 3)))
cat(sprintf("r/c %9s%%\n", round(cs["r"] * 100 / cs["c"], 3)))
