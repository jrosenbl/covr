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
suppressMessages(library(xts))

fixNames <- function(nmList) {
    nmList <- tolower(nmList)
    nmList <- gsub("[/ ]", "_", nmList)
    return(nmList)
}

convDate <- function(dateVec) {
    #' So far, only one report file used m/d/y. The rest were Y-m-d.
    if (grepl('/', dateVec[1])) {
        return(as.Date(dateVec, "%m/%d/%y"))
    }
    return(as.Date(dateVec, "%Y-%m-%d"))
}
#'----------------------------------------
#' customize these constants.
kDataRoot <- "~/data/covid-19/COVID-19"
kDataDir <- "csse_covid_19_data/csse_covid_19_daily_reports"
kDefaultTargetState = 'California'

targetState <- kDefaultTargetState
args = commandArgs(trailingOnly=T)
if (length(args) == 1) {
    targetState <- args[1]
} else if (length(args) > 1) {
    stop("only state can be specified")
}

inDir <- file.path(kDataRoot, kDataDir)
setwd(inDir)
reports <- list.files(pattern="*.csv")
if (length(reports) == 0) {
    stop("no reports found")
}
latestReport <- reports[length(reports)]
targetReport <- latestReport

inPath <- file.path(kDataRoot, kDataDir, targetReport)
if (!file.exists(inPath)) {
    stop(inPath,' not found')
}
#'
#' convert report list to an xts object for easy time indexing
rd = sapply(strsplit(reports, "\\."), function(x) x[[1]])
rdi = xts(reports, order.by = as.Date(rd, "%m-%d-%Y"))
startDate = '2020-03-01'
dateRange = paste0(startDate, "/")
combDtl = data.table()
for (r in rdi[dateRange]) {
    dt = fread(r)
    names(dt) = fixNames(names(dt))
    us <- dt[country_region=="US", .(province_state,
                                     confirmed,
                                     deaths,
                                     recovered,
                                     last_update)]
    us[,report_date := convDate(last_update)]
    us[,report := r]    
    dtl <- data.table(gather(us, key=type, value=count, names(us)[2:4]))
    combDtl <- rbind(combDtl, dtl)
}

ct <- xtabs(count ~ report_date+type, data=combDtl[province_state==targetState])
ct <- data.table(date=as.Date(row.names(ct)), c = ct[,1], d=ct[,2], r=ct[,3])
ctdts <- xts(ct$d, order.by=ct$date)
ctcts <- xts(ct$c, order.by=ct$date)
dr = round(ct$d * 100 / ct$c,3)
ct[,dr_pct := dr]
ct[order(-c)]

cs <- colSums(ct[,2:4])
fdate <- ct[1, date]
ldate <- ct[nrow(ct), date]
ldate - fdate
cat(targetState,"totals for", fdate, "to", ldate, "(", ldate-fdate, "days)\n")
for (t in names(cs)) {
    cat(sprintf("%s   %9s\n",t,format(cs[t],big.mark=",")))
}
cat(sprintf("d/c %9s%%\n", round(cs["d"] * 100 / cs["c"], 3)))
cat(sprintf("r/c %9s%%\n", round(cs["r"] * 100 / cs["c"], 3)))
