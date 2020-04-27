#!/usr/local/bin/Rscript
#'
#' COVID-19 confirmed, deaths, recovered counts plus percent change in deaths
#' over time for a specified state and also for the US in total.
#'
#' This program shows the confirmed, deaths and recovered counts along
#' with daily deaths delta, percent change and rolling mean (5-day) of
#' percent change for the specified state and the US. It saves the
#' state statistics in a file named <state>.csv.  
#'
#' See constants section for customizable file paths, parameters and
#' defaults.
#' 
#' usage: cov_state_ts.R [state]
#'   quote the state if it contains a space like "New York".
#'   capitalize appropriately.#'
#' 
#' source data:
#' https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#' see also interactive map https://coronavirus.jhu.edu/map.html .
#' 
#' regarding last_update: So far, only one report file used m/d/y. The rest were Y-m-d.
rm(list=ls())
suppressMessages(library(data.table))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(htmlwidgets))
suppressMessages(library(xts))
#' ---------------
#' customize these constants.
kDataRoot <- "~/data/covid-19/COVID-19"
kDataDir <- "csse_covid_19_data/csse_covid_19_daily_reports"
kDefaultTargetState = 'New York'
kOutputDir <- "~/data/covid19"
kUSTotalOutput <- "us_totals.csv"
kStartDate <- "2020-03-15"
kRollMeanDays <- 5
kGraphDev <- "png"
kStatePop <- "~/data/nst-est2019-alldata.csv"

#' ---------------
#' 
fixNames <- function(nmList) {
    nmList <- tolower(nmList)
    nmList <- gsub("[/ ]", "_", nmList)
    return(nmList)
}


convDate <- function(dateVec) {
    return(as.Date(gsub('.csv','',dateVec), "%m-%d-%Y"))
}

#' xts lag not working. not sure why. wrote my own.
myLag <- function(v) {
    return(c(NA,v[1:length(v)-1]))
}

getStatePop <- function(targetState){
    stPop <- fread(kStatePop)
    names(stPop) <- tolower(names(stPop))
    stPop <- stPop[,.(name, popestimate2019)]
    setkey(stPop, name)
    pop <- stPop[targetState]$popestimate2019
    return(pop)
}
                       
                           
plotData <- function(dt, targetState) {
    p = ggplot(dt, aes(report_date, d)) +
        geom_bar(stat='identity') +
        geom_line(aes(report_date, dDelta), color='orange') +
        ylab('deaths') +
        ggtitle(paste(targetState, format(st[nrow(st), report_date], "%Y-%m-%d")))
    targetState = gsub(" ","_", tolower(targetState))
    s = paste0(targetState, ".", kGraphDev)
    s <- file.path(kOutputDir, s)
    suppressWarnings(ggsave(plot=p, file=s, device=kGraphDev))
    cat("plot saved in", s, "\n")
    return(p)
}

#'-- MAIN -----------------------
targetState <- kDefaultTargetState
args = commandArgs(trailingOnly=T)
if (length(args) == 1) {
    targetState <- args[1]
} else if (length(args) > 1) {
    stop("only state can be specified")
}

inDir <- file.path(kDataRoot, kDataDir)
setwd(inDir)
reports <- list.files(pattern="^[0-9]{2}-[0-9]{2}-[0-9]{4}.*.csv")
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
startDate = kStartDate
dateRange = paste0(startDate, "/")
combDt = data.table()
for (r in rdi[dateRange]) {
    dt = fread(r)
    names(dt) = fixNames(names(dt))
    us <- dt[country_region=="US", .(province_state,
                                     confirmed,
                                     deaths,
                                     recovered,
                                     last_update)]
    us[,report_date := convDate(r)]
    us[,report := r]
    combDt <- rbind(combDt, us)
}
#' get target state population
stPop <- getStatePop(targetState)

#' state totals over time
st <- combDt[province_state == targetState, .(r=sum(recovered), c=sum(confirmed),d=sum(deaths)), by=report_date]

#' state deaths delta and percent change
dDelta <- c(NA, diff(st$d, lag=1))
st[, dDelta:=dDelta]
dDeltaPct <- round(dDelta * 100 / myLag(st$d), 3)
st[,dDeltaPct:=dDeltaPct]

#' state deaths 5-day rolling mean of percent change
dRollMean <- rollmean(dDeltaPct, kRollMeanDays, align="right", fill=NA)
st[,dDeltaPctRollMean5d := dRollMean]
cat("\n",targetState,"\n")
st

#' US totals over time
usTot <- combDt[report_date > startDate,.(r=sum(recovered), c=sum(confirmed), d=sum(deaths)), by=report_date]

#' US deaths delta and percent change
delta <- c(NA, diff(usTot$d, lag=1))
usTot[,dDelta:=delta]
dDeltaPct <- round(delta * 100 / myLag(usTot$d), 3)
usTot[,dDeltaPct:=dDeltaPct]

#' US deaths 5-day rolling mean of percent change
dRollMean <- rollmean(dDeltaPct, 5, align="right", fill=NA)
usTot[,dDeltaPctRollMean5d := dRollMean]
cat("\nUS\n")
usTot

#' save xtab
s = gsub(" ", "_", targetState)
st[,state:=s]

s = file.path(kOutputDir, paste0(tolower(s),".csv"))
write.csv(st, file=s, quote=FALSE, row.names=FALSE)
cat(nrow(st), "lines saved in",s,"\n")
s = file.path(kOutputDir, kUSTotalOutput)
write.csv(usTot, file=s, quote=FALSE, row.names=FALSE)
cat(nrow(usTot), "lines saved in", s, "\n")

#' plot state deaths and 
#' plot state deaths and daily delta
pState <- plotData(st, targetState)
s <- file.path(kOutputDir,paste0(gsub(" ","", tolower(targetState)),".html"))
suppressWarnings(saveWidget(ggplotly(pState), s, selfcontained=T))
cat("interactive plot saved in", s, "\n")

pUS <- plotData(usTot, "US")
s <- file.path(kOutputDir,paste0(gsub(" ","", "us"),".html"))
suppressWarnings(saveWidget(ggplotly(pUS), s, selfcontained=T))
cat("interactive plot saved in", s, "\n")
cat(targetState,'state population US Census 2019 Estimate', format(stPop,big.mark=','), "\n")
cat('deaths per 100K', st[nrow(st)]$d/(stPop/1e5),'\n')
