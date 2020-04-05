#!/usr/local/bin/Rscript
#' combines (rbinds) all of the state.csv files written by covah.R
#' into one large table
#' try using this to generate the state cvs files:
#' perl -ane '`~/bin/cov_state_ts.R $_`; print $_' ~/data/state.short
#' 
rm(list=ls())
suppressMessages(library(data.table))
suppressMessages(library(lubridate))
options(width=200)

kInputFile <- "states"
kInputDir <- "~/data"
kDataDir <- "~/data/cova"
kOutputDir <- kInputDir
kOutputFile <- 'all_states.csv'

inPath <- file.path(kInputDir, kInputFile)

statedf <- read.table(inPath, header=F)
names(statedf) = c('name')
statedf$name = gsub(' ','_',statedf$name)
combdt = data.table()
for (s in statedf$name) {
    inPath = file.path(kDataDir, paste0(s,".csv"))
    cat(inPath,"\n")
    dt = fread(inPath)
    combdt = rbind(combdt, dt)
}
combdt$report_date = as.Date(combdt$report_date)
outPath = file.path(kOutputDir, kOutputFile)
write.csv(combdt, file=outPath, row.names=F, quote=F)
cat(nrow(combdt), "rows saved in", outPath,"\n")
## combdt[date==last(date),.(state,dDeltaPctRollMean5d)][order(dDeltaPctRollMean5d)]

endPeriod = last(combdt$report_date)
startPeriod = endPeriod - 5
xt <- xtabs(dDeltaPctRollMean5d ~ state+report_date, data=combdt[report_date > startPeriod])
xt
