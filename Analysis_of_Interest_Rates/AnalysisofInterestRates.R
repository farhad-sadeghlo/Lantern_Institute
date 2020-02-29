
# =========================== issues =========================
# ============================= 1 ============================
# We can resolve the issue with NaN values by just removing the
# corresponding row to each NaN value in a column or we can consider
# interpolation of the two closest data points
# ============================================================

rm(list=ls())

library(dplyr)

# ====== Wanna change the tenur names? Please run this part =======
LIBORxdata.f <- function(){
  library(quantmod)
  nn <- readline(prompt = "how many fred data would you like to receive?")
  LIBORxdata <- list()
    for (i in 1:nn){
      print(paste("what currency data are you looking for in USD or CAD for data number?", i))
      currency <- readline(prompt = )
      print(paste("In what period are you looking for in 1M, 2M - 12M for data number?", i))
      period <- readline(prompt = )
      if (i < 10 || (i > 12 && i<22)){
        LIBORdata = paste(currency, period, "TD156N", sep = "")
      }
      else if (i == 10||11||12||22||23||24){
        LIBORdata = paste(currency, period, "D156N", sep = "")
      }
      LIBORxdata[[LIBORdata]] <- LIBORdata
      LIBORxdataf <- getSymbols.FRED(LIBORxdata, env = globalenv())
    }
  return(LIBORxdata)
}

LIBORnames <- LIBORxdata.f()

names(LIBORnames)

# In case we want to prevent making the tenur names again 
# we can save the names of datasets as text file and donwload 
# them each time we want to study
# capture.output(names(LIBORnames), file = "         .txt")


# ===============================================================
# == Interested in 12MCAD or 12MUSD or 24MCADUSD Please run this ====
# LIBORnamesCAD12M.txt
# LIBORnamesUSD12M.txt
# LIBORnamesCU24M.txt
# Whicheve of the filenames chosen should be selected by section
dtable <- readline(prompt = "Which data set is your desire? \
  press 1 for '12 months Canadian dollar FRED LIBOR data set',\
  press 2 for '12 months US dollar FRED LIBOR data set', \
  or press 3 for '12 month Canadian dollar and 12 month US dollar \
  FRED LIBOR data set in one table'\
  press 4 for the group of each 3 months of Canadian dollar FRED LIBOR data set'\
  press 5 for the group of each 3 months of American dollar FRED LIBOR data set'\
  press 6 for 4 months of 1M, 6M, 9M, 12M, of each group,
  press 7 for 8 months of CAD and press 8 for 8 months of USD")

{
  library(tidyselect)
  if (dtable == 1){
    LIBORnames <- scan("LIBORnamesCAD12M.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, starts_with("CAD"))}
  else if (dtable == 2){
    LIBORnames <- scan("LIBORnamesUSD12M.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, starts_with("USD"))}
  else if (dtable == 3){
    LIBORnames <- scan("LIBORnamesCU24M.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, ends_with("D156N"))}
  else if (dtable == 4){
    LIBORnames <- scan("LIBORnamesCAD_each3M.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, starts_with("CAD"))}
  else if (dtable == 5){
    LIBORnames <- scan("LIBORnamesUSD_each3M.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, starts_with("USD"))}
  else if (dtable == 6){
    LIBORnames <- scan("LIBORnamesCU_4ofeach.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, ends_with("D156N"))}
  else if (dtable == 7){
    LIBORnames <- scan("LIBORnamesCAD8M.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, starts_with("CAD"))}
  else if (dtable == 8){
    LIBORnames <- scan("LIBORnamesUSD8M.txt", what="", sep=" ",  encoding = "latin-1")
    LIBORnames = vars_select(LIBORnames, starts_with("USD"))}
LIBORnames = as.character(LIBORnames)
}

library(dplyr)
library(quantmod)

LIBORnames <- getSymbols.FRED(LIBORnames, env = globalenv())
callLIBOR <- get(LIBORnames[[1]])


# ===============================================================
# ============== merging and cleaning all of the data =============
{
for (i in 2:length(LIBORnames)){
  kk = merge(callLIBOR,get(LIBORnames[[i]]))
  if (i < length(LIBORnames)){
    callLIBOR = kk
  }
}
callLIBOR = kk
}

names(callLIBOR) <- substring(names(callLIBOR), 0, 5)
# filtering our data frame - always must be done
callLIBOR = callLIBOR[complete.cases(callLIBOR),]




# ===============================================================
# ========= 1st part of the project - the 1st presentation ============
library(quantmod)

# cc <- as.integer(readline(prompt = 'which column of the dataframe do you wanna have average study for?'))

callLIBOR_S1 <- callLIBOR[,6]

title = paste(colnames(callLIBOR_S1))
plot(callLIBOR_S1, main=title, xlab='dates', ylab=data)

# using xts to provide the results in time series
callLIBOR_S1_xts = xts(callLIBOR_S1)

head(callLIBOR_S1_xts)
tail(callLIBOR_S1_xts)
summary(callLIBOR_S1_xts)
str(callLIBOR_S1_xts)  # returns the object structure which in this case is time series

# plotting daily prices

ggplot(callLIBOR_S1_xts, aes(x = index(callLIBOR_S1_xts), y = callLIBOR_S1_xts[,1])) + geom_line(color = "darkblue") + ggtitle(title) + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

# some technical analysis
# plotting of moving averages in the prices of the graphs

# subsetting the data since 2016
callLIBOR_S1_xts_mm <- subset(callLIBOR_S1_xts, index(callLIBOR_S1_xts) >= "1990-01-01")

# using rollmean to compute means, maximums, medians, and sums
callLIBOR_S1_xts_mm1 <- rollmean(callLIBOR_S1_xts_mm[,1], 2, fill = list(NA, NULL, NA), align = "right") #  Omitted after mm
callLIBOR_S1_xts_mm25 <- rollmean(callLIBOR_S1_xts_mm[,1], 25, fill = list(NA, NULL, NA), align = "right") #  omitted after mm

# extracting the coredata 
callLIBOR_S1_xts_mm$mm1 <- coredata(callLIBOR_S1_xts_mm1)
callLIBOR_S1_xts_mm$mm25 <- coredata(callLIBOR_S1_xts_mm25)

# callLIBOR_S1_xts_mm = callLIBOR_S1_xts_mm[complete.cases(callLIBOR_S1_xts_mm),]

# plotting prices series and moving averages(unweighted mean of 
# the previous n data) from 2016 till now

ggplot(callLIBOR_S1_xts_mm, aes(x = index(callLIBOR_S1_xts_mm))) +
  geom_line(aes(y = callLIBOR_S1_xts_mm[,1], color = "USD or CAD")) + ggtitle(title) + # omitted after mm
  geom_line(aes(y = callLIBOR_S1_xts_mm$mm1, color = "MM1")) +
  geom_line(aes(y = callLIBOR_S1_xts_mm$mm25, color = "MM25")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("USD or CAD"="gray40", "MM1"="firebrick4", "MM25"="darkcyan"))

# Additional surprise
# verifying how stock returns has behaved
# calculating the log return
callLIBOR_S1_xts_ret <- diff(log(callLIBOR_S1_xts[,1])) # omitted after xts befor parantheses
callLIBOR_S1_xts_ret <- callLIBOR_S1_xts_ret[-1,]
summary(callLIBOR_S1_xts_ret)


# in for loop preparation for histograms

# you can diagnose the year period here
rm(list=ls())


# aaaa <- callLIBOR

# callLIBOR <- aaaa

thedatexts1 = as.Date("2008-01-01", "%Y-%m-%d")  # keep it 2007 till now
thedatexts11 = as.Date("2009-01-01", "%Y-%m-%d")
# names(callLIBOR) <- substring(names(callLIBOR), 0, 5)
callLIBOR <- subset(callLIBOR, index(callLIBOR) >= thedatexts1 & index(callLIBOR) <= thedatexts11)


# callLIBOR_S1 <- callLIBOR[,cc]
p11 = list()
p12 = list()
nn = matrix(0, ncol = length(LIBORnames), nrow = nrow(callLIBOR))
forhistmm1 = data.frame(nn)
forhistmm25 = data.frame(nn)




library(ggplot2)

for (i in 1:length(LIBORnames)){

  callLIBOR_S1 <- callLIBOR[,i]

  title = paste(colnames(callLIBOR_S1))
  plot(callLIBOR_S1, main=title, xlab='dates', ylab=data)

# using xts to provide the results in time series
  callLIBOR_S1_xts = xts(callLIBOR_S1)

  head(callLIBOR_S1_xts)
  tail(callLIBOR_S1_xts)
  summary(callLIBOR_S1_xts)
  str(callLIBOR_S1_xts)  # returns the object structure which in this case is time series

# plotting daily prices

  p11[[i]] <- ggplot(callLIBOR_S1_xts, aes(x = index(callLIBOR_S1_xts), y = callLIBOR_S1_xts[,1])) + geom_line(color = "darkblue") + ggtitle(title) + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

# some technical analysis
# plotting of moving averages in the prices of the graphs

# subsetting the data since 2016
  callLIBOR_S1_xts_mm <- callLIBOR_S1_xts # subset(callLIBOR_S1_xts, index(callLIBOR_S1_xts) >= "1990-01-01")

# using rollmean to compute means, maximums, medians, and sums
  callLIBOR_S1_xts_mm1 <- rollmean(callLIBOR_S1_xts_mm[,1], 2, fill = list(NA, NULL, NA), align = "right") #  Omitted after mm
  callLIBOR_S1_xts_mm25 <- rollmean(callLIBOR_S1_xts_mm[,1], 25, fill = list(NA, NULL, NA), align = "right") #  omitted after mm
  
# extracting the coredata 
  callLIBOR_S1_xts_mm$mm1 <- coredata(callLIBOR_S1_xts_mm1)
  callLIBOR_S1_xts_mm$mm25 <- coredata(callLIBOR_S1_xts_mm25)

# callLIBOR_S1_xts_mm = callLIBOR_S1_xts_mm[complete.cases(callLIBOR_S1_xts_mm),]

# plotting prices series and moving averages(unweighted mean of 
# the previous n data) from 2016 till now

  p12[[i]] <- ggplot(callLIBOR_S1_xts_mm, aes(x = index(callLIBOR_S1_xts_mm))) +
    geom_line(aes(y = callLIBOR_S1_xts_mm[,1], color = "USD or CAD")) + ggtitle(title) + # omitted after mm
    geom_line(aes(y = callLIBOR_S1_xts_mm$mm1, color = "MM1")) +
    geom_line(aes(y = callLIBOR_S1_xts_mm$mm25, color = "MM25")) + xlab("Date") + ylab("Price") +
    theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
    scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
    scale_colour_manual("Series", values=c("USD or CAD"="gray40", "MM1"="firebrick4", "MM25"="darkcyan"))

# Additional surprise
# verifying how stock returns has behaved
# calculating the log return
  callLIBOR_S1_xts_ret <- diff(log(callLIBOR_S1_xts[,1])) # omitted after xts befor parantheses
  callLIBOR_S1_xts_ret <- callLIBOR_S1_xts_ret[-1,]
  summary(callLIBOR_S1_xts_ret)
  
  # pcoll1 = merge(pcoll1,p11)
  # pcoll2 = merge(pcoll2,p12)
  forhistmm1[[i]] = coredata(callLIBOR_S1_xts_mm1)
  forhistmm25[[i]] = coredata(callLIBOR_S1_xts_mm25)
}

forhistmm1 = forhistmm1[complete.cases(forhistmm1),]
forhistmm25 = forhistmm25[complete.cases(forhistmm25),]

dev.off()
# before running this last part of this section run multiplot.r
multiplot(plotlist = p11, cols=4)
multiplot(plotlist = p12, cols=4)
# ===============================================================
# ================= 2nd part of the project =====================
# === fitting histogram of the IRs, norm and lnorm distributions ====================

# Works fine with LIBORnamesCU_4ofeach.txt
par(mfrow = c(length(colnames(callLIBOR))%/%4,length(colnames(callLIBOR))%/%2))
for (i in 1:length(colnames(callLIBOR))){
  hist(callLIBOR[,i],freq = F, xlab=colnames(callLIBOR)[i], cex.lab=1.5, ylim=c(0,max(callLIBOR[,i])/2))
  legend(legend = c("Lognormal Distribution"), "topright", lty=c(1), 
         lwd=c(1.5), col=c("red"))
  mu = mean(callLIBOR[,i])
  sigma = sd(callLIBOR[,i])
  yrange = seq(min(callLIBOR[,i]), max(callLIBOR[,i]), 0.01)
  points(yrange, dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
}

log_callLIBOR = log(callLIBOR)
par(mfrow = c(length(colnames(callLIBOR))%/%4,length(colnames(callLIBOR))%/%2))
for (i in 1:length(colnames(callLIBOR))){
  hist(log_callLIBOR[,i],freq = F, xlab=colnames(callLIBOR)[i], cex.lab=1.5, ylim=c(0,max(callLIBOR[,i])*1.2))
  legend(legend = c("Normal Distribution"), "topright", lty=c(1), 
         lwd=c(1), col=c("red"))
  mu = mean(log_callLIBOR[,i])
  sigma = sd(log_callLIBOR[,i])
  yrange = seq(min(log_callLIBOR[,i]), max(log_callLIBOR[,i]), 0.01)
  points(yrange, dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
}

# === fitting histogram of 1-Day increments, norm and lnorm distributions ===========
par(mfrow = c(length(colnames(callLIBOR))%/%4,length(colnames(callLIBOR))%/%2))
for (i in 1:length(colnames(callLIBOR))){
  hist(as.numeric(forhistmm1[,i]),freq = F, xlab=colnames(callLIBOR)[i], cex.lab=1.5, ylim=c(0,max(callLIBOR[,i])/1.92))
  legend(legend = c("Lognormal Distribution"), "topright", lty=c(1), 
         lwd=c(1), col=c("red"))
  mu = mean(as.numeric(forhistmm1[,i]))
  sigma = sd(as.numeric(forhistmm1[,i]))
  yrange = seq(min(as.numeric(forhistmm1[,i])), max(as.numeric(forhistmm1[,i])), 0.01)
  points(yrange, dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
}


log_forhistmm1 = log(forhistmm1)
par(mfrow = c(length(colnames(callLIBOR))%/%4,length(colnames(callLIBOR))%/%2))
for (i in 1:length(colnames(callLIBOR))){
  hist(log_forhistmm1[,i],freq = F, xlab=colnames(callLIBOR)[i], cex.lab=1.5, ylim=c(0,max(callLIBOR[,i])*1.2))
  legend(legend = c("Normal Distribution"), "topright", lty=c(1), 
         lwd=c(1), col=c("red"))
  mu = mean(log_forhistmm1[,i])
  sigma = sd(log_forhistmm1[,i])
  yrange = seq(min(log_forhistmm1[,i]), max(log_forhistmm1[,i]), 0.01)
  points(yrange, dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
}

# === fitting histogram of 25-Day increments, norm and lnorm distributions ===========
par(mfrow = c(length(colnames(callLIBOR))%/%4,length(colnames(callLIBOR))%/%2))
for (i in 1:length(colnames(callLIBOR))){
  hist(forhistmm25[,i],freq = F, xlab=colnames(callLIBOR)[i], cex.lab=1.5, ylim=c(0,max(callLIBOR[,i])/1.7))
  legend(legend = c("Lognormal Distribution"), "topright", lty=c(1), 
         lwd=c(1), col=c("red"))
  mu = mean(forhistmm25[,i])
  sigma = sd(forhistmm25[,i])
  yrange = seq(min(forhistmm25[,i]), max(forhistmm25[,i]), 0.01)
  points(yrange, dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
}

log_forhistmm25 = log(forhistmm25)
par(mfrow = c(length(colnames(callLIBOR))%/%4,length(colnames(callLIBOR))%/%2))
for (i in 1:length(colnames(callLIBOR))){
  hist(log_forhistmm25[,i],freq = F, xlab=colnames(callLIBOR)[i], cex.lab=1.5, ylim=c(0,max(callLIBOR[,i])*1.8))
  legend(legend = c("Normal Distribution"), "topright", lty=c(1), 
         lwd=c(1), col=c("red"))
  mu = mean(log_forhistmm25[,i])
  sigma = sd(log_forhistmm25[,i])
  yrange = seq(min(log_forhistmm25[,i]), max(log_forhistmm25[,i]), 0.01)
  points(yrange, dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
}

# ====== fitting historgarms by several distributions by fitdistrplus =======
library(fitdistrplus)
par(mfrow = c(length(colnames(callLIBOR))%/%4,length(colnames(callLIBOR))%/%2))
for (i in 1:length(colnames(callLIBOR))){
  # plotdist(as.numeric(forhistmm1[,i]), histo = TRUE, demp = TRUE)
  descdist(as.numeric(forhistmm1[,i]), boot = 4000)
}

# you need to run this for many tenurs, and save the plots

plotdist(as.numeric(forhistmm1[,8]), histo = TRUE, demp = TRUE)

# === several fitting to histograms QQplot, CDFs', and P-P plot ===
forhistmm1fw <- fitdist(as.numeric(forhistmm1[,8]), "weibull")
class(forhistmm1fw)
summary(forhistmm1fw)

forhistmm25fw <- fitdist(as.numeric(forhistmm25[,8]), "weibull")
class(forhistmm25fw)
summary(forhistmm25fw)

forhistmm1fln <- fitdist(as.numeric(forhistmm1[,8]), "lnorm")
class(forhistmm1fln)
summary(forhistmm1fln)

forhistmm25fln <- fitdist(as.numeric(forhistmm25[,8]), "lnorm")
class(forhistmm25fln)
summary(forhistmm25fln)

forhistmm1fg <- fitdist(as.numeric(forhistmm1[,8]), "gamma")
class(forhistmm1fg)
summary(forhistmm1fg)

forhistmm25fg <- fitdist(as.numeric(forhistmm25[,8]), "gamma")
class(forhistmm25fg)
summary(forhistmm25fg)
par(mfrow=c(1,2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(forhistmm1fw, forhistmm1fln, forhistmm1fg), legendtext = plot.legend)
denscomp(list(forhistmm25fw, forhistmm25fln, forhistmm25fg), legendtext = plot.legend)

qqcomp(list(forhistmm1fw, forhistmm1fln, forhistmm1fg), legendtext = plot.legend)
qqcomp(list(forhistmm25fw, forhistmm25fln, forhistmm25fg), legendtext = plot.legend)

cdfcomp(list(forhistmm1fw, forhistmm1fln, forhistmm1fg), legendtext = plot.legend)
cdfcomp(list(forhistmm25fw, forhistmm25fln, forhistmm25fg), legendtext = plot.legend)

ppcomp(list(forhistmm1fw, forhistmm1fln, forhistmm1fg), legendtext = plot.legend)
ppcomp(list(forhistmm25fw, forhistmm25fln, forhistmm25fg), legendtext = plot.legend)

# CDF plot to compare the t of four distributions
fendo.ln <- fitdist(as.numeric(forhistmm25[,8]), "lnorm")
library("actuar")
fendo.ll <- fitdist(as.numeric(forhistmm25[,8]), "llogis", start = list(shape = 1, scale = 500))
fendo.P <- fitdist(as.numeric(forhistmm25[,8]), "pareto", start = list(shape = 1, scale = 500))
fendo.B <- fitdist(as.numeric(forhistmm25[,8]), "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 1))
cdfcomp(list(fendo.ln, fendo.ll, fendo.P, fendo.B), xlogscale = TRUE, ylogscale = TRUE, legendtext = c("lognormal", "loglogistic", "Pareto", "Burr"))



# ======= calculating and plotting Average term structure ======

# # calculating the term structure for year 2008
# library(lubridate)
# callLIBOR2008 <- callLIBOR[index(callLIBOR) >= as.Date("2008-01-01") & index(callLIBOR) <= as.Date("2008-12-31")]
# # callLIBOR2008 <- subset(callLIBOR, as.Date("2008-01-01") <= date(index(callLIBOR)) <= as.Date("2008-12-31"))
# callLIBOR <- callLIBOR2008
# callLIBORMd <- aggregate(callLIBOR, list(day(callLIBOR), mean))
# callLIBORMw <- aggregate(callLIBOR, list(week(callLIBOR), mean))
# callLIBORMm <- aggregate(callLIBOR, list(month(callLIBOR), mean))
# callLIBORMy <- aggregate(callLIBOR, list(year(callLIBOR), mean))
# 
# callLIBORSd <- aggregate(callLIBOR, list(day(callLIBOR), sd))
# callLIBORSw <- aggregate(callLIBOR, list(week(callLIBOR), sd))
# callLIBORSm <- aggregate(callLIBOR, list(month(callLIBOR), sd))
# callLIBORSy <- aggregate(callLIBOR, list(year(callLIBOR), sd))
# 
# # to change the input data just change "callLIBORMw" to each of the 6 previous data
# calldata4ts <- callLIBORMd
# index.calldata4ts = index(calldata4ts)
# 
# lim.calldata4ts <- data.frame(term = 1:length(calldata4ts[i, ]), rates = t(coredata(calldata4ts[i, ])))
# gplot <- ggplot(data = lim.calldata4ts, aes(x = lim.calldata4ts[,1], y = lim.calldata4ts[,2])) + geom_line() +geom_point()
# gplot <- gplot + scale_x_discrete(breaks=lim.calldata4ts$term, labels=colnames(calldata4ts[i,]))
# gplot <- gplot + ggtitle(paste("Market Zero Rates for:", index.calldata4ts)) + xlab("Term") + ylab("Rates (%)")
# gplot

# # constructing the yield curve
# ir_const_maturities <- calldata4ts
# library(lubridate)
# term.str.dt <- c()
# for (j in colnames(ir_const_maturities)) {
#   term_num <- as.numeric(substring(j, 1, nchar(j) - 1))
#   term <- switch(tolower(substring(j, nchar(j))),
#                  "y" = years(term_num),
#                  "m" = months(term_num),
#                  "w" = weeks(term_num),
#                  stop("Unit unrecognized in the term structure file"))
#   term.str.dt <- cbind(term.str.dt, ad + term)
# }
# term.str.dt <- as.Date(term.str.dt, origin = "1970-01-01")

rm(list=ls())
dev.off()
par(mfrow=c(1,1))

require(xts)
require(lattice)
library(tidyselect)
library(dplyr)
library(quantmod)
library(ggplot2)
library(lubridate)

"LIBORnamesUSD8M" # change this filename to consider CAD and USD
LIBORnames <- scan("LIBORnamesUSD8M.txt", what="", sep=" ",  encoding = "latin-1")
LIBORnames = vars_select(LIBORnames, ends_with("D156N"))
LIBORnames = as.character(LIBORnames)
LIBORnames <- getSymbols.FRED(LIBORnames, env = globalenv())
callLIBOR <- get(LIBORnames[[1]])

{
  for (i in 2:length(LIBORnames)){
    kk = merge(callLIBOR,get(LIBORnames[[i]]))
    if (i < length(LIBORnames)){
      callLIBOR = kk
    }
  }
  callLIBOR = kk
}

names(callLIBOR) <- substring(names(callLIBOR), 0, 5)

# filtering our data frame - always must be done
callLIBOR = callLIBOR[complete.cases(callLIBOR),]

thedate1 = as.Date("2009-01-01", "%Y-%m-%d")  # keep it 2007 till now
thedate11 = as.Date("2010-01-01", "%Y-%m-%d")
# callLIBOR tail 2013-05-31
# you can diagnose the year period here
callLIBOR <- subset(callLIBOR, index(callLIBOR) >= thedate1 & index(callLIBOR) <= thedate11)

# library(lubridate)
# callLIBORMd <- aggregate(callLIBOR, list(day(callLIBOR), mean))
callLIBORMw <- apply.weekly(callLIBOR[,1:length(names(callLIBOR))], mean)
callLIBORMm <- apply.monthly(callLIBOR[,1:length(names(callLIBOR))], mean)
callLIBORMy <- apply.yearly(callLIBOR[,1:length(names(callLIBOR))], mean)
callLIBORMq <- apply.quarterly(callLIBOR[,1:length(names(callLIBOR))], mean)
  
callLIBORSw <- apply.weekly(callLIBOR[,1], sd)
callLIBORSm <- apply.monthly(callLIBOR[,1], sd)
callLIBORSy <- apply.yearly(callLIBOR[,1], sd)
callLIBORSq <- apply.quarterly(callLIBOR[,1], sd)

for (i in 2:length(names(callLIBOR))){
# callLIBORSd <- aggregate(callLIBOR, list(day(callLIBOR), sd))
  callLIBORSw <- merge(callLIBORSw, apply.weekly(callLIBOR[,i], sd))
  callLIBORSm <- merge(callLIBORSm, apply.monthly(callLIBOR[,i], sd))
  callLIBORSy <- merge(callLIBORSy, apply.yearly(callLIBOR[,i], sd))
  callLIBORSq <- merge(callLIBORSq, apply.quarterly(callLIBOR[,i], sd))
}

# callLIBORMd <- aggregate(callLIBOR, list(day(callLIBOR), mean))
# callLIBORMw <- aggregate(callLIBOR, list(epiweek(callLIBOR), mean))
# callLIBORMm <- aggregate(callLIBOR, list(month(callLIBOR), mean))
# # callLIBORMy <- aggregate(callLIBOR, list(year(callLIBOR), mean))
# 
# callLIBORSd <- aggregate(callLIBOR, list(day(callLIBOR), sd))
# callLIBORSw <- aggregate(callLIBOR, list(week(callLIBOR), sd))
# callLIBORSm <- aggregate(callLIBOR, list(month(callLIBOR), sd))
# # callLIBORSy <- aggregate(callLIBOR, list(year(callLIBOR), sd))

callLIBORts <- callLIBORMm # take monthly or yearly data
callLIBORtse <- callLIBORSm # the SD of monthly or yearly data

xt = xts(callLIBORts,order.by=as.Date(index(callLIBORts)))
xyplot.ts(xt,scales=list(y=list(relation="same")),ylab="Yield (%)")

xyplot.ts(xt,superpose=TRUE,auto.key=list(columns=1),ylab="Yield (%)")
treasury.maturity <- c(1/12,6/12,9/12,1,1/12,6/12,9/12,1)#2,3,5,10)
mv = max(tail(callLIBORts,n=as.numeric(Sys.Date()-as.Date(thedate1))))
g <- gray.colors(6,start=0.3,end=0.9,gamma=2.2)



# gplot = list()
# for (i in 1:length(names(callLIBORts))){
  # gplot[[i]] <-


# probably this one is incorrect the next one is correct
ggplot(callLIBORts,  aes(x = index(callLIBORts))) +
         geom_line(aes(y = callLIBORts[,1], color = "CAD1M")) + geom_point(aes(y = callLIBORts[,1], color = "CAD1M")) + ggtitle("Monthly Prices") + # omitted after mm
         geom_line(aes(y = callLIBORts[,2], color = "CAD6M")) + geom_point(aes(y = callLIBORts[,2], color = "CAD6M")) + 
         geom_line(aes(y = callLIBORts[,3], color = "CAD9M")) + geom_point(aes(y = callLIBORts[,3], color = "CAD9M")) +  xlab("Date") + ylab("Interest Rates(%)") +
         geom_line(aes(y = callLIBORts[,4], color = "CAD12M")) + geom_point(aes(y = callLIBORts[,4], color = "CAD12M")) + 
         geom_line(aes(y = callLIBORts[,5], color = "USD1M")) + geom_point(aes(y = callLIBORts[,5], color = "USD1M")) + 
         geom_line(aes(y = callLIBORts[,6], color = "USD6M")) + geom_point(aes(y = callLIBORts[,6], color = "USD6M")) + 
         geom_line(aes(y = callLIBORts[,7], color = "USD9M")) + geom_point(aes(y = callLIBORts[,7], color = "USD9M")) + 
         geom_line(aes(y = callLIBORts[,8], color = "USD12M")) + geom_point(aes(y = callLIBORts[,8], color = "USD12M")) + 
         theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) 
         # scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
         # scale_colour_manual("Series", values=c("CAD1M"="gray40", "CAD6M"="firebrick4", "CAD9M"="darkcyan", 
         #                                        "CAD12M"="red", "USD1M"="blue", "USD6M"="green", 
         #                                        "USD9M"="brown", "USD12"="purple"))
         # 
     # type="l",
     # main="Maturity Yield Curve - Term Structure",
     # sub="Current Values And Change Over Six Months",
     # xlab="Maturity (yr)",
     # ylab="Yield (%)",
     # lwd=2,
     # col="darkred")
# }



# dev.new(width = 4, height = 4)
# plot(1:n, pch = 16, cex = 2, col = cols)


gcolors = c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
            "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
            "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
            "#8A7C64", "#599861")
x11()
# It provides great pictures
tcallLIBORts <- data.frame(t(callLIBORts))
tcallLIBORtse <- data.frame(t(callLIBORtse))
colnames(tcallLIBORts) <- index(callLIBORts)
plotxticks = c("CAD1M", "CAD6M", "CAD9M", "CAD12", "USD1M", "USD6M", "USD9M", "USD12")
pd <- position_dodge(0.1) # move error bars left or right
ggplot(tcallLIBORts,  aes(x=1:nrow(tcallLIBORts))) +
# show it by on full picture and then two
# separate groups of 4 month each so total of 3 pictures
  geom_line(aes(y = tcallLIBORts[,1]), color="blue" ,position=pd) + geom_point(aes(y = tcallLIBORts[,1]), color="blue" ,position=pd) +  # omitted after mm
  geom_errorbar(aes(ymin=tcallLIBORts[,1]-tcallLIBORtse[,1], ymax=tcallLIBORts[,1]+tcallLIBORtse[,1]), width=.1, color="blue" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,2]), color="green" ,position=pd) + geom_point(aes(y = tcallLIBORts[,2]), color="green" ,position=pd) +
  geom_errorbar(aes(ymin=tcallLIBORts[,2]-tcallLIBORtse[,2], ymax=tcallLIBORts[,2]+tcallLIBORtse[,2]), width=.1, color="green" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,3]), color="pink" ,position=pd) + geom_point(aes(y = tcallLIBORts[,3]), color="pink" ,position=pd) +
  geom_errorbar(aes(ymin=tcallLIBORts[,3]-tcallLIBORtse[,3], ymax=tcallLIBORts[,3]+tcallLIBORtse[,3]), width=.1, color="pink" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,4]), color="purple" ,position=pd) + geom_point(aes(y = tcallLIBORts[,4]), color="purple" ,position=pd) + #labs(colour = "Legend Title")+
  geom_errorbar(aes(ymin=tcallLIBORts[,4]-tcallLIBORtse[,4], ymax=tcallLIBORts[,4]+tcallLIBORtse[,4]), width=.1, color="purple" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,5]), color="brown" ,position=pd) + geom_point(aes(y = tcallLIBORts[,5]), color="brown" ,position=pd) +
  geom_errorbar(aes(ymin=tcallLIBORts[,5]-tcallLIBORtse[,5], ymax=tcallLIBORts[,5]+tcallLIBORtse[,5]), width=.1, color="brown" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,6]), color="darkgreen" ,position=pd) + geom_point(aes(y = tcallLIBORts[,6]), color="darkgreen" ,position=pd) +
  geom_errorbar(aes(ymin=tcallLIBORts[,6]-tcallLIBORtse[,6], ymax=tcallLIBORts[,6]+tcallLIBORtse[,6]), width=.1, color="darkgreen" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,7]), color="black" ,position=pd) + geom_point(aes(y = tcallLIBORts[,7]), color="black" ,position=pd) +
  geom_errorbar(aes(ymin=tcallLIBORts[,7]-tcallLIBORtse[,7], ymax=tcallLIBORts[,7]+tcallLIBORtse[,7]), width=.1, color="black" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,8]), color="lightblue" ,position=pd) + geom_point(aes(y = tcallLIBORts[,8]), color="lightblue" ,position=pd) +
  geom_errorbar(aes(ymin=tcallLIBORts[,8]-tcallLIBORtse[,8], ymax=tcallLIBORts[,8]+tcallLIBORtse[,8]), width=.1, color="lightblue" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,9]), color="orange" ,position=pd) + geom_point(aes(y = tcallLIBORts[,9]), color="orange") +
  geom_errorbar(aes(ymin=tcallLIBORts[,9]-tcallLIBORtse[,9], ymax=tcallLIBORts[,9]+tcallLIBORtse[,9]), width=.1, color="orange" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,10]), color="burlywood3" ,position=pd) + geom_point(aes(y = tcallLIBORts[,10]), color="burlywood3") +
  geom_errorbar(aes(ymin=tcallLIBORts[,10]-tcallLIBORtse[,10], ymax=tcallLIBORts[,10]+tcallLIBORtse[,10]), width=.1, color="burlywood3" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,11]), color="red" ,position=pd) + geom_point(aes(y = tcallLIBORts[,11]), color="red") +
  geom_errorbar(aes(ymin=tcallLIBORts[,11]-tcallLIBORtse[,11], ymax=tcallLIBORts[,11]+tcallLIBORtse[,11]), width=.1, color="red" ,position=pd) +
  geom_line(aes(y = tcallLIBORts[,12]), color="darkkhaki" ,position=pd) + geom_point(aes(y = tcallLIBORts[,12]), color="darkkhaki") +
  geom_errorbar(aes(ymin=tcallLIBORts[,12]-tcallLIBORtse[,12], ymax=tcallLIBORts[,12]+tcallLIBORtse[,12]), width=.1, color="darkkhaki" ,position=pd) +
  
  # annotate("text", x = 4, y = 25, label = "Some text") +
  ggtitle("Term Structure, 2009,01:blue, 2009,02:green, 2009,03:pink, 2009,04:purple, \n
          2009,05:brown ,2009,06:darkgreen, 2009,07:black, 2009,08:lightblue, 2009,09:orange, 2009,10:burlywood3, 2009,11:red, 2009,12:darkkhaki") +
  ylab("Interest Rates(%)") + xlab("1:1M, 2:3M, 3:5M, 4:6M, 5:8M, 6:9M, 7:10M, 8:12M") +
  theme(plot.title = element_text(hjust = 0.5),legend.title.align = 1, panel.border = element_blank()) # + facet_wrap( ~ variable, scales="free")

# theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.border = element_blank()) # + facet_wrap( ~ variable, scales="free")




# ggplot(callLIBORts,  aes(x=1:4)) +
#   geom_line(aes(y = callLIBORts[,1], color = "CAD1M")) + geom_point(aes(y = callLIBORts[,1], color = "CAD1M")) + ggtitle("Term Structure") + # omitted after mm
#   geom_line(aes(y = callLIBORts[,2], color = "CAD6M")) + geom_point(aes(y = callLIBORts[,2], color = "CAD6M")) +
#   geom_line(aes(y = callLIBORts[,3], color = "CAD9M")) + geom_point(aes(y = callLIBORts[,3], color = "CAD9M")) +  xlab("Date") + ylab("SD") +
#   geom_line(aes(y = callLIBORts[,4], color = "CAD12M")) + geom_point(aes(y = callLIBORts[,4], color = "CAD12M")) +
#   geom_line(aes(y = callLIBORts[,5], color = "USD1M")) + geom_point(aes(y = callLIBORts[,5], color = "USD1M")) +
#   geom_line(aes(y = callLIBORts[,6], color = "USD6M")) + geom_point(aes(y = callLIBORts[,6], color = "USD6M")) +
#   geom_line(aes(y = callLIBORts[,7], color = "USD9M")) + geom_point(aes(y = callLIBORts[,7], color = "USD9M")) +
#   geom_line(aes(y = callLIBORts[,8], color = "USD12M")) + geom_point(aes(y = callLIBORts[,8], color = "USD12M")) +
#   theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank())# + facet_wrap( ~ variable, scales="free"))
#   # + annotate(geom="text", x=index(t(callLIBORts)), y=t(callLIBORts)[,1:4], label = 1:6, size = 6,
           #  colour = "white"))










# library(ggplot2)
# library(tidyr) # for the gather function
# (callLIBORts) %>% 
#   gather("key", "value", -row.names((callLIBORts))) %>% 
#   ggplot(aes(x = row.names((callLIBORts)), y = value, color = key)) +
#   geom_line()
# 
# 
# callLIBORts %>% 
#   # gather("key", "value", -row.names(t(callLIBORts))) %>% 
#   ggplot(aes(x = row.names(callLIBORts), y = value, color = key)) +
#   geom_line() +
#   geom_point() +
#   geom_label(aes(y = value, label=key), hjust = 0, vjust = -0.2) +
#   labs(title = "Reli")+
#   labs(x="Dates")+
#   labs(y="")+ geom_path(data = callLIBORts, aes(x =row.names(callLIBORts) ,y = callLIBORts[,1:8]), inherit.aes = FALSE )+
#   guides(color = guide_legend(title = ""))
# 
# 
# plot(callLIBORts[,1]~factor(row.names(callLIBORts)), t(callLIBORts), las=2, 
#      xlab="", main="Mean Acceptability Ranking per Sentence")











# ggplot(callLIBORts,  aes(x = index(callLIBORts))) +
#   geom_line(aes(y = callLIBORts[1,], color = "CAD1M")) + #geom_point(aes(y = callLIBORts[1,], color = "CAD1M")) + ggtitle("Term Structure") + # omitted after mm
#   geom_line(aes(y = callLIBORts[2,], color = "CAD6M")) + #geom_point(aes(y = callLIBORts[2,], color = "CAD6M")) + 
#   geom_line(aes(y = callLIBORts[3,], color = "CAD9M")) + #geom_point(aes(y = callLIBORts[3,], color = "CAD9M")) +  xlab("Date") + ylab("SD") +
#   geom_line(aes(y = callLIBORts[4,], color = "CAD12M")) + #geom_point(aes(y = callLIBORts[4,], color = "CAD12M")) + 
#   geom_line(aes(y = callLIBORts[5,], color = "USD1M")) + #geom_point(aes(y = callLIBORts[5,], color = "USD1M")) + 
#   geom_line(aes(y = callLIBORts[6,], color = "USD6M")) + #geom_point(aes(y = callLIBORts[6,], color = "USD6M")) + 
#   geom_line(aes(y = callLIBORts[7,], color = "USD9M")) + #geom_point(aes(y = callLIBORts[7,], color = "USD9M")) + 
#   geom_line(aes(y = callLIBORts[8,], color = "USD12M")) + #geom_point(aes(y = callLIBORts[8,], color = "USD12M")) + 
#   theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) + facet_wrap( ~ variable, scales="free")
# 
# ggplot(data = t(callLIBORts), aes(x = index(callLIBORts), y = c)) +
#   geom_point() + 
#   geom_smooth(method = "lm", se = FALSE, lwd = .5, col = "black") +
#   facet_wrap(~ Name, scales = "free_y") 









# mcallLIBORts <- (callLIBORts)
# 
# library(reshape2)
# mcallLIBORts <- melt(callLIBORts)  #the function melt reshapes it from wide to long
# mcallLIBORts$rowid <- 1:4
# mcallLIBORts = mcallLIBORts[-c(33,34,35,36),]
# 
# ggplot(mcallLIBORts, aes(variable, value, group=factor(rowid))) + geom_line(aes(color=factor(rowid))) + geom_point(aes(color=factor(rowid))) + xlab(colnames(callLIBORts))
# library(lubridate)
# ggplot(mcallLIBORts, aes(year(Var2), value, group=factor(rowid))) + geom_line(aes(color=factor(rowid))) + geom_point(aes(color=factor(rowid)))# + xlab((colnames(callLIBORts)))
# mcallLIBORts = list()
# for (i in nrow(callLIBORts)){
#   for (j in ncol(callLIBORts)){
#     mcallLIBORts[j,i] = callLIBORts[i,j]
#   }
# }











# ggplot(callLIBORts,  aes(x = c(1:8))) + 
#   geom_line(aes(y = callLIBORts[1,1:8])) + ylab(index(callLIBORts)) + xlab(colnames(callLIBORts))+# geom_point(aes(y = callLIBORts[1,], color = "CAD1M")) + ggtitle("Term Structure") + # omitted after mm
  # geom_line(aes(y = callLIBORts[1:nrow(callLIBORts),2], color = "CAD6M")) +# geom_point(aes(y = callLIBORts[2,], color = "CAD6M")) + 
  # geom_line(aes(y = callLIBORts[1:nrow(callLIBORts),3], color = "CAD9M")) +# geom_point(aes(y = callLIBORts[3,], color = "CAD9M")) +  xlab("Date") + ylab("SD") +
  # geom_line(aes(y = callLIBORts[1:nrow(callLIBORts),4], color = "CAD12M")) +# geom_point(aes(y = callLIBORts[4,], color = "CAD12M")) + 
  # geom_line(aes(y = callLIBORts[,5], color = "USD1M")) + geom_point(aes(y = callLIBORts[,5], color = "USD1M")) + 
  # geom_line(aes(y = callLIBORts[,6], color = "USD6M")) + geom_point(aes(y = callLIBORts[,6], color = "USD6M")) + 
  # geom_line(aes(y = callLIBORts[,7], color = "USD9M")) + geom_point(aes(y = callLIBORts[,7], color = "USD9M")) + 
  # geom_line(aes(y = callLIBORts[,8], color = "USD12M")) + geom_point(aes(y = callLIBORts[,8], color = "USD12M")) + 
  # theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) + facet_wrap( ~ variable, scales="free")








# points(treasury.maturity,
#        as.numeric(callLIBORts[nrow(callLIBORts),-12]),
#        col="darkred")


# for (i in 1:length(names(callLIBORts))){
#   callLIBORtsplot <- callLIBORts[,i]
#  gplot[[i]] <- ggplot(callLIBORtsplot, aes(x = index(callLIBORtsplot))) +
#   geom_line(aes(y = callLIBORtsplot, color = "USD or CAD")) + ggtitle(title) + # omitted after mm
#   theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
#   scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
#   scale_colour_manual("Series", values=c("USD or CAD"="gray40", "MM1"="firebrick4", "MM25"="darkcyan"))
# }

# multiplot(plotlist = gplot, cols=4)


# points(treasury.maturity,
#        as.numeric(callLIBOR[nrow(callLIBOR),-12]),
#        col="darkred")

# for ( i in 1:6 ) {   # original value is 30, for week \
#   # divide 30 by 7, for month divide 30 by 30, for year divide \
#   # 30 by 365
#   lines(treasury.maturity,
#         as.numeric(callLIBOR[nrow(callLIBOR)-i*30/7,-12]),
#         type="l",
#         col=g[i])
# }

# mtext(callLIBOR,side=3,col="darkgray",cex=0.7)


# ========= Relationship between SD of the 1-Day and 25-Day IR increments =======


# ===== Using corrplot package plot correlation matrix between various tenors =====

# for (i in 2:length(LIBORnames)){
  
# callLIBOR_S1 <- callLIBOR
#   
# title = paste(colnames(callLIBOR_S1))
# 
#   # using xts to provide the results in time series
#   
#   # using rollmean to compute means, maximums, medians, and sums
# callLIBOR_S1_xts_mm1 <- rollmean(callLIBOR_S1_xts_mm, 2, fill = list(NA, NULL, NA), align = "right") #  Omitted after mm
# callLIBOR_S1_xts_mm25 <- rollmean(callLIBOR_S1_xts_mm, 25, fill = list(NA, NULL, NA), align = "right") #  omitted after mm
#   
#   kkk = merge(callLIBOR_S1_xts_dd1, callLIBOR_S1_xts_mm1[,i])
#   ggg = merge(callLIBOR_S1_xts_dd25, callLIBOR_S1_xts_mm25[,i])
# # }
# callLIBOR_S1_xts_mm = kkk
# callLIBOR_S1_xts_mm25 = ggg
# 
# # for (i in 2:length(LIBORnames)){
# #   kk = merge(callLIBOR,get(LIBORnames[[i]]))
# #   if (i < length(LIBORnames)){
# #     callLIBOR = kk
# #   }
# # }
# # callLIBOR = kk
# # }

rm(list = ls())
dev.off()
# LIBORnamesCAD12M.txt
# LIBORnamesUSD12M.txt
LIBORnames <- scan("LIBORnamesCU24M.txt", what="", sep=" ",  encoding = "latin-1")
LIBORnames = vars_select(LIBORnames, ends_with("D156N"))
LIBORnames = as.character(LIBORnames)
LIBORnames <- getSymbols.FRED(LIBORnames, env = globalenv())
callLIBOR <- get(LIBORnames[[1]])

{
  for (i in 2:length(LIBORnames)){
    kk = merge(callLIBOR,get(LIBORnames[[i]]))
    if (i < length(LIBORnames)){
      callLIBOR = kk
    }
  }
  callLIBOR = kk
}

names(callLIBOR) <- substring(names(callLIBOR), 0, 5)

# filtering our data frame - always must be done
callLIBOR = callLIBOR[complete.cases(callLIBOR),]

callLIBOR_S1_xts = xts(callLIBOR)


# names(callLIBOR) <- substring(names(callLIBOR), 0, 5)
# callLIBOR <- subset(callLIBOR, index(callLIBOR) >= thedatexts1 & index(callLIBOR) <= thedatexts11)

thedate2 = as.Date("2008-01-01", "%Y-%m-%d")  # keep it 2007 till now
thedate22 = as.Date("2009-01-01", "%Y-%m-%d")

# you can diagnose the range of years here
callLIBOR_S1_xts_mm <- subset(callLIBOR_S1_xts, index(callLIBOR_S1_xts) >= thedate2 & index(callLIBOR) <= thedate22)

callLIBOR_S1_xts_mm1 <- rollmean(callLIBOR_S1_xts_mm, 2, align = "left") #  Omitted after mm
callLIBOR_S1_xts_mm25 <- rollmean(callLIBOR_S1_xts_mm, 25, align = "left") #  omitted after mm

# callLIBOR_S1_xts_mm1 <- rollmean(callLIBOR_S1_xts_mm, 2, fill = list(NA, NULL, NA), align = "center") #  Omitted after mm
# callLIBOR_S1_xts_mm25 <- rollmean(callLIBOR_S1_xts_mm, 25, fill = list(NA, NULL, NA), align = "center") #  omitted after mm


callLIBOR_S1_xts_mm1 = as.matrix(callLIBOR_S1_xts_mm1)
callLIBOR_S1_xts_mm25 = as.matrix(callLIBOR_S1_xts_mm25)
library(corrplot)

# Kendall correlation plot
cormm1 <- cor(callLIBOR_S1_xts_mm1, method = "kendall")
cormm25 <- cor(callLIBOR_S1_xts_mm25, method = "kendall")
# set.seed(0)
# x11()
corrplot(cormm1, is.corr = FALSE)
title("Kendall 1D correlation plot year 2008 ", adj = 0.5, line = 3.2)
corrplot(cormm25, is.corr = FALSE)
title("Kendall 25D correlation plot year 2008 ", adj = 0.5, line = 3.2)

# Spearman correlation plot
cormm1 <- cor(callLIBOR_S1_xts_mm1, method = "spearman")
cormm25 <- cor(callLIBOR_S1_xts_mm25, method = "spearman")
# set.seed(0)
corrplot(cormm1, is.corr = FALSE)
title("Spearman 1D correlation plot year 2008 ", adj = 0.5, line = 3.2)
corrplot(cormm25, is.corr = FALSE)
title("Spearman 25D correlation plot year 2008 ", adj = 0.5, line = 3.2)

ev1_ev2 = eigen(cormm1)
ev1LIBOR = ev1_ev2$values   # eigenvalues
ev2LIBOR = ev1_ev2$vectors   # eigenvectors

# Plot of eigenvectors for CAD
ev2LIBORCAD = data.frame(ev2LIBOR[1:12,])
ggplot(ev2LIBORCAD,  aes(x = index(ev2LIBORCAD))) +
  geom_line(aes(y = ev2LIBORCAD[,1], color = "CAD1M")) + geom_point(aes(y = ev2LIBORCAD[,1], color = "CAD1M")) + ggtitle("eigenvectors") + # omitted after mm
  # geom_line(aes(y = ev2LIBORCAD[,2], color = "CAD2M")) + geom_point(aes(y = ev2LIBORCAD[,2], color = "CAD2M")) + 
  geom_line(aes(y = ev2LIBORCAD[,3], color = "CAD3M")) + geom_point(aes(y = ev2LIBORCAD[,3], color = "CAD3M")) +  xlab("Tenurs") + ylab("PCA") +
  # geom_line(aes(y = ev2LIBORCAD[,4], color = "CAD4M")) + geom_point(aes(y = ev2LIBORCAD[,4], color = "CAD4M")) +# ggtitle("eigenvectors") + # omitted after mm
  geom_line(aes(y = ev2LIBORCAD[,5], color = "CAD5M")) + geom_point(aes(y = ev2LIBORCAD[,5], color = "CAD5M")) + 
  geom_line(aes(y = ev2LIBORCAD[,6], color = "CAD6M")) + geom_point(aes(y = ev2LIBORCAD[,6], color = "CAD6M")) +#  xlab("Tenurs") + ylab("PCA") +
  # geom_line(aes(y = ev2LIBORCAD[,7], color = "CAD7M")) + geom_point(aes(y = ev2LIBORCAD[,7], color = "CAD7M")) +# ggtitle("eigenvectors") + # omitted after mm
  # geom_line(aes(y = ev2LIBORCAD[,8], color = "CAD8M")) + geom_point(aes(y = ev2LIBORCAD[,8], color = "CAD8M")) + 
  # geom_line(aes(y = ev2LIBORCAD[,9], color = "CAD9M")) + geom_point(aes(y = ev2LIBORCAD[,9], color = "CAD9M")) +#  xlab("Tenurs") + ylab("PCA") +
  # geom_line(aes(y = ev2LIBORCAD[,10], color = "CAD10M")) + geom_point(aes(y = ev2LIBORCAD[,10], color = "CAD10M")) +# ggtitle("eigenvectors") + # omitted after mm
  # geom_line(aes(y = ev2LIBORCAD[,11], color = "CAD11M")) + geom_point(aes(y = ev2LIBORCAD[,11], color = "CAD11M")) + 
  # geom_line(aes(y = ev2LIBORCAD[,12], color = "CAD12M")) + geom_point(aes(y = ev2LIBORCAD[,12], color = "CAD12M")) +#  xlab("Tenurs") + ylab("PCA") +
  
  theme(plot.title = element_text(hjust = 0.5))

# Plot of eigenvectors for USD
ev2LIBORUSD = data.frame(ev2LIBOR[12:24,])
ggplot(ev2LIBORUSD,  aes(x = index(ev2LIBORUSD))) +
  geom_line(aes(y = ev2LIBORUSD[,1], color = "USD1M")) + geom_point(aes(y = ev2LIBORUSD[,1], color = "USD1M")) + ggtitle("eigenvectors") + # omitted after mm
  # geom_line(aes(y = ev2LIBORUSD[,2], color = "USD2M")) + geom_point(aes(y = ev2LIBORUSD[,2], color = "USD2M")) + 
  geom_line(aes(y = ev2LIBORUSD[,3], color = "USD3M")) + geom_point(aes(y = ev2LIBORUSD[,3], color = "USD3M")) +  xlab("Tenurs") + ylab("PCA") +
  # geom_line(aes(y = ev2LIBORUSD[,4], color = "USD4M")) + geom_point(aes(y = ev2LIBORUSD[,4], color = "USD4M")) +# ggtitle("eigenvectors") + # omitted after mm
  geom_line(aes(y = ev2LIBORUSD[,5], color = "USD5M")) + geom_point(aes(y = ev2LIBORUSD[,5], color = "USD5M")) + 
  geom_line(aes(y = ev2LIBORUSD[,6], color = "USD6M")) + geom_point(aes(y = ev2LIBORUSD[,6], color = "USD6M")) +#  xlab("Tenurs") + ylab("PCA") +
  # geom_line(aes(y = ev2LIBORUSD[,7], color = "USD7M")) + geom_point(aes(y = ev2LIBORUSD[,7], color = "USD7M")) +# ggtitle("eigenvectors") + # omitted after mm
  # geom_line(aes(y = ev2LIBORUSD[,8], color = "USD8M")) + geom_point(aes(y = ev2LIBORUSD[,8], color = "USD8M")) + 
  # geom_line(aes(y = ev2LIBORUSD[,9], color = "USD9M")) + geom_point(aes(y = ev2LIBORUSD[,9], color = "USD9M")) +#  xlab("Tenurs") + ylab("PCA") +
  # geom_line(aes(y = ev2LIBORUSD[,10], color = "USD10M")) + geom_point(aes(y = ev2LIBORUSD[,10], color = "USD10M")) +# ggtitle("eigenvectors") + # omitted after mm
  # geom_line(aes(y = ev2LIBORUSD[,11], color = "USD11M")) + geom_point(aes(y = ev2LIBORUSD[,11], color = "USD11M")) + 
  # geom_line(aes(y = ev2LIBORUSD[,12], color = "USD12M")) + geom_point(aes(y = ev2LIBORUSD[,12], color = "USD12M")) +#  xlab("Tenurs") + ylab("PCA") +
  
  theme(plot.title = element_text(hjust = 0.5))


# === Calculate and plot three first principal components of IR 1-Day and 25-Day ===

# first three PCA by using "princomp" function
pcallLIBOR_S1_xts_mm1 <- princomp(callLIBOR_S1_xts_mm1, cor = TRUE, scores = TRUE)
summary(pcallLIBOR_S1_xts_mm1)
plot(pcallLIBOR_S1_xts_mm1)
plot(pcallLIBOR_S1_xts_mm1, type='l')
biplot(pcallLIBOR_S1_xts_mm1)

pcallLIBOR_S1_xts_mm25 <- princomp(callLIBOR_S1_xts_mm25, cor = TRUE, scores = TRUE)
summary(pcallLIBOR_S1_xts_mm25)
plot(pcallLIBOR_S1_xts_mm25)
plot(pcallLIBOR_S1_xts_mm25, type='l')
biplot(pcallLIBOR_S1_xts_mm25)

# first three PCA by using "prcomp" function
library(ade4)

# for 1-Day
pcallLIBOR_S1_xts_mm1 <- prcomp(callLIBOR_S1_xts_mm1)
summary(pcallLIBOR_S1_xts_mm1)
plot(pcallLIBOR_S1_xts_mm1)
plot(pcallLIBOR_S1_xts_mm1, type='l')

stand_devmm1 = pcallLIBOR_S1_xts_mm1[[1]]
var_mm1 = stand_devmm1^2

pc1mm1 = pcallLIBOR_S1_xts_mm1[[2]][,1]
pc2mm1 = pcallLIBOR_S1_xts_mm1[[2]][,2]
pc3mm1 = pcallLIBOR_S1_xts_mm1[[2]][,3]
                                
pcasmm1 = cbind(pc1mm1,pc2mm1,pc3mm1)

# x11()
# component 1 of PCA vs component 2 of PCA for 1-Day
s.corcircle(pcasmm1[,-3], clabel = 0.2, sub="cp1 vs. cp2", possub="topright")
s.arrow(pcasmm1[,-3], clab = .8)


# for 25-Day
pcallLIBOR_S1_xts_mm25 <- prcomp(callLIBOR_S1_xts_mm25)
summary(pcallLIBOR_S1_xts_mm25)
plot(pcallLIBOR_S1_xts_mm25)
plot(pcallLIBOR_S1_xts_mm25, type='l')

stand_devmm25 = pcallLIBOR_S1_xts_mm25[[1]]
var_mm25 = stand_devmm25^2

pc1mm25 = pcallLIBOR_S1_xts_mm25[[2]][,1]
pc2mm25 = pcallLIBOR_S1_xts_mm25[[2]][,2]
pc3mm25 = pcallLIBOR_S1_xts_mm25[[2]][,3]

pcasmm25 = cbind(pc1mm25,pc2mm25,pc3mm25)

# x11()
# component 1 of PCA vs component 2 of PCA for 25-Day
s.corcircle(pcasmm25[,-3], clabel = 0.2, sub="cp1 vs. cp2", possub="topright")
s.arrow(pcasmm25[,-3], clab = .8)


# component 1 of PCA vs component 3 of PCA for 1-Day
s.corcircle(pcasmm1[,-2], clabel = 0.2, sub="cp1 vs. cp3", possub="topright")
s.arrow(pcasmm1[,-2], clab = .8)

# component 1 of PCA vs component 3 of PCA for 25-Day
s.corcircle(pcasmm25[,-2], clabel = 0.2, sub="cp1 vs. cp3", possub="topright")
s.arrow(pcasmm25[,-2], clab = .8)


# component 2 of PCA vs component 3 of PCA for 1-Day
s.corcircle(pcasmm1[,-1], clabel = 0.2, sub="cp2 vs. cp3", possub="topright")
s.arrow(pcasmm1[,-1], clab = .8)

# component 2 of PCA vs component 3 of PCA for 25-Day
s.corcircle(pcasmm25[,-1], clabel = 0.2, sub="cp2 vs. cp3", possub="topright")
s.arrow(pcasmm25[,-1], clab = .8)

pcasmm1CAD = data.frame(pcasmm1[1:12,])
ggplot(pcasmm1CAD,  aes(x = index(pcasmm1CAD))) +
  geom_line(aes(y = pcasmm1CAD[,1], color = "PCA1")) + geom_point(aes(y = pcasmm1CAD[,1], color = "PCA1")) + ggtitle("PCAs' 12 tenur CAD 1-Day") + # omitted after mm
  geom_line(aes(y = pcasmm1CAD[,2], color = "PCA2")) + geom_point(aes(y = pcasmm1CAD[,2], color = "PCA2")) + 
  geom_line(aes(y = pcasmm1CAD[,3], color = "PCA3")) + geom_point(aes(y = pcasmm1CAD[,3], color = "PCA3")) +  xlab("Tenurs") + ylab("PCA") +
  theme(plot.title = element_text(hjust = 0.5))

pcasmm25CAD = data.frame(pcasmm25[1:12,])
ggplot(pcasmm25CAD,  aes(x = index(pcasmm25CAD))) +
  geom_line(aes(y = pcasmm25CAD[,1], color = "PCA1")) + geom_point(aes(y = pcasmm25CAD[,1], color = "PCA1")) + ggtitle("PCAs' 12 tenur CAD 25-Day") + # omitted after mm
  geom_line(aes(y = pcasmm25CAD[,2], color = "PCA2")) + geom_point(aes(y = pcasmm25CAD[,2], color = "PCA2")) + 
  geom_line(aes(y = pcasmm25CAD[,3], color = "PCA3")) + geom_point(aes(y = pcasmm25CAD[,3], color = "PCA3")) +  xlab("Tenurs") + ylab("PCA") +
  theme(plot.title = element_text(hjust = 0.5))

pcasmm1USD = data.frame(pcasmm1[12:24,])
ggplot(pcasmm1USD,  aes(x = index(pcasmm1USD))) +
  geom_line(aes(y = pcasmm1USD[,1], color = "PCA1")) + geom_point(aes(y = pcasmm1USD[,1], color = "PCA1")) + ggtitle("PCAs' 12 tenur USD 1-Day") + # omitted after mm
  geom_line(aes(y = pcasmm1USD[,2], color = "PCA2")) + geom_point(aes(y = pcasmm1USD[,2], color = "PCA2")) + 
  geom_line(aes(y = pcasmm1USD[,3], color = "PCA3")) + geom_point(aes(y = pcasmm1USD[,3], color = "PCA3")) +  xlab("Tenurs") + ylab("PCA") +
  theme(plot.title = element_text(hjust = 0.5))
  
pcasmm25USD = data.frame(pcasmm25[12:24,])
ggplot(pcasmm25USD,  aes(x = index(pcasmm25USD))) +
  geom_line(aes(y = pcasmm25USD[,1], color = "PCA1")) + geom_point(aes(y = pcasmm25USD[,1], color = "PCA1")) + ggtitle("PCAs' 12 tenur USD 25-Day") + # omitted after mm
  geom_line(aes(y = pcasmm25USD[,2], color = "PCA2")) + geom_point(aes(y = pcasmm25USD[,2], color = "PCA2")) + 
  geom_line(aes(y = pcasmm25USD[,3], color = "PCA3")) + geom_point(aes(y = pcasmm25USD[,3], color = "PCA3")) +  xlab("Tenurs") + ylab("PCA") +
  theme(plot.title = element_text(hjust = 0.5))



# ====== Using SDE package fit Vasicek type model to each tenor of USD and CAD ======

rm(list = ls())
require(yuima)

# ================== Brownian Motions ===================
# make it to callLIBOR data
# X=data.frame(ncol=24,nrow=nrow(callLIBOR))
library(sde)
# ex1 .14. R
# set.seed(123)
# par("mar"=c(3 ,2 ,1 ,1))
X11()
i = 24 # i is the column number
par(mfrow=c (2 ,1))
npaths <- 30
N <- nrow(callLIBOR[,i])-1
sigma <- .9
nu <- -5


X <- sde.sim(X0 = callLIBOR[[1,i]], drift = expression (0), sigma = expression(0.5), pred =F, N=N,M= npaths )
Y <- X + nu* time(X)

# for (i in (1:24)){
#   X[,i] <- sde.sim(X0 = callLIBOR[[1,i]], drift = expression (0), sigma = expression(0.5), pred =F, N=N,M= npaths )
#   Y[,i] <- X[,i] + nu* time(X)
# }
girsanov <- exp (0.25 * (-nu/ sigma *X[N ,] - 0.5 *(nu/ sigma )^2))
girsanov <- ( girsanov - min ( girsanov )) / diff ( range ( girsanov ))
col.girsanov <- gray ( girsanov )
matplot ( time (X),Y, type ="l",lty =1, col =" black ",xlab ="t")
matplot ( time (X),Y, type ="l",lty =1, col = col.girsanov , xlab ="t")
# matplot(callLIBOR[,i], type = "l")

BrownLIBOR = matrix(nrow = nrow(Y), ncol = 31)
BrownLIBOR = cbind(Y, callLIBOR[,i])
BrownLIBOR = data.frame(BrownLIBOR)
ggplot(BrownLIBOR,  aes(x = index(BrownLIBOR))) +
  geom_line(aes(y = Y[,1], color = "Brownian")) + #geom_point(aes(y = callLIBORts[,1], color = "CAD1M")) + ggtitle("Term Structure") + # omitted after mm
  geom_line(aes(y = Y[,2], color = "Brownian")) +
  geom_line(aes(y = Y[,3], color = "Brownian")) +
  geom_line(aes(y = Y[,4], color = "Brownian")) +
  geom_line(aes(y = Y[,5], color = "Brownian")) +
  geom_line(aes(y = Y[,6], color = "Brownian")) +
  geom_line(aes(y = Y[,7], color = "Brownian")) +
  geom_line(aes(y = Y[,8], color = "Brownian")) +
  geom_line(aes(y = Y[,9], color = "Brownian")) +
  geom_line(aes(y = Y[,10], color = "Brownian")) +
  geom_line(aes(y = Y[,11], color = "Brownian")) +
  geom_line(aes(y = Y[,12], color = "Brownian")) +
  geom_line(aes(y = Y[,13], color = "Brownian")) +
  geom_line(aes(y = Y[,14], color = "Brownian")) +
  geom_line(aes(y = Y[,15], color = "Brownian")) +
  geom_line(aes(y = Y[,16], color = "Brownian")) +
  geom_line(aes(y = Y[,17], color = "Brownian")) +
  geom_line(aes(y = Y[,18], color = "Brownian")) +
  geom_line(aes(y = Y[,19], color = "Brownian")) +
  geom_line(aes(y = Y[,20], color = "Brownian")) +
  geom_line(aes(y = Y[,21], color = "Brownian")) +
  geom_line(aes(y = Y[,22], color = "Brownian")) +
  geom_line(aes(y = Y[,23], color = "Brownian")) +
  geom_line(aes(y = Y[,24], color = "Brownian")) +
  geom_line(aes(y = Y[,25], color = "Brownian")) +
  geom_line(aes(y = Y[,26], color = "Brownian")) +
  geom_line(aes(y = Y[,27], color = "Brownian")) +
  geom_line(aes(y = Y[,28], color = "Brownian")) +
  geom_line(aes(y = Y[,29], color = "Brownian")) +
  geom_line(aes(y = Y[,30], color = "Brownian")) +
  geom_line(aes(y = BrownLIBOR[,31], color = "CAD TENUR 1M")) #+ geom_point(aes(y = callLIBORts[,2], color = "CAD6M")) +
  # facet_wrap( ~ variable, scales="free")

# ===================== Vasicek Model =================
# make it on callLIBOR data

dcOU <- function (x, t, x0 , theta , log = FALSE ){
  Ex <- theta [1] / theta [2]+( x0 - theta [1] / theta [2]) * exp (- theta [2] *t)
  Vx <- theta [3]^2 *(1- exp (-2* theta [2] *t))/(2* theta [2])
  dnorm (x, mean =Ex , sd = sqrt (Vx), log = log )
}
OU.lik <- function ( theta1 , theta2 , theta3 ){
  n <- length (X)
  dt <- deltat (X)
  -sum ( dcOU (X [2: n], dt , X [1:(n -1)] , c( theta1 , theta2 , theta3 ), log = TRUE ))
}

 # ex3 .01. R
require ( stats4 )
require ( sde )
# set.seed (123)

# results for USD8M
X <- sde.sim (X0 = callLIBOR[[1,20]], model ="OU", theta =c(pcasmm1[[20,1]] ,pcasmm1[[20,2]] ,pcasmm1[[20,3]]) , N =1000 , delta =1)
fit <- mle(OU.lik, start = list ( theta1 =pcasmm1[[20,1]], theta2 =pcasmm1[[20,2]] , theta3 =pcasmm1[[20,3]]), method ="L-BFGS-B", lower =c(-Inf ,0 ,0))
summary ( fit )

# ex3 .01. R ( cont .)
prof <- profile ( fit )
par ( mfrow =c (1 ,3))
plot (prof)
par ( mfrow =c (1 ,1))
vcov ( fit )

# ================= End of the Project code =================


