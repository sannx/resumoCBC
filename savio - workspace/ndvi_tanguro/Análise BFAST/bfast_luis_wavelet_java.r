library(bfast)
library(strucchange)
library(wavelets)

###
###
h = 0.06099599;
season = 'harmonic';
strDates <- c('2000-02-18','2000-03-05','2000-03-21','2000-04-06','2000-04-22','2000-05-08','2000-05-24','2000-06-09','2000-06-25','2000-07-11','2000-07-27','2000-08-12','2000-08-28','2000-09-13','2000-09-29','2000-10-15','2000-10-31','2000-11-16','2000-12-02','2000-12-18','2001-01-01','2001-01-17','2001-02-02','2001-02-18','2001-03-06','2001-03-22','2001-04-07','2001-04-23','2001-05-09','2001-05-25','2001-06-10','2001-06-26','2001-07-12','2001-07-28','2001-08-13','2001-08-29','2001-09-14','2001-09-30','2001-10-16','2001-11-01','2001-11-17','2001-12-03','2001-12-19','2002-01-01','2002-01-17','2002-02-02','2002-02-18','2002-03-06','2002-03-22','2002-04-07','2002-04-23','2002-05-09','2002-05-25','2002-06-10','2002-06-26','2002-07-12','2002-07-28','2002-08-13','2002-08-29','2002-09-14','2002-09-30','2002-10-16','2002-11-01','2002-11-17','2002-12-03','2002-12-19','2003-01-01','2003-01-17','2003-02-02','2003-02-18','2003-03-06','2003-03-22','2003-04-07','2003-04-23','2003-05-09','2003-05-25','2003-06-10','2003-06-26','2003-07-12','2003-07-28','2003-08-13','2003-08-29','2003-09-14','2003-09-30','2003-10-16','2003-11-01','2003-11-17','2003-12-03','2003-12-19','2004-01-01','2004-01-17','2004-02-02','2004-02-18','2004-03-05','2004-03-21','2004-04-06','2004-04-22','2004-05-08','2004-05-24','2004-06-09','2004-06-25','2004-07-11','2004-07-27','2004-08-12','2004-08-28','2004-09-13','2004-09-29','2004-10-15','2004-10-31','2004-11-16','2004-12-02','2004-12-18','2005-01-01','2005-01-17','2005-02-02','2005-02-18','2005-03-06','2005-03-22','2005-04-07','2005-04-23','2005-05-09','2005-05-25','2005-06-10','2005-06-26','2005-07-12','2005-07-28','2005-08-13','2005-08-29','2005-09-14','2005-09-30','2005-10-16','2005-11-01','2005-11-17','2005-12-03','2005-12-19','2006-01-01','2006-01-17','2006-02-02','2006-02-18','2006-03-06','2006-03-22','2006-04-07','2006-04-23','2006-05-09','2006-05-25','2006-06-10','2006-06-26','2006-07-12','2006-07-28','2006-08-13','2006-08-29','2006-09-14','2006-09-30','2006-10-16','2006-11-01','2006-11-17','2006-12-03','2006-12-19','2007-01-01','2007-01-17','2007-02-02','2007-02-18','2007-03-06','2007-03-22','2007-04-07','2007-04-23','2007-05-09','2007-05-25','2007-06-10','2007-06-26','2007-07-12','2007-07-28','2007-08-13','2007-08-29','2007-09-14','2007-09-30','2007-10-16','2007-11-01','2007-11-17','2007-12-03','2007-12-19','2008-01-01','2008-01-17','2008-02-02','2008-02-18','2008-03-05','2008-03-21','2008-04-06','2008-04-22','2008-05-08','2008-05-24','2008-06-09','2008-06-25','2008-07-11','2008-07-27','2008-08-12','2008-08-28','2008-09-13','2008-09-29','2008-10-15','2008-10-31','2008-11-16','2008-12-02','2008-12-18','2009-01-01','2009-01-17','2009-02-02','2009-02-18','2009-03-06','2009-03-22','2009-04-07','2009-04-23','2009-05-09','2009-05-25','2009-06-10','2009-06-26','2009-07-12','2009-07-28','2009-08-13','2009-08-29','2009-09-14','2009-09-30','2009-10-16','2009-11-01','2009-11-17','2009-12-03','2009-12-19','2010-01-01','2010-01-17','2010-02-02','2010-02-18','2010-03-06','2010-03-22','2010-04-07','2010-04-23','2010-05-09','2010-05-25','2010-06-10','2010-06-26','2010-07-12','2010-07-28','2010-08-13','2010-08-29','2010-09-14','2010-09-30','2010-10-16','2010-11-01','2010-11-17','2010-12-03','2010-12-19','2011-01-01','2011-01-17','2011-02-02','2011-02-18','2011-03-06','2011-03-22','2011-04-07','2011-04-23','2011-05-09','2011-05-25','2011-06-10','2011-06-26','2011-07-12','2011-07-28','2011-08-13','2011-08-29','2011-09-14','2011-09-30','2011-10-16','2011-11-01','2011-11-17','2011-12-03','2011-12-19','2012-01-01','2012-01-17','2012-02-02','2012-02-18','2012-03-05','2012-03-21','2012-04-06','2012-04-22','2012-05-08','2012-05-24','2012-06-09','2012-06-25','2012-07-11','2012-07-27','2012-08-12','2012-08-28','2012-09-13','2012-09-29','2012-10-15','2012-10-31','2012-11-16','2012-12-02','2012-12-18','2013-01-01','2013-01-17','2013-02-02','2013-02-18','2013-03-06','2013-03-22','2013-04-07','2013-04-23','2013-05-09','2013-05-25','2013-06-10','2013-06-26','2013-07-12','2013-07-28','2013-08-13',
              '2013-08-29','2013-09-14','2013-09-30','2013-10-16','2013-11-01','2013-11-17','2013-12-03','2013-12-19','2014-01-01','2014-01-17','2014-02-02','2014-02-18','2014-03-06','2014-03-22','2014-04-07','2014-04-23','2014-05-09','2014-05-25','2014-06-10','2014-06-26','2014-07-12','2014-07-28','2014-08-13','2014-08-29','2014-09-14','2014-09-30','2014-10-16','2014-11-01','2014-11-17','2014-12-03','2014-12-19','2015-01-01','2015-01-17','2015-02-02','2015-02-18','2015-03-06','2015-03-22','2015-04-07','2015-04-23','2015-05-09','2015-05-25','2015-06-10','2015-06-26','2015-07-12','2015-07-28','2015-08-13','2015-08-29','2015-09-14','2015-09-30','2015-10-16','2015-11-01','2015-11-17','2015-12-03','2015-12-19','2016-01-01','2016-01-17','2016-02-02','2016-02-18','2016-03-05','2016-03-21','2016-04-06','2016-04-22','2016-05-08');
inputFile = "/home/savio/R_workspace/ndvi_tanguro/DADOS/bfast_input/bfast_input.txt"

args = commandArgs(trailingOnly=TRUE)
h = if(!is.na(args[1])) as.numeric(args[1]) else h
season = if(!is.na(args[2])) as.character(args[2]) else season
strDates = if(!is.na(args[3])) eval(parse(text=as.character(args[3]))) else strDates
dates <- as.Date(strDates)
wavelets_dates <- dates[seq(1,length(dates),2)]
inputFile = if(!is.na(args[4])) as.character(args[4]) else inputFile

#read values splited by lines
rows <- read.csv(file=inputFile, header=FALSE, sep=",")

sart.timet = Sys.time()
bfit = sapply(1:nrow(rows), function(i){
  #read each line values
  values = as.numeric(rows[i,])

  if(all(is.na(values))) {c(NA,NA,NA,NA)} else{
    sart.timet = Sys.time()
    
    pixel = as.numeric(values)
    pixel[is.na(pixel)] = 0
    dwtobj <- dwt(pixel[2:length(pixel)], filter = "d2", n.levels=2)
    wavelet_data = dwtobj@V$V1
    
    dat = bfastts(wavelet_data[,1], dates[1:187], type = c("16-day"))
    
    time.dat=as.vector(time(dat))               ## variável tempo.
    stl_dat=stl(dat, "per",robust=TRUE)
    dat=stl_dat$time.series[,2]+stl_dat$time.series[,3]
    ti <- time(dat)
    bp.dat <- breakpoints(dat ~ 1,h=h)       ## estima os break points.OBS: usa um modelo mais simples que o bfast usual. 

    breaks = bp.dat$breakpoints
    lastBreak = if(!all(is.na(breaks))) breaks[length(breaks)] else 1
    lastBreakDate = wavelets_dates[lastBreak]
    lastBreakJulianDate = format(lastBreakDate, "%Y%j")

    fm1 <- lm(dat ~ time.dat*breakfactor(bp.dat, breaks = length(bp.dat$breakpoints))) ## estima a tendência linear    
    Tt <- ts(fitted(fm1))
    tsp(Tt) <- tsp(dat)
    lastSegment = Tt[lastBreak:length(Tt)]
    slope = (lastSegment[2*floor(length(lastSegment)/3)] - lastSegment[floor(length(lastSegment)/3)])/(2*floor(length(lastSegment)/3) - floor(length(lastSegment)/3))
    
    time = Sys.time() - sart.timet
    return(c(as.character(rows[i,][1,1]), time, as.numeric(lastBreakJulianDate), as.numeric(slope)))
  }
}
)

for(i in 1:ncol(bfit)) {
  cat(bfit[,i], "\n")
}
time = Sys.time() - sart.timet  
# cat(time)