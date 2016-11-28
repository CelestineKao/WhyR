load("SFO_LAXweather2011.rda")

####GLOBALS######
monthNames = c("January", "February", "March", "April",
               "May", "June", "July", "August", "September",
               "October", "November", "December")
daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                31, 30, 31, 30, 31)
cumDays = cumsum(c(1, daysInMonth))
monthLabels = c()
for(i in 1:12){
  monthLabels = c(monthLabels,floor(median(cumDays[i]:cumDays[i+1])))
}

makePlotRegion = function( p_xlim, p_ylim, bgcolor, ylabels, margins, xtop) {
  
  par(mar = margins, yaxs = 'i', xaxs = 'i', bg=bgcolor)
  plot(NULL, xlim= p_xlim, ylim= p_ylim, yaxt="n", xaxt="n", xlab = '', ylab = '', frame.plot = FALSE)
  
  par(mgp = c(0,.5,0))
  if(xtop == TRUE){
    axis(3, at=monthLabels, tck = 0, cex.axis = .8, labels = monthNames,
        lwd = 0, col.axis="wheat4", font = 2)
  }
  else{
    axis(1, at=monthLabels, tck = 0, cex.axis = .8, labels = monthNames,
         lwd = 0, col.axis="wheat4", font = 2)
  }
  
  if(p_ylim[length(p_ylim)] >20){
    axis(2, at= ylabels,labels = paste0(ylabels,intToUtf8(176)), 
         las=2, tck=0, cex.axis = .7, col.axis = "wheat4", col = "wheat3", lwd = 0)
    axis(4, at= ylabels,labels = paste0(ylabels,intToUtf8(176)), 
         las=2, tck=0, cex.axis = .7, col.axis = "wheat4", col = "wheat3", lwd = 0)
  }
  else{
    axis(2, at= ylabels,labels = TRUE,
         las=2, tck=0, cex.axis = .7, col.axis = "wheat4", col = "wheat3", lwd = 0)
    axis(4, at= ylabels, labels = TRUE,
         las=2, tck=0, cex.axis = .7, col.axis = "wheat4", col = "wheat3", lwd = 0)
  }

}

drawTempRegion = function(high, low, col){
  # This plot will produce 365 rectangles, one for each day
  # It will be used for the record Temps, normal Temps, and 
  # observed Temps
  
  # day - a numeric vector of 365 dates
  # high - a numeric vector of 365 high Temperatures
  # low - a numeric vector of 365 low Temperatures
  # col - color to fill the rectangles
  
  rect(xleft = seq(1,365), xright = seq(2,366), ybottom = low, ytop =high, 
       col = col, border = NA)
  
}
addGrid = function(location, col, ltype, vertical = TRUE) {
  # This function adds a set of parallel grid lines
  # It will be used to place vertical and horizontal lines
  # on both Temp and Precip plots
  
  # location is a numeric vector of locations for the lines
  # col - the color to make the lines
  # ltype - the type of line to make
  # vertical - indicates whether the lines are vertical or horizontal
  
  for(loc in location){
    if(vertical == TRUE){
      abline(v=loc,col=col,lty=ltype,lwd=1)
    }
    else{
      abline(h=loc,col=col,lty=ltype, lwd=1)
    }
  }
  
}

monthPrecip = function(day, dailyPrecip, normal){
  # This function adds one month's Precipitation to the 
  #   Precipitation plot.
  # It will be called 12 times, once for each month
  # It creates the cumulative Precipitation curve,
  # fills the area below with color, add the total
  # Precipitation for the month, and adds a reference
  # line and text for the normal value for the month
  
  # day a numeric vector of dates for the month
  # dailyPrecip a numeric vector of Precipitation recorded
  # for the month (any NAs can be set to 0)
  # normal a single value, which is the normal total Precip
  #  for the month
  
  dailyPrecip[is.na(dailyPrecip)] = 0
  cumulative = cumsum(dailyPrecip)
  lines(day,cumulative, type="l", col="turquoise4")
  day = c(min(day),day, max(day))
  cumulative = c(0,cumulative,0)
  polygon(day, cumulative, col="wheat2", border = NA)
  segments(min(day),normal, max(day), normal, col="steelblue3", lty = 3)
  text(day[1], normal+.25, paste(normal), pos = 4, cex=.7, col="wheat4", offset = .1, font=3)
  
}

finalPlot = function(Temp, Precip){
  pdf("HW9_plot_139", width = 8 , height = 11 )
  layout(matrix(c(1,2), 2, 1, byrow=TRUE), heights = c(2,1))

  
  TempLowerBound = min(min(Temp$Low, na.rm = TRUE), 
                       min(Temp$RecordLow, na.rm = TRUE),
                       min(Temp$NormalLow, na.rm=TRUE))
  TempUpperBound = max(max(Temp$High, na.rm = TRUE), 
                       max(Temp$RecordHigh, na.rm = TRUE),
                       max(Temp$NormalHigh, na.rm=TRUE))
  nice_lower = 10*round((TempLowerBound-3)/10)  
  nice_upper = 10*round((TempUpperBound+3)/10) 
  nice_TempSequence = seq(nice_lower, nice_upper,10)
  
  makePlotRegion(c(0,366), c(nice_lower,nice_upper), "white", 
                 nice_TempSequence, c(2,2,3.5,2), TRUE)
  
  
  #NA's exist in observed values, replace with corresponding norm value
  na_index = which(is.na(Temp$Low))
  Temp$Low[na_index] = Temp$NormalLow[na_index]
  na_index = which(is.na(Temp$High))
  Temp$High[na_index] = Temp$NormalHigh[na_index]
 
  drawTempRegion(Temp$RecordHigh, Temp$RecordLow, "wheat2")
  drawTempRegion(Temp$NormalHigh, Temp$NormalLow, "wheat3")
  drawTempRegion(Temp$High, Temp$Low, "violetred4")
  
  for(i in 1:365){
    abline(v=i, lty = 1, lwd = 1, col="white")
  }
  
  abline(v=0, lty=1, lwd=5,col = "wheat3")
  abline(v=366, lty=1, lwd=5,col = "wheat3")
  
  ### Call addGrid to add the grid lines to the plot
  addGrid(cumDays[c(-1, -1*length(cumDays))], "wheat4", lty = 3, vertical = TRUE)
  addGrid(nice_TempSequence[c(-1, -1*length(nice_TempSequence))], "white", lty = 1, vertical = FALSE)
  
  ### Add the markers for the record breaking days
  # I think there is a flaw in the data. Days 337 and 339 indicate that 2011 was a record breaking low year.
  # However, the observed Temps and records Temps don't support that. 49 vs 42, 43 vs 36 respectively.
  # I also can't tell which years are tied, since the Low/High years appear to be updated with 2011,
  # so I can't know if a prior year held the records. That being said, I am going to ignore 337 and 339.
  lowest = which(Temp$LowYr == 2011 & Temp$Low <= Temp$RecordLow)
  highest = which(Temp$HiYear ==2011)
  for(i in 1:length(lowest)){
    segments(lowest[i]+.5,Temp$Low[lowest[i]], lowest[i]+.5, Temp$Low[lowest[i]]-15, col = "wheat4")
    if(i%%2 == 1){
      text(lowest[i]+.5,Temp$Low[lowest[i]]-14.5, pos = 2,cex=.7,offset = .1, col = "wheat4",
           labels= paste("RECORD LOW:", paste0(Temp$Low[i],intToUtf8(176)),sep = " "))
    }
    else{
      text(lowest[i]+.5,Temp$Low[lowest[i]]-14.5, pos = 4,cex=.7,offset = .1,col = "wheat4",
           labels= paste("RECORD LOW:", paste0(Temp$Low[i],intToUtf8(176)),sep = " "))
    }
  }
  for(day in highest){
    segments(day+.5,Temp$High[day], day+.5, Temp$High[day]+15, col = "wheat4")
    text(day+.5,Temp$High[day]+14.5, pos = 4, cex=.7, offset = .1,col = "wheat4",
         labels= paste("RECORD HIGH:", paste0(Temp$High[day],intToUtf8(176)),sep = " "))
  }
  
  
  ### Add the titles 
  mtext("Los Angeles City's Weather in 2011", side = 3, adj=0,
        line = 2, col = "wheat4", font = 2)
  
  ### Call makePlotRegion to create the plotting region
  ### for the Precipitation plot
  Precip$precip= as.numeric(as.character(Precip$precip))
  Precip$normal= as.numeric(as.character(Precip$normal))
  maxPrecip =  max(Precip$precip, na.rm = TRUE)
  nice_Precip = seq(0, .5*round((maxPrecip+.5)/.5),1)
  
  makePlotRegion(c(0,366), c(0,.5*round((maxPrecip+.5)/.5)), "white", 
                 nice_Precip, c(2,2,2,2), FALSE)
  
  for(i in 1:12){
    monthPrecip(cumDays[i]:cumDays[i+1], Temp$Precip[cumDays[i]:cumDays[i+1]], Precip$normal[i] )
  }
  
  ### Call addGrid to add the grid lines to the plot
  abline(v=0, lty=1, lwd=5,col = "wheat3")
  abline(v=366, lty=1, lwd=5,col = "wheat3")
  addGrid(nice_Precip[c(-1, -1*length(nice_Precip))], "white", lty = 1, vertical = FALSE)
  addGrid(cumDays[c(-1, -1*length(cumDays))], "wheat4", lty = 3, vertical = TRUE)
  
  for(i in 1:12){
    day = cumDays[i]:cumDays[i+1]
    dailyPrecip = Temp$precip[cumDays[i]:cumDays[i+1]]
    normal = Precip$normal[i] 
    cumulative = cumsum(dailyPrecip)
  
    if(day[1] == 1){
      text(day[length(day)], Precip$precip[i]+.25, paste("ACTUAL",Precip$precip[i], sep=" "),
          pos = 2, cex=.7, col="wheat4", offset = .1, font=1)  
    }
    else{
      text(day[length(day)], Precip$precip[i]+.25, Precip$precip[i],
          pos = 2, cex=.7, col="wheat4", offset = .1, font=1)  
    }
    text(day[1], normal+.25, paste(normal), pos = 4, cex=.7, col="wheat4", offset = .1, font=3)
  }
  ### Add the titles
  text(1, Precip$normal[1]+.5, "NORMAL", pos = 4, cex=.7, col="wheat4", offset = .1, font=3)
  mtext("Preciptation", side = 3, adj=0,
        line = .25, col = "wheat4", font = 2)
  total_p = sum(Precip$precip)
  total_n = sum(Precip$normal)
  
  dev.off()
  
}
finalPlot(Temp = laxWeather, Precip = laxMonthlyPrecip)



