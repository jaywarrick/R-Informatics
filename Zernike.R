# Zernike Moment calcualtions

#zTable <- getMomentTable(x3, baseName='ZernikeMag')

getZernikeOrderReps <- function(x)
{
     ret <- unique(tstrsplit(tstrsplit(getColNamesContaining(x2, 'ZernikeMag'), 'Mag', keep=2L)[[1]], '_', keep=1L)[[1]])
     return(ret)
}

# This gets called on the entire table to convert Mag and Phase into Re and Im.
calculateZernikeReAndIm <- function(x)
{
     magNames <- getColNamesContaining(x, 'ZernikeMag')
     for(magName in magNames)
     {
          phaseName <- gsub('Mag','Phase',magName)
          if(!(phaseName %in% names(x)))
          {
               next
          }
          reName <- gsub('Mag','Re',magName)
          imName <- gsub('Mag','Im',magName)
          x[, (reName):=get(magName) * cosd(get(phaseName))]
          x[, (imName):=get(magName) * sind(get(phaseName))]
     }
     return(x[])
     # mags <- x[grepl('ZernikeMag',Measurement,fixed=T)]
     # phases <- x[grepl('ZernikePhase',Measurement,fixed=T)]
     # result1 <- copy(mags)
     # result1[,Measurement:=gsub('ZernikeMag','ZernikeRe',Measurement)]
     # result1$Value <- mags$Value * cosd(phases$Value)
     # result2 <- copy(mags)
     # result2[,Measurement:=gsub('ZernikeMag','ZernikeIm',Measurement)]
     # result2$Value <- mags$Value * sind(phases$Value)
     # ret <- rbindlist(list(result1,result2),use.names=TRUE)
     # return(ret)
}

# This gets called on the entire table to convert Mag and Phase into Re and Im.
addReAndImParts <- function(x)
{
     reNames <- getColNamesContaining(x, 'ZernikeRe')
     for(reName in reNames)
     {
          imName <- gsub('Re','Im',reName)
          if(!(imName %in% names(x)))
          {
               next
          }
          x[, (reName):=get(magName) * cosd(get(phaseName))]
          x[, (imName):=get(magName) * sind(get(phaseName))]
     }
     return(x)
     
     # mags <- x[grepl('ZernikeMag',Measurement,fixed=T)]
     # phases <- x[grepl('ZernikePhase',Measurement,fixed=T)]
     # result1 <- copy(mags)
     # result1[,Measurement:=gsub('ZernikeMag','ZernikeRe',Measurement)]
     # result1$Value <- mags$Value * cosd(phases$Value)
     # result2 <- copy(mags)
     # result2[,Measurement:=gsub('ZernikeMag','ZernikeIm',Measurement)]
     # result2$Value <- mags$Value * sind(phases$Value)
     # ret <- rbindlist(list(result1,result2),use.names=TRUE)
     # return(ret)
}

# Do this AFTER the '_Order_' and '_Rep_' strings have been removed from names
# Do this BEFORE differencing channels
# Do this BEFORE standardizing any data
calculateZernikeDotProduct <- function(x)
{
     y <- calculateZernikeReAndIm(x2)
     comboProducts <- calculateChannelProducts(y)
     reProducts <- comboProducts[grepl('ZernikeRe',Measurement,fixed=T)]
     imProducts <- comboProducts[grepl('ZernikeIm',Measurement,fixed=T)]
     ret <- copy(reProducts)
     ret[,ImageChannel:=gsub('_times_','_dot_',ImageChannel)]
     ret[,Measurement:=gsub('ZernikeRe','ZernikeDot',Measurement)]
     ret[,Value:=NULL]
     ret$Value <- reProducts$Value + imProducts$Value
     x <- rbindlist(list(x, ret), use.names = TRUE)
     return(x)
}



