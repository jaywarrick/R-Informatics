# Zernike Moment calcualtions

#' JEX outputs an affine invariant Zernike moment (position, scale, & rotation)
#' In order to make it inensity invariant, the magnitude must be divided by the mean.
#' This is meant to be used with long tables.
#' 
#' Not sure yet whether to call this before or after calculating dot products (maybe)
meanNormalizeZernikeMoments <- function(x, idCols='cId')
{
	mNames <- getMeasurementNamesContaining(x, 'ZernikeMag')
	for(mName in mNames)
	{
		x <- divideMAbyMBbyRef(x, mName, 'Stats.Mean', idCols=idCols)
	}
	return(x)
}

# Do this AFTER the '_Order_' and '_Rep_' strings have been removed from names
# Do this BEFORE differencing channels
# Do this BEFORE standardizing any data
calculateZernikeDotProduct <- function(x, imageChannelValsToPermute=unique(x$ImageChannel)[!grepl('_',unique(x$ImageChannel,fixed=T))])
{
	calculateZernikeReAndIm(x)
	reProducts <- calculateChannelProducts(x, sep='_', comboCol='ImageChannel', valsToPermute=imageChannelValsToPermute, idCols=c('cId','MaskChannel'), mColContains='ZernikeRe')
	imProducts <- calculateChannelProducts(x, sep='_', comboCol='ImageChannel', valsToPermute=imageChannelValsToPermute, idCols=c('cId','MaskChannel'), mColContains='ZernikeIm')
	ret <- addReAndImProducts(reProducts=reProducts, imProducts=imProducts)
	removeColsContaining(x, c('ZernikeRe','ZernikeIm'))
	x <- merge(x, ret, all=T)
	return(x)
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
}

# This is a helper function for calculateZernikeDotProduct.
addReAndImProducts <- function(reProducts, imProducts)
{
	reNames <- getColNamesContaining(reProducts, 'ZernikeRe')
	dotNames <- gsub('Re','Dot',reNames)
	for(reName in reNames)
	{
		imName <- gsub('Re','Im',reName)
		if(!(imName %in% names(x)))
		{
			next
		}
		reProducts[, (reName):=get(reName) + imProducts[[imName]]]
	}
	setnames(reProducts, reNames, dotNames)
	return(reProducts)
	
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

# Do this AFTER the '_Order_' and '_Rep_' strings have been removed from names (part of fixColNames())
getZernikeOrderReps <- function(x)
{
	ret <- unique(tstrsplit(tstrsplit(getColNamesContaining(x, 'ZernikeMag'), 'Mag', keep=2L)[[1]], '_', keep=1L)[[1]])
	return(ret)
}
