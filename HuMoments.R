# Hu and "Hue" Moment calcualtors

# duh <- data.table(a=1:3, b=4:6)
# duh[,c('f','g','h'):=test(a,b)]
# test <- function(x, y)
# {
#      return(list(c=x-y, d=x+y, e=x*y))
# }
# mTable <- getMomentTable(x3)

calculateHuMoments <- function(x, wideTable=T, baseName='ImageMoments.NormalizedCentralMoment')
{
	if(wideTable)
	{
		mTable <- getMomentTableFromWideTable(x, baseName)
	}
	else
	{
		mTable <- getMomentTableFromLongTable(x, baseName)
	}
     
     if(nrow(mTable) > 0)
     {
          # MaxOrder - Components - Hu Moments
          # 2 - 40,42 - 1,2
          # 3 - 51,53 - 3,4,5,6,7
          # 4 - 60,62,64 - 8,9,10,11,12
          # 5 - 71,73,75 - 13,14,15,16,17,18

          template <- getMoment(x, wideTable=wideTable, mTable, 1, 1, value=FALSE)
          template$Value <- NA
          maxOrder <- max(mTable[,'orderx'])
          results <- list()
          if(maxOrder <= 1)
          {
               return(NULL)
          }
          if(maxOrder >= 2)
          {
               toAdd <- data.table(Value=M1(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu1'
               results$RHu1 <- toAdd
               
               toAdd <- data.table(Value=M2(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu2'
               results$RHu2 <- toAdd
          }
          if(maxOrder >= 3)
          {
          	
          	toAdd <- data.table(Value=M3(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu3'
               results$RHu3 <- toAdd
               
               toAdd <- data.table(Value=M4(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu4'
               results$RHu4 <- toAdd
               
               toAdd <- data.table(Value=M5(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu5'
               results$RHu5 <- toAdd
               
               toAdd <- data.table(Value=M6(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu6'
               results$RHu6 <- toAdd
               
               toAdd <- data.table(Value=M7(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu7'
               results$RHu7 <- toAdd
          }
          if(maxOrder >= 4)
          {
          	
          	toAdd <- data.table(Value=M8(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu8'
               results$RHu8 <- toAdd
               
               toAdd <- data.table(Value=M9(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu9'
               results$RHu9 <- toAdd
               
               toAdd <- data.table(Value=M10(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu10'
               results$RHu10 <- toAdd
               
               toAdd <- data.table(Value=M11(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu11'
               results$RHu11 <- toAdd
               
               toAdd <- data.table(Value=M12(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu12'
               results$RHu12 <- toAdd
          }
          if(maxOrder >= 4)
          {
          	
          	toAdd <- data.table(Value=M13(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu13'
               results$RHu13 <- toAdd
               
               toAdd <- data.table(Value=M14(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu14'
               results$RHu14 <- toAdd
               
               toAdd <- data.table(Value=M15(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu15'
               results$RHu15 <- toAdd
               
               toAdd <- data.table(Value=M16(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu16'
               results$RHu16 <- toAdd
               
               toAdd <- data.table(Value=M17(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu17'
               results$RHu17 <- toAdd
               
               toAdd <- data.table(Value=M18(x,wideTable,mTable))
               toAdd$Measurement <- 'RHu18'
               results$RHu18 <- toAdd
          }
          
          if(wideTable)
          {
          	for(mName in names(results))
          	{
          		dummyVar <- results[[mName]] # For some reason, I can't integrate this into the next line. I need two lines, using the dummyVar as an intermediate.
          		x[,c(mName):=dummyVar$Value]
          	}
          }
          else
          {
          	ret <- rbindlist(results)
          	x <- rbindlist(list(x, ret), use.names = TRUE)
          }
          
          return(x)
     }
     return(NULL)
}

getMomentTableFromLongTable <- function(x, baseName='ImageMoments.CentralMoment')
{
	theNames <- unique(x[['Measurement']])
	theNames <- theNames[grepl(baseName,theNames, fixed=TRUE)]
	start <- nchar(baseName)
	orders <- substr(theNames, start+1, start+2)
	ret <- data.frame(Measurement=theNames, orderx=as.numeric(substr(orders,1,1)), ordery=as.numeric(substr(orders,2,2)))
	return(ret)
}

getMomentTableFromWideTable <- function(x, baseName='ImageMoments.CentralMoment')
{
	theNames <- unique(names(x))
	theNames <- theNames[grepl(baseName,theNames, fixed=TRUE)]
	start <- nchar(baseName)
	orders <- substr(theNames, start+1, start+2)
	ret <- data.frame(Measurement=theNames, orderx=as.numeric(substr(orders,1,1)), ordery=as.numeric(substr(orders,2,2)))
	return(ret)
}


getMoment <- function(x, wideTable, mTable, order1, order2, value=TRUE)
{
	if(wideTable)
	{
		if(value)
		{
			return(x[,mTable[mTable$orderx==order1 & mTable$ordery==order2,'Measurement'], with=F][[1]])
		}
		else
		{
			return(x[,mTable[mTable$orderx==order1 & mTable$ordery==order2,'Measurement'], with=F])
		}
	}
	else
	{
		if(value)
		{
			return(x[Measurement==mTable[mTable$orderx==order1 & mTable$ordery==order2,'Measurement']]$Value)
		}
		else
		{
			return(x[Measurement==mTable[mTable$orderx==order1 & mTable$ordery==order2,'Measurement']])
		}
	}
}

a40 <- function(x, wideTable, mTable)
{
     # n20 + n02
     return(getMoment(x,wideTable,mTable,2,0) + getMoment(x,wideTable,mTable,0,2))
}

a42 <- function(x, wideTable, mTable)
{
     # n20 - n02
     return(getMoment(x,wideTable,mTable,2,0) - getMoment(x,wideTable,mTable,0,2))
}

b42 <- function(x, wideTable, mTable)
{
     # b42 = 2*n11;
     return(2*getMoment(x,wideTable,mTable,1,1))
}

a51 <- function(x, wideTable, mTable)
{
     # a51 = n30 + n12;
     return(getMoment(x,wideTable,mTable,3,0) + getMoment(x,wideTable,mTable,1,2))
}

b51 <- function(x, wideTable, mTable)
{
     # b51 = n21 + n03;
     return(getMoment(x,wideTable,mTable,2,1) + getMoment(x,wideTable,mTable,0,3))
}

a53 <- function(x, wideTable, mTable)
{
     # a53 = n30 - 3*n12;
     return(getMoment(x,wideTable,mTable,3,0) - 3*getMoment(x,wideTable,mTable,1,2))
}

b53 <- function(x, wideTable, mTable)
{
     # b53 = 3*n21 - n03;
     return(3*getMoment(x,wideTable,mTable,2,1) - getMoment(x,wideTable,mTable,0,3))
}

a60 <- function(x, wideTable, mTable)
{
     # a60 = n40 + 2*n22 + n04;
     return(getMoment(x,wideTable,mTable,4,0) + 2*getMoment(x,wideTable,mTable,2,2) + getMoment(x,wideTable,mTable,0,4))
}

a62 <- function(x, wideTable, mTable)
{
     # a62 = n40 - n04;
     return(getMoment(x,wideTable,mTable,4,0) - getMoment(x,wideTable,mTable,0,4))
}

b62 <- function(x, wideTable, mTable)
{
     # b62 = 2*(n31 + n13);
     return(2*(getMoment(x,wideTable,mTable,3,1) + getMoment(x,wideTable,mTable,1,3)))
}

a64 <- function(x, wideTable, mTable)
{
     # a64 = n40 - 6*n22 + n04;
     return(getMoment(x,wideTable,mTable,4,0) - 6*getMoment(x,wideTable,mTable,2,2) + getMoment(x,wideTable,mTable,0,4))
}

b64 <- function(x, wideTable, mTable)
{
     # b64 = 4*(n31 - n13);
     return(4*(getMoment(x,wideTable,mTable,3,1) - getMoment(x,wideTable,mTable,1,3)))
}

a71 <- function(x, wideTable, mTable)
{
     # a71 = n50 + 2*n32 + n14;
     return(getMoment(x,wideTable,mTable,5,0) + 2*getMoment(x,wideTable,mTable,3,2) + getMoment(x,wideTable,mTable,1,4))
}

b71 <- function(x, wideTable, mTable)
{
     # b71 = n41 + 2*n23 + n05;
     return(getMoment(x,wideTable,mTable,4,1) + 2*getMoment(x,wideTable,mTable,2,3) + getMoment(x,wideTable,mTable,0,5))
}

a73 <- function(x, wideTable, mTable)
{
     # a73 = n50 - 2*n32 - 3*n14;
     return(getMoment(x,wideTable,mTable,5,0) - 2*getMoment(x,wideTable,mTable,3,2) - 3*getMoment(x,wideTable,mTable,1,4))
}

b73 <- function(x, wideTable, mTable)
{
     # b73 = 3*n41 + 2*n23 - n05;
     return(3*getMoment(x,wideTable,mTable,4,1) + 2*getMoment(x,wideTable,mTable,2,3) - getMoment(x,wideTable,mTable,0,5))
}

a75 <- function(x, wideTable, mTable)
{
     # a75 = n50 - 10*n32 + 5*n14;
     return(getMoment(x,wideTable,mTable,5,0) - 10*getMoment(x,wideTable,mTable,3,2) + 5*getMoment(x,wideTable,mTable,1,4))
}

b75 <- function(x, wideTable, mTable)
{
     # b75 = 5*n41 - 10*n23 + n05;
     return(5*getMoment(x,wideTable,mTable,4,1) - 10*getMoment(x,wideTable,mTable,2,3) + getMoment(x,wideTable,mTable,0,5))
}

M1 <- function(x, wideTable, mTable)
{
     # M(1) = a40;
     return(a40(x,wideTable,mTable))
}

M2 <- function(x, wideTable, mTable)
{
     # M(2)  = a42^2 + b42^2;
     return(a42(x,wideTable,mTable)^2 + b42(x,wideTable,mTable)^2)
}

M3 <- function(x, wideTable, mTable)
{
     # M(3)  = a53^2 + b53^2;
     return(a53(x,wideTable,mTable)^2 + b53(x,wideTable,mTable)^2)
}

M4 <- function(x, wideTable, mTable)
{
     # M(4)  = a51^2 + b51^2;
     return(a51(x,wideTable,mTable)^2 + b51(x,wideTable,mTable)^2)
}

M5 <- function(x, wideTable, mTable)
{
     # M(5)  = a51*a53*(a51^2 - 3*b51^2) + b51*b53*(3*a51^2 - b51^2);
     return(a51(x,wideTable,mTable)*a53(x,wideTable,mTable)*(a51(x,wideTable,mTable)^2 - 3*b51(x,wideTable,mTable)^2) + b51(x,wideTable,mTable)*b53(x,wideTable,mTable)*(3*a51(x,wideTable,mTable)^2 - b51(x,wideTable,mTable)^2))
}

M6 <- function(x, wideTable, mTable)
{
     # M(6)  = a42*(a51^2 - b51^2) + 2*a51*b51*b42;
     aa51 <- a51(x,wideTable,mTable)
     return(a42(x,wideTable,mTable)*(a51(x,wideTable,mTable)^2 - b51(x,wideTable,mTable)^2) + 2*a51(x,wideTable,mTable)*b51(x,wideTable,mTable)*b42(x,wideTable,mTable))
}

M7 <- function(x, wideTable, mTable)
{
     # M(7)  = a51*b53*(a51^2 - 3*b51^2) - b51*a53*(3*a51^2 - b51^2);
     aa51 <- a51(x,wideTable,mTable)
     bb51 <- b51(x,wideTable,mTable)
     return(aa51*b53(x,wideTable,mTable)*(aa51^2 - 3*bb51^2) - bb51*a53(x,wideTable,mTable)*(3*aa51^2 - bb51^2))
}

M8 <- function(x, wideTable, mTable)
{
     # M(8)  = a60;
     return(a60(x,wideTable,mTable))
}

M9 <- function(x, wideTable, mTable)
{
     # M(9)  = a62^2 + b62^2;
     return(a62(x,wideTable,mTable)^2 + b62(x,wideTable,mTable)^2)
}

M10 <- function(x, wideTable, mTable)
{
     # M(10) = a64^2 + b64^2;
     return(a64(x,wideTable,mTable)^2 + b64(x,wideTable,mTable)^2)
}

M11<- function(x, wideTable, mTable)
{
     # M(11) = a64*(a62^2 - b62^2) + 2*a62*b62*b64;
     return(a64(x,wideTable,mTable)*(a62(x,wideTable,mTable)^2 - b62(x,wideTable,mTable)^2) + 2*a62(x,wideTable,mTable)*b62(x,wideTable,mTable)*b64(x,wideTable,mTable))
}

M12 <- function(x, wideTable, mTable)
{
     # M(12) = b64*(a62^2 - b62^2) - 2*a62*b62*a64;
     return(b64(x,wideTable,mTable)*(a62(x,wideTable,mTable)^2 - b62(x,wideTable,mTable)^2) - 2*a62(x,wideTable,mTable)*b62(x,wideTable,mTable)*a64(x,wideTable,mTable))
}

M13 <- function(x, wideTable, mTable)
{
     # M(13) = a62*(a71^2 - b71^2) + 2*a71*b71*b62;
     return(a62(x,wideTable,mTable)*(a71(x,wideTable,mTable)^2 - b71(x,wideTable,mTable)^2) + 2*a71(x,wideTable,mTable)*b71(x,wideTable,mTable)*b62(x,wideTable,mTable))
}

M14 <- function(x, wideTable, mTable)
{
     # M(14) = a71^2 + b71^2;
     return(a71(x,wideTable,mTable)^2 + b71(x,wideTable,mTable)^2)
}

M15<- function(x, wideTable, mTable)
{
     # M(15) = a73^2 + b73^2;
     return(a73(x,wideTable,mTable)^2 + b73(x,wideTable,mTable)^2)
}

M16<- function(x, wideTable, mTable)
{
     # M(16) = a75^2 + b75^2;
     return(a75(x,wideTable,mTable)^2 + b75(x,wideTable,mTable)^2)
}

M17<- function(x, wideTable, mTable)
{
     # M(17) = a71*a73*(a71^2 - 3*b71^2) + b71*b73*(3*a71^2 - b71^2);
     return(a71(x,wideTable,mTable)*a73(x,wideTable,mTable)*(a71(x,wideTable,mTable)^2 - 3*b71(x,wideTable,mTable)^2) + b71(x,wideTable,mTable)*b73(x,wideTable,mTable)*(3*a71(x,wideTable,mTable)^2 - b71(x,wideTable,mTable)^2))
}

M18<- function(x, wideTable, mTable)
{
     # M(18) = a71*a73*(a71^2 - 3*b71^2) + b71*b73*(3*a71^2 - b71^2);
     return(a71(x,wideTable,mTable)*a73(x,wideTable,mTable)*(a71(x,wideTable,mTable)^2 - 3*b71(x,wideTable,mTable)^2) + b71(x,wideTable,mTable)*b73(x,wideTable,mTable)*(3*a71(x,wideTable,mTable)^2 - b71(x,wideTable,mTable)^2))
}





