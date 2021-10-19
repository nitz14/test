luna = function(x){length(unique(x))}


year <- function(date_string){
	substring(date_string, 1, 4)
}


month <- function(date_string){
	substring(date_string, 6, 7)
}


rep.row <- function(x,n){
	matrix(rep(x,each=n), nrow=n)
}


# When the DQET files have been opened in Excel and saved in Excel, even though they are CSVs, the dates in the formats
# yyyy-mm-dd will have been converted into an Excel standard such as d/m/yyyy. If we then try to read them in using the
# functions here, they would fail without the correction included below which replaces:
#	year_dqet  <- year(max(unique(DQET$publish_date_dd_mm_yyyy)))
#	month_dqet <- month(max(unique(DQET$publish_date_dd_mm_yyyy)))

get.max.month.year.from.data <- function(DQET){   
	
	RFormat <- grepl("-", DQET$publish_date_dd_mm_yyyy)
	if(RFormat[1]){   
		year_dqet  	<- year(max(unique(as.Date(x=DQET$publish_date_dd_mm_yyyy, format="%Y-%m-%d"))))
		month_dqet 	<- month(max(unique(as.Date(x=DQET$publish_date_dd_mm_yyyy, format="%Y-%m-%d"))))
	}else{
		year_dqet  	<- year(max(unique(as.Date(x=DQET$publish_date_dd_mm_yyyy, format="%d/%m/%Y"))))
		month_dqet 	<- month(max(unique(as.Date(x=DQET$publish_date_dd_mm_yyyy, format="%d/%m/%Y"))))
	}
	
	month_dqet 		<- add.leading.zero.to.month(month_dqet)
	month_char 		<- as.Date(paste(year_dqet,month_dqet,"01",sep="-"))
	
	return(month_char)
}


add.leading.zero.to.month <- function(month_input){
	
	month_input <- as.numeric(month_input)
	month_with_leading_zero = ""
	if(month_input<10){
		month_with_leading_zero = paste('0', as.character(month_input), sep = '')
	}else{
		month_with_leading_zero = as.character(month_input)
	}

	return(month_with_leading_zero)
}


year.month.convert.to.string <- function(year_month_list){
  	
	year                   <- as.character(year_month_list[["year"]])
	month                  <- add.leading.zero.to.month(year_month_list[["month"]])
	year_month_as_string   <- paste(year, month, sep="")
	
	return(year_month_as_string)
}


year.month.increase <- function(year_month_list){
  	
	year  <- year_month_list[["year"]]
	month <- year_month_list[["month"]]
	if(month==12){
		month <- 1
		year  <- year + 1
	}else{
		month <- month + 1
	}
	list_to_return <- list(year, month)
	names(list_to_return) <- c("year", "month")

	return(list_to_return)
}





set_countryISOToName		<- function(ISO, ISOCodes){
	return(ISOCodes$Country[match(ISO, ISOCodes$Code)])
}

set_countryNameToISO		<- function(name, ISOCodes){
 	return(ISOCodes$Code[match(name, ISOCodes$Country)])
}

set_countryChildToParent	<- function(ISO){
	return(Dependent_Territories$Child.ISO.code[match(ISO, Dependent_Territories$Sovereign.ISO.code)])
}

	
set_codeToSourceName		<- function(name, required_source_IDs){
	CodeDictionary		<- data.frame(source=as.character(required_source_IDs$Source.ID),
      						  ID=as.character(required_source_IDs$Source.Short.Name))
	return(CodeDictionary$ID[match(name, CodeDictionary$source)])
}

set_sourceToCode			<- function(name, required_source_IDs){
	CodeDictionary		<- data.frame(source=as.character(required_source_IDs$Source.Short.Name),
      						  ID=as.character(required_source_IDs$Source.ID))
	return((CodeDictionary$ID[match(name, CodeDictionary$source)]))
}


set_FATF_ID 			<- function(name){
	IDDictionary = data.frame(source=(c(paste("Rec..",		1:40,				sep=""),
						      paste("Rec..",		1:40,		" 2013",	sep=""),
							paste("Effectiveness.",	1:11,		" 2013",	sep=""))),
						ID=(c(paste("S-AML",		19:58,			sep=""),
							paste("S-AML",		78:117,			sep=""),
							paste("S-AML",		118:128,			sep=""))))
	return(IDDictionary$ID[match(name, IDDictionary$source)])
}


GetInputFiles			<- function(FilePath, FileNameDistinguishingFeature, TRFiles){
	
	InputFileNames		<- list.files(FilePath, pattern=FileNameDistinguishingFeature, full.names=T)
	if(TRFiles){
		mc			<- detectCores()
		cl			<- makeCluster(mc)
		system.time(
			InputFiles	<- parLapply(cl, InputFileNames, fun=read.csv, skip=2, na.strings="INVALID")
			)
		stopCluster(cl)
	} else {
		InputFiles		<- llply(InputFileNames, function(x) read.csv(x, na.strings="INVALID"), .progress="text")
	}
	EndName			<- gregexpr(pattern=".csv", InputFileNames)
	StartName			<- gregexpr(pattern="/", InputFileNames)
	EndNameVector		<- as.numeric(llply(EndName, max)) - 1
	StartNameVector		<- as.numeric(llply(StartName, max)) + 1
	names(InputFiles)		<- substr(InputFileNames, StartNameVector, EndNameVector)
	
	return(InputFiles)
}
