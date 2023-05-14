#### Packages ####

library(rjson)
library(tidyverse)
library(skimr)
library(data.table)
library(RCurl)
library(httr)
library(janitor)

options(download.file.method="libcurl")
set_config(config(ssl_verifypeer = 0L))
#### Functions ####

#Get data from UN Comtrade API
get_Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec= 100000
                         ,type="C"     # C for commodity, S for service
                         ,freq="A"     # A for annual
                         ,px="HS"      # Trade reporting scheme (HS is most common)
                         ,ps           # Period of trade
                         ,r            # Reporter country code
                         ,p            # Partner country code
                         ,rg           # 1 for imports, 2 for exports
                         ,cc           # HS code for the product
                         ,fmt="csv"    # CSV or JSON
)
{
  # This is the URL that will be used for the API call
  string<- paste0(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    
    #d <- getURL(string, header = TRUE, timeout = 100)
    #raw.data<- read.csv(string, header = TRUE)
    #return(list(validation=NULL, data=raw.data))
    data <- GET(string, timeout(300))
    gadata <- read.csv(textConnection(content(data, as = "text")), check.names=FALSE)
    return(gadata)
    
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}




# Get the list of reporters on UN Comtrade (countries and regions)
getReporters <- function(writeCSV = FALSE)
{
  string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
  reporters <- fromJSON(file=string)
  reporters <- as.data.frame(t(sapply(reporters$results,rbind)))
  reporters$V1 <- unlist(reporters$V1)
  reporters$V2 <- unlist(reporters$V2)
  
  
 
  #Optionally write the reporters list to a csv file
  if(writeCSV) 
  {   
    write.csv(x = reporters,file = "countrycodes.csv", row.names = FALSE)
  }
 
  return(reporters)
}


# Converts country name to country code
#getCountryCode <- function(reporter_df, country_name)
# {
#   index <- which(reporters$V2 == country_name, arr.ind = TRUE)
#   code =  unlist((reporters$V1[index])[1])
#   
#   return(code)
# 
# }



#### Building the Matrix ####

#Type in countries/regions that you would like to ignore
ignoreList <- c("All", "World")

#get all the reporters and filter out countries/regions from ignoreList
reporters <- getReporters() 
reporters <- reporters %>%
  filter(!(V2 %in% ignoreList)) %>%
  rename(code = V1, countries = V2)

#write_csv(reporters, "./Data/reporters.csv")

#Creates the data frame of reporters and partners. It is the list of reporters cross-joined with itself
flowDataFrame <- as.data.frame(CJ(Reporter = as.vector(reporters$countries), Partner = as.vector(reporters$countries)))

years = c("2016,2015,2014,2013", "2012,2011,2010,2009,2008", "2007,2006,2005,2004,2003", "2002,2001,2000,1999,1998", "1997,1996,1995,1994,1993", "1992,1991,1990")
commodities = c("390110","390120","390210", "3915", "390760", "390761", "390769") #LDPE, HDPE, Polypropylene, Plastic waste, PET (pre-2017), PET (2017 and after, high viscosity), PET (2017 and after, low viscosity)


request <- get_Comtrade(fmt = "csv", r = paste0(reporters$code[1:5], collapse = ","), p="ALL", ps = "2017", rg = "1,2", cc = paste0(commodities[1:4], collapse = ","))
#print(request$validation)
cleanRequest <- clean_names(request) %>%
  #select(rtTitle, ptTitle, yr, pfCode, cmdCode, cmdDescE, rgCode, rgDesc, TradeQuantity, qtDesc, TradeValue) #Use this if fmt = "json"
  select(reporter, partner, year, classification, commodity_code, commodity,  trade_flow_code, trade_flow, qty, qty_unit, trade_value_us)

cleanRequest <- data.frame(reporter = character(),
                           partner = character(),
                           year = integer(), 
                           classification = character(),
                           commodity_code = character(),
                           commodity = character(),
                           trade_flow_code = character(),
                           trade_flow = character(), 
                           qty = double(), 
                           qty_unit = character(), 
                           trade_value_us = character())
numRequests = 0

for(y in years)
{  
  for(i in seq(1,length(reporters[,1]),5))  
  { 
    if(i == 291){
      request <- get_Comtrade(fmt = "csv", r = paste0(reporters$code[i], collapse = ","), p="ALL", ps = "2017,2018,2019", rg = "1,2", cc = "3915")
    
    }else {
      request <- get_Comtrade(fmt = "csv", r = paste0(reporters$code[i:(i+4)], collapse = ","), p="ALL", ps = "2017,2018,2019", rg = "1,2", cc = "3915")
    }
    
    # if(is.na(request$Reporter[1]))
    # {
    #   next 
    # }
    
    
    
    cleanRequest <- rbind(cleanRequest, clean_names(request) %>%
      select(reporter, partner, year, classification, commodity_code, commodity,  trade_flow_code, trade_flow, qty, qty_unit, trade_value_us))
    
    Sys.sleep(1)
    # to comply with request restrictions
    
    numRequests = numRequests + 1
    print(numRequests)
    
    if(numRequests == 99 | numRequests == 198 | numRequests == 297 | numRequests == 396 | numRequests == 495 | numRequests == 594)
    {
      for(x in 1:3600){
        Sys.sleep(1)
        print(3600-x)
      }
      
    }
    write_csv(cleanRequest, "./Data/waste171819.csv")
  }
}



test <- cleanRequest %>%
  filter(year == "1998",
         reporter == "Belarus")



x <- cleanRequest %>%
  group_by(reporter) %>%
  summarize(count = n())

cleanRequest <- cleanRequest %>%
  arrange(Reporter, Partner) %>%
  filter(!is.na(Reporter))

flowDataFrame <- left_join(flowDataFrame, cleanRequest)

flowMatrix <- flowDataFrame %>%
  spread(key = Partner, value = Qty)

#Note: transpose the export matrix (but not the import matrix) for compatibility with ORA






