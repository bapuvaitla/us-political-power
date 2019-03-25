  library(tidyverse)
  library(rvest)
  library(stringr)
  library(rebus)
  library(lubridate)
  library(xml2)
  
  # DOWNLOAD, CLEAN----
  
    # Alaska----
      ## Scrape HTML pages
  ak_2012 <- html_table(html_nodes(read_html("http://www.elections.alaska.gov/statistics/vi_vrs_stats_party_2012.11.03.htm"), "table")[[41]])
  ak_2014 <- html_table(html_nodes(read_html("http://www.elections.alaska.gov/statistics/vi_vrs_stats_party_2014.11.03.htm"), "table")[[41]])
  ak_2016 <- html_table(html_nodes(read_html("http://www.elections.alaska.gov/statistics/2016/NOV/VOTERS%20BY%20PARTY%20AND%20PRECINCT.htm"), "table")[[83]], fill = T)
  ak_2018 <- html_table(html_nodes(read_html("http://www.elections.alaska.gov/statistics/2018/NOV/VOTERS%20BY%20PARTY%20AND%20PRECINCT.htm"), "table")[[83]], fill = T)
  
      ## Clean
        ### 2012
  colnames(ak_2012) <- ak_2012[1,]
  ak_2012 <- ak_2012[-1,-1]
  ak_2012 <- as.data.frame(t(sapply(ak_2012, function(x) as.numeric(as.character(gsub(",","",x))))))
  ak_2012$oth_reg <- ak_2012$TOTAL - ak_2012$D - ak_2012$R
  ak_2012 <- ak_2012[c("TOTAL", "D", "R", "oth_reg")]
  names(ak_2012) <- c("tot_reg", "dem_reg", "rep_reg", "oth_reg")
  ak_2012$cd <- 0
  ak_2012$year <- 2012
        ### 2014
  colnames(ak_2014) <- ak_2014[1,]
  ak_2014 <- ak_2014[-1,-1]
  ak_2014 <- as.data.frame(t(sapply(ak_2014, function(x) as.numeric(as.character(gsub(",","",x))))))
  ak_2014$oth_reg <- ak_2014$TOTAL - ak_2014$D - ak_2014$R
  ak_2014 <- ak_2014[c("TOTAL", "D", "R", "oth_reg")]
  names(ak_2014) <- c("tot_reg", "dem_reg", "rep_reg", "oth_reg")
  ak_2014$cd <- 0
  ak_2014$year <- 2014
        ### 2016
  ak_2016 <- ak_2016[c(-1,-4),]
  colnames(ak_2016) <- ak_2016[1,]
  ak_2016 <- ak_2016[-1,c(-1,-3,-4)]
  ak_2016 <- as.data.frame(t(sapply(ak_2016, function(x) as.numeric(x))))
  ak_2016$oth_reg <- ak_2016$TOTAL - ak_2016$D - ak_2016$R
  ak_2016 <- ak_2016[c("TOTAL", "D", "R", "oth_reg")]
  names(ak_2016) <- c("tot_reg", "dem_reg", "rep_reg", "oth_reg")
  ak_2016$cd <- 0
  ak_2016$year <- 2016
        ### 2018
  ak_2018 <- ak_2018[c(-1,-4),]
  colnames(ak_2018) <- ak_2018[1,]
  ak_2018 <- ak_2018[-1,c(-1,-3,-4)]
  ak_2018 <- as.data.frame(t(sapply(ak_2018, function(x) as.numeric(x))))
  ak_2018$oth_reg <- ak_2018$TOTAL - ak_2018$D - ak_2018$R
  ak_2018 <- ak_2018[c("TOTAL", "D", "R", "oth_reg")]
  names(ak_2018) <- c("tot_reg", "dem_reg", "rep_reg", "oth_reg")
  ak_2018$cd <- 0
  ak_2018$year <- 2018
  
      ## Bind
  ak <- bind_rows(ak_2012, ak_2014, ak_2016, ak_2018)
  ak$state <- "Alaska"
  ak$state_po <- "AK"
  ak$state_fips <- 2
  
    # Arizona----
    
      ## Download raw data tables
    az_2012 <- read.csv("http://apps.azsos.gov/election/voterreg/2012-10-30.csv", skip = 114, nrows = 91, header = T)
    
    az_2014 <- read.csv("http://apps.azsos.gov/election/voterreg/2014-11-04.csv", skip = 116, nrows = 89, header = T)
    
    az_2016 <- read.csv("http://apps.azsos.gov/election/voterreg/2016-11-08.csv", skip = 60, nrows = 48, header = T)
    
    az_2018 <- read.csv("http://apps.azsos.gov/election/voterreg/2018-10-01.csv", skip = 76, nrows = 58,  header = T)
    
      ## 2012
        ### Retain only needed columns
    az_2012 <- az_2012[c("X.2", "X.4", "Democratic", 
                         "Green", "Libertarian", "Republican", 
                         "X.9", "X.10")]
        ### Correct import row alignment problem with last two columns
    az_2012 <- az_2012 %>% mutate_at(c("X.9", "X.10"), funs(lag), n=1)
        ### Keep only rows with CD label and total values
    az_2012 <- az_2012 %>% filter(grepl("Congressional District", X.2) |
                                    grepl("TOTALS", X.4))
        ### Adjust alignment on CD labels
    az_2012 <- az_2012 %>% mutate(X.2 = lag(X.2))
        ### Delete empty rows
    az_2012 <- az_2012 %>% filter(!X.10 == "")
        ### Clean up columns
    az_2012$X.4 <- NULL
    names(az_2012) <- c("cd", "dem_reg", "grn_reg", "lib_reg",
                        "rep_reg", "oth_reg", "tot_reg")
    az_2012$cd <- gsub("[^0-9.-]", "", az_2012$cd)
        ### Add year column
    az_2012$year <- 2012
        ### Convert all to numeric
    az_2012 <- as.data.frame(sapply(az_2012, function(x) as.numeric(as.character(gsub(",","",x)))))
    
      ## 2014
        ### Retain only needed columns
    az_2014 <- az_2014[c("X.2", "X.4", "Democratic", 
                        "Libertarian", "Republican",  "X.9", "X.10")]
        ### Correct import row alignment problem with last two columns
    az_2014 <- az_2014 %>% mutate_at(c("X.9", "X.10"), funs(lag), n=1)
        ### Keep only rows with CD label and total values
    az_2014 <- az_2014 %>% filter(grepl("Congressional District", X.2) |
                                    grepl("TOTALS", X.4))
        ### Adjust alignment on CD labels
    az_2014 <- az_2014 %>% mutate(X.2 = lag(X.2))
        ### Delete empty rows
    az_2014 <- az_2014 %>% filter(!X.10 == "")
        ### Clean up columns
    az_2014$X.4 <- NULL
    names(az_2014) <- c("cd", "dem_reg", "lib_reg",
                        "rep_reg", "oth_reg", "tot_reg")
    az_2014$cd <- gsub("[^0-9.-]", "", az_2014$cd)
        ### Add year column
    az_2014$year <- 2014
        ### Convert all to numeric
    az_2014 <- as.data.frame(sapply(az_2014, function(x) as.numeric(as.character(gsub(",","",x)))))
    
      ## 2016
        ### Retain only needed columns
    az_2016 <- az_2016[c("X.1", "X.2", "Democratic", "Green",
                         "Libertarian", "Republican",  "Other", "TOTAL")]
        ### Keep only rows with CD label and total values
    az_2016 <- az_2016 %>% filter(grepl("Congressional District", X.1) |
                                    grepl("TOTALS", X.2))
        ### Adjust alignment on CD labels
    az_2016 <- az_2016 %>% mutate(X.1 = lag(X.1))
        ### Delete empty rows
    az_2016 <- az_2016 %>% filter(!TOTAL == "")
        ### Clean up columns
    az_2016$X.2 <- NULL
    names(az_2016) <- c("cd", "dem_reg", "grn_reg", "lib_reg",
                        "rep_reg", "oth_reg", "tot_reg")
    az_2016$cd <- gsub("[^0-9.-]", "", az_2016$cd)
        ### Add year column
    az_2016$year <- 2016
        ### Convert all to numeric
    az_2016 <- as.data.frame(sapply(az_2016, function(x) as.numeric(as.character(gsub(",","",x)))))
    
      ## 2018
        ### Retain only needed columns
    az_2018 <- az_2018[c("District", "Democratic", "Green",
                         "Libertarian", "Republican",  "Other", "Total")]
        ### Keep only rows with  total values
    az_2018 <- az_2018 %>% filter(grepl("Total", District))
        ### Add CD labels
    az_2018$District <- c(1:9)
        ### Clean up columns
    names(az_2018) <- c("cd", "dem_reg", "grn_reg", "lib_reg",
                        "rep_reg", "oth_reg", "tot_reg")
        ### Add year column
    az_2018$year <- 2018
        ### Convert all to numeric
    az_2018 <- as.data.frame(sapply(az_2018, function(x) as.numeric(as.character(gsub(",","",x)))))
    
      ## Bind all
    az <- bind_rows(az_2012, az_2014, az_2016, az_2018)
    az$state <- "Arizona"
    az$state_po <- "AZ"
    az$state_fips <- 4
    
    
    
    
    
    
    
    
    
    
    
    
    
   
    
    
    
    
    
    
    