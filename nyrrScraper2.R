rm(list = setdiff(ls(), "nycm"))

rm(list = ls())
gc()

#############
##| Setup |##
#############

# Load Selenium #
require("RSelenium")
require("data.table")

# Start driver #
Dr <- rsDriver(port = 4344L,
               browser = c("chrome"),
               chromever = "86.0.4240.22")

# Start remote driver #
nycm.RemDr <- Dr$client

# Create empty data frame #
nycm <- data.frame(
  bib = integer(), name = character(), city = character(), country = character(), age = integer(), sex = character(), finish = character(), place = integer(), totRun = integer(),
  place.sex = integer(), place.age = integer(), av.pace = character(), mile3.time = character(), mile4.time = character(), mile5.time = character(), mile6.time = character(), 
  mile7.time = character(), mile8.time = character(), mile9.time = character(), mile10.time = character(), mile11.time = character(), mile12.time = character(), 
  mile13.time = character(),  half.time = character(), mile14.time = character(), mile15.time = character(), mile16.time = character(), mile17.time = character(), 
  mile18.time = character(), mile19.time = character(), mile20.time = character(), mile21.time = character(), mile22.time = character(), mile23.time = character(), 
  mile24.time = character(), mile25.time = character(), mile26.time = character()
) 

# OR #

# Read previous results file #
nycm <- fread("scraperResults-71430.csv",
              header = TRUE,
              sep = ",",
              data.table = FALSE,
              stringsAsFactors = FALSE)

###############
##| Scraper |##
###############
for(i in 1:74000) {
  
  # Navigation #
  site <- paste0("https://results.nyrr.org/event/M2019/result/",i) 
  nycm.RemDr$navigate(site)
  Sys.sleep(2) # For page load
  
  # Test for results home redirect, if not scrape data #
  if (unlist(nycm.RemDr$getCurrentUrl()) != "https://results.nyrr.org/home") {
    
    # Age, sex, and bib number #
    errorCatch <- try({
      suppressMessages({
        ageSexBibElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[1]/div[3]/div/div[1]")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      age <- NA
      sex <- NA
      bib <- i
    } else {
      age <- as.integer(gsub(".*?([0-9]+).*", "\\1", strsplit(as.character(ageSexBibElm$getElementText()), " ")[[1]][2]))
      sex <- substr(strsplit(as.character(ageSexBibElm$getElementText()), " ")[[1]][2], 1, 1)
      bib <- as.integer(strsplit(as.character(ageSexBibElm$getElementText()), " ")[[1]][8])
      rm(ageSexBibElm)
    }
    rm(errorCatch)
    
    # Name #
    errorCatch <- try({
      suppressMessages({
        nameElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[1]/div[1]/div/div/div[1]")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      name <- NA
    } else {
      name <- as.character(nameElm$getElementText())
      rm(nameElm)
    }
    rm(errorCatch)

    # City and country # 
    errorCatch <- try({
      suppressMessages({
        cityCountryElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[1]/div[1]/div/div/div[2]")
      })
    },
    silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      city <- NA
      country <- NA
    } else {
      city <- strsplit(as.character(cityCountryElm$getElementText()), " \\| ")[[1]][1]
      country <- strsplit(as.character(cityCountryElm$getElementText()), " \\| ")[[1]][2]
      rm(cityCountryElm)
    }
    rm(errorCatch)
    
    # Overall place #
    errorCatch <- try({
      suppressMessages({
        placeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[2]/div[3]/span[1]")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      place <- NA
    } else {
      place <- as.integer(gsub(",", "", as.character(placeElm$getElementText())))
      rm(placeElm)
    }
    rm(errorCatch)
    
    # Total runners (for participant type designation) #
    errorCatch <- try({
      suppressMessages({
        totRunElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div[1]/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[2]/div[3]/span[2]")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      totRun <- NA
    } else {
      totRun <- as.integer(gsub(".*?([0-9]+).*", "\\1", gsub(",", "", as.character(totRunElm$getElementText())))) 
      rm(totRunElm)
    }
    rm(errorCatch)
    
    # Sex place #  
    errorCatch <- try({
      suppressMessages({
        place.sexElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[3]/div[1]/span[1]")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      place.sex <- NA
    } else {
      place.sex <- as.integer(gsub(",", "", as.character(place.sexElm$getElementText())))
      rm(place.sexElm)
    }
    rm(errorCatch)
    
    # Age place #  
    errorCatch <- try({
      suppressMessages({
        place.ageElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[3]/div[2]/span[1]")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      place.age <- NA
    } else {
      place.age <- as.integer(gsub(",", "", as.character(place.ageElm$getElementText())))
      rm(place.ageElm)
    }
    rm(errorCatch)
    
    # Finish time #
    errorCatch <- try({
      suppressMessages({
        finishElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[2]/div[1]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      finish <- NA
    } else {
      finish <- as.character(finishElm$getElementText())
      rm(finishElm)
    }
    rm(errorCatch)
    
    # Average pace #
    errorCatch <- try({
      suppressMessages({
        av.paceElm <- nycm.RemDr$findElement(using = "xpath", value ="/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[2]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      av.pace <- NA
    } else {
      av.pace <- as.character(av.paceElm$getElementText())
      rm(av.paceElm)
    }
    rm(errorCatch)
    
    # Mile 3 #
    errorCatch <- try({
      suppressMessages({
        mile3.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[1]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile3.time <- NA
    } else {
      mile3.time <- as.character(mile3.timeElm$getElementText())
      rm(mile3.timeElm)
    }
    rm(errorCatch)
    
    # Mile 4 #
    errorCatch <- try({
      suppressMessages({
        mile4.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[3]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile4.time <- NA
    } else {
      mile4.time <- as.character(mile4.timeElm$getElementText())
      rm(mile4.timeElm)
    }
    rm(errorCatch)
    
    # Mile 5 #
    errorCatch <- try({
      suppressMessages({
        mile5.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[4]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile5.time <- NA
    } else {
      mile5.time <- as.character(mile5.timeElm$getElementText())
      rm(mile5.timeElm)
    }
    rm(errorCatch)
    
    # Mile 6 #
    errorCatch <- try({
      suppressMessages({
        mile6.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[5]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile6.time <- NA
    } else {
      mile6.time <- as.character(mile6.timeElm$getElementText())
      rm(mile6.timeElm)
    }
    rm(errorCatch)
    
    # Mile 7 #
    errorCatch <- try({
      suppressMessages({
        mile7.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[7]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile7.time <- NA
    } else {
      mile7.time <- as.character(mile7.timeElm$getElementText())
      rm(mile7.timeElm)
    }
    rm(errorCatch)
    
    # Mile 8 #
    errorCatch <- try({
      suppressMessages({
        mile8.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[8]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile8.time <- NA
    } else {
      mile8.time <- as.character(mile8.timeElm$getElementText())
      rm(mile8.timeElm)
    }
    rm(errorCatch)
    
    # Mile 9 #
    errorCatch <- try({
      suppressMessages({
        mile9.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[9]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile9.time <- NA
    } else {
      mile9.time <- as.character(mile9.timeElm$getElementText())
      rm(mile9.timeElm)
    }
    rm(errorCatch)
    
    # Mile 10 #
    errorCatch <- try({
      suppressMessages({
        mile10.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[11]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile10.time <- NA
    } else {
      mile10.time <- as.character(mile10.timeElm$getElementText())
      rm(mile10.timeElm)
    }
    rm(errorCatch)
    
    # Mile 11 #
    errorCatch <- try({
      suppressMessages({
        mile11.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[12]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile11.time <- NA
    } else {
      mile11.time <- as.character(mile11.timeElm$getElementText())
      rm(mile11.timeElm)
    }
    rm(errorCatch)
    
    # Mile 12 #
    errorCatch <- try({
      suppressMessages({
        mile12.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[13]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile12.time <- NA
    } else {
      mile12.time <- as.character(mile12.timeElm$getElementText())
      rm(mile12.timeElm)
    }
    rm(errorCatch)
    
    # Mile 13 #
    errorCatch <- try({
      suppressMessages({
        mile13.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[15]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile13.time <- NA
    } else {
      mile13.time <- as.character(mile13.timeElm$getElementText())
      rm(mile13.timeElm)
    }
    rm(errorCatch)
    
    # Half #
    errorCatch <- try({
      suppressMessages({
        half.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[16]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      half.time <- NA
    } else {
      half.time <- as.character(half.timeElm$getElementText())
      rm(half.timeElm)
    }
    rm(errorCatch)

    # Mile 14 #
    errorCatch <- try({
      suppressMessages({
        mile14.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[1]/div[17]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile14.time <- NA
    } else {
      mile14.time <- as.character(mile14.timeElm$getElementText())
      rm(mile14.timeElm)
    }
    rm(errorCatch)
    
    # Mile 15 #
    errorCatch <- try({
      suppressMessages({
        mile15.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[1]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile15.time <- NA
    } else {
      mile15.time <- as.character(mile15.timeElm$getElementText())
      rm(mile15.timeElm)
    }
    rm(errorCatch)
    
    # Mile 16 #
    errorCatch <- try({
      suppressMessages({
        mile16.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[3]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile16.time <- NA
    } else {
      mile16.time <- as.character(mile16.timeElm$getElementText()) 
      rm(mile16.timeElm)
    }
    rm(errorCatch)
    
    # Mile 17 #
    errorCatch <- try({
      suppressMessages({
        mile17.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[4]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile17.time <- NA
    } else {
      mile17.time <- as.character(mile17.timeElm$getElementText())
      rm(mile17.timeElm)
    }
    rm(errorCatch)
    
    # Mile 18 #
    errorCatch <- try({
      suppressMessages({
        mile18.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[5]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile18.time <- NA
    } else {
      mile18.time <- as.character(mile18.timeElm$getElementText()) 
      rm(mile18.timeElm)
    }
    rm(errorCatch)
    
    # Mile 19 #
    errorCatch <- try({
      suppressMessages({
        mile19.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[7]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile19.time <- NA
    } else {
      mile19.time <- as.character(mile19.timeElm$getElementText())
      rm(mile19.timeElm)
    }
    rm(errorCatch)
    
    # Mile 20 #
    errorCatch <- try({
      suppressMessages({
        mile20.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[8]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile20.time <- NA
    } else {
      mile20.time <- as.character(mile20.timeElm$getElementText())
      rm(mile20.timeElm)
    }
    rm(errorCatch)
    
    # Mile 21 #
    errorCatch <- try({
      suppressMessages({
        mile21.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[9]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile21.time <- NA
    } else {
      mile21.time <- as.character(mile21.timeElm$getElementText())
      rm(mile21.timeElm)
    }
    rm(errorCatch)
    
    # Mile 22 #
    errorCatch <- try({
      suppressMessages({
        mile22.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[11]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile22.time <- NA
    } else {
      mile22.time <- as.character(mile22.timeElm$getElementText())
      rm(mile22.timeElm)
    }
    rm(errorCatch)
    
    # Mile 23 #
    errorCatch <- try({
      suppressMessages({
        mile23.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[12]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile23.time <- NA
    } else {
      mile23.time <- as.character(mile23.timeElm$getElementText())
      rm(mile23.timeElm)
    }
    rm(errorCatch)
    
    # Mile 24 #
    errorCatch <- try({
      suppressMessages({
        mile24.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[13]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile24.time <- NA
    } else {
      mile24.time <- as.character(mile24.timeElm$getElementText())
      rm(mile24.timeElm)
    }
    rm(errorCatch)
    
    # Mile 25 #
    errorCatch <- try({
      suppressMessages({
        mile25.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[15]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile25.time <- NA
    } else {
      mile25.time <- as.character(mile25.timeElm$getElementText())
      rm(mile25.timeElm)
    }
    rm(errorCatch)
    
    # Mile 26 #
    errorCatch <- try({
      suppressMessages({
        mile26.timeElm <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[6]/div[3]/div[2]/div[16]/div[2]/span")
      })
    }, silent = TRUE)
    if ("try-error" %in% class(errorCatch)){
      mile26.time <- NA
    } else {
      mile26.time <- as.character(mile26.timeElm$getElementText())
      rm(mile26.timeElm)
    }
    rm(errorCatch)
    
    # Combine and add to results set # 
    temp <- as.data.frame(
      cbind(
        bib, name, city, country, age, sex, finish, place, totRun, place.sex, place.age, av.pace, mile3.time, mile4.time, mile5.time, mile6.time, mile7.time, mile8.time, 
        mile9.time, mile10.time, mile11.time, mile12.time, mile13.time, half.time, mile14.time, mile15.time, mile16.time, mile17.time, mile18.time, mile19.time, mile20.time, 
        mile21.time, mile22.time, mile23.time, mile24.time, mile25.time, mile26.time
      )
    )
    nycm <- rbind(nycm, temp)
    rm(temp)
  }
}

# Write to file #
write.csv(nycm,
          "scraperResults.csv",
          quote = TRUE,
          row.names = FALSE,
          na = "")

View(nycm)


#:::::::::::::::::::::::::::::::::::::::$

errorCatch <- try({
  suppressMessages({
    # element assignment
  })
}, silent = TRUE)
if ("try-error" %in% class(errorCatch)){
  #  < - NA
} else {
  # assignments 
  # rm(Elm)
}
rm(errorCatch)

#For some runners it's this xpath ... 
#name <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div[1]/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[1]/div[1]/div/div/div[1]")"

#For some runners it's this xpath ... 
#city <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div[1]/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[1]/div[1]/div/div/div[2]")

#For some runners it's this xpath ... 
#finish <- nycm.RemDr$findElement(using = "xpath", value = "/html/body/div[1]/div[2]/div/main/div/div[2]/div[2]/div/div/div[3]/div[1]/div[2]/div[1]/span")
