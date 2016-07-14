library(shiny);library(RJSONIO);library(dplyr);library(plotly);library(DT);library(lubridate);library(gridExtra);library(grid);library(ggplot2);library(ggthemes)


##factorise this to pieces, just in case...?
#def get base URL
URL <- paste("http://ergast.com/api/f1")
format <- ".json?limit=1000"
laps <- "laps" 
options(digits.secs=3) # format time to 3 digits
#year <- "2016" # for console
#round <- "1" # for console
#race <- "4" # for console


#total available years of data
years <- seq(1950, as.numeric(format(Sys.Date(), "%Y")), 1)


#helper functions
helper.null=function(x){# function to return NA instead of NULL for readability
  if (is.null(x)) {NA} else {x}
}

time.laps.strp=function(x){
  strptime(x, "%M:%OS")
}

time.race.strp=function(x){
  strptime(x, "%H:%M:%OS")
}

time.laps.fmt=function(x){
as.numeric(difftime(time.laps.strp(x),format(time.laps.strp(x),"%Y-%m-%d 00:00.000"), units = "secs"))
}
# 
# time.laps.frmt=function(x){
#   format(x, "%M:%OS")
# }
# 
# 
# time.race.frmt=function(x){
#   format(x, "%H:%M:%OS")
# }
# 
# time.laps.dif=function(x,y){
# round(difftime(time.laps.strp(x), time.laps.strp(y), units="secs"), digits = 3)
# }
# 
# time.race.dif=function(x,y){
#   round(difftime(time.race.strp(x), time.race.strp(y), units="secs"), digits = 3)
# }
# 
# time.secs=function(x){
# difftime(x,as.POSIXct(format(x,"%Y-%m-%d 00:00.000")), units = "secs")
# }

# get URl functions
getLapsResultURL=function(year, round, lap){# used by getLapsData to get nbr of laps per race
  paste(URL,year, round, laps,paste0(lap, format) ,sep='/')
  
}

getRoundResultsURL=function(year, round){# used by getLapsData
  paste(URL,year, round, paste0("results",format) ,sep='/')
}

getQualURL=function(year, round){ #not used yet
  paste(URL, year, round, paste0("qualifying",format),sep='/')
}


getRaceResultsURL=function(year){ # used by getRace.Data
  paste(URL,year,paste0("results", format), sep='/')
}


getDriversURL=function(year){# used to getDriversData.year
  paste(URL,year,"drivers.json",sep='/')
}

getSeriesURL=function(year){# used to getDriversData.year
  paste(URL,year,"drivers.json",sep='/')
}

getPitURL=function(year, round){# used to getDriversData.year
  paste(URL,year,round, paste0("pitstops",format),sep='/')
}

#get data functions

# driver data * checked ok
getDriversData.year=function(year){
  #build empty df for rbind
  drivers.data=data.frame(
    name=character(),
    code=character(),
    dateofbirth=as.Date(character()),
    nationality=character(),
    driverId=character(),
    season=numeric()
  )
  drivers.json=fromJSON(getDriversURL(year),simplify=FALSE)
  drivers=drivers.json$MRData$DriverTable$Drivers
  for (i in 1:length(drivers)){
    drivers.data=rbind(drivers.data,data.frame(
      driverId=drivers[[i]]$driverId,
      name=paste(drivers[[i]]$givenName, drivers[[i]]$familyName, sep = ' '),
      code=drivers[[i]]$code,
      dateofbirth=as.Date(drivers[[i]]$dateOfBirth),
      nationality=drivers[[i]]$nationality,
      season=year
    ))
  }
  drivers.data
}

# get all lap data from a race by year, and round number
getLapsData.race=function(year, round){
  #build empty df for rbind
  Laps.data=data.frame(
    round=numeric(),
    lap=character(),
    driverId=character(),
    position=character(),
    time=character()
  )
  
  result.json=fromJSON(getRoundResultsURL(year, round),simplify=FALSE)
  max.lap=as.numeric(result.json$MRData$RaceTable$Races[[1]]$Results[[1]]$laps)
  
  for (n in 1:max.lap){
    
    laps.json=fromJSON(getLapsResultURL(year, round, n),simplify=FALSE)$MRData
    
    for (i in 1:length(laps.json$RaceTable$Races[[1]]$Laps[[1]]$Timings)){
      Laps.data=rbind(Laps.data,data.frame(
        round=round,
        lap=as.integer(n),
        driverId=laps.json$RaceTable$Races[[1]]$Laps[[1]]$Timings[[i]]$driverId,
        position=laps.json$RaceTable$Races[[1]]$Laps[[1]]$Timings[[i]]$position,
        time=as.character(laps.json$RaceTable$Races[[1]]$Laps[[1]]$Timings[[i]]$time)
      ))
    }
    
  }
  
  Laps.data
}


getLapsData.year=function(year){ ## nog niet af!!!!!!!!!!!!!!
  #build empty df for rbind
  Laps.data.year=data.frame(
    round=numeric(),
    lap=character(),
    driverId=character(),
    position=character(),
    time=character()
  )
  
  #get rounds per year
  max.rounds=length(getSeasonData(year)$round)

  for (i in 1:max.rounds) {
    
    Laps.data.year = rbind(
      Laps.data.year,
      data.frame(getLapsData.race(year,i))
    )
  }    
  Laps.data.year
}


# get race results for a whole year.
getRace.Data = function(year) {
  #build empty df for rbind
  race.data = data.frame(
    year = numeric(),
    round = numeric(),
    racename = character(),
    circuit = character(),
    locality = character(),
    country = character(),
    lat = character(),
    lon = character(),
    date = character(),
    Pos = character(),
    No = numeric(),
    Driver = character(),
    Constructor = character(),
    Laps = numeric(),
    Grid = numeric(),
    Time = character(),
    Status = character(),
    Points = numeric(),
    FastedLap = numeric(),
    FLrank = numeric(),
    FLtime = character(),
    FLAverageSpeed = numeric(),
    units = character()
  )
  
  
  race.result.json = fromJSON(getRaceResultsURL(year), simplify = FALSE)$MRData
  races = length(race.result.json$RaceTable$Races)
  
  for (i in 1:races) {
    result = length(race.result.json$RaceTable$Races[[i]]$Results) #get max results per race for inner loop
    race.result = race.result.json$RaceTable$Races[[i]] #shorten table for inner loop
    
    for (r in 1:result) {
      race.data = rbind(
        race.data,
        data.frame(
          year = race.result$season,
          round = race.result$round,
          racename = race.result$raceName,
          circuit = race.result$Circuit$circuitName,
          locality = race.result$Circuit$Location$locality,
          country = race.result$Circuit$Location$country,
          lat = race.result$Circuit$Location$lat,
          lon = race.result$Circuit$Location$long,
          date = race.result$date,
          Pos = helper.null(race.result$Results[[r]]$position),
          No = helper.null(race.result$Results[[r]]$number),
          Driver = helper.null(race.result$Results[[r]]$Driver$driverId),
          Constructor = helper.null(race.result$Results[[r]]$Constructor$name),
          Laps = helper.null(race.result$Results[[r]]$laps),
          Grid = helper.null(race.result$Results[[r]]$grid),
          Time = helper.null(race.result$Results[[r]]$Time$time),
          Status = helper.null(race.result$Results[[r]]$status),
          Points = helper.null(race.result$Results[[r]]$points),
          FastedLap = helper.null(race.result$Results[[r]]$FastestLap$lap),
          FLrank = helper.null(race.result$Results[[r]]$FastestLap$rank),
          FLtime = helper.null(race.result$Results[[r]]$FastestLap$Time$time),
          FLAverageSpeed = helper.null(race.result$Results[[r]]$FastestLap$AverageSpeed$speed),
          units = helper.null(race.result$Results[[r]]$FastestLap$AverageSpeed$units)
        )
      )
      
    }
  }
  
  race.data
}



getSeasonData = function(year) { #gets a single calander year of driven races
  #build empty df for rbind
  season.data = data.frame(
    year = numeric(),
    round = numeric(),
    racename = character(),
    circuit = character(),
    locality = character(),
    country = character(),
    lat = character(),
    lon = character(),
    date = character()
    )
  
  season.json = fromJSON(getRaceResultsURL(year), simplify = FALSE)$MRData
  races = length(season.json$RaceTable$Races)

for (i in 1:races) {
  season.data = rbind(
    season.data,
    data.frame(
      year = season.json$RaceTable$Races[[i]]$season,
      round = season.json$RaceTable$Races[[i]]$round,
      racename = season.json$RaceTable$Races[[i]]$raceName,
      circuit = season.json$RaceTable$Races[[i]]$Circuit$circuitName,
      locality = season.json$RaceTable$Races[[i]]$Circuit$Location$locality,
      country = season.json$RaceTable$Races[[i]]$Circuit$Location$country,
      lat = season.json$RaceTable$Races[[i]]$Circuit$Location$lat,
      lon = season.json$RaceTable$Races[[i]]$Circuit$Location$long,
      date = season.json$RaceTable$Races[[i]]$date
    )
  )
  
}
  season.data
}

getSeasonData.Full = function(x) {
  #build empty df for rbind
  season.data.full = data.frame(
    year = numeric(),
    round = numeric(),
    racename = character(),
    circuit = character(),
    locality = character(),
    country = character(),
    lat = character(),
    lon = character(),
    date = character()
  )
  
  for (i in min(years):max(years)) {

    season.data.full = rbind(
      season.data.full,
      data.frame(getSeasonData(i))
    )
  }    
  season.data.full
}

getPitData.year = function(year) {
  #build empty df for rbind
  pit.data = data.frame(
    round = numeric(),
    driverId = character(),
    stop = numeric(),
    lap = numeric(),
    duration = character()
  )
  # get rounds per year
  max.rounds = length(getSeasonData(year)$round)

  for (i in 1:max.rounds) {
    #get pit data by round
    pit.json = fromJSON(getPitURL(year, i), simplify = FALSE)$MRData
    #get max number of pits per round
    max.pits = length(pit.json$RaceTable$Races[[1]]$PitStops)
    
    for (n in 1:max.pits) {
      pit.data = rbind(
        pit.data,
        data.frame(
          round = as.numeric(pit.json$RaceTable$round),
          driverId = pit.json$RaceTable$Races[[1]]$PitStops[[n]]$driverId,
          stop = as.numeric(pit.json$RaceTable$Races[[1]]$PitStops[[n]]$stop),
          lap = as.numeric(pit.json$RaceTable$Races[[1]]$PitStops[[n]]$lap),
          duration = pit.json$RaceTable$Races[[1]]$PitStops[[n]]$duration
        )
      )
    }
  }
  pit.data
}

getPitData.race = function(year, round) {
  #build empty df for rbind
  pit.data = data.frame(
    driverId = character(),
    stop = numeric(),
    lap = numeric(),
    duration = character()
  )

      #get pit data by round
    pit.json = fromJSON(getPitURL(year, round), simplify = FALSE)$MRData
    #get max number of pits per round
    max.pits = length(pit.json$RaceTable$Races[[1]]$PitStops)
    
    for (i in 1:max.pits) {
      pit.data = rbind(
        pit.data,
        data.frame(
          driverId = pit.json$RaceTable$Races[[1]]$PitStops[[i]]$driverId,
          stop = as.numeric(pit.json$RaceTable$Races[[1]]$PitStops[[i]]$stop),
          lap = as.numeric(pit.json$RaceTable$Races[[1]]$PitStops[[i]]$lap),
          duration = pit.json$RaceTable$Races[[1]]$PitStops[[i]]$duration
        )
      )
    }

  pit.data
}

theme_custom=function(base_size=12,base_family="") {
  theme_dark(base_size=base_size,base_family=base_family) %+replace%
    theme(
      # line = element_line(colour="white", size=8, linetype = "solid", lineend ="round"),
      # # Specify axis options
      # axis.line=element_blank(), 
      axis.text.x=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,vjust=1),
      axis.text.y=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,hjust=1),
      axis.ticks=element_line(color="white",size = 0.2),
      axis.title.x=element_text(size=base_size,color="white",vjust=1),
      axis.title.y=element_text(size=base_size,color="white",angle=90,
                                vjust=0.5),
      # axis.ticks.length=unit(0.3,"lines"), 
      # axis.ticks.margin=unit(0.5,"lines"),
      # # Specify legend options
      legend.background=element_rect(color="black",fill="black"), 
      # legend.key=element_rect(color=NA, fill="black"), 
      # legend.key.size=unit(1.2,"lines"), 
      # legend.key.height=NULL, 
      # legend.key.width=NULL,     
      legend.text=element_text(size=base_size*0.8,color="white"),
      legend.title=element_text(size=base_size*0.8,face="bold",hjust=0, color="white"),
      legend.position="top", 
      
      # legend.text.align=NULL, 
      # legend.title.align=NULL, 
      legend.direction="horizontal", 
      #legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill= '#0d0d0d',color = '#0d0d0d'),
      # panel.border=element_rect(fill=NA,color=NA), 
      # panel.grid.major=element_blank(), 
      # panel.grid.minor=element_blank(), 
      # panel.margin=unit(0.25,"lines"),  
      # # Specify facetting options
      # strip.background=element_rect(fill="grey30",color="grey10"), 
      # strip.text.x=element_text(size=base_size*0.8,color="white"), 
      # strip.text.y=element_text(size=base_size*0.8,color="white",
      #                           angle=-90), 
      # # Specify plot options
      plot.background=element_rect(color="black",fill="black"), 
      plot.title=element_text(size=base_size*1.2,color="white")#, 
      # plot.margin=unit(c(1,1,0.5,0.5),"lines")
    )
}



theme_custom2=function(base_size=12,base_family="") {
  theme_dark(base_size=base_size,base_family=base_family) %+replace%
    theme(
      # line = element_line(colour="white", size=8, linetype = "solid", lineend ="round"),
      # # Specify axis options
      # axis.line=element_blank(), 
      axis.text.x=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,vjust=1),
      axis.text.y=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,hjust=1),
      axis.ticks=element_line(color="white",size = 0.2),
      axis.title.x=element_text(size=0,color="white",vjust=1),
      axis.title.y=element_text(size=0,color="white",angle=90,
                                vjust=0.5),
      # axis.ticks.length=unit(0.3,"lines"), 
      # axis.ticks.margin=unit(0.5,"lines"),
      # # Specify legend options
      legend.background=element_rect(color="black",fill="black"), 
      # legend.key=element_rect(color=NA, fill="black"), 
      # legend.key.size=unit(1.2,"lines"), 
      # legend.key.height=NULL, 
      # legend.key.width=NULL,     
      legend.text=element_text(size=base_size*0.8,color="white"),
      legend.title=element_text(size=base_size*0.8,face="bold",hjust=0, color="white"),
      legend.position="top", 
      
      # legend.text.align=NULL, 
      # legend.title.align=NULL, 
      legend.direction="horizontal", 
      #legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill= '#0d0d0d',color = '#0d0d0d'),
      # panel.border=element_rect(fill=NA,color=NA), 
      # panel.grid.major=element_blank(), 
      # panel.grid.minor=element_blank(), 
      # panel.margin=unit(0.25,"lines"),  
      # # Specify facetting options
      # strip.background=element_rect(fill="grey30",color="grey10"), 
      # strip.text.x=element_text(size=base_size*0.8,color="white"), 
      # strip.text.y=element_text(size=base_size*0.8,color="white",
      #                           angle=-90), 
      # # Specify plot options
      plot.background=element_rect(color="black",fill="black"), 
      plot.title=element_text(size=base_size*0.9,color="white")#, 
      # plot.margin=unit(c(1,1,0.5,0.5),"lines")
    )
}


theme_custom3=function(base_size=12,base_family="") {
  theme_dark(base_size=base_size,base_family=base_family) %+replace%
    theme(
      # line = element_line(colour="white", size=8, linetype = "solid", lineend ="round"),
      # # Specify axis options
      # axis.line=element_blank(), 
      axis.text.x=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,vjust=1),
      axis.text.y=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,hjust=1),
      axis.ticks=element_line(color="white",size = 0.2),
      axis.title.x = element_blank(),
      axis.title.y=element_text(size=0,color="white",angle=90,
                                vjust=0.5),
      # axis.ticks.length=unit(0.3,"lines"), 
      # axis.ticks.margin=unit(0.5,"lines"),
      # # Specify legend options
      legend.background=element_rect(color="black",fill="black"), 
      # legend.key=element_rect(color=NA, fill="black"), 
      # legend.key.size=unit(1.2,"lines"), 
      # legend.key.height=NULL, 
      # legend.key.width=NULL,     
      legend.text=element_text(size=base_size*0.8,color="white"),
      legend.title=element_text(size=base_size*0.8,face="bold",hjust=0, color="white"),
      legend.position="top", 
      
      # legend.text.align=NULL, 
      # legend.title.align=NULL, 
      legend.direction="horizontal", 
      #legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill= '#0d0d0d',color = '#0d0d0d'),
      # panel.border=element_rect(fill=NA,color=NA), 
      # panel.grid.major=element_blank(), 
      # panel.grid.minor=element_blank(), 
      # panel.margin=unit(0.25,"lines"),  
      # # Specify facetting options
      # strip.background=element_rect(fill="grey30",color="grey10"), 
      # strip.text.x=element_text(size=base_size*0.8,color="white"), 
      # strip.text.y=element_text(size=base_size*0.8,color="white",
      #                           angle=-90), 
      # # Specify plot options
      plot.background=element_rect(color="black",fill="black"), 
      plot.title=element_text(size=base_size*0.9,color="white")#, 
      # plot.margin=unit(c(1,1,0.5,0.5),"lines")
    )
}
