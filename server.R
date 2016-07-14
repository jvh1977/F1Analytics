

# Define server logic
shinyServer(function(input, output) {
  
  # UI ----------------------------------------------------------------------
  output$seasons <- renderUI({
    selectInput(inputId = "seasons", label = "seasons", choices =  years, selected = max(years), multiple = TRUE, selectize = FALSE)
  })
  
  output$roundsps <- renderUI({
    rounds=getSeasonData(input$seasons)
    rounds=levels(rounds[,2])
    selectInput(inputId = "roundsps", label = "roundsps", choices = rounds, selected = max(rounds), multiple = FALSE, selectize = FALSE)
  })
  
  output$driver.a.Control <- renderUI({
    ddrivers <-drivers.data.year()
    dList=levels(ddrivers$driverId)
    names(dList)=ddrivers$name
    selectInput("driver.a", "Choose Driver A", dList, multiple = FALSE, selectize = FALSE)
  })
  
  output$driver.b.Control <- renderUI({
    ddrivers <-drivers.data.year()
    #ddrivers <- ddrivers[ddrivers$driverId != input$driver.a,]
    dList=levels(ddrivers$driverId)
    names(dList)=ddrivers$name
    selectInput("driver.b", "Choose Driver B", dList,multiple = FALSE, selectize = FALSE)
  })
  
  output$time.control <- renderUI({
  sliderInput("time.control", "set min and max lap time to remove in & outlap:", 
              min = 0.00, 
              max = 1.00, 
              value = c(0.05,0.95), 
              step= 0.01)
  })


  # Call Data ---------------------------------------------------------------
  drivers.data.year <- reactive({
    getDriversData.year(input$seasons)
    #drivers.data.year= getDriversData.year(2016)
  })
  
  laps.data.race <- eventReactive(input$goButton, {
    getLapsData.race(year = input$seasons, round = input$roundsps)
    #laps.data.race=getLapsData.race(2016,9)
  })
  
  season.data.year <- eventReactive(input$goButton, {
    getSeasonData(input$seasons)
    #season.data.year=getSeasonData(2016)
  })
  
  pit.data.race <- eventReactive(input$goButton, {
    getPitData.race(year = input$seasons, round = input$roundsps)
    #pit.data.race=getPitData.race(2016,9)
  })
  
  
  # Mangle data -------------------------------------------------------------
  
  data.race <- eventReactive(input$goButton, {
    data.race=laps.data.race()
    #data.race=laps.data.race
  })
  
  data.race.time <- eventReactive(input$goButton, {
    data.race=data.race()
    data.race$time <- ms(data.race$time)
    data.race$OS <- as.numeric(as.duration(data.race$time))
    data.race$time <- as.character(paste0(data.race$time))
  })
  
  # data.driver <- eventReactive(input$goButton, {
  #   data.driver=laps.data.race()
  #   data.driver$round=as.numeric(data.driver$round)
  #   pit.data.race=pit.data.race()
    #pit.data.race$pit[pit.data.race$lap[!is.na(pit.data.race$pit)]+1] <- "pit"
    # outlap=pit.data.race
    # outlap$lap = outlap$lap+1
    # outlap$stop[!is.na(outlap$stop)] <- 'outlap'
    # outlap$duration[!is.na(outlap$duration)] <- NA
    # pit.data.race=rbind(pit.data.race,outlap)
    # data.driver=data.driver[data.driver$driverId %in% c(input$driver.a, input$driver.b),] %>% 
    #   dplyr::left_join(pit.data.race, by=c("lap", "driverId"))
    # data.driver
    
    # data.driver=laps.data.race
    # data.driver$round=as.numeric(data.driver$round)
    # pit.data.race=pit.data.race
    # pit.data.race$round=as.numeric(pit.data.race$round)
    # data.driver=data.driver %>%
    #   dplyr::filter(driverId %in% c("max_verstappen", "ricciardo")) %>%
    #   left_join(pit.data.race, by=c("lap", "driverId"))
    
  # })
  
  data.driver <- eventReactive(input$goButton, {
    data.driver=laps.data.race()
    data.driver$round=as.numeric(data.driver$round)
    pit.data.race=pit.data.race()
    
    data.driver=data.driver[data.driver$driverId %in% c(input$driver.a, input$driver.b),] %>% 
      dplyr::left_join(pit.data.race, by=c("lap", "driverId"))
    

    data.driver$OS <- time.laps.fmt(data.driver$time)
    data.driver$timevar <- NA
    
    data.driver = data.driver[data.driver$lap <= min(
      max(data.driver$lap[data.driver$driverId == input$driver.a]),
      max(data.driver$lap[data.driver$driverId == input$driver.b])),]
    
    for (i in 1:max(data.driver$lap)) {
      data.driver$timevar[data.driver$lap == i] <- data.driver$OS [data.driver$lap == i & data.driver$driverId != input$driver.a] - data.driver$OS [data.driver$lap == i & data.driver$driverId == input$driver.a]
      data.driver$timevar[data.driver$lap == i & data.driver$driverId == input$driver.a] <- 0
    }
    data.driver$OS <- as.numeric(data.driver$OS)
    data.driver$timevar <- as.numeric(data.driver$timevar) 
    data.driver
    
    
    # data.driver = data.driver
    # data.driver$OS <- time.laps.fmt(data.driver$time)
    # data.driver$timevar <- NA
    # 
    # for (i in 1:max(data.driver$lap)) {
    #   data.driver$timevar[data.driver$lap == i] <- data.driver$OS [data.driver$lap == i & data.driver$driverId != "max_verstappen"] - data.driver$OS [data.driver$lap == i & data.driver$driverId == "max_verstappen"]
    #   data.driver$timevar[data.driver$lap == i & data.driver$driverId == "max_verstappen"] <- 0
    # }
    # data.driver$OS <- as.character(data.driver$OS)
    # data.driver$timevar <- as.character(data.driver$timevar) 
    # data.driver
    
    
    
    
    
  })
  
  # Season -------------------------------------------------------------
  output$season.tbl.year <- DT::renderDataTable({
    datatable(season.data.year(), escape = FALSE, selection = 'single')
  })

  
    
  # Lap Charts --------------------------------------------------------------
  
  output$LapChart <- renderPlotly({
    data.race.time=data.race()
    data.race.time$time <- ms(data.race.time$time)
    data.race.time$OS <- as.duration(data.race.time$time)
    plot_ly(data.race.time[data.race.time$OS >= (quantile(data.race.time$OS,input$time.control[1]))  & data.race.time$OS <= (quantile(data.race.time$OS,input$time.control[2])),],x =lap, y = OS, text = paste("Driver: ", driverId, "Time: ", time),mode = "markers", color = driverId)
  })
  
  output$LapChart5 <- renderPlotly({
    data.race.time=data.race()
    data.race.time$time <- ms(data.race.time$time)
    data.race.time$OS <- as.duration(data.race.time$time)
    plot_ly(data.race.time[data.race.time$OS >= (quantile(data.race.time$OS,input$time.control[1]))  & data.race.time$OS <= (quantile(data.race.time$OS,input$time.control[2])),], x = driverId, y = OS, color = driverId, type = "box")
  })
  
  output$laps.dif.driver <- DT::renderDataTable({
    data.race.time=data.race()
    data.race.time$time <- ms(data.race.time$time)
    data.race.time$OS <- as.duration(data.race.time$time)
    datatable(data.race.time, escape = FALSE, selection = 'single')
  })
  
  # Laptable ----------------------------------------------------------------
  output$laps.tbl.race <- DT::renderDataTable({
    datatable(data.race.time(), escape = FALSE, selection = 'single')
  })
  
  

# Driver Position ---------------------------------------------------------
  output$alldriverpos <- renderPlotly({
    data.race.time=data.race()
    data.race.time$time <- ms(data.race.time$time)
    data.race.time$OS <- as.duration(data.race.time$time)
    plot_ly(data.race.time,x =driverId, y = position, color = driverId, type = "box")
  })

# Driver charts -----------------------------------------------------------

  output$DriverChart1 <- renderPlot({
    
    data.driver = data.driver()
    data.var = data.driver[data.driver$timevar >= (quantile(data.driver$timevar, input$time.control[1]))
                           & data.driver$timevar <= (quantile(data.driver$timevar, input$time.control[2])),]
    
    ggplot(data = data.var[data.var$driverId == input$driver.b,]) +
      aes(x=lap, y = timevar, colour = timevar) +
      geom_point(size = 2, na.rm = TRUE,show.legend = FALSE) +
      geom_smooth(colour = "orange", na.rm = TRUE) +
      scale_y_reverse() +
      scale_colour_gradientn(colours = rainbow(3)) +
      theme_custom2() +
      ggtitle(paste0("Laptiming variance of ",input$driver.b, " relative to: ",input$driver.a))
    
    
  })  
  
  output$DriverChart2 <- renderPlot({
    data.driver = data.driver()
    
    data.var = data.driver[data.driver$timevar >= (quantile(data.driver$timevar, input$time.control[1]))
                           & data.driver$timevar <= (quantile(data.driver$timevar, input$time.control[2])),]
    
    data.pos = data.driver[data.driver$OS >= (quantile(data.driver$OS, input$time.control[1]))
                           & data.driver$OS <= (quantile(data.driver$OS, input$time.control[2])),]
    
    
    pVARb <-ggplot(data = data.var[data.var$driverId == input$driver.b,]) 
    
    POSa <- ggplot(data = data.pos[data.pos$driverId == input$driver.a,]) 
    POSb <- ggplot(data = data.pos[data.pos$driverId == input$driver.b,]) 
    
    p2 <- pVARb +
      aes(x = factor(driverId), y = timevar, na.rm = TRUE) +
      geom_violin(fill = "steelblue",na.rm = TRUE) +
      theme_custom3() + 
      coord_flip() +
      ggtitle(paste0("Laptiming variance of ",input$driver.b, " relative to: ",input$driver.a))
    
    p2.a <- POSa +
      aes(x = factor(driverId), y = OS, na.rm = TRUE) +
      geom_violin(fill = "orange",na.rm = TRUE) +
      theme_custom3() + 
      coord_flip() +
      ggtitle(paste0("Laptime distrubution of ",input$driver.b))
    
    p2.b <- POSb +
      aes(x = factor(driverId), y = OS, na.rm = TRUE) +
      geom_violin(fill = "steelblue",na.rm = TRUE) +
      theme_custom3() + 
      coord_flip() +
      ggtitle(paste0("Laptime distrubution of ",input$driver.a))
    
    grid.arrange(p2, p2.a,p2.b, ncol = 1, nrow = 3)
    
  })  
  
  
  output$driver.tbl <- DT::renderDataTable({
    data=data.driver()
    datatable(data[,c(2:7,9)], escape = FALSE, selection = 'single',rownames = FALSE, options = list(dom = 'tp')) %>% 
      formatStyle(columns = c(1:7), color = 'black')
  })


  
})
