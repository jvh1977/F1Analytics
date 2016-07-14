##shinyLapData
#Experiments around ergast API lap time data

library(shiny);library(shinydashboard);library(plotly)

header <- dashboardHeader(title = "F1 driver analytics")

sidebar <- dashboardSidebar(
  uiOutput("seasons"),
  uiOutput("roundsps"),
  uiOutput("driver.a.Control"),
  uiOutput("driver.b.Control"),
  actionButton("goButton", "Go!"),
  uiOutput("time.control"),
  div("This demo provides a couple of views over Formula One laptime data obtained from the",
      a(href='http://ergast.com/mrd/',
        "Ergast Developer API")),
  div("The code is available as a gist:",a(href="https://gist.github.com/4188912","Shiny F1 laptime explorer"))
) # end sidebar

body <- dashboardBody(
  tabsetPanel(
    tabPanel("Season", DT::dataTableOutput("season.tbl.year"),tableOutput("values")),
    tabPanel("Driver Position", plotlyOutput("alldriverpos")),
    tabPanel("Lap Chart", plotlyOutput("LapChart"), plotlyOutput("LapChart5"),DT::dataTableOutput("laps.dif.driver")),
    tabPanel("Driver Chart", fluidRow(
             box(title = "", width = 6, background = "black", plotOutput("DriverChart1")),
             box(title = "", width = 6, background = "black", plotOutput("DriverChart2")),
             box(title = "", width = 6, background = "black",DT::dataTableOutput("driver.tbl")))),
    tabPanel("Lap Table", DT::dataTableOutput("laps.tbl.race"))
  )
) # end body

dashboardPage(header, sidebar, body)
