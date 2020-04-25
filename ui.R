shinyUI(fluidPage(id = "main",
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css")
  ),
  navbarPage(title = "Food habits", id = "pages", theme = shinytheme("flatly"),
             tabPanel(title = "Choropleth map", value = "map", fluidRow(
                 leafletOutput("choropleth", height = "850", width = "100%"),
                 absolutePanel(id = "controls",
                               top = 80, right = 40, width = 300,
                               draggable = TRUE,
                               selectInput("foodType", label = p("Food type"),
                                           choices = list("Eggs", "Meat", "Alcoholic beverages", "Cereals", "Fruits", "Milk", 
                                                          "Sugar", "Animal fat", "Fish, seafood", "Vegetables", "Tea",
                                                          "Legumes", "Coffee", "Starchy roots(potatoes etc.)")),
                               sliderInput("Year", label = p("Year"),
                                           min = 1963, max = 2013, value = 2013, step = 5) #,actionButton("jumpToP2", "See details")
             ))),
             tabPanel(title = "Consumption timelines", value = "plotly", fluidRow(
               sidebarLayout(
                 div(style="width: 60%;", sidebarPanel(
                   selectizeInput("plotlyCountry", label = "Country name(s) of Interest",
                                  choices = unique(csvFile$Country), multiple = T,
                                  options = list(maxItems = 3, placeholder = 'Select a country'), selected = "Romania"),
                   selectizeInput("plotlyFoodType", label = p("Food type(s)"), 
                                  choices = c(c("All"), levels(unique(csvFile$Item))),
                                  options = list(placeholder = "Select a type", maxItems = 3), selected = "Eggs"),
                   sliderInput("plotlyYear", label = p("Year range"),
                               min = 1963, max = 2013, value = c(1963, 2013), step = 5)
                 )),
                 mainPanel(
                   plotlyOutput("plotlyput", height = 850, width = 1300)
                 ))
             )),
             tabPanel(title = "BMI scatter plot", value = "scatter", fluidRow(
               sidebarLayout(
                 div(style = "width: 60%;", sidebarPanel(
                   selectizeInput("bmiFoodType", label = p("Food type(s)"), 
                                  choices = c(c("All"), levels(unique(csvFile$Item))),
                                  options = list(placeholder = "Select a type", maxItems = 3), selected = "Eggs"),
                   strong("Info:"),
                   htmlOutput("description")
                 )),
                 mainPanel(
                   plotlyOutput("bmiScatter", height = 850, width = 1300)
                 )
               )
             ))
  )
))