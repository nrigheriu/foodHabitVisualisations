library(sf)
map = leaflet()
map = addProviderTiles(map, "Stamen.TonerLite", group = "Toner Lite", options = providerTileOptions(minZoom = 2, maxZoom = 6))
geoFileName = "output.geo.json"
#geoFile = readOGR(geoFileName, "OGRGeoJSON")
geoFile = as(sf::st_read(geoFileName), "Spatial")

map = setView(map, 0, 27, zoom = 2)

shinyServer(function(input, output, session){
  
  convertedData = reactive({
    yearData = switch(as.character(input$Year),
                      "1963" = geoFile$X1963, "1968" = geoFile$X1968,"1973" = geoFile$X1973,"1978" = geoFile$X1978,
                      "1983" = geoFile$X1983,"1988" = geoFile$X1988,"1993" = geoFile$X1993,"1998" = geoFile$X1998,
                      "2003" = geoFile$X2003, "2008" = geoFile$X2008, "2013" = geoFile$X2013)
   
    fileFrame = data.frame(yearData)
    
    milkValues = numeric(length = 180)
    teaValues = numeric(length = 180)
    fishValues = numeric(length = 180)
    meatValues = numeric(length = 180)
    vegetableValues = numeric(length = 180)
    alcoholValues = numeric(length = 180)
    animalFatValues = numeric(length = 180)
    starchyRootValues = numeric(length = 180)
    legumeValues = numeric(length = 180)
    sugarValues = numeric(length = 180)
    fruitValues = numeric(length = 180)
    cerealValues = numeric(length = 180)
    eggValues = numeric(length = 180)
    coffeeValues = numeric(length = 180)
    
    for (i in 1:nrow(fileFrame)){
          currentRow = fileFrame[[1]][i]
          currentRow = as.character(currentRow)
          if (grepl("\\d", currentRow)){
            split1 = strsplit(currentRow, "{ \"Milk - Excluding Butter\": ", fixed = TRUE)
            split2 = strsplit(split1[[1]][2], ", \"Tea (including mate)\": ", fixed = TRUE)
            split3 = strsplit(split2[[1]][2], ", \"Fish, Seafood\": ", fixed = TRUE)
            split4 = strsplit(split3[[1]][2], ", \"Meat\": ", fixed = TRUE)
            split5 = strsplit(split4[[1]][2], ", \"Vegetables\": ", fixed = TRUE)
            split6 = strsplit(split5[[1]][2], ", \"Alcoholic Beverages\": ", fixed = TRUE)
            split7 = strsplit(split6[[1]][2], ", \"Animal fats\": ", fixed = TRUE)
            split8 = strsplit(split7[[1]][2], ", \"Starchy Roots\": ", fixed = TRUE)
            split9 = strsplit(split8[[1]][2], ", \"Pulses\": ", fixed = TRUE)
            split10 = strsplit(split9[[1]][2], ", \"Sugar & Sweeteners\": ", fixed = TRUE)
            split11 = strsplit(split10[[1]][2], ", \"Fruits - Excluding Wine\": ", fixed = TRUE)
            split12 = strsplit(split11[[1]][2], ", \"Cereals - Excluding Beer\": ", fixed = TRUE)
            split13 = strsplit(split12[[1]][2], ", \"Eggs\": ", fixed = TRUE)
            split14 = strsplit(split13[[1]][2], ", \"Coffee and products\": ", fixed = TRUE)
            split15 = strsplit(split14[[1]][2], " }", fixed = TRUE)
            
            milkValues[i] = as.numeric((split2[[1]][1]))
            teaValues[i] = as.numeric((split3[[1]][1]))
            fishValues[i] = as.numeric((split4[[1]][1]))
            meatValues[i] = as.numeric(split5[[1]][1])
            vegetableValues[i] = as.numeric((split6[[1]][1]))
            alcoholValues[i] = as.numeric((split7[[1]][1]))
            animalFatValues[i] = as.numeric((split8[[1]][1]))
            starchyRootValues[i] = as.numeric((split9[[1]][1]))
            legumeValues[i] = as.numeric((split10[[1]][1]))
            sugarValues[i] = as.numeric((split11[[1]][1]))
            fruitValues[i] = as.numeric((split12[[1]][1]))
            cerealValues[i] = as.numeric((split13[[1]][1]))
            eggValues[i] = as.numeric(split14[[1]][1])
            coffeeValues[i] = as.numeric((split15[[1]][1]))
          }
          else{
            meatValues[i] = NA
            eggValues[i] = NA
            animalFatValues[i] = NA
            fishValues[i] = NA
            sugarValues[i] = NA
            alcoholValues[i] = NA
            cerealValues[i] = NA
            fruitValues[i] = NA
            coffeeValues[i] = NA
            vegetableValues[i] = NA
            teaValues[i] = NA
            starchyRootValues[i] = NA
            legumeValues[i] = NA
            milkValues[i] = NA
          }
        }
        convertedData = data.frame(meatValues, eggValues, animalFatValues, fishValues, sugarValues,
                                   alcoholValues, cerealValues, fruitValues, coffeeValues,
                                   vegetableValues, teaValues, starchyRootValues, legumeValues, milkValues)

    })
  convertedByTypeData = function(){   #this is done to avoid reloading data when changing just food type. Whole data will be reloaded and parsed only when changing year
    currentData = convertedData()
    convertedByTypeData = switch (input$foodType,
                                  "Eggs" = currentData$eggValues,
                                  "Meat" = currentData$meatValues,
                                  "Animal fat" = currentData$animalFatValues,
                                  "Milk" = currentData$fishValues,
                                  "Sugar" = currentData$sugarValues,
                                  "Fish, seafood" = currentData$alcoholValues,
                                  "Cereals" = currentData$cerealValues,
                                  "Alcoholic beverages" = currentData$alcoholValues,
                                  "Fruits" = currentData$fruitValues,
                                  "Vegetables" = currentData$vegetableValues,
                                  "Starchy roots(potatoes etc.)" = currentData$starchyRootValues,
                                  "Legumes" = currentData$legumeValues,
                                  "Tea" = currentData$teaValues,
                                  "Coffee" = currentData$coffeeValues
    )
  }

  foodScatter = reactive({
    validate(
      need(input$bmiFoodType != "", "Oops, Plot twist! Please select a food type")
    )
    foodPercentiles = subset(foodPercentiles, foodPercentiles$Item %in% input$bmiFoodType)
  })
  accumulate_by = function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  valRange = reactive({switch (input$foodType,
    "Eggs" = seq(from = 0, to = 20, by = 2),
    "Meat" = seq(from = 0, to = 130, by = 10),
    "Animal fat" = seq(from = 0, to = 30, by = 5),
    "Milk" = seq(from = 0, to = 70, by = 5),
    "Sugar" = seq(from = 0, to = 75, by = 5),
    "Fish, seafood" = seq(from = 0, to = 180, by = 10),
    "Cereals" = seq(from = 0, to = 280, by = 40),
    "Alcoholic beverages" = seq(from = 0, to = 200, by = 20),
    "Fruits" = seq(from = 0, to = 240, by = 20),
    "Vegetables" = seq(from = 0, to = 360, by = 20),
    "Starchy roots(potatoes etc.)" = seq(from = 0, to = 320, by = 20),
    "Legumes" = seq(from = 0, to = 26, by = 2),
    "Tea" = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.3, 1.6, 1.9, 2.2, 2.5, 2.8, 3.1, 3.4, 3.7, 4,
              5.1),
    "Coffee" = seq(from = 0, to = 14, by = 2)
  )
  })
  timelineData = reactive({
    validate(
      need(input$plotlyCountry != "", "Oops, Plot twist! Please select a country"),
      need(input$plotlyFoodType != "", "Oops, Plot twist! Please select a food type")
    )
      csvFile = subset(csvFile, csvFile$Country %in% c(input$plotlyCountry))
      csvFile = subset(csvFile, csvFile$Item %in% c(input$plotlyFoodType))
      csvFile = subset(csvFile, Year >= input$plotlyYear[1] & Year <= input$plotlyYear[2])
  })
  observeEvent(input$jumpToP2, {
       #updateNavlistPanel(session, "pages", selected = "consumption")
    clickData = clickedCountry()
    })
  popFunction = function(convertedByTypeData, foodParameter){
    paste(convertedByTypeData, " kg ", input$foodType, " per year per person", sep="")
  }
  clickedCountry = reactive({
    event = input$choropleth_shape_click
    reverseCode = revgeocode(location = c(event$lng, event$lat), output = "more")
    countryClicked = as.character(reverseCode$country)
    if (length(countryClicked) > 0){ #if a country was found
      clickedCountry = countryClicked
      print(paste("setting coutnry", seeDetailsCountry))
    }
    else{
      print("country not found")
    }
  })
  output$choropleth <- renderLeaflet({
    valuesRange = valRange()
    convertedByTypeData = convertedByTypeData()
    pal <- colorNumeric(
      palette = "PuBuGn",
      domain = valuesRange
    )
    # add the polygons to the map
    map = addPolygons(map,
                    data = geoFile,
                    stroke = FALSE,
                    smoothFactor = 0.2,
                    fillOpacity = 0.6,
                    color = ~pal(convertedByTypeData),
                    popup = popFunction(convertedByTypeData, input$foodType),
    )
 
    # add legend
    map = addLegend(map, "bottomright", pal = pal, values = valuesRange,
                  title = "Color coding",
                  labFormat = labelFormat(prefix = " "),
                  opacity = 0.75
    )
    map
  }) 
  output$plotlyput <- renderPlotly({
      timelineData = timelineData()
      timelineData = timelineData %>% accumulate_by(~timelineData$Year)
      timelinePlot =  ggplot(data = timelineData,
                             aes(x = timelineData$Year, y = timelineData$Value,
                                by = timelineData$Country, color = timelineData$Country,
                                shape = timelineData$Item))#, frame = frame))  
      timelinePlot = timelinePlot + geom_line() + geom_point(size=4)
      timelinePlot = timelinePlot + labs(x = "Year", y = "Kilos per year per person", title = "Consumption") 
      timelinePlot = timelinePlot + scale_color_hue("Legend", l = 70, c = 130) + ggthemes::theme_few()
  })
  
  
  output$bmiScatter <- renderPlotly({
    foodScatter = foodScatter()
    x = list(title = "Mean BMI (Body mass index)")
    y = list(title = "Percentile value")
    bmiPlot = plot_ly(data = foodScatter, x = foodScatter$Mean.BMI, y = foodScatter$medianValues,
                      color = foodScatter$Item, #size = foodScatter$Total.population,
                      hoverinfo = 'text', text = ~paste('Country: ', foodScatter$Country,
                                                        '<br> BMI value: ', foodScatter$Mean.BMI,
                                                        '<br> Percentile value: ', foodScatter$medianValues)) %>%
      layout(xaxis = x, yaxis = y)
  })
  output$description <- renderUI({
    HTML(paste("Percentile value is computed as follows:", 
            "the 100th Percentile is the country which 
                     consumes the most of a particular item from all available countries, whereas
                     the first is the one which consumes the least. ",
            "The 50th percentile is the world median value of 
                     consumption of the respective item",
            "All data is from year 2013", sep = "<br/>"))
  })
  
})