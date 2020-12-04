month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

TPC<- function(T,Topt,CTmin, CTmax){
  F=T
  F[]=NA
  sigma= (Topt-CTmin)/4
  F[T<=Topt & !is.na(T)]= exp(-((T[T<=Topt & !is.na(T)]-Topt)/(2*sigma))^2) 
  F[T>Topt & !is.na(T)]= 1- ((T[T>Topt & !is.na(T)]-Topt)/(Topt-CTmax))^2
  #set negetative to zero
  F[F<0]<-0
  
  return(F)
}

# Read the csv file
Lepidosauria <- fread("Lepidosauria.csv")

source("cicerone.R", local = T)

shinyServer <- function(input, output, session) {
  
  observeEvent(input$tour1, guide1$init()$start())
  
  observeEvent(input$reset1, {
    reset("page")
  })
  
  observeEvent(input$tour2, guide2$init()$start())
  
  observeEvent(input$reset2, {
    reset("page")
  })
  
  # Brief background information of selected species
  output$species_info <- renderText({
    org <- input$species
    filename <- paste("Lizards/",gsub(" ","_",org),".Rmd", sep= "")
    includeMarkdown(filename)
  })
  
  # Show or hide the "Thermal performance curve" tabset depending on the species selected
  observeEvent(input$species, {
    if(is.na(Lepidosauria[Lepidosauria$Binomial == input$species, "tmin"]) || is.na(Lepidosauria[Lepidosauria$Binomial == input$species, "Topt"])) {
      hideTab(inputId = "tabset", target = "Thermal Performance Curve")
    } else {
      showTab(inputId = "tabset", target = "Thermal Performance Curve")
    }
  })
  
  # Add an option of "Thermoregulating" to the shade input if the selected species has the data for it.
  observe({
    if (is.na(Lepidosauria[Lepidosauria$Binomial == input$species, "Topt"])) {
      choices = c("Exposed", "Covered")
    } else {
      choices = c("Exposed", "Covered", "Thermoregulating")
    }
    updateCheckboxGroupInput(session, inputId = "shade", choices = choices, selected = "Exposed")
  })

  
  # MAP tab
  # Read the data file of the selected species to make a map
  data_by_org <- eventReactive(input$run, {
    org <- input$species
    filename <- paste("Data/",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename)
    
    # assign order to months, hours and scenarios so that they appear in order.
    df$Month <- factor(df$Month, levels = names(month))
    df$Hour <- factor(df$Hour, levels = hours)
    df$Scenario <- factor(df$Scenario, levels = scenarios)
    
    df
  }, ignoreNULL = FALSE
  )

  # Filter data by the inputs to map the distribution
  dataInput <- eventReactive(input$run, {
    df <- data_by_org() %>% filter(Hour %in% input$hour & Month %in% input$month & Scenario %in% input$scenario)
    
    if ("Thermoregulating" %in% input$shade) {
      if (!is.na(Lepidosauria[Lepidosauria$Binomial == input$species, "Topt"])) {
        tmax <- Lepidosauria[Lepidosauria$Binomial == input$species, Tmax]
        topt <- Lepidosauria[Lepidosauria$Binomial == input$species, Topt]
        diff <- tmax - topt
        df$check <- F
        for (x in unique(df$x)) {
          for (y in unique(df$y)) {
            byPoint <- df[df$x == x & df$y == y, ]
            len <- dim(byPoint)[1]
            if (len != 0) {
              for(i in 1:(len/2)) {
                newRow <- byPoint[i,]
                newRow$Shade <- "Thermoregulating"
                tsm_ex <- byPoint[i, "Tsm"]
                tsm_co <- byPoint[i + len/2, "Tsm"]
                if (tsm_ex > diff) {
                  newRow$Tsm <- tsm_ex
                } else if (tsm_co < diff) {
                  newRow$Tsm <- tsm_co
                } else {
                  newRow$Tsm <- diff
                }
                df <- rbind(df, newRow)
              }
            }
          }
        }
      }
    }
    df <- filter(df, Shade %in% input$shade)
    df
  }, ignoreNULL = FALSE
  )
  
  # make input$columns responsive to input$run
  x_variable <- eventReactive( input$run, {
    input$columns
  }, ignoreNULL = FALSE
  )
  
  # make input$rows responsive to input$run
  y_variable <- eventReactive( input$run, {
    input$rows
  }, ignoreNULL = FALSE
  )
  
  #Title of the map
  title <- eventReactive( input$run, {
    str <- input$species
    if (x_variable() != "Month" && y_variable() != "Month") {
      str <- paste(str, "|", input$month)
    }
    if (x_variable() != "Hour" && y_variable() != "Hour") {
      str <- paste(str, "|", input$hour)
    }
    if (x_variable() != "Scenario" && y_variable() != "Scenario") {
      str <- paste(str, "|", input$scenario)
    }
    if (x_variable() != "Shade" && y_variable() != "Shade") {
      str <- paste(str, "|", input$shade)
    }
    str
  }, ignoreNULL = FALSE
  )
  
  # World map
  output$mymap <- renderLeaflet({
    if (input$map_onoff) {
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = (max(dataInput()$x) + min(dataInput()$x)) / 2, lat = (max(dataInput()$y) + min(dataInput()$y))/2, zoom = 3) %>%
      addRectangles(lng1 = min(dataInput()$x), lng2 = max(dataInput()$x), lat1 = min(dataInput()$y), lat2 = max(dataInput()$y))
    }
  })
  
  # The main map
  output$plot1 <- renderPlot({    
    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    
    if (input$scale) {  # Discrete
      ggplot() +
        borders(fill="grey",colour="black") +
        ggtitle(title()) + xlab("Longitude (°)") + ylab("Latitude (°)") +
        geom_raster(data=dataInput(), aes(x = x, y = y, fill = cut(Tsm, c(-Inf, 0, 2, 5, Inf)))) + 
        scale_fill_manual(name = "Thermal Safety \nMargin (°C)", values = c("(-Inf,0]" = "red", "(0,2]" = "orange", "(2,5]" = "green", "(5, Inf]" = "blue"), 
                          labels = c("(-Inf,0]" = "< 0", "(0,2]" = "0 - 2", "(2,5]" = "2 - 5", "(5, Inf]" = "5 >")) +
        facet_grid(facet_formula) + coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)), expand = TRUE) +
        theme_bw( ) + theme(strip.text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) +
        #theme(plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "#F5F5F5")) +
        theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))
    } else {  # Continuous
      ggplot() +
        borders(fill="grey",colour="black") +
        ggtitle(title()) + xlab("Longitude (°)") + ylab("Latitude (°)") +
        geom_raster(data=dataInput(), aes(x = x, y = y, fill = Tsm)) + 
        scale_fill_gradientn(name = "Thermal Safety \nMargin (°C)", colors = heat.colors(12), breaks = seq(-50, 50, by = 5)) +
        facet_grid(facet_formula) + coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)), expand = TRUE) +
        theme_bw( ) + theme(strip.text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) +
        #theme(plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "#F5F5F5")) +
        theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))      
    }
  })
  
  # step <- eventReactive(input$run,{
  #   n = 1
  #   if (x_variable() == "Month" | y_variable() == "Month") {
  #     n = n * length(input$month)
  #   } 
  #   if (x_variable() == "Hour" | y_variable() == "Hour") {
  #     n = n * length(input$hour)
  #   } 
  #   if (x_variable() == "Scenario" | y_variable() == "Scenario") {
  #     n = n * length(input$scenario)
  #   } 
  #   if (x_variable() == "Shade" | y_variable() == "Shade"){
  #     n = n * length(input$shade)
  #   }
  #   n
  # }, ignoreNULL = FALSE)


  # Density plot
  output$density <- renderPlot({

    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    mx <- max(dataInput()$Tsm)
    mn <- min(dataInput()$Tsm)
    p <- ggplot(data = dataInput()) + 
      geom_density(aes(x = Tsm, y = ..scaled.., fill = Scenario), alpha = 0.5) +
      # geom_density_ridges2(aes(x = Tsm, y = 0, fill = Scenario), scale = 1, alpha = 0.5) +
      xlab("Thermal Safety Margin (°C)") + ylab("Frequency") + 
      scale_fill_manual(name = "Scenarios", values = c("blue", "orange", "green"), labels = c("Normal", "+ 1.5°C", "+ 2°C")) +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
      theme_bw() + facet_grid(facet_formula, scales = "free") +
      theme(strip.text = element_text(size = 12), plot.title = element_text(size = 18), 
            plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "azure"), 
            legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
            axis.title = element_text(size = 14), axis.text = element_text(size = 12))#, axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    # Add colored lines that correspond to the fills on the map
    if (mn < 5) {
      p <- p + geom_vline(xintercept = -0.1, size = 1.2, col = "red") + geom_segment(aes(x = -Inf, y = 0, xend = -0.1, yend = 0), col = "red", size = 1.2)
    }
    if(mx > -8 & mn < 7) {
      p <- p + geom_vline(xintercept = c(0.1, 1.9), size = 1.2, col = "orange") + geom_segment(aes(x = 0.1, y = 0, xend = 1.9, yend = 0), col = "orange", size = 1.2)
    }
    if (mx > 0 & mn < 10) {
      p <- p + geom_vline(xintercept = c(2.1, 4.9), size = 1.2, col = "green") + geom_segment(aes(x = 2.1, y = 0, xend = 4.9, yend = 0), col = "green", size = 1.2)
    }
    if (mx > 0) {
      p <- p + geom_vline(xintercept = 5.1, size = 1.2, col = "blue") + geom_segment(aes(x = 5.1, y = 0, xend = Inf, yend = 0), col = "blue", size = 1.2)
    }
    p
  })
  
  # Text output of the clicked location
  output$info <- renderText({
    validate(
      need(input$plot_click, "Click on the map to get temperature data")
    )
    tsm <- round(colMeans(nearPoints(dataInput(), input$plot_click, xvar = "x", yvar = "y")["Tsm"]), digits = 1)
    tmax <- Lepidosauria[Lepidosauria$Binomial == input$species, "Tmax"]
    output <- paste("Operative temperature:", tmax - tsm, "°C\nTSM:", tsm, "°C")
    output
  })
  
  
  # TPC tab
  # Data to be used for TPC
  dataTPC <- reactive({
    org <- input$species
    filename <- paste("Data/",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename) %>% filter(Hour %in% input$hour_tpc & Month %in% names(month[as.numeric(input$month_tpc)]) & Scenario %in% input$scenario_tpc & Shade %in% input$shade_tpc)
    Lepidosauria[Lepidosauria$Binomial == input$species, Tmax] - mean(df$Tsm)
  })
  
  # TPC plot
  output$TPC <- renderPlot({
    Lepidosauria <- Lepidosauria[Lepidosauria$Binomial == input$species, ]
    curve <- TPC(0:pmax(50, dataTPC()), Lepidosauria$Topt, as.numeric(as.character(Lepidosauria$tmin)), Lepidosauria$Tmax)
    
    ggplot() + geom_line(data = NULL, aes(x = c(0:pmax(50, dataTPC())), y = curve), col = "steelblue", size = 1.2) + xlab("Temperature (°C)") + ylab("Performance") +
      geom_segment(aes(x = Lepidosauria$Tmax - 5, y = 0.1, xend = Lepidosauria$Tmax, yend = 0), size = 1) + geom_text(aes(x = Lepidosauria$Tmax - 6, y = 0.14, label = "Tmax"), size = 5) +
      geom_vline(xintercept = dataTPC(), size = 1.2) + geom_text(aes(x = dataTPC() - 1.7, label = "Operative temperature", y = 0.45), size = 5, angle = 90) + theme_classic( ) +
      geom_rect(aes(xmin = Lepidosauria$Tmax, xmax = Inf, ymin = -Inf, ymax = Inf), alpha = 0.7, fill = "red") +
      scale_x_continuous(limits = c(0,pmax(50, dataTPC())+2), expand = c(0,0)) + scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.background = element_rect(fill = "#F5F5F5"), 
            panel.background = element_rect(fill = "azure"))
  })
  
  # PLOT tab
  # Read the data file of the selected species to make a plot
  data_by_org_2 <- reactive({
    org_2 <- input$species
    filename <- paste("Data/",gsub(" ","_",org_2),"_combined.rds", sep= "")
    df2 <- readRDS(filename)
    df2$Month <- factor(df2$Month, levels = names(month))
    df2$Hour <- factor(df2$Hour, levels = hours)
    df2$Scenario <- factor(df2$Scenario, levels = scenarios)
    df2
  })
  
  # Filter data by selected inputs to plot density
  dataInput_2 <- reactive({
    data_by_org_2() %>% filter(Month %in% names(month[as.numeric(input$month_2)]))
  })
  
  
  # The main plot
  output$plot2 <- renderPlot({
    
    if (input$facet == "Shade") {
      p <- ggplot(dataInput_2(), aes(x=Tsm, y= Hour)) + 
        geom_density_ridges2(aes(fill=Scenario), rel_min_height = 0.01, scale=2, alpha=0.5) + facet_wrap(~Shade)
    } else {
      p <- ggplot(dataInput_2(), aes(x=Tsm, y= Hour)) + 
        geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + 
        facet_wrap(~factor(Scenario, levels = scenarios)) + scale_fill_manual(values = c("blue","red"))
    }
    p <- p + ggtitle(paste(input$species,"|", names(month[as.numeric(input$month_2)]))) + 
      scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + xlab("Thermal Safety Margin (°C)") + 
      theme(strip.text = element_text(size = 12), plot.title = element_text(size = 18), 
            axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
            legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
            plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "azure"))
    p
  }, height = 600, width = 600)  
  
  # # DATA
  # data_by_org_data <- reactive({
  #   org <- input$species
  #   filename <- paste("Data/",gsub(" ","_",org),"_combined.rds", sep= "")
  #   df <- readRDS(filename)
  #   df$Month <- factor(df$Month, levels = names(month))
  #   df$Hour <- factor(df$Hour, levels = hours)
  #   df$Scenario <- factor(df$Scenario, levels = scenarios)
  #   df
  # })
  # 
  # 
  # dataInput_data <- reactive({
  #   if(input$month_data == "Annual") {
  #     data_by_org_data() %>% filter(Scenario %in% input$scenario_data & Shade %in% input$shade_data)
  #   } else {
  #     data_by_org_data() %>% filter(Month %in% names(month[as.numeric(input$month_data)]) & Scenario %in% input$scenario_data & Shade %in% input$shade_data)
  #   }
  # })
  
  # output$text_data <- renderText({
  #   str <- ""
  #   if(input$month_data == "Annual") {
  #     for(m in 1:12){
  #       count = 0
  #       for (hour in 1:length(hours)) {
  #         average <- mean(dataInput_data()[(dataInput_data()$Month == names(month[m])) & (dataInput_data()$Hour == hours[hour]), "Tsm"])
  #         if (average < input$value) {
  #           count = count + 1
  #           add <- paste(hours[hour], ": ", round(average, digits = 2), "°C")
  #           if (count == 1) {
  #             add <- HTML(paste('<b>', names(month)[m], '</b>', add, sep = '<br/>'))
  #           }
  #           str <- paste(str, add)
  #           str <- HTML(paste(str, "", sep = '<br/>'))
  #         }
  #       }
  #     }
  #   } else {
  #     for (hour in 1:length(hours)) {
  #       average <- mean(dataInput_data()[dataInput_data()$Hour == hours[hour], "Tsm"])
  #       if (average < input$value) {
  #         str <- paste(str, hours[hour], ": ", round(average, digits = 2), "°C")
  #         str <- HTML(paste(str, "", sep = '<br/>'))
  #       } 
  #     }
  #   }
  #   if(str == "") {
  #     str = "No data" 
  #   }
  #   if(input$month_data != "Annual") {
  #     str <- HTML(paste("<b>", names(month[as.numeric(input$month_data)]), "</b>", str, sep = '<br/>'))
  #   }
  #   str
  # })
}

