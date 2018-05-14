library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Asap');
                    
                    h2 {
                    font-family: 'Asap';
                    font-weight: 500;
                    line-height: 1.1;
                    color: #000000;
                    }
                    
                    body {
                    background : #ccdfff;
                    font-family: 'Asap';
                    font-weight: 500;
                    line-height: 1.1;
                    color: #000000;
                    }

                    #firstPlot{height:80vh !important;}
                    #regularPlot{height:80vh !important;}
                    #distPlot{height:80vh !important;}
                    #secondPlot{height:80vh !important;}
                    #boxPlot{height:80vh !important;}

                    "))
    ),
  # Application title
  titlePanel("The World - Dusk of 20th Century and the Dawn of 21st Century"),
  
  # Sidebar with stuff
  sidebarLayout(
    
    sidebarPanel(
      selectInput("reg_choice", "Regression Variable [1]",
                  c("Gold Price" = "gold_price","Gasoline Spot" = "gasoline_spot","WTI Spot" = "wti_spot",
                    "Brent Spot" = "brent_spot",
                    "Jet Fuel Spot" = "jet_fuel_spot", "Finance Crisis"="financial_crisis",
                    "Gold Price - WTI Spot Ratio"="gold_price_wti_spot_ratio",
                    "Gold Price - Brent Spot Ratio"="gold_price_brent_spot_ratio")),
      verbatimTextOutput("custom_text"),
      checkboxGroupInput(inputId="choices", "Select Variables [2][3][4][5]", 
                         c("Gold Price" = "gold_price","Gasoline Spot" = "gasoline_spot","WTI Spot" = "wti_spot",
                           "Brent Spot" = "brent_spot",
                           "Jet Fuel Spot" = "jet_fuel_spot", "Finance Crisis"="financial_crisis",
                           "Gold Price - WTI Spot Ratio"="gold_price_wti_spot_ratio",
                           "Gold Price - Brent Spot Ratio"="gold_price_brent_spot_ratio"),
                         selected = c("gold_price")),
      sliderInput("slider2", label = h3("Timeline Range"), min = 1990, 
                  max = 2018, value = c(1990,2018), sep = ""),
      #submitButton("Update Charts"), hr(), 
      actionButton("help", "Help")
    ),
    
    # space for plot(s)
    mainPanel(
      tabsetPanel(
        tabPanel(
          "[1] Line Graph - Regression",plotlyOutput("firstPlot",height = "100%",width = "100%")
        ),
        tabPanel(
          "[2] Original Line Graph",plotlyOutput("regularPlot",height = "100%",width = "100%")
        ),
        tabPanel(
          "[3] Normalized Line Graph",plotlyOutput("distPlot",height = "100%",width = "100%")
        ),
        tabPanel(
          "[4] Spark Line Graph",plotlyOutput("secondPlot",height = "100%",width = "100%")
        ),
        tabPanel(
          "[5] Box Plot",plotlyOutput("boxPlot")
        )
      )
      
    )
  )
    )

###########################################################

server <- function(input, output) {
  
  #setwd("C:\\Users\\Kavish\\OneDrive - mezza9 solutions sdn bhd\\_Data Science - UM\\Group Assignment\\FINAL")
  
  data <- read.csv("./data/final_dataset.csv", fileEncoding="UTF-8-BOM")
  data$time <- as.Date(as.character(data$time))
  
  unchanged_data <- data
  data2 <- data
  original_data <- data
  
  #function to normalize the variables
  normalize <- function(x) {
    return ((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))
  }
  
  counter <<- 0
  
  #normalize and stack on top 
  normalize2 <- function(x) {
    counter <<- counter + 1
    return ((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T))+counter)
  }
  
  #normalize variables here
  data$gold_price <- normalize(data$gold_price)
  data$wti_spot <- normalize(data$wti_spot)
  data$brent_spot <- normalize(data$brent_spot)
  data$gasoline_spot <- normalize(data$gasoline_spot)
  data$jet_fuel_spot <- normalize(data$jet_fuel_spot)
  data$financial_crisis <- normalize(data$financial_crisis)
  data$gold_price_wti_spot_ratio <- normalize(data$gold_price_wti_spot_ratio)
  data$gold_price_brent_spot_ratio <- normalize(data$gold_price_brent_spot_ratio)
  
  #create reactive container
  original_action <- reactive({
    sel <- input$choices
    
    #create custom data frame
    df <- original_data %>%
      select("time", sel) %>%
      gather(key = "variable", value = "value", -time)
    return(df)
  })
  
  #temp create reactive container
  temp_action <- reactive({
    sel <- input$reg_choice
    
    modified_data <- subset( x = unchanged_data, 
                             subset = (as.Date(paste(toString(input$slider2[1]),"-01-01",sep="")) <= time) & (as.Date(paste(toString(input$slider2[2]),"-01-01",sep="")) >= time))
    
    #create custom data frame
    df <- modified_data %>%
      select("time", sel) %>%
      gather(key = "variable", value = "value", -time)
    return(df)
  })
  
  # create line equation
  lm_eqn <- function(df){
    
    m <- lm(value ~ time, df);
    # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
    #                  list(a = format(coef(m)[1], digits = 2), 
    #                       b = format(coef(m)[2], digits = 2), 
    #                       r2 = format(summary(m)$r.squared, digits = 3)))
    #as.character(as.expression(eq));
    return (format(summary(m)$r.squared, digits = 3))
  }
  
  output$custom_text <- renderText({paste("R-squared: ",lm_eqn(temp_action()))})
  
  saved <- geom_smooth(method='lm',formula=y~x)
  #render the regression plot
  output$firstPlot <- renderPlotly({
    ggplot(temp_action(), aes(x = time, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      theme_minimal() + labs(x = "Timeline", y="Value") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   limits = c(as.Date(paste(toString(input$slider2[1]),"-01-01",sep="")), 
                              as.Date(paste(toString(input$slider2[2]),"-01-01",sep="")))) +
      saved + 
      geom_text(x = -Inf, y = Inf, hjust = 0, vjust = 1, label = lm_eqn(temp_action()), parse = TRUE, size = 5)
  })
  
  
  ## start original plot
  
  #render the original plot
  output$regularPlot <- renderPlotly({
    
    validate(
      need(input$choices, 'Check at least one variable at the side panel!')
    )
    
    ggplot(original_action(), aes(x = time, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      theme_minimal() + labs(x = "Timeline", y="Original Value") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   limits = c(as.Date(paste(toString(input$slider2[1]),"-01-01",sep="")), 
                              as.Date(paste(toString(input$slider2[2]),"-01-01",sep="")))) 
  })
  
  ## end original plot
  
  
  #create reactive container
  action <- reactive({
    sel <- input$choices
    
    #create custom data frame
    df <- data %>%
      select("time", sel) %>%
      gather(key = "variable", value = "value", -time)
    return(df)
  })
  
  #render the normalized plot
  output$distPlot <- renderPlotly({
    
    validate(
      need(input$choices, 'Check at least one variable at the side panel!')
    )
    
    ggplot(action(), aes(x = time, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      theme_minimal() + labs(x = "Timeline", y="Normalized Value") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   limits = c(as.Date(paste(toString(input$slider2[1]),"-01-01",sep="")), 
                              as.Date(paste(toString(input$slider2[2]),"-01-01",sep="")))) 
  })
  
  
  #### Extra Stuff - Second Plot####
  
  #normalize variables here
  data2$gold_price <- normalize2(data2$gold_price)
  data2$wti_spot <- normalize2(data2$wti_spot)
  data2$brent_spot <- normalize2(data2$brent_spot)
  data2$gasoline_spot <- normalize2(data2$gasoline_spot)
  data2$jet_fuel_spot <- normalize2(data2$jet_fuel_spot)
  data2$financial_crisis <- normalize2(data2$financial_crisis)
  data2$gold_price_wti_spot_ratio <- normalize2(data2$gold_price_wti_spot_ratio)
  data2$gold_price_brent_spot_ratio <- normalize2(data2$gold_price_brent_spot_ratio)
  
  #create reactive container
  action2 <- reactive({
    sel <- input$choices
    
    #create custom data frame
    df <- data2 %>%
      select("time", sel) %>%
      gather(key = "variable", value = "value", -time)
    return(df)
  })
  
  #date filtering?
  
  output$secondPlot <- renderPlotly({
    
    validate(
      need(input$choices, 'Check at least one variable at the side panel!')
    )
    
    ggplot(action2(), aes(x = time, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      theme_minimal() + labs(x = "Timeline", y="Normalized Value") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   limits = c(as.Date(paste(toString(input$slider2[1]),"-01-01",sep="")), 
                              as.Date(paste(toString(input$slider2[2]),"-01-01",sep=""))))
    
    
  })
  
  ## start boxplot logic
  
  boxplot_action <- reactive({
    sel <- input$choices
    
    modified_data <- subset( x = unchanged_data, 
                             subset = (as.Date(paste(toString(input$slider2[1]),"-01-01",sep="")) <= time) & (as.Date(paste(toString(input$slider2[2]),"-01-01",sep="")) >= time))
    #create custom data frame
    df <- modified_data %>%
      select("time", sel) %>%
      gather(key = "variable", value = "value", -time)
    return(df)
  })
  
  output$boxPlot <- renderPlotly({
    
    validate(
      need(input$choices, 'Check at least one variable at the side panel!')
    )
    
    ggplot(boxplot_action(), aes(x = variable, y = value)) + 
      geom_boxplot() + 
      facet_wrap( ~ variable, scales="free") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  
  ## end boxplot logic
  
  
  ## help section :/ #
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help",
      HTML("This app was designed to enable users to explore datasets and facilitate data analysis. Currently only time series datasets are compatible.<br><br>
            1. Changing the regression variable will only affect Tab 1. The R-squared value of the regression line created is displayed at the side panel.<br><br>
            2. Selecting the variables in the checkbox will only affect Tab 2 to Tab 5.<br><br>
            3. Changing the timeline range will affect all of the plots.<br><br>
            4. Changing any of the variables at the side panel will automatically regenerate all the affected plots.<br><br>
            5. The Plotly features may be used to customise and download the generated plots.<br><br><br><br>
            Created by Kavish Punchoo and Niraj Baxi")
    ))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)