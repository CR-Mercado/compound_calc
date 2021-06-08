library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(

  h4("Compare Compounding Differences"),
  numericInput(inputId = "initial",
             label = "Initial Amount", 
             value = 1000, min = 0, max = 999999, step = 1000),
  numericInput(inputId = "rate",
               label = "Interest Rate",
               value = .05, min = 0, max = 9999, step = .05), 
hr(), 
  fluidRow(
    column(6, plotlyOutput("plotly")),
    column(6, plotlyOutput("rate_plotly"))
  )    
)

server <- function(input, output, session) {
  
  rates_by_compound <- readRDS("rates_by_compound_granular.rds")
  rates_by_compound$rate <- rates_by_compound$rate*100
  
  calc_returns <- function(initial, rate, compounds){ 
    initial * (1 + rate/compounds)^compounds
  } 
  
  totals <- reactive({ 
    list(
      annual = calc_returns(initial = input$initial, rate = input$rate,
                            compounds = 1) - input$initial, 
      semi = calc_returns(initial = input$initial, rate = input$rate,
                          compounds = 2) - input$initial,
      quarter = calc_returns(initial = input$initial, rate = input$rate,
                             compounds = 4) - input$initial,
      month = calc_returns(initial = input$initial, rate = input$rate,
                           compounds = 12) - input$initial,
      daily = calc_returns(initial = input$initial, rate = input$rate,
                           compounds = 365) - input$initial,
      continuous = input$initial * 2.7183^(input$rate)  - input$initial
    )
    })
  
  plot <- reactive({ 
    d = data.frame(id = names(totals()), vals = unlist(totals()),
                   stringsAsFactors = TRUE)
    
    d$id <- factor(d$id,
                   levels = c("annual","semi","quarter",
                              "month","daily","continuous"))
    
    ggplot(d, aes(x = id, y = vals)) + geom_bar(stat = "identity") + 
      geom_abline(slope = 0, intercept = input$initial) + theme_classic() + 
      labs(
        x = "Compound Type",
        y = "Growth Above Initial",
        title = "The benefits of continuous compounding add up!") + 
      theme(axis.title = element_text(size = rel(1.25)), 
            plot.title = element_text(size = rel(1.3), hjust = 0.5),
            plot.subtitle = element_text(size = rel(1.3), hjust = 0.5)) + 
      scale_fill_continuous(type = "viridis")
    })
  
  plot2 <- reactive({ 
    rates_by_compound$compound_type <- factor(rates_by_compound$compound_type,
                   levels = c("annual","semi","quarter",
                              "month","daily","continuous"))
      
    ggplot(rates_by_compound, aes(x = rate, y = growth, col = compound_type)) + 
      geom_line() + theme_classic() + 
      labs(
        x = "Interest Rate %",
        y = "Growth Above Initial $1,000",
        title = "Benefits of Compounding Grow with Interest Rate") + 
      theme(axis.title = element_text(size = rel(1.25)), 
            plot.title = element_text(size = rel(1.3), hjust = 0.5),
            plot.subtitle = element_text(size = rel(1.3), hjust = 0.5))
    
    })
  
  output$plotly <- renderPlotly({ 
    ggplotly(plot())
    })
  
  output$rate_plotly <- renderPlotly({ 
    ggplotly(plot2())
    })
  
}

shinyApp(ui, server)