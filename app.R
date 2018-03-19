library(shiny)
library(plotly)
library(shinyjs)
rand <- function() {
  runif(1, min=1, max=9)
}

ui <- fluidPage(
  # includeCSS("styles.css"),

  headerPanel(h1("Streaming in Plotly: Multiple Traces", align = "center")),
  br(),
  div(actionButton("button", "Extend Traces"),actionButton("buttonReset", "Reset Traces"), align = "center"),
  br(),
  div(plotlyOutput("plot"), id='graph'),
  useShinyjs()
)

server <- function(input, output, session) {
  values <- reactiveValues()

  values$p <- plot_ly(
      type = 'scatter',
      mode = 'lines'
    ) %>%
      add_trace(
        y = c(rand(),rand(),rand()),
        line = list(
          color = '#25FEFD',
          width = 3
        )
      ) %>%
      add_trace(
        y = c(rand(),rand(),rand()),
        line = list(
          color = '#636EFA',
          width = 3
        )
      ) %>%
      layout(
        yaxis = list(range = c(0,10))
      )


  output$plot <- renderPlotly({values$p})

  observe({
    invalidateLater(1000, session)
    req(input$button > 0)
    plotlyProxy("plot", session) %>%
          plotlyProxyInvoke("extendTraces", list(y=list(list(rand()), list(rand()))), list(1,2))
    })
  observeEvent(input$buttonReset,{
      values$p  <- plot_ly(
        type = 'scatter',
        mode = 'lines'
      ) %>%
        add_trace(
          y = c(rand(),rand(),rand()),
          line = list(
            color = '#25FEFD',
            width = 3
          )
        ) %>%
        add_trace(
          y = c(rand(),rand(),rand()),
          line = list(
            color = '#636EFA',
            width = 3
          )
        ) %>%
        layout(
          yaxis = list(range = c(0,10))
        )
    runjs("Shiny.onInputChange('button',0)")
  })

}
shinyApp(ui, server)
