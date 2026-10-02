#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Drawing Mixed Strategy Graphs, Step by Step"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(
              "xvar",
              "Name of Agent 1's probability:",
              value = "p"
            ),
            textInput(
              "yvar",
              "Name of Agent 2's probability:",
              value = "q"
            ),
            textInput(
              "s11",
              "Name of Agent 1's first strategy:",
              value = "A"
            ),
            textInput(
              "s12",
              "Name of Agent 2's first strategy:",
              value = "B"
            ),
            textInput(
              "s21",
              "Name of Agent 1's first strategy:",
              value = "a"
            ),
            textInput(
              "s22",
              "Name of Agent 2's first strategy:",
              value = "b"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("stepZero")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$stepZero <- renderPlot({
        # draw the best response curves
        ggplot() +
          scale_x_continuous(
            breaks = c(0, 1),
            labels = c(0, 1)
          ) +
          scale_y_continuous(
            breaks = c(0, 1),
            labels = c(0, 1)
          ) +
          coord_cartesian(
            xlim = c(0, 1),
            ylim = c(0, 1),
            expand = F,
            clip = "off"
          ) +
          labs(
            x = as.character(input$xvar),
            y = as.character(input$yvar)
          ) +
          annotate(
            geom = "label",
            x = 0.97,
            y = -0.05,
            label = as.character(input$s11),
            colour = "purple"
          ) +
          annotate(
            geom = "label",
            x = 0.03,
            y = -0.05,
            label = as.character(input$s12),
            colour = "purple"
          ) +
          annotate(
            geom = "label",
            x = -0.02,
            y = 0.95,
            label = as.character(input$s21),
            colour = "purple"
          ) +
          annotate(
            geom = "label",
            x = -0.02,
            y = 0.05,
            label = as.character(input$s22),
            colour = "purple"
          ) +
          theme_classic()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
