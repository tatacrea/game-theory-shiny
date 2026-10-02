#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(bslib)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- page_fluid(

    # Application title
    title = "Drawing Mixed Strategy Graphs, Step by Step",

    # Show a plot of the generated distribution
    card(
      layout_sidebar(
        card_header("Step 0: Familiarise yourself with the axes"),
        sidebar = sidebar(
          textInput(
            "x_var",
            "Name of Agent 1's probability:",
            value = "p"
          ),
          textInput(
            "y_var",
            "Name of Agent 2's probability:",
            value = "q"
          ),
          hr(), 
          textInput(
            "s_1_1",
            "Name of Agent 1's first strategy:",
            value = "A"
          ),
          textInput(
            "s_1_2",
            "Name of Agent 2's first strategy:",
            value = "B"
          ),
          textInput(
            "s_2_1",
            "Name of Agent 1's first strategy:",
            value = "a"
          ),
          textInput(
            "s_2_2",
            "Name of Agent 2's first strategy:",
            value = "b"
          )
        ),
        plotOutput("stepZero")
      )
    ),
    card(
      layout_sidebar(
        card_header("Step 1: Mark any pure-strategy Nash equilibrium"),
        sidebar = sidebar(
          selectInput(
            "pure_strategies",
            "Are there any Nash equilibria in pure strategies?",
            list("Yes", "No"),
            selected = "Yes"
          ),
          hr()
        ),
        plotOutput("stepOne")
      )
    ),
    card(
      layout_sidebar(
        card_header("Step 2: Draw the lines of indifference"),
        sidebar = sidebar(
          sliderInput(
            "pval",
            "Probability of p:",
            min = 0,
            max = 1,
            value = 0.5
          ),
          sliderInput(
            "qval",
            "Probability of q:",
            min = 0,
            max = 1,
            value = 0.5
          )
        ),
        plotOutput("stepTwo")
      )
    ),
    card(
      layout_sidebar(
        card_header("Step 3: Connect each player's line of indifference to their respective best responses"),
        sidebar = sidebar(
          
        ),
        plotOutput("stepThree")
      )
    ),
    card(
      layout_sidebar(
        card_header("Step 4: Mark the mixed-strategies Nash equilibrium"),
        sidebar = sidebar(
          
        ),
        plotOutput("stepFour")
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$stepZero <- renderPlot({
        # draw the best response curves
        base_plot <- ggplot() +
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
            x = as.character(input$x_var),
            y = as.character(input$y_var)
          ) +
          theme_classic()
        base_plot +
          annotate(
            geom = "label",
            x = 0.97,
            y = -0.05,
            label = as.character(input$s_1_1),
            colour = "purple"
          ) +
          annotate(
            geom = "label",
            x = 0.03,
            y = -0.05,
            label = as.character(input$s_1_2),
            colour = "purple"
          ) +
          annotate(
            geom = "label",
            x = -0.02,
            y = 0.95,
            label = as.character(input$s_2_1),
            colour = "purple"
          ) +
          annotate(
            geom = "label",
            x = -0.02,
            y = 0.05,
            label = as.character(input$s_2_2),
            colour = "purple"
          )
    })
    
    output$stepOne <- renderPlot({
      # draw the best response curves
      base_plot <- ggplot() +
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
          x = as.character(input$x_var),
          y = as.character(input$y_var)
        ) +
        theme_classic()
      base_plot
    })
    
    output$stepTwo <- renderPlot({
      # draw the best response curves
      base_plot <- ggplot() +
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
          x = as.character(input$x_var),
          y = as.character(input$y_var)
        ) +
        theme_classic()
      base_plot
    })
    
    output$stepThree <- renderPlot({
      # draw the best response curves
      base_plot <- ggplot() +
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
          x = as.character(input$x_var),
          y = as.character(input$y_var)
        ) +
        theme_classic()
      base_plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
