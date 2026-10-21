#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

if (!require(pacman)) install.packages("pacman")
p_load(shiny, bslib, tidyverse, ggplot2, ggforce)

# Define UI for application that draws multiple best-response curves
ui <- page_fluid(

    # Application title
    title = "Drawing Mixed Strategy Graphs, Step by Step",

    # Step 0 card
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
    
    # Step 1 card
    card(
      layout_sidebar(
        card_header("Step 1: Draw the lines of indifference"),
        sidebar = sidebar(
          uiOutput("slider_first_prob"),
          uiOutput("slider_second_prob")
        ),
        plotOutput("stepOne")
      )
    ),
    
    # Step 2 card
    card(
      layout_sidebar(
        card_header("Step 2: Mark all best responses"),
        sidebar = sidebar(
          uiOutput("pure_strat_a_selector"),
          uiOutput("pure_strat_b_selector")
        ),
        plotOutput("stepTwo")
      )
    ),
    
    # Step 3 card
    card(
      layout_sidebar(
        card_header("Step 3: Mark the Nash equilibrium"),
        sidebar = sidebar(
          
        ),
        plotOutput("stepThree")
      )
    )
  )

  
# Define plot rendering logic
server <- function(input, output, session) {
  
  ## Output plots ##
  
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
        y = -0.02,
        label = as.character(input$s_1_1),
        colour = "purple"
      ) +
      annotate(
        geom = "label",
        x = 0.03,
        y = -0.02,
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
    base_plot +
      geom_segment(
        aes(
          x = input$p_val,
          y = 0,
          xend = input$p_val,
          yend = 1
        ),
        linewidth = 1.5,
        colour = "red" # check whether there is a colour selector component
      ) +
      geom_segment(
        aes(
          x = 0,
          y = input$q_val,
          xend = 1,
          yend = input$q_val
        ),
        linewidth = 1,
        colour = "blue"
      )
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
      geom_segment(
        aes(
          x = input$p_val,
          y = 0,
          xend = input$p_val,
          yend = 1
        ),
        linewidth = 1.5,
        colour = "red" # check whether there is a colour selector component
      ) +
      geom_segment(
        aes(
          x = 0,
          y = input$q_val,
          xend = 1,
          yend = input$q_val
        ),
        linewidth = 1,
        colour = "blue"
      ) +
      theme_classic()
    if (input$brs_a == as.character(input$s_1_1) & input$brs_b == as.character(input$s_2_1)) {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 1,
            xend = input$p_val,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 0,
            xend = 1,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = 0,
            xend = 1,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = input$q_val,
            xend = 0,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
      two_plot
    } else if (input$brs_a == as.character(input$s_1_1) & input$brs_b == as.character(input$s_2_2)) {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = input$p_val,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 1,
            xend = 1,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = 0,
            xend = 1,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = input$q_val,
            xend = 0,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
      two_plot
    } else if (input$brs_a == as.character(input$s_1_2) & input$brs_b == as.character(input$s_2_1)) {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 1,
            xend = input$p_val,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 0,
            xend = 1,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = 0,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = input$q_val,
            xend = 1,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
      two_plot
    } else {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = input$p_val,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 1,
            xend = 1,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = 0,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = input$q_val,
            xend = 1,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
      two_plot
    }
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
      geom_segment(
        aes(
          x = input$p_val,
          y = 0,
          xend = input$p_val,
          yend = 1
        ),
        linewidth = 1.5,
        colour = "red" # check whether there is a colour selector component
      ) +
      geom_segment(
        aes(
          x = 0,
          y = input$q_val,
          xend = 1,
          yend = input$q_val
        ),
        linewidth = 1,
        colour = "blue"
      ) +
      theme_classic()
    if (input$brs_a == as.character(input$s_1_1) & input$brs_b == as.character(input$s_2_1)) {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 1,
            xend = input$p_val,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 0,
            xend = 1,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = 0,
            xend = 1,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = input$q_val,
            xend = 0,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
    } else if (input$brs_a == as.character(input$s_1_1) & input$brs_b == as.character(input$s_2_2)) {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = input$p_val,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 1,
            xend = 1,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = 0,
            xend = 1,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = input$q_val,
            xend = 0,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
    } else if (input$brs_a == as.character(input$s_1_2) & input$brs_b == as.character(input$s_2_1)) {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 1,
            xend = input$p_val,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 0,
            xend = 1,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = 0,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = input$q_val,
            xend = 1,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
    } else {
      two_plot <- base_plot +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = input$p_val,
            yend = 0
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = input$p_val,
            y = 1,
            xend = 1,
            yend = 1
          ),
          linewidth = 1.5,
          colour = "red"
        ) +
        geom_segment(
          aes(
            x = 0,
            y = 0,
            xend = 0,
            yend = input$q_val
          ),
          linewidth = 1,
          colour = "blue"
        ) +
        geom_segment(
          aes(
            x = 1,
            y = input$q_val,
            xend = 1,
            yend = 1
          ),
          linewidth = 1,
          colour = "blue"
        )
    }
    if (input$q_val == 0 & input$brs_b == as.character(input$s_2_1)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = (1 + input$p_val) / 2,
            y0 = 0,
            a = (1 - input$p_val) / 2 + 0.05,
            b = 0.1,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
      three_plot
    } else if (input$q_val == 0 & input$brs_b == as.character(input$s_2_2)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = input$p_val / 2,
            y0 = 0,
            a = input$p_val / 2 + 0.05,
            b = 0.1,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
    } else if (input$q_val == 1 & input$brs_b == as.character(input$s_2_1)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = input$p_val / 2,
            y0 = 1,
            a = input$p_val / 2 + 0.05,
            b = 0.1,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
    } else if (input$q_val == 1 & input$brs_b == as.character(input$s_2_2)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = (1 + input$p_val) / 2,
            y0 = 1,
            a = (1 - input$p_val) / 2 + 0.05,
            b = 0.1,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
    } else if (input$p_val == 0 & input$brs_a == as.character(input$s_1_1)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = 0,
            y0 = (1 + input$q_val) / 2,
            a = 0.05,
            b = (1 - input$q_val) / 2 + 0.05,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
    } else if (input$p_val == 0 & input$brs_a == as.character(input$s_1_2)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = 0,
            y0 = input$q_val / 2,
            a = 0.05,
            b = input$q_val / 2 + 0.05,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
    } else if (input$p_val == 1 & input$brs_a == as.character(input$s_1_1)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = 1,
            y0 = (1 + input$q_val) / 2,
            a = 0.05,
            b = (1 - input$q_val) / 2 + 0.05,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
    } else if (input$p_val == 0 & input$brs_a == as.character(input$s_1_1)) {
      three_plot <- two_plot +
        geom_ellipse(
          aes(
            x0 = 1,
            y0 = input$q_val / 2,
            a = 0.05,
            b = input$q_val / 2 + 0.05,
            angle = 0
          ),
          linewidth = 1,
          colour = "purple"
        )
    } else {
      three_plot <- two_plot +
        annotate(
          "point",
          x = input$p_val,
          y = input$q_val,
          shape = 21,
          size = 10,
          stroke = 1,
          colour = "purple"
        )
    }
    if (input$brs_a == as.character(input$s_1_1) & input$brs_b == as.character(input$s_2_1)) {
      three_b_plot <- three_plot +
        annotate(
          "point",
          x = 1,
          y = 0,
          shape = 21,
          size = 10,
          stroke = 1,
          colour = "purple"
        ) +
        annotate(
          "point",
          x = 0,
          y = 1,
          shape = 21,
          size = 10,
          stroke = 1,
          colour = "purple"
        )
      three_b_plot
    } else if (input$brs_a == as.character(input$s_1_2) & input$brs_b == as.character(input$s_2_2)) {
      three_b_plot <- three_plot +
        annotate(
          "point",
          x = 0,
          y = 0,
          shape = 21,
          size = 10,
          stroke = 1,
          colour = "purple"
        ) +
        annotate(
          "point",
          x = 1,
          y = 1,
          shape = 21,
          size = 10,
          stroke = 1,
          colour = "purple"
        )
      three_b_plot
    } else {
      three_plot
    }
  })
  
  ## Input-dependent UI selectors ##
  
  output$pure_strat_a_selector <- renderUI({
    selectInput(
      "brs_a",
      str_c("Enter player A's best response when ", input$y_var, " = 0"),
      list(
        as.character(input$s_1_1),
        as.character(input$s_1_2)
      )
    )
  })
  
  output$slider_first_prob <- renderUI({
    sliderInput(
      "p_val",
      str_c("Probability of ", input$x_var, ":"),
      min = 0,
      max = 1,
      value = 0.5
    )
  })
  
  output$slider_second_prob <- renderUI({
    sliderInput(
      "q_val",
      str_c("Probability of ", input$y_var, ":"),
      min = 0,
      max = 1,
      value = 0.5
    )
  })

  output$pure_strat_a_selector <- renderUI({
    selectInput(
      "brs_a",
      str_c("Enter player A's best response when ", input$y_var, " = 0"),
      list(
        as.character(input$s_1_1),
        as.character(input$s_1_2)
      )
    )
  })
  
  output$pure_strat_b_selector <- renderUI({
    selectInput(
      "brs_b",
      str_c("Enter player B's best response when ", input$x_var, " = 0"),
      list(
        as.character(input$s_2_1),
        as.character(input$s_2_2)
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)