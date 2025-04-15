#' Run Monty Hall Shiny App
#'
#' Launches an interactive Shiny app for Monty Hall simulations.
#'
#' @return Launches a Shiny app.
#' @examples
#' if (interactive()) run_mhp_app()
#' @export
run_mhp_app <- function() {
  ui <- bslib::page_sidebar(
    title = "Monty Hall Simulator",
    sidebar = bslib::sidebar(
      numericInput("n_doors", "Number of Doors (>= 3):", value = 3, min = 3),
      numericInput("n_sim", "Number of Simulations:", value = 1000, min = 1),
      actionButton("run", "Run Simulation")
    ),
    bslib::card(
      plotOutput("results_plot"),
      verbatimTextOutput("results_text")
    )
  )

  server <- function(input, output, session) {
    sim_results <- eventReactive(input$run, {
      monty_hall_sim(input$n_doors, input$n_sim)
    })

    output$results_plot <- renderPlot({
      results <- sim_results()
      barplot(c(results$stick, results$switch),
        names.arg = c("Stick", "Switch"),
        col = c("red", "blue"),
        ylim = c(0, 1),
        main = paste("Monty Hall with", input$n_doors, "Doors"),
        ylab = "Win Rate"
      )
    })

    output$results_text <- renderPrint({
      results <- sim_results()
      cat("Sticking Win Rate:", round(results$stick, 3), "\n")
      cat("Switching Win Rate:", round(results$switch, 3), "\n")
      cat("Total:", round(results$stick + results$switch, 3))
    })
  }

  shiny::shinyApp(ui, server)
}

