library(shiny)
source("selex_fxns.R")

server <- function(input, output, session) {
  # Input for logistic parameters;
  # make sure sliders and numeric iputs show the same thing
  observe({
    updateNumericInput(session, "par2N", value = input$par2)
  })
  observe({
    updateSliderInput(session, "par2", value = input$par2N)
  })
  observe({
    updateNumericInput(session, "par1N", value = input$par1)
  })
  observe({
    updateSliderInput(session, "par1", value = input$par1N)
  })

  # Input for double normal parameters: all parameters on the scale the user enters in SS3
  # gray out slider and write in box if using -999 instead;
  observeEvent(input$use_999_init, {
    if (input$use_999_init == TRUE) {
      disable("par.e")
      disable("par.eN")
    } else {
      enable("par.e")
      enable("par.eN")
    }
  })
  observeEvent(input$use_999_fin, {
    if (input$use_999_fin == TRUE) {
      disable("par.f")
      disable("par.fN")
    } else {
      enable("par.f")
      enable("par.fN")
    }
  })

  # make sure sliders and numeric iputs show the same thing
  observe({
    updateNumericInput(session, "par.aN", value = input$par.a)
  })
  observe({
    updateSliderInput(session, "par.a", value = input$par.aN)
  })
  observe({
    updateNumericInput(session, "par.bN", value = input$par.b)
  })
  observe({
    updateSliderInput(session, "par.b", value = input$par.bN)
  })
  observe({
    updateNumericInput(session, "par.cN", value = input$par.c)
  })
  observe({
    updateSliderInput(session, "par.c", value = input$par.cN)
  })
  observe({
    updateNumericInput(session, "par.dN", value = input$par.d)
  })
  observe({
    updateSliderInput(session, "par.d", value = input$par.dN)
  })
  observe({
    updateNumericInput(session, "par.eN", value = input$par.e)
  })
  observe({
    updateSliderInput(session, "par.e", value = input$par.eN)
  })
  observe({
    updateNumericInput(session, "par.fN", value = input$par.f)
  })
  observe({
    updateSliderInput(session, "par.f", value = input$par.fN)
  })
  # get the lengths (or ages) based on the range the user inputs.
  # note using 0.1 bins.
  len <- reactive({
    seq(as.numeric(input$range[1]), as.numeric(input$range[2]), 0.1)
  })

  # Calculate the selectivity based on user input
  selex <- reactive({
    switch(input$type,
      "Logistic (1)" = logistic1.fn(len(), input$par1, input$par2),
      "Double Normal (24 length, 20 age)" = doubleNorm24.fn(
        len(), input$par.a, input$par.b,
        input$par.c, input$par.d,
        input$par.e, input$par.f, use_e_999 = input$use_999_init,
        use_f_999 = input$use_999_fin
      )
    )
  })

  # Create the plot title
  output$caption <- renderText({
    input$type
  })
  # creat the plot
  output$selPlot <- renderPlot({
    plot(len(), selex(), type = "l", lwd = 3, xlab = "Length or Age", ylab = "Selectivity (S)", ylim = c(0, 1))
  })
}
