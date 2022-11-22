# dependencies -----
library(shiny)
library(bslib)
library(shinydashboard)
library(shinyjs)

# header ----
header <-
  dashboardHeader(
    title = "SS3 Helper",
    # add help link
    dropdownMenu(
      type = "notifications",
      headerText = strong("HELP"),
      icon = icon("question-circle"),
      badgeStatus = NULL,
      notificationItem(
        text = "Ask a question",
        icon = icon("question"),
        href = "https://vlab.noaa.gov/web/stock-synthesis/public-forums"
      ),
      notificationItem(
        text = "Report an issue",
        icon = icon("bug"),
        href = "https://github.com/r4ss/shiny_selex/issues"
      )
    ),
    # add a link to the source code
    tags$li(
      a(
        tags$img(src = "https://raw.githubusercontent.com/nmfs-fish-tools/nmfs-fish-tools.github.io/master/static/icons8-code-fork-24.png", height = "30"),
        href = "https://github.com/r4ss/shiny_selex",
        title = "See the source code",
      ),
      class = "dropdown"
    ),
    # add noaa branding
    tags$li(
      a(
        tags$img(src = "NOAA-Logo-Without-Ring.png", height = "30"),
        href = "https://www.noaa.gov",
        title = "Learn more about NOAA",
      ),
      class = "dropdown"
    )
  )

# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Selectivity", tabName = "Selectivity", icon = icon("ship")),
    # menuItem("Time-Varying Parameters", tabName = "tv", icon = icon("clock")),
    menuItem("Info", tabName = "info", icon = icon("info"))
  )
)

# body ----
body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(
      tabName = "Selectivity",
      sidebarLayout(
        # Sidebar controls to select selectivity type and parameters
        sidebarPanel(
          selectInput("type", "Selectivity Pattern:",
            choices = c(
              "Logistic (1)",
              "Double Normal (24 length, 20 age)"
            )
          ),
          sliderInput("range", "Length or Age Range:",
            min = 0, max = 100, value = c(0, 50)
          ),
          h2("Enter parameters below (slider or box)"),
          conditionalPanel(
            condition = "input.type == 'Logistic (1)'",
            fluidRow(
              column(
                8,
                sliderInput(
                  "par1", "Parameter 1 (p1), Size at inflection:",
                  0, 100, 10, 0.1
                ),
                sliderInput(
                  "par2", "Parameter 2 (p2), Width for 95% selection:",
                  -100, 100, 1, 0.1
                )
              ),
              column(
                4,
                numericInput("par1N", "Parameter 1:", 10),
                numericInput("par2N", "Parameter 2:", 1)
              )
            ) # ,
            # not sure how to get eqns to work, with shiny dashboard, but would
            # be nice to add
            # fluidRow(h2("Equation:")),
            # fluidRow(helpText('$$S = \\frac{1}{1+e^{\\frac{-ln(19)(L - p1)}{p2}}}$$')
            # )
          ),
          conditionalPanel(
            condition = "input.type == 'Double Normal (24 length, 20 age)'",
            fluidRow(
              column(
                8,
                sliderInput("par.a", "Length or Age for Peak:", 0, 100, 25, 0.1),
                sliderInput("par.b", "Width of Top:", -5, 5, 0, 0.1),
                sliderInput("par.c", "Ascending width:", -5, 10, 3, 0.1),
                sliderInput("par.d", "Descending width:", -5, 10, 3, 0.1),
                sliderInput("par.e", "Initial Selectivity:", 0, 1, 0.1, 0.05),
                sliderInput("par.f", "Final Selectivity:", 0, 1, 0.9, 0.05)
              ),
              column(
                4,
                numericInput("par.aN", "Peak", 25),
                numericInput("par.bN", "Top", 0),
                numericInput("par.cN", "Ascending width", 3),
                numericInput("par.dN", "Descending width", 3),
                numericInput("par.eN", "Initial", 0.1),
                numericInput("par.fN", "Final", 0.9)
              )
            ),
            fluidRow(
              column(
                6,
                checkboxInput("use_999_init", "Use -999 for Initial")
              ),
              column(
                6,
                checkboxInput("use_999_fin", "Use -999 for Final")
              )
            )
          )
        ),
        mainPanel(
          h3(textOutput("caption")),
          plotOutput("selPlot"),
          conditionalPanel(
            condition = "input.type == 'Double Normal (24 length, 20 age)'",
            fluidRow(
              h2("Additional options for double normal selectivity"),
              tags$ul(
                tags$li("For initial, an additional option not illustrated is to use a value <-1000, which ignores the initial selectivity algorithm and sets selectivity equal to 1.0e-06 for size bins 1 through bin = -1001 – value. So a value of –1003 would set selectivity to a nil level for bins 1 through 2 and begin using the modeled selectivity in bin 3."),
                tags$li("For final, an additional option not illustrated is to use a value <-1000. This sets selectivity as constant for bins greater than bin number = -1000 – value. ")
              )
            )
          )
        )
      )
    ),
    # tabItem(
    #   tabName = "tv",
    #   p("To be developed")
    # ),
    tabItem(
      tabName = "info",
      h2("Authors"),
      p("Andrea Havron, Allan Hicks, Ian Taylor, and Kathryn Doering"),
      h2("Links to learn more about NOAA"),
      tags$ul(
        tags$li(a(href = "https://www.commerce.gov/", "U.S. Department of Commerce")),
        tags$li(a(href = "https://www.noaa.gov", "National Oceanographic and Atmospheric Administration (NOAA)")),
        tags$li(a(href = "https://www.fisheries.noaa.gov/", "NOAA Fisheries"))
      )
    )
  )
)

# define ui ----
ui <- dashboardPage(
  # withMathJax(), # math jax doesn't seem to work with shinydashboard?
  skin = "blue", # the theme
  header,
  sidebar,
  body
)
