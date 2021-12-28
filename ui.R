library(shiny)
library(bslib)
library(shinydashboard)

ui <- dashboardPage(
  #withMathJax(),
  # theme = bslib::bs_theme(bootswatch = "cerulean"),
  skin = "blue",
  # Application title ----
  dashboardHeader(title = "SS3 Helper",
    # add NOAA icon and link
    tags$li(
      a(
        tags$img(src="NOAA-Logo-Without-Ring.png", height="30"),
        href = "https://www.noaa.gov",
        title = "Learn more about NOAA",
      ),
      class = "dropdown"
    ),
    # add a link to the source code
    tags$li(
      a(
        tags$img(src="https://raw.githubusercontent.com/nmfs-fish-tools/nmfs-fish-tools.github.io/master/static/icons8-code-fork-24.png", height="30"),
        href = "https://github.com/r4ss/shiny_selex",
        title = "See the source code",
      ),
      class = "dropdown"
    )
  ),
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Selectivity", tabName = "Selectivity", icon = icon("ship")),
      menuItem("Info", tabName = "info", icon = icon("info"))
    )
  ),
  # body ----
  dashboardBody(
    tabItems(
      tabItem(tabName = "Selectivity",
        sidebarLayout(
        # Sidebar controls to select selectivity type and parameters
          sidebarPanel(
            selectInput("type", "Selectivity Pattern:",
              choices = c(
                "Logistic (1)",
                "Double Normal (24)"
              )
            ),
            sliderInput("range", "Length Range:",
              min = 0, max = 100, value = c(0, 50)
            ),
            h2("Enter parameters below (slider or box)"),
            conditionalPanel(
              condition = "input.type == 'Logistic (1)'",
              fluidRow(
                column(
                  8,
                  sliderInput("par1", "Parameter 1 (p1), Size at inflection:",
                              0, 100, 10, 0.1),
                  sliderInput("par2", "Parameter 2 (p2), Width for 95% selection:",
                              0, 100, 1, 0.1)
                ),
                column(
                  4,
                  numericInput("par1N", "Parameter 1:", 10),
                  numericInput("par2N", "Parameter 2:", 1)
                )
              )#,
              # fluidRow(h2("Equation:")),
              # fluidRow(helpText('$$S = \\frac{1}{1+e^{\\frac{-ln(19)(L - p1)}{p2}}}$$')
              # )
            ),
            conditionalPanel(
              condition = "input.type == 'Double Normal (24)'",
              fluidRow(
                column(
                  8,
                  sliderInput("par.a", "PEAK:", 0, 100, 25, 0.1),
                  sliderInput("par.b", "TOP:", -5, 5, 0, 0.1),
                  sliderInput("par.c", "ASC-WIDTH:", -5, 10, 3, 0.1),
                  sliderInput("par.d", "DESC-WIDTH:", -5, 10, 3, 0.1),
                  sliderInput("par.e", "INIT:", 0, 1, 0.1, 0.05),
                  sliderInput("par.f", "FINAL:", 0, 1, 0.9, 0.05)
                ),
                column(
                  4,
                  numericInput("par.aN", "PEAK", 25),
                  numericInput("par.bN", "TOP", 0),
                  numericInput("par.cN", "ASC-WIDTH", 3),
                  numericInput("par.dN", "DESC-WIDTH", 3),
                  numericInput("par.eN", "INIT", 0.1),
                  numericInput("par.fN", "FINAL", 0.9)
                )
              )
            )
          ),
          mainPanel(
            h3(textOutput("caption")),
            plotOutput("selPlot")
          )
        )
      ),
      tabItem(
        tabName = "info",
        h2("Authors"),
        p("Andrea Havron, Allan Hicks, Ian Taylor, and Kathryn Doering"),
        h2("Links to Learn more about NOAA"),
        tags$ul(
          tags$li(a(href = "https://www.commerce.gov/","U.S. Department of Commerce")),
          tags$li(a(href = "https://www.noaa.gov","National Oceanographic and Atmospheric Administration (NOAA)")),
          tags$li(a(href = "https://www.fisheries.noaa.gov/","NOAA Fisheries"))
        )
      )
    )
  )
)
