# UI Code for Probability Distribution Calculator
# Date: May 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

library(shinydashboard)

# ui function
function(request){
dashboardPage(

  # App Title
  dashboardHeader(title = "Probability Distribution Calculator"),

  # Disable Sidebar
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Github Source Code", href = "https://github.com/jennguyen1/probability_distribution_calculator", icon = icon("github"))
    )
  ),

  # Vertical layout for inputs
  dashboardBody( fluidRow(
    ######################################
    # Select Distribution and Parameters #
    ######################################
    box(

      # set box title and image of the box
      title = "Select Distribution",
      status = "primary", solidHeader = TRUE, collapsible = TRUE,
      
      # box contents
      wellPanel(

        # print X distribution in common notation
        h3(textOutput("model_descr"), align = "center"),
        br(),
        
        tabsetPanel(

          # CONTINUOUS DISTRIBUTIONS #

          # Normal distribution
          tabPanel(
            "Normal",
            flowLayout(
              numericInput("norm.mean", "mean", 0),
              numericInput("norm.sd", "standard deviation", 1, min = 0)
            )
          ),
          # T distribution
          tabPanel(
            "T",
            numericInput("t.df", "df", 3, min = 0)
          ),
          # X2 distribution
          tabPanel(
            "Chi-Square",
            numericInput("chisq.df", "df", 1, min = 0)
          ),
          # F distribution
          tabPanel(
            "F",
            flowLayout(
              numericInput("f.df1", "df numerator", 1, min = 0),
              numericInput("f.df2", "df denominator", 1, min = 0)
            )
          ),
          # Gamma distribution
          tabPanel(
            "Gamma",
            flowLayout(
              numericInput("gamma.shape", "shape", 1, min = 0),
              numericInput("gamma.rate", "rate", 1, min = 0)
            )
          ),
          # Uniform distribution
          tabPanel(
            "Uniform",
            flowLayout(
              numericInput("unif.min", "lower", 0),
              numericInput("unif.max", "upper", 1)
            )
          ),
          # Beta distribution
          tabPanel(
            "Beta",
            flowLayout(
              numericInput("beta.shape1", "shape 1", 1, min = 0),
              numericInput("beta.shape2", "shape 2", 1, min = 0)
            )
          ),

          # DISCRETE DISTRIBUTIONS #

          # Binomial distribution
          tabPanel(
            "Binomial",
            flowLayout(
              numericInput("binom.n", "number of trials", 10, min = 0),
              numericInput("binom.p", "probability of success", 0.5, min = 0, max = 1, step = 0.1)
            )
          ),
          # Poisson Distribution
          tabPanel(
            "Poisson",
            numericInput("pois.lambda", "mean", 1, min = 0)
          ),
          # Negative Binomial Distribution
          tabPanel(
            "Negative Binomial",
            flowLayout(
              numericInput("nbinom.n", "number of trials", 10, min = 0),
              numericInput("nbinom.p", "probability of success", 0.5, min = 0, max = 1, step = 0.1)
            )
          ),

          # panel descriptions
          id = "dist"
        )
      )
    ),

    ##################
    # Select Options #
    ##################
    box(

      # set box title and image of the box
      title = "Select Options",
      solidHeader = TRUE, status = "primary", collapsible = TRUE,

      # box contents
      wellPanel( verticalLayout(

        # one-tailed or two-tailed of distribution
        flowLayout(
          verticalLayout(
            radioButtons("side", "side", c("one-sided", "two-sided")),
            uiOutput("side_option")
          ),
          h3(textOutput("prob_descr"), align = "center")
        ),

        br(),

        # input percentiles or quantiles
        flowLayout(

          # option to select x or p
          radioButtons("type", "input", c("x", "probability")),

          # select X
          verticalLayout(
          uiOutput("x_option1"),
          uiOutput("x_option2"),

          # select p
          uiOutput("p_option"))
        )
      ))
    )),

    ################
    # Plot Outputs #
    ################
    fluidRow(column(width = 12,
    box(width = NULL,

      title = "Plots",
      solidHeader = TRUE, status = "primary",

      # statement of distribution and model
      h3(textOutput("prob"), align = "center"),

      uiOutput("plots"), 
      bookmarkButton()
      
    )
  )),
  span(p("Copyright (c) 2018 Jennifer N Nguyen under the MIT License"), style = "font-size:12px; color:grey")
  )
)} # end of ui function
