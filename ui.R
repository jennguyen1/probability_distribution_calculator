# UI Code for Probability Distribution Calculator
# Date: May 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

library(shinydashboard)

# ui function
dashboardPage(

  # App Title
  dashboardHeader(title = "Probability Distribution Calculator"),

  # Disable Sidebar
  dashboardSidebar(disable = TRUE),

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
              numericInput("norm.sd", "standard deviation", 1)
            )
          ),
          # T distribution
          tabPanel(
            "T",
            numericInput("t.df", "df", 3)
          ),
          # X2 distribution
          tabPanel(
            "Chi-Square",
            numericInput("chisq.df", "df", 1)
          ),
          # F distribution
          tabPanel(
            "F",
            flowLayout(
              numericInput("f.df1", "df numerator", 1),
              numericInput("f.df2", "df denominator", 1)
            )
          ),
          # Gamma distribution
          tabPanel(
            "Gamma",
            flowLayout(
              numericInput("gamma.shape", "shape", 1),
              numericInput("gamma.rate", "rate", 1)
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
              numericInput("beta.shape1", "shape 1", 1),
              numericInput("beta.shape2", "shape 2", 1)
            )
          ),

          # DISCRETE DISTRIBUTIONS #

          # Binomial distribution
          tabPanel(
            "Binomial",
            flowLayout(
              numericInput("binom.n", "number of trials", 10),
              numericInput("binom.p", "probability of success", 0.5)
            )
          ),
          # Poisson Distribution
          tabPanel(
            "Poisson",
            numericInput("pois.lambda", "mean", 1)
          ),
          # Negative Binomial Distribution
          tabPanel(
            "Negative Binomial",
            flowLayout(
              numericInput("nbinom.n", "number of trials", 10),
              numericInput("nbinom.p", "probability of success", 0.5)
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

      # density function plot with shaded regions
      plotOutput("density", height = "300px"),

      # cumulative density function plot if P(X < x)
      plotOutput("cdf", height = "300px")
    )
  )))
) # end of ui function
