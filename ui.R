

fluidPage(
  # App Title
  h1("Probability Distribution Calculator"),
  
  # Split Layout
  sidebarLayout(
    # Panel to select x and distributions
    sidebarPanel(
      
      ##################
      # Select Options #
      ##################
      h3("Select Options"),
      
      # Which side of distribution to select
      radioButtons("side", "side", c("one-sided", "two-sided")),
      uiOutput("side_option"),
      
      # Select X
      uiOutput("x_option1"),
      uiOutput("x_option2"),

      ######################################
      # Select Distribution and Parameters #
      ######################################
      h3("Select Distribution"),
      tabsetPanel(
        
        # CONTINUOUS DISTRIBUTIONS #

        # Normal distribution
        tabPanel(
          "Normal",
          numericInput("norm.mean", "mean", 0),
          numericInput("norm.sd", "standard deviation", 1)
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
          numericInput("f.df1", "df numerator", 1),
          numericInput("f.df2", "df denominator", 1)
        ),
        # Gamma distribution
        tabPanel(
          "Gamma",
          numericInput("gamma.shape", "shape", 1),
          numericInput("gamma.rate", "rate", 1)
        ),
        # Uniform distribution
        tabPanel(
          "Uniform",
          numericInput("unif.min", "lower", 0),
          numericInput("unif.max", "upper", 1)
        ),
        
        # DISCRETE DISTRIBUTIONS #

        # Binomial distribution
        tabPanel(
          "Binomial",
          numericInput("binom.n", "number of trials", 10),
          numericInput("binom.p", "probability of success", 0.5)
        ),
        # Poisson Distribution
        tabPanel(
          "Poisson",
          numericInput("pois.lambda", "mean", 1)
        ),
        # Negative Binomial Distribution
        tabPanel(
          "Negative Binomial",
          numericInput("nbinom.n", "number of trials", 10),
          numericInput("nbinom.p", "probability of success", 0.5)
        ),
        
        # panel descriptions
        id = "dist", 
        position = "left"
      )
    ),
    
    # Print Plot
    mainPanel(
      h4(textOutput("prob")),
      plotOutput("density", height = "200px"),
      plotOutput("cdf", height = "200px")
    )
  )
)
