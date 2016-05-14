

fluidPage(
  # App Title
  h1("Probability Distribution Calculator"),
  
  # Split Layout
  sidebarLayout(
    # Panel to select x and distributions
    sidebarPanel(
      
      # Select distribution & parameters
      tabsetPanel(
        tabPanel(
          "Normal",
          numericInput("norm.mean", "mean", 0),
          numericInput("norm.sd", "standard deviation", 1)
        ),
        tabPanel(
          "T",
          numericInput("t.df", "df", 3)
        ),
        tabPanel(
          "Chi-Square",
          numericInput("chisq.df", "df", 1)
        ),
        tabPanel(
          "F",
          numericInput("f.df1", "df numerator", 1),
          numericInput("f.df2", "df denominator", 1)
        ),
        tabPanel(
          "Gamma",
          numericInput("gamma.shape", "shape", 1),
          numericInput("gamma.rate", "rate", 1)
        ),
        tabPanel(
          "Uniform",
          numericInput("unif.min", "lower", 0),
          numericInput("unif.max", "upper", 1)
        ),
        tabPanel(
          "Binomial",
          numericInput("binom.n", "number of trials", 10),
          numericInput("binom.p", "probability of success", 0.5)
        ),
        tabPanel(
          "Negative Binomial",
          numericInput("nbinom.n", "number of trials", 10),
          numericInput("nbinom.p", "probability of success", 0.5)
        ),
        tabPanel(
          "Poisson",
          numericInput("pois.lambda", "mean", 1)
        ),
        id = "dist", 
        position = "left"
      )
    ),
    
    # Print Plot
    mainPanel(

    )
  )
)
