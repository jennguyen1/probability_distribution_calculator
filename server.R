

library(stringr)
library(plyr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

function(input, output){
  
  ########################
  # Process User Options #
  ########################
  
  # convert user distributions from English to R
  dist <- reactive({
    r_dist <- c("norm", "t", "gamma", "chisq", "f", "unif", "binom", "nbinom", "pois")
    eng_dist <- c("Normal", "T", "Gamma", "Chi-Square", "F", "Uniform", "Binomial", "Negative Binomial", "Poisson")
    plyr::mapvalues(input$dist, eng_dist, r_dist)
  })
  
  # flag for discrete distributions
  is_discrete <- reactive({ ifelse(dist() %in% c("norm", "t", "gamma", "chisq", "f", "unif"), FALSE, TRUE) })
  
  # set distribution parameters
  get_params <- reactive({
    
    # convert into function
    pfun <- match.fun(paste0("p", dist()))
    dfun <- match.fun(paste0("d", dist()))

    # set the parameters for the distributions
    switch(input$dist,
           "Binomial" = {
             n <- input$binom.n
             p <- input$binom.p
             min <- min(input$x, qbinom(1e-4, n, p))
             max <- max(input$x, qbinom(1e-4, n, p, lower.tail = FALSE))
             formals(pfun)$size <- n
             formals(pfun)$prob <- p
             formals(dfun)$size <- n
             formals(dfun)$prob <- p
           },
           "Negative Binomial" = {
             n <- input$nbinom.n
             p <- input$nbinom.p
             min <- min(input$x, qnbinom(1e-4, n, p))
             max <- max(input$x, qnbinom(1e-4, n, p, lower.tail = FALSE))
             formals(pfun)$size <- n
             formals(pfun)$prob <- p
             formals(dfun)$size <- n
             formals(dfun)$prob <- p
           },
           "Poisson" = {
             lambda <- input$pois.lambda
             min <- min(input$x, qpois(1e-4, lambda))
             max <- max(input$x, qpois(1e-4, lambda, lower.tail = FALSE))
             formals(pfun)$lambda <- lambda
             formals(dfun)$lambda <- lambda
           },
           "Normal" = {
             mean <- input$norm.mean
             sd <- input$norm.sd
             min <- min(input$x, qnorm(1e-4, mean, sd))
             max <- max(input$x, qnorm(1e-4, mean, sd, lower.tail = FALSE))
             formals(pfun)$mean <- mean
             formals(pfun)$sd <- sd
             formals(dfun)$mean <- mean
             formals(dfun)$sd <- sd
           },
           "T" = {
             df <- input$t.df
             min <- min(input$x, -4)
             max <- max(input$x, 4)
             formals(pfun)$df <- df
             formals(dfun)$df <- df
           },
           "Gamma" = {
             shape <- input$gamma.shape
             rate <- input$gamma.rate
             min <- 0
             max <- max(input$x, qgamma(1e-4, shape, rate = rate, lower.tail = FALSE))
             formals(pfun)$shape <- shape
             formals(pfun)$rate <- rate
             formals(dfun)$shape <- shape
             formals(dfun)$rate <- rate
           },
           "Chi-Square" = {
             df <- input$chisq.df
             min <- 0
             max <- max(input$x, qchisq(1e-4, df, lower.tail = FALSE))
             formals(pfun)$df <- df
             formals(dfun)$df <- df
           },
           "F" = {
             df.num <- input$f.df1
             df.denom <- input$f.df2
             min <- 0
             max <- max(input$x, qf(0.05, df.num, df.denom, lower.tail = FALSE))
             formals(pfun)$df1 <- df.num
             formals(pfun)$df2 <- df.denom
             formals(dfun)$df1 <- df.num
             formals(dfun)$df2 <- df.denom
           },
           "Uniform" = {
             lower <- input$unif.min
             upper <- input$unif.max
             min <- min(input$x, lower) - 0.02
             max <- max(input$x, upper) + 0.02
             formals(pfun)$min <- lower
             formals(pfun)$max <- upper
             formals(dfun)$min <- lower
             formals(dfun)$max <- upper
           }
    )
    
    # return list of outputs
    list(x = input$x, min = min, max = max, pfun = pfun, dfun = dfun)
  })
  
  # provide additional options for one or two sided values
  output$side_option <- renderUI({
    switch(input$side,
           "one-sided" = radioButtons("side2", "one-sided options", c("less than", "greater than")),
           "two-sided" = radioButtons("side2", "two-sided options", c("between", "outside"))
    )
  })
    
  # provide additional options for one or two sided values in terms of x
  output$x_option1 <- renderUI({
    switch(input$side,
           "one-sided" = numericInput("x", "x", 1),
           "two-sided" = numericInput("x1", "x1", 1)
    )
  })
  output$x_option2 <- renderUI({
    if( input$side == "two-sided" ) numericInput("x2", "x2", 1)
  })

  #######################
  # Compute Probability #
  #######################
  
  # probability depending on the side option
  output$prob <- renderText({

    # initalize the output text with distribution
    outText <- paste0(input$dist, ": ")
    
    # compute probabilities for one-sided computations
    if(input$side == "one-sided"){
      
      if(input$side2 == "less than"){
        paste0(outText, "P(X < ", input$x, ") = ", signif(get_params()$pfun(input$x), 3))
      } else{
        paste0(outText, "P(X > ", input$x, ") = ", signif(get_params()$pfun(input$x, lower.tail = FALSE), 3))
      }
      
    # compute probabilities for two-sided computations
    } else{
      
      if(input$side2 == "between"){
        paste0(outText, "P(", input$x1, " < X < ", input$x2, ") = ", signif(get_params()$pfun(input$x2) - get_params()$pfun(input$x1) , 3))
      } else{
        paste0(outText, "P(X < ", input$x1, ", X > ", input$x2, ") = ", signif(get_params()$pfun(input$x2, lower.tail = FALSE) + get_params()$pfun(input$x1) , 3))
      }
    }
    
  })
  
  ###################
  # Generate Graphs #
  ###################
  
  # probability density function
  output$density <- renderPlot({
    # obtain parameters
    params <- get_params()
    
    # discrete distributions
    if( is_discrete() ){
      # generate data and shading
      x <- params$min:params$max
      data <- data.frame(x = x, y = params$dfun(x))
      data$shade <- switch(input$side2,
                           "less than" = ifelse(data$x <= params$x, "shade", "no-shade"),
                           "greater than" = ifelse(data$x <= params$x, "no-shade", "shade")
      )
      
      # generate plot: portion of plot is shaded
      density <- ggplot(data = data, aes(x = x, xend = x, y = 0, yend = y, color = shade)) +
        geom_segment(size = 2) +
        # shade in area less than x
        scale_color_manual(values = c("black", "royalblue")) +
        theme(legend.position = "none") 
      
      # generate plot: all of plot is shaded (above can't distinguish b/n shades)
      if( all(data$shade == "shade") ){
        density <- ggplot(data = data, aes(x = x, xend = x, y = 0, yend = y)) +
          geom_segment(size = 2, color = "royalblue") 
      }
      
    # continuous distributions
    } else{
      
      # function for shading in area under the curve wrt probability
      shade_fun <- function(x){
        # generate the y values of graph
        y <- params$dfun(x)
        
        # only keep the y values that correspond to the probability inequality
        switch(input$side2,
               "less than" = y[x > input$x] <- NA,
               "greater than" = y[x < input$x] <- NA
                )
        
        # return results
        return(y)
      }

      # generate plot using function
      density <- ggplot(data = NULL, aes(x = c(params$min, params$max))) +
        stat_function(fun = params$dfun, size = 1.25) +
        stat_function(fun = shade_fun, geom = "area", fill = "royalblue", color = "royalblue")
    }
    
    # add axis labels
    density <- density + xlab("X") + ylab("Density")
    
    # print plot
    density
  })
  

}




