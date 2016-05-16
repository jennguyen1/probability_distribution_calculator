# Server Code for Probability Distribution Calculator
# Date: May 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# load libraries
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# server function
function(input, output){
  
  ########################
  # Process User Options #
  ########################
  
  # convert user distributions from English to R
  dist <- reactive({
    r_dist <- c("norm", "t", "gamma", "chisq", "f", "unif", "binom", "nbinom", "pois", "beta")
    eng_dist <- c("Normal", "T", "Gamma", "Chi-Square", "F", "Uniform", "Binomial", "Negative Binomial", "Poisson", "Beta")
    plyr::mapvalues(input$dist, eng_dist, r_dist)
  })
  
  # flag for discrete distributions
  is_discrete <- reactive({ dist() %in% c("binom", "nbinom", "pois") })
  
  # flag for symmetric distributions about 0
  is_symmetric <- reactive({ dist() %in% c("norm", "t", "unif") })
  
  # provide additional options depending on one or two sided values
  output$side_option <- renderUI({
    switch(input$side,
           "one-sided" = radioButtons("side2", "one-sided options", c("lower tail", "upper tail")),
           "two-sided" = radioButtons("side2", "two-sided options", c("between", "both tails"))
    )
  })
  
  # provide description of the model being generated
  output$prob_descr <- renderText({
    switch(input$side,
           "one-sided" = ifelse(input$side2 == "lower tail", "P(X <= x)", "P(X > x)"),
           "two-sided" = ifelse(input$side2 == "between", "P(x1 < X < x2)", "P(X < x1 or X > x2)")
    )
  })
  
  # set distribution parameters
  get_params <- reactive({
    
    # convert distribution into function
    pfun <- match.fun(paste0("p", dist()))
    dfun <- match.fun(paste0("d", dist()))
    qfun <- match.fun(paste0("q", dist()))
    
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
             formals(qfun)$size <- n
             formals(qfun)$prob <- p
             model_descr <- paste0("X ~ Bin(", n, ", ", p, ")")
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
             formals(qfun)$size <- n
             formals(qfun)$prob <- p
             model_descr <- paste0("X ~ NBin(", n, ", ", p, ")")
           },
           "Poisson" = {
             lambda <- input$pois.lambda
             min <- min(input$x, qpois(1e-4, lambda))
             max <- max(input$x, qpois(1e-4, lambda, lower.tail = FALSE))
             formals(pfun)$lambda <- lambda
             formals(dfun)$lambda <- lambda
             formals(qfun)$lambda <- lambda
             model_descr <- paste0("X ~ Pois(", lambda, ")")
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
             formals(qfun)$mean <- mean
             formals(qfun)$sd <- sd
             model_descr <- paste0("X ~ N(", mean, ", ", sd^2, ")")
           },
           "T" = {
             df <- input$t.df
             min <- min(input$x, -4)
             max <- max(input$x, 4)
             formals(pfun)$df <- df
             formals(dfun)$df <- df
             formals(qfun)$df <- df
             model_descr <- paste0("X ~ T(", df, ")")
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
             formals(qfun)$shape <- shape
             formals(qfun)$rate <- rate
             model_descr <- paste0("X ~ Gamma(", shape, ", ", rate, ")")
           },
           "Chi-Square" = {
             df <- input$chisq.df
             min <- 0
             max <- max(input$x, qchisq(1e-4, df, lower.tail = FALSE))
             formals(pfun)$df <- df
             formals(dfun)$df <- df
             formals(qfun)$df <- df
             model_descr <- paste0("X ~ X^2(", df, ")")
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
             formals(qfun)$df1 <- df.num
             formals(qfun)$df2 <- df.denom
             model_descr <- paste0("X ~ F(", df1, ", ", df2, ")")
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
             formals(qfun)$min <- lower
             formals(qfun)$max <- upper
             model_descr <- paste0("X ~ Unif(", lower, ", ", upper, ")")
           },
           "Beta" = {
             shape1 <- input$beta.shape1
             shape2 <- input$beta.shape2
             min <- min(input$x, 0) - 0.02
             max <- max(input$x, 1) + 0.02
             formals(pfun)$shape1 <- shape1
             formals(pfun)$shape2 <- shape2
             formals(dfun)$shape1 <- shape1
             formals(dfun)$shape2 <- shape2
             formals(qfun)$shape1 <- shape1
             formals(qfun)$shape2 <- shape2
             model_descr <- paste0("X ~ Beta(", shape1, ", ", shape2, ")")
           }
    )
    
    # return list of outputs
    # model_descr: model notation
    # min/max: of x ranges for x-axis
    # pfun: returns x given probabilities
    # dfun: returns density given x
    # qfun: returns probability given x
    list(model_descr = model_descr, min = min, max = max, pfun = pfun, dfun = dfun, qfun = qfun)
    
  })
  
  # provide description of the model being generated
  output$model_descr <- renderText(get_params()$model_descr)
  
  # provide additional options for one or two sided values in terms of x or probability
  output$x_option1 <- renderUI({
    if(input$type == "x"){
      switch(input$side,
             "one-sided" = numericInput("x", "x", 1),
             "two-sided" = numericInput("x1", "x1", 1)
      )
    } else{
      numericInput("p", "p", 0.25)
    }
  })
  output$x_option2 <- renderUI({
    if(input$type == "x"){
      if( input$side == "two-sided" ) numericInput("x2", "x2", 2)
    }
  })
  
  # initialize x's depending on whether the input is x or probability
  in_x <- reactive({ 
    ifelse(input$type == "x", input$x, signif(get_params()$qfun(input$p, lower.tail = input$side2 == "lower tail"), 3)) 
  })
  in_x1 <- reactive({ 
    use_p <- ifelse(input$side2 == "both tails", input$p/2, .5 - input$p/2)
    ifelse(input$type == "x", input$x1, signif(get_params()$qfun(use_p), 3)) 
  })
  in_x2 <- reactive({ 
    use_p <- ifelse(input$side2 == "both tails", input$p/2, .5 - input$p/2)
    ifelse(input$type == "x", input$x2, signif(get_params()$qfun(use_p, lower.tail = FALSE), 3)) 
  })
  
  #######################
  # Compute Probability #
  #######################
  
  # probability 
  output$prob <- renderText({
    
    # initalize the output text with distribution
    prefix <- paste0(input$dist, ": ")
    
    # compute probabilities 
    outText <- switch(input$side,
                      # compute probabilities for one-sided computations
                      "one-sided" = {
                        switch(input$side2,
                               "lower tail" = paste0(prefix, "P(X <= ", in_x(), ") = ", signif(get_params()$pfun(in_x()), 3)),
                               "upper tail" = paste0(prefix, "P(X > ", in_x(), ") = ", signif(get_params()$pfun(in_x(), lower.tail = FALSE), 3))
                        )
                      },
                      # compute probabilities for two-sided computations
                      "two-sided" = {
                        switch(input$side2,
                               "between" = paste0(prefix, "P(", in_x1(), " < X < ", in_x2(), ") = ", signif(get_params()$pfun(in_x2()) - get_params()$pfun(in_x1()) , 3)),
                               "both tails" = paste0(prefix, "P(X < ", in_x1(), " or X > ", in_x2(), ") = ", signif(get_params()$pfun(in_x2(), lower.tail = FALSE) + get_params()$pfun(in_x1()) , 3))
                        )
                      }
    )
    
    # output probability
    
    # if p is specified, print probability as long as it is not a two-tailed of a non-symmetric distribution
    if(input$type == "probability"){
      if( (input$side == "one-sided") | (input$side == "two-sided" & is_symmetric()) ){
        outText
      } else{
        paste0(prefix, "distribution is not symmetric")
      }
      # if x's are specified, print probability
    } else{
      outText
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
      data$shade <- switch(input$side,
                           "one-sided" = switch(input$side2,
                                                "lower tail" = ifelse(data$x <= in_x(), "shade", "no-shade"),
                                                "upper tail" = ifelse(data$x <= in_x(), "no-shade", "shade")
                           ),
                           "two-sided" = switch(input$side2,
                                                "between" = ifelse(dplyr::between(data$x, in_x1(), in_x2()), "shade", "no-shade"),
                                                "both tails" = ifelse(dplyr::between(data$x, in_x1(), in_x2()), "no-shade", "shade")
                           )
      )
      
      # generate plot: if portion of plot is shaded
      density <- ggplot(data = data, aes(x = x, xend = x, y = 0, yend = y, color = shade)) +
        geom_segment(size = 2) +
        scale_color_manual(values = c("black", "royalblue")) +
        theme(legend.position = "none") 
      
      # generate plot: if all of plot is shaded (above can't distinguish b/n shades)
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
        switch(input$side,
               "one-sided" = switch(input$side2,
                                    "lower tail" = y[x > in_x()] <- NA,
                                    "upper tail" = y[x < in_x()] <- NA
               ),
               "two-sided" = switch(input$side2,
                                    "between" = y[!dplyr::between(x, in_x1(), in_x2())] <- NA,
                                    "both tails" = y[dplyr::between(x, in_x1(), in_x2())] <- NA
               )
        )
        
        # return results
        return(y)
      }
      
      # generate plot using shading function
      density <- ggplot(data = NULL, aes(x = c(params$min, params$max))) +
        stat_function(fun = params$dfun, size = 1.25) +
        stat_function(fun = shade_fun, geom = "area", fill = "royalblue", color = "royalblue")
    }
    
    # add axis labels
    density <- density + xlab("X") + ylab("Density") + ggtitle(paste(input$dist, "distribution"))
    
    # output plot
    
    # if p is specified, print plot as long as it is not a two-tailed of a non-symmetric distribution
    if(input$type == "probability"){
      if( (input$side == "one-sided") | (input$side == "two-sided" & is_symmetric()) ){
        density
      }
      # if x's are specified, print plot
    } else{
      density
    }
    
  })
  
  # cumulative distribution function - only if side option is one-sided lower tail
  output$cdf <- renderPlot({
    if( input$side == "one-sided" & input$side2 == "lower tail"){
      
      # obtain parameters
      params <- get_params()
      
      # generate x axis
      x <- seq(params$min, params$max, length.out = 1000)
      
      # generate probabilities for discrete 
      if( is_discrete() ){
        # fix for values that aren't 0 at x = 0
        data <- data.frame(x = c(x[1] - 0.02, x), y = c(params$pfun(x[1] - 0.02), params$pfun(floor(x))))
        
        # generate probabilites for continuous
      } else{
        data <- data.frame(x = x, y = params$pfun(x))
      }
      
      # generate plot: cdf
      ggplot(data = data, aes(x = x, y = y)) +
        geom_line(size = 1.25) +
        # generate segment and point for x
        geom_segment(aes(x = in_x(), xend = in_x(), y = 0, yend = params$pfun(in_x())), size = 0.85, color = "royalblue") +
        geom_point(aes(x = in_x(), y = params$pfun(in_x())), size = 2, color = "royalblue") +
        # axis labels
        xlab("X") + ylab("Cumulative Probability")
      
    }
  })
  
} # end of server function

