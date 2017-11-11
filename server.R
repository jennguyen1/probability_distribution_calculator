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

signif_digits <- 4

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
  
  is_discrete <- reactive({ dist() %in% c("binom", "nbinom", "pois") })
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
             validate(
              need(input$binom.n, "Provide all requested inputs"),
              need(input$binom.p, "Provide all requested inputs"), 
              need(input$binom.n > 0, "N must be greater than 0"),
              need(dplyr::between(input$binom.p, 0, 1), "P must be between 0 and 1"),
              need(abs(input$binom.n - round(input$binom.n)) == 0, "N must be an integer")
             )
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
             validate(
               need(input$nbinom.n, "Provide all requested inputs"),
               need(input$nbinom.p, "Provide all requested inputs"),
               need(input$nbinom.n > 0, "N must be greater than 0"),
               need(dplyr::between(input$nbinom.p, 0, 1), "P must be between 0 and 1"),
               need(abs(input$nbinom.n - round(input$nbinom.n)) == 0, "N must be an integer")
             )
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
             validate(
               need(input$pois.lambda, "Provide all requested inputs"), 
               need(input$pois.lambda > 0, "Lambda must be greater than 0"), 
               need(abs(input$pois.lambda - round(input$pois.lambda)) == 0, "Lambda must be an integer")
             )
             lambda <- input$pois.lambda
             min <- min(input$x, qpois(1e-4, lambda))
             max <- max(input$x, qpois(1e-4, lambda, lower.tail = FALSE))
             formals(pfun)$lambda <- lambda
             formals(dfun)$lambda <- lambda
             formals(qfun)$lambda <- lambda
             model_descr <- paste0("X ~ Pois(", lambda, ")")
           },
           "Normal" = {
             validate(
               need(input$norm.mean, "Provide all requested inputs"),
               need(input$norm.sd, "Provide all requested inputs"), 
               need(input$norm.sd > 0, "Standard deviation must be greater than 0")
             )
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
             validate(
               need(input$t.df, "Provide all requested inputs"), 
               need(input$t.df > 0, "DF must be greater than 0")
             )
             df <- input$t.df
             min <- min(input$x, -4)
             max <- max(input$x, 4)
             formals(pfun)$df <- df
             formals(dfun)$df <- df
             formals(qfun)$df <- df
             model_descr <- paste0("X ~ T(", df, ")")
           },
           "Gamma" = {
             validate(
               need(input$gamma.shape, "Provide all requested inputs"),
               need(input$gamma.rate, "Provide all requested inputs"), 
               need(input$gamma.shape > 0 & input$gamma.rate > 0, "Shape and rate must be greater than 0")
             )
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
             validate(
               need(input$chisq.df, "Provide all requested inputs"), 
               need(input$chisq.df > 0, "DF must be greater than 0")
             )
             df <- input$chisq.df
             min <- 0
             max <- max(input$x, qchisq(1e-4, df, lower.tail = FALSE))
             formals(pfun)$df <- df
             formals(dfun)$df <- df
             formals(qfun)$df <- df
             model_descr <- paste0("X ~ X^2(", df, ")")
           },
           "F" = {
             validate(
               need(input$f.df1, "Provide all requested inputs"),
               need(input$f.df2, "Provide all requested inputs"), 
               need(input$f.df1 > 0 & input$f.df2 > 0, "DF must be greater than 0")
             )
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
             model_descr <- paste0("X ~ F(", df.num, ", ", df.denom, ")")
           },
           "Uniform" = {
             validate(
               need(input$unif.min, "Provide all requested inputs"),
               need(input$unif.max, "Provide all requested inputs"), 
               need(input$unif.min < input$unif.max, "Min must be less than max")
             )
             lower <- input$unif.min
             upper <- input$unif.max
             min <- min(input$x, lower)
             max <- max(input$x, upper)
             formals(pfun)$min <- lower
             formals(pfun)$max <- upper
             formals(dfun)$min <- lower
             formals(dfun)$max <- upper
             formals(qfun)$min <- lower
             formals(qfun)$max <- upper
             model_descr <- paste0("X ~ Unif(", lower, ", ", upper, ")")
           },
           "Beta" = {
             validate(
               need(input$beta.shape1, "Provide all requested inputs"),
               need(input$beta.shape2, "Provide all requested inputs"), 
               need(input$beta.shape1 > 0 & input$beta.shape2 > 0, "Shape must be greater than 0")
             )
             shape1 <- input$beta.shape1
             shape2 <- input$beta.shape2
             min <- min(input$x, 0) 
             max <- max(input$x, 1) 
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

  output$model_descr <- renderText(get_params()$model_descr)

  # provide additional options for one or two sided values in terms of x or probability
  output$x_option1 <- renderUI({
    if(input$type == "x"){
      min_val <- ifelse(is_discrete(), 0, NA)
      step_val <- ifelse(is_discrete(), 1, 0.1)
      switch(input$side,
             "one-sided" = numericInput("x", "x", 1, step = step_val, min = min_val),
             "two-sided" = numericInput("x1", "x1", 1, step = step_val, min = min_val)
      )
    } else{
      numericInput("p", "p", 0.25, min = 0, max = 1, step = 0.1)
    }
  })
  output$x_option2 <- renderUI({
    if(input$type == "x"){
      if( input$side == "two-sided" ) {
        min_val <- ifelse(is_discrete(), 0, NA)
        step_val <- ifelse(is_discrete(), 1, 0.1)
        numericInput("x2", "x2", 1, step = step_val, min = min_val)
      }
    }
  })

  # initialize x depending on input: if x return x else if probability  return qfun(p)
  in_x <- reactive({
    
    if(input$type == "x"){
      validate(
        need(input$x, "Provide all requested inputs")
      )
      input$x
    } else{
      validate(
        need(input$p, "Provide all requested inputs")
      )
      signif(get_params()$qfun(input$p, lower.tail = input$side2 == "lower tail"), signif_digits)
    }
  })
  in_x1 <- reactive({
    validate(
      need(input$x1, "Provide all requested inputs")
    )
    if(input$type == "x"){
      input$x1
    } else{
      use_p <- ifelse(input$side2 == "both tails", input$p/2, .5 - input$p/2)
      signif(get_params()$qfun(use_p), signif_digits)
    }
  })
  in_x2 <- reactive({
    if(input$type == "x"){
      validate(
        need(input$x2, "Provide all requested inputs"),
        need(input$x2 >= input$x1, "X2 must be greater than X1")
      )
      input$x2
    } else{
      use_p <- ifelse(input$side2 == "both tails", input$p/2, .5 - input$p/2)
      signif(get_params()$qfun(use_p, lower.tail = FALSE), signif_digits)
    }
  })

  # error msg if cannot draw plot
  plot_eligible <- reactive({
    msg <- paste0(input$dist, ": distribution is not symmetric")
    validate(
      need(!(input$type == "probability" & input$side == "two-sided" & !is_symmetric()), msg)
    )
  })
  
  #######################
  # Compute Probability #
  #######################

  # probability
  output$prob <- renderText({

    # initalize the output text with distribution
    prefix <- paste0(input$dist, ": ")

    # compute probabilities
    out_text <- switch(
      input$side,
      # compute probabilities for one-sided computations
      "one-sided" = {
        switch(
          input$side2,
          "lower tail" = paste0(prefix, "P(X <= ", in_x(), ") = ", signif(get_params()$pfun(in_x()), signif_digits)),
          "upper tail" = paste0(prefix, "P(X > ", in_x(), ") = ", signif(get_params()$pfun(in_x(), lower.tail = FALSE), signif_digits))
        )
      },
      # compute probabilities for two-sided computations
      "two-sided" = {
        switch(
          input$side2,
         "between" = paste0(prefix, "P(", in_x1(), " < X < ", in_x2(), ") = ", signif(get_params()$pfun(in_x2()) - get_params()$pfun(in_x1()) , signif_digits)),
         "both tails" = paste0(prefix, "P(X < ", in_x1(), " or X > ", in_x2(), ") = ", signif(get_params()$pfun(in_x2(), lower.tail = FALSE) + get_params()$pfun(in_x1()) , signif_digits))
        )
      }
    )

    # if p is specified, print probability as long as it is not a two-tailed of a non-symmetric distribution
    plot_eligible()
    out_text

  })

  ###################
  # Generate Graphs #
  ###################
  
  output$density <- renderPlot({
    # obtain parameters
    params <- get_params()

    # discrete distributions
    if( is_discrete() ){

      # generate data and shading
      x <- params$min:params$max
      data <- data.frame(x = x, y = params$dfun(x))
      data$shade <- switch(
        input$side,
        "one-sided" = switch(
          input$side2,
          "lower tail" = ifelse(data$x <= in_x(), "shade", "no-shade"),
          "upper tail" = ifelse(data$x <= in_x(), "no-shade", "shade")
        ),
        "two-sided" = switch(
          input$side2,
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

      shade_lim <- switch(
        input$side,
        "one-sided" = switch(
          input$side2, 
          "lower tail" = c(params$min, in_x()),
          "upper tail" = c(in_x(), params$max)
        ),
        "two-sided" = c(in_x1(), in_x2())
      )
      
      # fix for if probability = 0: make the shading invisible
      if(shade_lim[1] == shade_lim[2]){
        shade_lim <- c(shade_lim[1], shade_lim[1]*1.0001)
      }
      a_val <- ifelse(shade_lim[1] == shade_lim[2], 0, 1)
      
      # apply outline to all considered values
      range_lim <- c(min(params$min, shade_lim), max(params$max, shade_lim))
      
      density <- ggplot(data = NULL, aes(x = range_lim)) +
        geom_area(stat = "function", fun = params$dfun, fill = "royalblue", xlim = shade_lim, alpha = a_val) +
        geom_area(stat = "function", fun = params$dfun, fill = "white", color = "black", size = 1.25, alpha = 0)
    }
    
    density <- density + xlab("X") + ylab("Density") + ggtitle(paste(input$dist, "distribution"))

    # if p is specified, print plot as long as it is not a two-tailed of a non-symmetric distribution
    plot_eligible()
    density
  })

  output$cdf <- renderPlot({

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
  })
  
  # plot outputs
  output$plots <- renderUI({
    if( input$side == "one-sided" & input$side2 == "lower tail"){
      column(width = 12, plotOutput("density", height = "300px"), plotOutput("cdf", height = "300px"))
    } else {
      plotOutput("density", height = "300px")
    }
  })
  

} # end of server function

