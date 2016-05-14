
library(plyr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

function(input, output){
  
  # convert user distributions from English to R
  dist <- reactive({
    r_dist <- c("norm", "t", "gamma", "chisq", "f", "unif", "binom", "nbinom", "pois")
    eng_dist <- c("Normal", "T", "Gamma", "Chi-Square", "F", "Uniform", "Binomial", "Negative Binomial", "Poisson")
    plyr::mapvalues(input$dist, eng_dist, r_dist)
  })
  
  # flag for discrete distributions
  discrete <- reactive({ifelse(dist() %in% c("norm", "t", "gamma", "chisq", "f", "unif"), FALSE, TRUE)})
  
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
  
}
