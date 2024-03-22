# Collection of internal functions for the package


q2_5 <- function(x)c(q2_5 = quantile(x,probs = c(0.025),
                                     names = FALSE))

q97_5 <- function(x)c(q97_5 = quantile(x,probs = c(0.975),
                                       names = FALSE))
