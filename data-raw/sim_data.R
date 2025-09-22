set.seed(1)
n <- 1500
id <- rep(1:300, each = 5)        ## panel id (300 panels x 5 periods)
time <- rep(1:5, times = 300)     ## panel time
x1 <- rnorm(n); x2 <- rnorm(n); u <- rnorm(n)
ps <- plogis(-0.2 + 0.8*x1 - 0.6*x2 + 0.8*u)
d  <- rbinom(n, 1, ps)
tau <- 1 + 0.5*x1
y <- 2 + tau*d + 0.5*x1 - 0.4*x2 + 0.8*u + rnorm(n, sd = .8)

sim_data <- tibble::tibble(id, time, y, d, x1, x2)
usethis::use_data(sim_data, overwrite = TRUE)
