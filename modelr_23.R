library(tidyverse)
library(modelr)
library(purrr)

options(na.action = na.warn)

ggplot(sim1, aes(x,y))+
  geom_point() 

models <- tibble(
  a1= runif(250, -20, 40),
  a2= runif(250, -5, 5)
)

models
ggplot(sim1, aes(x,y))+
  geom_point() +
  geom_abline(aes(intercept=a1,
                  slope=a2),
              data = models,
              alpha= 1/8)


model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7,1.5), sim1)

qq <-as.tibble(model1(c(7, 1.5), sim1))

qq


measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)


sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))


models

ggplot(sim1, aes(x,y)) +
  geom_point(size=2, colour="darkgrey")+
  geom_abline(
    aes(intercept=a1, 
        slope=a2,
        colour = -dist),
    data=filter(models, rank(dist) <10)
  )

ggplot(models,
       aes(a1,a2))+
  geom_point(data=filter(models,
                         rank(dist)<11),
                         size=5,
                         colour="red")+
  geom_point(aes(colour=-dist))


best <- optim(c(0,0), measure_distance, data=sim1)

best$par

ggplot(sim1,
       aes(x,y))+
  geom_point(size=3,
             colour="grey")+
  geom_abline(intercept = best$par[1],
              slope = best$par[2])


tupac <- lm(y~x, data=sim1)
coef(tupac)

tupac$coefficients

ggplot(sim1,
       aes(x,y))+
  geom_point(size=3,
             colour="red")+
  geom_abline(intercept = tupac$coefficients[1],
              slope = tupac$coefficients[2])


# exercise ----------------------------------------------------------------

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

ggplot(sim1a, aes(y,x))+
  geom_point()

q1a <- lm(y~x, data = sim1a)
q1a

ggplot(sim1a, aes(x,y)) +
  geom_point(size=4) +
  geom_abline(intercept = q1a$coefficients[1],
              slope = q1a$coefficients[2])


# predictions -------------------------------------------------------------


grid <-  sim1 %>%
  data_grid(x)
grid

grid <- grid %>%
  add_predictions(tupac)

ggplot(sim1, aes(x)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=pred), data = grid,
            colour="red", size = 3)

sim1 <- sim1 %>%
  add_residuals(tupac)

sim1
ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 1)



ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()



# EXERCISE 2 -------------------------------------------------------------
### 1
q1a <- loess(y~x, data = sim1a)
q1a

grid <-  sim1a %>%
  data_grid(x)
grid

grid <- grid %>%
  add_predictions(q1a)

grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=pred), data = grid,
            colour="red", size = 3)

sim1 <- sim1 %>%
  add_residuals(q1a)

sim1
ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 1)



ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

### 2

grid <- grid %>%
  add_predictions(q1a)

?gather_predictions
?spread_predictions


df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)


m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))
grid %>% add_predictions(m1)

m2 <- lm(y ~ poly(x, 2), data = df)

grid %>% spread_predictions(m1, m2)
grid %>% gather_predictions(m1, m2)


?geom_ref_line



# formulas and families ---------------------------------------------------

pp <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)

pp


model_matrix(pp, y ~ x1)

model_matrix(pp, y ~ x1 + x2)

pp <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
) 

pp
model_matrix(pp, response ~ sex)


ggplot(sim2) +
  geom_point(aes(x,y))


mod2 <- lm(y ~ x, data = sim2)

mod2

grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)

grid


ggplot(sim2, aes(x)) +
  geom_point(aes( y = y )) +
  geom_point(data=grid, aes(y=pred), colour="red", size = 5)

ggplot(sim3, aes(x1, y))+
  geom_point(aes(colour=x2))


mod1 <- lm(y ~ x1 + x2, data=sim3)
mod1
mod2 <- lm(y ~ x1 * x2, data=sim3)
mod2

grid <- sim3 %>%
  data_grid(x1,x2) %>%
  gather_predictions(mod1, mod2)

grid


ggplot(sim3, aes(x1, y, colour=x2))+
  geom_point()+
  geom_line(data=grid, aes(y = pred))+
  facet_wrap(~ model)


sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)
sim3


ggplot(sim3, aes(x1, resid, colour=x2))+
  geom_point() +
  facet_grid(model ~ x2)


ggplot(sim4, aes(x=x1,
                 y=y))+
  geom_point(aes(colour = x2))


mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)


grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5), pretty=TRUE,
    x2 = seq_range(x2, 5), pretty=TRUE) %>%
  gather_predictions(mod1, mod2)
 
grid

ggplot(grid, aes(x1,x2)) +
  geom_tile(aes(fill=pred))+
  facet_wrap(~model)


ggplot(grid,
       aes(x1, pred, colour = x2,
           group=x2))+
  geom_line()+
  facet_wrap(~model)

ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)


df


model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)
model_matrix(df, y~ poly(x,2))



library(splines)
 model_matrix(df, y~ ns(x, 2))
 
 
 sim5 <-  tibble(
   x = seq(0, 3.5*pi, length = 50),
   y = 4 * sin(x) + rnorm(length(x))
 )
 
 sim5
 
 ggplot(sim5, aes(x, y)) +
   geom_point()
 
