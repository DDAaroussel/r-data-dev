library(tidyverse)
library(modelr)
library(splines)
library(ggrepel)
library(viridis)
options(na.action = na.warn)

ggplot(sim1, aes(x, y)) + 
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x,y)) + geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()

model1 <- function(a, data){
  a[1] + data$x * a[2]
}
model1(c(7.5, 1.5), sim1)

measure_distance <- function(mod, data){
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

measure_distance(c(7.5, 1.5), sim1)

sim1_dist <- function(a1, a2){
  measure_distance(c(a1, a2), sim1)
}

models <- models %>%
  mutate(dist = map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

sim1_mod <- lm(y ~ x, data = sim1a)
coef(sim1_mod)
ggplot(sim1a, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = sim1_mod$coefficients[1], slope = sim1_mod$coefficients[2])

measure_distance <- function(mod, data) {
  diff <- data$y - make_prediction(mod, data)
  mean(abs(diff))
}

grid <- sim1 %>%
  data_grid(x)
grid <- grid %>%
  add_predictions(sim1_mod)

ggplot(sim1, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) + geom_freqpoly(binwidth = 0.5)

sim1_mod <- loess(y ~ x, data = sim1)
sim1 <- sim1 %>%
  add_predictions(sim1_mod)
sim1
 
ggplot(sim2) + 
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)


mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)

ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)

mod <- lm(y ~ x, data = df, na.action = na.exclude)
nobs(mod)

library(tidyverse)
library(modelr)
options(na.action = na.warn)
install.packages("nycflights13")
library(nycflights13)
library(lubridate)
install.packages("hexbin")

ggplot(data = diamonds, aes(x = cut, y = price)) + geom_boxplot()
ggplot(data = diamonds, aes(x = clarity, y = price)) + geom_boxplot()
ggplot(data = diamonds, aes(x = color, y = price)) + geom_boxplot()

ggplot(data = diamonds, aes(x = carat, y = price)) + geom_hex(bins = 50)

diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))
  
ggplot(data = diamonds2, aes(x = lcarat, y = lprice)) + geom_hex(bins = 50)
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)

ggplot(data = diamonds2, aes(x = carat, y = price)) + geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond, "lresid")

ggplot(data = diamonds2, aes(x = carat, y = lresid)) + geom_hex(bins = 50)

ggplot(data = diamonds2, aes(x = cut, y = lresid)) + geom_boxplot()
ggplot(data = diamonds2, aes(x = clarity, y = lresid)) + geom_boxplot()
ggplot(data = diamonds2, aes(x = color, y = lresid)) + geom_boxplot()

mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamond2) %>%
  add_predictions(mod_diamond2)

ggplot(data = grid, aes(x = cut, y = pred)) + geom_point()

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond2, var = "lresid2")
ggplot(data = diamonds2, aes(x = lcarat, y = lresid2)) + geom_hex(bins = 50)

pricing_error <- diamonds2 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamond2, var = "pred") %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)

#There appear to be some pricing errors in the Fair cut category with one Fair diamond in particular
#being priced at ~$10k

library(nycflights13)
library(lubridate)
head(flights)
daily <- flights %>%
  mutate(date = make_date(year = year, month = month, day = day)) %>%
  group_by(date) %>%
  summarise(n = n())
daily
ggplot(data = daily, aes(x = date, y = n)) + geom_line()

daily <- daily %>%
  mutate(wday = wday(date, label = TRUE))

ggplot(data= daily, aes(x = wday, y = n)) + geom_boxplot()
mod <- lm(n ~ wday, data = daily)
grid <- daily %>%
  data_grid(wday) %>%
  add_predictions(mod, "pred")
ggplot(data = daily, aes(x = wday, y = n)) + geom_boxplot() +
  geom_point(data = grid, aes(x = wday, y = pred), colour = "red", size = 4) 

daily <- daily %>%
  add_residuals(mod)
ggplot(data = daily, aes(x = date, y = resid, colour = wday)) + geom_ref_line(h = 0) + geom_line()

daily %>%
  ggplot(aes(x = date, y = resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") +
  geom_smooth(se = FALSE, span = 0.2)

daily %>%
  filter(wday == 'Sat') %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summmer", "fall")
  )
}

daily <- daily %>%
  mutate(term = term(date))

daily %>%
  filter(wday == 'Sat') %>%
  ggplot(aes(x = date, y = n, colour = term)) +
  geom_point() +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")
  
daily %>%
  ggplot(aes(x = wday, y = n, colour = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday + term, data = daily)

daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(x = date, y = resid, colour = model)) +
  geom_line(alpha = 0.75)

grid <- daily %>%
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(data = daily, aes(x = wday, y = n)) + geom_boxplot() +
  geom_point(data = grid, aes(colour = "red")) + facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday + term, data = daily)

daily %>%
  add_residuals(mod3, "resid") %>%
  ggplot(aes(x = date, y = resid)) + geom_line()

mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) + labs(title = "Fuel efficiency decreases with engine size",
                                 subtitle = "Sports cars are the exception due to their light weight",
                                 caption = "data from whereever",
                                 x = "Engine size",
                                 y = "Highway miles",
                                 colour = "Car type")

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point(aes(colour = class)) +
  geom_point(data = best_in_class, size = 3, shape = 1) +
  ggrepel::geom_label_repel(data = best_in_class, aes(label = model), nudge_y = 2)





