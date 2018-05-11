library(tidyverse)
library(modelr)
library(boot)
library(car)
library(corrplot)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)


diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))


ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)


mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

mod_diamond_glm <- glm(lprice ~ lcarat, data = diamonds2, family = gaussian)
glm.diag.plots(mod_diamond_glm)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

##multiple variables
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
summary(mod_diamond2)
avPlots(mod_diamond2)

#non_numeric <- c('cut', 'color', 'clarity')
#subset(df, select=-c(z,u))
M <- cor(subset(diamonds2, select = -c(cut, color, clarity)))
corrplot.mixed(M, lower.col = "black", number.cex = .7)

grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) + 
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)


