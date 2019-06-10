#packages required:
require(tidyverse)
require(nycflights13)
require(gapminder)
require(Lahman)
require(ggplot2)
require(maps)
require(modelr)
require(lubridate)
require(hexbin)
require(splines)

# show data about mpg
mpg

# descriptive information about mpg
?mpg

#number of rows in the dataset
nrow(mpg)
#number of columns in the dataset
ncol(mpg)

#About visuals: 
#Bar charts, histograms, and frequency polygons bin your data and then plot bin counts,
#the number of points that fall in each bin.
#Smoothers fit a model to your data and then plot predictions from the model.
#Boxplots compute a robust summary of the distribution and then display a specially formatted box.

#Visualize displ vs hwy:
#A geom is the geometrical object that a plot uses to represent data.
#The plot shows a negative relationship between engine size (displ)
#and fuel efficiency (hwy). In other words, cars with big engines use more fuel. 
#Does this confirm or refute your hypothesis about fuel efficiency and engine size?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#The values of hwy and displ are rounded so the points appear on a grid and
#many points overlap each other. This problem is known as overplotting.
#This arrangement makes it hard to see where the mass of the data is. 
#Are the data points spread equally throughout the graph, or is there one 
#special combination of hwy and displ that contains 109 values?
#You can avoid this gridding by setting the position adjustment to “jitter”.
#position = "jitter" adds a small amount of random noise to each point.
#This spreads the points out because no two points are likely to 
#receive the same amount of random noise.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

#visualize hwy vs cyl
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl, color = class))

#Let's map class to the alpha aesthetic, which controls the transparency 
#of the points, or to the shape aesthetic, which controls the shape of the points.
# alpha
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,color = class, alpha = class))

# shape
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,color = class, shape = class))

#One way to add additional variables is with aesthetics. Another way,
#particularly useful for categorical variables, is to split your plot into facets,
#subplots that each display one subset of the data.
#The variable that you pass to facet_wrap() should be discrete.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,color = class)) + 
  facet_wrap(~ class, nrow = 2)

#To facet your plot on the combination of two variables, 
#add facet_grid() to your plot call. The first argument of facet_grid() 
#is also a formula. This time the formula should contain two variable names separated by a ~.
#If you prefer to not facet in the rows or columns dimension, 
#use a . instead of a variable name, e.g. + facet_grid(. ~ cyl).
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,color = class)) + 
  facet_grid(drv ~ cyl)

#geom smooth shows a line graph
ggplot(data = mpg) + 
geom_smooth(mapping = aes(x = displ, y = hwy))

#you could set the linetype of a line. geom_smooth() will draw a different line,
#with a different linetype, for each unique value of the variable that you map to
#linetype.
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color=drv))

#To display multiple geoms in the same plot, add multiple geom functions to ggplot():
  ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy, color=class)) +
        geom_smooth(mapping = aes(x = displ, y = hwy))
#or
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) + 
    geom_point() + 
    geom_smooth()  

#You can use the same idea to specify different data for each layer. 
#Here, our smooth line displays just a subset of the mpg dataset, the subcompact
#cars. The local data argument in geom_smooth() overrides the global data argument in ggplot() for that layer only.
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point(mapping = aes(color = class)) + 
    geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

  
#display data using a bar graph  
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = cut))
#or
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut, fill = cut))

#change the stat of geom_bar() from count (the default) to identity.
#This lets me map the height of the bars to the raw values of a y variable. 
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)
#visualize
ggplot(data = demo) + 
  geom_bar(mapping = aes(x = cut,y = freq,color=cut, fill = cut),stat = "identity")

#You might want to override the default
#mapping from transformed variables to aesthetics.
#you might want to display a bar chart of proportion, rather than count:
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))


#You might want to draw greater attention to the statistical transformation
#in your code. For example, you might use stat_summary(), which summarises the 
#y values for each unique x value, to draw attention to the summary that you’re computing:
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )


#Note what happens if you map the fill aesthetic to another variable, 
#like clarity: the bars are automatically stacked. Each colored rectangle 
#represents a combination of cut and clarity.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))


#The stacking is performed automatically by the position adjustment
#specified by the position argument. If you don’t want a stacked bar chart, 
#you can use one of three other options: "identity", "dodge" or "fill".

#position = "identity" will place each object exactly where it falls in the 
#context of the graph. This is not very useful for bars, because it overlaps them.
#To see that overlapping we either need to make the bars slightly transparent by
#setting alpha to a small value, or completely transparent by setting fill = NA.
#The identity position adjustment is more useful for 2d geoms, like points, where
#it is the default.
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")


#position = "fill" works like stacking, but makes each set of stacked bars
#the same height. This makes it easier to compare proportions across groups.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

#position = "dodge" places overlapping objects directly beside one another.
#This makes it easier to compare individual values.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")


#coord_flip() switches the x and y axes. This is useful (for example),
#if you want horizontal boxplots. It’s also useful for long labels:
#it’s hard to get them to fit without overlapping on the x-axis.
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()


#coord_quickmap() sets the aspect ratio correctly for maps.
#This is very important if you’re plotting spatial data with ggplot2
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()


#coord_polar() uses polar coordinates. Polar coordinates reveal an
#interesting connection between a bar chart and a Coxcomb chart.
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()



#MODELLING
#The goal of a model is to provide a simple low-dimensional summary of a dataset.
#Strong patterns will hide subtler trends, so we’ll use models to help peel back
#layers of structure as we explore a dataset.

#Lets take a look at the simulated dataset sim1, 
#included with the modelr package. It contains two continuous variables,
#x and y. Let’s plot them to see how they’re related:
ggplot(sim1, aes(x, y)) + 
  geom_point()


#Let’s start by getting a feel for what models from that family look like by
#randomly generating a few and overlaying them on the data. For this simple case,
#we can use geom_abline() which takes a slope and intercept as parameters.
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

#We need to find the good models by making precise our intuition that a good model
#is “close” to the data.One easy place to start is to find the vertical distance between
#each point and the model.This distance is just the difference between the y value given 
#by the model (the prediction), and the actual y value in the data (the response).

#we first turn our model family into an R function. This takes the model parameters and
#the data as inputs, and gives values predicted by the model as output:
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

#Next, we need some way to compute an overall distance between the predicted and actual
#values. In other words, the plot above shows 30 distances: how do we collapse that into
#a single number?One common way to do this in statistics to use the “root-mean-squared 
#deviation”. We compute the difference between actual and predicted, square them, average
#them, and the take the square root.
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

#Now we can use purrr to compute the distance for all the models defined above. We need a 
#helper function because our distance function expects the model as a numeric vector of length 2.
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

#overlay the 10 best models on to the data. I’ve coloured the models by -dist: this is an easy way
#to make sure that the best models (i.e. the ones with the smallest distance) get the brighest colours.
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

#We can no longer directly see how the model compares to the data, but we can see many models at once.
#Again, I’ve highlighted the 10 best models, this time by drawing red circles underneath them.
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#generate an evenly spaced grid of points (this is called a grid search). I picked the parameters of
#the grid roughly by looking at where the best models were in the plot above.
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

#overlay the best 10 models back on the original data:
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

#You could imagine iteratively making the grid finer and finer
#until you narrowed in on the best model. But there’s a better way to tackle that
#problem: a numerical minimisation tool called Newton-Raphson search. The intuition
#of Newton-Raphson is pretty simple: you pick a starting point and look around for
#the steepest slope. You then ski down that slope a little way, and then repeat 
#again and again, until you can’t go any lower. In R, we can do that with optim()
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

#R has a tool specifically designed for fitting linear models called lm().
#lm() has a special way to specify the model family: formulas. Formulas look
#like y ~ x, which lm() will translate to a function like y = a_1 + a_2 * x. 
#We can fit the model and look at the output:
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

#To visualise the predictions from a model, we start by generating an evenly
#spaced grid of values that covers the region where our data lies. The easiest way
#to do that is to use modelr::data_grid(). Its first argument is a data frame, 
#and for each subsequent argument it finds the unique variables and then generates all combinations:
grid <- sim1 %>% 
  data_grid(x) 
grid

#Next we add predictions. We’ll use modelr::add_predictions() which takes a data frame and a model.
#It adds the predictions from the model to a new column in the data frame:
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

#plot the predictions
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)


#The flip-side of predictions are residuals. The predictions tells you the pattern that the model has
#captured, and the residuals tell you what the model has missed. The residuals are just the distances 
#between the observed and predicted values that we computed above.
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

#frequency polygon to help you understand the spread of the residuals.
#This helps you calibrate the quality of the model: how far away are the predictions from 
#the observed values? Note that the average of the residual will always be 0.
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

#You’ll often want to recreate plots using the residuals instead of the original predictor.
ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 


# If you want to see what R actually does, you can use the model_matrix() function. It takes
#a data frame and a formula and returns a tibble that defines the model equation: each column 
#in the output is associated with one coefficient in the model, the function is always y = a_1 * out1 + a_2 * out_2
df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
model_matrix(df, y ~ x1)

#The model matrix grows in an unsurprising way when you add more variables to the the model
model_matrix(df, y ~ x1 + x2)

#sim2 dataset from modelr
ggplot(sim2) + 
  geom_point(aes(x, y))

#fit a model, and generate predictions
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

#see if we overlay the predictions on top of the original data:
ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#visualize sim3 from modelr. it constains a categorical predictor
#and a continuous predictor
ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

#two possible models you could fit in this data
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

#When you add variables with +, the model will estimate
#each effect independent of all the others. It’s possible to fit the so-called
#interaction by using *. For example, y ~ x1 * x2 is translated to y = a_0 + a_1 * x1 + a_2 * x2 + a_12 * x1 * x2.
#Note that whenever you use *, both the interaction and the individual components are included in the model.
#To visualise these models we need two new tricks:
#We have two predictors, so we need to give data_grid() both variables. 
#It finds all the unique values of x1 and x2 and then generates all combinations.
#To generate predictions from both models simultaneously, we can use gather_predictions()
#which adds each prediction as a row. The complement of gather_predictions() is 
#spread_predictions() which adds each prediction to a new column.
grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

#We can visualise the results for both models on one plot using facetting:
ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

#Which model is better for this data? We can take look at the residuals. 
#Here I’ve facetted by both model and x2 because it makes it easier to see 
#the pattern within each group.
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

#visualize
ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

#Let’s take a look at the equivalent model for two continuous variables.
#Initially things proceed almost identically to the previous example:
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

#pretty = TRUE will generate a “pretty” sequence, i.e. something that looks
#nice to the human eye. This is useful if you want to produce tables of output:
seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

#trim = 0.1 will trim off 10% of the tail values. This is useful if the
#variables have a long tailed distribution and you want to focus on generating
#values near the center:
x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)

#expand = 0.1 is in some sense the opposite of trim() it expands the range by 10%.
x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

#visualise that model. We have two continuous predictors, so you can imagine the
#model like a 3d surface. We could display that using geom_tile():
ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

#Instead of looking at the surface from the top, we could look at it from 
#either side, showing multiple slices:
ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

#WHY ARE LOW QUALITY DIAMONDS MORE EXPENSIVE?
#we’ve seen a surprising relationship between the quality of diamonds and their
#price: low quality diamonds (poor cuts, bad colours, and inferior clarity) have
#higher prices.
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

#It looks like lower quality diamonds have higher prices because there is an important
#confounding variable: the weight (carat) of the diamond.
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

#lets make a couple of tweaks to the diamonds dataset to make it easier to work with:
#1.Focus on diamonds smaller than 2.5 carats (99.7% of the data)
#2.Log-transform the carat and price variables.
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

#Together, these changes make it easier to see the relationship between carat
#and price:
ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

#remove that strong linear pattern by the pattern explicit by fitting a model:
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

#look at what the model tells us about the data. Note that I back transform the
#predictions, undoing the log transformation, so I can overlay the predictions on the raw data:
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

#Now we can look at the residuals, which verifies that we’ve successfully removed the strong linear pattern:
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

#we can now re-do our motivating plots using those residuals instead of price.
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

#include color, cut, and clarity into the model so that we also make explicit the
#effect of these three categorical variables:
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

#This model now includes four predictors, so it’s getting harder to visualise.
#Fortunately, they’re currently all independent which means that we can plot them
#individually in four plots. To make the process a little easier, we’re going to 
#use the .model argument to data_grid:
grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) + 
  geom_point()

#If the model needs variables that you haven’t explicitly supplied, data_grid() 
#will automatically fill them in with “typical” value. For continuous variables,
#it uses the median, and categorical variables it uses the most common value
#(or values, if there’s a tie).
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

#This plot indicates that there are some diamonds with quite large residuals
#- remember a residual of 2 indicates that the diamond is 4x the price that we 
#expected. It’s often useful to look at unusual values individually:
diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

#Nothing really jumps out at me here, but it’s probably worth spending time
#considering if this indicates a problem with our model, or if there are errors
#in the data. If there are mistakes in the data, this could be an opportunity to
#buy diamonds that have been priced low incorrectly.


#WHAT AFFECTS THE NUMBER OF DAILY FLIGHTS?
#Let’s get started by counting the number of flights per day and visualising it.
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily

ggplot(daily, aes(date, n)) + 
  geom_line()

#Understanding the long-term trend is challenging because there’s a very strong
#day-of-week effect that dominates the subtler patterns. Let’s start by looking 
#at the distribution of flight numbers by day-of-week:
daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))

ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

#fit the model, and display its predictions overlaid on the original data:
mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

#compute and visualise the residuals:
daily <- daily %>% 
  add_residuals(mod)
daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

#Our model seems to fail starting in June: you can still see a strong regular
#pattern that our model hasn’t captured. Drawing a plot with one line for each day
#of the week makes the cause easier to see:
ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()
#Our model fails to accurately predict the number of flights on Saturday: during summer
#there are more flights than we expect, and during Fall there are fewer.

#There are some days with far fewer flights than expected:
daily %>% 
  filter(resid < -100)
#If you’re familiar with American public holidays, you might spot New Year’s day, July 4th,
#Thanksgiving and Christmas. There are some others that don’t seem to correspond to public holidays.

#There seems to be some smoother long term trend over the course of a year.
#We can highlight that trend with geom_smooth():
daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)
#There are fewer flights in January (and December), and more in summer (May-Sep).

#Let’s first tackle our failure to accurately predict the number of flights on Saturday.
#A good place to start is to go back to the raw numbers, focussing on Saturdays:
daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")
#I suspect this pattern is caused by summer holidays: many people go on holiday in the summer,
#and people don’t mind travelling on Saturdays for vacation. Looking at this plot, we might guess
#that summer holidays are from early June to late August. That seems to line up fairly well with
#the state’s school terms: summer break in 2013 was Jun 26–Sep 9.
#Why are there more Saturday flights in the Spring than the Fall? I asked around and they suggested
#that it’s less common to plan family vacations during the Fall because of the big Thanksgiving and
#Christmas holidays. We don’t have the data to know for sure, but it seems like a plausible working hypothesis.

#create a “term” variable that roughly captures the three school terms, and check our work with a plot:
term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

#It’s useful to see how this new variable affects the other days of the week:
daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()

#there is significant variation across the terms, so fitting a separate day of
#week effect for each term is reasonable. This improves our model, but not as much as we might hope:
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

#We can see the problem by overlaying the predictions from the model on to the raw data:
grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)

#Our model is finding the mean effect, but we have a lot of big outliers, so mean
#tends to be far away from the typical value. We can alleviate this problem by 
#using a model that is robust to the effect of outliers: MASS::rlm(). 
#This greatly reduces the impact of the outliers on our estimates, and gives a 
#model that does a good job of removing the day of week pattern:
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

#it’s a good idea to bundle the creation of variables up into a function so there’s
#no chance of accidentally applying a different transformation in different places.
#For example, we could write:
compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date), 
      wday = wday(date, label = TRUE)
    )
}

#A simple linear trend isn’t adequate, so we could try using a natural spline to
#fit a smooth curve across the year:
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()