
# SESSION AIM
# 1. Download and use libraries and data to vizualise the relationship between
#    population size, life expectancy, and economic development (GDP per capita)
#    in various countries and various years
# 2. Filter and subset the data (i.e. select parts of the data to use)


# Install required libraries: 
#   1. the data (gapminder), 
#   2. a package that allows us to use R in a "tidy" way (tidyverse), and 
#   3. a graphics package (ggplot2)

# install.packages("gapminder")
# install.packages("tidyverse")
# install.packages("ggplot2")

# Tell R we want to use these libraries
library(gapminder)
library(tidyverse)
library(ggplot2)

# let's look at the first few rows of the data
head(gapminder)
View(gapminder)

# We'll filter the data and just use some of it
#  (Can you see what this code does?)
gapminder_euro2007 <- gapminder  %>%
  filter(continent == "Europe" & year ==  2007) %>%
  mutate(pop_e6 = pop / 1000000)
#selecting data from 2007 of European countries, with population notation shortened

# Now we can use ggplot plot the data:population with life expectancy
ggplot(gapminder_euro2007, aes(x = pop_e6, y = lifeExp)) +
  geom_point(col ="red")


# YOUR TASK
# Use ggplot to plot life expectancy with gdpPercap (GDP per capita)
ggplot(gapminder_euro2007, aes(x = lifeExp, y = gdpPercap)) + geom_point()

# Questions
# 1. What sort of "model" might fit the relationship between life expectancy
#     and GDP per capita?
#A linear regression model might fit well for the relationship between life expectancy and GDP per capita,
# as there appears to be a consistent positive correlation where the higher the life expectancy, the higher
# GDP per capita is.

# 2. Does the pattern look the same for countries in other continents, e.g. asia?
gapminder_asia2007 <- gapminder  %>% filter(continent == "Asia" & year ==  2007) %>% mutate(pop_e6 = pop / 1000000)
gapminder_asia2007 = gapminder_asia2007 %>% arrange(lifeExp)
head(gapminder_asia2007)
gapminder_no_afghanistan = gapminder_asia2007 %>% filter(country != "Afghanistan")
head(gapminder_no_afghanistan)
ggplot(gapminder_asia2007, aes(x = lifeExp, y = gdpPercap)) + geom_point()
ggplot(gapminder_no_afghanistan, aes(x = lifeExp, y = gdpPercap)) + geom_point()
#The pattern appears to hold for Asia as well, as in general, a higher life expectancy correlates with a higher
# GDP per capita. However, Afghanistan stands out, with a significantly lower life expectancy
# than any other Asian country in 2007.

# 3. Does the pattern look the same for years in the mid-late 20th century.
gapminder_1982 <- gapminder  %>% filter(year ==  1982) %>% mutate(pop_e6 = pop / 1000000)
head(gapminder_1982)
ggplot(gapminder_1982, aes(x = lifeExp, y = gdpPercap)) + geom_point()
#The positive correlation seems to certainly hold even in the mid-late 20th century (1982), but it appears
# less linear and more exponential, when looking in a global context.