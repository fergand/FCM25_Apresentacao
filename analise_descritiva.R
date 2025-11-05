# install.packages(c("palmerpenguins", "tidyverse", "ggthemes"))
library(palmerpenguins)
library(tidyverse)
library(ggthemes); theme_set(theme_clean())


data("penguins", package = "palmerpenguins")

glimpse(penguins)
