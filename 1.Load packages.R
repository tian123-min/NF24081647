#Load packages
my_packages = c('ranger','cowplot', 'RSNNS', 'h2o',
                'data.table', 'tidyverse','pdp',
                'e1071')

for (package in my_packages)  {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
theme_set(theme_cowplot())

