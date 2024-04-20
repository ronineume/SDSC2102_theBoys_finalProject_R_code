library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)

df <- read_csv('/Users/parkjongwan/Desktop/SDSC2102 Project/Arrivaldata.csv')

colnames(df)

glimpse(df)

df <- df %>%
  select(
    `Mainland Visitors`,
    MainlandReddays,
    ExtremeWeather,
    Date
  ) %>%
  rename(
    MV = `Mainland Visitors`,
    MR = MainlandReddays,
    EW = ExtremeWeather,
    date = Date
  )

#split_object <- initial_split(data=df, prop=0.5)

#Splitting the data
df_train <- df %>%
  filter(month(date) %in% c(5, 7, 9))

df_test <- df %>%
  filter(month(date) %in% c(6, 8, 10))

#df_train <- training(split_object)
#df_test <- testing(split_object)

glimpse(df_train)

glimpse(df_test)

tree <- rpart(MV ~ MR + EW , data = df_train)
prp(tree)

tree_predictions <- predict(tree, df_test)
y_test <- df_test %>% pull(MV)
cor(tree_predictions, y_test)
