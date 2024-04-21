library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)

df <- read_csv('Dataset/Departuredata.csv')

colnames(df)

glimpse(df)

df <- df %>%
  select(
    `Hong Kong Residents`,
    MainlandReddays,
    ExtremeWeather,
    `Red days`,
    Weekday,
    Date
  ) %>%
  rename(
    HK = `Hong Kong Residents`,
    MR = MainlandReddays,
    EW = ExtremeWeather,
    RD = `Red days`,
    WD = Weekday,
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

tree <- rpart(HK ~ MR + EW + RD + WD , data = df_train)
prp(tree)
rpart.plot(tree, extra = 101, under = TRUE, cex = 2, box.palette = "auto")


tree_predictions <- predict(tree, df_test)
y_test <- df_test %>% pull(HK)
cor(tree_predictions, y_test)


