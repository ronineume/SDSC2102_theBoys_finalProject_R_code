library(tidyverse)
library(tidymodels)

# Loading the data
df = read_csv('Dataset/Departuredata.csv')
glimpse(df)

df = df %>%
  select(
    Date, # 
    'Hong Kong Residents', # 
    'Mainland Visitors', # 
    'Other Visitors', # 
    'Red days',
    MainlandReddays,
    ExtremeWeather,
    Weekday
  ) %>%
  rename(
    date = Date,
    hkr = 'Hong Kong Residents',
    mldv = 'Mainland Visitors',
    othv = 'Other Visitors',
    redhk= 'Red days',
    redmld = MainlandReddays,
    exw = ExtremeWeather,
    wkd = Weekday
  )

#Splitting the data
df_train <- df %>%
  filter(month(date) %in% c(5, 7, 9))

df_test <- df %>%
  filter(month(date) %in% c(6, 8, 10))

#fitting the data
model <- lm(hkr~wkd+redhk+exw, data=df_train)
model_summary <- tidy(
  model,
  conf.int=TRUE,
  conf.level=0.95
)

model_summary

model_summary %>%
  select(estimate, conf.low, conf.high, p.value)

#Making Prediction
test_preds <- predict(model, newdata=df_test, type='response')
print(test_preds[1:10])

#Draw a graph
dates <- as.Date(c(df_test$date))
drawdata <- data.frame(dates, 
                       Actual = df_test$hkr, Predicted = test_preds)
Sys.setlocale("LC_TIME", "English")

plot(drawdata$dates,drawdata$Actual,type="o",col="red",xlab="Date",ylab="Amount",
     main="Actual vs Prediction for Departure of Hong Kong Residents")
lines(drawdata$dates,drawdata$Predicted,type="o",col="blue")
legend("topleft",title="Data type",c("Actual","Prediction"),
       lty=c(1,1),inset=.05,pch=c(1,1),col=c("red","blue"))