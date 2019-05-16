# #02.Introduction 
# The purpose of this project is to analyze 3 different algorithms for the financial
# forecasting of Daimler share. Disclaimer I am working on Advanced Analytics of 
# Daimler AG. This analysis is for educational purposes and not for financial advising.
# 
# #03.Methodology
# We will use Daimler historical share market datasets (from 2010). For forecasting future 
# values. On those nature of forecasting we assume that some patterns of our sets 
# have carriage on future short linear interims. The same approach is applied on 
# the weather forecasting.

#install required libraries 

if(!require(ggthemes)) install.packages("ggthemes")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
if(!require(TSA)) install.packages("TSA")
if(!require(forecast)) install.packages("forecast")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(corrplot)) install.packages("corrplot")
if(!require(data.table)) install.packages("data.table")
if(!require(TTR)) install.packages("TTR")
if(!require(quantmod)) install.packages("quantmod")
if(!require(caret)) install.packages("caret")
if(!require(xgboost)) install.packages("xgboost")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(DT)) install.packages("DT")
if(!require(xts)) install.packages("xts")
if(!require(doParallel)) install.packages("doParallel")
if(!require(e1071)) install.packages("e1071")
if(!require(dynlm)) install.packages("dynlm")
if(!require(dplyr)) install.packages("dplyr")


#If you dont have keras and tensorflow in your system please install them
# devtools::install_github("rstudio/keras")
# devtools::install_github("rstudio/tensorflow")
#install_tensorflow()
library(dplyr)
library(dynlm)
library(ggthemes)
library(ggplot2)
library(tidyr)
library(tseries)
library(TSA)
library(forecast)
library(gridExtra)
library(corrplot)
library(data.table)
library(TTR)
library(quantmod)
library(plotly)
library(GGally)
library(TTR)
library(data.table)
library(kableExtra)
library(plotly)
library(TTR)
library(caret)
library(xgboost)
library(dplyr)
library(lubridate)
library(DT)
library(xts)
library(e1071)
library(doParallel)
library(keras)
library(tensorflow)


#------------------------------------------
# Downloading Daimler AG Symbol (DDAIF)
#------------------------------------------
# Caution the google API is not operational anymore
# We used yahoo
start <- as.Date("2010-05-01")
end <- as.Date("2019-05-01")

require(quantmod)
getSymbols("DDAIF", from = start, to = end)

head(DDAIF)
require(dplyr)
glimpse(DDAIF)
#Add Daimler stock log returns
DDAIF_log_returns <- DDAIF%>%Ad()%>%dailyReturn(type='log')

#We plot a chart series graph to perform technical analysis on the  price chart 
#since 2010:
DDAIF %>% Ad() %>%chartSeries()

#We select only 2018. We analyze volume feature, Bollinger Bands, and Moving 
# Average Convergence Divergence.
DDAIF %>% chartSeries(TA='addBBands();addVo();addMACD()',subset='2018')

#We check the columns for na'S
apply(DDAIF, 2, function(x) any(is.na(x)))

# View Data Frame
View(DDAIF)

# We plot an interactive graph with opening Prices since 2010 per semester
require(ggthemes)
require(ggplot2)
require(plotly)
p1 <- ggplot(DDAIF, aes(x = index(DDAIF), y = DDAIF[,1])) + geom_line(color = "yellow") + 
  ggtitle("Daimler stock Opening Prices") + xlab("Date") + ylab("Opening Prices") + 
    scale_x_date(date_labels = "%b %y", date_breaks = "6 months")+
  theme_solarized(light=FALSE)+
  theme(axis.text=element_text(size=10,angle = 90),plot.title = element_text(size = 11, color = "yellow", hjust = 0.5))

ggplotly(p1)

# We plot an interactive graph with the Adjusted Closing Prices
p2 <- ggplot(DDAIF, aes(x = index(DDAIF), y = DDAIF[,6])) + geom_line(color = "yellow") + 
  ggtitle("Daimler Adjusted Closing Prices") + xlab("Date") + ylab("Adjusted Closing Prices") + 
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
theme_solarized(light=FALSE)+
  theme(axis.text=element_text(size=10,angle = 90),plot.title = element_text(size = 11, color = "yellow", hjust = 0.5))

ggplotly(p2)

# We plot an An open-high-low-close chart (also OHLC) 
#LINK:(https://en.wikipedia.org/wiki/Open-high-low-close_chart)
require(gridExtra)
options(repr.plot.width=10, repr.plot.height=10) 
popen = ggplot(DDAIF, aes(DDAIF.Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "yellow", fill = "yellow", alpha = 0.2) + geom_density()
phigh = ggplot(DDAIF, aes(DDAIF.High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "blue", fill = "red", alpha = 0.2) + geom_density()
plow = ggplot(DDAIF, aes(DDAIF.Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.2) + geom_density()
pclose= ggplot(DDAIF, aes(DDAIF.Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "black", fill = "red", alpha = 0.2) + geom_density()
grid.arrange(popen,phigh,plow,pclose, nrow=2,ncol=2)
#------------------------------------------
# Data preparation for Stock Time Series 
#------------------------------------------
#We create a response variable. To predicting future days price, we apply 
# lag function in the price change
# We Calculate the various moving averages (MA) of a series for volume.
#For the past 10, 20 , 60 days
require(TTR)
DDAIF$Avg_volume_10  <- SMA(DDAIF$DDAIF.Volume, n = 10)
DDAIF$Avg_volume_20  <- SMA(DDAIF$DDAIF.Volume, n = 20)
DDAIF$Avg_volume_60  <- SMA(DDAIF$DDAIF.Volume, n = 60)

# We calculate the % of the average volume of the above days
DDAIF$Volume_perc_avg_10 <- (DDAIF$DDAIF.Volume/DDAIF$Avg_vol_10)*100
DDAIF$Volume_perc_avg_20 <- (DDAIF$DDAIF.Volume/DDAIF$Avg_vol_20)*100
DDAIF$Volume_perc_avg_60 <- (DDAIF$DDAIF.Volume/DDAIF$Avg_vol_60)*100

# We calculate the range between high and low 
DDAIF$Range <- DDAIF$DDAIF.High - DDAIF$DDAIF.Low 

# % change of closing price. 
DDAIF$perc_change_closing <- (DDAIF$DDAIF.Close - lag(DDAIF$DDAIF.Close))/lag(DDAIF$DDAIF.Close) * 100

# Range between  prior days closing price and todays closing price
DDAIF$change_from_yest <- DDAIF$DDAIF.Close - lag(DDAIF$DDAIF.Close)

# We Calculate again the various moving averages (MA) for range now .
#For the past 10, 20 , 60 days
DDAIF$moving_avg_10 <- SMA(DDAIF$Range, n = 10)
DDAIF$moving_avg_20 <- SMA(DDAIF$Range, n = 20)
DDAIF$moving_avg_60 <- SMA(DDAIF$Range, n = 60)

# We calculate the % of the average range of the above days
DDAIF$perc_moving_avg_10  <- (DDAIF$Range/DDAIF$moving_avg_10) * 100
DDAIF$perc_moving_avg_20 <- (DDAIF$Range/DDAIF$moving_avg_20) * 100
DDAIF$perc_moving_avg_60 <- (DDAIF$Range/DDAIF$moving_avg_60) * 100

# The tot amount of money traded multiplied by the volume (in dollars)
DDAIF$cash_tradet <- DDAIF$DDAIF.Close*DDAIF$DDAIF.Volume

# The average volume of cash trated for the same periods as above
DDAIF$avg_cash_trated_10 <- SMA(DDAIF$cash_tradet, n = 10)
DDAIF$avg_cash_trated_20 <- SMA(DDAIF$cash_tradet, n = 20)
DDAIF$avg_cash_trated_60 <- SMA(DDAIF$cash_tradet, n = 60)

# The % of the avgo volume today.
DDAIF$Avg_Dollar_volume_pct_10 <- (DDAIF$cash_tradet/DDAIF$avg_cash_trated_10) * 100
DDAIF$Avg_Dollar_volume_pct_20 <- (DDAIF$cash_tradet/DDAIF$avg_cash_trated_20) * 100
DDAIF$Avg_Dollar_volume_pct_60 <- (DDAIF$cash_tradet/DDAIF$avg_cash_trated_60) * 100


# Todays open vs Yesterdays Close. 
require(data.table)
require(dplyr)

DDAIF$nightgap <- DDAIF$DDAIF.Open - lag(DDAIF$DDAIF.Close)

# The Gap % win or loss from yesterday closing prices
DDAIF$night_gap_perc <- (DDAIF$DDAIF.Open - lag(DDAIF$DDAIF.Close))/lag(DDAIF$DDAIF.Close) * 100

# % of range: daimler stock success MAX=100, MIN=0 
# Williams %R, also known as the Williams Percent Range, is a type of momentum 
# indicator that moves between 0 and -100 and measures overbought and oversold 
# levels. The Williams %R may be used to find entry and exit points in the market.
# The indicator is very similar to the Stochastic oscillator and is used in the 
# same way. It was developed by Larry Williams and it compares a stock’s closing
# price to the high-low range over a specific period, typically 14 days or periods.
# LINK (https://www.investopedia.com/terms/w/williamsr.asp)
DDAIF$perc_range_previous = abs((DDAIF$DDAIF.Close - DDAIF$DDAIF.Open)/(DDAIF$DDAIF.High-DDAIF$DDAIF.Low)*100)
DDAIF$perc_range_atpr = (DDAIF$Range/DDAIF$DDAIF.Close)*100
DDAIF$perc_range_williams = (DDAIF$DDAIF.High-DDAIF$DDAIF.Close)/(DDAIF$DDAIF.High-DDAIF$DDAIF.Low)*100

# Compute range for 1 Month
require(zoo)
one_month_range_perc <- rollapply(DDAIF$DDAIF.High, 20, max) - rollapply(DDAIF$DDAIF.Low, 20, max)

DDAIF$one_month_range_perc = (DDAIF$DDAIF.Close- DDAIF$DDAIF.Low)/one_month_range_perc*100
gc()#clean RAM

# Moving averages smooth the price data to form a trend following indicator. They
# do not predict price direction, but rather define the current direction, though 
# they lag due to being based on past prices. Despite this, moving averages help 
# smooth price action and filter out the noise. They also form the building blocks
# for many other technical indicators and overlays, such as Bollinger Bands, MACD 
# and the McClellan Oscillator. The two most popular types of moving averages are
# the Simple Moving Average (SMA) and the Exponential Moving Average (EMA)
#LINK (https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages)
require(TTR)
DDAIF$EMA10 <- EMA(DDAIF$DDAIF.Low, n = 10)
DDAIF$EMA20 <- EMA(DDAIF$DDAIF.Low, n = 20)

#Weighted Moving Average
DDAIF$EMA60 <- EMA(DDAIF$DDAIF.Low, n = 60)

#Double Exponential Moving Average is a measure of a security's trending average
# price that gives the most weight to recent price data. Like exponential moving 
# average, or EMA, it is more reactive to price fluctuations than a simple moving 
# average LINL (https://www.investopedia.com/ask/answers/121814/what-double-exponential-moving-average-dema-formula-and-how-it-calculated.asp)
DDAIF$WMA10 <- WMA(DDAIF$DDAIF.Low, n = 10)

# The EVWMA uses the volume to declare the period of the MA.
DDAIF$EVWMA10 <- EVWMA(DDAIF$DDAIF.Low, DDAIF$DDAIF.Volume)

#Zero Lag Exponential Moving Average (ZLEMA) As is the case with the double 
# exponential moving average (DEMA) and the triple exponential moving average 
# (TEMA) and as indicated by the name, the aim is to eliminate the inherent lag 
# associated to all trend following indicators which average a price over time.
# LINK(https://en.wikipedia.org/wiki/Zero_lag_exponential_moving_average)
DDAIF$ZLEMA10 <- ZLEMA(DDAIF$DDAIF.Low, n = 10)

#Volume weighted average price (VWAP) and moving volume weighted average price 
# (MVWAP) are trading tools that can be used by all traders. However, these tools
# are used most frequently by short-term traders and in algorithm-based trading 
# programs.(https://www.investopedia.com/articles/trading/11/trading-with-vwap-mvwap.asp)
DDAIF$VWAP10 <- VWAP(DDAIF$DDAIF.Low, DDAIF$DDAIF.Volume)

#The Hull Moving Average (HMA), developed by Alan Hull, is an extremely fast 
# and smooth moving average. In fact, the HMA almost eliminates lag altogether 
# and manages to improve smoothing at the same time
# https://www.fidelity.com/learning-center/trading-investing/technical-analysis/
#   technical-indicator-guide/hull-moving-average
DDAIF$HMA10 <- HMA(DDAIF$DDAIF.Low, n = 20)

#The ALMA moving average uses curve of the Normal (Gauss) distribution which 
# can be placed by Offset parameter from 0 to 1. This parameter allows regulating
# the smoothness and high sensitivity of the Moving Average.
#(https://www.prorealcode.com/prorealtime-indicators/alma-arnaud-legoux-moving-average/)
DDAIF$ALMA10 <- ALMA(DDAIF$DDAIF.Low, n = 9, offset = 0.85, sigma = 6)

#------------------------------------------
# Augmented Dickey-Fuller test 
# AND correlation test          
#------------------------------------------
require(tseries)

#From package tseries we use the () adf.test- Computes the Augmented Dickey-Fuller 
#test for the null that x has a unit root. 
adf.test(DDAIF$DDAIF.Adjusted)
#The result shows that we need to move stationarity
require(forecast)
require(xts)
require(e1071)
require(doParallel)
require(dynlm)
require(caret)

DDAIF_lm <- na.omit(DDAIF)#we handle missing values
set.seed(123)#algorithm for reproducability
X <- DDAIF_lm[,-6]
y <- DDAIF_lm[,6]

# We scale the variables in order to run the models
X.scaled <- scale(X)
gc()#clean RAM

# We merge them back 
DDAIF_lm <- cbind(X.scaled, y)

#create index
numerical_Vars <- which(sapply(DDAIF_lm, is.numeric)) 

#save the vector
numerical_VarNames <- names(numerical_Vars)
cat('They exist', length(numerical_Vars), 'numerical variables.\n')

sum_numVar <- DDAIF_lm[, numerical_Vars]

#We calculate the correlations of all numerical variables
correl_numVar <- cor(sum_numVar, use="pairwise.complete.obs") 

#We order of the decreasing correlations vs sales price
correl_sorted <- as.matrix(sort(correl_numVar[,'DDAIF.Adjusted'], decreasing = TRUE))

#We choose only the high corellated
Correl_High <- names(which(apply(correl_sorted, 1, function(x) abs(x)>0.5)))
correl_numVar <- correl_numVar[Correl_High, Correl_High]

require(corrplot)
#Using mixed methods to visualize a correlation matrix.
corrplot.mixed(correl_numVar, tl.col="black", tl.pos = "lt", number.cex=0.5)
gc()#clean RAM

# We remove the highly correlated variables to avoid overfitting of models           
del <- cor(DDAIF_lm)
del[upper.tri(del)] <- 0
diag(del) <- 0

DDAIF_lm <- DDAIF_lm[,!apply(del,2,function(x) any(x > 0.90))]

#------------------------------------------
# We create our Train and Test Datasets
#------------------------------------------
# For next day forecast n = days_forecast + 1. If you want more days change the 
#+1
days_to_forecast = 7
n = days_to_forecast + 1
X_train = DDAIF_lm[1:(nrow(DDAIF_lm)-(n-1)),-17]
# Our dependent var: Is the price adj
y_train = DDAIF_lm[n:nrow(DDAIF_lm),17]
X_test = DDAIF_lm[((nrow(DDAIF_lm)-(n-2)):nrow(DDAIF_lm)),-17]

require(quantmod)
#We create the validation test of the real prices of the next 7 days
# Adapt dates according to your n days of forecast
DDAIF2 = getSymbols('DDAIF', from='2019-04-22', to='2019-05-01',auto.assign = FALSE)
#create dates for the plots
ourdate <- time(DDAIF2)
#real prices
y_test <- as.numeric(DDAIF2$DDAIF.Adjusted)
#our train dataset
train <- cbind(X_train,y_train)
#check the number of features if they are same
dim(X_train)
dim(X_test)

#------------------------------------------
# KERAS DEEP LEARNING : backend TensorFlow
#------------------------------------------
# We will apply deep learning networks of linear stack densely connected layers
#If you already have installed Keras and tensorflow then skip the below commands
# devtools::install_github("rstudio/keras")
# devtools::install_github("rstudio/tensorflow")
#install_tensorflow()
require(keras)
require(tensorflow)

ker = ncol(X_train)
## We compose : From Keras Model a linear stack of layersc
keras_model <- keras_model_sequential() 

keras_model %>% 
  #We ddd a densely-connected NN layer to an output
  #ReLU (Rectified Linear Unit) Activation Function
  layer_dense(units = 60, activation = 'relu', input_shape = ker) %>% 
  layer_dropout(rate = 0.2) %>% #We apply dropout  to prevent overfitting
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'linear')

str(keras_model)

keras_model %>% compile(
  optimizer = 'rmsprop',
  loss = 'mse',
  metrics = 'mse')

#------------------------------------------
# Step 2: We train the model 
#------------------------------------------

keras_history <- keras_model %>% fit(X_train, y_train, epochs=200,
                                     batch_size=28, validation_split = 0.1,
                                     callbacks = callback_tensorboard("logs/run_a"))

#Plot history -optional
plot(keras_history)
#View keras board on browser- optional
tensorboard("logs/run_a")

keras_pred <- keras_model %>% predict(X_test, batch_size = 28)
#------------------------------------------
# Step 3: We plot the predictions 
#------------------------------------------
require(ggplot2)
require(ggthemes)
require(plotly)

real_VS_pred <- data.frame(keras_pred,y_test)
colnames(real_VS_pred) <- c("KERAS_PRED","REAL_PRICES")    

pkeras <-  ggplot(real_VS_pred, aes(ourdate)) + 
  geom_line(aes(y = keras_pred, colour = "KERAS_PRED"))+
  geom_line(aes(y = y_test, colour = "REAL_PRICES"))+
    geom_point(aes(y = keras_pred, colour = "KERAS_PRED"), size=2) +
  geom_point(aes(y = y_test, colour = "REAL_PRICES"), size=2) +
  labs(title = "Keras (Predicted vs Actual)",x = "Date",y = "Daimler Share Price in $")+
  theme_solarized(light=FALSE) 

ggplotly(pkeras) 

#We display Pred vs Actual of all models   
require(kableExtra)
kable(real_VS_pred) %>%
  kable_styling(bootstrap_options = "bordered" , full_width = F , position = "center") %>%
  column_spec(1,bold = T ,color = "red" , background = "grey" )


#------------------------------------------
# Lasso regression model   
#------------------------------------------
# With caret package we will apply cross validation in order to find the optimal 
# hyperparameters
gc()#clean RAM
require(caret)

train$DDAIF.Adjusted <- as.numeric(train$DDAIF.Adjusted)
set.seed(123)#algorithm for reproducability
trainControl <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lassomod <- train(DDAIF.Adjusted ~., data = na.omit(train), method='glmnet', trControl= trainControl, 
                  tuneGrid=lassoGrid) 
#we display the optimal alpha and lambda penalties
lassomod$bestTune

#We display the root mean squared error
min(lassomod$results$RMSE)

#From caret package we use () varImp. Is a generic method for calculating 
#variable importance for objects produced by train and method specific methods
lasso_VarImp <- varImp(lassomod,scale=F)
lasso_Importance <- lasso_VarImp$importance
vars_Selected <- length(which(lasso_Importance$Overall!=0))
vars_NotSelected <- length(which(lasso_Importance$Overall==0))
#Display the Laso vars penalty
cat('The Lasso regression used', vars_Selected, 'variables, and did not used', vars_NotSelected, 'variables.\n')

#Run the prediction for the next 7 days
LassoPred <- predict(lassomod, X_test)

# Display the prediction for the next 7 days
real_VS_pred <- data.frame(keras_pred,LassoPred,y_test)
colnames(real_VS_pred) <- c("KERAS_PRED","LASSO_PRED","REAL_PRICES")    

#Interactive plot of Keras, Lasso regression Predicted vs Actual Prices
require(ggplot2)
require(GGally)
require(plotly)

plasso <-  ggplot(real_VS_pred, aes(ourdate)) + 
  geom_line(aes(y = keras_pred, colour = "KERAS_PRED")) + 
  geom_line(aes(y = LassoPred, colour = "LASSO_PRED")) + 
  geom_line(aes(y = y_test, colour = "REAL_PRICES"))+
  geom_point(aes(y = keras_pred, colour = "KERAS_PRED"), size=2) +
  geom_point(aes(y = LassoPred, colour = "LASSO_PRED"), size=2) +
  geom_point(aes(y = y_test, colour = "REAL_PRICES"), size=2) +
  labs(title = "Keras - Lasso (Predicted vs Actual)",x = "Date",y = "Daimler Share Price in $")+
  theme_solarized(light=FALSE) 

ggplotly(plasso) 

require(kableExtra)
kable(real_VS_pred) %>%
  kable_styling(bootstrap_options = "bordered" , full_width = F , position = "center") %>%
  column_spec(3,bold = T ,color = "red" , background = "grey" )

#------------------------------------------
# XGBoost model 
#------------------------------------------
library(xgboost)
library(caret)
#We define the parameters that caret will use in the finding of hyperparameters
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1)

gc()#clean RAM
# With the 5 fold cross validation, caret package can find the optimal hyperparameters
# for our model (takes a lot of time...)
xgb_hyperparam <- train(DDAIF.Adjusted~., data = na.omit(train), method='xgbTree', trControl= trainControl, tuneGrid=xgb_grid) 
gc()#clean RAM

#We display the best hyperparameters
xgb_hyperparam$bestTune

#Creation of label
labeltrain <- y_train[!is.na(y_train)]

# For this model we have to tranform our sets into Dmatrix objects
train_dmatrix <- xgb.DMatrix(data = as.matrix(X_train), label= labeltrain)
test_dmatrix <- xgb.DMatrix(data = as.matrix(X_test))

# We apply the best hyperparameters of the cross validation
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.1, 
  gamma=0,
  max_depth=3, 
  min_child_weight=2, 
  subsample=1,
  colsample_bytree=1)

#We apply the cross validation function of xgboost to find optimal nrounds
xgb_cv <- xgb.cv( params = default_param, data = train_dmatrix, nrounds = 1000, 
                  nfold = 5, showsd = T, stratified = T, print_every_n = 40, 
                  early_stopping_rounds = 10, maximize = F, verbose = TRUE)

#Best nrounds
xgb_cv        
#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = train_dmatrix, params=default_param, nrounds = 829)
xgb_pred <- predict(xgb_mod, test_dmatrix)
xgb_pred

real_VS_pred <- data.frame(keras_pred,LassoPred,xgb_pred,y_test)
colnames(real_VS_pred) <- c("KERAS_PRED","LASSO_PRED","XGB_PRED","REAL_PRICES")    

#Interactive plot of Keras, Lasso and XGB: Predicted vs Actual Prices
require(ggplot2)
require(GGally)
require(plotly)
pxgb <-  ggplot(real_VS_pred, aes(ourdate)) + 
  geom_line(aes(y = keras_pred, colour = "KERAS_PRED"))+
  geom_line(aes(y = LassoPred, colour = "LASSO_PRED")) + 
  geom_line(aes(y = xgb_pred, colour = "XGB_PRED"))+
  geom_line(aes(y = y_test, colour = "REAL_PRICES"))+
  geom_point(aes(y = keras_pred, colour = "KERAS_PRED"), size=2) +
  geom_point(aes(y = LassoPred, colour = "LASSO_PRED"), size=2) +
  geom_point(aes(y = xgb_pred, colour = "XGB_PRED"), size=2) +
  geom_point(aes(y = y_test, colour = "REAL_PRICES"), size=2) +
  labs(title = "Lasso - XGB (Predicted vs Actual)",x = "Date",y = "Daimler Share Price in $")+
  theme_solarized(light=FALSE) 

ggplotly(pxgb) 

require(kableExtra)
kable(real_VS_pred) %>%
  kable_styling(bootstrap_options = "bordered" , full_width = F , position = "center") %>%
  column_spec(4,bold = T ,color = "red" , background = "grey" )



# #10.Conclusion
# Usually the share price daily fluctuation is between 1 - 2 percent in ordinary
# time periods. Unfortunately the above models presented high daily fluctuation. 
# Regardless from our application of the mathematical technical indicators.
# Into our datasets before the training of the models.
# 
# Unfortunately currently our models are not adequate to forecast time series 
# of markets successfully. 
# 
# #11.Proposal
# On another paper, i have also created some models to analyze the correlation of 
# social media sentiment and Daimler share price. There my models forecasting was
# significantly more successful. I would propose to create a model that would
# analyze and combine the results of: 



