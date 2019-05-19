
# Loading the required libraries
library(quantmod) ; library(TTR);

system.time ({

Intial_Equity = 100000

# Pulling NSE data from Yahoo finance
symbol = "PNB.NS"
data = getSymbols(symbol, from ="2010-01-01",auto.assign = FALSE)
colnames(data) = c("Open","High","Low","Close","Volume","Adjusted")

data = na.omit(data)
closeprice = Cl(data)

# Computing RSI
rsi = round(RSI(closeprice, n = 14, maType="WMA"),1);
rsi = c(NA,head(rsi,-1))

# Computing SMA and LMA
ShMA = 20; LMA = 50;

sma = round(SMA(closeprice, ShMA),1)
sma = c(NA,head(sma,-1))

lma = round(SMA(closeprice, LMA),1)
lma = c(NA,head(lma,-1))

# Computing ADX 
data22 = ADX(data[,c("High","Low","Close")])
data22 = as.data.frame(data22)
adx = round(data22$ADX,1)
adx = c(NA,head(adx,-1))

# Create the label 
data$Return = round(dailyReturn(data$Close, type='arithmetic'),2)
colnames(data) = c("Open","High","Low","Close","Volume","Adjusted","Return")

class = character(nrow(data))
class = ifelse(coredata(data$Return) >= 0,"Up","Down")

data2 = data.frame(data,class,rsi,sma,lma,adx)
write.csv(data2,file="decision tree charting_table.csv")

data = data.frame(class,rsi,sma,lma,adx)
data = na.omit(data)

write.csv(data,file="decision tree charting.csv")

})
