file <- read.csv(//location//file.csv, Header = T) \\file has all data points measured so far
ifelse(nrow(file)>250, data<-file[c(nrow(file)-249, nrow(file)),], data<-file)
while(TRUE)
{
Sys.sleep(15)
if(class(read.csv(//location//data.csv)) != "try-error")
    {
    new <- read.csv(//location//new.csv, header = T)
    data <- data[-1,]
    data[250,] <- new[2,]
    }
x <- BoxCox.lambda(data[,ncol(data)]) //renders data homoscedastic

\\Dickey-Fuller test to check for stationarity w.r.t trend and seasonality of the mean
ns <- nsdiffs(x)  
if (ns>0)
    xstar <- diff(x, lag=frequency(x),differences=ns)
else
    xstar <- x
nd <- ndiffs(xstar)
if(nd >0)
    xstar <- diff (star,differences=nd)

\\ARIMA
model <- auto.arima(xstar)
result <- forecast(model)
write.table(data.frame(c(2),result),"export.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
