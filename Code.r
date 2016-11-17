file <- read.csv(//location//file.csv, Header = T) \\file has all data points measured so far
ifelse(nrow(file)>250, data<-file[c(nrow(file)-248:nrow(file)),], data<-file)
count <- 2000 \\ no. of iterations needed
for ( i in 1:count)
{
Sys.sleep(15)
if(class(read.csv(//location//data.csv)) != "try-error")
    {
    new <- read.csv(//location//new.csv, header = T)
    data[250,] <- new[2,]
    }
else
    {
    next
    }
x <- BoxCox.lambda(data[,ncol(data)]) \\renders data homoscedastic

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
}
