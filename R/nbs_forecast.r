nbsForecast <-function(ts='',days) {
	timeseries<-c(ts);
	# Necessary Packages
	library(forecast)

	period = ts(timeseries,frequency=7);
	fit<-auto.arima(timeseries,approximation=FALSE);
	future<-forecast(fit,h=days);

	mean = as.vector(future$mean);
	lower = future$lower[,1];
	upper = future$upper[,1];

	return (rbind(mean,lower,upper));
}
