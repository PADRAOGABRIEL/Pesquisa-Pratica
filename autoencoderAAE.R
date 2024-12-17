#loading Harbinger
library(daltoolbox)

## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo

## 
## Attaching package: 'daltoolbox'

## The following object is masked from 'package:base':
## 
##     transform


library(harbinger) 




#loading the example database
data(examples_anomalies)

#Using the simple time series 
dataset <- examples_anomalies$simple
head(dataset)



#ploting the time series
plot_ts(x = 1:length(dataset$serie), y = dataset$serie)




# establishing han_autoencoder method 
model <- han_autoencoder(3, 2, aae_encode_decode, num_epochs = 1500)




# fitting the model
model <- fit(model, dataset$serie)




# making detections
detection <- detect(model, dataset$serie)

# filtering detected events
print(detection |> dplyr::filter(event==TRUE))



# evaluating the detections
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

##           event      
## detection TRUE  FALSE
## TRUE      1     4    
## FALSE     0     96

# ploting the results
grf <- har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)


# ploting the results
res <-  attr(detection, "res")
plot(res)


