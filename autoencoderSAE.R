
```r
# Harbinger Package
# version 1.1.707



#loading Harbinger
library(daltoolbox)
library(harbinger) 
```


```r
#loading the example database
data(examples_anomalies)
```


```r
#Using the simple time series 
dataset <- examples_anomalies$simple
head(dataset)
```

```
##       serie event
## 1 1.0000000 FALSE
## 2 0.9689124 FALSE
## 3 0.8775826 FALSE
## 4 0.7316889 FALSE
## 5 0.5403023 FALSE
## 6 0.3153224 FALSE
```


```r
#ploting the time series
plot_ts(x = 1:length(dataset$serie), y = dataset$serie)
```

![plot of chunk unnamed-chunk-4](fig/han_autoencoder_sae/unnamed-chunk-4-1.png)


```r
# establishing han_autoencoder method 
model <- han_autoencoder(3, 2, sae_encode_decode, num_epochs = 1500)
```


```r
# fitting the model
model <- fit(model, dataset$serie)
```


```r
# making detections
detection <- detect(model, dataset$serie)
```


```r
# filtering detected events
print(detection |> dplyr::filter(event==TRUE))
```

```
## [1] idx   event type 
## <0 rows> (or 0-length row.names)
```


```r
# evaluating the detections
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)
```

```
##           event      
## detection TRUE  FALSE
## TRUE      0     0    
## FALSE     1     100
```


```r
# ploting the results
grf <- har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)
```

![plot of chunk unnamed-chunk-10](fig/han_autoencoder_sae/unnamed-chunk-10-1.png)

```r
# ploting the results
res <-  attr(detection, "res")
plot(res)
```

![plot of chunk unnamed-chunk-11](fig/han_autoencoder_sae/unnamed-chunk-11-1.png)