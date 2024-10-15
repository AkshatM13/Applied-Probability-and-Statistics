---
  title: "Quantifying uncertainty using simulation"
output:
  pdf_document: default
html_document:
  df_print: paged
editor_options:
  chunk_output_type: console
---
  
  #### Execute the following cells to load the libraries
  ```{r}
library(ggplot2)
library(dplyr)
```

#### Sampling space and likelihood vector
```{r}
s = c('H', 'T')
p = c(0.5, 0.5)
```


#### Function to simulate one trial of the random experiment which in this case is tossing 10 fair coins at a time
```{r}
simTrial = function(){
  return(sample(s, 10, replace = TRUE, prob = p))
}
```

#### Replicate the trials of the random experiment a large number of times
```{r}
nsimulations = 10000
simulatedData = replicate(nsimulations, simTrial())
```

#### Function to check if event of interest has occured which in this case is getting exactly 3 heads
```{r}
checkEvent = function(data){
  return(sum(data == 'H') == 3)
}
```

#### Calculate the fraction of times the event of interest occured which is what we define as the approximate probability of the event E happening in the next trial of the random experiment
```{r}
#simulatedData
mean(apply(simulatedData, 2, checkEvent))

```


```{r}
s = c('H', 'T')
p = c(0.1, 0.9)
cointoss = function(){
  return(sample(s, 10, replace = TRUE, prob = p))
}
result = replicate(1000, cointoss())
checkEvent = function(data){
  
  if(data[1]!='T'){
    return(1)}
  else{
    return(0)}}
a=apply(result,2,checkEvent)
mean(a)
```

##read court data
```{r}
file = 'C:/Users/91630/Desktop/court.csv'
courtData = read.csv(file, header = TRUE)
head(courtData, 10)
```

```{r}
simTrialjudge=function(n){
  jcp=apply(courtData,2,mean)
  jicp=1-jcp
  
  s=c(1,0)
  court_decision=vector('integer',n)
  for (j in 1:n) {
    court_decision[j]=sample(s,1,p=c(jcp[j],jicp[j]))
    
  }
  return(court_decision)
  
}
checkEvent=function(data){
  if(sum(data) <=2){
    return(1)
  }
  else{
    return(0)}
}
simulatedData=replicate(1000,simTrialjudge(5))
mean(apply(simulatedData,2,checkEvent))
```

```{r}
simTrialjudge=function(n){
  jcp=apply(courtData,2,mean)
  jicp=1-jcp
  
  s=c(1,0)
  court_decision=vector('integer',n)
  for (j in 1:n) {
    court_decision[j]=sample(s,1,p=c(jcp[j],jicp[j]))
    
  }
  return(court_decision)
  
}
checkEvent=function(data){
  if(sum(data) <=2){
    return(1)
  }
  else{
    return(0)}
}
simulatedData=replicate(1000000,simTrialjudge(5))
mean(apply(simulatedData,2,checkEvent))


```

```{r}
checkEvent=function(data){
  if(sum(data) <=2){
    return(1)
  }
  else{
    return(0)}
}
```

```{r}
simulatedData=replicate(1000,simTrialjudge(5))
mean(apply(simulatedData,2,checkEvent))
```

```{r}

```

