# Severe Weather Events Investigation
Johan Jordaan  
16 March 2016  

## Synopsis
Synopsis: Immediately after the title, there should be a synopsis which describes and summarizes your analysis in at most 10 complete sentences.

Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variable) are most harmful with respect to population health?
Across the United States, which types of events have the greatest economic consequences?

## Data Processing

```r
library(lubridate)
library(dplyr)
library(ggplot2)
```


```r
data <- read.csv("repdata-data-StormData.csv.bz2")
```


```r
dim(data)
```

```
## [1] 902297     37
```

There should be a section titled Data Processing which describes (in words and code) how the data were loaded into R and processed for analysis. In particular, your analysis must start from the raw CSV file containing the data. You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider using the 𝚌𝚊𝚌𝚑𝚎 = 𝚃𝚁𝚄𝙴 option for certain code chunks.

## Results
At least one but not more than 3 figures



