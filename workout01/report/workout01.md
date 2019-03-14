Workout 1
================
Claudia Korea

<img src="C:/Users/CK/Documents/stat133/hw-stat133/workout01/images/gsw-shot-chart.png" width="80%" style="display: block; margin: auto;" />

``` r
gsw <- read.csv('C:/Users/CK/Documents/stat133/hw-stat133/workout01/data/shots-data.csv')

two_pt_shooting <- arrange(summarise(group_by(gsw, "name"), total = sum("shot_type" == "2PT Field Goal"), made = sum("shot_type" == "2PT Field Goal" & "shot_made_flag" == "shot_yes"), perc_made = made/total), desc = TRUE)
```
