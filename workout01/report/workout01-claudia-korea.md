Workout 1
================
Claudia Korea

### Introduction

The Golden State Warriors have been rising stars in the basketball world, with one of the highest effective field goal percentage. They are on track to be the fourth team with a effective field goal percentage over fifty percent and are a phenomenon. With such an amazing team so close to home, it is worth analyzing their star players. In this report, we will analyze the Golden State Warrior's five players, Kevin Durant, Andre Iguodala, Klay Thompson, Stephen Curry, and Draymond Green. We will compare whether they have a better two point field goal percentage or a better three point field goal percentage.

### Background

According to John Schuhmann from NBA.com, in December, 2017, the Golden State Warriors had an effective field goal percentage of 58.5 percent. Since 2015, the Golden State Warriors have been setting the all-time record for effective field goal percentage. The truly valuable aspect of the Golden State Warriors is their offensive efficiency, which make up for their lackluster performance in their free throw rates, turnover percentage and offensive rebounding percentage. Although individual star players actually decreased in their effective field goal percentage, as a whole team, other team members of the Golden State Warriors have stepped up to the challenged and brought the overall average of the effective field-goal percentage.

We have data on five players: Andre Iguodala, Draymond Green, Kevin Durant, Klay Thompson, Stephen Curry. Our database gives us information about the shots each player attempted throughout the 2016 season. We have information about which period the shot was made in, how many minutes were remaining in the game, how many seconds were remaining in the game, whether the shot was made or not, what kind of basketball move was made by the player, either to pass defenders, gain access to the basket or to cleanly pass the ball to a teammate, the type of shot the player attempted, either two or three point field goals, the distance away from the basket where the shot was attempted, in feet, and the x and y coordinates of where the shot was attempted, in inches.

------------------------------------------------------------------------

Below is a figure of our five players: Andre Iguodala, Draymond Green, Kevin Durant, Klay Thompson and Stephen Curry's two and three point shot attempts from the 2016 seasons. The red dot indicates a failed shot attempt, the green dot indicates a successful shot attempt.

###### Figure:

<img src="C:/Users/CK/Documents/stat133/hw-stat133/workout01/images/gsw-shot-chart.png" width="80%" style="display: block; margin: auto;" />

From simple observation of the figure, it is impossible to determine whether two point field goals have a higher percentage of shots made or whether three point field goals have a higher percentage of shots made. Given that the Golden State Warriors' effective field goal percentage hover around fifty percent, it makes sense that the charts for each player also have a relatively even distribution of green and red dots. From a crude observation, we can see that Andre Iguodala has most likely the least total number of shots attempted and Raymond Green has the second least number of attempted shots.

------------------------------------------------------------------------

Given the data we have, we can calculate many meaningful statistics that can bring insight about the effective field goal percentages of two pointer field shots and three pointer field shots. We can first calculate which player attempts the greatest number of shots:

``` r
total_attempts<- arrange(summarise(group_by(gsw, name), total = sum(shot_type == "2PT Field Goal" | shot_type == "3PT Field Goal")),desc(total))
kable(total_attempts)
```

| name           |  total|
|:---------------|------:|
| Stephen Curry  |   1250|
| Klay Thompson  |   1220|
| Kevin Durant   |    915|
| Draymond Green |    578|
| Andre Iguodala |    371|

We get an interesting result! Stephen Curry made the most shooting attempts of 1250, followed by Klay Thompson with 1220 attempts. Interestingly enough, third, fourth, and fifth place shooters have significantly lower shot attempts successively. Kevin Durant made 300 shots attempts less than the second place, Draymond Green approximately 300 shot attempts less than Kevin Durant and Andre Iguodala approximately 200 shot attempts less than Draymond Green. The significant lack of attempted shots of third, fourth and fifth place may be due to multiple factors, including how much play time each player gets during a game, their role on the team and how often their teammates pass to them in a position to take a shot. For instance, Draymond Green is a strong offensive player and it would make sense that he does make as many shooting attempts as a shooting guard like Stephen Curry.

------------------------------------------------------------------------

With this in mind, we can observe the effective field goal percentages of the players for two point field shots and three point field shots, with the column "name" corresponding to the player name, the column "total" corresponding to the total two or three point shots attempted, respectively, the column "made" corresponding to the total two or three point shots made, respectively, and the column "perc\_made" corresponding to the percentage of two or three point field goal shots made, respectively. The tables are arranged in descending order based on the column "perc\_made".

###### Two Point Field Goal Shooting

| name           |  total|  made|  perc\_made|
|:---------------|------:|-----:|-----------:|
| Andre Iguodala |    210|   134|   0.6380952|
| Kevin Durant   |    643|   390|   0.6065319|
| Stephen Curry  |    563|   304|   0.5399645|
| Klay Thompson  |    640|   329|   0.5140625|
| Draymond Green |    346|   171|   0.4942197|

###### Three Point Field Goal Shooting

| name           |  total|  made|  perc\_made|
|:---------------|------:|-----:|-----------:|
| Klay Thompson  |    580|   246|   0.4241379|
| Stephen Curry  |    687|   280|   0.4075691|
| Kevin Durant   |    272|   105|   0.3860294|
| Andre Iguodala |    161|    58|   0.3602484|
| Draymond Green |    232|    74|   0.3189655|

###### Total Field Goal Shooting

| name           |  total|  made|  perc\_made|
|:---------------|------:|-----:|-----------:|
| Kevin Durant   |    915|   495|   0.5409836|
| Andre Iguodala |    371|   192|   0.5175202|
| Klay Thompson  |   1220|   575|   0.4713115|
| Stephen Curry  |   1250|   584|   0.4672000|
| Draymond Green |    578|   245|   0.4238754|

We get some interesting results once more! The player that attempted the least amount of shots, Andre Iguodala, made the greatest percentage of two point field goal effective percentage. Klay Thompson was fourth best at two point field goal shooting, but best at three point field goal shooting, while Kevin Durant came out on top in the total field goal effective shooting percentage. Draymond Green remained last for all three categories and Stephen Curry, who attempted the most number of shots, was fourth place for total effective field goal shooting percentage. In general, all players made more of their two point field goal shooting than their three point field goal shooting.

### Conclusion

In conclusion, we can see that all Golden State Warrior players that we analyzed were better at two point field goal shooting than three point field goal shooting. This makes more sense, because it is easier to make two point field goal shots as they are closer to the basket. Furthermore, in general more players attempted shots at the two point zone rather than the three point zone. Ultimately, the Golden State Warriors are better at two point shooting! However, this is a general statement for most players. It is hard to say whether their shooting for two or three pointers are good compared to other professional players without more data.
