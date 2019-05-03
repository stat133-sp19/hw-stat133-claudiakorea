# Stat 133, Spring 2019, Binomial Package

Workout03 Binomial Package

- Name: Claudia Korea
- Github username: claudiakorea
- Email: ckor [at] berkeley.edu
- Lab section: 103
- GSI: Yulun Wu

-----

## Overview
`binomial` is a R package that provides functions to calculate with the Binomial distribution

 -`bin_choose(n, k)` calculates the number of combinations in which k successes can occur in n trials
 - `bin_probability(success, trials, prob)` calculates the probability of successes in trials, given the probability of a successful trial
 - `bin_distribution(trials, prob)` calculates the Binomial distribution given the number of trials and the probability for success in a trial
 - `bin_cumulative(trials, prob)` does the same as `bin_distribution(trials, prob)` but also calculates the cumulative probabilities
 - `bin_variable(trials, prob)` calculates a random binomial variable given the number of trials and probability of success
 
