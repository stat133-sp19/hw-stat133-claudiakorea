#Description: checks whether input is a valid probability
#Inputs
# prob: an integer, should be a probability
#Output
# TRUE or error if not a probability
check_prob <- function(prob = NULL){
  if(prob >= 0 & prob <= 1){
    return (TRUE);
  } else{
    stop("\np has to be a number between 0 and 1")
  }
}


#Description: checks whether input is a valid number of trials
#Inputs
# trials: an integer, should be non-negative
#Output
# TRUE or error if not a valid number
check_trials <- function(trials){
  if(min(trials) < 0 ){
    stop("\ninvalid trials value")
  } else{
    return (TRUE)
  }
}


#Description: checks whether success is valid number given number of successes and trials
#Inputs
# success: number of successes, an integer
# trials: number of trials, an integer
#Outputs
# TRUE or error if invalid number
check_success <- function(success, trials){
  check_trials(trials)
  if(max(success) > max(trials) | min(success) < 0){
    stop("\ninvalid success value")
  } else{
    return (TRUE)
  }
}

#Description: returns the mean of a binomial distribution
#Inputs
# trials: the number of trials, an integer
# prob: the probability of success
#Output
# the mean of the distribution ~Binomial(trials, prob)
aux_mean <- function(trials, prob){
  return(trials * prob)
}

#Description: returns the variance of a binomial distribution
#Inputs
# trials: the number of trials, an integer
# prob: the probability of success
#Output
# the variance of the distribution ~Binomial(trials, prob)
aux_variance <- function(trials, prob){
  return(trials*prob*(1 - prob))
}

#Description: returns the mode of a binomial distribution
#Inputs
# trials: the number of trials, an integer
# prob: the probability of success
#Output
# the mode of the distribution ~Binomial(trials, prob)
aux_mode <- function(trials, prob){
  return(round(trials * prob + prob))
}

#Description: returns the skewness of a binomial distribution
#Inputs
# trials: the number of trials, an integer
# prob: the probability of success
#Output
# the skewness of the distribution ~Binomial(trials, prob)
aux_skewness <- function(trials, prob){
  return((1 - 2*prob)/(sqrt(trials * prob * (1 - prob))))
}

#Description: returns the kurtosis of a binomial distribution
#Inputs
# trials: the number of trials, an integer
# prob: the probability of success
#Output
# the kurtosis of the distribution ~Binomial(trials, prob)
aux_kurtosis <- function(trials, prob){
  return((1 - 6*prob * (1 - prob)) / (trials * prob * (1-prob)))
}

#'@title binomial choose
#'@description Calculates the number of combinations in which k successes can occur in n trials
#'@param n trials
#'@param k successes
#'@return n choose k
#'@export
#'@examples
#'
#'bin_choose(n = 5, k = 2)
#'

bin_choose <- function(n, k){
  if(max(k) > max(n)){
    stop("\nk cannot be greater than n")
  }
  numer <- factorial(n)
  denom <- factorial(k) * factorial(n-k)
  return(numer/denom)
}

#'@title binomial probability
#'@description Calculates the probability of the number of successes in the number of trials with the probability of success prob
#'@param success the number of successful trials
#'@praam trials the number of trials
#'@param prob the probability of success
#'@return probability of the successes
#'@export
#'@examples
#'bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#'
#'bin_probability(success = 0:2, trials = 5, prob = 0.5)

bin_probability <- function(success, trials, prob){
  check_prob(prob)
  check_success(success, trials)

  return(bin_choose(trials, success) * prob^success * (1 - prob)^(trials - success))
}


#'@title binomial distribution
#'@description Calculates the binomial distribution ~Binomial(n, trials) with probability prob for each possible number of successes
#'@param trials the number of trials run
#'@param prob the probability of success
#'@return a data frame of number of successes and the corresponding probability of class bindis and data.frame
#'@export
#'@examples
#'bin_distribution(trials = 5, prob = 0.5)

bin_distribution <- function(trials, prob){
  successes <- c(0:trials)
  probs <- bin_probability(successes, trials, prob)
  dist <- data.frame(success = successes, probability = probs)
  class(dist) <- c("bindis", "data.frame")
  return(dist)
}

#'@export
plot.bindis <- function(object){
  barplot((object$probability), xlab = "successes", ylab = "probability" , names.arg = object$success)
}

#'@title binomial cumulative
#'@description Calculates the probability distribution and cumulative probabilites of a binomial distribution given the number of trials and the probability of success
#'@param trials the number of trials run
#'@param prob the probability of success
#'@return a data frame with successes in the first column, probability in the second column, cumulative in the third column of type bincum and data.frame
#'@export
#'@examples
#'bin_cumulative(trials = 5, prob = 0.5)

bin_cumulative <- function(trials, prob){
  successes <- c(0:trials)
  probs <- bin_probability(successes, trials, prob)
  cums <- cumsum(probs)
  dist <- data.frame(success = successes, probability = probs, cumulative = cums)
  class(dist) <- c("bincum", "data.frame")
  return (dist)
}

#'@export
plot.bincum <- function(object){
  plot(object$success, object$cumulative, xlab = "probability", ylab = "successes")

  lines(object$success, object$cumulative, xlab = "probability", ylab = "successes")

}

#'@title binomial variable
#'@description Creates a binomial random variable
#'@param trials the number of trials
#'@param prob the probability of success
#'@return an object of class binvar that contains a list of trials and prob
#'@export
#'@examples
#'bin_variable(10, 0.3)

bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  var <- list(trials = trials, prob = prob)
  class(var) <- "binvar"
  return (var)
}

#'@export
print.binvar <- function(object){
  print("Binomial Variable")
  cat("\n\nParameters\n- number of trials: ")
  cat((object$trials[1]))
  cat("\n- prob of success: ")
  cat((object$prob[1]))
}

#'@export
summary.binvar <- function(object){
  trials <- object$trials
  prob <- object$prob

  sum <- (list(trials = trials, prob = prob, mean = aux_mean(trials, prob), variance = aux_variance(trials, prob), mode = aux_mode(trials, prob), skewness = aux_skewness(trials, prob), kurtosis = aux_kurtosis(trials, prob)))
  class(sum) <- "summary.binvar"
  return(sum)
}

#'@export
print.summary.binvar <- function(object){

  print("Summary Binomial")

  cat("\nParameters")
  cat("\n- number of trials: ")
  cat(object$trials)
  cat("\n- prob of success: ")
  cat(object$prob)
  cat("\n\nMeasures")
  cat("\n-mean    : ")
  cat(object$mean)
  cat("\n-variance: ")
  cat(object$variance)

  cat("\n-mode    : ")
  cat(object$mode)

  cat("\n-skewness: ")
  cat(object$skewness)

  cat("\n-kurtosis: ")
  cat(object$kurtosis)


}

#'@title binomial mean
#'@description Calculates the mean of a binomial distribution given the number of trials and the probability for success
#'@param trials the number of trials
#'@param prob the probability of success
#'@return the mean of the binomial distribution
#'@export
#'@example
#'bin_mean(10, 0.3)

bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  return(aux_mean(trials, prob))

}

#'@title binomial variance
#'@description Calculates the variance of a binomial distribution given the number of trials and the probability for success
#'@param trials the number of trials
#'@param prob the probability of success
#'@return the mean of the binomial distribution
#'@export
#'@example
#'bin_variance(10, 0.3)

bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  return(aux_variance(trials, prob))
}

#'@title binomial mode
#'@description Calculates the mode of a binomial distribution given the number of trials and the probability for success
#'@param trials the number of trials
#'@param prob the probability of success
#'@return the mode of the binomial distribution
#'@export
#'@example
#'bin_mode(10, 0.3)

bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  return(aux_mode(trials, prob))
}


#'@title binomial skewness
#'@description Calculates the skewness of a binomial distribution given the number of trials and the probability for success
#'@param trials the number of trials
#'@param prob the probability of success
#'@return the skewness of the binomial distribution
#'@export
#'@example
#'bin_skewness(10, 0.3)

bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  return(aux_skewness(trials, prob))
}


#'@title binomial kurtosis
#'@description Calculates the kurtosis of a binomial distribution given the number of trials and the probability for success
#'@param trials the number of trials
#'@param prob the probability of success
#'@return the kurtosis of the binomial distribution
#'@export
#'@example
#'bin_kurtosis(10, 0.3)

bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  return(aux_kurtosis(trials, prob))
}
