#Re-writing PrefeR so that it is fast AND doesnt throw as many errors

#Start with the linear case.

#Issues to correct:
# %>% conflicts with dplyr and is a very poor choice. A simple prefer function would suffice
# metrop is one of those "you are lucky if it converges" style functions, but literally nothing is output (within prefeR) to say it hasn't converged (when it in fact usually hasnt). Will start by conversion to adaptive metropolis to ensure at least some sensible convergence rates.
# MCMC starts at place with 0 posterior mass if exp priors used (0), so convergence not guaranteed theoretically.
# Sigma is just fixed not learned, but it could be learned if opened up (not now). As least now changed to reflect prior instead of given nonsense (fixed at 0.1 for no reason in prefeR)
#Remove indifference, as not applicable (probably) for our continuum style DTs
#The Exp() prior is weird in that it's prior mode is 0, but it is meant to be provided for cases where we know the sign of the weight. If there is not sufficient data to overcome the prior mode, we might get stuck with 0 weights. I've seen this happen with prefeR, even if we are stuck in a local mode of the posterior (use of metrop doesnt help). Will create a gamma prior with similar motivation and more flexibility.

#' log-likelihood function of observed preferences (one preference) 
#' @param w the weights we wish to infer
#' @param pref A vector of 2 integers pointing to the rows of the data, with the meaning "User prefers data[pref[1],] to data[pref[2],].
#' @param p the preference elication object
logProbOnePref <- function(w, pref, p){
  d <- as.matrix(p$data[pref[1], ] - p$data[pref[2], ])
  tvar <- t(d) %*% p$Sigma %*% d
  tmean <- w %*% d
  return(pnorm(tmean, 0, sqrt(tvar), log.p = T)[1])
}

#' Calculates the log posterior of the weights given data
#' @param w The weights
#' @param p the preference elication object
#' @return the logposterior
logPosterior <- function(w, p){
  # calculate the prior
  logPrior <- sum(mapply(function(w, prior) prior$f(w), w, p$priors))
  if(is.infinite(logPrior))
    return(-Inf)
  
  # For each preference stated, get the independent log-probability for it, and sum them 
  loglik  <- sum(apply(p$prefs, 1, function(tpref) logProbOnePref(w, tpref, p)))
  return(loglik + logPrior)
}

#' Calculates density of a gamma distribution with given mean and variance (with conversion to alpha/beta)
#' Allows mu to be set negative so that we have a reflected gamma distribution (for when we know weights must be negative, e.g. suppose we have preferences over deaths in a epi context)
#' Note this means the prior is not strictly gamma
#' @param mu Mean of the prior
#' @param sigma Sd of the prior
gamma_prior <- function(mu, sigma){
  alpha <- mu^2/sigma^2
  beta <- sign(mu)*mu/sigma^2
  f <- function(x) dgamma(sign(mu)*x,alpha,beta, log=TRUE)
  class(f) <- c("function", "prior", "gamma")
  sample_fun <- function() -rgamma(1, alpha, beta)
  return(list(f=f, mean=mu, sd=sigma, s_fun = sample_fun))
}

#' A convenience function for generating Normal priors. (From prefeR totally)
#' @examples Normal(0, 1)(1) == dnorm(1, log = TRUE)
#' @param  mu The mean of the normal distribution
#' @param  sigma The standard deviation of the prior
#' @family priors
#' @importFrom stats dnorm
#' @export
#' @return A function yielding the log-PDF at x of a normal distribution with given statistics.
Normal <- function(mu = 0.0, sigma = 1.0){
  f <- function(x) dnorm(x, mu, sigma, log = T)
  class(f) <- c("function", "prior", "Normal")
  sample_fun <- function() rnorm(1,mu, sigma)
  return(list(f=f, mean=mu, sd=sigma, s_fun=sample_fun))
}

#' A function to determine lower bounds on the posterior for use by parallel tempering mcmc only. Function is designed to be conservative with the broad aim to scale an output to [0,1] and for the fitted parameters to roughly be interpretable as weights ([-1,1]) where negative weight indicates a preference for less of a thing (like area)
#' @param p an object of class PrefElciitClass
#' @param index which input (as in which data column) do you want a bound for?
lbfun <- function(p, index){
  if(attr(p$priors[[index]]$f, "class")[3] == "gamma"){
    #either we want the weight strictly positive or negative and, if the former, we return 0.
    if(p$priors[[index]]$mean>0)
      return(0)
    else
      return(-10)
    return(-100/max(p$data[,index]))#E.g. negative weight and max = 300 then, at the lb, the weight is -10, which is an order of magnitude larger than I hope, but encapsulates the possibilities for posterior inference.
  }
  else{
    #Prior is normal so weight can be any sign.
    tlargest <- max(abs(p$data[,index]))
    #return(-100/tlargest)
    return(-10)
  }
}

#' A function to determine upper bounds on the posterior for use by parallel tempering mcmc only. (see lbfun).
#' @param p an object of class PrefElicitClass
#' @param index which input (as in which data column) do you want a bound for?
ubfun <- function(p, index){
  if(attr(p$priors[[index]]$f, "class")[3] == "gamma"){
    #either we want the weight strictly positive or negative and, if the latter, we return 0.
    if(p$priors[[index]]$mean<0)
      return(0)
    else
      return(10)
      #return(100/max(p$data[,index]))#E.g. negative weight and max = 300 then, at the lb, the weight is -10, which is an order of magnitude larger than I hope, but encapsulates the possibilities for posterior inference.
  }
  else{
    #Prior is normal so weight can be any sign.
    tlargest <- max(abs(p$data[,index]))
    #return(100/tlargest)
    return(10)
  }
}

#' An object containing all data necessary for preference elicitation. Adapted from prefeR: Main changes from prefeR include:
#' Removing sigma/Sigma, these come from the prior
#' enabling data appending to allow only data for which preferences have been included to be assessed.
#' Change the way preferences are expressed and stored from list of lists to 2 column matrix (prefer column 1 to column 2)
#' Remove indifference (for now)
#' Add MCMC object that can be used to get more samples, or look at samples.
#' replace weights with posterior_mean
#' @name PrefElicitClass
#' @import methods
#' @field data A matrix or dataframe of data. Need only contain rows that have been compared by a user
#' @field priors A list of functions that give the prior on each variable.
#' @field Sigma weight variance matrix determined on fitting via the prior
#' @field prefs A 2 column matrix of preferences. For each row x[i,], x[i,1] > x[i,2]
#' @field posterior_mean A vector of expected posterior preference weights.
PrefElicitClass <- setRefClass("PrefElicitClass",
                              fields = c("data",
                                         "priors",
                                         "Sigma",
                                         "prefs",
                                         "posterior_mean",
                                         "samples"), 
                              methods = list(
                                initialize = function(...){
                                  
                                  # Provide default values
                                  data    <<- NA
                                  priors  <<- list()
                                  Sigma <<- NA
                                  prefs   <<- c()
                                  posterior_mean <<- NA
                                  samples <<- c()
                                  
                                  # Call super to override any defaults
                                  callSuper(...)
                                }, 
                                show = function(){
                                  "Standard printing function"
                                  cat("Preference elicitation object with:\n")
                                  
                                  # Number of observations
                                  if (is.null(nrow(data))) {
                                    cat("\tNo Data\n")
                                  } else {
                                    cat(paste0("\t", nrow(data), " observations of ", ncol(data), " variables.\n"))
                                  }
                                  
                                  cat("And the following preferences:\n")
                                  # preferences
                                  if (is.null(prefs)) {
                                    cat("\tNo preferences.\n")
                                  } else if (dim(prefs)[1] < 10) {
                                    for (x in 1:nrow(prefs)){
                                      cat(paste0("\t",prefs[x,1], " preferred to ", prefs[x,2], "\n"))
                                    } 
                                  } else {
                                    cat(paste("\t", dim(prefs)[1], "preferences.\n"))
                                  }
                                  
                                  # Return silently
                                  invisible()
                                },
                                data_augment = function(x){
                                  # Adds at least 2 rows to the data, representing new rows shown to the data
                                  if(!(ncol(x)==ncol(data)))
                                    stop("Data passed does not have the same number of columns as our data")
                                  if(!all(colnames(x)==colnames(data)))
                                    stop("Data passed does not have the same column names as our data")
                                  
                                  data <<- rbind(data, x)
                                  invisible()
                                  
                                },
                                addPref = function(x) {
                                  #"Adds a preference in the form of a single two element vector, where element 1 is preferred to element 2"
                                  
                                  # Make sure rows exist
                                  if(!(all(x <= nrow(data)))) 
                                    stop(paste("Only ", nrow(data), " rows in the data."))
                                  
                                  # Undo any previous inference
                                  posterior_mean <<- NA
                                  
                                  prefs <<- rbind(prefs, x)
                                  
                                  # Return silently
                                  invisible()
                                  
                                },
                                update = function(method = "adapt",nbatch=1000) {
                                  "Calls the global `update` function to do the Bayesian inference for the weights"
                                  global_update <- get("update", envir = .GlobalEnv)
                                  posterior_mean <<- global_update(p = .self, method = method,nbatch=nbatch)
                                  return(posterior_mean)
                                }
                              )
)


#' A shortcut to create objects of the class BayesPrefClass.
#' @param data A matrix or dataframe of data. Each column should be a variable, each row an observation.
#' @param priors A list of functions that give the prior on each variable. E.g. see help(Flat)
#' @param  ... Other parameters to pass to the class constructor. 
#' @export
prefObject <- function(data = NA, priors = list(), ...) PrefElicitClass(data = data, priors = priors, ...)

#' A function that estimates the user's underlying utility function. 
#' @examples
#' #p <- prefObject(data = data.frame(c(1,0,1), c(0,1,1), c(1,1,1)),
#'                priors = list(Normal(0, 1), Normal(0.5,2), gamma_prior(-1,0.3)))
#'                p$addPref(c(1, 2))
#'                p$addPref(c(2,3))
#'                p$update(method="adapt")
#'                p$posterior_mean
#' @param p A PrefElicitClass instance.
#' @param method The type of MCMC used for inference: Plan for adaptive MCMC and Parallel tempering. Types are `adapt` and `ptmcmc`
#' @param nbatch If using Monte Carlo estimates, the number of posterior samples (note difference to prefeR). Defaults to 1000. 
#' @param ... other arguments to MCMC method
#' @return Vector of weight representing the posterior mean, and posterior samples (in some form tbd)
#' @importFrom stats optim
#' @import adaptMCMC
#' @import ptmc
#' @export
update <- function(p, method = "adapt", nbatch = 1000, .DEBUG=FALSE, ...){
  # Basic escape if data missing
  if (any(is.na(p$data))){
    if (length(p$data) == 1) {
      stop("No data supplied. Populate the ``data'' field with a matrix/dataframe of your alternatives")
    } else {
      stop("Dataframes containing NA values are not supported---filter or impute them first.")
    }
  }
  
  # Convert to a matrix object for inference
  p$data <- as.matrix(p$data)
  
  # keyword validation
  if(!(method %in% c("adapt","ptmcmc")))
    stop(paste("Unknown method option", method))
  
  if (!(is(p$priors, "list"))) {
    # Then repeat whatever the user had N times
    p$priors <- rep(c(p$priors), ncol(p$data))
  }
  
  # Start by validating our object
  ncol(p$data) == length(p$priors) || stop(paste("Found", length(p$priors), 
                                                 "prior(s) for", 
                                                 ncol(p$data), "dimensional data. Please supply a prior on each column.",sep=""))
  
  # Now that the arguments match, we can set the Sigma field 
  p$Sigma <- diag(unlist(lapply(p$priors, function(e) e$sd^2)))
  
  #IF WE WANT TO START MCMC AT MAP, NEED DERIVATIVES AND nlminb TO DO THIS PROPERLY
  
  #lb <- rep(-Inf, ncol(p$data))
  #ub <- rep(Inf,  ncol(p$data))
  
  # Figure out which priors have a 0 value one one side of 0, and constrain algorithm
  # appropriately 
  #lb[is.infinite(sapply(p$priors, function(f) f(-1.0)))] <- 0
  #ub[is.infinite(sapply(p$priors, function(f) f( 1.0)))] <- 0
  
  
  fun <- function(w) logPosterior(w, p)
  
  #Start at the prior mean for (in lieu of a derivative-based optimisation to start at map)
  prior_mean <- unlist(lapply(p$priors, function(e) e$mean))
  # Run the MCMC via the method
  if(method== "adapt"){
    samples <- MCMC.parallel(p=fun, n=(10*nbatch)+1000, init=prior_mean, n.chain=4, n.cpu=4, adapt=1000, acc.rate = 0.2)
    #samples <- MCMC(p=fun, n=(10*nbatch)+1000, init=prior_mean, adapt=1000, acc.rate = 0.2)
    if(.DEBUG)
      return(samples)
    else{
      tsams <- samples[[1]]$samples[-c(1:1000),] #-1000 removes burnin
      for(i in 2:length(samples)){
        tsams <- rbind(tsams, samples[[i]]$samples[-c(1:1000),])
      }
      tsams <- tsams[which(c(1:nrow(tsams))%%10==0),]#Thin every 10
      posterior_mean <- apply(tsams, MARGIN=2, FUN=mean)
      return(posterior_mean)
    }
  }
  if(method == "ptmcmc"){
    model = list(
      namesOfParameters = colnames(p$data),
      samplePriorDistributions = function(datalist) {
        unlist(lapply(p$priors, function(e) e$s_fun()))
      },
      evaluateLogPrior = function(params, datalist) {
        sum(mapply(function(params, prior) prior$f(params), params, p$priors))
      },
      evaluateLogLikelihood = function(params, covariance, datalist) {
        p = datalist
        sum(apply(p$prefs, 1, function(tpref) logProbOnePref(params, tpref, p)))
      }
    )
    settings <-  list(
      numberChainRuns = 4,
      numberTempChains = 6,
      iterations = 500+10*nbatch/4,
      burninPosterior = 500,
      thin = 10,
      consoleUpdates = 30,
      numberFittedPar = length(prior_mean),
      onAdaptiveCov = TRUE,
      updatesAdaptiveCov = 50,
      burninAdaptiveCov = 500,
      onAdaptiveTemp = TRUE,
      updatesAdaptiveTemp = 100,
      onDebug = FALSE,
      covarInitVal = 1e-12, # make very small if struggling to sample to beginning
      covarInitValAdapt = 1e-8, # make very small if struggling to sample to beginning
      covarMaxVal = .1, # decrease if struggling to sample in the middle
      runParallel = TRUE,
      numberCores = 4,
      lowerParBounds = sapply(1:ncol(p$data), function(k) lbfun(p, k)),
      upperParBounds = sapply(1:ncol(p$data), function(k) ubfun(p, k))
    )
    samples <- ptmc_func(model=model, data=list(prefs=p$prefs, data=p$data, Sigma=p$Sigma), settings=settings)
    if(.DEBUG)
      return(samples)
    tsams <- samples$mcmc[[1]]
    for(i in 2:4){
      tsams <- rbind(tsams, samples$mcmc[[i]])
    }
    posterior_mean = apply(tsams, MARGIN=2, FUN=mean)
    return(posterior_mean)
  }
}


#p$update(method="ptmcmc")

