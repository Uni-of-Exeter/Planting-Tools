
# Generate unconstrained legal inputs
generate_legal_samples <- function(n, k, legal_non_zero_values, random_or_maximin_lhs) {
  # maximinLHS takes too long when n and k are too large, randomLHS is almost instantaneous
  if (random_or_maximin_lhs == "maximin") {
    values <- lhs::maximinLHS(n, k)
  } else if (random_or_maximin_lhs == "random") {
    values <- lhs::randomLHS(n, k)
  }
  # Turn values between 0 and 1 to legal area values by rounding then multiplying
  values <- continuous_to_categorical(values,
                                      legal_non_zero_values)
  return(values)
}

generate_legal_unique_samples <- function(n, k,
                                          d = 6, # effective dimension for Random Embeddings
                                          legal_non_zero_values,
                                          max_threshold = NULL,
                                          max_attempts = 10,
                                          constrained = FALSE,
                                          previous_values = NULL,
                                          RRembo = FALSE,
                                          RRembo_hyper_parameters = NULL,
                                          RRembo_smart = FALSE,
                                          current_task_id,
                                          global_log_level = LOG_LEVEL) {
  
  if (isTRUE(RRembo) && is.null(RRembo_hyper_parameters)) {
    msg <- "generate_legal_unique_samples() has been called without specifying the argument RRembo_hyper_parameters"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  
  attempts <- 1
  valid_samples <- if (n > 3e5) dplyr::tibble() else data.frame()
  valid_samples_low_dimension <- if (n > 3e5) dplyr::tibble() else data.frame()
  valid_samples_high_dimension <- if (n > 3e5) dplyr::tibble() else data.frame()
  
  constraint_function <- function(row, max_threshold) {
    sum(row) <= max_threshold
  }
  
  # If there are too many data or dimensions, use lhs::randomLHS instead of lhs::maximinLHS
  random_or_maximin_lhs <- if (k <= 15 && n <= 500) "maximin" else "random"
  use_dplyr <- isTRUE(n > 3e5)
  # TODO REMBO with dplyr
  if (isTRUE(RRembo)) use_dplyr <- FALSE
  
  while (nrow(valid_samples) < n && attempts <= max_attempts) {
    if (current_task_id != get_latest_task_id()) {
      return()
    }
    msg <- paste0("Attempt ", attempts, "/", max_attempts, " at generating inputs (", nrow(valid_samples), "/", n, " so far)... ")
    notif(msg, log_level = "debug", global_log_level = global_log_level)
    
    # Keep old rows if they were really random
    if (random_or_maximin_lhs == "random" && nrow(valid_samples) > 0) {
      number_of_rows_left_to_generate <- n - nrow(valid_samples)
    } else {
      number_of_rows_left_to_generate <- n
      valid_samples <- if (isTRUE(use_dplyr)) dplyr::tibble() else data.frame(matrix(nrow = 0, ncol = k))
      if (isTRUE(RRembo)) {
        valid_samples_low_dimension <- if (isTRUE(use_dplyr)) dplyr::tibble() else data.frame(matrix(nrow = 0, ncol = d))
        valid_samples_high_dimension <- if (isTRUE(use_dplyr)) dplyr::tibble() else data.frame(matrix(nrow = 0, ncol = k))
      }
    }
    
    # Generate a maximinLHS or randomLHS sample
    # But if the sample is really random (i.e. if the parameters are not massive), we don't need to make it much larger
    # and therefore it takes much less computing time
    if (random_or_maximin_lhs == "maximin") {
      rows <- 2 * number_of_rows_left_to_generate
    } else if (random_or_maximin_lhs == "random") {
      rows <- 2 * number_of_rows_left_to_generate
    }
    if (isFALSE(constrained)) {
      rows <- 4 * rows
    }
    use_dplyr <- isTRUE(rows > 3e5)
    # TODO REMBO with dplyr
    if (isTRUE(RRembo)) use_dplyr <- FALSE
    
    if (current_task_id != get_latest_task_id()) {
      return()
    }
    if (isTRUE(RRembo)) {
      
      if (isTRUE(RRembo_smart)) {
        msg <- "generate_legal_unique_samples() -> generate_samples_RRembo() ..."
        notif(msg, log_level = "debug", global_log_level = global_log_level)
        samples <- generate_samples_RRembo(d = d,
                                           lower = rep(0, k),
                                           upper = rep(1, k),
                                           legal_non_zero_values = legal_non_zero_values,
                                           RRembo_hyper_parameters = RRembo_hyper_parameters,
                                           global_log_level = global_log_level)
        notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
      } else {
        msg <- "generate_legal_unique_samples() -> generate_samples_RRembo_basic() ..."
        notif(msg, log_level = "debug", global_log_level = global_log_level)
        if (attempts > 1) {
          temp <- lhs::optAugmentLHS(lhs = valid_samples, m = number_of_rows_left_to_generate, mult = 1)
          # Only take the new rows
          temp <- temp[(length(valid_samples) + 1):length(temp), ]
          temp_high_dimension <- RRembo_project_low_dimension_to_high_dimension_basic(DoE_low_dimension = samples, A = A)
          # Turn values between 0 and 1 to legal area values by rounding then multiplying
          temp_high_dimension_categorical <- continuous_to_categorical(values = temp_high_dimension,
                                                                       legal_non_zero_values = legal_non_zero_values)
          samples <- list(sample_low_dimension = temp,
                          sample_high_dimension = temp_high_dimension,
                          sample_high_dimension_categorical = temp_high_dimension_categorical)
        } else {
          samples <- generate_samples_RRembo_basic(d = d,
                                                   D = k,
                                                   legal_non_zero_values = legal_non_zero_values,
                                                   RRembo_hyper_parameters = RRembo_hyper_parameters,
                                                   global_log_level = global_log_level)
        }
        notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
      }
      
      samples_low_dimension <- samples$sample_low_dimension
      samples_high_dimension <- samples$sample_high_dimension
      samples <- samples$sample_high_dimension_categorical
      
    } else {
      
      msg <- "generate_legal_unique_samples() -> generate_legal_samples() ..."
      notif(msg, log_level = "debug", global_log_level = global_log_level)
      samples <- generate_legal_samples(rows, k, legal_non_zero_values, random_or_maximin_lhs)
      notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
      
    }
    if (current_task_id != get_latest_task_id()) {
      return()
    }
    
    # Add samples to valid_samples, and filter on the threshold if isTRUE(constrained)
    if (isTRUE(constrained)) {
      if (is.null(max_threshold)) {
        msg <- "generate_legal_unique_samples was called with constrained=TRUE but no value for max_threshold"
        notif(msg, log_level = "error", global_log_level = global_log_level)
        stop(paste("[ERROR]", msg))
      }
      # Remove rows where the sum is less than max_threshold
      # If we are using maximinLHS, valid_samples is an empty data.frame/tibble
      if (isFALSE(use_dplyr)) {
        indices_to_keep <- apply(samples, 1, constraint_function, max_threshold = max_threshold)
        # There are problems with rbind when the column names are empty and/or different
        colnames(valid_samples) <- colnames(samples) <- 1:k
        valid_samples <- rbind(valid_samples,
                               samples[indices_to_keep, ])
        if (isTRUE(RRembo)) {
          colnames(valid_samples_low_dimension) <- colnames(samples_low_dimension) <- 1:d
          colnames(valid_samples_high_dimension) <- colnames(samples_high_dimension) <- 1:k
          valid_samples_low_dimension <- rbind(valid_samples_low_dimension,
                                               samples_low_dimension[indices_to_keep, ])
          valid_samples_high_dimension <- rbind(valid_samples_high_dimension,
                                                samples_high_dimension[indices_to_keep, ])
        }
      } else {
        # TODO: indices_to_keep with dplyr when RRembo
        # dplyr is faster on very large values of n
        suppressMessages({
          valid_samples <- valid_samples %>%
            as_tibble(.name_repair = "minimal") %>%
            bind_rows(samples %>% as_tibble(.name_repair = "minimal")) %>%
            mutate(row_sum = rowSums(across(everything()))) %>%
            filter(row_sum <= max_threshold) %>%
            dplyr::select(-row_sum)
        })
      }
    } else {
      if (isFALSE(use_dplyr)) {
        valid_samples <- rbind(valid_samples,
                               samples)
        if (isTRUE(RRembo)) {
          valid_samples_low_dimension <- rbind(valid_samples_low_dimension,
                                               samples_low_dimension)
          valid_samples_high_dimension <- rbind(valid_samples_high_dimension,
                                                samples_high_dimension)
        }
      } else {
        # TODO: indices_to_keep with dplyr with RRembo
        # dplyr is faster on very large values of n
        suppressMessages({
          valid_samples <- valid_samples %>%
            as_tibble(.name_repair = "minimal") %>%
            bind_rows(samples %>% as_tibble(.name_repair = "minimal"))
        })
        # if (isTRUE(RRembo)) {
        #   
        # }
      }
    }
    
    # Remove duplicate rows (unlikely to happen, but just in case)
    if (isFALSE(use_dplyr)) {
      indices_to_keep <- !duplicated(valid_samples)
      valid_samples <- valid_samples[indices_to_keep, ]
      if (isTRUE(RRembo)) {
        valid_samples_low_dimension <- valid_samples_low_dimension[indices_to_keep, ]
        valid_samples_high_dimension <- valid_samples_high_dimension[indices_to_keep, ]
      }
    } else {
      valid_samples <- dplyr::distinct(valid_samples)
      if (isTRUE(RRembo)) {
        valid_samples_low_dimension <- dplyr::distinct(valid_samples_low_dimension)
        valid_samples_high_dimension <- dplyr::distinct(valid_samples_high_dimension)
      }
    }
    
    # Remove samples already tested
    # Only do it when needed, this is an expensive operation
    # TODO: work on all samples
    # if (!is.null(previous_values) && nrow(valid_samples) >= n) {
    #   suppressMessages({
    #     valid_samples <- dplyr::anti_join(valid_samples %>% as.data.frame(),
    #                                       previous_values %>% as.data.frame())
    #   })
    # }
    
    attempts <- attempts + 1
  }
  msg <- "... generate_legal_unique_samples() -> loop done"
  notif(msg, log_level = "debug", global_log_level = global_log_level)
  
  # Return the first n valid samples
  valid_samples <- as.matrix(valid_samples)
  nb_rows_to_return <- min(n, nrow(valid_samples))
  if (nrow(valid_samples) < n) {
    warning("[WARNING] The number of rows generated with generate_legal_unique_samples is lower than expected: ",
            nrow(valid_samples), " instead of ", n,
            ". Maximum number of iterations (", attempts, ") reached.")
  }
  # Work even if no valid samples are found
  if (nb_rows_to_return >= 1) {
    valid_samples <- valid_samples[1:nb_rows_to_return, ]
    if (isTRUE(RRembo)) {
      valid_samples_low_dimension <- valid_samples_low_dimension[1:nb_rows_to_return, ]
      valid_samples_high_dimension <- valid_samples_high_dimension[1:nb_rows_to_return, ]
    }
  }
  
  if (isTRUE(RRembo)) {
    return(list(valid_samples_high_dimension_categorical = valid_samples,
                valid_samples_low_dimension = as.matrix(valid_samples_low_dimension),
                valid_samples_high_dimension = as.matrix(valid_samples_high_dimension)))
  } else {
    return(list(valid_samples_high_dimension_categorical = valid_samples))
  }
}

RRembo_defaults <- function(d, D, init,
                            budget = 100, control,
                            global_log_level = LOG_LEVEL) {
  if (is.null(control$Atype)) 
    control$Atype <- "isotropic"
  if (is.null(control$testU)) 
    control$testU <- TRUE
  if (is.null(control$standard)) 
    control$standard <- FALSE
  if (is.null(control$maxitOptA)) 
    control$maxitOptA <- 100
  if (is.null(control$lightreturn)) 
    control$lightreturn <- FALSE
  if (is.null(control$warping)) 
    control$warping <- "Psi"
  if (is.null(control$inneroptim)) 
    control$inneroptim <- "pso"
  if (is.null(control$popsize)) 
    control$popsize <- 80
  if (is.null(control$gen)) 
    control$gen <- 40
  if (is.null(control$designtype)) 
    control$designtype <- "unif"
  if (is.null(control$reverse)) 
    control$reverse <- TRUE
  if (is.null(control$maxf)) 
    control$maxf <- control$popsize * control$gen
  if (is.null(control$tcheckP)) 
    control$tcheckP <- 1e-04
  if (is.null(control$roll)) 
    control$roll <- FALSE
  if (is.null(init$Amat)) {
    msg <- "RREMBO generating data. Creating the random embedding matrix A ..."
    notif(msg, log_level = "debug", global_log_level = global_log_level)
    A <- selectA(d, D, type = control$Atype, control = list(maxit = control$maxitOptA))
    notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  }
  else {
    A <- init$Amat
  }
  if (d == D) {
    A <- diag(D)
    control$bxsize <- 1
    bxsize <- 1
  }
  tA <- t(A)
  if (is.null(control$bxsize)) {
    if (control$standard) {
      bxsize <- sqrt(d)
    } else {
      bxsize <- sqrt(D)
    }
  }
  if (!control$reverse) {
    if (control$warping == "kY") {
      map <- function(y, A) {
        if (is.null(nrow(y))) 
          y <- matrix(y, nrow = 1)
        return(y)
      }
    }
    if (control$warping == "kX") {
      map <- randEmb
    }
    if (control$warping == "Psi") {
      if (control$Atype == "standard") {
        map <- Psi_Y_nonort
        formals(map)$pA <- ginv(t(A) %*% A) %*% t(A)
        formals(map)$invA <- ginv(A)
      } else {
        map <- Psi_Y
      }
    }
    Amat <- NULL
    Aind <- NULL
  } else {
    Amat <- cbind(A, matrix(rep(c(1, 0), times = c(1, D - 
                                                     1)), D, D), matrix(rep(c(-1, 0), times = c(1, D - 
                                                                                                  1)), D, D))
    Aind <- cbind(matrix(c(D, 1:D), D + 1, d), rbind(rep(1, 
                                                         D * 2), c(1:D, 1:D), matrix(0, D - 1, D * 2)))
    if (control$warping == "kY") {
      map <- function(z, A) {
        if (is.null(nrow(z))) 
          z <- matrix(z, nrow = 1)
        return(z)
      }
    }
    if (control$warping == "kX") {
      map <- mapZX
      formals(map)$Amat <- Amat
      formals(map)$Aind <- Aind
    }
    if (control$warping == "Psi") {
      map <- Psi_Z
      formals(map)$Amat <- Amat
      formals(map)$Aind <- Aind
    }
  }
  if (is.null(init$n) && is.null(init$low_dim_design)) {
    n.init <- max(4 * d, round(budget/3))
  } else {
    if (!is.null(init$n)) 
      n.init <- init$n
    if (!is.null(init$low_dim_design)) 
      n.init <- 0
  }
  return(list(d = d,
              D = D,
              A = A,
              tA = tA,
              Amat = Amat,
              Aind = Aind,
              n.init = n.init,
              control = control,
              map = map,
              bxsize = bxsize))
}

RRembo_project_low_dimension_to_high_dimension_basic <- function(DoE_low_dimension, A, global_log_level = LOG_LEVEL) {
  if (ncol(A) != ncol(DoE_low_dimension)) {
    msg <- paste0("In RRembo_project_low_dimension_to_high_dimension_basic(), matrix sizes are not compatible for the product.",
                   "A has dimensions ", paste(dim(A), collapse = "x"),
                  " but t(DoE_low_dimension) has dimensions ", paste(dim(t(DoE_low_dimension)), collapse = "x"))
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  DoE_high_dimension <- t(tcrossprod(A, as.matrix(DoE_low_dimension)))
  return(DoE_high_dimension)
}

RRembo_project_low_dimension_to_high_dimension_zonotope <- function(DoE_low_dimension,
                                                                    A ,
                                                                    Amat,
                                                                    Aind,
                                                                    upper,
                                                                    lower) {
  # if (control$reverse)
  DoE_high_dimension <- ((mapZX(DoE_low_dimension, A, Amat = Amat, Aind = Aind) + 1)/2) %*%
    diag(upper - lower) +
    matrix(lower, nrow = nrow(DoE_low_dimension), ncol = length(lower), byrow = TRUE)
  return(DoE_high_dimension)
}

RRembo_project_low_dimension_to_high_dimension_original <- function(DoE_low_dimension,
                                                                    A,
                                                                    Amat,
                                                                    Aind,
                                                                    upper,
                                                                    lower) {
  # if (!control$reverse)
  DoE_high_dimension <- ((randEmb(DoE_low_dimension, A) + 1)/2) %*%
    diag(upper - lower) +
    matrix(lower, nrow = nrow(DoE_low_dimension), ncol = length(lower), byrow = TRUE)
  return(DoE_high_dimension)
}

# bisection assigns half the values to 0, and half to 1, by finding a cutoff point
continuous_to_categorical <- function(values,
                                      legal_non_zero_values) {
  n <- nrow(values)
  
  solutions <- values
  bisection_cutoff <- median(values)
  
  solutions <- apply(values, 1, function(row, bisection_cutoff, legal_non_zero_values) {
    indices_below_cutoff <- which(row < bisection_cutoff)
    indices_above_cutoff <- which(row >= bisection_cutoff)
    new_row <- row
    new_row[indices_below_cutoff] <- 0
    new_row[indices_above_cutoff] <- legal_non_zero_values[indices_above_cutoff]
    return(new_row)
  }, bisection_cutoff = bisection_cutoff, legal_non_zero_values = legal_non_zero_values)
  solutions <- t(solutions)
  result <- solutions
  
  return(result)
}

generate_samples_RRembo <- function(d, lower, upper, budget = 100,
                                    legal_non_zero_values,
                                    # control = list(Atype = "isotropic",
                                    #                reverse = TRUE,
                                    #                bxsize = NULL,
                                    #                testU = TRUE,
                                    #                standard = FALSE, 
                                    #                maxitOptA = 100,
                                    #                lightreturn = FALSE,
                                    #                warping = "Psi",
                                    #                designtype = "unif",
                                    #                tcheckP = 1e-04,
                                    #                roll = F,
                                    #                inneroptim = "pso",
                                    #                popsize = 80, 
                                    #                gen = 40),
                                    init = NULL,
                                    # init$n, init$Amat
                                    RRembo_hyper_parameters = NULL,
                                    global_log_level = LOG_LEVEL) {
  
  if (is.null(RRembo_hyper_parameters)) {
    msg <- "generate_samples_RRembo() has been called without specifying the argument RRembo_hyper_parameters"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  
  D <- length(lower)
  A <- Amat <- RRembo_hyper_parameters$A
  tA <- RRembo_hyper_parameters$tA
  Amat <- RRembo_hyper_parameters$Amat
  Aind <- RRembo_hyper_parameters$Aind
  n.init <- RRembo_hyper_parameters$n.init
  control <- RRembo_hyper_parameters$control
  bxsize <- RRembo_hyper_parameters$bxsize
  map <- RRembo_hyper_parameters$map
  
  if (control$reverse) {
    if (is.null(init$low_dim_design)) {
      msg <- paste0("RREMBO generating data. Generate low dimension data (", control$designtype, ") ...")
      notif(msg, log_level = "debug", global_log_level = global_log_level)
      DoE_low_dimension <- designZ(n.init, tA, bxsize, type = control$designtype)
      notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
    } else {
      indtest <- testZ(init$low_dim_design, tA)
      if (!all(indtest)) 
        warning("Not all initial low dimensional designs belong to Z.")
      DoE_low_dimension <- init$low_dim_design
    }
    msg <- "RREMBO generating data. Generate high dimension data (smart projection low-dim to high-dim) ..."
    notif(msg, log_level = "debug", global_log_level = global_log_level)
    DoE_high_dimension <- RRembo_project_low_dimension_to_high_dimension_zonotope(DoE_low_dimension = DoE_low_dimension,
                                                                                  A = A,
                                                                                  Amat = Amat,
                                                                                  Aind = Aind,
                                                                                  upper = upper,
                                                                                  lower = lower)
    notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  } else {
    if (is.null(init$low_dim_design)) {
      msg <- "RREMBO generating data. Generate low dimension data ..."
      notif(msg, log_level = "debug", global_log_level = global_log_level)
      DoE_low_dimension <- designU(n.init, A, bxsize, type = control$designtype, 
                                   standard = control$standard)
      notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
    } else {
      DoE_low_dimension <- init$low_dim_design
    }
    msg <- "RREMBO generating data. Generate high dimension data (basic projection low-dim to high-dim) ..."
    notif(msg, log_level = "debug", global_log_level = global_log_level)
    DoE_high_dimension <- RRembo_project_low_dimension_to_high_dimension_original(DoE_low_dimension = DoE_low_dimension,
                                                                                  A = A,
                                                                                  Amat = Amat,
                                                                                  Aind = Aind,
                                                                                  upper = upper,
                                                                                  lower = lower)
    notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  }
  msg <- "RREMBO generating data. Generate low dimension data part 2 (Psi_Y_nonort) ..."
  notif(msg, log_level = "debug", global_log_level = global_log_level)
  DoE_low_dimension <- map(DoE_low_dimension, A)
  notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  
  # Turn values between 0 and 1 to legal area values by rounding then multiplying
  msg <- "RREMBO generating data. Generate high dimension data part 2 (map to categorical values) ..."
  notif(msg, log_level = "debug", global_log_level = global_log_level)
  values <- continuous_to_categorical(values = DoE_high_dimension,
                                      legal_non_zero_values = legal_non_zero_values)
  notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  notif("... generate_samples_RRembo() done", log_level = "debug", global_log_level = global_log_level)
  
  return(list(sample_high_dimension = DoE_high_dimension,
              sample_high_dimension_categorical = values,
              sample_low_dimension = DoE_low_dimension,
              Amat = A))
}

generate_samples_RRembo_basic <- function(d, D, budget = 100,
                                          legal_non_zero_values,
                                          # control = list(Atype = "isotropic",
                                          #                reverse = TRUE,
                                          #                bxsize = NULL,
                                          #                testU = TRUE,
                                          #                standard = FALSE, 
                                          #                maxitOptA = 100,
                                          #                lightreturn = FALSE,
                                          #                warping = "Psi",
                                          #                designtype = "unif",
                                          #                tcheckP = 1e-04,
                                          #                roll = F,
                                          #                inneroptim = "pso",
                                          #                popsize = 80, 
                                          #                gen = 40),
                                          init = NULL,
                                          # init$n, init$Amat
                                          RRembo_hyper_parameters = NULL,
                                          global_log_level = LOG_LEVEL) {
  
  if (is.null(RRembo_hyper_parameters)) {
    msg <- "generate_samples_RRembo_basic() has been called without specifying the argument RRembo_hyper_parameters"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  
  A <- Amat <- RRembo_hyper_parameters$A
  n.init <- RRembo_hyper_parameters$n.init
  
  
  msg <- "RREMBO generating data. Generate data part 1 (randomLHS and map to higher dimension) ..."
  notif(msg, log_level = "debug", global_log_level = global_log_level)
  DoE_low_dimension <- lhs::randomLHS(n.init, d)
  DoE_high_dimension <- RRembo_project_low_dimension_to_high_dimension_basic(DoE_low_dimension = DoE_low_dimension, A = A)
  notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  
  # Turn values between 0 and 1 to legal area values by rounding then multiplying
  
  msg <- "RREMBO generating data. Generate data part 2 (map to categorical values) ..."
  notif(msg, log_level = "debug", global_log_level = global_log_level)
  DoE_high_dimension_categorical <- continuous_to_categorical(values = DoE_high_dimension,
                                                              legal_non_zero_values = legal_non_zero_values)
  notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  notif(paste("generate_samples_RRembo_basic() done"), log_level = "debug", global_log_level = global_log_level)
  
  return(list(sample_high_dimension = DoE_high_dimension,
              sample_high_dimension_categorical = DoE_high_dimension_categorical,
              sample_low_dimension = DoE_low_dimension,
              Amat = A))
}

EI_Rembo <- function(x, model, mval = -10, RRembo_hyper_parameters = NULL,
                     batch_size = 1,
                     coefficient = 0,
                     M = NUMBER_OF_VECCHIA_NEIGHBOURS,
                     type = "expected improvement",
                     minimize_objective_function = TRUE,
                     global_log_level = LOG_LEVEL) {
  
  if (is.null(RRembo_hyper_parameters)) {
    msg <- "EI_Rembo has been called without specifying the argument RRembo_hyper_parameters"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  
  A <- Amat <- RRembo_hyper_parameters$A
  tA <- RRembo_hyper_parameters$tA
  Amat <- RRembo_hyper_parameters$Amat
  Aind <- RRembo_hyper_parameters$Aind
  n.init <- RRembo_hyper_parameters$n.init
  control <- RRembo_hyper_parameters$control
  bxsize <- RRembo_hyper_parameters$bxsize
  map <- RRembo_hyper_parameters$map
  
  D <- nrow(A)
  d <- ncol(A)
  
  if (is.null(nrow(x))) 
    x <- matrix(x, nrow = 1)
  inDomain <- rep(TRUE, nrow(x))
  # if (control$reverse) {
  #   inDomain <- testZ(x, tA)
  # } else {
  #   if (control$testU) {
  #     inDomain <- testU(x, A)
  #   }
  # }
  res <- rep(NA, nrow(x))
  if (any(inDomain)) {
    xtmp <- map(x[inDomain, ], A)
    # Prevention of numerical instability for a new observation
    # tmp <- GPareto::checkPredict(xtmp, list(model), control$tcheckP, 
    #                                distance = "euclidean")
    tmp <- mycheckPredict(xtmp, list(model), control$tcheckP, distance = "euclidean")
    
    res[inDomain[tmp]] <- 0
    if (any(!tmp)) {
      # res[inDomain[!tmp]] <- EI(x = xtmp[!tmp, ], model = model)
      res[inDomain[!tmp]] <- acquisition_function_dgpsi(gp_object = model,
                                                        data_points = xtmp[!tmp, ],
                                                        batch_size = batch_size,
                                                        coefficient = coefficient,
                                                        type = type,
                                                        minimize_objective_function = minimize_objective_function)
    }
  }
  if (any(!inDomain)) 
    res[!inDomain] <- mval * apply(x[!inDomain, , drop = FALSE], 1, distance, x2 = 0)
  return(res)
}

# https://github.com/mbinois/RRembo/blob/f679110d45cc31ca336a61ddf84c8ff4fa738fde/R/warpings.R#L149
distance <- function(x1, x2){
  return(sqrt(sum((x1-x2)^2)))
}

# Replacement of GPareto::checkPredict, for RRembo with dgpsi::gp
mycheckPredict <- function (x, model, threshold = 1e-04, distance = "euclidean") {
  if (is.null(dim(x))) {
    x <- matrix(x, nrow = 1)
  }
  if (is.null(distance)) 
    distance <- "euclidean"
  if (is.null(threshold)) 
    threshold <- 1e-04
  if (distance == "none") 
    return(rep(FALSE, nrow(x)))
  if (distance == "euclidean") {
    mindist <- apply(model[[1]]$data$X, 1, function(row) distance(row, x))
    mindist <- min(mindist)
  }
  # else {
  #   if (distance == "covratio") {
  #     mindist <- Inf
  #     for (i in 1:length(model)) {
  #       if (!is(model[[i]], "fastfun")) {
  #         pred.sd <- sqrt(predict(model[[i]], x)$results$var)
  #         model.sd <- sqrt(predict(model[[i]], model[[i]]$data$X)$results$var)
  #         mindist <- pmin(mindist, pred.sd/model.sd)
  #       }
  #     }
  #   }
  #   else {
  #     mindist <- Inf
  #     for (i in 1:length(model)) {
  #       if (!is(model[[i]], "fastfun")) {
  #         kn_xx <- predict(model[[i]], x)$results$var
  #         kn_yy <- predict(model[[i]], model[[i]]$data$X)$results$var
  #         kxy <- covMat1Mat2(model[[i]]@covariance, # PROBLEM
  #                            x, model[[i]]@X)
  #         kyy <- covMatrix(model[[i]]@covariance, model[[i]]@X)$C
  #         kn_xy <- kxy - kxy %*% chol2inv(model[[i]]@T) %*% 
  #           kyy
  #         mindist <- pmin(mindist, sqrt(pmax(0, kn_xx - 
  #                                              apply(2 * kn_xy - matrix(kn_yy, nrow(x), 
  #                                                                       model[[i]]@n, byrow = T), 1, max))/model[[i]]@covariance@sd2))
  #       }
  #     }
  #   }
  return(mindist < threshold)
}

# Normalization: recommended to apply it on inputs to fit into (D)GPs
normalize_minmax <- function(x) {
  min_value <- min(x)
  max_value <- max(x)
  return((x - min_value) / (max_value - min_value))
}

# Active Subspace Method
dimension_reduction_asm_weights <- function(inputs, outputs, global_log_level = LOG_LEVEL) {
  library(BASS)
  library(concordance)
  library(zipfR)
  source(file.path("functions", "concordance-extra-function.R"))
  
  mymodel <- bass(xx = inputs, y = outputs, verbose = isTRUE(global_log_level != "none"))
  msg <- "Calculating ASM covariance matrix ..."
  notif(msg, log_level = "debug", global_log_level = global_log_level)
  covariance_matrix <- C_bass(mymodel)
  notif(paste(msg, "done"), log_level = "debug", global_log_level = global_log_level)
  
  # Perform eigenvalue decomposition
  eigen_decomp <- eigen(covariance_matrix)
  
  # Sort eigenvalues and eigenvectors
  sorted_indices <- order(eigen_decomp$values, decreasing = TRUE)
  eigenvalues <- eigen_decomp$values[sorted_indices]
  eigenvectors <- eigen_decomp$vectors[, sorted_indices]
  
  #eigenvalues <- eigen(cov_matrix)$values
  sorted_eigenvalues <- sort(eigenvalues, decreasing = TRUE)
  
  cumulative_var <- cumsum(sorted_eigenvalues) / sum(sorted_eigenvalues)
  
  # # Plot the eigenvalues
  # plot(sorted_eigenvalues, type = "b", xlab = "Eigenvalue Index", ylab = "Eigenvalue")
  # plot(cumulative_var, type = "b", xlab = "Eigenvalue Index", ylab = "cumulative_var")
  # abline(v=5,col=2,lwd=2)
  
  # Select the top-k eigenvectors
  k <- which(cumulative_var >= 0.95)[1]
  weights <- eigenvectors[, 1:k]
  
  return(weights)
}
dimension_reduction_asm_generate_new_inputs <- function(inputs, weights) {
  new_inputs <- inputs %*% weights
  
  # Normalize the specified columns
  normalized_new_inputs <- apply(new_inputs, 2, normalize_minmax)
  
  return(normalized_new_inputs)
}

dimension_reduction_pca <- function(inputs,
                                    percentage_variance_explained = NULL,
                                    center = FALSE, scale = FALSE,
                                    global_log_level = LOG_LEVEL,
                                    ...) {
  if (is.null(percentage_variance_explained)) {
    msg <- "In dimension_reduction_pca, specify percentage_variance_explained"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  
  pca_result <- prcomp(inputs, center = center, scale. = scale, ...)
  
  eigenvalues <- (pca_result$sdev)^2
  cumulative_variance_explained <- cumsum(eigenvalues) / sum(eigenvalues)
  number_of_components <- which(cumulative_variance_explained >= percentage_variance_explained)[1]
  
  return(list(pca_object = pca_result, number_of_components = number_of_components))
}
dimension_reduction_pca_generate_new_inputs <- function(pca_object, inputs, number_of_components) {
  return(predict(object = pca_object, newdata = inputs)[, 1:number_of_components])
}

dimension_reduction_svd <- function(inputs,
                                    percentage_variance_explained = NULL,
                                    number_of_components = NULL,
                                    global_log_level = LOG_LEVEL) {
  
  if (is.null(percentage_variance_explained) && is.null(number_of_components)) {
    msg <- "In dimension_reduction_svd, specify either percentage_variance_explained or number_of_components"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  if (!is.null(percentage_variance_explained) && !is.null(number_of_components)) {
    msg <- "In dimension_reduction_svd, specify either percentage_variance_explained or number_of_components, but not both"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  
  svd_result <- svd(inputs)
  
  u <- svd_result$u
  d <- diag(svd_result$d)
  v <- svd_result$v
  
  # If we don't force return a fixed number of components
  if (is.null(number_of_components)) {
    eigenvalues <- svd_result$d^2
    cumulative_variance_explained <- cumsum(eigenvalues) / sum(eigenvalues)
    number_of_components <- which(cumulative_variance_explained >= percentage_variance_explained)[1]
  }
  
  reduced_u <- as.matrix(u[, 1:number_of_components])
  reduced_d <- as.matrix(d[1:number_of_components, 1:number_of_components])
  reduced_v <- as.matrix(v[, 1:number_of_components])
  
  # approximated_inputs <- reduced_u %*% reduced_d %*% t(reduced_v)
  
  # Both are equal
  # reduced_inputs <- reduced_u %*% reduced_d
  # reduced_inputs <- inputs %*% reduced_v
  
  return(reduced_v)
}
dimension_reduction_svd_generate_new_inputs <- function(svd_object, inputs) {
  return(inputs %*% svd_object)
}

# https://www.r-bloggers.com/2022/11/pca-for-categorical-variables-in-r/
# MCA is better than FAMD when inputs are only categorical variables
dimension_reduction_mca <- function(inputs,
                                    percentage_variance_explained = NULL,
                                    number_of_components = NULL,
                                    categories,
                                    ...) {
  
  if (is.null(percentage_variance_explained) && is.null(number_of_components)) {
    msg <- "In dimension_reduction_mca, specify either percentage_variance_explained or number_of_components"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  if (!is.null(percentage_variance_explained) && !is.null(number_of_components)) {
    msg <- "In dimension_reduction_mca, specify either percentage_variance_explained or number_of_components, but not both"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  
  # Factorize the input data
  ## with data.frame
  inputs_factorized <- as.data.frame(matrix(NA, ncol = ncol(inputs), nrow = nrow(inputs)))
  for (j in 1:ncol(inputs)) {
    inputs_factorized[,j] <- factor(inputs[,j],
                                    levels = ordered(categories[, j]),
                                    ordered = TRUE)
  }
  ## with dplyr
  # inputs_factorized <- inputs %>%
  #   as_tibble(.name_repair = "minimal") %>%
  #   mutate(across(everything(), ~ factor(.x,
  #                                        levels = ordered(categories[, as.integer(cur_column())]),
  #                                        ordered = TRUE))) %>%
  #   as.data.frame()
  
  # If we don't force return a fixed number of components
  if (is.null(number_of_components)) {
    number_of_components <- ncol(inputs)
  }
  
  mca_object <- MCA(inputs_factorized, graph = FALSE, ncp = number_of_components, ...)
  
  if (!is.null(percentage_variance_explained)) {
    cumulative_variance_explained <- mca_object$eig[, "cumulative percentage of variance"] / 100
    number_of_components <- which(cumulative_variance_explained >= percentage_variance_explained)[1]
    names(number_of_components) <- NULL
  }
  
  return(list(mca_object = mca_object, number_of_components = number_of_components))
}
dimension_reduction_mca_generate_new_inputs <- function(mca_object, inputs, categories, number_of_components) {
  # MCA can rename the levels of the inputs
  # so we need to change the inputs and match the MCA modification
  # otherwise MCA refuses to predict
  
  # If we don't force return a fixed number of components
  if (is.null(number_of_components)) {
    number_of_components <- ncol(inputs)
  }
  predict_data <- as.data.frame(matrix(NA, ncol = ncol(inputs), nrow = nrow(inputs)))
  mca_data <- mca_object$call$X
  for (j in 1:ncol(predict_data)){
    predict_data[, j] <- factor(inputs[, j],
                                levels = categories[, j],
                                # The line below does not have a typo. Factors in R are weird.
                                labels = levels(mca_data[, j]),
                                ordered = is.ordered(mca_data[, j]))
  }
  result <- predict(object = mca_object, newdata = predict_data)$coord[, 1:number_of_components]
  
  return(result)
}

fastest_design_point_selection_method <- function(gp_model, input_candidates_for_gp, batch_size, workers, parallel) {
  
  # If the inputs have low dimensions, don't waste time, "mice" is good and fast
  n <- nrow(gp_model$data$X)
  k <- ncol(gp_model$data$X)
  
  if (n <= 2000 && k <= 200) return("mice")
  
  get_time <- function(gp_model, input_candidates_for_gp, batch_size, workers, method) {
    design_method <- get(method, envir = asNamespace("dgpsi"))
    temp <- system.time(tryCatch({
      success <- design_method(gp_model, x_cand = input_candidates_for_gp,
                               batch_size = batch_size,
                               workers = workers)
    }, error = function(e) {warning("You can ignore this error:\n", e, "\n")}))["elapsed"]
    if (exists("success")) {
      result <- temp
      rm(success)
    } else {
      result <- Inf
    }
    return(result)
  }
  
  time <- c()
  
  if (isTRUE(parallel)) {
    library(future)
    plan(multisession)
    
    future_pei <- future(get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "pei"), seed = 1)
    future_vigf <- future(get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "vigf"), seed = 1)
    future_mice <- future(get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "mice"), seed = 1)
    future_alm <- future(get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "alm"), seed = 1)
    
    time$pei <- value(future_pei)
    time$vigf <- value(future_vigf)
    time$mice <- value(future_mice)
    time$alm <- value(future_alm)
    
    plan(sequential)
    future:::ClusterRegistry("stop")
    
  } else {
    time <- c()
    time$pei <- get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "pei")
    time$vigf <- get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "vigf")
    time$mice <- get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "mice")
    time$alm <- get_time(gp_model, input_candidates_for_gp, batch_size, workers, method = "alm")
  }
  
  if (all(is.infinite(unlist(time)))) {
    msg <- "All Bayesian optimization batch methods (dgpsi -> pei, vigf, mice, alm) failed"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  return(names(time)[which.min(time)])
}

# Bayesian Optimization over Hybrid Spaces
# https://arxiv.org/abs/2106.04682
diffusion_discrete_kernel <- function(x1, x2, diffusion_coefficients) {
  if (missing(diffusion_coefficients)) {
    diffusion_coefficients <- rep(1, length(x1))
  }
  nominator <- 1 - exp(-2 * diffusion_coefficients)
  # 0 if x1 == x2, 1 if x1 != x2
  exponents <- (x1 != x2) * 1
  nominator <- nominator^exponents
  denominator <- 1 + exp(-2 * diffusion_coefficients)
  
  product <- prod(nominator / denominator)
  return(product)
}

get_diffusion_covariance_matrix <- function(data,
                                            diffusion_coefficients = rep(1, ncol(data)),
                                            nugget = 1e-8) {
  if (missing(diffusion_coefficients)) {
    diffusion_coefficients <- rep(1, ncol(data))
  }
  
  n <- nrow(data)
  p <- ncol(data)
  
  # This is faster than a loop
  dist_matrix <- outer(
    1:n, 1:n,
    Vectorize(function(i, j) diffusion_discrete_kernel(data[i, ], data[j, ], diffusion_coefficients))
  )
  # Nugget ensures that the matrix is positive-definite enough for e.g. a Cholesky decomposition
  # In practice, the minimum eigenvalues are too close to 0 and cause issues
  diag(dist_matrix) <- diag(dist_matrix) + nugget
  
  return(dist_matrix)
}

# NOT USED YET, BUT MAYBE AT SOME POINT LATER
get_vecchia_reduced_diffusion_covariance_matrix <- function(data,
                                                            diffusion_coefficients = rep(1, ncol(data)),
                                                            # m nearest neighbours
                                                            m = 5) {
  if (missing(diffusion_coefficients)) {
    diffusion_coefficients <- rep(1, ncol(data))
  }
  
  n <- nrow(data)
  
  # Find m nearest neighbours
  pairwise_distances <- as.matrix(dist(data))
  # knn_indices has n rows, each row contains the indices of its k nearest neighbours (plus itself)
  mnn_indices <- t(apply(pairwise_distances, 1, order)[1:(k+1), ])
  
  # This is faster than a loop
  vecchia_covariance_matrix <- outer(
    1:n, 1:(m+1),
    Vectorize(function(i, j) {
      jth_nearest_neighbour_of_i <- mnn_indices[i, j]
      diffusion_discrete_kernel(data[i, ], data[jth_nearest_neighbour_of_i, ], diffusion_coefficients)
    })
  )
  
  return(vecchia_covariance_matrix)
}

acquisition_function_dgpsi <- function(data_points,
                                       gp_object,
                                       coefficient = 0,
                                       type = "expected improvement",
                                       M = NUMBER_OF_VECCHIA_NEIGHBOURS,
                                       batch_size = 1,
                                       minimize_objective_function = TRUE,
                                       # Optimizers typically minimize, but we want to maximize this
                                       return_negative = FALSE) {
  
  # Format the data
  if (is.vector(data_points)) {
    gp_data <- gp_object$data$X
    formatted_data_points <- matrix(data_points, ncol = ncol(gp_data))
  } else {
    formatted_data_points <- data_points
  }
  
  # Extract useful variables
  # gp_predictions <- gp_object$emulator_obj$predict(x = formatted_data_points, m = M)
  gp_predictions <- dgpsi:::predict.gp(object = gp_object, x = formatted_data_points, M = M)
  mu <- gp_predictions$results$mean
  sigma <- sqrt(gp_predictions$results$var)
  original_outputs <- gp_predictions$constructor_obj$Y
  
  result <- acquisition_function(gp_predicted_means = mu,
                                 gp_predicted_standard_deviations = sigma,
                                 original_outputs = original_outputs,
                                 coefficient = coefficient,
                                 type = type,
                                 M = M,
                                 batch_size = batch_size,
                                 minimize_objective_function = minimize_objective_function,
                                 # Optimizers typically minimize, but we want to maximize this
                                 return_negative = return_negative)
  
  return(as.vector(result))
}

acquisition_function_gpvecchia <- function(data_points,
                                           gp_object,
                                           coefficient = 0,
                                           type = "expected improvement",
                                           M = NUMBER_OF_VECCHIA_NEIGHBOURS,
                                           batch_size = 1,
                                           minimize_objective_function = TRUE,
                                           # Optimizers typically minimize, but we want to maximize this
                                           return_negative = FALSE) {
  
  # Format the data
  if (is.vector(data_points)) {
    gp_data <- gp_object$data$X
    formatted_data_points <- matrix(data_points, ncol = ncol(gp_data))
  } else {
    formatted_data_points <- data_points
  }
  
  GPvecchia::vecchia_pred(vecchia.est = gp_model,
                          locs.pred = new_candidates_obj_inputs_for_gp)
  mu <- gp_predictions$mean.pred
  sigma <- sqrt(gp_predictions$var.pred)
  original_outputs <- gp_model$z + gp_model$beta.hat
  
  # Use a custom acquisition function and selection method
  result <- acquisition_function(gp_predicted_means = mu,
                                 gp_predicted_standard_deviations = sigma,
                                 original_outputs = original_outputs,
                                 coefficient = coefficient,
                                 type = type,
                                 M = M,
                                 batch_size = batch_size,
                                 minimize_objective_function = minimize_objective_function,
                                 # Optimizers typically minimize, but we want to maximize this
                                 return_negative = return_negative)
  
  return(as.vector(result))
}

acquisition_function <- function(gp_predicted_means,
                                 gp_predicted_standard_deviations,
                                 original_outputs,
                                 coefficient = 0,
                                 type = "expected improvement",
                                 M = NUMBER_OF_VECCHIA_NEIGHBOURS,
                                 batch_size = 1,
                                 minimize_objective_function = TRUE,
                                 # Optimizers typically minimize, but we want to maximize this
                                 return_negative = FALSE) {
  
  y <- original_outputs
  mu <- gp_predicted_means
  sigma <- gp_predicted_standard_deviations
  
  if (type == "expected improvement") {
    if (isTRUE(minimize_objective_function)) {
      ymin <- min(y)
      improvement <- ymin - mu - coefficient
    } else {
      ymax <- max(y)
      improvement <- mu - ymax - coefficient
    }
    Z <- improvement / sigma
    result <- (improvement * pnorm(Z)) + (sigma * dnorm(Z))
    
    # When variance == 0, return 0
    if (0 %in% sigma) {
      indices_of_zero_variance <- which(sigma == 0)
      result[indices_of_zero_variance] <- 0
    }
    
  } else if (type == "probability of improvement") {
    if (isTRUE(minimize_objective_function)) {
      ymin <- min(y)
      improvement <- mu - ymin - coefficient
    } else {
      ymax <- max(y)
      improvement <- mu - ymax - coefficient
    }
    Z <- improvement / sigma
    result <- pnorm(Z)
  }
  
  if (batch_size > 1 && isTRUE(multiple_data_points)) {
    result <- batch_selection(result, batch_size)
  }
  
  if (isTRUE(return_negative)) {
    result <- -result
  }
  
  return(result)
}

# https://dspace.mit.edu/handle/1721.1/128591
batch_selection <- function(acquisition_values, batch_size = 1) {
  if (batch_size > length(acquisition_values)) {
    msg <- "In batch_selection, the batch_size is larger than the number of possible values"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  return(order(acquisition_values, decreasing = TRUE)[1:batch_size])
}

objective_function <- function(inputs, # c(area_vector)
                               area_sum_threshold, # number
                               area_possible_non_zero_values, # vector
                               outcomes_to_minimize_matrix = NULL, # 1 row per parcel, 1 column per outcome (e.g. carbon)
                               outcomes_to_minimize_SD_matrix = NULL, # 1 row per parcel, 1 column per outcome (e.g. carbon)
                               outcomes_to_minimize_sum_threshold_vector = NULL, # vector
                               outcomes_to_maximize_matrix = NULL, # 1 row per parcel, 1 column per outcome (e.g. carbon)
                               outcomes_to_maximize_SD_matrix = NULL, # 1 row per parcel, 1 column per outcome (e.g. carbon)
                               outcomes_to_maximize_sum_threshold_vector = NULL, # vector
                               penalty_coefficient_arg = PENALTY_COEFFICIENT,
                               preference_weight_area = -1, # Minimize -> negative
                               preference_weights_minimize = NULL, # Minimize -> negative
                               preference_weights_maximize = NULL, # Maximize -> positive
                               exploration = FALSE, # vector, 1 value per outcome
                               scale = FALSE,
                               tolvec,
                               alpha = 0.9,
                               multi_objectives = FALSE) {
  
  number_of_locations <- length(inputs)
  
  penalty_coefficient <- penalty_coefficient_arg
  
  # Retrieve variables from vector
  idx <- 1:number_of_locations
  area_vector <- inputs[idx]
  
  # idx <- max(idx) + 1:number_of_locations
  # years_vector <- inputs[idx]
  
  objectives <- c()
  result <- 0
  cantelli_threshold <- - sqrt(alpha / (1 - alpha))
  
  ## Penalties to make the variables binary instead of continuous
  
  # Where Area is zero, ignore
  indices_of_non_zero_area <- which(area_vector != 0)
  # Area
  area_vector <- area_vector[indices_of_non_zero_area]
  area_possible_non_zero_values <- area_possible_non_zero_values[indices_of_non_zero_area]
  # Years
  # years_vector <- years_vector[indices_of_non_zero_area]
  # years_possible_non_zero_values <- years_possible_non_zero_values[indices_of_non_zero_area]
  
  # Other outcomes to maximize + scale (x-min)/(max-min)
  if (is.null(outcomes_to_maximize_matrix) == FALSE) {
    outcomes_to_maximize_matrix <- outcomes_to_maximize_matrix %>%
      dplyr::slice(indices_of_non_zero_area)
    outcomes_to_maximize_SD_matrix <- outcomes_to_maximize_SD_matrix %>%
      dplyr::slice(indices_of_non_zero_area)
    # dplyr::mutate(across(everything(), ~ (.x - min(.x)) / (max(.x) - min(.x))))
  }
  # Other outsomes to minimize + scale (x-min)/(max-min)
  if (is.null(outcomes_to_minimize_matrix) == FALSE) {
    outcomes_to_minimize_matrix <- outcomes_to_minimize_matrix %>%
      dplyr::slice(indices_of_non_zero_area)
    outcomes_to_minimize_SD_matrix <- outcomes_to_minimize_SD_matrix %>%
      dplyr::slice(indices_of_non_zero_area)
    # dplyr::mutate(across(everything(), ~ (.x - min(.x)) / (max(.x) - min(.x))))
  }
  
  # # Add a penalty based on distances to closest acceptable values
  # values <- area_vector
  # possible_values <- area_possible_non_zero_values
  # for (i in seq_along(values)) {
  #   # Distances to 0 and to the possible (max) value
  #   distance_to_zero <- values[i]
  #   distance_to_non_zero <- abs(possible_values[i] - values[i])
  #   
  #   result <- result + 2 * penalty_coefficient_arg * min(distance_to_zero, distance_to_non_zero)
  # }
  
  ## Penalties for deviating from the thresholds
  
  # Area and a penalty to avoid going above the target
  vector_sum <- sum(area_vector)
  threshold <- area_sum_threshold
  penalty <- penalty_coefficient * max(0, vector_sum - threshold)
  # Minimize -> negative
  preference_weight <- - preference_weight_area
  if (exploration == FALSE) {
    objectives[1] <- vector_sum + penalty
    result <- result + preference_weight * vector_sum + penalty
  } else {
    objectives[1] <- penalty
    result <- result + penalty
  }
  
  
  # # Do something similar for other outcomes (minimize, avoid going above the threshold)
  # if (is.null(outcomes_to_minimize_matrix) == FALSE && nrow(outcomes_to_minimize_matrix) > 0) {
  #   for (outcome_idx in 1:ncol(outcomes_to_minimize_matrix)) {
  #     vector_sum <- sum(outcomes_to_minimize_matrix[, outcome_idx])
  #     threshold <- outcomes_to_minimize_sum_threshold_vector[outcome_idx]
  #     penalty <- penalty_coefficient * max(0, vector_sum - threshold)
  #     if (exploration == FALSE) {
  #       objectives <- c(objectives, vector_sum + penalty)
  #       result <- result + vector_sum + penalty
  #     } else {
  #       objectives <- c(objectives, penalty)
  #       result <- result + penalty
  #     }
  #   }
  # }
  # # Do something similar for other outcomes (maximize, avoid going below the threshold)
  # if (is.null(outcomes_to_maximize_matrix) == FALSE && nrow(outcomes_to_maximize_matrix) > 0) {
  #   for (outcome_idx in 1:ncol(outcomes_to_maximize_matrix)) {
  #     vector_sum <- sum(outcomes_to_maximize_matrix[, outcome_idx])
  #     threshold <- outcomes_to_maximize_sum_threshold_vector[outcome_idx]
  #     penalty <- penalty_coefficient * min(0, vector_sum - threshold)
  #     if (exploration == FALSE) {
  #       objectives <- c(objectives, - vector_sum - penalty)
  #       result <- result - vector_sum - penalty
  #     } else {
  #       objectives <- c(objectives, - penalty)
  #       result <- result - penalty
  #     }
  #   }
  # }
  # Do something similar for other outcomes (minimize, avoid going above the threshold)
  if (is.null(outcomes_to_minimize_matrix) == FALSE && nrow(outcomes_to_minimize_matrix) &&
      is.null(outcomes_to_minimize_SD_matrix) == FALSE && nrow(outcomes_to_minimize_SD_matrix)> 0) {
    for (outcome_idx in 1:ncol(outcomes_to_minimize_matrix)) {
      vector_sum <- sum(outcomes_to_minimize_matrix[, outcome_idx])
      vector_sum_sd <- sqrt(sum((outcomes_to_minimize_SD_matrix[, outcome_idx])^2))
      threshold <- outcomes_to_minimize_sum_threshold_vector[outcome_idx]
      # Minimize -> negative
      preference_weight <- - preference_weights_minimize[outcome_idx]
      outcome_name <- colnames(outcomes_to_minimize_matrix)[outcome_idx]
      # minus outcome and threshold, because the standard formula handles the case of maximizing the outcome, but here we minimize
      implausibility <- Impl(Target = - threshold,
                             EY = - vector_sum,
                             SDY = vector_sum_sd,
                             alpha = alpha,
                             # TODO: Fix names of columns, make them usable with FullTable targets and TARGETS
                             tol = tolvec[names(tolvec) != "Area"][outcome_idx])$Im
      penalty <- penalty_coefficient * min(0, cantelli_threshold - implausibility)
      if (exploration == FALSE) {
        objectives <- c(objectives, vector_sum - penalty)
        result <- result + preference_weight * vector_sum - penalty
      } else {
        objectives <- c(objectives, penalty)
        result <- result - penalty
      }
    }
  }
  # Do something similar for other outcomes (maximize, avoid going below the threshold)
  if (is.null(outcomes_to_maximize_matrix) == FALSE && nrow(outcomes_to_maximize_matrix) &&
      is.null(outcomes_to_maximize_SD_matrix) == FALSE && nrow(outcomes_to_maximize_SD_matrix) > 0) {
    for (outcome_idx in 1:ncol(outcomes_to_maximize_matrix)) {
      vector_sum <- sum(outcomes_to_maximize_matrix[, outcome_idx])
      vector_sum_sd <- sqrt(sum((outcomes_to_maximize_SD_matrix[, outcome_idx])^2))
      threshold <- outcomes_to_maximize_sum_threshold_vector[outcome_idx]
      browser()
      preference_weight <- preference_weights_maximize[outcome_idx]
      implausibility <- Impl(Target = threshold,
                             EY = vector_sum,
                             SDY = vector_sum_sd,
                             alpha = alpha,
                             # TODO: Fix names of columns, make them usable with FullTable targets and TARGETS
                             tol = tolvec[names(tolvec) != "Area"][outcome_idx])$Im
      penalty <- penalty_coefficient * min(0, cantelli_threshold - implausibility)
      if (exploration == FALSE) {
        # objectives <- c(objectives, - vector_sum - penalty)
        # result <- result - vector_sum - penalty
        objectives <- c(objectives, - preference_weight * vector_sum - penalty)
        result <- result - preference_weight * vector_sum - penalty
      } else {
        # objectives <- c(objectives, - penalty)
        # result <- result - penalty
        objectives <- c(objectives, - penalty)
        result <- result + penalty
      }
    }
  }
  
  if (!is.finite(result)) {
    browser()
    message("area_vector=", area_vector)
    message("area_sum_threshold=", area_sum_threshold)
    message("area_possible_non_zero_values=", area_possible_non_zero_values)
  }
  
  if (isTRUE(multi_objectives)) {
    return(objectives)
  } else {
    return(result)
  }
}

gp_performance <- function(gp_means,
                           test_inputs,
                           true_outputs,
                           type = "MSE") {
  # Mean Squared Error
  mse <- mean((gp_means - true_outputs)^2)
  if (type == "MSE") return(mse)
  
  # Root Mean Squared Error
  rmse <- sqrt(mse)
  if (type == "RMSE") return(rmse)
  
  # Mean Absolute Error
  mae <- mean(abs(gp_means - true_outputs))
  if (type == "MAE") return(mae)
  
  # R-squared
  total_sum_of_squares <- sum((true_outputs - mean(true_outputs))^2)
  residual_sum_of_squares <- sum((gp_means - true_outputs)^2)
  if (total_sum_of_squares == 0) {
    warning("Total sum of squares is zero, R-squared is not defined.")
    return(NA)
  } else {
    r_squared <- 1 - residual_sum_of_squares / total_sum_of_squares
  }
  if (type == "R-squared") return(r_squared)
}

theme_Publication <- function(base_size = 10) {
  library(grid)
  library(ggthemes)
  theme_foundation(base_size = base_size) +
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(size = rel(1.2)),
          axis.title.y = element_text(angle = 90, vjust = 2),
          axis.title.x = element_text(vjust = - 0.2),
          # axis.text = element_text(size = rel(1.1)),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour = "#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.key.size= unit(0.5, "cm"),
          legend.spacing = unit(0, "cm"),
          legend.title = element_text(),
          # plot.margin = unit(c(5, 5, 5, 5), "mm"),
          strip.background = element_rect(colour = "#f0f0f0", fill = "transparent"),
          strip.text = element_text()
    )
}

# https://ntfy.sh/uoerstudioserver
# https://ntfysenate.uboracle1.freeddns.org/uoerstudioserver
notif <- function(msg, quiet = TRUE, curl_flags = NULL, ntfy_priority = "default", rbind = FALSE, pad_character = "_",
                  ntfy = TRUE, file = TRUE, message_arg = TRUE,
                  log_level = "info", global_log_level = LOG_LEVEL) {
  
  log_level_msg <- toupper(log_level)
  log_level <- switch(
    log_level,
    "none" = 0,
    "error" = 1,
    "warning" = 2,
    "info" = 3,
    "debug" = 4,
  )
  global_log_level <- switch(
    global_log_level,
    "none" = 0,
    "error" = 1,
    "warning" = 2,
    "info" = 3,
    "debug" = 4,
  )

  if (log_level > global_log_level) return()
  
  if (isFALSE(rbind)) {
    msg <- paste0("[", log_level_msg, "] ", msg)
  }
  if (isTRUE(ntfy)) {
    pad_notif_message <- function(msg, pad_character = "_") {
      max_key_width <- max(nchar(rownames(msg)))
      padded_notif_msg <- sapply(1:nrow(msg), function(i, max_key_width, msg) {
        key <- rownames(msg)[i]
        value <- msg[i]
        # result <- sprintf("%-*s  %s", max_key_width, key, value)
        pad_length <- max_key_width - nchar(key)
        formatted_key <- paste0(key, strrep(pad_character, pad_length))
        result <- paste0(formatted_key, pad_character, pad_character, value)
        return(result)
      }, max_key_width = max_key_width, msg = msg)
      padded_notif_msg <- paste(padded_notif_msg, collapse = "\n")
      return(padded_notif_msg)
    }
    
    if (isTRUE(rbind)) {
      # Convert the message to a string without column names like "[,1]"
      if (isTRUE(dim(msg)[2] > 0) && is.null(colnames(msg))) {
        colnames(msg) <- ""
      }
      # # gsub in case msg contains strings that get quoted with \"
      # msg <- paste(gsub('\"', '', capture.output(msg)), collapse = "\n")
      
      msg <- pad_notif_message(msg, pad_character = pad_character)
    }
    
    # Windows curl has problems with my ntfy server. I install the new curl in C:/curl and search for it if it exists
    # Otherwise, I use the official ntfy.sh, but it has a daily free limit that I hit quickly.
    search_directory <- "C:/curl"
    curl_binary <- Sys.which("curl")
    if (dir.exists(search_directory)) {
      # Use list.files to find all instances of curl.exe recursively
      curl_binary <- list.files(path = search_directory, pattern = "curl\\.exe$", full.names = TRUE, recursive = TRUE)
    }
    
    # Construct the curl command
    url <- "https://ntfysenate.uboracle1.freeddns.org/uoerstudioserver"
    command <- paste0(
      curl_binary,
      ' --silent --show-error ',
      # Priority (https://docs.ntfy.sh/publish/?h=priority#message-priority)
      '-H "Priority: ', ntfy_priority, '"',
      curl_flags,
      ' --data "', 
      msg,
      '" "', url, '"'
    )
    
    # Execute the command
    result <- system(command, ignore.stdout = quiet, timeout = 5)
    # If there was a curl error
    if (result != 0) {
      url <- "https://ntfy.sh/uoerstudioserver"
      command <- paste0(
        'curl --silent --show-error ',
        # Priority (https://docs.ntfy.sh/publish/?h=priority#message-priority)
        '-H "Priority: ', ntfy_priority, '"',
        curl_flags,
        ' --data "', 
        msg,
        '" "', url, '"'
      )
      result <- system(command, ignore.stdout = quiet, timeout = 5)
      if (result != 0) {
        warning("[ERROR] notification cannot be sent to ntfy.sh/uoerstudioserver. You should disable notifications to that website by changing the default argument (ntfy) of notif to FALSE.")
      }
    }
    
  }
  
  if (isTRUE(file)) {
    # Log to file, because ntfy has a quota
    FolderSource <- get_foldersource()
    log_filename <- normalizePath(file.path(FolderSource, "log.txt"))
    lockfile_name <- normalizePath(file.path(FolderSource, "log_lockfile"))
    
    if (isFALSE(file.exists(log_filename))) {
      file.create(log_filename)
    }
    
    mylock <- flock::lock(lockfile_name)
    base::write(x = msg, file = log_filename, append = TRUE)
    flock::unlock(mylock)
  }
  
  if (isTRUE(message_arg)) {
    message(msg)
  }
}

get_foldersource <- function() {
  FolderSource <- normalizePath(getwd())
  if (!grepl("/srv/shiny-server", FolderSource) && !grepl("ShinyForestry", FolderSource)) {
    FolderSource <- normalizePath(file.path(FolderSource, "ShinyForestry"))
  }
  return(FolderSource)
}

get_latest_task_id <- function(global_log_level = LOG_LEVEL) {
  FolderSource <- get_foldersource()
  task_id_filename <- normalizePath(file.path(FolderSource, "task_id.txt"))
  lockfile_name <- normalizePath(file.path(FolderSource, "task_id_lockfile"))
  
  if (isFALSE(file.exists(task_id_filename))) {
    msg <- "get_latest_task_id() is trying to read the file task_id.txt but it does not exist"
    notif(msg, log_level = "error", global_log_level = global_log_level)
    stop(paste("[ERROR]", msg))
  }
  mylock <- flock::lock(lockfile_name)
  latest_task_id <- as.integer(readLines(task_id_filename))
  flock::unlock(mylock)
  file.remove(lockfile_name)
  return(latest_task_id)
}

set_latest_task_id <- function(task_id) {
  FolderSource <- get_foldersource()
  task_id_filename <- normalizePath(file.path(FolderSource, "task_id.txt"))
  lockfile_name <- normalizePath(file.path(FolderSource, "task_id_lockfile"))
  
  if (isFALSE(file.exists(task_id_filename))) {
    file.create(task_id_filename)
  }
  mylock <- flock::lock(lockfile_name)
  base::write(x = task_id, file = task_id_filename)
  flock::unlock(mylock)
  file.remove(lockfile_name)
}

bayesian_optimization <- function(
    seed = 1,
    FullTable,
    area_sum_threshold,
    outcomes_to_maximize_sum_threshold_vector = NULL,
    outcomes_to_minimize_sum_threshold_vector = NULL,
    global_log_level = LOG_LEVEL,
    PLOT = FALSE,
    
    # We track the task ID. If it changes from get_latest_task_id(), this gets cancelled.
    current_task_id,
    
    BAYESIAN_OPTIMIZATION_ITERATIONS = 10,
    progressr_object = function(amount = 0, message = "") {},
    BAYESIAN_OPTIMIZATION_BATCH_SIZE = 1,
    PENALTY_COEFFICIENT,
    EXPLORATION = TRUE, # FALSE for tab 1, TRUE for tab 2
    EXPLORATION_COEFFICIENT = 0,
    
    SCALE = FALSE,
    
    preference_weight_area = 1,
    preference_weights_maximize = NULL,
    
    CUSTOM_DESIGN_POINTS_STRATEGIES = c("expected improvement", "probability of improvement"),
    DESIGN_POINTS_STRATEGY = "expected improvement",
    
    CONSTRAINED_INPUTS = TRUE,
    
    RREMBO_CONTROL = list(
      # method to generate low dimensional data in RRembo::designZ ("LHS", "maximin", "unif"). default unif
      designtype = "LHS",
      # if TRUE, use the new mapping from the zonotope, otherwise the original mapping with convex projection. default TRUE
      reverse = FALSE),
    RREMBO_HYPER_PARAMETERS = RRembo_defaults(d = 6, D = nrow(FullTable),
                                              init = list(n = 10 * nrow(FullTable)), budget = 100,
                                              control = list(
                                                # method to generate low dimensional data in RRembo::designZ ("LHS", "maximin", "unif"). default unif
                                                designtype = "LHS",
                                                # if TRUE, use the new mapping from the zonotope, otherwise the original mapping with convex projection. default TRUE
                                                reverse = FALSE),
                                              global_log_level = LOG_LEVEL),
    RREMBO_SMART = FALSE,
    
    # GP
    KERNEL = "matern2.5", # matern2.5 or sexp
    NUMBER_OF_VECCHIA_NEIGHBOURS = 20,
    
    tolvec,
    alpha = alphaLVL
) {
  pb <- progressr_object
  notif(paste0("task ", current_task_id, ", Starting a Bayesian Optimization ..."), global_log_level = global_log_level)
  # if (isFALSE(reticulate::py_module_available("dgpsi"))) {
  #   tryCatch({dgpsi::init_py(verb = VERBOSE)},
  #            error = function(e) {warning(e);stop(reticulate::py_last_error())})
  # }
  # shiny::showNotification("Starting a search for the best strategy ...", duration = 10)
  
  # Setup parameters ----
  set.seed(seed)
  # Number of locations/dimensions
  number_of_locations <- k <- nrow(FullTable)
  # Number of sample points
  # Generate 10 * the dimension of the input
  n <- 10 * k
  begin <- Sys.time()

  area_possible_non_zero_values <- FullTable %>% sf::st_drop_geometry() %>% dplyr::select(area) %>% unlist(use.names = FALSE)
  
  # Outcomes to maximize
  outcomes_to_maximize_matrix <- outcomes_to_maximize_SD_matrix <- c()
  if (!is.null(outcomes_to_maximize_sum_threshold_vector)) {
    for (name in names(outcomes_to_maximize_sum_threshold_vector)) {
      # All can match with Allophyes_oxyacanthae
      if (name == "All") {
        outcomes_to_maximize_matrix <- dplyr::bind_cols(outcomes_to_maximize_matrix,
                                                        FullTable %>%
                                                          sf::st_drop_geometry() %>%
                                                          dplyr::select("BioMean_All"))
        outcomes_to_maximize_SD_matrix <- dplyr::bind_cols(outcomes_to_maximize_SD_matrix,
                                                           FullTable %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::select("BioSD_All"))
        # Carbon is named "Jules" in FullTable, but can take e.g. "JulesMeanY2"
      } else if (name == "Carbon") {
        outcomes_to_maximize_matrix <- dplyr::bind_cols(outcomes_to_maximize_matrix,
                                                        FullTable %>%
                                                          sf::st_drop_geometry() %>%
                                                          dplyr::select("JulesMean"))
        outcomes_to_maximize_SD_matrix <- dplyr::bind_cols(outcomes_to_maximize_SD_matrix,
                                                           FullTable %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::select("JulesSD"))
      } else {
        outcomes_to_maximize_matrix <- dplyr::bind_cols(outcomes_to_maximize_matrix,
                                                        FullTable %>%
                                                          sf::st_drop_geometry() %>%
                                                          dplyr::select(contains(name) & contains("Mean")))
        outcomes_to_maximize_SD_matrix <- dplyr::bind_cols(outcomes_to_maximize_SD_matrix,
                                                           FullTable %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::select(contains(name) & contains("SD")))
      }
    }
  }
  # Outcomes to minimize
  outcomes_to_minimize_matrix <- outcomes_to_minimize_SD_matrix <- c()
  if (!is.null(outcomes_to_minimize_sum_threshold_vector)) {
    for (name in names(outcomes_to_minimize_sum_threshold_vector)) {
      # All can match with Allophyes_oxyacanthae
      if (name == "All") {
        outcomes_to_minimize_matrix <- dplyr::bind_cols(outcomes_to_minimize_matrix,
                                                        FullTable %>%
                                                          sf::st_drop_geometry() %>%
                                                          dplyr::select("BioMean_All"))
        outcomes_to_minimize_SD_matrix <- dplyr::bind_cols(outcomes_to_minimize_matrix,
                                                           FullTable %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::select("BioSD_All"))
        # Carbon is named "Jules" in FullTable, but can take e.g. "JulesMeanY2"
      } else if (name == "Carbon") {
        outcomes_to_minimize_matrix <- dplyr::bind_cols(outcomes_to_minimize_matrix,
                                                        FullTable %>%
                                                          sf::st_drop_geometry() %>%
                                                          dplyr::select("JulesMean"))
        outcomes_to_minimize_SD_matrix <- dplyr::bind_cols(outcomes_to_minimize_matrix,
                                                           FullTable %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::select("JulesSD"))
      } else {
        outcomes_to_minimize_matrix <- dplyr::bind_cols(outcomes_to_minimize_matrix,
                                                        FullTable %>%
                                                          sf::st_drop_geometry() %>%
                                                          dplyr::select(contains(name) & contains("Mean")))
        outcomes_to_minimize_SD_matrix <- dplyr::bind_cols(outcomes_to_minimize_matrix,
                                                           FullTable %>%
                                                             sf::st_drop_geometry() %>%
                                                             dplyr::select(contains(name) & contains("SD")))
      }
    }
  }
  carbon_possible_non_zero_values <- FullTable %>% sf::st_drop_geometry() %>% dplyr::select(JulesMean) %>% unlist(use.names = FALSE)
  
  # Generate inputs + outputs ----
  if (current_task_id != get_latest_task_id()) {
    return(FALSE)
  }
  notif(paste0("task ", current_task_id, ", Generating initial inputs and outputs..."), global_log_level = global_log_level)
  obj_inputs_full_constrained <- generate_legal_unique_samples(10 * 6, k,
                                                               legal_non_zero_values = area_possible_non_zero_values,
                                                               max_threshold = area_sum_threshold,
                                                               constrained = CONSTRAINED_INPUTS,
                                                               RRembo = TRUE,
                                                               RRembo_hyper_parameters = RREMBO_HYPER_PARAMETERS,
                                                               RRembo_smart = RREMBO_SMART,
                                                               current_task_id = current_task_id,
                                                               global_log_level = global_log_level)
  if (current_task_id != get_latest_task_id()) {
    return(FALSE)
  }
  obj_inputs_full_unconstrained <- generate_legal_unique_samples(10 * 6, k,
                                                                 legal_non_zero_values = area_possible_non_zero_values,
                                                                 max_threshold = area_sum_threshold,
                                                                 constrained = FALSE,
                                                                 RRembo = TRUE,
                                                                 RRembo_hyper_parameters = RREMBO_HYPER_PARAMETERS,
                                                                 RRembo_smart = RREMBO_SMART,
                                                                 current_task_id = current_task_id,
                                                                 global_log_level = global_log_level)
  
  if (current_task_id != get_latest_task_id()) {
    return(FALSE)
  }
  
  # Add a strategy that plants everywhere, and one that plants nowhere, to handle the case when the user has some extreme thresholds
  obj_inputs_full_maximum_planting_high_dim_categorical <- area_possible_non_zero_values
  obj_inputs_full_maximum_planting_high_dim <- area_possible_non_zero_values
  obj_inputs_full_maximum_planting_low_dim <- area_possible_non_zero_values
  
  obj_inputs_full <- list(valid_samples_high_dimension_categorical = rbind(obj_inputs_full_constrained$valid_samples_high_dimension_categorical,
                                                                           obj_inputs_full_unconstrained$valid_samples_high_dimension_categorical),
                          valid_samples_low_dimension = rbind(obj_inputs_full_constrained$valid_samples_low_dimension,
                                                              obj_inputs_full_unconstrained$valid_samples_low_dimension),
                          valid_samples_high_dimension = rbind(obj_inputs_full_constrained$valid_samples_high_dimension,
                                                               obj_inputs_full_unconstrained$valid_samples_high_dimension))
  
  obj_inputs <- obj_inputs_full$valid_samples_high_dimension_categorical
  obj_outputs <- apply(obj_inputs, 1, objective_function,
                       area_sum_threshold = area_sum_threshold,
                       area_possible_non_zero_values = area_possible_non_zero_values,
                       outcomes_to_maximize_matrix = outcomes_to_maximize_matrix,
                       outcomes_to_maximize_SD_matrix = outcomes_to_maximize_SD_matrix,
                       outcomes_to_maximize_sum_threshold_vector = outcomes_to_maximize_sum_threshold_vector,
                       exploration = EXPLORATION,
                       penalty_coefficient_arg = PENALTY_COEFFICIENT,
                       preference_weight_area = preference_weight_area,
                       preference_weights_maximize = preference_weights_maximize,
                       scale = SCALE,
                       tolvec = tolvec,
                       alpha = alpha)
  notif(paste0("task ", current_task_id, ", Generating initial inputs and outputs done"), global_log_level = global_log_level)
  if (current_task_id != get_latest_task_id()) {
    return(FALSE)
  }
  
  obj_inputs_for_gp <- obj_inputs_full$valid_samples_low_dimension
  
  # Fitting GP ----
  msg <- paste0("task ", current_task_id, ", Fitting GP...")
  notif(msg, global_log_level = global_log_level)
  gp_model <- dgpsi::gp(obj_inputs_for_gp, obj_outputs,
                        name = KERNEL,
                        vecchia = TRUE,
                        M = NUMBER_OF_VECCHIA_NEIGHBOURS,
                        verb = FALSE)
  notif(paste0(msg, "done"), global_log_level = global_log_level)
  if (current_task_id != get_latest_task_id()) {
    return(FALSE)
  }
  
  time <- data.frame()
  gp_metric <- c()
  
  # Bayesian optimization loop ----
  max_loop_progress_bar <- BAYESIAN_OPTIMIZATION_ITERATIONS * 3
  # pb <- progressor(steps = max_loop_progress_bar, message = "Bayesian optimization")
  pb_amount <- 0
  for (i in 1:BAYESIAN_OPTIMIZATION_ITERATIONS) {
    
    if (current_task_id != get_latest_task_id()) {
      return(FALSE)
    }
    set.seed(i)
    
    ## Candidate set ----
    pb_amount <- pb_amount + 1 / max_loop_progress_bar
    msg <- paste0(pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Generating candidate set...")
    pb(message = msg)
    msg <- paste0("task ", current_task_id, ", ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", msg)
    notif(msg, global_log_level = global_log_level)
    
    if (rstudioapi::isBackgroundJob()) {
      message("[INFO] ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Generating candidate set... ")
    }
    begin_inside <- Sys.time()
    if (current_task_id != get_latest_task_id()) {
      return(FALSE)
    }
    new_candidates_obj_inputs_full_constrained <- generate_legal_unique_samples(50, k,
                                                                                legal_non_zero_values = area_possible_non_zero_values,
                                                                                max_threshold = area_sum_threshold,
                                                                                constrained = CONSTRAINED_INPUTS,
                                                                                RRembo = TRUE,
                                                                                RRembo_hyper_parameters = RREMBO_HYPER_PARAMETERS,
                                                                                RRembo_smart = RREMBO_SMART,
                                                                                current_task_id = current_task_id,
                                                                                global_log_level = global_log_level)
    if (current_task_id != get_latest_task_id()) {
      return(FALSE)
    }
    # if (rstudioapi::isBackgroundJob()) {
    #   message("[INFO] ...", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", pb_amount, "/", max_loop_progress_bar, " Generating candidate set done")
    # }
    time_sample <- Sys.time() - begin_inside
    
    new_candidates_obj_inputs <- new_candidates_obj_inputs_full_constrained$valid_samples_high_dimension_categorical
    new_candidates_obj_inputs_for_gp <- new_candidates_obj_inputs_full_constrained$valid_samples_low_dimension
    
    ## Generate acquisition values ----
    # pb_amount <- pb_amount + 1 / max_loop_progress_bar
    # pb(message = paste0(pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Calculating acquisition values ..."))
    # if (rstudioapi::isBackgroundJob()) {
    #   message("[INFO] ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Calculating acquisition values ...", appendLF = FALSE)
    # }
    
    begin_inside <- Sys.time()
    
    if (isTRUE(RREMBO_SMART) && isTRUE(RREMBO_HYPER_PARAMETERS$control$reverse)) {
      acquisition_values <- EI_Rembo(x = new_candidates_obj_inputs_for_gp,
                                     model = gp_model,
                                     RRembo_hyper_parameters = RREMBO_HYPER_PARAMETERS,
                                     M = NUMBER_OF_VECCHIA_NEIGHBOURS,
                                     type = DESIGN_POINTS_STRATEGY,
                                     minimize_objective_function = TRUE)
    } else {
      # Use a custom acquisition function and selection method
      acquisition_values <- acquisition_function_dgpsi(data_points = new_candidates_obj_inputs_for_gp,
                                                       gp_object = gp_model,
                                                       coefficient = EXPLORATION_COEFFICIENT,
                                                       type = DESIGN_POINTS_STRATEGY,
                                                       M = NUMBER_OF_VECCHIA_NEIGHBOURS)
    }
    new_input_indices <- batch_selection(acquisition_values, batch_size = 1)
    
    # Consider max(acquisition) and min(GP_predicted_means)
    best_initial_acquisition_value_obj_input_for_gp <- new_candidates_obj_inputs_for_gp[which.max(acquisition_values), ]
    
    if (is.vector(new_candidates_obj_inputs_for_gp)) {
      gp_data <- gp_object$data$X
      temp <- matrix(new_candidates_obj_inputs_for_gp, ncol = ncol(gp_data))
    } else {
      temp <- new_candidates_obj_inputs_for_gp
    }
    gp_means <- dgpsi:::predict.gp(object = gp_model, x = temp, M = NUMBER_OF_VECCHIA_NEIGHBOURS)$results$mean
    lowest_gp_mean_obj_input_for_gp <- new_candidates_obj_inputs_for_gp[which.min(gp_means), ]
    
    optimization_inital_values <- rbind(best_initial_acquisition_value_obj_input_for_gp,
                                        lowest_gp_mean_obj_input_for_gp)
    
    time_batch_trick <- Sys.time() - begin_inside
    # if (rstudioapi::isBackgroundJob()) {
    #   message("done")
    # }
    msg <- paste0("task ", current_task_id, ", ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Generating candidate set... done")
    notif(msg, global_log_level = global_log_level)
    
    ## Optimization of acquisition function around its max and the lowest GP mean ----
    pb_amount <- pb_amount + 1 / max_loop_progress_bar
    msg <- paste0(pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Optimizing acquisition function at max(EI) and min(GP_mean) ...")
    pb(message = msg)
    notif(paste0("task ", current_task_id, ", ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", msg), global_log_level = global_log_level)
    
    best_inputs_for_gp <- matrix(NA, nrow = 2, ncol = 6)
    for (i in 1:nrow(optimization_inital_values)) {
      optimization_inital_value <- optimization_inital_values[i, ]
      if (isTRUE(RREMBO_SMART)) {
        if (RREMBO_HYPER_PARAMETERS$control$reverse) {
          boundsEIopt <- rowSums(abs(RREMBO_HYPER_PARAMETERS$tA))
        } else {
          boundsEIopt <- rep(RREMBO_HYPER_PARAMETERS$bxsize, RREMBO_HYPER_PARAMETERS$d)
        }
        spartan <- pmin(boundsEIopt, pmax(-boundsEIopt, optimization_inital_value +
                                            rnorm(RREMBO_HYPER_PARAMETERS$d, sd = 0.05)))
      } else {
        spartan <- optimization_inital_value
      }
      spartan <- matrix(spartan, nrow = 1)
      boundsEIopt <- rep(RREMBO_HYPER_PARAMETERS$bxsize, RREMBO_HYPER_PARAMETERS$d)
      # Optimize low dimensional acquisition function parameters
      optimum <- nlminb(start = spartan,
                        objective = acquisition_function_dgpsi,
                        lower = rep(0, RREMBO_HYPER_PARAMETERS$d),
                        upper = boundsEIopt,
                        # minimize_objective_function = TRUE,
                        gp_object = gp_model,
                        M = NUMBER_OF_VECCHIA_NEIGHBOURS,
                        return_negative = TRUE)
      # gradient = ,
      # hessian = ,
      # control = list(trace = VERBOSE))
      
      if (optimum$convergence != 0) {
        msg <- paste0("task ", current_task_id, ", In B.O. iteration ", i, ", the acquisition function optimization failed to converge with message: ", optimum$message)
        warning(paste("[WARNING]", msg))
        notif(msg, log_level = "warning", global_log_level = global_log_level)
      }
      
      best_inputs_for_gp[i, ] <- optimum$par
    }
    
    if (isTRUE(RREMBO_SMART)) {
      if (isTRUE(RREMBO_HYPER_PARAMETERS$control$reverse)) {
        best_inputs_continuous <- RRembo_project_low_dimension_to_high_dimension_zonotope(DoE_low_dimension = best_inputs_for_gp,
                                                                                          A = RREMBO_HYPER_PARAMETERS$A,
                                                                                          Amat = RREMBO_HYPER_PARAMETERS$Amat,
                                                                                          Aind = RREMBO_HYPER_PARAMETERS$Aind,
                                                                                          upper = rep(1, k),
                                                                                          lower = rep(0, k))
      } else {
        best_inputs_continuous <- RRembo_project_low_dimension_to_high_dimension_original(DoE_low_dimension = best_inputs_for_gp,
                                                                                          A = RREMBO_HYPER_PARAMETERS$A,
                                                                                          Amat = RREMBO_HYPER_PARAMETERS$Amat,
                                                                                          Aind = RREMBO_HYPER_PARAMETERS$Aind,
                                                                                          upper = rep(1, k),
                                                                                          lower = rep(0, k))
      }
    } else {
      best_inputs_continuous <- RRembo_project_low_dimension_to_high_dimension_basic(DoE_low_dimension = best_inputs_for_gp,
                                                                                     A = RREMBO_HYPER_PARAMETERS$A)
    }
    best_inputs <- continuous_to_categorical(values = best_inputs_continuous,
                                             legal_non_zero_values = area_possible_non_zero_values)
    best_inputs <- matrix(best_inputs, ncol = k)
    
    msg <- paste0("task ", current_task_id, ", ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Optimizing acquisition function ... done")
    notif(msg, global_log_level = global_log_level)
    
    ## Objective function on the new inputs ----
    # pb_amount <- pb_amount + 1 / max_loop_progress_bar
    # pb(message = paste0(pb_amount, "/", max_loop_progress_bar, " Computing true outputs ..."))
    # if (rstudioapi::isBackgroundJob()) {
    #   message("[INFO] ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", pb_amount, "/", max_loop_progress_bar, " Computing true outputs ... ", appendLF = FALSE)
    # }
    begin_inside <- Sys.time()
    best_outputs <- apply(best_inputs, 1, objective_function,
                          area_sum_threshold = area_sum_threshold,
                          area_possible_non_zero_values = area_possible_non_zero_values,
                          outcomes_to_maximize_matrix = outcomes_to_maximize_matrix,
                          outcomes_to_maximize_SD_matrix = outcomes_to_maximize_SD_matrix,
                          outcomes_to_maximize_sum_threshold_vector = outcomes_to_maximize_sum_threshold_vector,
                          exploration = EXPLORATION,
                          penalty_coefficient_arg = PENALTY_COEFFICIENT,
                          preference_weight_area = 1,
                          preference_weights_maximize = preference_weights_maximize,
                          scale = SCALE,
                          tolvec = tolvec,
                          alpha = alpha)
    time_obj_function <- Sys.time() - begin_inside
    # if (rstudioapi::isBackgroundJob()) {
    #   message("done")
    # }
    
    ## Diagnostics GP ----
    gp_means <- as.vector(predict(gp_model, best_inputs_for_gp)$results$mean)
    gp_metric[i] <- gp_performance(gp_means = gp_means,
                                   type = "RMSE",
                                   test_inputs = best_inputs_for_gp,
                                   true_outputs = best_outputs)
    
    ## Update samples for GP ----
    pb_amount <- pb_amount + 1 / max_loop_progress_bar
    msg <- paste0(pb_amount * max_loop_progress_bar, "/", max_loop_progress_bar, " Updating GP... ")
    pb(message = msg)
    msg <- paste0("task ", current_task_id, ", ", i, "/", BAYESIAN_OPTIMIZATION_ITERATIONS, " subjob ", msg)
    notif(msg, global_log_level = global_log_level)
    obj_inputs <- rbind(obj_inputs, best_inputs)
    obj_inputs_for_gp <- rbind(obj_inputs_for_gp, best_inputs_for_gp)
    obj_outputs <- c(obj_outputs, best_outputs)
    
    if (current_task_id != get_latest_task_id()) {
      return(FALSE)
    }
    begin_inside <- Sys.time()
    gp_model <- dgpsi::update(gp_model, obj_inputs_for_gp, obj_outputs,
                              verb = isTRUE(global_log_level != "none"),
                              # Refit every 2 loop iterations
                              refit = isTRUE(i %% 2 == 0),
                              # Retrain every 15 loop iterations
                              reset = isTRUE(i %% 15 == 0))
    time_update_gp <- Sys.time() - begin_inside
    notif(paste(msg, "done"), global_log_level = global_log_level)
    
    ## See what takes time ----
    time <- rbind(time,
                  cbind(candidate_sample = if (exists("time_sample")) time_sample else 0,
                        batch_trick = if (exists("time_batch_trick")) time_batch_trick else 0,
                        obj_fun = time_obj_function,
                        update_gp = time_update_gp))
  }
  # Avoid warning message from progressor function
  pb(amount = 0)
  
  end <- Sys.time()
  
  # Update the other (non-area) outcomes
  area_vector <- obj_inputs[which.min(obj_outputs), ]
  indices_zero <- which(area_vector == 0)
  if (isFALSE(is.null(outcomes_to_maximize_matrix))) {
    outcomes_to_maximize_matrix <- outcomes_to_maximize_matrix %>%
      mutate(across(everything(), ~ ifelse(row_number() %in% indices_zero, 0, .)))
    outcomes_to_maximize_SD_matrix <- outcomes_to_maximize_SD_matrix %>%
      mutate(across(everything(), ~ ifelse(row_number() %in% indices_zero, 0, .)))
  }
  if (isFALSE(is.null(outcomes_to_minimize_matrix))) {
    outcomes_to_minimize_matrix <- outcomes_to_minimize_matrix %>%
      mutate(across(everything(), ~ ifelse(row_number() %in% indices_zero, 0, .)))
    outcomes_to_minimize_SD_matrix <- outcomes_to_minimize_SD_matrix %>%
      mutate(across(everything(), ~ ifelse(row_number() %in% indices_zero, 0, .)))
  }
  
  # Gather results ----
  if (isTRUE(EXPLORATION)) {
    
    legal_output_indices <- which(obj_outputs == 0)
    legal_inputs <- obj_inputs[legal_output_indices, ]
    
    area_vectors <- legal_inputs
    
    locations_ignored_idx <- apply(area_vectors, 1, function(row) {which(row == 0)})
    if (nrow(area_vectors) > 0) {
      carbon_vectors <- do.call(rbind, lapply(1:nrow(area_vectors), function(x) carbon_possible_non_zero_values))
    } else {
      carbon_vectors <- data.frame()
    }
    
    if (length(locations_ignored_idx) > 0) {
      for (i in 1:nrow(carbon_vectors)) {
        carbon_vectors[i, locations_ignored_idx[[i]]] <- 0
      }
    }
    
    time_per_solution <- difftime(end, begin, units = "secs")
    time_per_solution <- signif(time_per_solution / length(legal_output_indices))
    unit_time_per_solution <- units(time_per_solution)
    # turn to milliseconds
    if (time_per_solution < 1) {
      time_per_solution <- time_per_solution * 1000
      unit_time_per_solution <- "ms"
    }
    notif_msg <- rbind(
      # "legal_output_indices" = paste0(legal_output_indices, collapse = "; "),
      "# strategies found" = length(legal_output_indices),
      "# strategies searched" = n + n * BAYESIAN_OPTIMIZATION_ITERATIONS,
      "# strategies evaluated" = BAYESIAN_OPTIMIZATION_ITERATIONS * BAYESIAN_OPTIMIZATION_BATCH_SIZE,
      "# locations" = number_of_locations,
      "Σarea summary" = paste0(round(summary(rowSums(area_vectors), 1), 2), collapse = "; "),
      "area max" = area_sum_threshold,
      "Σcarbon summary" = paste0(round(summary(rowSums(carbon_vectors), 1), 2), collapse = "; "),
      "carbon min" = outcomes_to_maximize_sum_threshold_vector[1],
      # "sum area_invalidness" = sum(diff_area_possible, na.rm = TRUE),
      # "obj_value" = paste0(obj_outputs[legal_output_indices], collapse = "; "),
      # "exploration coefficient" = EXPLORATION_COEFFICIENT,
      "# initial design points" = n,
      "smart REMBO" = RREMBO_SMART,
      "reduced REMBO dimension" = ncol(obj_inputs_for_gp),
      "iterations" = BAYESIAN_OPTIMIZATION_ITERATIONS,
      "batch size" = BAYESIAN_OPTIMIZATION_BATCH_SIZE,
      # "batch strategy" = if (isTRUE(RREMBO_OPTIMIZE_ACQUISITION)) "frequentist optimization of acquisition" else "candidate sets",
      "acquisition function" = DESIGN_POINTS_STRATEGY,
      "Vecchia neighbours" = NUMBER_OF_VECCHIA_NEIGHBOURS,
      "Kernel" = KERNEL,
      "penalty" = PENALTY_COEFFICIENT,
      "constrained inputs" = CONSTRAINED_INPUTS,
      "total time" = paste(round(end - begin, 1), units(end - begin)),
      "time per solution" = paste(time_per_solution, unit_time_per_solution)
    )
    # print(notif_msg)
    notif(notif_msg, rbind  = TRUE, global_log_level = global_log_level)
    
    # Sum area / Sum carbon
    if (isTRUE(PLOT)) {
      
      carbon_vectors <- matrix(carbon_possible_non_zero_values, ncol = k, nrow = nrow(obj_inputs), byrow = TRUE)
      carbon_vectors[obj_inputs == 0] <- 0
      
      carbonareasums <- data.frame(sumarea = rowSums(obj_inputs), sumcarbon = rowSums(carbon_vectors)) |> unique()
      carbonareasums |> 
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumarea, y = sumcarbon), size = 0.5) +
        geom_vline(xintercept = area_sum_threshold, color = "red") +
        geom_hline(yintercept = outcomes_to_maximize_sum_threshold_vector[1], color = "red") +
        labs(title = "Carbon sums / Area sums, tab 2",
             x = "Area sums",
             y = "Carbon sums")
      ggsave("tab2-pair-plot.png")
      # Zoom
      valid_indices <- which(carbonareasums$sumarea <= area_sum_threshold &
                               carbonareasums$sumcarbon >= outcomes_to_maximize_sum_threshold_vector)
      carbonareasums <- carbonareasums[valid_indices, ]
      ggplot() +
        theme_Publication() +
        geom_point(data = carbonareasums, aes(x = sumarea, y = sumcarbon), size = 1) +
        geom_vline(xintercept = area_sum_threshold, color = "red") +
        geom_hline(yintercept = outcomes_to_maximize_sum_threshold_vector[1], color = "red") +
        labs(title = "Valid Carbon sums / Area sums, tab 2",
             x = "Area sums",
             y = "Carbon sums")
      ggsave("tab2-valid-pair-plot.png")
      
      # Carbon sum / objective
      data.frame(sumcarbon = rowSums(carbon_vectors), obj = obj_outputs) |>
        unique() |>
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumcarbon, y = obj), size = 1) +
        annotate("rect",
                 xmin = outcomes_to_maximize_sum_threshold_vector[1], xmax = Inf, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Carbon sums, tab 2",
             subtitle = "Green is the set of target-compatible carbon sums",
             x = "Carbon sums",
             y = "Obj value")
      ggsave("tab2-carbon-obj.png")
      # with animation
      all_rows <- data.frame(sumcarbon = rowSums(obj_inputs), obj = obj_outputs) |>
        unique()
      first_rows <- all_rows %>%
        slice_head(n = n) %>%
        mutate(color = "blue", size = 1) %>%
        tidyr::crossing(frame = 1:(BAYESIAN_OPTIMIZATION_ITERATIONS + 1))
      last_rows <- all_rows %>%
        slice_tail(n = BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        mutate(color = "red", size = 4, frame = 1 + 1:BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        rowwise() %>%
        do(data.frame(sumcarbon = .$sumcarbon, obj = .$obj, color = .$color, size = .$size, frame = seq(.$frame, BAYESIAN_OPTIMIZATION_ITERATIONS + 1))) %>%
        ungroup()
      
      a <- bind_rows(first_rows, last_rows) %>%
        unique() %>%
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumcarbon, y = obj, colour = color, size = size)) +
        transition_states(states = frame, wrap = FALSE) +
        annotate("rect",
                 xmin = outcomes_to_maximize_sum_threshold_vector[1], xmax = Inf, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Area sums, tab 2, {closest_state}",
             subtitle = "Green is the set of target-compatible carbon sums",
             x = "Area sums",
             y = "Obj value")
      gganimate::animate(a,
                         nframes = max(a$data$frame),
                         fps = 2,
                         width = 1920,
                         height = 1080,
                         renderer = gifski_renderer()) |>
        gganimate::anim_save(filename = "tab2-carbon-obj.gif")
      
      
      
      # Area sum / objective
      data.frame(sumarea = rowSums(obj_inputs), obj = obj_outputs) |>
        unique() |>
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumarea, y = obj), size = 1) +
        annotate("rect",
                 xmin = -Inf, xmax = area_sum_threshold, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Area sums, tab 2",
             subtitle = "Green is the set of target-compatible area sums",
             x = "Area sums",
             y = "Obj value")
      ggsave("tab2-area-obj.png")
      # with animation
      all_rows <- data.frame(sumarea = rowSums(obj_inputs), obj = obj_outputs) |>
        unique()
      first_rows <- all_rows %>%
        slice_head(n = n) %>%
        mutate(color = "blue", size = 1) %>%
        tidyr::crossing(frame = 1:(BAYESIAN_OPTIMIZATION_ITERATIONS + 1))
      last_rows <- all_rows %>%
        slice_tail(n = BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        mutate(color = "red", size = 4, frame = 1 + 1:BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        rowwise() %>%
        do(data.frame(sumarea = .$sumarea, obj = .$obj, color = .$color, size = .$size, frame = seq(.$frame, BAYESIAN_OPTIMIZATION_ITERATIONS + 1))) %>%
        ungroup()
      
      a <- bind_rows(first_rows, last_rows) %>%
        unique() %>%
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumarea, y = obj, colour = color, size = size)) +
        transition_states(states = frame, wrap = FALSE) +
        annotate("rect",
                 xmin = -Inf, xmax = area_sum_threshold, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Area sums, tab 2, {closest_state}",
             subtitle = "Green is the set of target-compatible carbon sums",
             x = "Area sums",
             y = "Obj value")
      gganimate::animate(a,
                         nframes = max(a$data$frame),
                         fps = 2,
                         width = 1920,
                         height = 1080,
                         renderer = gifski_renderer()) |>
        gganimate::anim_save(filename = "tab2-area-obj.gif")
      
      # Map
      # Target-compatible obj_inputs
      valid_obj_inputs <- obj_inputs[valid_indices, ]
      # How many times each parcel is planted among the planting strategies 
      obj_input_counts <- colSums(valid_obj_inputs != 0)
      # Normalize to (0,1)
      obj_input_counts_normalized <- obj_input_counts / max(obj_input_counts)
      
      color_gradient <- colorRamp(c("lightcyan", "royalblue4"))
      colors_for_map <- rgb(color_gradient(obj_input_counts_normalized) / 255)
      
      # Create labels and colors for the legend
      legend_labels <- sort(unique(obj_input_counts)) # = c(0, 1, ..., highest_frequency)
      legend_colors <- rgb(color_gradient(legend_labels / max(legend_labels)) / 255)
      
      BaseMap2("NoExtent", shconv = shconv, GreyPolygonWidth = 1)$map %>%
        addPolygons(data = shconv$geometry,
                    color = colors_for_map,
                    stroke = FALSE,
                    fillOpacity = 1) %>%
        # Add the legend
        addLegend(position = "bottomright",
                  colors = rev(legend_colors),
                  labels = rev(legend_labels), 
                  title = "Planting frequency",
                  opacity = 1) %>%
        mapview::mapshot(file = "tab2-map.png")
    }
  } else if (isFALSE(EXPLORATION)) {
    area_vector <- obj_inputs[which.min(obj_outputs), ]
    
    if (isTRUE(PLOT)) {
      # Map
      library(leaflet)
      indices_to_plot <- which(area_vector != 0)
      
      map <- BaseMap2("NoExtent", shconv = shconv, GreyPolygonWidth= 1 )$map
      map <- addPolygons(map, data = shconv$geometry[indices_to_plot], color = "blue")
      map
      mapview::mapshot(map, file = "tab1-map.png")
    }
    
    if (isTRUE(PLOT)) {
      carbon_vectors <- matrix(carbon_possible_non_zero_values, ncol = k, nrow = nrow(obj_inputs), byrow = TRUE)
      carbon_vectors[obj_inputs == 0] <- 0
      
      carbonareasums <- data.frame(sumarea = rowSums(obj_inputs), sumcarbon = rowSums(carbon_vectors)) |> unique()
      ggplot() +
        theme_Publication() +
        geom_point(data = carbonareasums, aes(x = sumarea, y = sumcarbon), size = 0.5) +
        geom_vline(xintercept = area_sum_threshold, color = "red") +
        geom_hline(yintercept = outcomes_to_maximize_sum_threshold_vector[1], color = "red") +
        labs(title = "All Carbon sums / Area sums, tab 1",
             x = "Area sums",
             y = "Carbon sums")
      ggsave("tab1-pair-plot.png")
      
      valid_indices <- which(carbonareasums$sumarea <= area_sum_threshold &
                               carbonareasums$sumcarbon >= outcomes_to_maximize_sum_threshold_vector)
      carbonareasums <- carbonareasums[valid_indices, ]
      ggplot() +
        theme_Publication() +
        geom_point(data = carbonareasums, aes(x = sumarea, y = sumcarbon), size = 1) +
        geom_vline(xintercept = area_sum_threshold, color = "red") +
        geom_hline(yintercept = outcomes_to_maximize_sum_threshold_vector[1], color = "red") +
        labs(title = "Valid Carbon sums / Area sums, tab 1",
             x = "Area sums",
             y = "Carbon sums")
      ggsave("tab1-valid-pair-plot.png")
      
      # Carbon sum / objective
      data.frame(sumcarbon = rowSums(carbon_vectors), obj = obj_outputs) |>
        unique() %>%
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumcarbon, y = obj), size = 1) +
        annotate("rect",
                 xmin = outcomes_to_maximize_sum_threshold_vector[1], xmax = Inf, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Carbon sums, tab 1",
             subtitle = "Green is the set of target-compatible carbon sums",
             x = "Carbon sums",
             y = "Obj value")
      ggsave("tab1-carbon-obj.png")
      # with animation
      all_rows <- data.frame(sumcarbon = rowSums(carbon_vectors), obj = obj_outputs) |>
        unique()
      first_rows <- all_rows %>%
        slice_head(n = n) %>%
        mutate(color = "blue", size = 1) %>%
        tidyr::crossing(frame = 1:(BAYESIAN_OPTIMIZATION_ITERATIONS + 1))
      last_rows <- all_rows %>%
        slice_tail(n = BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        mutate(color = "red", size = 4, frame = 1 + 1:BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        rowwise() %>%
        do(data.frame(sumcarbon = .$sumcarbon, obj = .$obj, color = .$color, size = .$size, frame = seq(.$frame, BAYESIAN_OPTIMIZATION_ITERATIONS + 1))) %>%
        ungroup()
      
      a <- bind_rows(first_rows, last_rows) %>%
        unique() %>%
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumcarbon, y = obj, colour = color, size = size)) +
        transition_states(states = frame, wrap = FALSE) +
        annotate("rect",
                 xmin = outcomes_to_maximize_sum_threshold_vector[1], xmax = Inf, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Carbon sums, tab 1, {closest_state}",
             subtitle = "Green is the set of target-compatible carbon sums",
             x = "Carbon sums",
             y = "Obj value")
      gganimate::animate(a,
                         nframes = max(a$data$frame),
                         fps = 2,
                         width = 1920,
                         height = 1080,
                         renderer = gifski_renderer()) |>
        gganimate::anim_save(filename = "tab1-carbon-obj.gif")
      
      
      
      
      # Area sum / objective
      data.frame(sumarea = rowSums(obj_inputs), obj = obj_outputs) |>
        unique() |>
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumarea, y = obj), size = 1) +
        annotate("rect",
                 xmin = -Inf, xmax = area_sum_threshold, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Area sums, tab 1",
             subtitle = "Green is the set of target-compatible area sums",
             x = "Area sums",
             y = "Obj value")
      ggsave("tab1-area-obj.png")
      # with animation
      all_rows <- data.frame(sumarea = rowSums(obj_inputs), obj = obj_outputs) |>
        unique()
      first_rows <- all_rows %>%
        slice_head(n = n) %>%
        mutate(color = "blue", size = 1) %>%
        tidyr::crossing(frame = 1:(BAYESIAN_OPTIMIZATION_ITERATIONS + 1))
      last_rows <- all_rows %>%
        slice_tail(n = BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        mutate(color = "red", size = 4, frame = 1 + 1:BAYESIAN_OPTIMIZATION_ITERATIONS) %>%
        rowwise() %>%
        do(data.frame(sumarea = .$sumarea, obj = .$obj, color = .$color, size = .$size, frame = seq(.$frame, BAYESIAN_OPTIMIZATION_ITERATIONS + 1))) %>%
        ungroup()
      
      a <- bind_rows(first_rows, last_rows) %>%
        unique() %>%
        ggplot() +
        theme_Publication() +
        geom_point(aes(x = sumarea, y = obj, colour = color, size = size)) +
        transition_states(states = frame, wrap = FALSE) +
        annotate("rect",
                 xmin = -Inf, xmax = area_sum_threshold, ymin = -Inf, ymax = Inf,
                 fill = "green", alpha = 0.2) +
        labs(title = "Obj value / Area sums, tab 1, {closest_state}",
             subtitle = "Green is the set of target-compatible carbon sums",
             x = "Area sums",
             y = "Obj value")
      gganimate::animate(a,
                         nframes = max(a$data$frame),
                         fps = 2,
                         width = 1920,
                         height = 1080,
                         renderer = gifski_renderer()) |>
        gganimate::anim_save(filename = "tab1-area-obj.gif")
    }
    
    locations_ignored_idx <- which(area_vector == 0)
    carbon_vector <- carbon_possible_non_zero_values
    carbon_vector[locations_ignored_idx] <- 0
    
    notif_msg1 <- cbind("area" = area_vector,
                        # "area_invalidness" = diff_area_possible,
                        "carbon" = carbon_vector
    )
    rownames(notif_msg1) <- rep("", nrow(notif_msg1))
    # print(notif_msg1)
    # notif(notif_msg1, quiet = FALSE, rbind  = TRUE, global_log_level = global_log_level)
    
    notif_msg2 <- rbind(
      "# strategies searched" = n + n * BAYESIAN_OPTIMIZATION_ITERATIONS,
      "# strategies evaluated" = BAYESIAN_OPTIMIZATION_ITERATIONS * BAYESIAN_OPTIMIZATION_BATCH_SIZE,
      "# locations" = number_of_locations,
      "Σarea" = sum(area_vector),
      "area max" = area_sum_threshold,
      "Σcarbon" = sum(carbon_vector),
      "carbon min" = outcomes_to_maximize_sum_threshold_vector[1],
      # "sum area_invalidness" = sum(diff_area_possible, na.rm = TRUE),
      "obj_value" = min(obj_outputs),
      "preference weight area" = preference_weight_area,
      "preference weight outcomes to maximize" = toString(preference_weights_maximize),
      "# initial design points" = n,
      "smart REMBO" = RREMBO_SMART,
      "reduced REMBO dimension" = ncol(obj_inputs_for_gp),
      "iterations" = BAYESIAN_OPTIMIZATION_ITERATIONS,
      "batch size" = BAYESIAN_OPTIMIZATION_BATCH_SIZE,
      # "batch strategy" = if (isTRUE(RREMBO_OPTIMIZE_ACQUISITION)) "frequentist optimization of acquisition" else "candidate sets",
      "acquisition function" = DESIGN_POINTS_STRATEGY,
      "Vecchia neighbours" = NUMBER_OF_VECCHIA_NEIGHBOURS,
      "Kernel" = KERNEL,
      "penalty" = PENALTY_COEFFICIENT,
      "constrained inputs" = CONSTRAINED_INPUTS,
      "total time" = paste(round(end - begin, 1), units(end - begin))
    )
    # print(notif_msg2)
    notif(notif_msg2, rbind  = TRUE, global_log_level = global_log_level)
  }
  
  # End the function ----
  # Return nothing (NULL) if constraints are not respected
  all_constraints_are_respected <- TRUE
  vector_sum <- sum(area_vector)
  threshold <- area_sum_threshold
  all_constraints_are_respected <- isTRUE(max(0, vector_sum - threshold) > 0)
  
  cantelli_threshold <- - sqrt(alpha / (1 - alpha))
  
  # Do something similar for other outcomes (minimize, avoid going above the threshold)
  if (is.null(outcomes_to_minimize_matrix) == FALSE && nrow(outcomes_to_minimize_matrix) &&
      is.null(outcomes_to_minimize_SD_matrix) == FALSE && nrow(outcomes_to_minimize_SD_matrix)> 0) {
    for (outcome_idx in 1:ncol(outcomes_to_minimize_matrix)) {
      vector_sum <- sum(outcomes_to_minimize_matrix[, outcome_idx])
      vector_sum_sd <- sqrt(sum((outcomes_to_minimize_SD_matrix[, outcome_idx])^2))
      threshold <- outcomes_to_minimize_sum_threshold_vector[outcome_idx]
      preference_weight <- preference_weights_minimize[outcome_idx]
      # minus outcome and threshold, because the standard formula handles the case of maximizing the outcome, but here we minimize
      implausibility <- Impl(Target = - threshold,
                             EY = - vector_sum,
                             SDY = vector_sum_sd,
                             alpha = alpha,
                             # TODO: Fix names of columns, make them usable with FullTable targets and TARGETS
                             tol = tolvec[names(tolvec) != "Area"][outcome_idx])$Im
      if (implausibility > cantelli_threshold) {
        all_constraints_are_respected <- FALSE
      }
    }
  }
  # Do something similar for other outcomes (maximize, avoid going below the threshold)
  if (is.null(outcomes_to_maximize_matrix) == FALSE && nrow(outcomes_to_maximize_matrix) &&
      is.null(outcomes_to_maximize_SD_matrix) == FALSE && nrow(outcomes_to_maximize_SD_matrix) > 0) {
    for (outcome_idx in 1:ncol(outcomes_to_maximize_matrix)) {
      vector_sum <- sum(outcomes_to_maximize_matrix[, outcome_idx])
      vector_sum_sd <- sqrt(sum((outcomes_to_maximize_SD_matrix[, outcome_idx])^2))
      threshold <- outcomes_to_maximize_sum_threshold_vector[outcome_idx]
      preference_weight <- preference_weights_maximize[outcome_idx]
      implausibility <- Impl(Target = threshold,
                             EY = vector_sum,
                             SDY = vector_sum_sd,
                             alpha = alpha,
                             # TODO: Fix names of columns, make them usable with FullTable targets and TARGETS
                             tol = tolvec[names(tolvec) != "Area"][outcome_idx])$Im
      if (implausibility > cantelli_threshold) {
        all_constraints_are_respected <- FALSE
      }
    }
  }
  
  if (isFALSE(all_constraints_are_respected)) {
    return()
  } else {
    return(list(area_vector = obj_inputs[which.min(obj_outputs), ],
                outcomes_to_maximize = outcomes_to_maximize_matrix,
                outcomes_to_maximize_SD = outcomes_to_maximize_SD_matrix,
                outcomes_to_minimize = outcomes_to_minimize_matrix,
                outcomes_to_minimize_SD = outcomes_to_minimize_SD_matrix,
                time = time,
                gp_metrics = gp_metric))
  }
  
}
# server(...){
#   plan(multisession, workers = 5)
#   
#   observeEvent({input$slider}, {
#     task <- ExtendedTask$new(function(progressr_object) {
#       future_promise({
#         for (i in 1:10) {
#           Sys.sleep(5)
#           progressr_object(1/10)
#         }
#       })
#     })
#     
#     progressr::withProgressShiny(
#       message = "Finding strategy",
#       value = 0,
#       expr = {
#         progressr_object <- progressor(steps = 10)
#         task$invoke(progressr_object)
#       }
#     )
#   })
# }
# 
# server(...){
#   plan(multisession, workers = 5)
#   
#   observeEvent({input$slider}, {
#     task <- ExtendedTask$new(function() {
#       progressr::withProgressShiny(
#         message = "Finding strategy",
#         value = 0,
#         expr = {
#           progressr_object <- progressor(steps = 10)
#           future_promise({
#             for (i in 1:10) {
#               Sys.sleep(5)
#               progressr_object(1/10)
#             }
#           })
#         }
#       )
#     })
#     task$invoke()
#   })
# }

