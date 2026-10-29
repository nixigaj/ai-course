get_shortest_path <- function(start, goal, edges) {
  if (start == goal) return (start)
  queue <- list(start)
  parent <- rep(NA, 40)
  visited <- rep(FALSE, 40)
  visited[start] <- TRUE
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    if (current == goal) break
    neighbours <- unique(c(
      edges[edges[, 1] == current, 2],
      edges[edges[, 2] == current, 1]
    ))
    for (n in neighbours) {
      if (!visited[n]) {
        visited[n] <- TRUE
        parent[n] <- current
        queue <- c(queue, list(n))
      }
    }
  }
  path <- goal
  curr <- goal
  while (!is.na(parent[curr])) {
    curr <- parent[curr]
    path <- c(curr, path)
  }
  return(path)
}
  
myFunction <- function(moveInfo,readings,positions,edges,probs) {
  first_turn <- length(moveInfo$moves) == 0
  mem <- moveInfo$mem 
  # Only do this part the first time
  if(is.null(mem$transition_matrix)) {
    mem$beliefs <- rep(1/40, 40)
    mem$status <- 1
    
    # Create croc's transition matrix
    transition_matrix <- matrix(0, nrow=40, ncol=40)
    for (i in 1:40) {
      destinations <- c(
        edges[edges[, 1] == i, 2],
        edges[edges[, 2] == i, 1],
        i
      )
      transition_matrix[i, destinations] <- 1 / length(destinations)
    }
    mem$transition_matrix <- transition_matrix
  }

  if (first_turn) {
    mem$beliefs <- rep(1/40, 40)
  } else if (is.null(mem$beliefs)) {
    mem$beliefs <- rep(1/40, 40)
  }
  
  # HMM FORWARD STEP (PREDICTION & UPDATE)
  prediction <- if (first_turn) {
    mem$beliefs
    } else {
      as.vector(mem$beliefs %*% mem$transition_matrix)
    }
  
  likelihood <- rep(0, 40)
  for (i in 1:40) {
    sal_mean <- probs$salinity[i, 1]
    sal_sd <- probs$salinity[i, 2]
    
    phos_mean <- probs$phosphate[i, 1]
    phos_sd <- probs$phosphate[i, 2]
    
    nit_mean <- probs$nitrogen[i, 1]
    nit_sd <- probs$nitrogen[i, 2]
    
    prob_sal<- dnorm(readings[1], sal_mean, sal_sd)
    prob_phos<- dnorm(readings[2], phos_mean, phos_sd)
    prob_nit<- dnorm(readings[3], nit_mean, nit_sd)
    
    likelihood[i] <- prob_sal * prob_phos * prob_nit
  }
  unnormalized_beliefs <- prediction * likelihood
  normalized_beliefs <- unnormalized_beliefs / sum(unnormalized_beliefs)
  
  mem$beliefs <- normalized_beliefs
  
 
  croc_location <- NULL
  for (i in 1:2){
    if (!is.na(positions[i]) && positions[i] < 0) {
        croc_location <- abs(positions[i])
        break
    }
  }
  
  if (!is.null(croc_location)) {
        # Tourist got eaten this turn, croc is here
        mem$beliefs <- rep(0,40)
        mem$beliefs[croc_location] <- 1
  } else {
    alive_tourists <- positions[1:2][!is.na(positions[1:2]) & 
                                       positions[1:2] > 0]
    mem$beliefs[alive_tourists] <- 0
    # Normalize beliefs again
    if (sum(mem$beliefs) > 0) {
      mem$beliefs <- mem$beliefs / sum(mem$beliefs)
    } else {
      mem$beliefs <- prediction / sum(prediction)
    }
  }
  
  # Decide the next move
  ranger_pos <- positions[3]
  target <- which.max(mem$beliefs)
  if(length(target) == 0 || is.na(target)) target <-ranger_pos
  
  path <- get_shortest_path(ranger_pos, target, edges)
  
  searched_this_turn <-0
  if (length(path) > 2) {
    # Target is 2 or more steps away.
    # Take 2 steps toward the target
    step1 <- path[2]
    step2 <- path[3]
    if (mem$beliefs[step1] > 0.25) {
      # Stop at first step if confidence is high
      moveInfo$moves <- c(step1, 0)
      searched_this_turn <- step1
    } else {
      moveInfo$moves <- c(step1, step2)
    }
    
  } else if (length(path) == 2) {
    # Target is 1 step away
    # Move to target, then search (0).
    step1 <- path[2]
    moveInfo$moves <- c(step1, 0)
    searched_this_turn <- step1 
  } else {
    # Ranger is standing on the target
    # Search current location
    moveInfo$moves <- c(0, 0)
    searched_this_turn <- ranger_pos
  }
  
  if (searched_this_turn > 0) {
    mem$beliefs[searched_this_turn] <- 0
      
    # Normalize beliefs again
    if (sum(mem$beliefs) > 0) {
      mem$beliefs <- mem$beliefs / sum(mem$beliefs)
    } else {
      mem$beliefs <- rep(1/40, 40)
    }
  }
  
  # Update the mem field before returning
  moveInfo$mem <- mem
  
  return(moveInfo)
}