myFunction <- function(moveInfo,readings,positions,edges,probs) {
  moves <- moveInfo$moves 
  mem <- moveInfo$mem 
  
  # Only do this part the first time
  if(is.null(mem$status) || mem$status == 0) {
    mem$beliefs <- rep(1/40, 40)
    mem$status <- 1
    
    # Create croc's transition matrix
    transition_matrix <- matrix(0, nrow=40, ncol=40)
    for (i in 1:nrow(transition_matrix)) {
      connected_edges <-which(edges[, 1] == i | edges[, 2] == i)
      valid_destinations <- c(
        edges[edges[, 1] == i, 2],
        edges[edges[, 2] == i, 1],
        i
      )
      
      k <- length(valid_destinations)
      
      transition_matrix[i, valid_destinations] <- 1/k
      mem$transition_matrix <- transition_matrix
    }
  }
  
  # HMM FORWARD STEP (PREDICTION & UPDATE)
  prediction <- as.vector(mem$beliefs %*% mem$transition_matrix)
  
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
  
  # INCORPORATE EXTRA EVIDENCE (Backpackers & Searches)
  
  croc_location <- NULL
  for (i in 1:2){
    if (!is.na(positions[i]) && positions[i] < 0) {
        # Tourist got eaten this turn, croc is there
        croc_location <- abs(positions[i])
        break
    }
  }
  
  if (!is.null(croc_location)) {
        mem$beliefs <- rep(0,40)
        mem$beliefs[croc_location] <- 1.0
  } else {
    for (i in 1:2) {
      if (!is.na(positions[i]) && positions[i] > 0) {
        # Tourist alive, croc not there
        mem$beliefs[positions[i]] <- 0.0
      }
    }
    
    # Normalize beliefs again
    mem$beliefs <- mem$beliefs / sum(mem$beliefs)
  }
  
  # DECIDE THE NEXT MOVE
  ranger_pos <- positions[3]
  target <- which.max(mem$beliefs)
  
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
  
  path <- get_shortest_path(ranger_pos, target, edges)
  
  searched_this_turn <-0
  
  if (length(path) > 2) {
    # SCENARIO A: Target is 2 or more steps away.
    # Take 2 movement steps to sprint toward the target faster!
    step1 <- path[2]
    step2 <- path[3]
    moveInfo$moves <- c(as.numeric(step1), as.numeric(step2))
    
  } else if (length(path) == 2) {
    # SCENARIO B: Target is 1 step away (adjacent).
    # Move to target, then search (0).
    step1 <- path[2]
    moveInfo$moves <- c(as.numeric(step1), 0)
    searched_this_turn <- step1 
  } else {
    # SCENARIO C: Ranger is already standing on the target.
    # Search current location.
    moveInfo$moves <- c(0, 0)
    searched_this_turn <- ranger_pos
  }
  
  if (searched_this_turn != 0) {
    mem$beliefs[searched_this_turn] <- 0.0
      
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