# File:         demo.r
# Description:  Naive demo-solution given at classroom session
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Fredrik Nilsson
# Modified by:  Marcello Vendruscolo (2021)

# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryMan
# ?testDM

manhattanDistance <- function(x1, y1, x2, y2){
  return(abs(x1 - x2) + abs(y1 - y2))
}

aStarSearch <- function(startX, startY, goalX, goalY, trafficMatrix, gridDim) {
  frontier <- list()
  expandedMatrix = array(data=FALSE, dim=c(gridDim, gridDim))
  
  gCost <- array(data=Inf, dim=c(gridDim, gridDim))
  
  parentMatrix <- array(data=list(NULL), dim=c(gridDim, gridDim))
  
  # Create start node
  hStart <- manhattanDistance(startX, startY, goalX, goalY)
  gCost[startX, startY] <- 0
  
  startNode <- list(x=startX, y=startY, g=0, h=hStart, f=hStart, parent=NULL)
  frontier[[1]] <- startNode
  
  # Main search loop
  while(length(frontier) > 0) {

    # Find node with minimum f-value
    fValues <- sapply(frontier, function(node) node$f)
    currentIndex <- which.min(fValues)
    current <- frontier[[currentIndex]]

    # Check if goal reached
    if (current$x == goalX && current$y == goalY){
      return(reconstructPath(parentMatrix, current))
    }
    
    # Mark as expanded
    expandedMatrix[current$x, current$y] <- TRUE
    
    # Remove from frontier
    frontier <- frontier[-currentIndex]
    
    # Explore neighbours
    neighbours <- getNeighbours(current, gridDim)
    for (neighbour in neighbours){
      neighbourX <- neighbour$x
      neighbourY <- neighbour$y
      
      # Skip if already expanded
      if (expandedMatrix[neighbourX, neighbourY]) next
      
      moveCost <- getTrafficCost(trafficMatrix, current, neighbour)
      gCostNeighbour <- gCost[current$x, current$y] + moveCost
      
      # Update g_cost if better path found to neighbour
      if ( is.finite(gCostNeighbour) && (gCost[neighbourX, neighbourY] == Inf || gCostNeighbour < gCost[neighbourX, neighbourY] )){
        gCost[neighbourX, neighbourY] <- gCostNeighbour
        h <- manhattanDistance(neighbourX, neighbourY, goalX, goalY)
        
        parentMatrix[neighbourX, neighbourY][[1]] <- c(current$x, current$y)
        
        # Check if neighbour is already in the frontier
        if (length(frontier) > 0){
          existingIndex <- which(sapply(frontier, function(n) n$x == neighbourX && n$y == neighbourY))
        } else {
          existingIndex <- integer(0)
        }
        
        if ( length(existingIndex) > 0 && existingIndex[1] > 0 ) {
          # Update existing node in frontier
          frontier[[existingIndex[1]]]$g <- gCostNeighbour
          frontier[[existingIndex[1]]]$h <- h
          frontier[[existingIndex[1]]]$f <- gCostNeighbour + h
          frontier[[existingIndex[1]]]$parent <- c(current$x, current$y)
        } else {
          # Add nighbour to frontier
          newNode <- list(x=neighbourX, y=neighbourY, g=gCostNeighbour, h=h, f=gCostNeighbour+h, parent=c(current$x, current$y))
          frontier[[length(frontier) + 1]] <- newNode
        }
      }
    }
  }
  return(NULL)
}

reconstructPath <- function(parentMatrix, endPosition){
  path <- list()
  currentX <-endPosition$x
  currentY <-endPosition$y
  
  while (TRUE){
    parentCoordinates <- parentMatrix[currentX, currentY][[1]]
    if(is.null(parentCoordinates)) break
    
    path <- c(path, list(c(currentX, currentY)))
    currentX <- parentCoordinates[1]
    currentY <- parentCoordinates[2]
  }
  path <- rev(path)
  return (path)
}

getNeighbours <- function(position, gridDim){
  neighbours <- list()
  if (position$x > 1){
   neighbours[[length(neighbours) + 1]] <- list(x=position$x-1, y=position$y)     
  }
  
  if (position$x < gridDim){
   neighbours[[length(neighbours) + 1]] <- list(x=position$x+1, y=position$y)     
  }
  
  if (position$y > 1){
   neighbours[[length(neighbours) + 1]] <- list(x=position$x, y=position$y-1)     
  }
  
  if (position$y < gridDim){
   neighbours[[length(neighbours) + 1]] <- list(x=position$x, y=position$y+1)     
  }
  return (neighbours)
}

getTrafficCost <- function(trafficMatrix, from, to){
 if (to$y > from$y){ # Moving up
    return (trafficMatrix$vroads[from$x, from$y])   
 } else if (to$y < from$y){ # Moving down
    return (trafficMatrix$vroads[from$x, to$y])   
 } else if (to$x > from$x){ # Moving right
    return (trafficMatrix$hroads[from$x, from$y])   
 } else if (to$x < from$x){ # Moving left
    return (trafficMatrix$hroads[to$x, to$y])   
 } else { # Stay
    return (1) 
 }
}

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
    goalPosition <- nextPickup(trafficMatrix,
                                   carInfo,
                                   packageMatrix)
  } else {
    goalPosition <- packageMatrix[carInfo$load, c(3,4)]
  }

  # Run A*
  path = aStarSearch(
    startX = carInfo$x,
    startY = carInfo$y,
    goalX = goalPosition[1],
    goalY = goalPosition[2],
    trafficMatrix = trafficMatrix,
    gridDim = 10
  )
  
  # How do we get there?
  
  if (!is.null(path) && length(path) > 0){
    nextStep <- path[[1]]
    if (nextStep[1] > carInfo$x) carInfo$nextMove <- 6 # Right
    else if (nextStep[1] < carInfo$x) carInfo$nextMove <- 4 # Left
    else if (nextStep[2] > carInfo$y) carInfo$nextMove <- 8 # Up
    else if (nextStep[2] < carInfo$y) carInfo$nextMove <- 2 # Down
    else carInfo$nextMove <- 5 # Stand still
  }else{
    carInfo$nextMove <- 5
  }
  
  return(carInfo)
}

# Find the next pickup based on the best package order
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  result <- findBestPackageOrder(carInfo, packageMatrix)

  if(is.null(result)) {
    return(c(carInfo$x, carInfo$y))
  }

  nextPackage <- result$order[1]

  return(packageMatrix[nextPackage, c(1,2)])
}

generatePermutations <- function(x) {
  result <- matrix(x[1], nrow = 1)

  if (length(x) == 1) {
    return(result)
  }

  for (i in 2:length(x)) {
    newResult <- NULL

    for (row in 1:nrow(result)) {
      current <- result[row, ]

      for (pos in 0:length(current)) {
        newPermutation <- append(
          current,
          x[i],
          after = pos
        )

        newResult <- rbind(
          newResult,
          newPermutation
        )

      }
    }
    result <- newResult
  }
  return(result)
}

findBestPackageOrder <- function(carInfo, packageMatrix) {
  availablePackages <- which(packageMatrix[, 5] == 0)
  if (length(availablePackages) == 0) {
    return(NULL)
  }

  permutations <- generatePermutations(availablePackages)

  bestCost <- inf
  bestOrder <- NULL

  for (i in 1:nrow(permutations)) {
    order <- permutations[i, ]

    currentX <- carInfo$x
    currentY <- carInfo$y

    totalCost <- 0

    for (packageId in order) {
      pickupX <- packageMatrix[packageId, 1]
      pickupY <- packageMatrix[packageId, 2]

      deliveryX <- packageMatrix[packageId, 3]
      deliveryY <- packageMatrix[packageId, 4]

      #Cost to Pickup
      totalCost <- totalCost + manhattanDistance(currentX, currentY, pickupX, pickupY)

      #Cost to Deliver
      totalCost <- totalCost + manhattanDistance(pickupX, pickupY, deliveryX, deliveryY)

      currentX <- deliveryX
      currentY <- deliveryY
    }
    if ( totalCost < bestCost) {
      bestCost <- totalCost
      bestOrder <- order
    }

  }
  return(list(
    cost = bestCost,
    order = bestOrder
  ))



}