library(tidyverse)
grid_size <- 8
board <- matrix(sample(c(0,1,-1), grid_size^2, replace = T), nrow = grid_size, ncol = grid_size)

nb_report <- function(grid){
  neighbour_report <- matrix(ncol = 3)
  for(m_cycle in 1:grid_size){
    for(n_cycle in 1:grid_size){
      if(board[m_cycle, n_cycle] != 0){
      aux_nbs_same_as_self <- checkneighbours(board, c(m_cycle, n_cycle))
      neighbour_report <- rbind(neighbour_report, c(m_cycle, n_cycle, aux_nbs_same_as_self))
      }
    }
  }
  neighbour_report <- neighbour_report[-1, ]
  print("Avg Similar Neighbour: ")
  print(neighbour_report[, 3] %>% mean())
  return(neighbour_report)
}
nb_report(board)
checkneighbours <- function(grid, center){
  # setup
  nb_positions <- matrix(c(-1, -1, -1, 0, -1, 1,0, -1, 0, 1, 1, -1, 1, 0, 1, 1), ncol = 2, nrow = 8, byrow = T) 
  center_m <- center[1]
  center_n <- center[2]
  self <- grid[center_m, center_n]
  if(self != 0){
  nbs <- c()
  for(k in 1:8){
      aux_nb <- nb_positions[k, ] + c(center_m,center_n)
      m_iter <- aux_nb[1]
      n_iter <- aux_nb[2]
      if((m_iter %>% between(1,grid_size)) & (n_iter %>% between(1,grid_size))){
        nbs <- c(nbs, grid[m_iter,  n_iter])
      }else{
      }
    }
        
    # nbs_same_as_self = sum(nbs == self)
    nbs_same_as_self = mean(nbs == self)
    return(nbs_same_as_self)
  }else{
  nbs <- c()
  for(k in 1:8){
      aux_nb <- nb_positions[k, ] + c(center_m,center_n)
      m_iter <- aux_nb[1]
      n_iter <- aux_nb[2]
      if((m_iter %>% between(1,grid_size)) & (n_iter %>% between(1,grid_size))){
        # print(m_iter)
        # print(n_iter)
        nbs <- c(nbs, grid[m_iter,  n_iter])
      }else{
      }
    }
    
  # determining which person could move here
    count_1 <- sum (nbs == 1)
    count_neg1 <- sum (nbs == -1)
    if(count_1 > count_neg1){return(1)}
    if(count_1 < count_neg1){return(-1)}
    if(count_1 == count_neg1){return(0)}
  } 
}

someoneuncontent <- function(grid){
  aux <- nb_report(grid)
  out <- F
  if(any(aux[,3] < 0.5)){
    out <- TRUE
  }
  return(out)
}
someoneuncontent(board)

findemptyspots <- function(grid){
  emptyspots_report <- matrix(ncol = 3) # third col: who could move there: 0 if both could move there
  for(m in 1:grid_size){
    for(n in 1:grid_size){
      self_loc <- c(m, n)
      self <- grid[m, n]
      if(self == 0){
        emptyspots_report <- rbind(emptyspots_report,
                                   c(m, n, checkneighbours(grid, self_loc))
                                   )
        
      }
    }
  }
  emptyspots_report <- emptyspots_report[-1, ]
  return(emptyspots_report)
}

while(someoneuncontent(board)){
  nb_rep <- nb_report(board)
  positions_to_move <- nb_rep[nb_rep[,3] <= 0.5, ]
  movingorder <- sample(1:nrow(positions_to_move), nrow(positions_to_move))
  mover_list <- positions_to_move[movingorder, ]
  # select mover
    aux_mover <- mover_list[1, ]
    mover_list <- mover_list[-1, ]
    aux_mover_m <- aux_mover[1]
    aux_mover_n <- aux_mover[2]
    # find spots
    emptyspots <- findemptyspots(board)
    aux_spotsavailable_helper1 <- emptyspots[, 3] == board[aux_mover_m, aux_mover_n] 
    aux_spotsavailable_helper2 <- emptyspots[, 3] == 0
    aux_spotsavailable <- aux_spotsavailable_helper1 | aux_spotsavailable_helper2
    # relocate mover
    if(sum(aux_spotsavailable) == 0){
      print("No moves possible.")
      no_move_counter <- no_move_counter + 1
    }else{
      aux_next_pos_ind <- sample(which(aux_spotsavailable), 1)
      next_pos_m <- emptyspots[aux_next_pos_ind, 1]
      next_pos_n <- emptyspots[aux_next_pos_ind, 2]
      # emptyspots <- emptyspots[-aux_next_pos_ind, ]
      board[next_pos_m, next_pos_n] <- board[aux_mover_m, aux_mover_n]
      board[aux_mover_m, aux_mover_n] <- 0
      print(paste0("From: [", aux_mover_m, ", ", aux_mover_n, "] --- To: [", next_pos_m, ", ", next_pos_n, "]"))
      no_move_counter <- 0
    }
    if(no_move_counter == 5){
      print("No more moves possible")
      break
    }
}

nb_report(board)