rm(list = ls())

gameBoard <- matrix(rep(0,81),nrow = 9,ncol = 9)
# gameBoardHelper <- matrix(1:81,nrow = 9,ncol = 9)

prevGameBoard <- matrix(rep(0,81),nrow = 9,ncol = 9)
# gameBoard <- matrix(1:81,nrow = 9,ncol = 9)

generate_pieces <- function(){
  return(
    list(
      p_1x1 = matrix(1, 1, 1),
      p_2x1 = matrix(1, 1, 2),
      p_1x2 = matrix(1, 2, 1),
      p_3x1 = matrix(1, 1, 3),
      p_1x3 = matrix(1, 3, 1),
      p_4x1 = matrix(1, 1, 4),
      p_1x4 = matrix(1, 4, 1),
      p_5x1 = matrix(1, 1, 5),
      p_1x5 = matrix(1, 5, 1),
      p_2x2Dia_L = matrix(c(0, 1,
                            1, 0), 2, 2),
      p_2x2Dia_R = matrix(c(1, 0,
                            0, 1), 2, 2),
      p2x2L_0 = matrix(c(1, 1,
                         0, 1), 2, 2),
      p2x2L_90 = matrix(c(1, 1,
                          1, 0), 2, 2),
      p2x2L_180 = matrix(c(1, 0,
                           1, 1), 2, 2),
      p2x2L_270 = matrix(c(0, 1,
                           1, 1), 2, 2),
      p3x3L_180 = matrix(c(1, 0, 0,
                           1, 0, 0,
                           1, 1, 1), 3, 3),
      p3x3L_90 = matrix(c(1, 1, 1,
                          1, 0, 0,
                          1, 0, 0), 3, 3),
      p3x3L_270 = matrix(c(0, 0, 1,
                           0, 0, 1,
                           1, 1, 1), 3, 3),
      p3x3L_0 = matrix(c(1, 1, 1,
                         0, 0, 1,
                         0, 0, 1), 3, 3),
      p2x3T_0 = matrix(c(1, 0, 1,
                         1, 1, 0), 2, 3),
      p2x3T_90 = matrix(c(0, 1, 0,
                          1, 1, 1), 3, 2),
      p2x3T_180 = matrix(c(0, 1, 1,
                           1, 0, 1), 2, 3),
      p2x3T_270 = matrix(c(1, 1, 1,
                           0, 1, 0), 3, 2),
      p2x2 = matrix(1, 2, 2),
      p2x3L_0 = matrix(c(1, 1, 0,
                         1, 0, 1), 2, 3),
      p2x3L_90 = matrix(c(1, 1, 1,
                          1, 0, 0), 3, 2),
      p2x3L_180 = matrix(c(0, 1, 0,
                           1, 1, 1), 2, 3),
      p2x3L_270 = matrix(c(1, 1, 1,
                           0, 0, 1), 3, 2),
      p2x3U_0 = matrix(c(1, 1, 0,
                         1, 1, 1), 2, 3),
      p2x3U_90 = matrix(c(1, 1, 1,
                          1, 0, 1), 3, 2),
      p2x3U_180 = matrix(c(1, 1, 1,
                           0, 1, 1), 2, 3),
      p2x3U_270 = matrix(c(1, 0, 1,
                           1, 1, 1), 3, 2),
      p2x3S_0 = matrix(c(0, 1, 1,
                         1, 1, 0), 2, 3),
      p2x3S_90 = matrix(c(1, 1, 0,
                          0, 1, 1), 3, 2),
      p2x3S_180 = t(matrix(c(1, 1, 0,
                             0, 1, 1), 3, 2)),
      p2x3S_270 = t(matrix(c(0, 1, 1,
                             1, 1, 0), 2, 3)),
      p3x3T_0 = matrix(c(1, 0, 0,
                         1, 1, 1,
                         1, 0, 0), 3, 3),
      p3x3T_90 = matrix(c(0, 1, 0,
                          0, 1, 0,
                          1, 1, 1), 3, 3),
      p3x3T_180 = matrix(c(0, 0, 1,
                           1, 1, 1,
                           0, 0, 1), 3, 3),
      p3x3T_270 = matrix(c(1, 1, 1,
                           0, 1, 0,
                           0, 1, 0), 3, 3),
      p3x3Dia_R = matrix(c(1, 0, 0,
                           0, 1, 0,
                           0, 0, 1), 3, 3),
      p3x3Dia_L = matrix(c(0, 0, 1,
                           0, 1, 0,
                           1, 0, 0), 3, 3),
      P3x3Cross = matrix(c(0, 1, 0,
                           1, 1, 1,
                           0, 1, 0), 3, 3)
    )
  )
}

check_for_score <- function(gameBoard, prevGameBoard, clear_score = 18){
  
  score_func <- function(start, finish){
    initialScore <- .1 * start^2
    endingScore <- .1 * finish^2
    return(endingScore - initialScore)
  }

  rows_cleared <- rep(FALSE,9)
  cols_cleared <- rep(FALSE,9)
  cells_cleared <- rep(FALSE,9)
  
  rows_score <- rep(0,9)
  cols_score <- rep(0,9)
  cells_score <- rep(0,9)
  
  #left > right, top > bottom
  cell_coords <- list(
    c(1:3, 1:3), c(1:3, 4:6), c(1:3, 7:9),
    c(4:6, 1:3), c(4:6, 4:6), c(4:6, 7:9),
    c(7:9, 1:3), c(7:9, 4:6), c(7:9, 7:9)
  )
  
  for (i in 1:9) {
    #ROW CHECK
    rows_cleared[i] <- all(gameBoard[i,1:9] == 1)
    rows_score[i] <- score_func(start = sum(prevGameBoard[i, 1:9]), 
                                finish = sum(gameBoard[i, 1:9]))
    #COLUMN CHECK
    cols_cleared[i] <- all(gameBoard[1:9, i] == 1)
    cols_score[i] <- score_func(start = sum(prevGameBoard[1:9, i]), 
                                finish = sum(gameBoard[1:9, i]))
    #CELL CHECK
    cell <- cell_coords[[i]]
    cells_cleared[i] <- all(gameBoard[cell[1], cell[2]] == 1)
    cells_score[i] <- score_func(start = sum(prevGameBoard[cell[1], cell[2]]), 
                                 finish = sum(gameBoard[cell[1], cell[2]]))
  }
  
  final_score <- 
    (sum(rows_score) + sum(rows_cleared) * clear_score) +
    (sum(cols_score) + sum(cols_cleared) * clear_score)  +
    (sum(cells_score) + sum(cells_cleared) * clear_score) 
  
  return(final_score)
}

place_piece <- function(prime, beta, y, x) {
  prime_rows <- nrow(prime)
  prime_cols <- ncol(prime)
  beta_rows <- nrow(beta)
  beta_cols <- ncol(beta)
  
  # Check if beta fits in prime at the given position (x, y)
  if ((x + beta_rows - 1) > prime_rows || (y + beta_cols - 1) > prime_cols) {
    stop("Error: Beta matrix doesn't fit in the prime matrix at the given position.")
  }
  
  # Check if all values in the overlap of prime are zero
  for (i in 0:(beta_rows - 1)) {
    for (j in 0:(beta_cols - 1)) {
      if (prime[x + i, y + j] != 0) {
        stop("Error: Overlap area in prime matrix is not all zero.")
      }
    }
  }
  
  # Merge beta into prime
  for (i in 0:(beta_rows - 1)) {
    for (j in 0:(beta_cols - 1)) {
      prime[x + i, y + j] <- beta[i + 1, j + 1]
    }
  }
  
  return(prime)
}

pieces <- generate_pieces()


prevGameBoard <- gameBoard
gameBoard <- place_piece(gameBoard, pieces$p3x3T_90, 1,1)

check_for_score(gameBoard, prevGameBoard)
