rm(list = ls())

#' Convert square number to indexing coordinates.
#'
#' Note, numbering starts in top left, and it continues by column. Square 
#' number, row number, and column number start at 1. For example, on a 9x9 
#' square, square 15 is indexed by (6,2).
#' 
#' @param i: The square number
#' @param nrow: The number of rows on the rectangular grid. Defaults to 9 for 
#' sudoku
convert1dTo2d <- function(i, nrow=9)
{
    row <- ((i - 1) %% nrow) + 1
    col <- ((i - 1) %/% nrow) + 1
    return(c(row,col))
}

#' Convert indexing coordinates for a grid to square number.
#' 
#' Note, numbering starts in top left, and it continues by column. Square 
#' number, row number, and column number start at 1. For example, on a 9x9 
#' square, square 15 is indexed by (6,2).
#' 
#' @param i: The row number
#' @param j: The column number
#' @param nrow: The number of rows in the rectangular grid. Defaults to 9 for 
#' sudoku
convert2dTo1d <- function(i, j, nrow=9)
{
    element <- (j-1)*nrow + i
    return(element)
}

#' Get other numbers in the same row as a square number on solution grid.
#' 
#' A sudoku object has a solutionGrid, which contains the solution-in-
#' progress. This function collects all written-in answers in the same row
#' as the square number.
#'  
#' @param sudoku: The sudoku object
#' @param i: The square number
#' @return A (possibly empty) vector containing values from 0 - 9
getSameRowVals <- function(sudoku,i)
{
    coords <- convert1dTo2d(i)
    toDiff <- c(sudoku$solutionGrid[coords[1],])
    return(toDiff)
}

#' Get other numbers in the same column as a square number on solution grid.
#' 
#' A sudoku object has a solutionGrid, which contains the solution-in-
#' progress. This function collects all written-in answers in the same column
#' as the square number.
#'  
#' @param sudoku: The sudoku object
#' @param i: The square number
#' @return A (possibly empty) vector containing values from 0 - 9
getSameColVals <- function(sudoku, i)
{
    coords <- convert1dTo2d(i)
    toDiff <- c(sudoku$solutionGrid[,coords[2]])
    return(toDiff)
}

#' Get other numbers in the same box as a square number on solution grid.
#' 
#' A sudoku object has a solutionGrid, which contains the solution-in-
#' progress. This function collects all written-in answers in the same box
#' (a box is a 3x3 unit that tiles the 9x9 sudoku grid) as the square number. 
#'  
#' @param sudoku: The sudoku object
#' @param i: The square number
#' @return A (possibly empty) vector containing values from 0 - 9
getSameBoxVals <- function(sudoku, i)
{
    whichSqs <- sudoku$box == sudoku$box[i]
    toDiff <- c(sudoku$solutionGrid[whichSqs])
    return(toDiff)
}

#' Get numbers in same relative box position as a square on solution grid.
#' 
#' A sudoku object has a solutionGrid, which contains the solution-in-
#' progress. This function collects all written-in answers in the same relative 
#' box position as the square number. This is a consideration for so-called
#' Ultimate Sudoku.
#'  
#' @param sudoku: The sudoku object
#' @param i: The square number
#' @return A (possibly empty) vector containing values from 0 - 9
getSameUltimateVals <- function(sudoku, i)
{
    whichSqs <- sudoku$ultimate == sudoku$ultimate[i]
    toDiff <- c(sudoku$solutionGrid[whichSqs])
    return(toDiff)
}

#' Get all valid answers for a given square number on a sudoku grid
#' 
#' Given a square number, this function finds all legal numbers that can be 
#' written into that square by the rules of Sudoku.
#' 
#' @param sudoku: The sudoku object
#' @param i: The square number
#' @param ultimate: Boolean, TRUE if using Ultimate Sudoku rules 
#' @return A (possibly empty) vector subsetting 1:9. Empty if there are no 
#' valid answers or if an answer is already written in.
getMovesOneSq <- function(sudoku, i, ultimate = FALSE)
{
    if (sudoku$solutionGrid[i] == 0)
    {
        possibleVals <- 1:9
        rowVals <- getSameRowVals(sudoku, i)
        possibleVals <- setdiff(possibleVals, rowVals)
        colVals <- getSameColVals(sudoku, i)
        possibleVals <- setdiff(possibleVals, colVals)
        boxVals <- getSameBoxVals(sudoku, i)
        possibleVals <- setdiff(possibleVals, boxVals)
        if (ultimate)
        {
            ultimateVals <- getSameUltimateVals(sudoku,i)
            possibleVals <- setdiff(possibleVals, ultimateVals)
        }
        return(possibleVals)
    }
    else
    {
        return(integer(0))
    }
}

#' Get all valid answers for all squares on a sudoku grid
#' 
#' This function finds all numbers that can be written legally into each 
#' square by the rules of Sudoku.
#' 
#' @param sudoku: The sudoku object
#' @param ultimate: Boolean, TRUE if using Ultimate Sudoku rules 
#' @return A list of length 81, containing a set of valid answers for each 
#' square
getMoves <- function(sudoku, ultimate = FALSE)
{
    nCol <- ncol(sudoku$solutionGrid)
    nRow <- nrow(sudoku$solutionGrid)
    nSq <- nCol * nRow
    
    allMoves <- list()
    
    for (i in seq_len(nSq))
    {
        thisSqMoves <- getMovesOneSq(sudoku, i, ultimate) 
        allMoves <- c(allMoves, list(thisSqMoves))
    }
    
    return(allMoves)
}

#' Analyze a solution grid to determine if the current answers are all valid
#' 
#' The algorithm is the following. For each answer in the solution grid, 
#' remove that answer and check that square to see if the answer is a possible 
#' entry for that square. Also, for each empty square, check to see that there 
#' are possible solutions for that square
#' 
#' @param sudoku: The sudoku object
#' @param ultimate: Boolean, TRUE if using Ultimate Sudoku rules 
#' @return Returns TRUE if and only if the position is valid
isValidPosition <- function(sudoku, ultimate = FALSE)
{
    nCol <- ncol(sudoku$solutionGrid)
    nRow <- nrow(sudoku$solutionGrid)
    nSq <- nCol * nRow
    
    for (i in seq_len(nSq))
    {
        tmp <- sudoku$solutionGrid[i]
        if (1 <= tmp && tmp <= 9)
        {
            sudoku$solutionGrid[i] <- 0
            moves <- getMovesOneSq(sudoku, i, ultimate)
            if (!(tmp %in% moves))
            {
                return(FALSE)
            }
            sudoku$solutionGrid[i] <- tmp
        } else {
            moves <- getMovesOneSq(sudoku, i, ultimate)
            if (length(moves) == 0)
            {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

#' Recursively solve a Sudoku puzzle.
#'
#' This function is the workhorse for the sudoku class. It finds the solution 
#' to the Sudoku puzzle. The algorithm is the following
#' 
#' Do:
#'     Loop through entire working solution grid. For each square, find all 
#'     legal solutions. If there is exactly one solution, store it in the 
#'     working solution grid.
#' Stop when no values were written into the working solution during a loop
#' Check end conditions in order:
#'     1. If Sudoku puzzle is solved, return.
#'     2. If Sudoku puzzle is impossible to solve, return.
#'     3. Find an empty square in the solution grid, set up a loop to write in
#'     each possibility for that square and repeat this algorithm.
#'
#' @param sudoku: The sudoku object
#' @param level: The level of the recursion
#' @param ultimate: Boolean, TRUE if using Ultimate Sudoku rules 
#' @return Returns sudoku object with solution stored in solutionGrid. Other 
#' boolean flags and counts are set (see sudoku class documentation).
update <- function(sudoku, level, ultimate = FALSE)
{
    nCol <- ncol(sudoku$solutionGrid)
    nRow <- nrow(sudoku$solutionGrid)
    nSq <- nCol * nRow
    sqWasUpdated <- TRUE
    while (sqWasUpdated)
    { 
        sqWasUpdated <- FALSE
        for (i in seq_len(nSq))
        {
            movesOneSq <- getMovesOneSq(sudoku, i, ultimate)
            if (length(movesOneSq) == 1)
            {
                sudoku$solutionGrid[i] <- movesOneSq
                sqWasUpdated <- TRUE
            }
        }
        sudoku$nPasses <- sudoku$nPasses + 1
    }
    
    if (all(sudoku$solutionGrid != 0) && isValidPosition(sudoku))
    {
        sudoku$solved <- TRUE
        return(sudoku)
    } else {
        allMoves <- getMoves(sudoku, ultimate)
        if (length(unlist(allMoves)) == 0 | !isValidPosition(sudoku))
        {
            sudoku$noSolution <- TRUE
            return(sudoku)
        } else {
            # GUESS
            sudoku$nGuesses <- sudoku$nGuesses + 1
            i <- 1
            while (length(allMoves[[i]]) == 0)
            {
                i <- i + 1
            }
            for (j in allMoves[[i]])
            {
                sudoku$solutionGrid[i] <- j
                out <- update(sudoku, level + 1, ultimate)
                if (out$solved)
                {
                    return(out)
                }
            }
            sudoku$noSolution <- TRUE
            return(sudoku)
        }
    }
}

#' Initialize a sudoku object and solve the puzzle (S3 class)
#' 
#' Currently, one method is available for initializing this class. That is to 
#' use a string representation. Each number in the Sudoku puzzle is represented 
#' by that number, and each empty square is represented by a period ("."). 
#' Write the Sudoku puzzle in row-major order. Thus, write
#' 
#'                                             #-------------------------------
#' >> sudokuStr <- paste("6....41..",          #| 6  .  . | .  .  4 | 1  .  . |
#'                       "7....8..5",          #| 7  .  . | .  .  8 | .  .  5 |
#'                       "9.4.17368",          #| 9  .  4 | .  1  7 | 3  6  8 |
#'                       "",                   #-------------------------------
#'                       ".6738....",          #| .  6  7 | 3  8  . | .  .  . |
#'                       "82.7.1.36",          #| 8  2  . | 7  .  1 | .  3  6 |
#'                       ".13..57.9",          #| .  1  3 | .  .  5 | 7  .  9 |
#'                       "",                   #-------------------------------
#'                       ".981.3.7.",          #| .  9  8 | 1  .  3 | .  7  . |
#'                       ".4...6.1.",          #| .  4  . | .  .  6 | .  1  . |
#'                       "...49..5.", sep="")  #| .  .  . | 4  9  . | .  5  . |
#'                                             #-------------------------------
#'
#' or write the equivalent 81-character string in one line. Next call
#' 
#' >> s <- sudoku(sudokuStr)
#' 
#' and the puzzle will be solved. Finding a solution can take a long time (>> 1
#' minute) depending on how much guessing is required.
#' 
#' >> print(s)
#'     Prints the solution grid to standard in.
#' 
#' >> summary(s)
#'     Prints the start grid, the solution grid, and information about the 
#'     process of getting the solution to standard in.
#'     
#' >> plot(s)
#'     Plots the sudoku object by putting numbers in the start grid in a darker 
#'     cell and numbers discovered for the solution in a lighter cell.
#' 
#' Class members are 
#' startGrid: a 9x9 matrix containing the starting position of the puzzle
#' solutionGrid: a 9x9 matrix containing the solution to the starting position,
#'     if one exists
#' box: 9x9 matrix labeling the nine 3x3 boxes
#' ultimate: 9x9 matrix labeling the nine relative positions in the 3x3 boxes
#' nPasses: Defaults to 0, the number of passes through the grid until solution
#' nGuesses: Defaults to 0, is the number of guesses needed to find solution
#' noSolution: defaults to FALSE, becomes TRUE if it is proven there are no 
#'     solutions to the start position
#' validStart: TRUE if and only if the start position is valid
#' solved: defaults to FALSE, becomes TRUE if a solution is found and stored in 
#'     solutionGrid
#' 
#' @param start: A string of 81 characters representing the Sudoku start grid 
#' written in row-major order.
#' @param ultimate: A boolean to indicate if this sudoku object adheres to the 
#' rules of Ultimate Sudoku
#' @return Returns sudoku object with solution stored in solutionGrid. Other 
#' boolean flags and counts are set.
sudoku <- function(start, ultimate=FALSE)
{
    isDigit <- function(x)
    {
        return(grepl("^\\d$",x))
    }
    
    sudoku <- list()
    class(sudoku) <- "sudoku"
    
    if (class(start) == 'character' && length(start) == 1 && 
        nchar(start) == 81)
    {
        digits <- strsplit(start,"")[[1]]
        startGrid <- matrix(0, nrow=9, ncol=9)
        for (i in seq_len(length(digits)))
        {
            digit <- digits[i]
            if (isDigit(digit) && as.numeric(digit) != 0)
            {
                coords <- convert1dTo2d(i)
                startGrid[coords[2],coords[1]] <- as.numeric(digit)
            }
        }
        sudoku$startGrid <- startGrid
    } else if (class(start) == "matrix" && all(dim(start) == c(9,9))) {
        if (all(0 <= start & start <= 9)) 
        {
            sudoku$startGrid <- start
        }
    } else if (class(start) == "numeric" && length(start) == 81) {
        if (all(0 <= start & start <= 9) 
        {
            sudoku$startGrid <- matrix(start, nrow=9, ncol=9, byrow=TRUE)
        }
    } 
    else {
        message <- "Not a valid initialization of sudoku class"
        usage <- "USAGE: sudoku(character or vector of length 81[, boolean])"
        stop(paste(message,usage,sep="\n"))    
    }
    
    sudoku$solutionGrid <- sudoku$startGrid
    
    rep3x3 <- function(x) rep(rep(x,each=3),3)
    sudoku$box  <-  matrix(c(rep3x3(1:3), rep3x3(4:6), rep3x3(7:9)), nrow=9, 
                           ncol=9)
    sudoku$ultimate <- matrix(c(rep(1:3,3),rep(4:6,3),rep(7:9,3)), nrow=9, 
                              ncol=9)
    sudoku$nPasses <- 0
    sudoku$nGuesses <- 0
    sudoku$noSolution <- FALSE
    if (isValidPosition(sudoku))
    {
        sudoku$validStart <- TRUE
        sudoku$solved <- all(sudoku$solutionGrid != 0)
        recursionLevel <- 1
        sudoku <- update(sudoku, recursionLevel, ultimate)
    } else {
        sudoku$validStart <- FALSE
        sudoku$solved <- FALSE
    }
    
    return(sudoku)
}

sudoku2 <- function(start, n = 9)
{
    isDigit <- function(x)
    {
        return(grepl("^\\d$",x))
    }
    
    sudoku <- list()
    class(sudoku) <- "sudoku2"
    
    if (class(start) == 'character' && length(start) == 1 && 
        nchar(start) == n^2)
    {
        digits <- strsplit(start,"")[[1]]
        startGrid <- array(TRUE, dim=rep(n,3))
        for (i in seq_len(length(digits)))
        {
            digit <- digits[i]
            if (isDigit(digit) && as.numeric(digit) != 0)
            {
                coords <- convert1dTo2d(i,n)
                startGrid[coords[2],coords[1],] <- (seq_len(n) == digit)
            }
        }
        sudoku$startGrid <- startGrid
    }
}

#' Print a 9x9 matrix as a Sudoku puzzle.
catMat <- function(mat)
{
    for(i in 1:9)
    {
        if (i %% 3 == 1)
        {
            cat(rep("-", 31), sep="")
            cat("\n")
        }
        for (j in 1:9)
        {
            if (j %% 3 == 1)
            {
                cat("|")
            }
            if (mat[i,j] == 0)
            {
                cat(" . ")
            } else {
                cat(" ", mat[i,j], " ", sep="")
            }
        }
        cat("|\n")
    }
    cat(rep("-", 31),sep="")
    cat("\n")
}

#' Print two 9x9 matrices as Sudoku puzzles side by side.
catStartToSolved <- function(mat1, mat2)
{
    for(i in 1:9)
    {
        if (i %% 3 == 1)
        {
            cat(rep("-", 31), sep="")
            cat("  ")
            cat(rep("-", 31), sep="")
            cat("\n")
        }
        for (j in 1:9)
        {
            if (j %% 3 == 1)
            {
                cat("|")
            }
            if (mat1[i,j] == 0)
            {
                cat(" . ")
            } else {
                cat(" ", mat1[i,j], " ", sep="")
            }
        }
        cat("|")
        if (i == 5)
        {
            cat("=>")
        } else {
            cat("  ")
        }
        for (j in 1:9)
        {
            if (j %% 3 == 1)
            {
                cat("|")
            }
            if (mat2[i,j] == 0)
            {
                cat(" . ")
            } else {
                cat(" ", mat2[i,j], " ", sep="")
            }
        }
        cat("|\n")
    }
    cat(rep("-", 31),sep="")
    cat("  ")
    cat(rep("-", 31), sep="")
    cat("\n")
}

#' Print the solution grid for a sudoku object with nice formatting
print.sudoku <- function(sudoku)
{
    mat <- sudoku$solutionGrid
    catMat(mat)
}

#' Print start grid, solution grid, and information about the solution
summary.sudoku <- function(sudoku)
{
    if (!sudoku$validStart)
    {
        cat("Not a valid starting position!\n")
        catMat(sudoku$startGrid)
    } else {
        cat("Solution found:",sudoku$solved,"\n")
        catStartToSolved(sudoku$startGrid, sudoku$solutionGrid)
        cat("\n")
        cat("To find the solution, needed to pass through\nthe Sudoku")
        cat(" solution grid", sudoku$nPasses,"times and needed\nto guess a")
        cat(" number in the solution", sudoku$nGuesses, "times.\n" )
    }
}

#' Plot Sudoku puzzle with original numbers darker and solved numbers lighter
plot.sudoku <- function(sudoku, ..., labels=FALSE)
{
    nCol <- ncol(sudoku$solutionGrid)
    nRow <- nrow(sudoku$solutionGrid)
    nSq <- nCol * nRow
    
    mar <- c(0, 0, 0, 0)
    oldpar <- par(mar=mar)
    plot(0:(nCol + 1),0:(nRow + 1), type="n", axes = FALSE, ...)
    
    for (i in seq_len(nSq))
    {
        backgroundCol <- grey(0.75)
        if (sudoku$startGrid[i] == 0)
        {
            backgroundCol <- grey(0.95)
        }
        
        coords <- convert1dTo2d(i)
        y <- nRow + 1 - coords[1]
        x <- coords[2]
        polygon(c(x - 0.5, x - 0.5, x + 0.5, x + 0.5), 
                c(y - 0.5, y + 0.5, y + 0.5, y - 0.5),
                col = backgroundCol)
        entry <- sudoku$solutionGrid[i]
        if (entry != 0)
        {
            text(x, y, entry)
        }
    }
    
    for (i in seq(.5, nCol + 0.5, 3)) {
        lines(c(.5, nRow + 0.5), c(i,i), lwd=3)
    }
    for (i in seq(0.5, nRow + 0.5, 3))
    {
        lines(c(i,i), c(.5, nCol + 0.5), lwd=3)
    }
    
    if (labels)
    {
        for (i in seq_len(nCol))
        {
            text(i, 0, i, offset = 0)
        }
        for (i in seq_len(nRow))
        {
            text(0.25, i, i)
        }
    }
    
    par(oldpar)
}

test1 <- "1123....3241...."
print(sudoku2(test1))

# Test cases
# str <- paste(c(1:9, rep(0,27), 9:1, rep(0,27),2:9,1), collapse="")
# s <- sudoku(str)

#str1 <- "195.4.2.8..3.78.5..7.95.3.4..17....5.........2....47..6.4.39.8..8.42.9..3.9.8.642"
#s1 <- sudoku(str1)

# str2 <- paste(paste(rep(1:9, 6), collapse=""),paste(rep("123456...",3),collapse=""), sep="")
# s2 <- sudoku(str2)
# 
#str3 <- paste(rep(".", 81),collapse="")
#s3 <- sudoku(str3)
# 
#str4 <- "6....41..7....8..59.4.17368.6738....82.7.1.36.13..57.9.981.3.7..4...6.1....49..5."
#s4 <- sudoku(str4)
# 
# str5 <- "7.6.5.3.4.3.9.....4.9.3.7.1.8.4.....5.2.8.9.3.........3.5.7.8.6.........6.8.9.5.2"
# s5 <- sudoku(str5)
# 
# str6 <- "..4...7...3.1.5.9.2.......5.4.6.7.8.....3.....1.8.4.3.7.......6.9.7.3.4...6...1.."
# s6 <- sudoku(str6)
# 
# str7 <- "9.8............4.7..61..5...45.7..................3.91....5.........7...1......6."
# s7 <- sudoku(str7)
#s8 <- sudoku(".....123")

#str9 <- "682534197731968245954217368567389421829741536413625789298153674345876912176492853"
#s9 <- sudoku(str9)