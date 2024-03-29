# sudoku Generator

rm(list=ls())

# tsudoku draws a sudoku table
tsudoku <- function(seed=NULL, attempt=1, ultimate=F, iterate=T){
        
        blocks <- list() # Create a list of blocks that cannot share set elements.
        
        blocks$rows <- matrix(rep(1:9, times=9), nrow=9,ncol=9)
        blocks$cols <- matrix(rep(1:9, each=9), nrow=9,ncol=9)
        
        # Define a little function that will simply our box identifier code
        re3 <- function(x) rep(rep(x,each=3),3)
        blocks$box  <-  matrix(c(re3(1:3), re3(4:6), re3(7:9)), nrow=9,ncol=9)
        
        # Define a new block setup in which each square within each 3x3 cannot
        # have the same element as any of the other squares occupying the same
        # space in any other 3x3 matrix
        re3b <- function(x) rep(rep(x),3)
        if (ultimate) blocks$ultimate <- matrix(c(re3b(1:3), re3b(4:6), re3b(7:9)), nrow=9,ncol=9)
        
        Fset <- 1:9 # Each set is drawn from 1:9
        
        # Remove a set from another set
        ss <- function(set,less) set[!set%in%less]
        
        
        if (!is.null(seed)) set.seed(seed)
        fail <- F # An indicator that this randomized search has failed.
        
        sgrid <- matrix(NA,nrow=9,ncol=9)
        # Loop through each individual cell but only if we have not failed
        for (i in 1:nrow(sgrid)) for (ii in 1:ncol(sgrid)) if (!fail) {
                tset <- Fset # tset is a temporary set
                for (iii in 1:length(blocks)) {
                        tblock <- blocks[[iii]] # Temporary block
                        type <- tblock[i,ii]    # Identify type of current cell
                        values <- c(sgrid[tblock==type]) # Grab values from other cells
                        tset <- ss(tset, values) # reduce tset by any values observed in blocks
                }
                if (length(tset)==0) fail=T # If we ran out of feasible values then we have failed
                if (length(tset)>0) sgrid[i,ii] <- sample(rep(tset,2),1) 
                # Repeat twice so that we neven have a input value of length 1.
        }
        # Check if our algorithm found a solution
        if (fail) {
                if (iterate) {
                        cat(paste("Attempt",attempt,"failed\n"))
                        return(tsudoku(attempt=attempt+1, ultimate=ultimate, iterate=iterate))
                }
                if (!iterate) return("Fail")
        } else {
                returner <- list(sgrid=sgrid, blocks=blocks, ultimate=ultimate, attempt=attempt)
                class(returner) <- "sudoku"
                return(returner)
        }
}

s1 <- tsudoku() # This usually takes between 50-500 attempts
# It gives up before 

# Ultimate sudoku in which each respective square in the 3x3 grid
# cannot share a value with any other 3x3 grid of the same place is
# much harder to solve and usually takes between 3k and 10k attempts 
# before finding a solution. 20-30 seconds
s2="Fail"
attempt = 0
while (length(s2)==1) {
        s2 <- tsudoku(ultimate=T, iterate=F)
        attempt = attempt+1
        print(attempt)
}

print.sudoku <- function(sudo) {
        if (!is.null(sudo$ultimate)) cat(paste0("ultimate=",sudo$ultimate,"\n"))
        print("sgrid")
        print(sudo$sgrid)
        if (!is.null(sudo$mgrid)) {
                print("mgrid")
                print(sudo$mgrid)
        }
        # If problem has been solved, insert solution
        if (!is.null(sudo$fgrid)) {
                print("fgrid")
                print(sudo$fgrid)  
        }
        if (!is.null(sudo$s0steps)) {
                print("s0steps")
                print(sudo$s0steps)  
        }
}

print(s2)

# Define a plot function for sudoku
plot.sudoku <- function(sudo, mar=rep(0,4), axes=F, labels=F) {
        stable <- sudo$sgrid
        mgrid <- sudo$mgrid
        if (length(mgrid)==0) mgrid <- stable*0
        oldpar <- par(mar=mar) # Set up table parameters
        
        # Ititiate and Empty Plot
        plot(0:10,0:10, type="n",axes=axes)
        
        for (i in 1:9) for (ii in 1:9) {
                backcol=grey(.95)
                if (mgrid[i,ii]!=0) backcol=grey(.85)
                
                if (!is.null(sudo$ultimate)) if (sudo$ultimate) {
                        backcol=grey(.99-(sudo$blocks$ultimate[i,ii])*.07)
                        if (mgrid[i,ii]!=0) backcol=grey(1)
                }
                polygon(c(i-.5,i-.5,i+.5,i+.5), 
                        c(ii-.5,ii+.5,ii+.5,ii-.5), col=backcol)
        }
        
        # Insert numbers
        for (i in 1:9) for (ii in 1:9) 
                if (mgrid[i,ii]==0) text(i,ii,stable[i,ii],offset=0)
        
        # If problem has been solved, insert solution
        if (!is.null(sudo$fgrid)) for (i in 1:9) for (ii in 1:9) 
                if (sudo$fgrid[i,ii]!=0) text(i,ii,sudo$fgrid[i,ii],offset=0)
        
        # For reference it might be useful to stick in row and colum numbers
        if (labels) for (i in 1:9) {
                text(.25,i,i,offset=0)
                text(i,.25,i,offset=0)
        }
        
        # Plot Heavy Lines
        for (i in seq(.5,9.5,3)) {
                lines(c(.5,9.5),c(i,i), lwd=3)
                lines(c(i,i),c(.5,9.5), lwd=3)
        }
        
        par(oldpar)
}

plot(s1, labels=T)
plot(s2)

# Now generate the gaps
sudo.gaps <- function(sudo, inference.mean=4, inference.max=10) {
        sgrid <- sudo$sgrid       # grab the grid
        mgrid <- igrid <- sgrid*0 # and define a grid for missing and for inference
        # inference being the number of missing slots that
        # a particular square has after taking into account
        # all overlapping squares
        
        blocks <- sudo$blocks
        
        while (mean(igrid)<inference.mean) {
                tigrid <- igrid # temporary igrid
                tmgrid <- mgrid # temporary mgrid
                
                provisional <- c(sample(1:9,1), sample(1:9,1))
                
                tmgrid[provisional[1],provisional[2]] <- 1
                tsgrid <- sgrid*(1-tmgrid) # temporary sgrid
                
                for (i in 1:9) for (ii in 1:9) {
                        tset <- NULL
                        for (iii in 1:length(blocks)) {
                                tblock <- blocks[[iii]] # Temporary block
                                type <- tblock[i,ii]    # Identify type of current cell
                                values <- c(tsgrid[tblock==type]) # Grab values from other cells
                                tset <- unique(c(tset, values)) # combine 
                        }
                        tigrid[i,ii] <- 9-sum(tset!=0)
                }
                
                if (max(tigrid)<=inference.max) {
                        igrid <- tigrid
                        mgrid <- tmgrid
                }
        }
        sudo$mgrid <- mgrid 
        sudo$igrid <- igrid
        
        return(sudo)  
}

# Control the difficulty of the tables by setting the inference mean
# The higher the number the more spaces there will be.
s1 <- sudo.gaps(s1, inference.mean=1, inference.max=2)
plot(s1)
sum(s1$mgrid)

s2 <- sudo.gaps(s2, inference.mean=3, inference.max=5)
plot(s2)
# We can see that ultimate sudoku has a few more missing spaces for
# the mean inference because inference can be made using 4 dimensions rather
# than 3.
sum(s2$mgrid)

solver.sudoku <- function(sudo, ...) {
        mgrid <- sudo[["mgrid"]]
        tigrid <- sgrid <- fgrid <- mgrid*0
        
        sgrid[!mgrid] <- sudo[["sgrid"]][!mgrid]
        
        blocks <- sudo[["blocks"]]
        
        solver(sgrid=sgrid, mgrid=mgrid, blocks=blocks, ultimate=sudo$ultimate, ...)
}

solver <- function(sgrid, mgrid=NULL, noisily=F, blocks=list(), smatch=10, ultimate=F) {
        if (length(blocks)==0) { # If blocks is empty fill it
                blocks$rows <- matrix(rep(1:9, times=9), nrow=9,ncol=9)
                blocks$cols <- matrix(rep(1:9, each=9), nrow=9,ncol=9)
                re3 <- function(x) rep(rep(x,each=3),3)
                blocks$box  <-  matrix(c(re3(1:3), re3(4:6), re3(7:9)), nrow=9,ncol=9)
        }
        
        if (all(is.null(mgrid))) {
                mgrid <- sgrid*0
                mgrid[(sgrid==0)|is.na(sgrid)] <- 1
        }
        
        fgrid <- sgrid*0
        
        sgrid0 <- sgrid
        mgrid0 <- mgrid
        
        s0steps <- 0 # Count the number of steps using strategy 0
        
        j <- 0
        while ((sum(mgrid)>0)&(j<smatch)) {
                j <- j+1
                
                for (i in 1:9) for (ii in 1:9) if (sgrid[i,ii]==0) {
                        tset <- NULL # A temporary set to hold chosen values
                        for (iii in 1:length(blocks)) if (mgrid[i,ii]) {
                                tblock <- blocks[[iii]] # Temporary block
                                type <- tblock[i,ii]    # Identify type of current cell
                                values <- c(sgrid[tblock==type]) # Grab values from other cells
                                tset <- unique(c(tset, values)) # combine 
                        }
                        remainder <- (1:9)[!(1:9 %in% tset)]
                        if (length(remainder)==1) {
                                mgrid[i,ii] <- 0
                                sgrid[i,ii] <- fgrid[i,ii] <- remainder
                                if (noisily) print(paste("Round",j,"-sub out", i, ii, "with", fgrid[i,ii]))
                                s0steps <- s0steps + 1
                        }
                }}
        
        sudo <- list(sgrid=sgrid0, mgrid=mgrid0, s0steps=s0steps, 
                     fgrid=fgrid , ultimate=ultimate)  
        class(sudo) <- "sudoku"
        return(sudo)
}

s1 <- tsudoku()

s1 <- sudo.gaps(s1, inference.mean=2, inference.max=3)
plot(s1)

ss1 <- solver.sudoku(s1, noisily=T)
plot(ss1, labels=T)

s2 <- sudo.gaps(s2, inference.mean=1.25, inference.max=2)
plot(s2)

ss2 <- solver.sudoku(s2, noisily=T)
plot(ss2, labels=T)