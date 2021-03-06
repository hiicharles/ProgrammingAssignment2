About cachematrix.R
===================
***

*Charles Hii (hiicharles@gmail.com)*

***
### makeCacheMatrix(x, ...)

        - x is invertible matrix of 2x2, 3x3, 4x4 or NxN
        - Return a special matrix.
        - Special matrix is a list contain 5 functions.
            - get               ## Return the matrix
            - set               ## Update the matrix
            - haschanged        ## Determine matrix being updated and inverse not being updated
            - getinverse        ## Return inverse matrix
            - setinverse        ## Update the inverse matrix

        Note:
        - Why haschanged() function? 
         If the inverse has already been calculated (and the matrix has not changed), 
         then the cachesolve should retrieve the inverse from the cache.


***
### cacheSolve(x, ...)

        - x is special matrix generated by makeCacheMatrix function.
        - Determine whether to use existing inverse matrix (cache) or compute inverse matrix.
        - If inverse matrix is outdated (haschanged() return TRUE), inverse matrix will be computed.
        - Slow: Inverse matrix is computed using solve() function.
        - Fast: Inverse matrix is from getinverse. 
        
***
### How to test the assignment?

1. Create a invertible matrix having nrow = ncol.

        ## Create matrix 5 x 5
        > data <- matrix(sample(1:50, size=25, replace = TRUE), nrow=5, ncol=5)

        > class(data)
        [1] "matrix" 

        > data
             [,1] [,2] [,3] [,4] [,5]
        [1,]   15   37   33   17   15
        [2,]   16   12   46   21   48
        [3,]    8   38    5    2   21
        [4,]   17    7   25   24   40
        [5,]    1   32   16   17   36
        
        ## Use makeCacheMatrix to create special matrix
        > spec_data <- makeCacheMatrix(data) 

        > class(spec_data)
        [1] "list"        

        > spec_data$get()
             [,1] [,2] [,3] [,4] [,5]
        [1,]   15   37   33   17   15
        [2,]   16   12   46   21   48
        [3,]    8   38    5    2   21
        [4,]   17    7   25   24   40
        [5,]    1   32   16   17   36

        ## Check if data is changed
        > spec_data$haschanged()
        [1] TRUE

        ## Changed because inverse not yet being updated.
        > spec_data$getinverse()
        NULL

        ## Use cacheSolve to get inverse matrix
        ## Need to compute inverse matrix.               
        > cacheSolve(spec_data)
        Cache not found or outdated. Compute inverse matrix. Slow approach.
                     [,1]         [,2]        [,3]         [,4]         [,5]
        [1,]  0.006549707 -0.013938591  0.04318357  0.049634555 -0.064484181
        [2,]  0.011980302 -0.008309649  0.01122619 -0.008565660  0.009056527
        [3,]  0.012913832  0.034951318 -0.01510992 -0.037062428 -0.001987924
        [4,]  0.028324576 -0.052482545 -0.05017336  0.045868299  0.036477833
        [5,] -0.029946069  0.017023043  0.01923011  0.001047343  0.005176637

        ## Note that haschanged return FALSE because inverse matrix is calculated.
        > spec_data$haschanged()
        [1] FALSE
        
        ## This time use cache instead of compute inverse matrix. 
        > cacheSolve(spec_data)
        Cache found and still valid. Use cache inverse matrix. Fast approach.
                     [,1]         [,2]        [,3]         [,4]         [,5]
        [1,]  0.006549707 -0.013938591  0.04318357  0.049634555 -0.064484181
        [2,]  0.011980302 -0.008309649  0.01122619 -0.008565660  0.009056527
        [3,]  0.012913832  0.034951318 -0.01510992 -0.037062428 -0.001987924
        [4,]  0.028324576 -0.052482545 -0.05017336  0.045868299  0.036477833
        [5,] -0.029946069  0.017023043  0.01923011  0.001047343  0.005176637
        
        ## New invertible matrix of 4 x 4.
        > data2 <- matrix(sample(1:50, size=16, replace = TRUE), nrow=4, ncol=4)
        > data2
             [,1] [,2] [,3] [,4]
        [1,]   19   50   25   14
        [2,]   46   23   12   20
        [3,]   25   15    9    4
        [4,]   20   22   39   12

        ## Update spec_data using set() function
        > spec_data$set(data2)

        ## Check if matrix changed.  Return TRUE.
        > spec_data$haschanged()
        [1] TRUE

        ## Although cache exist, but cacheSolve will re-compute because outdated.
        ## Note the matrix dimension changed from 5 x 5 to 4 x 4.
        > cacheSolve(spec_data)
        Cache not found or outdated. Compute inverse matrix. Slow approach.
                     [,1]         [,2]        [,3]         [,4]
        [1,] -0.014618291  0.001897313  0.04977935 -0.002700632
        [2,]  0.029204873 -0.012467299  0.01551674 -0.018465766
        [3,] -0.011024496 -0.015199641  0.00789578  0.035562720
        [4,]  0.006651164  0.069093359 -0.13707423  0.006109452

        ## Inverse matrix had being updated so haschanged function return FALSE. 
        > spec_data$haschanged()
        [1] FALSE


        ## Second time will use cache.
        > cacheSolve(spec_data)
        Cache found and still valid. Use cache inverse matrix. Fast approach.
                     [,1]         [,2]        [,3]         [,4]
        [1,] -0.014618291  0.001897313  0.04977935 -0.002700632
        [2,]  0.029204873 -0.012467299  0.01551674 -0.018465766
        [3,] -0.011024496 -0.015199641  0.00789578  0.035562720
        [4,]  0.006651164  0.069093359 -0.13707423  0.006109452
 
***
 