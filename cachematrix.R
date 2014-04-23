# The following provides a memoized function to calculate the inverse
# of a matrix.
#
# Ideally, we would use a higher-order function that could memoize an
# arbitrary calculation. However, since R has no built-in hash table or
# associative array type that can take complex objects like matrices as keys,
# implementing a memoization function that could operate on the cache
# inverse function with good performance seems to be non-trivial without
# bringing in external packages. (There is a CRAN package called `memoise'
# that can do this; it in turn relies on the `digest' package to calculate
# hashes of arbitrary R objects.)

# given a matrix x, return a list with the following functions:
# get() : return x
# inverse() : return the inverse of x
#             calculate the inverse only on the first call for a given
#             value of x; memoize and return the cached value on
#             subsequent calls
# set(m) : replace x with m and clear the memoized inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    getinverse <- function() {
        if (is.null(inverse)) {
            inverse <<- solve(x)
        }
        inverse
    }
    get <- function() x
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    list(get = get,
         set = set,
         inverse = getinverse)
}

# return the inverse of a matrix wrapper returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
    # accept additional arguments to match the expected prototype, but
    # ignore them; passing them to solve() would require additional logic
    # to cache the arguments and only return cached results when cacheSolve()
    # is called with the same arguments (i.e. full memoization)
    x$inverse()
}
