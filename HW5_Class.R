# HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

# Validity Method
setValidity('sparse_numeric', function(object) {
  # positions must be within 1:length
  if (any(object@pos < 1L | object@pos > object@length)) {
    return('Positions must be between 1 and object length.')
  }
  # values and positions must match in size
  if (length(object@value) != length(object@pos)) {
    return('Value and position vectors must be of the same length.')
  }
  TRUE
})

# Coercion Methods
setAs('numeric', 'sparse_numeric', function(from) {
  pos <- which(from != 0)
  vals <- from[pos]
  new('sparse_numeric',
      value = vals,
      pos = as.integer(pos),
      length = as.integer(length(from)))
})

setAs('sparse_numeric', 'numeric', function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0)
    out[from@pos] <- from@value
  out
})

# Helper for same-length check
check_length <- function(x, y) {
  if (x@length != y@length)
    stop('Sparse vectors must be of the same length.')
}

# Define Generics
setGeneric('sparse_add', function(x, y, ...) standardGeneric('sparse_add'))
setGeneric('sparse_sub', function(x, y, ...) standardGeneric('sparse_sub'))
setGeneric('sparse_mult', function(x, y, ...) standardGeneric('sparse_mult'))
setGeneric('sparse_crossprod', function(x, y, ...) standardGeneric('sparse_crossprod'))

## sparse_add
setMethod('sparse_add', c('sparse_numeric', 'sparse_numeric'), function(x, y) {
  check_length(x, y)
  all_pos <- c(x@pos, y@pos)
  all_val <- c(x@value, y@value)
  summed <- tapply(all_val, all_pos, sum)
  new('sparse_numeric',
      value = as.numeric(summed),
      pos = as.integer(names(summed)),
      length = x@length)
})

## sparse_sub
setMethod('sparse_sub', c('sparse_numeric', 'sparse_numeric'), function(x, y) {
  check_length(x, y)
  all_pos <- c(x@pos, y@pos)
  all_val <- c(x@value, -y@value)
  diffed <- tapply(all_val, all_pos, sum)
  new('sparse_numeric',
      value = as.numeric(diffed),
      pos = as.integer(names(diffed)),
      length = x@length)
})

## sparse_mult
setMethod('sparse_mult', c('sparse_numeric', 'sparse_numeric'), function(x, y) {
  check_length(x, y)
  common <- intersect(x@pos, y@pos)
  if (length(common) == 0L) {
    return(new('sparse_numeric', value = numeric(0), pos = integer(0), length = x@length))
  }
  xv <- x@value[match(common, x@pos)]
  yv <- y@value[match(common, y@pos)]
  new('sparse_numeric',
      value = xv * yv,
      pos = as.integer(common),
      length = x@length)
})

## sparse_crossprod
setMethod('sparse_crossprod', c('sparse_numeric', 'sparse_numeric'), function(x, y) {
  check_length(x, y)
  common <- intersect(x@pos, y@pos)
  if (length(common) == 0L) return(0)
  xv <- x@value[match(common, x@pos)]
  yv <- y@value[match(common, y@pos)]
  sum(xv * yv)
})

# Operator Overloads
setMethod('+', c('sparse_numeric', 'sparse_numeric'), 
          function(e1, e2) sparse_add(e1, e2))

setMethod('-', c('sparse_numeric', 'sparse_numeric'),
          function(e1, e2) sparse_sub(e1, e2))

setMethod('*', c('sparse_numeric', 'sparse_numeric'),
          function(e1, e2) sparse_mult(e1, e2))


# Show Method
setMethod('show', 'sparse_numeric', function(object) {
  cat('Sparse Numeric Vector of length', object@length, '\n')
  if (length(object@pos) > 0) {
    cat('Non-zero elements:\n')
    print(data.frame(position = object@pos, value = object@value))
  } else {
    cat('All zeros.\n')
  }
})

# Plot Method (overlapping non-zero elements)
setMethod('plot', c('sparse_numeric', 'sparse_numeric'), function(x, y, ...) {
  check_length(x, y)
  common <- intersect(x@pos, y@pos)
  if (length(common) == 0L) {
    cat('No overlapping non-zero positions.\n')
    return(invisible(NULL))
  }
  plot(common, x@value[match(common, x@pos)], col = 'green', pch = 19,
       xlab = 'Position', ylab = 'Value', main = 'Overlapping Non-zero Elements', ...)
  points(common, y@value[match(common, y@pos)], col = 'orange', pch = 17)
  legend('topright', legend = c('x', 'y'), col = c('green', 'orange'), pch = c(19, 17))
})

# Extra Method: sparse_sum
setGeneric('sparse_sum', function(x, ...) standardGeneric('sparse_sum'))
setMethod('sparse_sum', 'sparse_numeric', function(x) sum(x@value))