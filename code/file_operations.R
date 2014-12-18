# Read in training / test data properly.
read.data <- function(filename) {
    read.table(filename, header=T, colClasses="factor")
}

# Write arc listing to disk.
write.arcs.to.disk <- function(filename, arcs) {
    write.table(arcs[,1:2], file=filename, quote=F, col.names=F, row.names=F)
}

# Write vector of probabilities to disk.
write.probs.to.disk <- function(filename, probs) {
    write.table(probs, file=filename, row.names=F, col.names=F)
}

# Read a vector of probabilities from disk. Do some basic validation to ensure
# that it really is a probability set.
read.probs.from.disk <- function(filename, expected.rows=1500) {
    probs <- read.table(filename)[,1]
    if (length(probs) != expected.rows) {
        stop("Number of probs not equal to ", expected.rows, ".")
    }
    if (any(is.na(probs))) stop("Contains N/A values.")
    if (abs(sum(probs) - 1) > 0.0000000001) stop("Doesn't sum up to one.")
    return(probs)
}
