# Predict the normalized probabilities of test data with given BN.
predict.test.probs <- function(my.bn) {
    predict.probs(my.bn, test.data)
}

# Predict the normalized probabilities of dev data with given BN.
predict.dev.probs <- function(my.bn) {
    predict.probs(my.bn, dev.data)
}

predict.probs <- function(my.bn, event.data) {
    probs <- probabilities.of.events(my.bn, event.data)
    return(probs / sum(probs))
}

probabilities.of.events <- function(my.bn, event.data) {
    data <- data.matrix(event.data)
    param.obj <- my.bn$param
    results <- unname(sapply(1:length(param.obj), function(var.index) {
        chosen.param <- param.obj[[var.index]]
        local.probs <- chosen.param$prob
        vars.by.name <- c(chosen.param$node, chosen.param$parents)
        var.values <- data[, vars.by.name, drop=F]
        local.probs[(var.values - 1) %*% 3^(0:(length(vars.by.name) - 1)) + 1]
    }))
    apply(results, 1, prod)
}

# List the top probabilities in a given vector of probabilities.
top.probs <- function(probs, n=10) {
    res <- tail(sort(probs), n)
    names(res) <- tail(order(probs), n)
    return(res)
}

# Plot the normalized test probabilities predicted by given BN.
plot.predict.test.probs <- function(bn, ...) {
    plot.probs(predict.test.probs(bn), name="test")
}

# Plot the given vector of probabilities.
plot.probs <- function(probs, name="", ...) {
    barplot(probs, names.arg=1:length(probs),
            main=paste("Distribution of", name, "data"),
            xlab="Test data",
            ylab="Probability", ..., ylim=c(0,0.5))
}

# Use model averaging to predict test/dev probabilities given a list of BNs.
model.averaging.probs <- function(bns, event.data, prior=NULL) {
    num.of.bns <- length(bns)
    probs.per.bn <- lapply(bns, function(bn) predict.probs(bn, event.data))

    if (is.null(prior)) {
        prior <- rep(1/(num.of.bns), num.of.bns)
    }

    probs <- vector(length=nrow(event.data))
    for (i in 1:num.of.bns) {
        probs <- probs + (prior[[i]] * probs.per.bn[[i]])
    }

    return(probs)
}
