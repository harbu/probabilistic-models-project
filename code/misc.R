# Calculate KL-divergence between a vector of true probabilities and predicted
# probabilities.
kl.divergence <- function(true.probs, predicted.probs) {
    sum(log(true.probs / predicted.probs) * true.probs)
}

# Generate completely random events.
gen.events <- function(n=1000) {
    num.of.nodes <- length(LETTERS)
    events <- matrix(as.factor(sample(1:3, num.of.nodes * n, replace=T)),
                     ncol=num.of.nodes, nrow=n)
    colnames(events) <- LETTERS
    return(as.data.frame(events))
}

# Generate data from a given Bayesian network.
gen.data <- function(my.bn, n=2500) {
    rbn(my.bn$param, n)
}

# Generate summary of a list of BNs.
summary.of.bns <- function(bns, full=F) {
    result <- list()

    bde.scores <- sapply(bns, my.score)
    aic.scores <- sapply(bns, function(bn) AIC(bn$model, training.data))
    bic.scores <- sapply(bns, function(bn) BIC(bn$model, training.data))

    result$num.of.bns <- length(bns)
    result$arc.counts <- table(sapply(bns, function(bn) nrow(arcs(bn$model))))
    result$bde.scores <- summary(bde.scores)
    result$bde.sd <- sd(bde.scores)
    result$aic.scores <- summary(aic.scores)
    result$aic.sd <- sd(aic.scores)
    result$bic.scores <- summary(bic.scores)
    result$bic.sd <- sd(bic.scores)

    if (full) {
        cv.likelihoods <- sapply(bns, cv.log.score)
        result$cv.likelihoods <- summary(cv.likelihoods)
        result$cv.sd <- sd(cv.likelihoods)
    }

    return(result)
}
