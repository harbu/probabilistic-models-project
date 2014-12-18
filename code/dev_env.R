library("multicore")
library("bnlearn")
library("Rgraphviz")

source("arc_operations.R")
source("probability_operations.R")
source("file_operations.R")
source("misc.R")

# constants
default.iss <- 1

# My own Bayesian network class wrapper that encapsulates the model structure
# along with fitted paramters (Bayes, ess=1).
my.bayesian.network <- function(bn, iss=default.iss) {
    my.bn <- list()
    my.bn$model <- bn

    # Fit parameters of Bayesian network to given data.
    my.bn$param <- bn.fit(my.bn$model, training.data, method="bayes", iss=iss)

    class(my.bn) <- "my.bayesian.network"
    return(my.bn)
}

# Given a list of BNs, relearn their parameters with a different equivalent
# sample size (= imaginary sample size).
relearn.params <- function(bns, new.iss) {
    lapply(models.from.bns(bns), function(x) my.bayesian.network(x, new.iss))
}

# Plot a Bayesian network in a visually appealing graph.
plot.my.bayesian.network <- function(my.bn) {
    graphviz.plot(my.bn$model, shape="ellipse")
}

# Build an averaged network from a given list of BNs.
averaged.bn <- function(my.bns) {
    arc.strs <- custom.strength(models.from.bns(my.bns), LETTERS)
    model <- averaged.network(arc.strs)
    return(my.bayesian.network(model))
}

# From a list of "my.bayesian.network" objects, return only the model part.
models.from.bns <- function(my.bns) {
    lapply(my.bns, function(x) x$model)
}

# Calculate BDE score of network.
my.score <- function(my.bn) {
    score(my.bn$model, training.data, type="bde", iss=default.iss)
}

# Create a custom golden standard network, generate a new set of training data
# from it, generate a random set of test data and predict its probabilities
# with the given custom golden standard network.
change.context.to.custom.golden.standard <- function(n) {
    all.arcs <- all.possible.arcs()

    # Generate random graph
    graph <- random.graph(LETTERS)
    num.of.arcs <- nrow(arcs(graph))
    while (num.of.arcs < n) {

        # Choose arc to add
        repeat {
            index <- sample(nrow(all.arcs), 1)
            arc <- all.arcs[index, ]
            if (!any(apply(arc == arcs(graph), 1, all))) break
        }

        new.graph <- set.arc(graph, arc[1], arc[2], check.cycles=F)

        if (acyclic(new.graph)) {
            graph <- new.graph
            num.of.arcs <- nrow(arcs(graph))
        }
    }

    # "Learn" parameters from current training data.
    golden.standard <- my.bayesian.network(graph)

    # Set-up new training data.
    training.data <<- gen.data(golden.standard)

    # Set-up new test data and probabilities.
    test.data <<- gen.events()
    test.probs <<- predict.test.probs(golden.standard)

    return(golden.standard)
}

# Perform 10-fold cross validation to learn parameters of given Bayesian
# network, and calculate total prediction error of held-out sets.
cv.log.score <- function(my.bn, k=10, iss=default.iss) {
    set.seed(614125)
    n <- nrow(training.data)
    model <- my.bn$model
    break.points <- c(seq(1, n, n / k), n+1)
    rows <- sample(1:n, n)

    sum(sapply(1:(length(break.points) - 1), function(j) {
        start.point <- break.points[j]
        end.point <- break.points[j+1] - 1
        validation.rows <- rows[start.point:end.point]

        validation.set <- training.data[validation.rows, ]
        training.set <- training.data[-validation.rows, ]

        fit <- bn.fit(model, training.set, method="bayes", iss=iss)
        sum(log(probabilities.of.events(list(param=fit), validation.set)))
    }))
}

# Compare two BNs. Return some simple statistics.
my.compare <- function(target.bn, learned.bn, show.spec.arcs=F) {
    res <- list()
    res$arcs <- compare(target.bn$model, learned.bn$model, arcs=show.spec.arcs)
    res$shd <- shd(target.bn$model, learned.bn$model)
    res$hamming <- hamming(target.bn$model, learned.bn$model)
    return(res)
}


# Initialization below

training.data <- read.data("data/training_data.txt")
test.data <- read.data("data/test_data.txt")
dev.data <- read.data("data/devel_data.txt")
dev.probs <- read.probs.from.disk("data/devel_probs.txt", 1000)

# Simple Tabu search with BDE scoring
bn <- my.bayesian.network(tabu(training.data, score="bde", iss=default.iss))

source("learning.R")
source("genetic_learning.R")

