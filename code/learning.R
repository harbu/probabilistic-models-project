# Learn multiple BNs using grow shrink. Returns a list of BNs.
grow_shrink <- function(n, bootstrap=T, iss=default.iss, ...) {
    mclapply(1:n, function(x) my.bayesian.network(gs(select.data(bootstrap))),
             mc.cores=3)
}

# Learn multiple BNs using tabu search. Returns a list of BNs.
tabu_search <- function(n, bootstrap=T, iss=default.iss, ...) {
    mclapply(1:n, function(x) {
        my.bayesian.network(tabu(select.data(bootstrap), random.graph(LETTERS),
                                 score="bde", iss=iss, ...))
    }, mc.cores=1)
}


# Learn multiple BNs using simulated annealing. Returns a list of BNs.
simulated_annealing <- function(n, bootstrap=T) {
    best.graphs <- select.first.state(training.data, n * 50)[1:n]
    gc()
    mclapply(best.graphs, function(initial.graph) {
        sa(select.data(bootstrap), initial.graph, 10000, 30000)
    }, mc.cores=3)
}


# Helper functions below

select.data <- function(bootstrap) {
    if (bootstrap) {
        cat("bootstrap\n")
        bootstrap.data(training.data)
    } else {
        training.data
    }
}

select.first.state <- function(data, num.of.candidates=5000) {
    graphs <- random.graph(LETTERS, num.of.candidates)
    energy.func.with.data <- function(x) energy.function(x, data)
    energies <- unlist(mclapply(graphs, energy.func.with.data, mc.cores=3))
    graphs[order(energies)]
}

# Create a bootstrap sample from given data.
bootstrap.data <- function(data) {
    data[sample(1:nrow(data), nrow(data), replace=T), ]
}

