sim.ane.all.arcs <- all.possible.arcs()

energy.function <- function(graph, data) {
    -score(graph, data, type="bde", iss=default.iss)
}

temperature.function <- function(last.temperature) {
    last.temperature * 0.97
}

acceptance.probability <- function(energy, new.energy, temp) {
    ifelse(new.energy < energy, 1, exp((1/temp) * (energy - new.energy)))
}

pick.random.neighbour <- function(graph, debug) {
    prob <- sample(1:3, 1)
    graph.is.empty <- nrow(arcs(graph)) == 0

    repeat {
        if (prob == 1 || graph.is.empty) { # Create new arc
            repeat {
                index <- sample(nrow(sim.ane.all.arcs), 1)
                arc <- sim.ane.all.arcs[index, ]
                if (graph.is.empty || !any(apply(arc == arcs(graph), 1, all))) break
            }
            new.graph <- set.arc(graph, arc[1], arc[2], check.cycles=F)
            if (acyclic(new.graph)) {
                if (debug) cat("add", arc[1], "->", arc[2], "\n")
                return(new.graph)
            }
        } else {
            arcs.of.graph <- arcs(graph)
            index <- sample(nrow(arcs.of.graph), 1)
            arc <- arcs.of.graph[index, ]

            if (prob == 2) { # Reverse arc
                if (debug) cat("reverse", arc[1], "->", arc[2], "\n")
                new.graph <- reverse.arc(graph, arc[1], arc[2], check.cycles=F)
                if (acyclic(new.graph)) {
                    return(new.graph)
                }
            } else {
                if (debug) cat("drop", arc[1], "->", arc[2], "\n")
                return(drop.arc(graph, arc[1], arc[2]))
            }
        }
    }
}


sa <- function(data, initial.state, max.steps, min.energy, debug=F, progress.debug=T) {
    state <- initial.state
    energy <- energy.function(state, data)
    best.state <- state
    best.energy <- energy
    temperature <- 10

    initial.energy <- energy # for stats

    if (debug) {
        cat("Initial arcs:   ")
        print(arcs(state))
        cat("Initial energy: ", energy, "\n\n")
    }

    step <- 0
    while (step < max.steps && energy > min.energy) {

        if (debug) cat("Begin step ", step, "\n")

        temperature <- temperature.function(temperature)
        new.state <- pick.random.neighbour(state, debug)
        new.energy <- energy.function(new.state, data)
        prob <- acceptance.probability(energy, new.energy, temperature)
        prob.threshold <- runif(1)

        if (debug) {
            cat("temperature     ", temperature, "\n")
            cat("new energy      ", new.energy, "\n")
            cat("acceptance prob.", prob, "\n")
            cat("threshold  prob.", prob.threshold, "\n")
        }


        if (prob > prob.threshold) {
            if (debug) cat("new state & energy set\n")
            state <- new.state
            energy <- new.energy
        }

        if (new.energy < best.energy) {
            if (debug) cat("new _best_ state & energy set\n")
            best.state <- new.state
            best.energy <- new.energy
        }

        step <- step + 1

        if (debug) {
            cat("Best energy: ", best.energy, "\n")
            cat("#arcs in best graph: ", narcs(best.state), "\n")
            cat("#arcs in current graph:", narcs(state), "\n\n")
            readLines("stdin", n = 1)
        }

        if (progress.debug && (step %% 200 == 0)) {
            cat(step/max.steps, " %\n")
        }
    }

    cat("Managed to improve start state by \n", initial.energy - best.energy)
    return(my.bayesian.network(best.state))
}
