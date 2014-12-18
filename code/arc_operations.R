# Given a list of BNs, produce a ranked arc listing ordered by occurrence.
ranked.arcs <- function(my.bns) {
    arc.strs <- custom.strength(models.from.bns(my.bns), LETTERS)

    pos.score <- apply(arc.strs[,c(3,4)], 1, function(row) {
        if (row["direction"] == 0) {
            (1/3) * row["strength"]
        } else {
            row["strength"] + row["direction"]
        }
    })

    arc.strs <- cbind(arc.strs, pos.score)
    arc.strs <- arc.strs[order(arc.strs[,"pos.score"], decreasing=T),]
    rownames(arc.strs) <- NULL

    return(arc.strs)
}

# Show arcs of given BN.
my.arcs <- function(my.bn) {
    arcs.with.strengths <- arc.strength(my.bn$model, training.data,
                                        criterion="bde", iss=default.iss)
    arcs.with.strengths[order(arcs.with.strengths[, 3]), ]

}

# Compare two ranked arc listings and show how they differ.
compare.arc.rankings <- function(ranked.arcs1, ranked.arcs2) {
    unlist(unname(apply(ranked.arcs2, 1, function(r) {
        which(r[1] == ranked.arcs1[,1] & r[2] == ranked.arcs1[,2])
    })))
}

# Plot the comparison of two ranked arc listings.
plot.compare.arc.rankings <- function(ranked.arcs1, ranked.arcs2) {
    plot(compare.arc.rankings(ranked.arcs1, ranked.arcs2),
         xlab="First ranking", ylab="Second ranking")
}

# Calculate the area under the curve given a set of "true" arcs and a ranked
# listing of predicted arcs.
area.under.curve <- function(true.arcs, predicted.arcs) {

    # Choose arc columns just in case.
    predicted.arcs <- predicted.arcs[,1:2]

    # Determine occurrences of arcs
    occurences <- apply(predicted.arcs, 1, function(arc) {
        any(apply(true.arcs, 1, function(tarc) all(arc == tarc)))
    })

    # Calculate points of (change) in ROC
    cum.y <- 0
    points <- NULL
    for (o in occurences) {
        if (o) {
            cum.y <- cum.y + 1
        } else {
            points <- c(points, cum.y)
        }
    }
    height.per.point <- points / nrow(true.arcs)
    return(sum(height.per.point) / length(height.per.point))

}


# Produce a list of all possible arcs.
all.possible.arcs <- function() {
    arcs <- as.matrix(expand.grid(LETTERS, LETTERS, stringsAsFactors=F))

    # Remove arcs from var to itself
    arcs <- arcs[!(arcs[, 1] == arcs[, 2]), ]
    return(arcs)
}
