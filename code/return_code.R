best.arcs <- read.table("../returned_materials/AndrewsEric_3_arcs.txt")
best.probs <- read.probs.from.disk("../returned_materials/AndrewsEric_3_probs.txt")
x <- sapply(sa.bns, function(bn) all(predict.test.probs(bn) < 0.3))

bde.score.indices <- order(sapply(sa.bns, my.score), decreasing=F)
log.score.indices <- order(sapply(sa.bns, cv.log.score), decreasing=F)
ind <- order(sapply(1:length(sa.bns), function(i) which(i == log.score.indices) + which(i == bde.score.indices)), decreasing=T)
