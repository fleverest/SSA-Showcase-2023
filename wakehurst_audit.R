library(ggplot2)
library(elections.dtree)
library(prefio)

ballots <- sample(as.preferences(read_preflib("nswla/00058-00000273.soi", from_preflib = TRUE)))
batcher_factory <- function() {
  batch_size <- 100
  i <- 0
  function() {
    bs <- ballots[(batch_size * i + 1):(batch_size*(i+1))]
    i <<- i + 1
    return(bs)
  }
}

pi <- dirichlet_tree$new(candidates = names(ballots))

ps_post <- matrix(nrow = 0, ncol = 3)
b <- batcher_factory()
for (i in 1:floor(length(ballots)/100)) {
  print(paste0(i, "/", floor(length(ballots)/100)))
  pi$update(b())
  ps <- pi$sample_posterior(n_elections = 500, n_ballots <- length(ballots), n_threads = 12)
  ps_post <- rbind(ps_post, cbind(i*100, ps, names(ps)))
}

rownames(ps_post) <- NULL
colnames(ps_post) <- c("Observations", "Posterior", "Candidate")
ps_post <- as.data.frame(ps_post)
ps_post$Observations <- as.numeric(ps_post$Observations)
ps_post$Posterior <- as.numeric(ps_post$Posterior)

write.csv(ps_post, "wakehurst_audit.csv")
ggplot(ps_post) + geom_line(aes(color=Candidate, x=Observations, y=Posterior)) + xlim(0, 2000)
