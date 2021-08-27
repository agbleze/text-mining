### topic modelling
## Latent Dirichlet Allocation (LDA)
library(topicmodels)
library(tidytext)
data("AssociatedPress")
AssociatedPress

#LDA topic model with two topics
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
