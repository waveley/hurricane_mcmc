############
#
# Loading Split Data
#
###########

load("generated_data/mcmc_split1.RData")
load("generated_data/mcmc_split2.RData")
load("generated_data/mcmc_split3.RData")
load("generated_data/mcmc_split4.RData")
load("generated_data/mcmc_split5.RData")

burn <- 5000
chain_length <- 10000

B_list <- c(res1[[1]], 
            res2[[1]], 
            res3[[1]], 
            res4[[1]], 
            res5[[1]])

mu_list <- c(res1[[2]], 
             res2[[2]], 
             res3[[2]], 
             res4[[2]], 
             res5[[2]])

sigmasq_list <- c(res1[[3]], 
                  res2[[3]], 
                  res3[[3]], 
                  res4[[3]], 
                  res5[[3]])

big_sigma_list <- c(res1[[4]], 
                    res2[[4]], 
                    res3[[4]], 
                    res4[[4]], 
                    res5[[4]])

res_list <- list(B = B_list[burn:chain_length],
                 mu = mu_list[burn:chain_length], 
                 sigmasq = sigmasq_list[burn:chain_length], 
                 big_sigma = big_sigma_list[burn:chain_length])


