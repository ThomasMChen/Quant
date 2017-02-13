library(MDPtoolbox)

#Markov Decision Process 
test_forest <- mdp_example_forest()

#Check validity of desc
mdp_check(test_forest$P, test_forest$R)
test_forest

#Set Lambda
discount <- 0.95 

#Run Predictive Algorithim
result <- mdp_policy_iteration(test_forest$P, test_forest$R, discount)

cat("Expected Reward Value by age class:", result$V)
cat("Policy: ", result$policy)