source('./ReserveDesign_R/getSite.r')
source('./ReserveDesign_R/binvec2dec.r')
source('./ReserveDesign_R/getstate.r')
source('./ReserveDesign_R/dec2binvec.r')
source('./ReserveDesign_R/mdp_example_reserve.r')
source('./ReserveDesign_R/explore_solution_reserve.r')
library(MDPtoolbox)

set.seed(1)
M <- round(matrix(nrow=7, ncol=20, data=runif(7*20,0,1)))

# Generate the transition and reward matrix
PR <- mdp_example_reserve(M, 0.2)
P <- PR$P
R <- PR$R

specs.val = mdp_value_iteration(P=P,R=R,discount=0.96, epsilon=0.001, max_iter = 1000)
specs.pol = mdp_policy_iteration(P=P,R=R,discount=0.96, max_iter = 1000, policy0 = rep(0,2187))

specs.Q = mdp_Q_learning(P=P, R=R, discount=0.96, N=1000000)

specs.val.eval = mdp_eval_policy_TD_0(P=P, R=R, discount=0.9, policy=specs.val$policy)
specs.pol.eval = mdp_eval_policy_TD_0(P=P, R=R, discount=0.9, policy=specs.pol$policy)
specs.Q.eval = mdp_eval_policy_TD_0(P=P, R=R, discount=0.9, policy=specs.Q$policy)

# Explore solution
explore_solution_reserve(numeric(7), specs.val$policy, M, P, R)
explore_solution_reserve(numeric(7), specs.pol$policy, M, P, R)
explore_solution_reserve(numeric(7), specs.Q$policy, M, P, R)