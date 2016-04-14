library(MDPtoolbox)

val_v = c()
val_iter=c()
val_p=c()
val_time = c()
pol_v = c()
pol_iter=c()
pol_p=c()
pol_time = c()
q_v = c()
q_p=c()

for(i in 10:1000){
  mdp.forest = mdp_example_forest(i, i, 0.1*i)
  
  mdp.val = mdp_value_iteration(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, epsilon=0.01, max_iter = 100)
  mdp.val.eval = mdp_eval_policy_TD_0(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, policy=mdp.val$policy)
  val_iter = append(val_iter, mdp.val$iter)
  val_p = append(val_p, mean(mdp.val$policy))
  val_v = append(val_v, sum(mdp.val$V))
  val_time = append(val_time, mdp.val$time)
  
  mdp.pol = mdp_policy_iteration(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, max_iter = 100, policy0 = rep(0,i))
  mdp.pol.eval = mdp_eval_policy_TD_0(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, policy=mdp.pol$policy)
  pol_iter = append(pol_iter, mdp.pol$iter)
  pol_p = append(pol_p, mean(mdp.pol$policy))
  pol_v = append(pol_v, sum(mdp.pol$V))
  pol_time = append(pol_time, mdp.pol$time)
  
  mdp.Q = mdp_Q_learning(P=mdp.forest$P, R=mdp.forest$R, discount=0.9)
  mdp.Q.eval = mdp_eval_policy_TD_0(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, policy=mdp.Q$policy, N = 100000)
  q_v = append(q_v, sum(mdp.Q$V))
  q_p = append(q_p, mean(mdp.Q$policy))
  print("loop1")
  print(i)
}

q_v2 = c()
q_p2 = c()

mdp.forest = mdp_example_forest(100, 100, 5)
mdp.Q = mdp_Q_learning(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, N=100000000)
mdp.Q.eval = mdp_eval_policy_TD_0(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, policy=mdp.Q$policy)
mdp.Q.val = sum(mdp.Q$V[mdp.Q$policy==2])

mdp.pol = mdp_policy_iteration(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, max_iter = 100, policy0 = rep(0,100))
mdp.pol.eval = mdp_eval_policy_TD_0(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, policy=mdp.pol$policy)
mdp.pol.val = sum(mdp.pol$V[mdp.pol$policy==2])

mdp.val = mdp_value_iteration(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, epsilon=0.01, max_iter = 100)
mdp.val.eval = mdp_eval_policy_TD_0(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, policy=mdp.val$policy)
mdp.val.val = sum(mdp.val$V[mdp.val$policy==2])


#for(i in 10000:100000){
#  print("loop2")
#  print(i)
#  mdp.Q = mdp_Q_learning(P=mdp.forest$P, R=mdp.forest$R, discount=0.9, N=i)
#  q_v2 = append(q_v2, sum(mdp.Q$V))
#  q_p2 = append(q_p2, mean(mdp.Q$policy))
#}

