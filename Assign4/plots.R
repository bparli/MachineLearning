source(source('~/MachineLearning/Assign4/forest.R'))
source(source('~/MachineLearning/Assign4/preserve.R'))

time= data.frame(val_time, pol_time, x=c(1:991))

time_plot = ggplot(time, aes(x)) + 
  geom_line(aes(y = val_time, colour = "Value Time")) + 
  geom_line(aes(y = pol_time, colour = "Policy Time")) +
  labs(title="Compute Time", x="Number of States", y="Time") +
  theme(legend.position="bottom")+
  theme(axis.title=element_text(face="bold.italic"), title=element_text(face="bold"))

iters= data.frame(val_iter, pol_iter, x=c(1:991))
iters_plot = ggplot(iters, aes(x)) + 
  geom_line(aes(y = val_iter, colour = "Value Iterations")) + 
  geom_line(aes(y = pol_iter, colour = "Policy Iterations")) +
  labs(title="Iterations", x="Number of States", y="Iterations") +
  theme(legend.position="bottom") +
  theme(axis.title=element_text(face="bold.italic"), title=element_text(face="bold")) 

choice = data.frame(val_p, pol_p, q_p, x=c(1:991))
choice_plot = ggplot(choice, aes(x)) + 
  geom_line(aes(y = val_p, colour = "Value Choices")) + 
  geom_line(aes(y = pol_p, colour = "Policy Choices")) +
  geom_line(aes(y = q_p, colour = "Q Choices")) +
  labs(title="Average Policy Choice", x="Number of States", y="Choice") +
  theme(legend.position="bottom") +
  theme(axis.title=element_text(face="bold.italic"), title=element_text(face="bold")) 

eval = data.frame(val_v, pol_v, q_v, x=c(1:991))
eval_plot = ggplot(eval, aes(x)) + 
  geom_line(aes(y = val_v, colour = "Value Value")) + 
  geom_line(aes(y = pol_v, colour = "Policy Value")) +
  geom_line(aes(y = q_v, colour = "Q Value")) +
  labs(title="Final Evaluation", x="Number of States", y="Value") +
  theme(legend.position="bottom") +
  theme(axis.title=element_text(face="bold.italic"), title=element_text(face="bold")) 