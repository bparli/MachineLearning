# count ones

library(magrittr)
library(dplyr)
library(ggplot2)
library(gtable)
library(scales)
library(gsubfn) # for strapplyc
library(gridExtra) # use ?arrangeGrob, over ?grid.arrange

takeLastWord <- function(x) {strapplyc(x, "(\\w+)\\s*$")}

onemax <- read.csv(file.path("ABAGAIL", "jython", "countones.csv"), header=FALSE)
names(onemax) <- c("Algo_type", "Description", "Value", "File")
onemax$Algorithm <- onemax$Algo_type
# rename the levels for SA
levels(onemax$Algorithm) <- c("GA", "MIMIC", "RHC", "SA")

onemax$Description %<>% as.character
onemax$Description %<>% takeLastWord %>% unlist
onemax$File %<>% as.character

file2num <- function(x, num) {
  y <- x %>% strapplyc(., "(\\d+)") %>% unlist %>% as.numeric
  return(y[num])
}
# unique identifier
onemax$UID <- paste0(onemax$File, onemax$Algo_type, collpase="")
onemax$problemsize <- sapply(onemax$File, function(x) {file2num(x, 1)})

# get the best subset of stuff that we want for our final plots
final_dat <- onemax[onemax$Description == "results",] %>% group_by(Algorithm, problemsize) %>%
  filter(Value == max(Value)) %>%
  filter(rank(Value, ties.method="first")==1) %>%
  arrange(Algorithm, problemsize)

# filter by UID

# final data set prepare for plots...
final_set <- onemax[onemax$UID %in% final_dat$UID,]

baseline <- final_set[final_set$Algorithm == "RHC",]
baseline$Base <- baseline$Value
baseline$Value <- NULL
baseline$Algorithm <- NULL
baseline$Algo_type <- NULL
baseline$UID <- NULL
baseline$File <- NULL

final_all <- left_join(final_set, baseline)
final_all$Ratio <- final_all$Value / final_all$Base
final_all$Diff <- final_all$Value - final_all$Base

final_all$Algorithm <- factor(final_all$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
final_set$Algorithm <- factor(final_set$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))

final_set$Run <- final_set$problemsize
final_all$Run <- final_all$problemsize

##################


p1 <- ggplot(final_set[final_set$Description=="results",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous() + 
  ggtitle("Count Ones - Result") +
  theme_bw() +
  xlab("Problem Size") +
  ylab("Result Value") +
  theme(legend.position="bottom")

p2 <- ggplot(final_set[final_set$Description=="calls",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous() + 
  theme_bw() +
  ylab("Number of Function Calls") +
  xlab("Problem Size") +
  ggtitle("Number of Function Calls")



ggplot(final_set[final_set$Description=="time",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ggtitle("Count Ones Function - Time Taken") +
  theme_bw()



