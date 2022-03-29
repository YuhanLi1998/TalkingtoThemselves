setwd("/Users/liyuhan/python_practice/NetworkCons")

install.packages("igraph")
install.packages("ergm")
install.packages("statnet")
install.packages("sna")
library(statnet)
library(igraph)
library(sna)
library(ergm)

#######---------SampleTest---------#######
#######---------SampleTest---------#######
#######---------SampleTest---------#######

mydata <- read.csv("CCvideo_matrix/今天，美国正式退出巴黎协定.csv.csv",header=F,sep = ",")
mydata.matrix <- as.matrix(mydata)
g <- graph.adjacency(mydata.matrix,weighted=TRUE,mode = "directed")
graph.density(g)
reciprocity(g)
transitivity(g)

#Detect communities using betweenness (Girvan & Newman, 2002)
g <- graph.adjacency(mydata.matrix,weighted=NULL,mode = "undirected")

# We should translate the mode from "directed" into "undirected"
# and translate the weighted choice from "TRUE" to "NULL"
# since Modularity is implemented for undirected graph
# and Modularity calculation with weighted edge betweenness community
# detection might not make sense -- modularity treats edge weights as 
# similarities while edge betwenness treats them as distances

cmt = edge.betweenness.community(g)
cmt$membership
View(cmt$membership)

#Calculate modularity score ----quality of community detection
modularity(cmt)

#######---------loop for CC video---------#######
#######---------loop for CC video---------#######
#######---------loop for CC video---------#######

#代码思路：先遍历文件夹(list.files),然后通过循环依次读写(read.csv)
#读取同一目录下的所有文件
filename <- list.files("/Users/liyuhan/python_practice/NetworkCons/CCvideo_matrix",
                       pattern = ".csv", full.names = TRUE)

#calculate density degree
results <- data.frame(filename="filename", density = "density")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline1 <- data.frame(t(c(filename=filename[i], density=graph.density(g))))
  results <- rbind(results, newline1)
}
write.table(results, file="CCsamplevideo_density.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")


#calculate transitivity degree
results <- data.frame(filename="filename", transitivity = "transitivity")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], transitivity=transitivity(g))))
  results <- rbind(results, newline2)
}
write.table(results, file="CCsamplevideo_transitivity.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")


#calculate reciprocity degree
results <- data.frame(filename="filename", reciprocity = "reciprocity")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline3 <- data.frame(t(c(filename=filename[i], reciprocity=reciprocity(g))))
  results <- rbind(results, newline3)
}
write.table(results, file="CCsamplevideo_reciprocity.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate average constraint degree
results <- data.frame(filename="filename", constraint = "constraint")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], constraint=mean(constraint(g)))))
  results <- rbind(results, newline2)
}
write.table(results, file="CCsamplevideo_constraint.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate average in-degree
results <- data.frame(filename="filename", in_degree = "In-degree")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], in_degree=max(degree(g,mode="in")))))
  results <- rbind(results, newline2)
}
write.table(results, file="CCsamplevideo_in_degree.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate average out-degree
results <- data.frame(filename="filename", out_degree = "out-degree")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], out_degree=max(degree(g,mode="out")))))
  results <- rbind(results, newline2)
}
write.table(results, file="CCsamplevideo_out_degree.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate the number of vertices
results <- data.frame(filename="filename", vertice = "vertice")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], vertice=vcount(g))))
  results <- rbind(results, newline2)
}
write.table(results, file="CCsamplevideo_vertice.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")


#calculate the number of edges
results <- data.frame(filename="filename", edge = "edge")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], edge=ecount(g))))
  results <- rbind(results, newline2)
}
write.table(results, file="CCsamplevideo_edge.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate the biggest betweenness
results <- data.frame(filename="filename", betweenness = "betweenness")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], betweenness=max(betweenness(g)))))
  results <- rbind(results, newline2)
}
write.table(results, file="CCsamplevideo_betweenness.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#######---------loop for GW video---------#######
#######---------loop for GW video---------#######
#######---------loop for GW video---------#######

filename <- list.files("/Users/liyuhan/python_practice/NetworkCons/GWvideo_matrix",
                       pattern = ".csv", full.names = TRUE)

#calculate density degree
results <- data.frame(filename="filename", density = "density")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline1 <- data.frame(t(c(filename=filename[i], density=graph.density(g))))
  results <- rbind(results, newline1)
}
write.table(results, file="GWsamplevideo_density.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")


#calculate transitivity degree
results <- data.frame(filename="filename", transitivity = "transitivity")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], transitivity=transitivity(g))))
  results <- rbind(results, newline2)
}
write.table(results, file="GWsamplevideo_transitivity.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate reciprocity degree
results <- data.frame(filename="filename", reciprocity = "reciprocity")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline3 <- data.frame(t(c(filename=filename[i], reciprocity=reciprocity(g))))
  results <- rbind(results, newline3)
}
write.table(results, file="GWsamplevideo_reciprocity.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate average constraint degree
results <- data.frame(filename="filename", constraint = "constraint")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], constraint=mean(constraint(g)))))
  results <- rbind(results, newline2)
}
write.table(results, file="GWsamplevideo_constraint.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate average in-degree
results <- data.frame(filename="filename", in_degree = "In-degree")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], in_degree=max(degree(g,mode="in")))))
  results <- rbind(results, newline2)
}
write.table(results, file="GWsamplevideo_in_degree.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate average out-degree
results <- data.frame(filename="filename", out_degree = "out-degree")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], out_degree=max(degree(g,mode="out")))))
  results <- rbind(results, newline2)
}
write.table(results, file="GWsamplevideo_out_degree.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate the number of vertices
results <- data.frame(filename="filename", vertice = "vertice")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], vertice=vcount(g))))
  results <- rbind(results, newline2)
}
write.table(results, file="GWsamplevideo_vertice.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")


#calculate the number of edges
results <- data.frame(filename="filename", edge = "edge")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], edge=ecount(g))))
  results <- rbind(results, newline2)
}
write.table(results, file="GWsamplevideo_edge.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#calculate the biggest betweenness
results <- data.frame(filename="filename", betweenness = "betweenness")

for(i in 1:length(filename)){
  file <- read.csv(filename[i], header=FALSE, sep = ",")
  matrix <- as.matrix(file)
  g <- graph.adjacency(matrix, weighted=TRUE,mode = "directed")
  newline2 <- data.frame(t(c(filename=filename[i], betweenness=max(betweenness(g)))))
  results <- rbind(results, newline2)
}
write.table(results, file="GWsamplevideo_betweenness.csv", row.names = FALSE,
            append = FALSE, col.names = TRUE, sep = ",")

#-------------------Propensity Score Matching-------------------#
#-------------------Propensity Score Matching-------------------#
#-------------------Propensity Score Matching-------------------#

install.packages("MatchIt")
library(MatchIt)
library(dplyr)
library(ggplot2)

sum_data <- read.csv("CalculateResult/SUM_analyse_data_5.csv")
del <- which(sum_data$Content_type == '3') # select content_type is neither climate politics nor climate science
sum_data <- sum_data[-del,] # delete these rows
#sum_data$Content_type[which(sum_data$Content_type == '3')] <- '0'
sum_data$Content_type[which(sum_data$Content_type == '2')] <- '0' #set 'climate science' as control/non-treated 0

###### Pre-analysis using non-matched data
## Difference-in-means: outcome variable

sum_data %>% 
  group_by(Content_type) %>% 
  summarize(filename = n(),
            vertice = mean(Number_of_vertice),
            centrality = mean(in_degree),
            density = mean(density),
            transitivity = mean(transitivity),
            reciprocity = mean(reciprocity),
            constraint = mean(constraint),
            topics = mean(Number_of_topics),
            betweenness = mean(betweenness))

###### Difference-in-means: pre-treatment covariates
sum_data_covs <- c('Verified_status', 'Government_account', 'Follower',
                   'Followee', 'Number_of_thumbs', 'Number_of_comments',
                   'Number_of_views', 'Video_length', 'Published_days')
sum_data %>% 
  group_by(Content_type) %>% 
  select(one_of(sum_data_covs)) %>% 
  summarise_all(funs(sd(., na.rm = T)))

with(sum_data, t.test(Verified_status ~ Content_type))
with(sum_data, t.test(Government_account ~ Content_type))
with(sum_data, t.test(Follower ~ Content_type))
with(sum_data, t.test(Followee ~ Content_type))
with(sum_data, t.test(Number_of_thumbs ~ Content_type))
with(sum_data, t.test(Number_of_comments ~ Content_type))
with(sum_data, t.test(Number_of_views ~ Content_type))
with(sum_data, t.test(Video_length ~ Content_type))
with(sum_data, t.test(Published_days ~ Content_type))

str(sum_data) # to see types of all columns
sum_data$Content_type <- as.numeric(sum_data$Content_type) # translate "Content_type" from character to numeric

sum_data <- sum_data %>% mutate(Number_of_views_1k = Number_of_views / 1000)
sum_data <- sum_data %>% mutate(Follower_1k = Follower / 1000)
sum_data <- sum_data %>% mutate(Video_length_1k = Video_length / 1000)
sum_data <- sum_data %>% mutate(Number_of_thumbs_100k = Follower / 100000)
sum_data <- sum_data %>% mutate(Number_of_comments_1k = Follower / 1000)
m_ps <- glm(Content_type ~ Verified_status + Follower_1k + Followee + Government_account + 
              Number_of_views_1k + Video_length_1k + Published_days + 
              Number_of_thumbs_100k + Number_of_comments_1k, family = binomial(),
            data = sum_data)
summary(m_ps)

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     Content_type = m_ps$model$Content_type)
head(prs_df)

### Examining the region of common support
labs <- paste("Video content type:", c("Climate Politics", "Climate Science"))
prs_df %>% 
  mutate(politics = ifelse(Content_type == 1, labs[1], labs[2])) %>% 
  ggplot(aes(x = pr_score)) + 
  geom_histogram(color = "white") + 
  facet_wrap(~ Content_type) + 
  xlab("Probability of choosing the theme of Climate Politics") + 
  theme_bw()

### Executing a matching algorithm
sum_data_nomiss <- sum_data %>% # MatchIt does not allow missing values
  select(constraint, Number_of_vertice, in_degree, density, transitivity, reciprocity, 
         Number_of_topics, betweenness, Content_type, one_of(sum_data_covs)) %>% 
  na.omit()

mod_match <- matchit(Content_type ~ I(Verified_status^4) + Verified_status + 
                    I(Government_account^2) + Government_account + Follower + 
                    I(Follower^2) + Followee + Number_of_comments + Number_of_thumbs + 
                    Number_of_views + Video_length + Published_days, 
                    method = "nearest", data = sum_data_nomiss,
                    caliper = 0.8, ratio = 1)
dta_m <- match.data(mod_match)
dim(dta_m)

### Visual inspection
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'Number_of_views') dta$variable <- dta$variable / 10^3
  if (variable == 'Follower') dta$variable <- dta$variable / 10^3
  if (variable == 'Video_length') dta$variable <- dta$variable / 10^3
  if (variable == 'Number_of_comments') dta$variable <- dta$variable / 10^3
  if (variable == 'Number_of_thumbs') dta$variable <- dta$variable / 10^5
  dta$Content_type <- as.factor(dta$Content_type)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = Content_type)) + 
    geom_point(alpha = 0.2, size = 1.3) + 
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support) + 
    scale_color_hue(name = "Video Theme",
                    breaks = c("0", "1"),
                    labels = c("Control", "Treated"))
}

library(gridExtra)
grid.arrange(
  fn_bal(dta_m, "Follower"),
  fn_bal(dta_m, "Verified_status") + theme(legend.position = "none"),
  fn_bal(dta_m, "Number_of_views"),
  fn_bal(dta_m, "Video_length") + theme(legend.position = "none"),
  fn_bal(dta_m, "Published_days"),
  fn_bal(dta_m, "Number_of_thumbs") + theme(legend.position = "none"),
  fn_bal(dta_m, "Number_of_comments"),
  fn_bal(dta_m, "Government_account") + theme(legend.position = "none"),
  widths = c(1, 0.8), nrow = 4
)

### Difference-in-means
dta_m %>% 
  group_by(Content_type) %>% 
  select(one_of(sum_data_covs)) %>% 
  summarise_all(funs(sd))

with(dta_m, t.test(Verified_status ~ Content_type))
with(dta_m, t.test(Government_account ~ Content_type))
with(dta_m, t.test(Follower ~ Content_type))
with(dta_m, t.test(Number_of_views ~ Content_type))
with(dta_m, t.test(Video_length ~ Content_type))
with(dta_m, t.test(Published_days ~ Content_type))
with(dta_m, t.test(Number_of_comments ~ Content_type))
with(dta_m, t.test(Number_of_thumbs ~ Content_type))

lapply(sum_data_covs, function(v) {
  t.test(dta_m[, v] ~ dta_m$Content_type)
})


### Estimating treatment effects
#### Firstly, we use OLS with or without covariates
with(dta_m, t.test(Number_of_topics ~ Content_type))

lm_treat1 <- lm(transitivity ~ Content_type, data = sum_data)
summary(lm_treat1)

lm_treated1 <- lm(transitivity ~ Content_type + I(Follower/10^3) + 
                  Verified_status + I(Number_of_views/10^3) + Video_length + 
                  Number_of_comments + Number_of_thumbs + Published_days
                  + Followee + Government_account, data = dta_m)
summary(lm_treated1)

lm_treat2 <- lm(density ~ Content_type, data = sum_data)
summary(lm_treat2)

lm_treated2 <- lm(density ~ Content_type + I(Follower/10^3) + 
                  Verified_status + I(Number_of_views/10^3) + Video_length +
                  Number_of_comments + Number_of_thumbs + Published_days 
                  + Followee + Government_account, data = dta_m)
summary(lm_treated2)

lm_treat3 <- lm(reciprocity ~ Content_type, data = sum_data)
summary(lm_treat3)

lm_treated3 <- lm(reciprocity ~ Content_type + I(Follower/10^3) + 
                    Verified_status + I(Number_of_views/10^3) + Video_length + 
                    Number_of_comments + Number_of_thumbs + Published_days + 
                    Followee + Government_account, data = dta_m)
summary(lm_treated3)

lm_treat4 <- lm(in_degree ~ Content_type, data = sum_data)
summary(lm_treat4)

lm_treated4 <- lm(in_degree ~ Content_type + I(Follower/10^3) + 
                    Verified_status + I(Number_of_views/10^3) + Video_length + 
                    Number_of_comments + Number_of_thumbs + Published_days + 
                    Followee + Government_account, data = dta_m)
summary(lm_treated4)

lm_treat5 <- lm(Number_of_topics ~ Content_type, data = sum_data)
summary(lm_treat5)

lm_treated5 <- lm(Number_of_topics ~ Content_type + I(Follower/10^3) + 
                    Verified_status + I(Number_of_views/10^3) + Video_length + 
                    Number_of_comments + Number_of_thumbs + Published_days + 
                    Followee  + Government_account, data = dta_m)
summary(lm_treated5)

lm_treat6 <- lm(constraint ~ Content_type, data = sum_data)
summary(lm_treat6)

lm_treated6 <- lm(constraint ~ Content_type + I(Follower/10^3) + 
                    Verified_status + I(Number_of_views/10^3) + Video_length + 
                    Number_of_comments + Number_of_thumbs + Published_days + 
                    Followee  + Government_account, data = dta_m)
summary(lm_treated6)

lm_treat7 <- lm(Number_of_vertice ~ Content_type, data = sum_data)
summary(lm_treat7)

lm_treated7 <- lm(Number_of_vertice ~ Content_type + I(Follower/10^3) + 
                    Verified_status + I(Number_of_views/10^3) + Video_length + 
                    Number_of_comments + Number_of_thumbs + Published_days,
                  data = dta_m)
summary(lm_treated7)

lm_treat8 <- lm(betweenness ~ Content_type, data = sum_data)
summary(lm_treat8)

lm_treated8 <- lm(betweenness ~ Content_type + I(Follower/10^3) + 
                    Verified_status + I(Number_of_views/10^3) + Video_length + 
                    Number_of_comments + Number_of_thumbs + Published_days + 
                    Followee, data = dta_m)
summary(lm_treated8)
