# 해당 코드는 R 버전 4.1.1에서 작성되었다.
# 본 논문의 통계 분석에서 활용한 코드와 데이터는 다음 웹페이지에서 확인할 수 있다.
# https://github.com/danielchoi5634/Replication-Data-for-Thesis
# RSiena 패키지에 대한 자세한 설명은 아래 웹페이지에서 찾을 수 있다.
# https://www.stats.ox.ac.uk/~snijders/siena/siena.html

library(RSiena) # version 1.3.0.1
library(parallel)

# this code uses parallel processing to speed up estimation process
# it is possible to estimate models without parallel processing by removing “useCluster=TRUE, nbrNodes=12” from the code
# however, replicating the results may not be possible in that case

# read network data and covariates

# setwd("C:/File Location")
discourse1 <- as.matrix(read.csv("제1기 2-mode.csv", header=TRUE)[ ,2:45])
discourse1[is.na(discourse1)] <- 0

discourse2 <- as.matrix(read.csv("제2기 2-mode.csv", header=TRUE)[ ,2:45])
discourse2[is.na(discourse2)] <- 0

discourse3 <- as.matrix(read.csv("제3기 2-mode.csv", header=TRUE)[ ,2:45])
discourse3[is.na(discourse3)] <- 0

actor.covariate <- as.matrix(read.csv("covariate(actor).csv")[ ,2:8])
concept.covariate <- as.matrix(read.csv("covariate(concept).csv")[ ,2:2])

colMeans(discourse1, na.rm=T)
colMeans(discourse2, na.rm=T)
colMeans(discourse3, na.rm=T)

# create RSiena objects from network and covariate data
(nractors <- dim(discourse1)[1])
(nrconcepts <- dim(discourse1)[2])
actors <- sienaNodeSet(nractors, nodeSetName="actors")
concepts <- sienaNodeSet(nrconcepts, nodeSetName="concepts")
discourse <- sienaDependent(array(c(discourse1, discourse2, discourse3),
                                  dim=c(nractors, nrconcepts, 3)),
                            "bipartite", nodeSet=c("actors", "concepts"))
discourse


# add covariates
colnames(actor.covariate)
group <- coCovar(actor.covariate[, 1], nodeSet="actors")
democrat <- coCovar(actor.covariate[, 2], nodeSet="actors")
republican <- coCovar(actor.covariate[, 3], nodeSet="actors")
executive <- coCovar(actor.covariate[, 4], nodeSet="actors")
press <- coCovar(actor.covariate[, 5], nodeSet="actors")
law <- coCovar(actor.covariate[, 6], nodeSet="actors")
civic<- coCovar(actor.covariate[, 7], nodeSet="actors")

colnames(concept.covariate)

evaluation <- coCovar(concept.covariate[, 1], nodeSet="concepts")

# create sienaData object
mydata <- sienaDataCreate(discourse, 
                          group, democrat, republican, executive, 
                          press, law, civic,
                          evaluation,
                          nodeSets = list(actors, concepts))


# model 1 : only endogenous effects
myeff.1 <- getEffects(mydata)
myeff.1 <- includeEffects(myeff.1, density, cycle4, inPop, outAct)
myeff.1

myalgorithm <- sienaAlgorithmCreate(projname = 'discourse', seed=1234, cond=FALSE)

model.1 <- siena07(myalgorithm, data = mydata, effects = myeff.1, useCluster=TRUE, nbrNodes=12)
model.1

# if convergence is lower than .25, it's good
# for individual convergence t-ratio, it is good when all values are lower than .10
# if either one is not good, estimate the model again
# reference at Ripley et al. (2021)

model.1.1 <- siena07(myalgorithm, data = mydata, effects = myeff.1, prevAns = model.1, useCluster=TRUE, nbrNodes=12)
model.1.1

wald.1 <- Multipar.RSiena(model.1.1, 3:6)
wald.1



# model 2: ego(official) and alter effects
myeff.2 <- getEffects(mydata)
myeff.2 <- includeEffects(myeff.2, density, cycle4, inPop, outAct)

myeff.2 <- includeEffects(myeff.2, egoX, name="discourse", interaction1="group")
myeff.2 <- includeEffects(myeff.2, egoX, name="discourse", interaction1="democrat")
myeff.2 <- includeEffects(myeff.2, egoX, name="discourse", interaction1="republican")
myeff.2 <- includeEffects(myeff.2, egoX, name="discourse", interaction1="executive")

myeff.2 <- includeEffects(myeff.2, altX, name="discourse", interaction1="evaluation")
myeff.2

myalgorithm <- sienaAlgorithmCreate(projname = 'discourse', seed=1234, cond=FALSE )

model.2 <- siena07(myalgorithm, data = mydata, effects = myeff.2, useCluster=TRUE, nbrNodes = 12)
model.2.1 <- siena07(myalgorithm, data = mydata, effects = myeff.2, prevAns = model.2, useCluster=TRUE, nbrNodes = 12)
model.2.1

wald.2 <- Multipar.RSiena(model.2.1, 3:11)
wald.2

# model 3: ego(official), alter, and interaction effects
myeff.3 <- getEffects(mydata )
myeff.3 <- includeEffects(myeff.3, density, cycle4, inPop, outAct)

myeff.3 <- includeEffects(myeff.3, egoX, name="discourse", interaction1="group")

myeff.3 <- includeEffects(myeff.3, egoX, name="discourse", interaction1="democrat")
myeff.3 <- includeEffects(myeff.3, egoX, name="discourse", interaction1="republican")
myeff.3 <- includeEffects(myeff.3, egoX, name="discourse", interaction1="executive")

myeff.3 <- includeEffects(myeff.3, altX, name="discourse", interaction1="evaluation")

myeff.3 <- includeInteraction(myeff.3, egoX, altX, name="discourse", interaction1=c("democrat", "evaluation"))

myeff.3 <- includeInteraction(myeff.3, egoX, altX, name="discourse", interaction1=c("republican", "evaluation"))
myeff.3 <- includeInteraction(myeff.3, egoX, altX, name="discourse", interaction1=c("executive", "evaluation"))
myeff.3

myalgorithm <- sienaAlgorithmCreate( projname = 'discourse', seed=1234, cond=FALSE )


model.3 <- siena07(myalgorithm, data = mydata, effects = myeff.3, useCluster=TRUE, nbrNodes = 12)
model.3.1 <- siena07(myalgorithm, data = mydata, effects = myeff.3, prevAns = model.3, useCluster=TRUE, nbrNodes = 12)
model.3.2 <- siena07(myalgorithm, data = mydata, effects = myeff.3, prevAns = model.3.1, useCluster=TRUE, nbrNodes = 12)
model.3.2

wald.3 <- Multipar.RSiena(model.3.2, 3:14)
wald.3 

# model 4: ego(unofficial) and alter effects
myeff.4 <- getEffects(mydata)
myeff.4 <- includeEffects(myeff.4, density, cycle4, inPop, outAct)

myeff.4 <- includeEffects(myeff.4, egoX, name="discourse", interaction1="group")
myeff.4 <- includeEffects(myeff.4, egoX, name="discourse", interaction1="press")
myeff.4 <- includeEffects(myeff.4, egoX, name="discourse", interaction1="law")
myeff.4 <- includeEffects(myeff.4, egoX, name="discourse", interaction1="civic")

myeff.4 <- includeEffects(myeff.4, altX, name="discourse", interaction1="evaluation")
myeff.4

myalgorithm <- sienaAlgorithmCreate( projname = 'discourse', seed=1234, cond=FALSE )

model.4 <- siena07(myalgorithm, data = mydata, effects = myeff.4, useCluster=TRUE, nbrNodes = 12)
model.4.1 <- siena07(myalgorithm, data = mydata, effects = myeff.4, prevAns = model.4, useCluster=TRUE, nbrNodes = 12)
model.4.2 <- siena07(myalgorithm, data = mydata, effects = myeff.4, prevAns = model.4.1, useCluster=TRUE, nbrNodes = 12)
model.4.3 <- siena07(myalgorithm, data = mydata, effects = myeff.4, prevAns = model.4.2, useCluster=TRUE, nbrNodes = 12)
model.4.3

wald.4 <- Multipar.RSiena(model.4.3, 3:11)
wald.4

# model 5: ego(unofficial), alter and interaction effects
myeff.5 <- getEffects(mydata)
myeff.5 <- includeEffects(myeff.5, density, cycle4, inPop, outAct)
myeff.5 <- includeEffects(myeff.5, egoX, name="discourse", interaction1="group")
myeff.5 <- includeEffects(myeff.5, egoX, name="discourse", interaction1="press")
myeff.5 <- includeEffects(myeff.5, egoX, name="discourse", interaction1="law")
myeff.5 <- includeEffects(myeff.5, egoX, name="discourse", interaction1="civic")


myeff.5 <- includeEffects(myeff.5, altX, name="discourse", interaction1="evaluation")

myeff.5 <- includeInteraction(myeff.5, egoX, altX, name="discourse", interaction1=c("press", "evaluation"))
myeff.5 <- includeInteraction(myeff.5, egoX, altX, name="discourse", interaction1=c("law", "evaluation"))
myeff.5 <- includeInteraction(myeff.5, egoX, altX, name="discourse", interaction1=c("civic", "evaluation"))

myeff.5

myalgorithm <- sienaAlgorithmCreate(projname = 'discourse', seed=1234, cond=FALSE )

model.5 <- siena07(myalgorithm, data = mydata, effects = myeff.5, useCluster=TRUE, nbrNodes = 12)
model.5.1 <- siena07(myalgorithm, data = mydata, effects = myeff.5, prevAns=model.5, useCluster=TRUE, nbrNodes = 12)
model.5.1

wald.5 <- Multipar.RSiena(model.5.1, 3:14)
wald.5

# print tables
siena.table(model.1.1, type="html", sig=TRUE)
siena.table(model.2.1, type="html", sig=TRUE)
siena.table(model.3.2, type="html", sig=TRUE)
siena.table(model.4.3, type="html", sig=TRUE)
siena.table(model.5.1, type="html", sig=TRUE)