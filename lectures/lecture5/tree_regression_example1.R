

#install.packages("tree")
library(tree)

data("trees")
head(trees)
A<-tree(formula = Volume~.,data = trees)

plot(trees[,c(3,1,2)])

plot(A)
text(A, pretty = 0)

A_pred<-predict(A)
plot(trees$Volume,A_pred)
mean((A_pred-trees$Volume)^2)


B<-tree(formula = Volume~.,data = trees,control = tree.control(nobs = 31,minsize = 3))
plot(B)
text(B, pretty = 0)
B_pred<-predict(B)
plot(trees$Volume,B_pred)
mean((B_pred-trees$Volume)^2)

lm_model<-lm(formula = Volume~.,data = trees)

lm_pred<-predict(lm_model)
plot(trees$Volume,lm_pred)
mean((lm_pred-trees$Volume)^2)

