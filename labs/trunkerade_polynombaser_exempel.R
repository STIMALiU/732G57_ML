source("https://raw.githubusercontent.com/STIMALiU/732G57_ML/refs/heads/main/labs/trunc_power_basis.R")






str(basis_list1)

n <- 500
set.seed(424)
x <- sort(runif(n , min = 0, max = 5  ))
y <- 0.5 + 3*x - 2*x^2 + .2*x^3 + log(2*x+1) + 3*sin(pi * x) + rnorm(n = n, sd = 1)
df <- data.frame(x = x, y = y)
plot(df)
library(ggplot2)

no_basis<-10 # antal basfunktioner, testa att ändra till 50

# linjära baser:
basis_list1<-trunc_power_basis(x = x,no_basis = no_basis,type = "linear",show_plot = TRUE)
# kvadratiska baser:
basis_list2<-trunc_power_basis(x = x,no_basis = no_basis,type = "quadratic",show_plot = TRUE)
# kubiska baser
basis_list3<-trunc_power_basis(x = x,no_basis = no_basis,type = "cubic",show_plot = TRUE)

str(basis_list3)

# modell 1: linjära baser
df_temp1<-data.frame(y=y,basis_list1$basis_mat)
head(df_temp1)
lm_cos1<-lm(y~.,data=df_temp1)


# modell 2: kvadratiska baser
df_temp2<-data.frame(y=y,basis_list2$basis_mat)
head(df_temp2)
lm_cos2<-lm(y~.,data=df_temp2)



# modell 3: kubiska baser
df_temp3<-data.frame(y=y,basis_list3$basis_mat)
head(df_temp3)

# plottar anpassningen
plot(df,main="linjära baser")
lines(x,fitted(lm_cos1),col="blue",lwd=3)
plot(df,main="kvadratiska baser")
lines(x,fitted(lm_cos2),col="blue",lwd=3)
plot(df,main="kubiska baser")
lines(x,fitted(lm_cos3),col="blue",lwd=3)

