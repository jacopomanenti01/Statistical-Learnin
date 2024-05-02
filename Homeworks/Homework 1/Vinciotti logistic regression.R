set.seed(123)

# genera i valori di x
x<-rnorm(100,0,2) 
# Setta il valori dei parametri
beta0<-1
beta1<-2

# setta la formula di eta e mu
eta<-beta0+beta1*x
mu<-exp(eta)

# simula i dati della variabile dipendente
y<-rpois(100,lambda=mu) # y_i = poisson(lamda = mu_i)
y[1:10]

# [plot 1]: plottiamo 
plot(x,y) 
plot(x,log(y)) #sembrerebbe che ci sia una relazione lineare fra log(y) e x

# [1° modello]:  provo a stimare i coefficienti con la regressione lineare utilizzando x e log(y)
z<-log(y)
summary(z) #tuttavia perdo un po di osservazioni, non va bene
z<-log(y+1) # evita questo problem

modA<-lm(z~x)
summary(modA) #possiamo vedere come le stime dei parametri sembrino essere state invertite.
# comunque non corrispondono a quelle settate da no!
plot(modA)
# 1) dal grafico Residual vs fitted, sembrerebbe esserci una relazione quadratica
# 2) in una regressione lineare, la varianza dovrebbe essere costante, mentre vediamo un altro trend


#2° modello: proviamo con il metodo quadratico 

modB<-lm(z~x+I(x^2)) #I() isola l'effetto della variabile elevata al quadrato
summary(modB)
plot(modB)
plot(x,log(y))

# 3° modello: generalized linear model con poisson distrbution

modC<-glm(y~x,family=poisson)
summary(modC) #perfetti
plot(modC)

# facciamo un po di predizione
p1<-predict(modC) #in R la funzione predict() serve a fare predizioni
summary(p1)

eta.fitted<-coefficients(modC)[1]+coefficients(modC)[2]*x
eta.fitted[1:10]
p1[1:10]

# eta è utilizzata per fare le predizioni
# eta è ricavata tramite i coefficienti


p2<-predict(modC,type="response")
summary(p2)
p2[1:10]
exp(eta.fitted)[1:10]

## applichiamo la regressione logistica

mu<-exp(eta)/(1+exp(eta))
plot(eta,mu)
plot(x,mu)

y<-rbinom(100,size=1,prob=mu)
y
modD<-glm(y~x,family=binomial)
modD<-glm(y~x,family=binomial(link="logit"))

p3<-predict(modD)
p3[1:10]
summary(modD)
p3<-predict(modD,type="response")
p3[1:10]
cc <- log(mu)/(1-log(mu))
summary(p3)

table(1*(p3>0.5),y) # confusion matrix
#

prova <-  (c(1, NA),100)
