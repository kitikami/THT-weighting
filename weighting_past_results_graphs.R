###########
# GRAPH 1 #
###########

n <- 1000000						#number of teams to simulate
size <- 162							#number of observations per year (or other unit of time)
u <- .5								#league average
sd <- .06							#standard deviation in talent levels
cor <- .9							#year-to-year correlation of talent
sd.rnd <- sqrt(sd^2/cor^2 - sd^2)	#standard deviation in random talent changes			
p1 <- rnorm(n,u,sd)					#initial talent levels
x1 <- rbinom(n,size,p1)				#results for first year

# generate new talent levels and results for second year
delta <- rnorm(n,0,sd.rnd)
	new <- p1 + delta
	factor <- sqrt(var(p1)/var(new))
	p2 <- factor*(new) + (1-factor)*mean(p1)
#	p2 <- new
	x2 <- rbinom(n,size,p2)

A <- var(x1/size)
B <- var(p1)
r <- cor

# calculate expected correlation of weighted results over two seasons to talent in year 2
cor.est <- function(A,B,r,w){
	sqrt(B*(r*w+1)^2/(A*w^2 + 2*B*r*w + A))
}
# put expected correlation into a function of x that can be easily graphed
cor.graph <- function(x){
	cor.est(A,B,r,x)
}

# calculate weight to give year one by maximizing expected correlation
w <- optimize(cor.est,interval=c(0,1),tol=.00001,maximum=T,A=A,B=B,r=r)$maximum


# draw graph of correlations for weights ranging from 0 to 1
curve(cor.graph,from=0,to=1
	,yaxt="n"
	,col="dark red"
	,lwd=2
	,xlab="weight",ylab="correlation with talent")
axis(side=2, at = pretty(range(par("usr")[3:4])),col.axis="dark red")
points(w,cor.est(A,B,r,w),col="dark red",pch=16)
abline(v=w,col="dark red")
#x <- w + .075
x <- w
y <- par("usr")[3]+.85*(par("usr")[4]-par("usr")[3])
text(x,y
	,paste("w =",round(w,3))
	,col="dark red"
	,pos=2)
title("Relationship between Simulated Results and Talent\nat Different Weights")

###############
# END GRAPH 1 #
###############


###########
# GRAPH 2 #
###########

# repeat above graph, but add second curve
# compares weighting of two-day sample (blue) to two-year sample (red)

n <- 1000000
size <- 162
u <- .5
sd <- .06
cor <- .9
sd.rnd <- sqrt(sd^2/cor^2 - sd^2)
p1 <- rnorm(n,u,sd)
x1 <- rbinom(n,size,p1)
delta <- rnorm(n,0,sd.rnd)
	new <- p1 + delta
	factor <- sqrt(var(p1)/var(new))
	p2 <- factor*(new) + (1-factor)*mean(p1)
#	p2 <- new
	x2 <- rbinom(n,size,p2)

A <- var(x1/size)
B <- var(p1)
#w <- .6234
r <- cor

cor.est <- function(A,B,r,w){
	sqrt(B*(r*w+1)^2/(A*w^2 + 2*B*r*w + A))
}
cor.graph <- function(x){
	cor.est(A,B,r,x)
}

w <- optimize(cor.est,interval=c(0,1),tol=.00001,maximum=T,A=A,B=B,r=r)$maximum


curve(cor.graph,from=0,to=1
	,yaxt="n"
	,col="dark red"
	,lwd=2
	,xlab="weight",ylab="correlation with talent")
axis(side=2, at = pretty(range(par("usr")[3:4])),col.axis="dark red")
points(w,cor.est(A,B,r,w),col="dark red",pch=16)
abline(v=w,col="dark red")
#x <- w + .075
x <- w
y <- par("usr")[3]+.65*(par("usr")[4]-par("usr")[3])
text(x,y
	,paste("w =",round(w,3))
	,col="dark red"
	,pos=2)


#regenerate data under new parameters

n <- 1000000
size <- 1
u <- .5
sd <- .06
cor <- .9
sd.rnd <- sqrt(sd^2/cor^2 - sd^2)
p1 <- rnorm(n,u,sd)
x1 <- rbinom(n,size,p1)

delta <- rnorm(n,0,sd.rnd)
	new <- p1 + delta
	factor <- sqrt(var(p1)/var(new))
	p2 <- factor*(new) + (1-factor)*mean(p1)
#	p2 <- new
	x2 <- rbinom(n,size,p2)

A <- var(x1/size)
B <- var(p1)
#w <- .6234
r <- cor

w <- optimize(cor.est,interval=c(0,1),tol=.00001,maximum=T,A=A,B=B,r=r)$maximum

# add to previous graph
par(new = TRUE)

curve(cor.graph,from=0,to=1
	,axes=FALSE
	,col='blue'
	,lwd=2
	,xlab='',ylab='')
axis(side=4, at = pretty(range(par("usr")[3:4])),col.axis="blue")
points(w,cor.est(A,B,r,w),col="blue",pch=16)
abline(v=w,col="blue")
x <- w
y <- par("usr")[3]+.85*(par("usr")[4]-par("usr")[3])
text(x,y
	,paste("w =",round(w,3))
	,col="blue"
	,pos=2)

x <- .2
y <- par("usr")[3]+.15*(par("usr")[4]-par("usr")[3])
text(x,y,"- two seasons",col="dark red")
y <- par("usr")[3]+.10*(par("usr")[4]-par("usr")[3])
text(x,y,"- two games",col="blue")

title("Relationship between Simulated Results and Talent\nat Different Weights")

###############
# END GRAPH 2 #
###############

###########
# GRAPH 3 #
###########
# graph linear regression coefficients

n <- 100000
size <- 4
t <- 180
u <- .33
sd <- .03
#sd.rnd <- .01
cor <- .998
sd.rnd <- sqrt(sd^2/cor^2 - sd^2)

variables <- c(n,size,t,u,sd,cor,sd.rnd)

simulate.data <- function(variables) {
	n <- variables[1]
	size <- variables[2]
	t <- variables[3]
	u <- variables[4]
	sd <- variables[5]
	cor <- variables[6]
	sd.rnd <- variables[7]
	
	p.full <- matrix(data=rep(0,n*t),nrow=n)
	x.full <- p.full

	p.full[,1] <- rnorm(n,u,sd)
	x.full[,1] <- rbinom(n,size,p.full[,1])

	for (i in 2:t) {
		delta <- rnorm(n,0,sd.rnd)
		new <- p.full[,(i-1)] + delta
		factor <- sqrt(var(p.full[,(i-1)])/var(new))
		p.full[,i] <- factor*(new) + (1-factor)*mean(p.full[,(i-1)])
		x.full[,i] <- rbinom(n,size,p.full[,i])
	}
	result <- list(p.full=p.full,x.full=x.full)
	return(result)
}

simulation <- simulate.data(variables)
p.full <- simulation$p.full
x.full <- simulation$x.full

calculate.sd.x <- function(v_t,v_x,r,w,d) {
	sqrt(
		v_t * 2 * 
			(  
				(r*w - (r*w)^d) / ( (1-r*w) * (1-w^2) ) +
				(w^(2*d)*((r/w)^d - r/w)) / ( (1-r/w) * (1-w^2) )
			) + 
		v_x * (1 - w^(2*d))/(1-w^2)
	)
}

reliability <- function(v_t,v_x,r,w,d) {
	sd.x <- calculate.sd.x(v_t,v_x,r,w,d)
	sd.y <- sqrt(v_t)
	cov <- v_t * (1-(r*w)^d)/(1-r*w)
	cov/(sd.x*sd.y)
}

v_t <- var(size*p.full[,1])
v_x <- var(x.full[,1])
r <- cor
# w <- ?
d <- t

w <- optimize(reliability,interval=c(0,1),tol=.000001,maximum=T,v_t=v_t,v_x=v_x,r=r,d=d)$maximum

#results as dependent variable
lm <- lm(x.full[,t]~x.full[,1:(t-1)])
coefficients <- rev(lm$coef[2:(t)])/lm$coef[t]

######
# graph linear regression coefficients

plot(coefficients
	,ylim=c(min(coefficients),max(coefficients))
	,ylab="weight"
	,xlab="days ago"
	,main="Weights Assigned to Past OBP Data by Linear Regression\nfor 10,000 simulated player seasons"
	)
	
lines(w^(0:(t-1)),col='dark red',lty=2,lwd=2)
y <- par("usr")[3] + .95*(par("usr")[4]-par("usr")[3])
text(.8*d,y,col="dark red",cex=.8
	,"\U00A6  weights predicted by model"
#	,paste("\U00A6  weights predicted by model\n(days^",round(w,3),")",sep=""))
	)

# compare to best-fit weight using mod on regression coefficients
coefficients[coefficients<=0] <- .00000000001
mod <- nls(y ~ exp(a * x), data=list(x=c(0:(t-2)),y=coefficients), start = list(a=0))
base <- as.double(exp(coef(mod)))
lines(base^(0:(t-1)),col='red',lwd=2)
CI.upper <- base + 1.96*summary(mod)$parameters[,2]
CI.lower <- base - 1.96*summary(mod)$parameters[,2]
polygon(c(0:(t-1),rev(0:(t-1))),c(CI.lower^(0:(t-1)),rev(CI.upper^(0:(t-1))))
	,col=rgb(150,100,100,alpha=40,maxColorValue=255)
	,border=NA
	)

y <- par("usr")[3] + .9*(par("usr")[4]-par("usr")[3])
text(.8*d,y,col="red",cex=.8
	,"|  best-fit to coefficients"
#	,paste("|  best-fit to coefficients\n(days^",round(base,3),")",sep=""))
	)
	
	
#add ghost lines for additional simulations to show variability

for(i in 1:5) {
	x.full.2 <- simulate.data(variables)$x.full
	lm.2 <- lm(x.full.2[,t]~x.full.2[,1:(t-1)])
	coefficients.2 <- rev(lm.2$coef[2:(t)])/lm.2$coef[t]	
#	lm.2 <- lm(apply(x.full.2[,(t-6):t],1,mean)~x.full.2[,1:(t-7)])
#	coefficients.2 <- rev(lm.2$coef[2:(t-6)])/lm.2$coef[t-6]

	coefficients.2[coefficients.2<=0] <- .00000000001
	mod <- nls(y ~ exp(a * x), data=list(x=c(0:(t-2)),y=coefficients.2), start = list(a=0))
	base <- as.double(exp(coef(mod)))
	lines(base^(0:(t-1)),col=rgb(150,100,100,alpha=80,maxColorValue=255),lwd=1.5)
	CI.upper <- base + 1.96*summary(mod)$parameters[,2]
CI.lower <- base - 1.96*summary(mod)$parameters[,2]
polygon(c(0:(t-1),rev(0:(t-1))),c(CI.lower^(0:(t-1)),rev(CI.upper^(0:(t-1))))
	,col=rgb(150,100,100,alpha=10,maxColorValue=255)
	,border=NA
	)
}

y <- par("usr")[3] + .85*(par("usr")[4]-par("usr")[3])
text(.8*d,y
	,col=rgb(150,100,100,alpha=200,maxColorValue=255)
	,cex=.8
	,"|  best-fit for repeat simulations"
	)

###############
# END GRAPH 3 #
###############