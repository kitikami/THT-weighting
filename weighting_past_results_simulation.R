# simulate day-to-day performance with random talent changes between days

n <- 100000							#number of players to simulate
size <- 4							#number of observations per day (or other unit of time)
t <- 162								#number of days
u <- .33								#league average
sd <- .03							#standard deviation of current talent levels
cor <- .999							#day-to-day correlation of talent
sd.rnd <- sqrt(sd^2/cor^2 - sd^2)	#sd of day-to-day talent changes

# create blank tables to store talent and results for each day in sim
p.full <- matrix(data=rep(0,n*t),nrow=n)		#table of true talent levels
x.full <- p.full								#table of results

# generate the talent levels and results for day 1
p.full[,1] <- rnorm(n,u,sd)
x.full[,1] <- rbinom(n,size,p.full[,1])

# simulate new talent levels and results for each successive day
for (i in 2:t) {
	delta <- rnorm(n,0,sd.rnd)										#amount each player's talent level changes
	new <- p.full[,(i-1)] + delta									#new talent levels
	factor <- sqrt(var(p.full[,(i-1)])/var(new))						#rescale new talent levels to maintain same spread as previous talent levels
																	#otherwise variance in talent will steadily grow over time
	p.full[,i] <- factor*(new) + (1-factor)*mean(p.full[,(i-1)])		#add new talent levels to table
	x.full[,i] <- rbinom(n,size,p.full[,i])							#add new results to table
}

# function to calculate the standard deviation of weighted results 
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

# function to calculate expected correlation between weighted results and current talent levels
reliability <- function(v_t,v_x,r,w,d) {
	sd.x <- calculate.sd.x(v_t,v_x,r,w,d)
	sd.y <- sqrt(v_t)
	cov <- v_t * (1-(r*w)^d)/(1-r*w)
	cov/(sd.x*sd.y)
}

v_t <- var(size*p.full[,1])		#variance in current talent levels
v_x <- var(x.full[,1])			#variance in on day of results
r <- cor							#day-to-day correlation of talent	
d <- t							#number of days (or other units of time) in sample
# note: v_t and v_x

################
# DECAY FACTOR #
################
# calculate decay factor by finding weight that maximized reliability function
w <- optimize(reliability,interval=c(0,1),tol=.000001,maximum=T,v_t=v_t,v_x=v_x,r=r,d=d)$maximum


# test above formulas against simulated data
w.vector <- w^(0:(d-1))								#vector of weights to apply to each day
weights <- matrix(data=w.vector,nrow=nrow(x.full[,1:d]),ncol=ncol(x.full[,1:d]),byrow=T)		#table of weights to apply to table of results
successes <- apply(x.full[,1:d]*weights,1,sum)		#weighted sum of results
#trials <- sum(w.vector*size)						#weighted number of opportunities
trials <- size * (1-w^d)/(1-w)						#weighted number of opportunities, same as above line but computationally simpler

# compare sd of weighted results from sim to prediction by mathematical model
sd(successes)
calculate.sd.x(v_t,v_x,r,w,d)

# compare correlation between weighted results and current talent from sim to prediction by mathematical model
cor(successes,p.full[,1])
reliability(v_t,v_x,r,w,d) 

#################################
# calculate regression constant #
#################################

# estimate regression constant 
# relies on binomial random variance, so stat must be binomial for this to work
sd.x <- calculate.sd.x(v_t,v_x,r,w,d)						#total observed sd of weighted results
bias <- size * (1-w^d)/(1-w) / (size * (1-w^d)/(1-w) - 1)	#mathematical correction for calculating true variance from observed and random variance
var.rnd <- size * u*(1-u)*(1-w^(2*d))/(1-w^2)				#random binomial variance
var.true <- bias * (sd.x^2 - var.rnd)						#total variance in weighted results that can be attributed to talent
var.explained <- bias * (1 - var.rnd/sd.x^2)					#proportion of variance in weighted results that can be attributed to talent
regr <- trials * (1-var.explained) / var.explained			#preliminary regression constant (applies to weighted average of talent over weighted results, not to current talent levels)
regr.i <- u*(1-u)/sd^2 #original regression constant		#regression constant ignoring changes in talent

# adjust regression constant to apply to current talent levels rather than weighted average over sample
A <- (1 - w)*(1 - (r*w)^d)/((1 - r*w)* (1 - (w)^d)) 
B <- 2 * 
			(  
				(r*w - (r*w)^d) / ( (1-r*w) * (1-w^2) ) +
				(w^(2*d)*((r/w)^d - r/w)) / ( (1-r/w) * (1-w^2) )
			) + 
		(1 - w^(2*d))/(1-w^2)	
C <- 2 * 
			(  
				(w - (w)^d) / ( (1-w) * (1-w^2) ) +
				(w^(2*d)*((1/w)^d - 1/w)) / ( (1-1/w) * (1-w^2) )
			) + 
		(1 - w^(2*d))/(1-w^2)		

sample.size.correction <- A/(B/C)
var.explained <- var.true*sample.size.correction^4/(var.true*sample.size.correction^4 + var.rnd)		#proportion of variance in weighted results attributable to current talent levels

#######################
# REGRESSION CONSTANT #
#######################
regr <- trials * (1-var.explained) / var.explained		#revised regression constant to account for weighting/talent changes

#test regression constant
#compare projections to actual talent
#check average error for above and below average players to test for over- or under-regression
#average error should be close to zero
mean ( (successes[successes>(trials*u)] + u*regr)/(trials+regr) - p.full[successes>(trials*u),1] )	#average error for above average players in sim
mean ( (successes[successes<(trials*u)] + u*regr)/(trials+regr) - p.full[successes<(trials*u),1] )	#average error for below average players in sim

#compare to original regression constant to verify that revising to account for weighting/talent changes works better
mean ( (successes[successes>(trials*u)] + u*regr)/(trials+regr.i) - p.full[successes>(trials*u),1] )
mean ( (successes[successes<(trials*u)] + u*regr)/(trials+regr.i) - p.full[successes<(trials*u),1] )


# test regression constant for more groupings of players
projected <- (successes + u*regr)/(trials+regr)			#projected talent levels using revised regression constant
projected.i <- (successes + u*regr.i)/(trials+regr.i)	#projected talent levels using original regression constant

# define limits for groupings of players
lower.list <- c(u+.022,u+.01,u,u-.01,u-.022,0)	#lower threshold of each grouping
upper.list <- c(1,u+.022,u+.01,u,u-.01,u-.022)	#upper threshhold of each grouping

table <- c()		#blank table to store average errors of projections for each player grouping
for(i in 1:length(lower.list)) {	
	lower<-lower.list[i]
	upper<-upper.list[i]
	
	range <- paste(lower,'-',upper,sep='')
	
	n.players <- length(projected[projected>lower & projected < upper])
	proj.u <- mean(projected[projected>lower & projected < upper])
	true.u <- mean(p.full[projected>lower & projected < upper])
	proj.e <- mean(p.full[projected>lower & projected < upper] - projected[projected>lower & projected < upper] )
	
	proj.i.u <- mean(projected.i[projected.i>lower & projected.i < upper])
	true.i.u <- mean(p.full[projected.i>lower & projected.i < upper])
	proj.i.e <- mean(p.full[projected.i>lower & projected.i < upper] - projected.i[projected.i>lower & projected.i < upper])
	
	table <- rbind(table,c(range,n.players,proj.e,proj.i.e))
}

table	#view table showing errors for each grouping of players

