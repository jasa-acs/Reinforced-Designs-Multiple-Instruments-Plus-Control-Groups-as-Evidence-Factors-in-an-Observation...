## This code creates Table 2 of the paper.

##  Required packages
library(AER)
library(senstrat)

### Please run the bottom part of 
### the code with the Functions first
###  

## In these for sets, each set creates 
##  4 rows of the table at a time.
## Each set can take upto a cople of days to run.

sink('simulate_power_dump_set1.txt')

o <- createtable_set1()

sink()

round(o, 2)


sink('simulate_power_dump_set2.txt')

o <- createtable_set2()

sink()

round(o, 2)



sink('simulate_power_dump_set3.txt')

o <- createtable_set3()

sink()

round(o, 2)



sink('simulate_power_dump_set4.txt')

o <- createtable_set4()

sink()

round(o, 2)


# test
# 
#runSimulation(N=100, pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, lambda1 = 0, lambda2=0, rho=0, skip.comp = c(1:3))
#
#runSimulation(N=100,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, lambda1=0, rho=0, skip.comp = c(1:3))


######################################################################
##  Functions	##

createtable_set1 <- function(){

	o <- matrix(NA, 16, 8)
	colnames(o) <- c('l1', 'l2', 'rho', 'delta', 'TSLS', 'Our (i)', 'Our (ii)', 'Our (iii)')

	## Lets create the table
	#1
	cat('#1\n')
	o[1,] <- runSimulation(N=10000, pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, lambda1 = 0, lambda2=0, rho=0, skip.comp = c(1:3))
	gc()
	#2
	cat('#2\n')
	o[2,] <-runSimulation(N=10000,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, lambda2=0, rho=0, skip.comp = c(1:3))
	gc()
	#3
	cat('#3\n')
	o[3,] <- runSimulation(N=10000,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, lambda1=0, rho=0, skip.comp = c(1:3))
	gc()
	#4
	cat('#4\n')
	o[4,] <- runSimulation(N=10000,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, rho=0, skip.comp = c(1:3))
	gc()

	return(o)
}


createtable_set2 <- function(){

	o <- matrix(NA, 16, 8)
	colnames(o) <- c('l1', 'l2', 'rho', 'delta', 'TSLS', 'Our (i)', 'Our (ii)', 'Our (iii)')

	## Lets create the table

	#5
	cat('#5\n')
	o[5,] <- runSimulation(N=10000,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, lambda1 = 0, lambda2=0, skip.comp = c(1:3))
	gc()
	#6
	cat('#6\n')
	o[6,] <- runSimulation(N=10000,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, lambda2=0, skip.comp = c(1:3))
	gc()
	#7
	cat('#7\n')
	o[7,] <- runSimulation(N=10000,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, skip.comp = c(1:3))
	gc()
	#8
	cat('#8\n')
	o[8,] <- runSimulation(N=10000,pZ2_Z1= c('1'=0.40, '0'=0.40), S=4450, skip.comp = c(1:3))
	gc()

	return(o)
}


createtable_set3 <- function(){

	o <- matrix(NA, 16, 8)
	colnames(o) <- c('l1', 'l2', 'rho', 'delta', 'TSLS', 'Our (i)', 'Our (ii)', 'Our (iii)')

	## Lets create the table
	#9
	cat('#9\n')
	o[9,] <- runSimulation(N=10000,lambda1 = 0, lambda2=0, rho=0, S=4450, skip.comp = c(1:3))
	gc()
	#10
	cat('#10\n')
	o[10,] <- runSimulation(N=10000,lambda2=0, rho=0, S=4450, skip.comp = c(1:3))
	gc()
	#11
	cat('#11\n')
	o[11,] <- runSimulation(N=10000,lambda1=0, rho=0, S=4450, skip.comp = c(1:3))
	gc()
	#12
	cat('#12\n')
	o[12,] <- runSimulation(N=10000,rho=0, S=4450, skip.comp = c(1:3))
	gc()

	return(o)
}

createtable_set4 <- function(){

	o <- matrix(NA, 16, 8)
	colnames(o) <- c('l1', 'l2', 'rho', 'delta', 'TSLS', 'Our (i)', 'Our (ii)', 'Our (iii)')

	## Lets create the table
	#13
	cat('#13\n')
	o[13,] <- runSimulation(N=10000,lambda1 = 0, lambda2=0, S=4450, skip.comp = c(1:3))
	gc()
	#14
	cat('#14\n')
	o[14,] <- runSimulation(N=10000,lambda2=0, S=4450, skip.comp = c(1:3))
	gc()
	#15
	cat('#15\n')
	o[15,] <- runSimulation(N=10000,S=4450, skip.comp = c(1:3))
	gc()
	#16
	cat('#16\n')
	o[16,] <- runSimulation(N=10000,S=4450, skip.comp = c(1:3))
	gc()	

	return(o)

}




######################################################################


runSimulation <- function(N=25000, S = 4450, nS = 25, 	# inter=c(-2,2), interGamma=c(1, 25),
		beta = .5, 							## True effect

		pZ1 = 0.34, pZ2_Z1= c('1'=0.49, '0'=0.35),	## Distribution of the instruments
		lambda1 = .1, lambda2 = .1,				## Direct effect of the IVs
		rho = .2, sigma_eta = 0.06, 				## correlation of the errors and the variance of eta
		skip.comp = c(),					## Which components are not consistent
										##	Do not get 'desgn sensitiivty' for them;
										##	This is determined from Dylan's table.
		nu = 0, k1 = 0.20, k2 = 0.25,				## second equation
		method = c("wilcoxon")) {


	# RZD = list()
	power.tsls = c()
	power.tsls.true = power.tsls.true1 = power.tsls.true2 = c()

	power.res.1	= power.res.2 = power.res.3 = c()

	for(i in 1:N){
		## Step 1: Simulate the two instruments
		#S = 25

		Z1 = 1*(runif(S)<pZ1)
		Z2 = 1*sapply(Z1, function(z1) runif(1)<pZ2_Z1[as.character(z1)])

		## Step 2: Simulate the treatment
		##	Use the second equation
		Z3.temp = nu + k1*Z1+k2*Z2
		## Step 2 is is incomplete.  We need to 
		##	as a noise to it to introduce selection bias
		# Noise in the first equation
		epsilon = rnorm(S)
		eta = sqrt(sigma_eta-rho^2)*rnorm(S) + rho*epsilon

		## go back to Z3
		Z3.temp = Z3.temp+eta
		Z3 = 1*sapply(Z3.temp, function(z3){ 
				z3 = min(max(z3, 0), 1)
				(runif(1)<z3)})
		## Ends Step 2
		## Now simulate the response

		R.0 = 0*Z3+lambda1*Z1 + lambda2*Z2 + epsilon
		R = R.0 + beta*Z3  #beta*Z3+lambda1*Z1 + lambda2*Z2 + epsilon

		## 2stage least squares+ (depreciated)
		#temp =  ivreg(R.0~Z3|Z1+Z2, data=data.frame(R.0,Z1,Z2,Z3))
		#if(!is.na(coef(temp)[2])) 
		#	power.tsls = c(power.tsls, summary(temp)$coefficients[2,4])

		temp =  ivreg(R~Z3|Z1+Z2, data=data.frame(R,Z1,Z2,Z3))
		if(!is.na(coef(temp)[2])) {
			#power.tsls.true = c(power.tsls.true, summary(temp)$coefficients[2,4])
			#power.tsls.true1 = c(power.tsls.true1, 
			#		pnorm( (summary(temp)$coefficients[2,1]-beta)/summary(temp)$coefficients[2,2], lower.tail=FALSE ) )

			power.tsls.true2 = c(power.tsls.true2, 
					2*pnorm( abs(summary(temp)$coefficients[2,1]-beta)/summary(temp)$coefficients[2,2], lower.tail=FALSE ) )
		}

		node.names = c("Urban", "Catholic", "CathSchool")

		## Our analysis
		## Stratification
		strata = as.factor(sample( rep(1:(S/nS), nS) ))
		
		#table(strata)

		## Urban
		st.1 = strata
		z.1 = Z1
		sc.1 = rscores(R, z.1, st.1, trt=Z3, tau = beta, method=method)
		res.1.g = senstrat(sc.1, z.1, st.1, detail=TRUE)$LinearBoundResult['P-value']
		#res.1.l = senstrat(sc.1, z.1, st.1, alternative='less',detail=TRUE,method="LS")$LinearBoundResult['P-value']
		#p.1 = 2*min(res.1.g$LinearBoundResult['P-value'], res.1.l$LinearBoundResult['P-value'])
		#power.res.1 = c(power.res.1, p.1)
		power.res.1 = c(power.res.1, res.1.g)
		
		
		## Religion
		st.2 = strata:as.factor(Z1)
		z.2 = Z2
		sc.2 = rscores(R, z.2, st.2, trt=Z3, tau = beta, method=method)
		res.2.g = senstrat(sc.2, z.2, st.2,  detail=TRUE)$LinearBoundResult['P-value']
		#res.2.l = senstrat(sc.2, z.2, st.2, trt=Z3, alternative='less',detail=TRUE,method="LS")
		#p.2 = 2*min(res.2.g$LinearBoundResult['P-value'], res.2.l$LinearBoundResult['P-value'])
		#power.res.2 = c(power.res.2, p.2)
		power.res.2 = c(power.res.2, res.2.g)

		## School
		st.3 = strata:as.factor(Z1):as.factor(Z2)
		z.3 = Z3
		sc.3 = rscores(R, z.3, st.3, tau = beta, method=method)
		res.3.g = senstrat(sc.3, z.3, st.3, detail=TRUE)$LinearBoundResult['P-value']
		#res.3.l = senstrat(sc.3, z.3, st.3, alternative='less',detail=TRUE,method="LS")$LinearBoundResult['P-value']
		#p.3 = 2*min(res.3.g$LinearBoundResult['P-value'], res.3.l$LinearBoundResult['P-value'])
		#power.res.3 = c(power.res.3, p.3)
		power.res.3 = c(power.res.3, res.3.g)

		if( ( (i/N)*100 )%%5 == 0 )
			cat(' -',( (i/N)*100 ),'%-')
		
	
	}

	cat('\n\n')
	o <- c( 	'TSLS'=mean(power.tsls.true2<0.05, na.rm=TRUE),
			'Our (i)'=mean(power.res.1<0.05, na.rm=TRUE),
			'Our (ii)'=mean(power.res.2<0.05, na.rm=TRUE),
			'Our (iii)'=mean(power.res.3<0.05, na.rm=TRUE) )

	o <- c('l1'=lambda1, 'l2'=lambda2, 'rho'=rho/sqrt(sigma_eta), 'delta'=(pZ2_Z1['1']-pZ2_Z1['0']),  o)
	print(o)
	cat('\n\n')

	return( o )


}



##########################################################################
### Supplementary functions
rscores <- function (y, z, st = NULL, trt=NULL, tau = 0, method=c("wilcoxon", "cs1", "cs2", "savage", "cs4", "sh", "U545")) {
    method = match.arg(method)
    stopifnot(length(tau) == 1)
    stopifnot(length(y) == length(z))
    if (is.null(st)) 
        st <- rep(1, length(y))
    stopifnot(length(st) == length(y))
    ust <- unique(st)
    nst <- length(ust)
    if (tau != 0){ 
	if(is.null(trt))
        y <- y - z * tau
	if(!is.null(trt))
   	  y <- y - trt * tau
	}
   sc <- rep(NA, length(y))

    for (i in 1:nst) {
        who <- st == ust[i]
        yi <- y[who]
        ni <- length(yi)
	  if (ni == 1) {
            sc[who] <- 0
        } else if (ni >= 2) {
		sc[who] <- score(rank(yi), ni, method=method)
		
        }
    }

   sc
}

score <- function(j, Ns, method=c("W", "wilcoxon", "cs1", "cs2", "savage", "cs4", "sh", "U545"), a=5){
	
	method = match.arg(method)

	sj <- switch(method, W = j, wilcoxon = j/(Ns+1),
				cs1 = sapply(j, function(j) prod( seq(j/(Ns+1), (j+a-2)/(Ns+1), by=1/(Ns+1)))),
				cs2 = (j/(Ns+1))^(a-1),
				savage = sapply(j, function(j) sum(1/(1-1:j/(Ns+1)))),
				cs4 = -log(1-j/(Ns+1)), 
				sh = sapply(j, function(j) ifelse(j<a, 0, prod(seq((j-a+1)/(Ns+1),(j-1)/Ns+1), by=1/(Ns+1) )) ),
				U545 = sapply(j, function(j) ifelse(j<5, 0, (Ns*choose(Ns, 5)^(-1))*sum( choose((j-1), (4-1):(5-1))*choose((Ns-j), (5-4):(5-5)) ))) )
	sj
}

