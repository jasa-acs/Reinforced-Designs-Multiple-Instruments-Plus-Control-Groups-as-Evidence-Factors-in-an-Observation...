## This code is sourced by the 
##  R code simulationtable_ds.R to create Table 3

xi <- function(N, n, m, gam){
	if(m<0 | n<0) return(0)

	sum( sapply(max(0, (m+n-N)):min(n,m),
			FUN = function(a) choose(m, a)*choose((N-m),(n-a))*exp(gam*a))  )
}

pigamma <- function(N, n, ui, m, gam){
	exp(gam*(ui))*xi( (N-1), (n-1), (m-ui), gam )/xi(N, n, m, gam)
}

pijgamma <- function(N, n, ui, uj, m, gam){
	exp(gam*(ui+uj))*xi( (N-2), (n-2), m-ui-uj, gam)/xi(N, n, m, gam)
}


muVar_gam <- function(q, Zt, gam, nZt){
	q = q[order(q)]
	if(!missing(Zt))
		n = sum(Zt)
	if(missing(Zt) & !missing(nZt)) n = nZt
	
	N = length(q)
	
	varu <- c()
	muu <- c()
	
	for(i in seq(from=1,length=max(0,N-1))){
		u = c(rep(0, i), rep(1, (N-i)))
		pij00 <- pijgamma(N, n, 0, 0, sum(u), gam)
		pij01 <- pijgamma(N, n, 0, 1, sum(u), gam)
		pij10 <- pijgamma(N, n, 1, 0, sum(u), gam)
		pij11 <- pijgamma(N, n, 1, 1, sum(u), gam)
		
		pij = outer(u, u)*pij11 + outer(u, 1-u)*pij10 + outer(1-u, u)*pij01 + outer(1-u, 1-u)*pij00		
		pi = u*pigamma(N, n, 1, sum(u), gam) + (1-u)*pigamma(N, n, 0, sum(u), gam)		
		diag(pij) <- pi
		
		muu <- c(muu, sum(pi*q))
		varu <- c(varu, sum( (pij-outer(pi, pi))* outer(q, q) ))
	}
	list(muu=muu, varu=varu)
}

## We want a function that takes the qs, a gamma and 
##  the treatment to find the max and the min mu.

senIntMu <- function(q, Zt, gam, type='min', nZt){
	q = q[order(q)]
	if(!missing(Zt))
		n = sum(Zt)
	if(missing(Zt) & !missing(nZt)) n = nZt
	
	N = length(q)
	
	
	#there are N-1 choices for maximum
	## get mu for each of them
	## Similarly N-1 choices for min

	Mu <- c()

	for(i in 1:(N-1)){
		# umax all 1 after ith total (N-i) 1s
		# two possible pis
		if(type=='max'){
			pi1max = pigamma(N, n, 1, (N-i), gam)
			pi0max = pigamma(N, n, 0, (N-i), gam)
		
			maxMuThis = sum(c(rep(pi0max, i), rep(pi1max, (N-i)))*q)

			Mu <- c(Mu, maxMuThis)  
		}
		
		# umin all 1 till ith total i ones
		if(type=='min'){
			pi1min = pigamma(N, n, 1, i, gam)
			pi0min = pigamma(N, n, 0, i, gam)

			minMuThis = sum(c(rep(pi1min, i), rep(pi0min, (N-i)))*q)

			Mu <- c(Mu, minMuThis)
		}
	}
	
	if(type=='min')
			return(min(Mu))
	if(type=='max')
			return(max(Mu))
}



## Finally solve for the lower and upper limit of
## the sensitivity interval

TBeta <- function(beta, R, Z, D){
	Rbeta = R-beta*D
	qbeta = rank(Rbeta)

	sum(qbeta*Z)
	
}
# for all beta you need to adjust the 
#	response


solveBeta <- function(beta, R, Z, D, Gamma, type='min'){
	Rbeta = R-beta*D
	qbeta = rank(Rbeta)

	Tbeta = sum(qbeta*Z)
	Mu = senIntMu(qbeta, Z, gam=log(Gamma), type=type)

	Tbeta - Mu
}


