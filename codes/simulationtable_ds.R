#### This code creates Table 3 of the paper

## Required packages: xtable, yaml
## This code will the R file 'functions.R'
##  please set the wd of R correctly.

## These four sets calculates the design 
## 	sensitivities for the four cases in Table 3
## This code can take quite a while to run.

#####
#####	run the bottom code with the Functions first
#####

## Two strong instruments
sink('simulate_ds_ss.txt')
o <- createtable(N = 500000, k1=.2, k2=.25)
sink()
o1 <- o

## Z_1 weak, Z_2 strong
sink('simulate_ds_ws.txt')
o <- createtable(N = 500000, k1=.09, k2=.25)
sink()
o2 <- o

## Z_1 strong, Z_2 weak
sink('simulate_ds_sw.txt')
o <- createtable(N = 500000, k1=.2, k2=.09)
sink()
o3 <- o

## Two weak instruments
sink('simulate_ds_ww.txt')
o <- createtable(N = 500000, k1=.09, k2=.09)
sink()
o4 <- o

library(xtable)
res <- round( rbind( cbind(o1,o4[,-(1:4)]), cbind(o2,o3[,-(1:4)]) ), 2) 
res[is.na(res)] = 'B'
print(xtable(res), include.rownames=FALSE)



#################################################################################################
##################### ##################### Functions	##################### ##################### 

createtable <- function(N = 500000, k1=.2, k2=.25){
	o <- matrix(NA, 16, 7)
	colnames(o) <- c('l1', 'l2', 'rho', 'delta', 'Our (i)', 'Our (ii)', 'Our (iii)')

	## Lets create the table
	#1
	cat('#1\n')
	o[1,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), lambda1 = 0, lambda2=0, rho=0, skip.comp = c())
	gc()
	#2
	cat('#2\n')
	o[2,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), lambda2=0, rho=0, skip.comp = c(1))
	gc()
	#3
	cat('#3\n')
	o[3,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), lambda1=0, rho=0, skip.comp = c(2))
	gc()
	#4
	cat('#4\n')
	o[4,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), rho=0, skip.comp = c(1,2))
	gc()

	#5
	cat('#5\n')
	o[5,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), lambda1 = 0, lambda2=0, skip.comp = c(3))
	#6
	cat('#6\n')
	o[6,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), lambda2=0, skip.comp = c(1,3))
	#7
	cat('#7\n')
	o[7,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), lambda1 = 0, skip.comp = c(2,3))
	#8
	cat('#8\n')
	o[8,] <- runSimulation(N=N, k1=k1, k2=k2, pZ2_Z1= c('1'=0.40, '0'=0.40), skip.comp = c(1,2,3))

	#9
	cat('#9\n')
	o[9,] <- runSimulation(N=N, k1=k1, k2=k2, lambda1 = 0, lambda2=0, rho=0, skip.comp = c())
	gc()
	#10
	cat('#10\n')
	o[10,] <- runSimulation(N=N, k1=k1, k2=k2, lambda2=0, rho=0, skip.comp = c(1))
	gc()
	#11
	cat('#11\n')
	o[11,] <- runSimulation(N=N, k1=k1, k2=k2, lambda1=0, rho=0, skip.comp = c(1,2))
	gc()
	#12
	cat('#12\n')
	o[12,] <- runSimulation(N=N, k1=k1, k2=k2, rho=0, skip.comp = c(1,2))
	gc()

	#13
	cat('#13\n')
	o[13,] <- runSimulation(N=N, k1=k1, k2=k2, lambda1 = 0, lambda2=0, skip.comp = c(3))
	gc()
	#14
	cat('#14\n')
	o[14,] <- runSimulation(N=N, k1=k1, k2=k2, lambda2=0, skip.comp = c(1,3))
	gc()
	#15
	cat('#15\n')
	o[15,] <- runSimulation(N=N, k1=k1, k2=k2, lambda1 = 0, skip.comp = c(1,2,3))
	gc()
	#16
	cat('#16\n')
	o[16,] <- runSimulation(N=N, k1=k1, k2=k2, skip.comp = c(1,2,3))
	gc()

	return(o)
}

######################################################################




runSimulation <- function(N=25000, S = 25, inter=c(-2,2), interGamma=c(1, 25),
		beta = .5, 							## True effect

		pZ1 = 0.34, pZ2_Z1= c('1'=0.49, '0'=0.35),	## Distribution of the instruments
		lambda1 = .1, lambda2 = .1,				## Direct effect of the IVs
		rho = .2, sigma_eta = 0.06, 				## correlation of the errors and the variance of eta
		skip.comp = c(),					## Which components are not consistent
										##	Do not get 'desgn sensitiivty' for them;
										##	This is determined from Dylan's table.
		nu = 0, k1 = 0.20, k2 = 0.25,				## second equation
		methodList = c("wilcoxon")) {


	RZD = list()
	#power.tsls = c()
	#power.tsls.true = power.tsls.true1 = power.tsls.true2 = c()

	for(i in 1:N){
		## Step 1: Simulate the two instruments

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


		n.levels = 3
		node.names = c("Urban", "Catholic", "CathSchool")
		.Z = list(Z1, Z2, Z3)
		names(.Z) = node.names


		RZD[[i]] = list()
		RZD[[i]]$R = R
		RZD[[i]]$D = Z3
		RZD[[i]]$noComparisons = 3

		RZD[[i]]$comparison = list()

		for(comp in 1:n.levels) RZD[[i]]$comparison[[comp]] = list()
		names(RZD[[i]]$comparison) = node.names


		StrataStruc <- list()
		for(comp in 1:n.levels){
			if(comp %in% skip.comp){
				RZD[[i]]$comparison[[node.names[comp]]] = NA
				next;
			}

			Strata = comparisonStrata(tree.diag.temp, 1:S, list(), at=1, comp=comp, .Z)

			StrataStruc$noStrata = length(Strata)
			if( StrataStruc$noStrata > 0) StrataStruc$Strata = list()

			for(s in seq(1, length=length(Strata))){
				StrataStruc$Strata[[s]] = list()
				StrataStruc$Strata[[s]]$Units <- Strata[[s]]
				if(length(Strata[[s]])>0){
					StrataStruc$Strata[[s]]$treatment <- as.numeric(.Z[[comp]][Strata[[s]]])
				}	else StrataStruc$Strata[[s]]$treatment = integer(0)

				StrataStruc$Strata[[s]]$Size <- length(StrataStruc$Strata[[s]]$Units)
				StrataStruc$Strata[[s]]$noTreated <- sum(StrataStruc$Strata[[s]]$treatment )
				
				#StrataStruc$Strata[[s]]$varu <- muVar_gam(RZD[[i]]$R[Strata[[s]]], 
				#			nZt=StrataStruc$Strata[[s]]$noTreated, gam=log(1))$varu[1]
			}
			RZD[[i]]$comparison[[node.names[comp]]] = StrataStruc
		}


		if( ( (i/N)*100 )%%10 == 0)
			cat(' -',( (i/N)*100 ),'%-')
	
	}



	cat('\n')
	cat("True effect = ",beta,"\n\n")

	if(length(skip.comp)==3){
		ds = c(NA, NA, NA)
		names(ds) = node.names
		names(ds) = c('Our (i)', 'Our (ii)', 'Our (iii)')

		o <- c('l1'=lambda1, 'l2'=lambda2, 'rho'=rho/sqrt(sigma_eta), 'delta'=(pZ2_Z1['1']-pZ2_Z1['0']),  ds)
		print(o)
		cat('\n\n')

		return( o )	
	}


	no.comparisons = RZD[[1]]$noComparisons
	
	toCompare = setdiff(1:no.comparisons, skip.comp)

	## Want to find the muMaxGamma

	## Need to get the table of Size and the noTreated for this comparison.
	## We also need this for each strata
	freq.comp <- lapply(1:no.comparisons, function(comp){		
			if(!(comp %in% toCompare)) return(NA)
			noStrata.comp = RZD[[1]]$comparison[[comp]]$noStrata
			lapply(1:noStrata.comp, function(s)
				table(sapply(RZD, function(rzd) rzd$comparison[[comp]]$Strata[[s]]$Size), 
					sapply(RZD, function(rzd) rzd$comparison[[comp]]$Strata[[s]]$noTreated)))})

	names(freq.comp) = node.names	


	ds <- matrix(NA, no.comparisons, length(methodList))
	rownames(ds) = node.names
	colnames(ds) = methodList

	for(method in methodList){

		cat("Scoring Method: ", method,"\n")
		testStat <- rowMeans(sapply(RZD, computetestStat, method, skip.comp))

		cat("Test Statistic", testStat,"\n")

		for(comp in toCompare){
			cat(node.names[comp])
			foo <- function(Gamma, comp, RZD, freq.comp, method='wilcoxon')
				muMaxComparisons(Gamma, comp, RZD, freq.comp, method) - testStat[comp]

			ds[node.names[comp], method] = ( uniroot(foo, interval = interGamma, tol=10^(-5), comp=comp, RZD=RZD, freq.comp=freq.comp, method=method)$root )
			cat(": ds = ",ds[node.names[comp], method],".\t")
		}

		cat("\n")
		#muMaxComparisons(Gamma, comp, RZD, freq.comp, method)

		## Finally the calculation of beta.
		foo.beta <- function(beta, Gamma, comp, RZD, freq.comp, method='wilcoxon'){
			mean(sapply(RZD, computetestStat.beta, beta =beta, comp=comp,method=method)) - muMaxComparisons(Gamma, comp, RZD, freq.comp, method)
		}

		## Random check 
		cat("Random check: \n")
		if(1 %in% toCompare){
			comp = 1	#resample(toCompare,1)
			cat(node.names[comp],"; ")
			Gamma = 1	#sample(c(1,ds[node.names[comp],method]),1)
			cat("Gamma =",Gamma)
		} else { comp = resample(toCompare,1)
			cat(node.names[comp],"; ")
			Gamma = sample(c(1,ds[node.names[comp],method]),1)
			cat("Gamma =",Gamma)
		} 
		beta.check = uniroot(foo.beta, interval=inter, tol=10^(-5), comp=comp, RZD=RZD, freq.comp=freq.comp, method=method, 
					Gamma=Gamma )$root
		cat("; beta = ", beta.check,".")
		
		cat("\n\n")
	}

	names(ds) = c('Our (i)', 'Our (ii)', 'Our (iii)')
	
	o <- c('l1'=lambda1, 'l2'=lambda2, 'rho'=rho/sqrt(sigma_eta), 'delta'=(pZ2_Z1['1']-pZ2_Z1['0']),  ds)
	print(o)
	cat('\n\n')

	return( o )

}




#### Supporting code and functions

source('functions.R')


score <- function(j, Ns, method=c("W", "wilcoxon", "cs1", "cs2", "savage", "cs4", "sh", "Ufvfvfv", "tsls", "Utntntn"), a=5){
	
	method = match.arg(method)

	sj <- switch(method, W = j, wilcoxon = j/(Ns+1),
				cs1 = sapply(j, function(j) prod( seq(j/(Ns+1), (j+a-2)/(Ns+1), by=1/(Ns+1)))),
				cs2 = (j/(Ns+1))^(a-1),
				savage = sapply(j, function(j) sum(1/(1-1:j/(Ns+1)))),
				cs4 = -log(1-j/(Ns+1)), 
				sh = sapply(j, function(j) ifelse(j<a, 0, prod(seq((j-a+1)/(Ns+1),(j-1)/Ns+1), by=1/(Ns+1) )) ),
				Ufvfvfv = sapply(j, function(j) ifelse(j<5, 0, (Ns*choose(Ns, 5)^(-1))*sum( choose((j-1), (4-1):(5-1))*choose((Ns-j), (5-4):(5-5)) ))),
				Utntntn = sapply(j, function(j) ifelse(j<10, 0, (Ns*choose(Ns, 10)^(-1))*sum( choose((j-1), (10-1):(10-1))*choose((Ns-j), (10-10):(10-10)) ))),
				tsls = sapply(j, function(j) j/Ns))
	sj
}




genID <- function(u, p, forUnits){
	if(length(forUnits)<=0) return(integer(0))
	Gamma = 1
	u = u[forUnits]
	q = ifelse(p>1/2, 1-p, p)
	Z = 2*(q*Gamma*((1+Gamma)^(-1))*u) + 2*(q*1*((1+Gamma)^(-1))*(1-u))
	
	Ids = sapply(Z, function(z) as.numeric(runif(1)<z) )
	if(q < p){ return(1-Ids) 
	} else return(Ids)
}



sampleStrata <- function(tree.diag.temp, u, forUnits, at, .Z=list(), .n.levels=1, .node.names){
	if(missing(.node.names)) .node.names = 1:.n.levels
	if(at>.n.levels) return(.Z)

	tree.diag.temp <- tree.diag.temp[[names(tree.diag.temp)[1]]]
	p = tree.diag.temp[[2]]$Prob

	Ids <- genID(u, p, forUnits)
	.Z[[.node.names[at]]][forUnits] = Ids
		
	.Z = sampleStrata(tree.diag.temp[2], u, forUnits[Ids==1], at=at+1, .Z, .n.levels, .node.names)
	
	.Z = sampleStrata(tree.diag.temp[3], u, forUnits[Ids==0], at=at+1, .Z, .n.levels, .node.names)

	.Z				
}



comparisonStrata <- function(tree.diag.temp, Units, Strata = list(), at, comp, .Z){
	tree.diag.temp <- tree.diag.temp[[names(tree.diag.temp)[1]]]

	if('dontCompare' %in% names(tree.diag.temp))
		if(!is.null(tree.diag.temp[['dontCompare']])) return(Strata)

	if(at == comp){
		Strata = c(Strata, list(Units))
		return(Strata)
	} 

	Units1 = Units[.Z[[at]][Units]==1]
	Units2 = Units[.Z[[at]][Units]==0]

	Strata = comparisonStrata (tree.diag.temp[2], Units1, Strata, at=at+1, comp, .Z)
	
	Strata = comparisonStrata (tree.diag.temp[3], Units2, Strata, at=at+1, comp, .Z)

	Strata				
}


testStatinStrata <- function(StrataStruc, R, method = "wilcoxon"){
	Units = StrataStruc$Units
	Size =  StrataStruc$Size
	if(method=="tsls"){
		ranks = R[Units]
		treatment = StrataStruc$treatment 
		ntrt = sum(treatment)		
		varu <- muVar_gam(R, nZt=ntrt, gam=log(1))$varu[1]
		
		return(	ifelse(StrataStruc$noTreated==0, 0, (Size)*sum(ranks[treatment==1])/varu ) )
		
	} else {
		ranks = rank(R[Units])
		treatment = StrataStruc$treatment 
		
		return(	ifelse(StrataStruc$noTreated==0, 0, sum(score(ranks[treatment==1], Size, method))) )
	}
}

computetestStat <- function(rzd, method = 'wilcoxon', skip.comp){
		n.levels = length(rzd$comparison)
		sapply(1:n.levels, function(comp) {
			if(comp %in% skip.comp) return(NA)
			sum(sapply(rzd$comparison[[comp]]$Strata, testStatinStrata, R=rzd$R, method), na.rm=TRUE)
		})
}

computetestStat.beta <- function(rzd, beta, comp, method = 'wilcoxon'){
		#n.levels = length(rzd$comparison)
		#sapply(1:n.levels, function(comp) 
		sum(sapply(rzd$comparison[[comp]]$Strata, testStatinStrata, R=rzd$R-beta*rzd$D, method), na.rm=TRUE)#)
}




## Now find muMaxGamma for each of these structures
muMaxComparisons <- function(Gamma, comp, RZD, freq.comp, method = 'wilcoxon'){
	#no.comparisons = RZD[[1]]$noComparisons
	#temp = lapply(1:no.comparisons, function(comp){	
	noStrata.comp = RZD[[1]]$comparison[[comp]]$noStrata
	
	if(method=="tsls"){
		res = mean( sapply(RZD, function(rzd){
			ttestinstrata = c()
			for(s in 1:noStrata.comp){
				StrataStruc = rzd$comparison[[comp]]$Strata[[s]]
				
				Units = StrataStruc$Units
				Size =  StrataStruc$Size
				treatment = StrataStruc$treatment
				ntrt = sum(treatment)
				
				R = rzd$R[Units]
								
				if(length(Units)<=0){				
					ttestinstrata = c(ttestinstrata, 0)
				} else {
					muu = max(muVar_gam(R, nZt=ntrt, gam=log(Gamma))$muu)
					varu = muVar_gam(R, nZt=ntrt, gam=log(1))$varu[1]
				
					ttestinstrata = c(ttestinstrata, ifelse(StrataStruc$noTreated==0, 0, 
							(Size)*(muu/varu) ))
				}
			}
			sum(ttestinstrata)
			}), na.rm=TRUE )
			
		return(res)
		
	} else {		
		temp = sapply(1:noStrata.comp, 
			function(s)	{
				freqZ = freq.comp[[comp]][[s]]
				#freqZ = freqZ[rownames(freqZ)!='0',,drop=FALSE]
				#print(freqZ )
				temp = matrix(NA, dim(freqZ)[1], dim(freqZ)[2], dimnames=dimnames(freqZ))
				for(Z1 in as.numeric(rownames(freqZ)))
					for(Z2 in as.numeric(colnames(freqZ)))
						temp [as.character(Z1), as.character(Z2)] = ifelse(Z2>Z1,0,muMaxGamma(Gamma, q=score(1:Z1, Z1, method=method), n=Z2))
				#print(temp)
				(sum(temp*freqZ)/length(RZD))
			}
			)
		return( sum(temp) )
	}
}





muMaxGamma <- function(Gamma, q, n){
	if(n==0) return(0)
	senIntMu(q=q, nZt=n, gam=log(Gamma), type='max')
}

muMinGamma <- function(Gamma, q, n){
	if(n==0) return(0)
	senIntMu(q=q, nZt=n, gam=log(Gamma), type='min')
}

tree.temp <- "
 Urbanization:
  Prob: 0
  Urban:
   Prob: 0
  Catholic:
   Prob: 0
   CathSchool: 
     Prob: 0
   PubSchool:
     Prob: 0
  NonCatholic:
   Prob: 0
   CathSchool:
     Prob: 0
   PubSchool:
     Prob: 0
 Rural:
  Prob: 0
  Catholic: 
    Prob: 0
    CathSchool:
      Prob: 0
    PubSchool:
      Prob: 0
  NonCatholic:
    Prob: 0
    CathSchool:
      Prob: 0.0
    PubSchool:
      Prob: 0
"
library(yaml)
tree.diag.temp <- yaml.load(tree.temp)

resample <- function(x, ...) x[sample.int(length(x), ...)]

