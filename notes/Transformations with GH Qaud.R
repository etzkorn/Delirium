
f <- function(frail){
	theta =    34
	alpha1 =    .083799080974709519
	alpha2 =    .092823725716908986
	k =         1071
	cpt =          11
	res1 =    .001314528895273043
	res3 =    0.0000000000000000
	cdc =            0
	cdc2 =            1
	aux1 =   0.18901485432598022
	aux2 =    1.2383619289961900
	exp(
		frail*theta*cpt - exp(frail)*(res1-res3)
		+ alpha1 * frail * cdc - exp(frail*alpha1) * aux1
		+ alpha2 * frail * cdc2 - exp(frail*alpha2) * aux2
		- (frail^2)/(2*theta))
}

gh <- statmod::gauss.quad(32, kind="hermite")
frail = gh$nodes
weight = gh$weights * exp(gh$nodes^2)

sum(weight*f(gh$nodes))
2*sum(weight*f(gh$nodes*2))
34^2*sum(weight*f(gh$nodes*34^2))
34*2.2*sum(weight*f(gh$nodes*34*2.2))

# normal
sum(gh$weights * exp(gh$nodes^2)*dnorm(gh$nodes))
1/2*sum(gh$weights * exp((gh$nodes)^2)*dnorm(gh$nodes/2))
2*sum(gh$weights * exp((gh$nodes)^2)*dnorm(gh$nodes*2))
10*sum(gh$weights * exp((gh$nodes)^2)*dnorm(gh$nodes*10))

# exponential
sum(gh$weights[11:20] * exp((gh$nodes[11:20])^2)*dexp(gh$nodes[11:20]))
sum(gh$weights[11:20] * exp((gh$nodes[11:20])^2)*dexp(gh$nodes[11:20]))
