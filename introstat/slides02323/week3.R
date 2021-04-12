
################################
## Spørgsmål: Uniform fordeling
## Sandsynlighed for medarbejder kommer mellem 8:20 og 8:30
punif(30,0,30) - punif(20,0,30)


################################
## Spørgsmål: Uniform fordeling
## Medarbejder kommer efter kl. 8:30
1 - punif(30,0,30)


################################
## Eksempel: Normalfordeling, spørgsmål 1
## Hvad er sandsynligheden for at brødet vejer under 490 g?
pnorm(490, mean=500, sd=10)


################################
## Eksempel: Normalfordeling, spørgsmål 2
## Hvad er sandsynligheden for at brødet ikke vejer mellem 480 og 520?
2 * pnorm(480, mean=500, sd=10)


################################
## Eksempel: Normalfordeling fraktiler
## "Omvendt spørgsmål": Hvilket interval, symmetrisk om midten, dækker 95\% af rugbrødene?
qnorm(c(0.025,0.975), mean=500, sd=10)


################################
## Eksempel: Standard Normalfordeling
## Hvad er sandsynligheden for at brødet vejer under 490 gram?
pnorm(-1)


################################
## Eksempel: Eksponentiel fordeling
## Sandsynlighed for ikke kommer flere kunder indefor en periode på 2 minutter
dpois(x=0, lambda=1)


################################
## Eksempel: Eksponentiel fordeling
## Sandsynlighed for ikke kommer flere kunder indefor en periode på 2 minutter
1-pexp(q=2, rate=1/2)
