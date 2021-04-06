
###########################################################################
## Sæt working directory

## I RStudio kan man nemt sætte working directory med menuen 
## "Session -> Set Working Directory -> To Source File Location" 
## Bemærk: i R bruges kun "/" til separering i stier 
## (altså ingen backslash).
setwd("Erstat her med stien til den mappe, hvor projektfilerne er gemt.")


###########################################################################
## Indlæs data

## Indlæs data fra soenderborg1_data.csv
D <- read.table("soenderborg1_data.csv", header=TRUE, sep=";", 
                as.is=TRUE)


###########################################################################
## Simpel opsummering af data

## Dimensionen af D (antallet af rækker og søjler)
dim(D)
## Søjle-/variabelnavne
names(D)
## De første rækker/observationer
head(D)
## De sidste rækker/observationer
tail(D)
## Udvalgte opsummerende størrelser
summary(D)
## En anden type opsummering af datasættet
str(D)


###########################################################################
## Histogram (empirisk tæthed)

## Histogram der beskriver den empiriske tæthed for obs.
## af varmeforbruget for hus 1 (histogram for de daglige 
## målinger normaliseret så arealet er lig 1)
hist(D$Q1, xlab="Varmeforbrug (hus 1)", prob=TRUE)


###########################################################################
## Konverter variabel til dato

## Konverterer variablen 't' til en dato-variabel i R
D$t <- as.Date(x=D$t, format="%Y-%m-%d")
## Tjekker resultatet
summary(D$t)


###########################################################################
## Plot af data over tid

## Plot varmeforbruget over tid
plot(D$t, D$Q1, type="l", xlim=as.Date(c("2008-10-02","2010-10-01")), 
     ylim=c(0,9), xlab="Dato", ylab="Varmeforbrug", col=2)
lines(D$t, D$Q2, col=3)
lines(D$t, D$Q3, col=4)
lines(D$t, D$Q4, col=5)
## Tilføj legend
legend("topright", legend=paste0("Q",c(1,2,3,4)), lty=1, col=2:5)


###########################################################################
## Udvælg deldatasæt

## Udvælg data fra januar og februar 2010
Dsel <- subset(D, "2010-01-01" <= t & t < "2010-3-01")


###########################################################################
## Boxplot opdelt efter hus

## Boxplot for varmeforbrug opdelt efter hus
boxplot(Dsel[ ,c("Q1","Q2","Q3","Q4")],
        xlab="Hus", ylab="Varmeforbrug")


###########################################################################
## Opsummerende størrelser for varmeforbruget

## Antal observationer af dagligt varmeforbrug for hus 1 i jan-feb 2010
## (medregner ej eventuelle manglende værdier)
sum(!is.na(Dsel$Q1))
## Stikprøvegennemsnit af dagligt varmeforbrug for hus 1 i jan-feb 2010
mean(Dsel$Q1, na.rm=TRUE)
## Stikprøvevarians af dagligt varmeforbrug for hus 1 i jan-feb 2010
var(Dsel$Q1, na.rm=TRUE)
## osv. 
## Argumentet 'na.rm=TRUE' sørger for at størrelsen
## udregnes selvom der eventuelt er manglende værdier


###########################################################################
## qq-plot til modelkontrol

## qq-plot for varmeforbruget i hus 1 
qqnorm(Dsel$Q1)
qqline(Dsel$Q1)


###########################################################################
## Konfidensinterval for middelværdi

## Konfidensinterval for middelværdi af dagligt varmeforbrug i hus 1
t.test(Dsel$Q1, conf.level=0.95)$conf.int


###########################################################################
## T-test for en enkelt stikprøve

## Test af hypotesen mu=2.38 for dagligt varmeforbrug i hus 1
t.test(Dsel$Q1, mu=2.38)


###########################################################################
## Welch t-test for sammenligning af to (uafhængige) stikprøver

## Sammenligning af varmeforbrug i hus 1 og 2
t.test(Dsel$Q1, Dsel$Q2)


###########################################################################
## Beregning af korrelation

## Beregning af korrelation mellem Q1 og G
cor(D[, c("Q1","G")], use="pairwise.complete.obs")

  
###########################################################################
## Delmængder i R

## Ekstra bemærkning om måder at udtage delmænger i R
##
## En logisk (logical) vektor med sandt (TRUE) eller falsk (FALSE) for
## hver række i D, f.eks. de dage hvor der har været frost
D$Ta < 0
## Den kan bruges, når man vil udvælge f.eks. varmeforbrug for hus 1
## på frostdage
D$Q1[D$Ta < 0]
## Alternativt kan man bruge funktionen 'subset'
subset(D, Ta < 0)
## Mere komplekse logiske udtryk kan laves, f.eks. udvælg kun
## observationer fra før 1. januar 2010 hvor der er frostvejr
subset(D, t < "2010-01-01" & Ta < 0)

  
###########################################################################
## Flere R-tips  

## Lav en for-løkke med beregning af et par opsummerende størrelser
## og gem resultatet i en ny data.frame
Tbl <- data.frame()
## Find søjlerne med varmeforbrug
udvalgte <- c("Q1","Q2","Q3","Q4")
## Beregn nøgletallene for hvert hus
for(i in udvalgte){
  ## Udtag den relevante søjle
  x <- Dsel[, i]
  ## Tag stikprøvegennemsnittet
  Tbl[i, "mean"] <- mean(x, na.rm=TRUE)
  ## Tag stikprøvevariansen
  Tbl[i, "var"] <- var(x, na.rm=TRUE) }
## Se hvad der er i Tbl
Tbl
  
## I R er der endnu mere kortfattede måder hvorpår sådanne udregninger 
## kan udføres. For eksempel
apply(Dsel[, udvalgte], 2, mean, na.rm=TRUE)
## eller flere ad gangen i et kald
apply(Dsel[, udvalgte], 2, function(x){
  c(mean=mean(x, na.rm=TRUE),
    var=var(x, na.rm=TRUE)) })
## Se flere smarte funktioner med: ?apply, ?aggregate og ?lapply
## og for ekstremt effektiv databehandling se f.eks. pakkerne: dplyr,
## tidyr, reshape2 og ggplot2.

## LaTeX tips:
## R-pakken "xtable" kan generere LaTeX tabeller og skrive dem direkte 
## ind i en fil, som derefter kan inkluderes i et .tex dokument.
## R-pakken "knitr" kan anvendes meget elegant til at lave et .tex 
## dokument der inkluderer R koden direkte i dokumentet. Dette 
## dokument og bogen er lavet med knitr.
