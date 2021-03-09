#general note: the following script takes into account sex, age and education as possible predictors,
#as well as a selected range of transformations for age and education. Other predictors and transformations
#can be taken into account: practitioners will thus make their choice and modify the script
#as they whish. 

#load the dataset
library(readxl)
ds <- read_excel("path")
View(ds)

#deactivate scientific notation in order to see more decimals
options(scipen=999)

#compute most common transformations for age
ds$cub_age <- ds$age**3
ds$quad_age <- ds$age**2
ds$ln100_age <- log(100-ds$age)
ds$log10100_age <- log10(100-ds$age)
ds$ln_age <- log(ds$age)
ds$log10_age <- log10(ds$age)
ds$sqrt_age <- sqrt(ds$age)
ds$inv_age <- 1/ds$age

#compute most common transformations for education
ds$cub_edu <- ds$edu**3
ds$quad_edu <- ds$edu**2
ds$ln_edu <- log(ds$edu)
ds$log10_edu <- log10(ds$edu)
ds$sqrt_edu <- sqrt(ds$edu)
ds$inv_edu <- 1/ds$edu

#label the outcome as "y"
ds$y <- ds$"outcome"

#test the effect of sex on y
summary(lm(ds$y~ds$sex))

#extract R-squareds for each transformed age 
r2_age=c(summary(lm(ds$y~ds$age))$r.squared,
         summary(lm(ds$y~ds$cub_age))$r.squared,
         summary(lm(ds$y~ds$quad_age))$r.squared,
         summary(lm(ds$y~ds$ln100_age))$r.squared,
         summary(lm(ds$y~ds$log10100_age))$r.squared,
         summary(lm(ds$y~ds$ln_age))$r.squared,
         summary(lm(ds$y~ds$sqrt_age))$r.squared,
         summary(lm(ds$y~ds$inv_age))$r.squared,
         summary(lm(ds$y~ds$log10_age))$r.squared)

#extract R-squareds for each transformed education 
r2_edu=c(summary(lm(ds$y~ds$edu))$r.squared,
         summary(lm(ds$y~ds$cub_edu))$r.squared,
         summary(lm(ds$y~ds$quad_edu))$r.squared,
         summary(lm(ds$y~ds$log10_edu))$r.squared,
         summary(lm(ds$y~ds$ln_edu))$r.squared,
         summary(lm(ds$y~ds$sqrt_edu))$r.squared,
         summary(lm(ds$y~ds$inv_edu))$r.squared)
r2_age
r2_edu

#note here the best transformations for [age] and [education] 

#run the multiple regression model by entering significant predictors with the highest R-squared 
m <- lm(ds$y~ds$"age"+ds$"edu"+ds$sex)
summary(m)

#extract coefficients of interest by substituting "x" with the row number corresponding to the coefficient of interest
bage <- round((coef(summary(m))["x",1]), digits=6)
bedu <- round((coef(summary(m))["x",1]), digits=6)
bsex <- round((coef(summary(m))["x",1]), digits=6)
bage
bedu
bsex
bsex/2

#compute the mean of entered transformed predictors 
mage <- round((mean(ds$"age", nar.rm=T)), digits=6)
medu <- round((mean(ds$"edu", na.rm=T)), digits=6)
mage
medu

#run this line if more decimals than those already shown are needed (e.g., mean of a cubic transformation)
print("m", digits="x")

#insert correction equation: PC=PG+(-bage)*(age-mage)+(-bedu)*(edu-medu)+(-sex)*(sex-0.5)

#compute adjusted scores (ASs)
ds$PC <- ds$y-(bage)*(ds$"age"-mage)-(bedu)*(ds$"edu"-medu)-(bsex)*(ds$sex-0.5)

#rank ASs
ds$rPC <- rank(ds$PC)

#create a dataset that is order according to ASs ranks
ds2 <- ds[order(ds$rPC),]
View(ds2)

install.packages("writexl")
library(writexl)
dse<-data.frame(ds2)
write_xlsx(dse,"path")

#identifying the observations (ris) corresponding to the outer and inner tolerance limits (oTL; iTL)
tolLimits <- function(n){
  q <- 0.05
  r1 <- r2 <- 0
  p1 <- pbeta(q, shape1=r1, shape2=n-(r1+1), lower.tail=T)
  while(p1 >= 0.95){
    r1 <- r1+1
    p1 <- pbeta(q, shape1=r1, shape2=n-(r1+1), lower.tail=T)
  }
  r1 <- r1-1
  p1 <- pbeta(q, shape1=r1, shape2=n-(r1+1), lower.tail=T)
  
  p2 <- pbeta(1-q, shape1=n-(r2+1), shape2=r2, lower.tail=T)
  while(p2<=0.95){
    r2 <- r2+1
    p2 <- pbeta(1-q, shape1=n-(r2+1), shape2=r2, lower.tail=T)
  }
  if(r1 == 0) r1 <- 'not defined'
  return(c(paste('oTL', r1),
           paste('iTL', r2),
           paste('p oTL', round(p1,5)),
           paste('p iTL', round(p2,5)))
  )
}

#insert sample size to get the ris corresponding to TLs
tolLimits(n="x")

#insert the sample size and the oTL to get the last Equivalent Scores (ESs); rounding controls are also provided
n <- "x"
oTL <- "y"
cd1 <- oTL/n
z1 <- qnorm(cd1)

z1_3 <- z1/3
z1_2 <- z1_3*2

cd2 <- pnorm(z1_2)
a <- (cd1-cd2)*n
a_r <- round(a)
ES1 <- -a_r+oTL
ES1

a
a_r

cd3 <- pnorm(z1_3)
b <- (cd3-cd2)*n
b_r <- round(b)
ES2 <- ES1+b_r
ES2

b
b_r

cd4 <- pnorm(0)
c <- (cd4-cd3)*n
c_r <- round(c)
ES3 <- ES2+c_r
ES3

c
c_r

#search for the corresponding ASs within the dataset to find TLs and ESs thresholds; note them in the following lines
#oTL [ri] [AS]
#iTL [ri] [AS]
#last ES1 [ri] [AS]
#last ES2 [ri] [AS]
#last ES3 [ri] [AS]

#run te first three lines and then the subsequent function to get selected correction factors to create adjustment grids 
age <- c(35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
edu <- c(5, 8, 11, 13, 16, 18)
sex <- c(0,1)
for(i in age){
  for (j in edu) {  
    for (k in sex){
      print(c(i, j, k))
      print(
        (-bage*(i-mage)-bedu*(j-medu))-bsex*(k-0.5))
    }  
  }
}