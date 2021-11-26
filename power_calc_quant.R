
#POWER CALC FOR QUANT TRAIT

#REPLACE "TRAIT" with tested trait
alpha=0.05
th=qchisq(alpha,df=1,lower.tail=F); # Significance threshold for chi-square, corresponding to P-value threshold
beta=TRAIT$beta.UK #beta in europeans
maf=TRAIT$EAF.GH # frequency in south asians
n=TRAIT$N #N south asians

pow=matrix(NA,nrow=length(beta),ncol=length(maf)); # Pre-allocalte matrix to store power values
rownames(pow)=beta; colnames(pow)=maf; # Assign row and column names

q2=2*maf*(1-maf)*(beta^2); ncp=n*q2/(1-q2); # Calculate qsq and then NCP parameter
pow=pchisq(th,df=1,lower.tail=F,ncp=ncp)

pow

sum(pow,na.rm=T) #sum up power and divide by number of loci/variants for total expected power

TRAIT$power=pow

TRAIT$powered=NA
TRAIT$powered=replace(TRAIT$powered, TRAIT$power>=0.8, "Yes")
TRAIT$powered=replace(TRAIT$powered, TRAIT$power<0.8, "No")
