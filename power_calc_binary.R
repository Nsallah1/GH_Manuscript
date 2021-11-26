#POWER CALC FOR Binary TRAIT

#REPLACE "TRAIT" with tested trait

TRAIT<-TRAIT[order(TRAIT$P.GH),] #keep ordering so you can merge with power calc
alpha<-0.05

## N south asians in GH
N_ctrl<-20898
N_cas<-1110
N_ctrl<-9000
N_cas<-9000
N_tot<-N_cas+N_ctrl
n<-N_tot


# ratio cases controls
r<-N_cas/N_ctrl

# proportion of exposed controls south asians
p_ctrl<-TRAIT$AF.Controls
f<-p_ctrl

# # # # beta europeans
b<-TRAIT$beta.UKB

phi<-r 

POWER<-pchisq(qchisq(alpha,df=1,lower = F), df=1, ncp = 2*f*(1-f)*n*phi*(1-phi)*b^2, lower = F)
POWER 
sum(POWER,na.rm=T) #sum up power and divide by number of loci/variants for total expected power

TRAIT$power=POWER

TRAIT$powered=NA
TRAIT$powered=replace(TRAIT$powered, TRAIT$power>=0.8, "Yes")
TRAIT$powered=replace(TRAIT$powered, TRAIT$power<0.8, "No")
