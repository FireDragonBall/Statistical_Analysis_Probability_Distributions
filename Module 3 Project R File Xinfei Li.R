
##  Part 1 ##
library(ggplot2)
x= c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

# a
a=dhyper(x,20,80,20, log = F)
View(a)

# b
b=phyper(x,20,80,20,lower.tail = T )
View(b)

# c
plot(x,a)

# d
plot(x,b)

# e
e1mean=sum(x*a)
e2var=sum((e1mean-x)^2*a)
e3sd=e2var^(1/2)

# f
f=runif(1000, min=0, max=1)
View(f)

# g
g=rhyper(1000,20,80,20)

# h
h1mean=mean(g)
h2var=var(g)
h3sd=sd(g)

# i
i01=mean(rhyper(20,20,80,20))
i02=mean(rhyper(40,20,80,20))
i03=mean(rhyper(60,20,80,20))
i04=mean(rhyper(80,20,80,20))
i05=mean(rhyper(100,20,80,20))
i06=mean(rhyper(200,20,80,20))
i07=mean(rhyper(300,20,80,20))
i08=mean(rhyper(400,20,80,20))
i09=mean(rhyper(500,20,80,20))
i10=mean(rhyper(600,20,80,20))
i11=mean(rhyper(700,20,80,20))
i12=mean(rhyper(800,20,80,20))
i13=mean(rhyper(900,20,80,20))
i14=mean(rhyper(1000,20,80,20))

# j
j01=phyper(20,20,80,20)
jmean01=sum(j01*e1mean)
j02=phyper(40,20,80,20)
jmean02=sum(e1mean*j02)
j03=phyper(60,20,80,20)
jmean03=sum(e1mean*j03)
j04=phyper(80,20,80,20)
jmean04=sum(e1mean*j04)
j05=phyper(100,20,80,20)
jmean05=sum(e1mean*j05)
j06=phyper(200,20,80,20)
jmean06=sum(e1mean*j06)
j07=phyper(300,20,80,20)
jmean07=sum(e1mean*j07)
j08=phyper(400,20,80,20)
jmean08=sum(e1mean*j08)
j09=phyper(500,20,80,20)
jmean09=sum(e1mean*j09)
j10=phyper(600,20,80,20)
jmean10=sum(e1mean*j10)
j11=phyper(700,20,80,20)
jmean11=sum(e1mean*j11)
j12=phyper(800,20,80,20)
jmean12=sum(e1mean*j12)
j13=phyper(900,20,80,20)
jmean13=sum(e1mean*j13)
j14=phyper(1000,20,80,20)
jmean14=sum(e1mean*j14)

# k
I=c(i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14)
n=c(20,40,60,80,100,200,300,400,500,600,700,800,900,1000)
plot(n,I,type='l', lty = 1)
abline(h =e1mean)



## Part 2 ##

library(readxl)
library(ggplot2)
Module_3_Project_Keno_v1_3_ <- read_excel("C:/Users/Administrator/Desktop/Module 3 Project_Keno_v1(3).xlsx", 
                                          sheet = "Part 2")
View(Module_3_Project_Keno_v1_3_)

# a
Pop0=c(Module_3_Project_Keno_v1_3_$`X - A Normal Population`)
Pop=c(Pop0)
mean(Pop)
var(Pop)
sd(Pop)

# b
hist(Pop,breaks = 20, prob=TRUE)

# c,d
x = sample(Pop, 900, replace = T)
result1 <-list()
for (i in 1:30) {
  result1[i] <- mean(sample(x, 30))
}


result2 <-list()
for (i in 1:30) {
  result2[i] <- var(sample(x, 30))
}

result3 <-list()
for (i in 1:30) {
  result3[i] <- sd(sample(x, 30))
}

# e
Samplemean=mean(as.numeric(result1))
Samplevar=mean(as.numeric(result2))
Samplesd=mean(as.numeric(result3))

# f.compare with Central Limit Theorem

mean(Pop)
Samplemean

sd(Pop)
Samplesd
sd(Pop)/sqrt(30)

# g
hist(as.numeric(result1),breaks = 30)



##### or for Part 2 c-e #####


# c

s01=sample(Pop, 30, replace = F)
s02=sample(Pop, 30, replace = F)
s03=sample(Pop, 30, replace = F)
s04=sample(Pop, 30, replace = F)
s05=sample(Pop, 30, replace = F)
s06=sample(Pop, 30, replace = F)
s07=sample(Pop, 30, replace = F)
s08=sample(Pop, 30, replace = F)
s09=sample(Pop, 30, replace = F)
s10=sample(Pop, 30, replace = F)
s11=sample(Pop, 30, replace = F)
s12=sample(Pop, 30, replace = F)
s13=sample(Pop, 30, replace = F)
s14=sample(Pop, 30, replace = F)
s15=sample(Pop, 30, replace = F)
s16=sample(Pop, 30, replace = F)
s17=sample(Pop, 30, replace = F)
s18=sample(Pop, 30, replace = F)
s19=sample(Pop, 30, replace = F)
s20=sample(Pop, 30, replace = F)
s21=sample(Pop, 30, replace = F)
s22=sample(Pop, 30, replace = F)
s23=sample(Pop, 30, replace = F)
s24=sample(Pop, 30, replace = F)
s25=sample(Pop, 30, replace = F)
s26=sample(Pop, 30, replace = F)
s27=sample(Pop, 30, replace = F)
s28=sample(Pop, 30, replace = F)
s29=sample(Pop, 30, replace = F)
s30=sample(Pop, 30, replace = F)


# Import samples into excel designate boxes

Module_3_Project_Keno_v1_3_$'Sample 1'[1:30]=s01
Module_3_Project_Keno_v1_3_$'Sample 2'[1:30]=s02
Module_3_Project_Keno_v1_3_$'Sample 3'[1:30]=s03
Module_3_Project_Keno_v1_3_$'Sample 4'[1:30]=s04
Module_3_Project_Keno_v1_3_$'Sample 5'[1:30]=s05
Module_3_Project_Keno_v1_3_$'Sample 6'[1:30]=s06
Module_3_Project_Keno_v1_3_$'Sample 7'[1:30]=s07
Module_3_Project_Keno_v1_3_$'Sample 8'[1:30]=s08
Module_3_Project_Keno_v1_3_$'Sample 9'[1:30]=s09
Module_3_Project_Keno_v1_3_$'Sample 10'[1:30]=s10
Module_3_Project_Keno_v1_3_$'Sample 11'[1:30]=s11
Module_3_Project_Keno_v1_3_$'Sample 12'[1:30]=s12
Module_3_Project_Keno_v1_3_$'Sample 13'[1:30]=s13
Module_3_Project_Keno_v1_3_$'Sample 14'[1:30]=s14
Module_3_Project_Keno_v1_3_$'Sample 15'[1:30]=s15
Module_3_Project_Keno_v1_3_$'Sample 16'[1:30]=s16
Module_3_Project_Keno_v1_3_$'Sample 17'[1:30]=s17
Module_3_Project_Keno_v1_3_$'Sample 18'[1:30]=s18
Module_3_Project_Keno_v1_3_$'Sample 19'[1:30]=s19
Module_3_Project_Keno_v1_3_$'Sample 20'[1:30]=s20
Module_3_Project_Keno_v1_3_$'Sample 21'[1:30]=s21
Module_3_Project_Keno_v1_3_$'Sample 22'[1:30]=s22
Module_3_Project_Keno_v1_3_$'Sample 23'[1:30]=s23
Module_3_Project_Keno_v1_3_$'Sample 24'[1:30]=s24
Module_3_Project_Keno_v1_3_$'Sample 25'[1:30]=s25
Module_3_Project_Keno_v1_3_$'Sample 26'[1:30]=s26
Module_3_Project_Keno_v1_3_$'Sample 27'[1:30]=s27
Module_3_Project_Keno_v1_3_$'Sample 28'[1:30]=s28
Module_3_Project_Keno_v1_3_$'Sample 29'[1:30]=s29
Module_3_Project_Keno_v1_3_$'Sample 30'[1:30]=s30


#d

S=c(s01,s02,s03,s04,s05,s06,s07,s08,s09,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30)
Slist=matrix(S,nrow=30,ncol=30)
Slist.df= data.frame(Slist)
funs <- function(C) { c(mean=mean(C), var=var(C), sd=sd(C))} 
sapply(Slist.df, funs)


#e

Slist.mean=c(sapply(Slist.df, mean))
Slist.var=c(sapply(Slist.df, var))
Slist.sd=c(sapply(Slist.df, sd))
mean(Slist.mean)
mean(Slist.var)
mean(Slist.sd)
