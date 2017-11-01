rm(list=ls())
setwd("C:\\Users\\joegbr\\Dropbox\\MMM_methods_paper\\wiki\\datafile") #set your working directory
require(sommer)
load("MMM_wiki_data.RData")
require(sommer)
m.biv<-mmer2(cbind(z1,z2)~1,random=~us(trait):ID, rcov=~us(trait):units, data=df.z)
summary(m.biv)
#proportion explained by between-individual variance
pin(m.biv,prop.ID~V1/(V1+V4))
pin(m.biv,prop.ID~V3/(V3+V6))
#correlation on the between-individual level
pin(m.biv,r.ID~V2/sqrt(V1*V3))
#correlation on the residual level
pin(m.biv,r.res~V5/sqrt(V4*V6))
#correlation on the phenotypic level
pin(m.biv,r.pheno~(V2+V5)/sqrt((V1+V4)*(V3+V6)))
#phenotypic variances
pin(m.biv,V.pheno.z1~V1+V4)*var(df.z$z1)
pin(m.biv,V.pheno.z2~V3+V6)*var(df.z$z2)
