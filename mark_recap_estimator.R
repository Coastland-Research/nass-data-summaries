# Andy's mark-recapture estimator code
library(tidyverse)
m=c(50,100,150,200)
r=seq(1,50,1)
c=c(100,150,200,250)

store=matrix(nrow=800,ncol=5)
colnames(store)<-c("m","c","r","n","cv")
store<-data.frame(store)
index=1
for (k in 1:4) {
  c.k=c[k]
  
  for (j in 1:4) {
    
    m.j=m[j]
    
    for (i in 1:50) {
      
      n=c.k*m.j/r[i]
      cv=sqrt(((m.j+1+1)*(c.k+1)*(m.j+r[i])*(c.k-r[i]))/((r[i]+1)^2*(r[i]+2)))/n
      store$m[index]=m.j
      store$c[index]=c.k
      store$r[index]=r[i]
      store$n[index]=n
      store$cv[index]=cv
      index=index+1
    }
  }
}

store

ggplot(store,aes(x=r,y=n,color=factor(c)))+
  geom_line()+
  facet_wrap(.~m)
labs(color="# of Captures",x="# of recaps")

ggsave("mr test.png",dpi=300,units="in",height=6,width=6)

ggplot(store,aes(x=r,y=cv,color=factor(c)))+
  geom_line()+
  facet_wrap(.~m)+
  labs(color="# of Captures",x="# of recaps")

ggsave("mr test cvs.png",dpi=300,units="in",height=6,width=6)
