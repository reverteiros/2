
setwd("C:/Users/rosa/Desktop")

aaa<-read.table("taula1.txt", header=T,colnames(1))
bbb<-read.table("taula2.txt", header=T,colnames(1))
ccc<-read.table("taula3.txt", header=T,colnames(1))
ddd<-read.table("taula4.txt", header=T,colnames(1))
eee<-read.table("taula5.txt", header=T,colnames(1))

a<-as.matrix(aaa)
b<-as.matrix(bbb)
c<-as.matrix(ccc)
d<-as.matrix(ddd)
e<-as.matrix(eee)

llista<-list(a,b,c,d,e)

arrayllista<-as.array(llista)


# data to test
 
 w1 <- matrix(ncol = 6, nrow = 6, c(1,1,1,1,1,0,
                                   1,1,0,1,0,1,
                                   1,0,1,0,0,0,
                                   1,0,0,0,0,0,
                                   0,1,0,1,0,0,
                                   1,0,0,0,0,0),
             dimnames = list(c("a","b", "c", "d", "e","f"),
                             c("A", "B", "C","D","E","F")), byrow = TRUE)
 
 w2 <- matrix(ncol = 5, nrow = 5, c(1,1,1,1,1,
                                   0,1,0,1,0, #small change in OS (2 links)
                                   1,0,0,0,0,
                                   1,0,1,0,0, #I also add one link to turnover
                                   0,1,0,1,0),
           dimnames = list(c("a","b", "c", "g", "h"),
                             c("A", "B", "C","G","H")), byrow = TRUE)
 
 
x <- betalink(w1,w2, calculate_2nd_decomposition = TRUE)

 #predictions:
 #(poisot decomposition)
 x$WN #is equal to 
 x$OS + x$ST
 
 
# (Carvalho decomposition)
 x$WN #is equal to 
 x$WN_3 + x$WNrich
 
# (Total decomposition)
 x$WN #is equal to
 x$OS_3 + x$OSrich + x$ST_3 + x$STrich
     
# calculate contributions:
 x$OSrich/x$OS # contribution of having higher conectance in one network among the species forming the common subweb
 x$STrich/x$ST # contribution of having higher number of links in one network among the non common species
