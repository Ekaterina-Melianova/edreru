#snippets.R

#  b <- deparse(substitute(var)) to refer to name only
options(scipen=999) # to supress scientific notation
FreqEM <- function(var){
  result <- freq(var, levels = "values", total = T)
  result <- rbind(result, 
                  UserNA = apply(result[as.character(99999997:99999999),],2,sum),
                  TotalNA = apply(result[c(99999997:99999999, "NA"),],2,sum, na.rm = T))
  print(colnames(result))
  newdata <<- result
  a <- descr::freq(var,plot=FALSE)
  b <- deparse(substitute(var))
  print(b)
  plot(a,main=paste(colnames(a)[1],"of",b),col="blue")
}

FreqEM(temp1$J4_1)

# System coded big numbers as missing
temp1$J4_1N <- as.numeric(temp1$J4_1)
freq(temp1$J4_1)
freq(temp1$J4_1N)

descr::freq(temp1$J4_1N)




result <- freq(temp1$J4_1,levels="values",total=T)

rownames(newdata)


options(scipen=999) # to supress scientific notation
FreqSP <- function(var){
  a <- descr::freq(var,plot=FALSE)
  print(class(a))
  print("ZZZZZZZZZZZZZZZZZZZZZZZZZZ")
  b <- deparse(substitute(var))
  z1 <- dim(a)[1] - 2
  print(z1)
  z2 <- rownames(a)[1:z1]
  z3 <- as.matrix(a[1:z1,1:3])
  colnames(z3) <-  colnames(a)
  print("xxxxxxxxxxxxxxxxxxxxxxxxxx")
  print(dim(z3))
  print(z3)
  c <- as.numeric(as.character(rownames(z3)))
  #z3b <- cbind(z3,c)
  #print(z3b)
  print("VVVVVV")
  rownames(z3) <- c
  z3 <- z3[order(rownames(z3)), ]
  print(z3)
  bar(z3[,2],main=paste(colnames(a)[1],"of",b),col="blue", ylab="count")
  axis(2,font=2,col="red")
  axis(1,font=1,col="black",labels=FALSE)
}
###
FreqSP(temp1$J4_1)
