#rids.R
# file to get rid en masse of datafiles in global environment

alist <- c(ls(pattern="OK.18.*"))
rm(list=alist)
rm(alist) # to get rid of list in values tab of global env.
# mget(paste0("DKR.",j,sep="")

#alist <- ls()
#alist <- alist[1]
#rm(list=alist)
#rm(alist)






