# extremes.R
# checking extreme values for Harry

wage_v <- df_mincer %>% arrange(wage) %>% select(wage)
percentile <- as.data.frame(quantile(wage_v$wage,probs=seq(0,1,length.out=100)))
colnames(percentile) <- "wage"
write.table(percentile,file="percentile_wage.txt",row.names = FALSE,col.names=FALSE)

exper_v <- df_mincer %>% arrange(exper) %>% select(exper)
percentile <- as.data.frame(quantile(exper_v$exper,probs=seq(0,1,length.out=100)))
colnames(percentile) <- "exper"
write.table(percentile,file="percentile_exper.txt",row.names = FALSE,col.names=FALSE)


edu_yrs_v <- df_mincer %>% arrange(edu_yrs) %>% select(edu_yrs)
percentile <- as.data.frame(quantile(edu_yrs_v$edu_yrs,probs=seq(0,1,length.out=100)))
colnames(percentile) <- "edu_yrs"
write.table(percentile,file="percentile_edu_yrs.txt",row.names = FALSE,col.names=FALSE)

