library("impute")
library("ggplot2")
library("Biobase")
library("curatedBreastData")
data(curatedBreastDataExprSetList)
proc_curatedBreastDataExprSetList <- processExpressionSetList(exprSetList=curatedBreastDataExprSetList[1:2])
#load up master clinical data table 
data("clinicalData")
# converting clinical data to data frames 

clintable_df <- data.frame(clinicalData$clinicalTable)
var<- data.frame(clinicalData$clinicalVarDef)
#patients
length(clintable_df$dbUniquePatientID)
#variables:
nrow(var)

#nominal:
na_removed<- clintable_df[!is.na( clintable_df$menopausal_status),]
menopausal_values <- na_removed$menopausal_status
menopausal_values

#ordinal:
na_removed <- clintable_df[!is.na( clintable_df$top2atri_preTrt),]
ordinal_values <- na_removed$top2atri_preTrt
ordinal_values

#interval:
na_removed <- clintable_df[!is.na( clintable_df$RFS_months_or_MIN_months_of_RFS),]
MONTHS <- na_removed$RFS_months_or_MIN_months_of_RFS
MONTHS
#ratio:
na_removed <- clintable_df[!is.na( clintable_df$tumor_size_cm_postTrt),]
ratio <- na_removed$tumor_size_cm_postTrt
ratio

na.exclude(clintable_df$tumor_size_cm_postTrt)



#b
age <- clintable_df$age
mean_age <- mean(age,na.rm = TRUE)
mean_age
median_age <- median(clintable_df$age, na.rm = TRUE)
median_age


# ii) 
hist(clintable_df$age,
     main = "Histogram of ages in Breast cancer data",
     xlab = "age")
#iii) 
hispanic_females <- clintable_df$race == "hispanic"
sum(hispanic_females, na.rm = TRUE)
asia <- clintable_df$race == "asian"
sum(asia, na.rm = TRUE)
table(clintable_df$nationality)


race <- data.frame(clintable_df$race)
race <- race[!is.na(race)]
unique(race)

#c
#cancer cells do not have estrogen, progesterone or HER2 receptors
four_columns <- three_columns <- data.frame(clintable_df$ESR1_preTrt,
                                            clintable_df$PR_preTrt,
                                            clintable_df$HER2_preTrt,
                                            clintable_df$pam50)

#removing NA's 
subsett <- four_columns[,colnames(four_columns)]
#data[complete.cases(subset_data),]
na_removed <- four_columns[complete.cases(subsett),]
colnames(na_removed)
triple_negative <- na_removed[na_removed$clintable_df.ESR1_preTrt == 0 &
                                  na_removed$clintable_df.PR_preTrt == 0 &
                                  na_removed$clintable_df.HER2_preTrt == 0,]

nrow(triple_negative)
triple_negative$clintable_df.pam50



#d
AJCC_age <- data.frame(clintable_df$clinical_AJCC_stage, clintable_df$age)
subsett <- AJCC_age[,colnames(AJCC_age)]
na_removed <- AJCC_age[complete.cases(subsett),]
table(na_removed$clintable_df.clinical_AJCC_stage)

IIIC <- na_removed[na_removed$clintable_df.clinical_AJCC_stage == "IIIC",]
min(IIIC$clintable_df.age)

#e

menopausal_status <- var[var$variableName == "menopausal_status",]
menopause <- clintable_df[!is.na(clintable_df$menopausal_status),]
menopause$menopausal_status

menopausal<- data.frame(menopause)


menopausal$menopausal_stat <- ifelse(menopausal$menopausal_status == "post", 1, 0)
table(menopausal$menopausal_stat)
table(menopausal$menopausal_status)


#f
write.csv(menopausal, "file.csv", row.names = F)
menopausal<- read.csv(file = "file.csv")

######E2##########

#A

boxplot(menopausal$tumor_size_cm_preTrt_preSurgery~ menopausal$menopausal_stat,
        main = "Tumor size and menopausal status",
        ylab = "Tumor size",
        xlab = "Menopausal status")

#using shapiro test for the tumor size
res_shapiro<- shapiro.test(menopausal$tumor_size_cm_preTrt_preSurgery)
res_shapiro$p.value

#since pvalue is lower than 0.05 the distribution is not normally distributed

#visualizing the frequency of tumor sizes 
hist(menopausal$tumor_size_cm_preTrt_preSurgery,
     main = "Tumour size frequency",
     xlab = "Tumour size (cm)")

#now we test the hypotheses using wilcox test 

pre <- subset(menopausal, menopausal$menopausal_stat== 0)
post <- subset(menopausal, menopausal$menopausal_stat== 1)

res_wilcox <- wilcox.test(pre$tumor_size_cm_preTrt_preSurgery, post$tumor_size_cm_preTrt_preSurgery)
res_wilcox$p.value




#B
#investigate the normality of RFS_months_or_MIN_months_of_RFS in each group

RFS_pre_menopause <- pre$RFS_months_or_MIN_months_of_RFS
RFS_post_menopause <- post$RFS_months_or_MIN_months_of_RFS

hist(RFS_post_menopause)
hist(RFS_pre_menopause)
#test for normality 
res_post <- shapiro.test(RFS_post_menopause)
res_post$p.value

res_pre <- shapiro.test(RFS_pre_menopause)
res_pre$p.value

#basic correlation between months until relapse and metastase in pre and post menopausal patients

#creating data frames 
post_cor <- data.frame(post$RFS_months_or_MIN_months_of_RFS, post$metastasis)
pre_cor <- data.frame(pre$RFS_months_or_MIN_months_of_RFS, pre$metastasis)

#correlation for the post menopausal patients
res <- cor.test(post_cor$post.RFS_months_or_MIN_months_of_RFS,post_cor$post.metastasis, method = 'spearman')
res

#correlation for the pre menopausal patients:
res <- cor.test(pre_cor$pre.RFS_months_or_MIN_months_of_RFS, pre_cor$pre.metastasis, method = 'spearman')
res


