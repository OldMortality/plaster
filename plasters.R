library(lme4)



setwd('~/Documents/plaster')
df <- read.csv('Adhesive Study Complete Dataset.csv',header=T)
colnames(df)
df <- df[which(df$q_complete == 1),]
df <- df[which(df$sensor_loss %in% c(0,1)),]
df <- df[which(df$patch_use %in% c(0,1)),]
df$patch_use <- factor(df$patch_use)
df$sensor_loss <- factor(df$sensor_loss)

m <- glmer(sensor_loss ~ patch_use + (1|df$participant_id),
    family='binomial',data=df)
summary(m)
exp(confint(m))

df$nzdepgroup <- 'LOW'
df[which(df$nzdep %in% c(4,5,6,7)),'nzdepgroup'] <- "MEDIUM"
df[which(df$nzdep %in% c(8,9,10)),'nzdepgroup'] <- "HIGH"
table(df$age)

df$sex <- factor(df$sex)
df$phase <- factor(df$phase)


m <- glmer(sensor_loss ~ sex + patch_use + nzdepgroup +  
             (1|df$participant_id),
           family='binomial',data=df)
exp(confint(m))
anova(m,test='Chisq')


## non compliance
table(df$patch_allocation,df$patch_use)
nrecs <- which(df$patch_allocation != df$patch_use)
non.comp <- unique(df[nrecs,"participant_id"])
length(non.comp)


m <- glmer(sensor_loss ~ patch_allocation + (1|df$participant_id),
           family='binomial',data=df)
summary(m)
exp(confint(m))

df <- df[-nrecs,]
m <- glmer(sensor_loss ~ patch_use + (1|df$participant_id),
           family='binomial',data=df)
summary(m)
exp(confint(m))


m <- data.frame(sensor_loss=df$sensor_loss,patch_use = df$patch_use
                )
margin.table(m)
t <- table(df$sensor_loss,df$patch_use)
addmargins(t,margin=c(1,2))
rownames(t) <- c("fred",'joe')
t
fisher.test(table(df$sensor_loss,df$patch_use))


df.pp <- df[-nrecs,]
dim(df.pp)


m <- glmer(sensor_loss ~ sex + factor(ethnicity) + phase + patch_use +   
             (1|participant_id),
           family='binomial',data=df)
