library(lme4)



setwd('~/Documents/plaster')
df <- read.csv('Adhesive Study Complete Dataset.csv',header=T)
colnames(df)
df <- df[which(df$q_complete == 1),]
df <- df[which(df$sensor_loss %in% c(0,1)),]
df <- df[which(df$patch_use %in% c(0,1)),]
df$patch_use <- factor(df$patch_use)
df$sensor_loss <- factor(df$sensor_loss)
m0 <- glmer(sensor_loss ~ 1 + (1|df$participant_id),
            family='binomial',data=df)

m <- glmer(sensor_loss ~ patch_allocation + (1|df$participant_id),
           family='binomial',data=df)
anova(m0,m)



m0 <- glmer(sensor_loss ~ 1 + (1|df$participant_id),
           family='binomial',data=df)

m <- glmer(sensor_loss ~ patch_use + (1|df$participant_id),
    family='binomial',data=df)
anova(m0,m)
summary(m)
exp(confint(m))

df$nzdepgroup <- 'LOW'
df[which(df$nzdep %in% c(4,5,6,7)),'nzdepgroup'] <- "MEDIUM"
df[which(df$nzdep %in% c(8,9,10)),'nzdepgroup'] <- "HIGH"
table(df$age)

df$sex <- factor(df$sex)
df$phase <- factor(df$phase)
  

m <- glmer(sensor_loss ~ sex + patch_use + nzdepgroup +  
             (1|df$participant_id), data=df, family='binomial')
summary(m)
exp(confint(m))
anova(m,test='Chisq')




dd <- expand.grid(f1 = factor(1:3),
                  f2 = LETTERS[1:2], g=1:9, rep=1:15,
                  KEEP.OUT.ATTRS=FALSE)
summary(mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2))))
dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
str(dd)
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


m <- glmer(sensor_loss ~  patch_use + (1|participant_id),
           family='binomial',data=df)
exp(confint(m))

m <- glmer(sensor_loss ~ sex + factor(ethnicity) + phase + patch_use +   
             (1|participant_id),
           family='binomial',data=df)

setwd('~/Documents/plaster')
df <- read.csv('Adhesive Study Complete Dataset.csv',header=T)
colnames(df)
df <- df[which(df$q_complete == 1),]
df <- df[which(df$sensor_loss %in% c(0,1)),]
df <- df[which(df$patch_use %in% c(0,1)),]

p.nopatch <- vector()
p.patch <- vector()
n.nopatch <- vector()
n.patch <- vector()
pers <- vector()
participants <- unique(df$participant_id)
for (i in 1:length(participants)) {
  p1 <- participants[i]
  y.patch <- df[which(df$participant_id==p1 & df$patch_use==1),"sensor_loss"]
  y.nopatch <- df[which(df$participant_id==p1 & df$patch_use==0),"sensor_loss"]
  y.patch <- as.numeric(as.character(y.patch))
  y.nopatch <- as.numeric(as.character(y.nopatch))
  p.patch[i] <- sum(y.patch)/length(y.patch)
  n.patch[i] <- length(y.patch)
  p.nopatch[i] <- sum(y.nopatch)/length(y.nopatch)
  pers[i] <- toString(p1)
  n.nopatch[i] <- length(y.nopatch)
  
}
df.prop <- data.frame(p.patch = p.patch,
                     p.nopatch = p.nopatch,
                     pers = pers,
                     n.patch = n.patch,
                     n.nopatch = n.nopatch)
dim(df.prop)

plot('',xlim=c(1,32),ylim=c(0,1),xlab='participant',
     ylab='proportion sensor loss')
points((1:32),df.prop$p.patch,col='red',pch=16,cex=df.prop$n.patch/5)
points((1:32),df.prop$p.nopatch,col='blue',pch=16,cex=df.prop$n.nopatch/10)
grid(nx=32,ny=10)


# legend(22,1.45,c('patch','no patch'),
#        lty=c(1,1), 
#        lwd=c(1.5,1.5),col=c('red','black'))
participants[5]

i = 18
p1 <- participants[i]
df[which(df$participant_id==p1),c("sensor_loss",'patch_use')]
which(p.patch < p.nopatch)
which(p.patch > p.nopatch)
which(p.patch == p.nopatch)




library(performance)
binned_residuals(m, term = NULL, n_bins = 30)




m <- glmer(sensor_loss ~ sex + factor(ethnicity) + phase + patch_use +   
             (1|participant_id),
           family='binomial',data=df)
