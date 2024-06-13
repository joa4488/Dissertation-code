# Author: Joachim Dejonckheere
# Purpose: Dissertation for Paster of Science in Statistical Data Analysis
# Academic year: 2023-2024

library(lattice) # for the Trellis plots
library(ggplot2) # for the Trellis plots
library (haven) # to read in SAS datasets later on
library(forestplot)

setwd("P:/JoachimD/TC/Results/Graphs")

# Read the type 3 / LS Means (created in SAS back in into R and create graphs for decision-making
pwr <- read_sas ("P:/JoachimD/TC/Results/Power/power_long.sas7bdat")
tail(pwr)
#pwr <- pw [pw$Rratio == "1:1",]
pwr$te <- paste('Treatment Effect', pwr$meandiff*100,'%')
pwr$model_mis <- paste(pwr$model, '-',pwr$misdata)

summary(pwr)
head(pwr)
table(pwr$model)
table(pwr$misdata)

# Graph of complete data
full <- pwr[pwr$misdata == "full" & pwr$meandiff > 0.15,]
pdf('Sample Sizes on Complete Data.pdf',width = 35 ,height = 20)
xyplot ( power ~ jitter(n) | factor(sd, labels =  c('Low Variation (SD=4)', 'Medium Variation (SD=6)', 'High Variation (SD=9)')) + factor(te),
         groups = factor (model_mis),
         auto.key = list(columns = 3, lines=TRUE, points=FALSE),
         data = full,
         type=c( 'smooth', 'smooth', 'smooth', 'p'),
         layout = c (3, 3, 1),
         abline=(h = 80),#, col="red" , lty = 2),
         main = 'Power for Different Endpoints (Full Data)',
         ylab='Power',
         xlab='Sample Size')
dev.off()

# Graph of MCAR data
mcar <- pwr[pwr$misdata == "mcar" & pwr$meandiff > 0.15,]
pdf('Sample Sizes on MCAR Data.pdf',width = 35 ,height = 20)
xyplot ( power ~ jitter(n) | factor(sd, labels =  c('Low Variation (SD=4)', 'Medium Variation (SD=6)', 'High Variation (SD=9)')) + factor(te),
         groups = factor (model_mis),
         auto.key = list(columns = 3, lines=TRUE, points=FALSE),
         data = mcar,
         type=c( 'smooth', 'smooth', 'smooth', 'p'),
         layout = c (3, 3, 1),
         abline=(h = 80),#, col="red" , lty = 2),
         main = 'Power for Different Endpoints (Full Data)',
         ylab='Power',
         xlab='Sample Size')
dev.off()


# Graph of MAR data
mar <- pwr[pwr$misdata == "mar" & pwr$meandiff > 0.15,]
pdf('Sample Sizes on MAR Data.pdf',width = 35 ,height = 20)
xyplot ( power ~ jitter(n) | factor(sd, labels =  c('Low Variation (SD=4)', 'Medium Variation (SD=6)', 'High Variation (SD=9)')) + factor(te),
         groups = factor (model_mis),
         auto.key = list(columns = 3, lines=TRUE, points=FALSE),
         data = mar,
         type=c( 'smooth', 'smooth', 'smooth', 'p'),
         layout = c (3, 3, 1),
         abline=(h = 80),#, col="red" , lty = 2),
         main = 'Power for Different Endpoints (Full Data)',
         ylab='Power',
         xlab='Sample Size')
dev.off()

# Graph of MNAR data
mnar <- pwr[pwr$misdata == "mnar" & pwr$meandiff > 0.15,]
pdf('Sample Sizes on MNAR Data.pdf',width = 35 ,height = 20)
xyplot ( power ~ jitter(n) | factor(sd, labels =  c('Low Variation (SD=4)', 'Medium Variation (SD=6)', 'High Variation (SD=9)')) + factor(te),
         groups = factor (model_mis),
         auto.key = list(columns = 3, lines=TRUE, points=FALSE),
         data = mnar,
         type=c( 'smooth', 'smooth', 'smooth', 'p'),
         layout = c (3, 3, 1),
         abline=(h = 80),#, col="red" , lty = 2),
         main = 'Power for Different Endpoints (Full Data)',
         ylab='Power',
         xlab='Sample Size')
dev.off()

# Graph of MMRM-UN
mmrm_un <- pwr[pwr$model == "mmrm_un" & pwr$meandiff > 0.15,]


pdf('Sample Sizes on for MMRM UN.pdf',width = 35 ,height = 20)
xyplot ( power ~ jitter(n) | factor(sd, labels =  c('Low Variation (SD=4)', 'Medium Variation (SD=6)', 'High Variation (SD=9)')) + factor(te),
         groups = factor (model_mis),
         auto.key = list(columns = 3, lines=TRUE, points=FALSE),
         data = mmrm_un,
         type=c( 'smooth', 'smooth', 'smooth', 'p'),
         layout = c (3, 3, 1),
         abline=(h = 80),#, col="red" , lty = 2),
         main = 'Power for Different Endpoints (Full Data)',
         ylab='Power',
         xlab='Sample Size')
dev.off()


### FORREST PLOTS OF BIAS (DESCRIPTIVE)  ####
bias <- read_sas ("P:/JoachimD/TC/Results/Power/emp_ci.sas7bdat")
head(bias)
bias$fvard <- gsub('Missingness','Dropout',bias$fvar)
table(bias$fvard)

### Interactions with Model ###
biasm <- bias[grepl('Model', bias$var ),]
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",biasm$fvard), 
                   c("Bias (95 %CI)","\n",biasm$prob_bias))

pdf('Bias (descriptively) - Model.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,biasm$bias_mean), 
           lower=c(NA,NA,biasm$lclm_bias), upper=c(NA,NA,biasm$uclm_bias),
           title="Mean bias and 95% CI",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "11" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "14" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "17" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "20.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "24" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "27" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "30" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "37" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "43" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "49.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-8, -6, -4, -2, 0, 2), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()


### Interactions with Type of Missing Data ###
biasmi <- bias[grepl('Missing', bias$var ) & bias$var != 'Missingness * Model',]
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",biasmi$fvard), 
                   c("Bias (95 %CI)","\n",biasmi$prob_bias))

pdf('Bias (descriptively) - Dropout.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,biasmi$bias_mean), 
           lower=c(NA,NA,biasmi$lclm_bias), upper=c(NA,NA,biasmi$uclm_bias),
           title="Mean bias and 95% CI",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "8" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "15" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "18" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "21" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "28" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "34" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "40" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "46" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "50" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "72" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "76" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-8, -6, -4, -2, 0, 2), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()



### FORREST PLOTS OF BIAS (DESCRIPTIVE) IN THE SELECTED SITUATION ####
bias_s <- read_sas ("P:/JoachimD/TC/Results/Power/emp_ci_sel.sas7bdat")
bias_s$fvard <- gsub('Missingness','Dropout',bias_s$fvar)


## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",bias_s$fvard), 
                   c("Bias (95 %CI)","\n",bias_s$prob_bias))

pdf('Bias in the selected situation (descriptively).pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,bias_s$bias_mean), 
           lower=c(NA,NA,bias_s$lclm_bias), upper=c(NA,NA,bias_s$uclm_bias),
           title="Mean bias and 95% CI",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "16" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "19" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "22" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "25" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-8, -6, -4, -2, 0, 2), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()


### FORREST PLOTS OF ABSOLUTE BIAS (DESCRIPTIVE)  ####

bias_a <- read_sas ("P:/JoachimD/TC/Results/Power/emp_ci_abs.sas7bdat")
bias_a$fvard <- gsub('Missingness','Dropout',bias_a$fvar)

### Interactions with Model ###
biasm_a <- bias_a[grepl('Model', bias_a$var ),]
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",biasm_a$fvard), 
                   c("Bias (95 %CI)","\n",biasm_a$prob_bias))

pdf('Bias Absolute (descriptively) - Model.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,biasm_a$bias_mean), 
           lower=c(NA,NA,biasm_a$lclm_bias), upper=c(NA,NA,biasm_a$uclm_bias),
           title="Mean bias and 95% CI",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "11" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "14" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "17" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "20.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "24" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "27" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "30" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "37" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "43" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "49.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-8, -6, -4, -2, 0, 2), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()


### Interactions with Type of Missing Data ###
biasmi_a <- bias_a[grepl('Missing', bias_a$var ) & bias_a$var != 'Missingness * Model',]
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",biasmi_a$fvard), 
                   c("Bias (95 %CI)","\n",biasmi_a$prob_bias))

pdf('Bias Absolute (descriptively) - Dropout.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,biasmi_a$bias_mean), 
           lower=c(NA,NA,biasmi_a$lclm_bias), upper=c(NA,NA,biasmi_a$uclm_bias),
           title="Mean bias and 95% CI",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "8" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "15" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "18" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "21" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "28" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "34" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "40" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "46" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "50" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "72" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "76" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-8, -6, -4, -2, 0, 2), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()



### FORREST PLOTS OF PRECISION ####
prec <- read_sas ("P:/JoachimD/TC/Results/Power/emp_ci.sas7bdat")
head(prec)
prec$fvard <- gsub('Missingness','Dropout',prec$fvar)

### Interactions with Model ###
precm <- prec[grepl('Model', prec$var ),]
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",precm$fvard), 
                   c("Width (95 %CI)","\n",precm$prob))

pdf('Precision - Model.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,precm$width), 
           lower=c(NA,NA,precm$diff_p2_5), upper=c(NA,NA,precm$diff_p97_5),
           title="95% Empirical CI around the estimate",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "11" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "14" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "17" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "20.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "24" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "27" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "30" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "37" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "43" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "49.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-2, 0, 2, 4, 6, 8, 10), 
                            gp = gpar(lty = 2, col = "gray")),
            cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()


### Interactions with Type of Missing Data ###
precmi <- prec[grepl('Missing', prec$var ) & prec$var != 'Missingness * Model',]
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",precmi$fvard), 
                   c("Width (95 %CI)","\n",precmi$prob))

pdf('Precision - Dropout.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,precmi$width), 
           lower=c(NA,NA,precmi$diff_p2_5), upper=c(NA,NA,precmi$diff_p97_5),
           title="95% Empirical CI around the estimate",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "8" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "15" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "18" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "21" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "28" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "34" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "40" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "46" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "50" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "72" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "76" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-2, 0, 2, 4, 6, 8, 10), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()

### FORREST PLOTS OF PRECISION (DESCRIPTIVE) IN THE SELECTED SITUATION ####

prec_s <- read_sas ("P:/JoachimD/TC/Results/Power/emp_ci_sel.sas7bdat")
prec_s$fvard <- gsub('Missingness','Dropout',prec_s$fvar)

## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",prec_s$fvard), 
                   c("Width (95 %CI)","\n",prec_s$prob))

pdf('Precision in the selected situation.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,prec_s$width), 
           lower=c(NA,NA,prec_s$diff_p2_5), upper=c(NA,NA,prec_s$diff_p97_5),
           title="95% Empirical CI around the estimate",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "16" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "19" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "22" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(-2, 0, 2, 4, 6, 8, 10), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()


### FORREST PLOTS OF POWER (DESCRIPTIVE) ####
pwr <- read_sas ("P:/JoachimD/TC/Results/Power/power_ci.sas7bdat")
head(pwr)
pwr$fvard <- gsub('Missingness','Dropout',pwr$fvar)

### Interactions with Model ###
pwrm <- pwr[grepl('Model', pwr$var ),]
pwrm$binr <- round(pwrm$bin,1)

## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",pwrm$fvard), 
                   c("Power","\n",pwrm$binr))

pdf('Power (descriptively) - Model.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,pwrm$bin), 
           lower=c(NA,NA,pwrm$binr), upper=c(NA,NA,pwrm$binr),
           title="Power",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "11" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "14" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "17" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "20.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "24" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "27" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "30" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "37" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "43" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "49.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(0, 20, 40, 60, 80, 100), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()



### Interactions with Type of Missing Data ###
pwrmi <- pwr[grepl('Missing', pwr$var ) & pwr$var != 'Missingness * Model',]
pwrmi$binr <- round(pwrmi$bin,1)
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",pwrmi$fvard), 
                   c("Power","\n",pwrmi$binr))

pdf('Power (descriptively) - Dropout.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,pwrmi$bin), 
           lower=c(NA,NA,pwrmi$binr), upper=c(NA,NA,pwrmi$binr),
           title="Power",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "8" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "15" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "18" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "21" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "28" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "34" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "40" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "46" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "50" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "64" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "68" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "72" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "76" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(0, 20, 40, 60, 80, 100), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()


### FORREST PLOTS OF POWER (DESCRIPTIVE) ####
t1 <- read_sas ("P:/JoachimD/TC/Results/Power/t1_ci.sas7bdat")
head(t1)
t1$fvard <- gsub('Missingness','Dropout',t1$fvar)

### Interactions with Model ###
t1m <- t1[grepl('Model', t1$var )& t1$var != 'Model * Treatment Effect',]
t1m$binr <- round(t1m$bin,2)

## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",t1m$fvard), 
                   c("Type I error (95% CI)","\n",t1m$binr))

pdf('Type I error (descriptively) - Model.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,t1m$bin), 
           lower=c(NA,NA,t1m$binr), upper=c(NA,NA,t1m$binr),
           title="Type I error",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "11" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "14" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "17" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "20.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "24" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "27" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "30" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "37" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "43" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "49.5" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(2.5), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(2, 2.25, 2.5, 2.75, 3), zero=NA,
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()




### Interactions with Type of Missing Data ###
t1mi <- t1[grepl('Missing', t1$var ) & t1$var != 'Missingness * Model' & t1$var != 'Missingness * Treatment Effect',]
t1mi$binr <- round(t1mi$bin,2)
## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",t1mi$fvard), 
                   c("Type I error (95% CI)","\n",t1mi$binr))

pdf('Type I error (descriptively) - Dropout.pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,t1mi$bin), 
           lower=c(NA,NA,t1mi$binr), upper=c(NA,NA,t1mi$binr),
           title="Type I error",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "8" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "15" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "18" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "21" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "28" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "34" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "40" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "46" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             
             "50" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "53" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "56" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "59" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(2.5), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(2, 2.25, 2.5, 2.75, 3), zero=NA,
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()

### FORREST PLOTS OF POWER ON THE SELECTED SAMPLE ####
pwr_s <- read_sas ("P:/JoachimD/TC/Results/Power/power_ci_sel.sas7bdat")
pwr_s$fvard <- gsub('Missingness','Dropout',pwr_s$fvar)

pwr_s$binr <- round(pwr_s$bin,1)

## The rest of the columns in the table. 
tabletext <- cbind(c("Variable","\n",pwr_s$fvard), 
                   c("Power","\n",pwr_s$binr))

pdf('Power for the selected scenario (descriptively).pdf',width = 21 ,height = 29)
forestplot(labeltext=tabletext, graph.pos=2, 
           mean=c(NA,NA,pwr_s$bin), 
           lower=c(NA,NA,pwr_s$binr), upper=c(NA,NA,pwr_s$binr),
           title="Power",
           #xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list(#"3" = gpar(lwd=1, col="#99999922"), 
             "3" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "7" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "12" = gpar(lwd=2, lineend="butt", columns=c(1:3), col="black"),
             "16" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "19" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black"),
             "22" = gpar(lwd=2, lty=3, lineend="butt", columns=c(1:3), col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=2.1),
                          ticks=gpar(cex=2.1),
                          xlab=gpar(cex = 2.1),
                          title=gpar(cex = 2.1)),
           
           col=fpColors(box="black", lines="black", zero = "gray"),
           grid = structure(c(0, 20, 40, 60, 80, 100), 
                            gp = gpar(lty = 2, col = "gray")),
           cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           xticks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()


# Posthoc graph
ph1 <- read_sas ("P:/JoachimD/TC/Results/Power/posthoc.sas7bdat")
ggplot(ph1, aes(corr, bias_median)) +
  geom_line (aes(color = model)) +
  geom_point() +
  geom_errorbar(aes(ymin = lclm_bias, ymax = uclm_bias))
