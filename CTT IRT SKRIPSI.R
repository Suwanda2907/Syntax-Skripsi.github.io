########################
# CLASICAL TEST THEORY #
########################

#HARUS PAKAI FORMAT CSV
setwd("C:/Users/suwan/OneDrive/Documents")

library(psych)
library(CTT)
library(tidyverse)

# Import Data
ctt.data <- read.csv("O11.csv", header = TRUE, na.strings = " ")
key <- as.matrix(ctt.data)

#scoring
myScore <- score(key, output.scored=TRUE)

#Descriptive
describe(myScore$score)

#############################
cronbachs.alpha <-
  function(X){
    
    X <- data.matrix(X)
    n <- ncol(X) # Number of items
    k <- nrow(X) # Number of examinees
    
    # Cronbachs alpha
    alpha <- (n/(n - 1))*(1 - sum(apply(X, 2, var))/var(rowSums(X)))
    
    return(list("Crombach's alpha" = alpha,
                "Number of items" = n,
                "Number of examinees" = k))
  }

dump("cronbachs.alpha", file = "cronbachs.alpha.R")

# compute cronbachs alpha
cronbachs.alpha(key)
################################

#Standard Error of Measurement
SEM <-
  function(X){
    source("cronbachs.alpha.R")
    X <- data.matrix(X)
    
    return(sd(rowSums(X)) * sqrt(1 - cronbachs.alpha(X)[[1]]))
  }
SEM(key)

###############################

#Item Analysis/ RPbis (a) & P-value (b)
item.analysis <- 
  function(responses){
    # CRITICAL VALUES
    cvpb = 0.20
    cvdl = 0.15
    cvdu = 0.85
    
    require(CTT, warn.conflicts = FALSE, quietly = TRUE)
    (ctt.analysis <- CTT::reliability(responses, itemal = TRUE, NA.Delete = TRUE))
    
    # Mark items that are potentially problematic
    item.analysis <- data.frame(item = seq(1:ctt.analysis$nItem),
                                r.pbis = ctt.analysis$pBis,
                                bis = ctt.analysis$bis,
                                item.mean = ctt.analysis$itemMean,
                                alpha.del = ctt.analysis$alphaIfDeleted)
    
    # code provided by Dr. Gordon Brooks
    if (TRUE) {
      item.analysis$check <- 
        ifelse(item.analysis$r.pbis < cvpb |
                 item.analysis$item.mean < cvdl |
                 item.analysis$item.mean > cvdu, "‡", "")
    }
    
    return(item.analysis)
  }

dump("item.analysis", file = "item.analysis.R")

knitr::kable(item.analysis(key), 
             align = "c",
             caption = "Item Analysis")
###############################################################################

#####################
# UNIDIMENSINALITAS #
#####################

#HARUS PAKAI FORMAT CSV
# Import Data
ctt.data <- read.csv("I11.csv", header = TRUE, na.strings = " ")
str(ctt.data)

library(tidyverse)
library(dplyr)
library(psych)

items <- ctt.data

extract <- 1
minload <- .3
nsize <- nrow(items)
numitems <- ncol(items)

require(paran, warn.conflicts = FALSE, quietly = TRUE)

pca <- paran(items,
             centile = 95,
             cfa = FALSE,
             iterations = 25,
             graph = TRUE,
             color = TRUE,
             all = TRUE)
#######################################

########################
# ITEM RESPONSE THEORY #
########################

# LTM (PAKAI EXCEL)
library(eRm)
library(ltm)
library(difR)

# 1Pl
out1 <- ltm(I11~z1, constraint = cbind(c(1:ncol(I11)), 2, 1))
coef(out1)

#PLOT 1PL
plot(out1, type = "ICC")
plot(out1, type = "IIC", items=0) # Test Information Function

# 2pl
out2 <- ltm(I11~z1)
coef(out2)

#PLOT 2PL
plot(out2, type = "ICC")
plot(out2, type = "ICC", legend = TRUE, items=9, col = "green") # melihat aitem bagus ICC
plot(out2, type = "ICC", legend = TRUE, items=3, col = "red") # melihat aitem jelek ICC
plot(out2, type = "IIC", items=0) # Test Information Function

# 3pl
out3 <- tpm(I11)
coef(out3)

#PLOT 3PL
plot(out3, type = "ICC")
plot(out3, type = "ICC", legend = TRUE, items=8) # melihat aitem bagus & jelek(JIKA DIGUNAKAN)
plot(out3, type = "IIC", items=0) # Test Information Function

###########################
#Garis belah plot
abline(v = 4, col = "blue")
abline(h = 1, col = "red")
###########################

# Goodnes of Fit Model
anova(out1, out2)
anova(out1, out3)
anova(out2, out3)

###########################
#PLOT standard error (dicek dulu lebih cocok pakai model apa)
library("mirt")

#info plot Standard Errors
mirt.mtf = mirt(I11, 1, itemtype = "2PL")
plot(mirt.mtf, type = "infoSE")
plot(mirt.mtf, type = "trace") #Memunculkan hasil semua aitem

###########################
