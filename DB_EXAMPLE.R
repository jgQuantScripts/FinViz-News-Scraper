require("quantmod");require("PerformanceAnaltics")

TICS = c("TSLA","AAPL","TLT","FB","HYG","VCSH","EEM")
SEC <- c("SPY",TICS)

e <- new.env()
getSymbols(SEC,from="1990-01-01",env=e)
tmp <- do.call(merge,lapply(eapply(e,to.monthly),Ad))["199412::"]
colnames(tmp) <- names(lapply(eapply(e,to.monthly),Ad))

RETS <- round(ROC(tmp,type="discrete"),4)
charts.PerformanceSummary(RETS,colorset=rich10equal,cex.legend=0.50)


getSymbols.FRED("DGS10",env = .GlobalEnv)
DGS10 <- Cl(to.monthly(DGS10["1994::"]))/100/12
DGS10 <- DGS10["199412::"]
colnames(DGS10) <- "DGS10"
DGS10 <- DGS10[index(RETS)]
RETS[is.na(RETS)]<-0
#save.image("IDKidk.RData")
#load("IDKidk.RData")
# ************************************************************************************************************************************************************
#                                                       DUAL BETA 
# ************************************************************************************************************************************************************
DUALBETA = function(x)
{
  EOD <- RETS[,x]
  BM = RETS[,"SPY"]
  
  TMP <- merge.xts(EOD,BM,DGS10)["1995::"]
  
  stock <- TMP[,1]
  BM <- TMP[,2]
  DGS10 <- TMP[,3]
  
  RETS1 <- merge.xts(stock,BM,DGS10)
  # ***********************************************
  # GET RANKs
  # ***********************************************
  D <- ifelse(RETS1$SPY >= 0, 1, 0) # Dummy Variable
  BETAup <- ifelse(RETS1$SPY  >= 0, (RETS1$SPY-RETS1$DGS10)*D, 0)
  BETAdn <- ifelse(RETS1$SPY < 0, (RETS1$SPY-RETS1$DGS10)*(1-D), 0)
  IDX <- paste0("",format(start(((RETS1[!cumsum(RETS1[,1])== 0]))), format="%Y%m"),"/",
                format(last(index(((RETS1[!(RETS1[,1])== 0]))), keep=TRUE), format="%Y%m"),"")
  
  DUALBETA <- try(lm((RETS1[,1][IDX]-RETS1$DGS10[IDX]) ~ BETAup[IDX] + BETAdn[IDX] + D[IDX]))
  if (!inherits(DUALBETA, 'try-error')){
    alphabeta <- as.data.frame(cbind(coef(DUALBETA)[1],coef(DUALBETA)[2],
                                     coef(DUALBETA)[3],coef(DUALBETA)[4],
                                     anova(DUALBETA)$'Pr(>F)'[1], anova(DUALBETA)$'Pr(>F)'[2]), 
                               row.names = names(RETS1)[1]) 
  }else{
    alphabeta <- as.data.frame(cbind(0,0,0,0,0,0),row.names = names(RETS1)[1])
  }
  colnames(alphabeta) <- c("ALPHAdn","BETAup","BETAdn","ALPHAup", "UPpval","DNpval")
  ALPHABETA<- round(alphabeta,4)
  rm(alphabeta,DUALBETA,BETAdn,BETAup,D,RETS1,RANGE)
  ALPHABETA <- cbind(x,ALPHABETA)
  ALPHABETA
}

# ************************************************************************************************************************************************************
#                                                       END
# *********************************************************************************************************************************************************
ALL <- lapply(as.list(TICS), function(x) DUALBETA(x=x))

ALL <- do.call(rbind,ALL)





























