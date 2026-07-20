library(openxlsx)
library(bnlearn)
library(Rgraphviz)
library(readr)
library(stringr)
library(corrplot)
library(ranger)
library(ggplot2)
library(igraph)
library(dplyr)
library(ciTools)
library(pROC)

##--------------------------------comorbidity patterns----------------------------------
{
  load("D:/GBD_data/cause_id.RData")
  cause_id<-dataall
  load("D:/GBD_data/factor_id.RData")
  factor_id<-dataall
  factor_id<-factor_id[,-2]
  rm(dataall)
  gbddata<-merge(cause_id,factor_id,by=c("location_id"))
  rm(cause_id,factor_id)
  
  gbdselect<-gbddata[-which(gbddata$location_name=="Global"),c(1,2,which(str_detect(colnames(gbddata),"2021")))]
  
  indexy<-sort(c(493,494,976))
  indexx<-sort(c(86,117,116,124,118,111,112,113,102,108,105,367,107,341,125,100,99))
  
  f1<-function(i)
  {
    colnames(gbdselect)[which(str_detect(colnames(gbdselect),paste("_",i,"_",sep="")))]
  }
  varx<-sapply(indexx,f1)
  vary<-sapply(indexy,f1)
  rm(f1)
  
  var<-c(varx,vary)
  gbdselect<-gbdselect[,c("location_id","location_name",var)]
  
  {
    f1<-function(v)
    {
      gsub(pattern="(.*)_","",v)
    }
    namex<-as.vector(sapply(varx,f1))
    namey<-as.vector(sapply(vary,f1))
    namey<-c("IHD","Stroke","T2DM")
    rm(f1)
  }
  colnames(gbdselect)<-c("location_id","location_name",namex,namey)
  rm(indexx,indexy,varx,vary,var)
}
{
lieming<-c("Min","P25","P50","P75","Max")
summ_x<-matrix(nrow=length(namex)+length(namey),ncol=length(lieming),dimnames=list(c(namex,namey),lieming))
for(x in c(namex,namey))
{
  summ_x[x,]<-quantile(gbdselect[,x],c(0,0.25,0.5,0.75,1))
}
rm(x,lieming)
}
{
gbdselect$pattern<-"a"
p50<-c(median(gbdselect$IHD),median(gbdselect$Stroke),median(gbdselect$T2DM))
gbdselect$pattern<-
  ifelse(gbdselect[,"IHD"]>=p50[1]&gbdselect[,"Stroke"]>=p50[2]&gbdselect[,"T2DM"]>=p50[3],"IST",
         ifelse(gbdselect[,"IHD"]>=p50[1]&gbdselect[,"Stroke"]>=p50[2]&gbdselect[,"T2DM"]<p50[3],"IS",
                ifelse(gbdselect[,"IHD"]>=p50[1]&gbdselect[,"Stroke"]<p50[2]&gbdselect[,"T2DM"]>=p50[3],"IT",
                       ifelse(gbdselect[,"IHD"]<p50[1]&gbdselect[,"Stroke"]>=p50[2]&gbdselect[,"T2DM"]>=p50[3],"ST","Others"))))
gbdselect$pattern<-factor(gbdselect$pattern,levels=c("IST","IS","IT","ST","Others"))
rm(p50)
}

##---------------------------SHAP analysis----------------------------
{shapdata<-gbdselect[,c(namey[i],namex)]
  
  set.seed(1)
  fit <- ranger(
    x = shapdata[, -1], 
    y = shapdata[,1],     
    importance = "permutation"
  )
  
  set.seed(1)
  a <- kernelshap(fit, shapdata[-1], bg_X = shapdata)
  
  av <- shapviz(a)
}

##---------------------------CMM risk network-----------------------------
{
  b<-tiers2blacklist(list(namex,namey))
  w<-NULL
  
  set.seed(546)
  boot<-boot.strength(data=gbdselect,algorithm="hc",R=1000,
                      algorithm.args=list(blacklist=b,whitelist=w,score="bic-g"))
  
  boot_raw<-boot
  
  p<-cor.mtest(gbdselect,conf.level=.95)$p
  for(i in 1:nrow(boot))
  {
    if(p[boot$from[i],boot$to[i]]>0.05)
      boot$strength[i]<-0
  }
  rm(i,p)
  
  arc<-as.data.frame(boot[which(boot$strength>0.7&boot$direction>0.4),])
  net <- empty.graph(nodes = c(namex,namey))
  arcs(net) <- as.matrix(arc[, c("from", "to")])
  
  arc_direct<-arc[which(arc$direction>=0.6),]
  net_direct<- empty.graph(nodes = c(namex,namey))
  arcs(net_direct) <- as.matrix(arc_direct[, c("from", "to")])
  
  graphviz.compare(net,net_direct)
  rm(b,w)
  arc_indirect<-arc[which(arc$direction<0.6),]
  
  Rnet<-list(boot=boot,arc=arc,arc_direct=arc_direct,arc_indirect=arc_indirect,net=net,
            net_direct=net_direct)
}
{
  arc<-Rnet$arc
  col <- colorRampPalette(  c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FDDBC7",
                              "#FFFFFF","#D1E5F0", "#92C5DE","#4393C3", "#2166AC", "#053061"))(100)
  col<-rev(col)
  
  pearson<-cor(gbdselect)
  res<-cor.mtest(gbdselect,conf.level=.95)
  
  pearson_adj<-pearson
  pearson_adj[]<-NA
  for(i in 1:nrow(arc))
  {
    pearson_adj[arc$from[i],arc$to[i]]<-pearson[arc$from[i],arc$to[i]]
    pearson_adj[arc$to[i],arc$from[i]]<-pearson[arc$to[i],arc$from[i]]
  }
  
  p_adj<-res$p
  p_adj[which(is.na(pearson_adj))]<-1
  p_adj[-which(is.na(pearson_adj))]<-0
  
  pdf("D:/GBD_data/cor_lower.pdf",width=9, height=9,bg="white",pointsize=8) 
  
  corrplot(pearson, method = "color",tl.col = "black", tl.srt = 25,tl.cex =0.9, p.mat = p_adj,
           sig.level = .05,insig = "blank",addgrid.col="white",addCoef.col = "black",col=col,type="lower")
  dev.off()  
} 

##-------------------------------Comparison of the prediction accuracy for Adj-BN and classical BN models----------------------------------
x_ygrade<-matrix(nrow=length(namex),ncol=length(namey),dimnames=list(namex,namey))
x_ygrade["Ambient particulate matter pollution",]<-c("far","near","far")
x_ygrade["Smoking",]<-c("mid","near","mid")
x_ygrade["Secondhand smoke",]<-c("far","far","far")
x_ygrade["High alcohol use",]<-c("mid","mid","mid")
x_ygrade["High fasting plasma glucose",]<-c("mid","far","near")
x_ygrade["High systolic blood pressure",]<-c("mid","near",NA)
x_ygrade["High body-mass index",]<-c("mid","far","mid")
x_ygrade["Diet low in fruits",]<-c("far","far","far")
x_ygrade["Diet low in vegetables",]<-c("mid","mid","far")
x_ygrade["Diet low in whole grains",]<-c("near","mid","far")
x_ygrade["Diet high in red meat",]<-c("far","far","far")
x_ygrade["Diet high in processed meat",]<-c("mid","mid","near")
x_ygrade["Diet high in sugar-sweetened beverages",]<-c("far","far","mid")
x_ygrade["Diet high in sodium",]<-c("mid","near",NA)
x_ygrade["Low physical activity",]<-c("mid","mid","mid")
x_ygrade["Kidney dysfunction",]<-c("near","mid",NA)
x_ygrade["High LDL cholesterol",]<-c("mid","mid","near")

{
pre_list<-vector("list",3)
names(pre_list)<-c("far","farmid","farmidnear")
netnew<-bn.fit(Rnet$net_direct,gbdselect)

for(s in c("far","farmid","farmidnear"))
{
  pre_list[[s]]<-list(pre_before=gbdselect[,namey],pre_after=gbdselect[,namey])
  for(y in 1:3)
  {
    newdata<-gbdselect
    var<-rownames(x_ygrade)[str_detect(s,x_ygrade[,y])]
    newdata[,setdiff(colnames(newdata),var)]<-NaN
    pre_list[[s]]$pre_after[,y]<-adjbn(newdata)[,namey[y]]
    
    set.seed(67)
    pre_list[[s]]$pre_before[,y]<-impute(netnew,newdata,method="bayes-lw")[,namey[y]]
  }
}
rm(s,y,var,newdata)
}
true_01<-gbdselect[,namey]
for(c in 1:3)
  true_01[,c]<-ifelse(gbdselect[,namey][,c]>quantile(gbdselect[,namey][,c],0.75),1,0)
rm(c)

AUC_list<-vector("list",3)
names(AUC_list)<-c("far","farmid","farmidnear")
for(s in c("far","farmid","farmidnear"))
{
  AUC_list[[s]]<-data.frame(type=s,outcome=namey,
                            before_ci=NA,after_ci=NA,diff_ci=NA,P=NA,
                            before=NA,l_before=NA,u_before=NA,
                            after=NA,l_after=NA,u_after=NA,
                            diff=NA,l_diff=NA,u_diff=NA,
                            yuzhi_before=NA,se_before=NA,sp_before=NA,
                            yuzhi_after=NA,se_after=NA,sp_after=NA)
  for(y in 1:3)
  {
    roc_before <- roc(true_01[,y], pre_list[[s]][[1]][,y], quiet = TRUE, ci = TRUE)  
    roc_after <- roc(true_01[,y], pre_list[[s]][[2]][,y], quiet = TRUE, ci = TRUE)  

    set.seed(30)
    test_result <- roc.test(
      roc_after,
      roc_before, 
      method = "delong", 
      paired = TRUE,         
      boot.n = 2000,         
      conf.level = 0.95,    
      reuse.auc = TRUE       
    )
    
    AUC_list[[s]][y,c("yuzhi_before","se_before","sp_before")]<- coords(
      roc_before, 
      "best", 
      best.method = "closest.topleft", 
      ret = c("threshold", "sensitivity", "specificity")
    )
    AUC_list[[s]][y,c("yuzhi_after","se_after","sp_after")]<- coords(
      roc_after, 
      "best", 
      best.method = "closest.topleft",  
      ret = c("threshold", "sensitivity", "specificity")
    )
    
    AUC_list[[s]][y,"before"]<-auc(roc_before)
    AUC_list[[s]][y,"l_before"]<-roc_before$ci[1]
    AUC_list[[s]][y,"u_before"]<-roc_before$ci[3]
    AUC_list[[s]][y,"after"]<-auc(roc_after)
    AUC_list[[s]][y,"l_after"]<-roc_after$ci[1]
    AUC_list[[s]][y,"u_after"]<-roc_after$ci[3]
    AUC_list[[s]][y,"diff"]<-test_result$estimate[1]-test_result$estimate[2]
    AUC_list[[s]][y,"l_diff"]<-test_result$conf.int[1]
    AUC_list[[s]][y,"u_diff"]<-test_result$conf.int[2]
    AUC_list[[s]][y,"P"]<-test_result$p.value
    
    AUC_list[[s]][y,"before_ci"]<-sprintf("%.3f (%.3f-%.3f)", 
                                       AUC_list[[s]][y,"before"], AUC_list[[s]][y,"l_before"], AUC_list[[s]][y,"u_before"])
    AUC_list[[s]][y,"after_ci"]<-sprintf("%.3f (%.3f-%.3f)", 
                                       AUC_list[[s]][y,"after"], AUC_list[[s]][y,"l_after"], AUC_list[[s]][y,"u_after"])
    AUC_list[[s]][y,"diff_ci"]<-sprintf("%.3f (%.3f-%.3f)", 
                                       AUC_list[[s]][y,"diff"], AUC_list[[s]][y,"l_diff"], AUC_list[[s]][y,"u_diff"])
  }
}
rm(s,y,roc_before,roc_after,true_01,test_result)
AUCs <- do.call(rbind, AUC_list)

##------------------------------Trends in the incidence rates of CMDs from 2010 to 2040 in global-------------------------------
{
  namey<-c("Ischemic heart disease","Stroke","Diabetes mellitus type 2")
  global<-as.data.frame(matrix(nrow=length(1990:2021),ncol=length(c(namex,namey)),dimnames=list(1990:2021,c(namex,namey))))
  {
    AAPC<-data.frame(var=c(namex,namey),AAPC=NA)
    pre<-matrix(nrow=length(2022:2040),ncol=length(c(namex,namey)),dimnames=list(2022:2040,c(namex,namey)))
    pre_l<-pre
    pre_u<-pre
    
    for(v in 1:length(c(namex,namey)))
    {
      global[,v]<-as.numeric(gbddata[which(gbddata$location_name=="Global"),str_detect(colnames(gbddata),c(namex,namey)[v])])
      
      glmdata<-data.frame(t=1990:2021,y=1990:2021)
      glmdata$y<-global[,v]
      glmdata<-glmdata[which(glmdata$t>=2010),]
      
      gmodel<-glm(y~t,data=glmdata,family = gaussian(link = "log"))
      AAPC[v,2]<-(exp(gmodel$coefficients[2])-1)*100
      newdata=data.frame(t=2022:2040)
      
      
      pre[,v]<-predict(gmodel, newdata, type = "response")
      ci<-add_pi(newdata, gmodel, alpha = 0.05, names = c("lwr", "upr"))
      pre_l[,v]<-ci[,"lwr"]
      pre_u[,v]<-ci[,"upr"]
      
    }
    rm(v,gmodel,glmdata,ci,newdata)
  }
}

##---------------------------Trends in the incidence rates of CMDs in five comorbidity pattern regions-----------------------------
pattern<-resultall[,c("location_id","pattern")]
{
  gbddata<-gbddata[which(gbddata$location_name!="Global"),]
  
  indexy<-sort(c(493,494,976))
  indexx<-sort(c(86,117,116,124,118,111,112,113,102,108,105,367,107,341,125,100,99))
  
  index<-as.character(c(indexx,indexy))
  
  a<-colnames(gbddata[,str_detect(colnames(gbddata),"2012")&!str_detect(colnames(gbddata),"pre_")])
  
  f1<-function(i)
  {
    a[which(str_detect(a,paste("_",i,"_",sep="")))]
  }
  varname<-sapply(index,f1)
  rm(f1,a)
  
  f2<-function(i)
  {
    gsub(pattern="(.*)_","",i)
  }
  varname<-sapply(varname,f2)
  rm(f2)
  
  AAPC_pattern<-as.data.frame(matrix(nrow=5,ncol=length(index),dimnames=list(levels(pattern$pattern),
                                                                             c(varname))))
  glmdata<-data.frame(t=1990:2021,y=1990:2021)
  
  pre_pattern<-list()
  for(p in levels(pattern$pattern))
    {
      pre<-matrix(nrow=length(2022:2040),ncol=length(c(namex,namey)),dimnames=list(2022:2040,c(namex,namey)))
      pre_l<-pre
      pre_u<-pre
      past<-matrix(nrow=length(2010:2021),ncol=length(c(namex,namey)),dimnames=list(2010:2021,c(namex,namey)))
      
      for(v in 1:length(varname))
    {
      id<-pattern$location_id[which(pattern$pattern==p)]
      glmdata$y<-apply(gbddata[is.element(gbddata$location_id,id),colnames(gbddata[,str_detect(colnames(gbddata),colnames(AAPC_pattern)[v])])],MARGIN = 2,mean)
      glmdata2010<-glmdata[which(glmdata$t>=2010),]
      past[,v]<-glmdata2010$y
      gmodel<-glm(y~t,data=glmdata2010,family = gaussian(link = "log"))
      AAPC_pattern[p,v]<-(exp(gmodel$coefficients[2])-1)*100

      newdata=data.frame(t=2022:2040)
      
      pre[,v]<-predict(gmodel, newdata, type = "response")
      ci<-add_pi(newdata, gmodel, alpha = 0.05, names = c("lwr", "upr"))
      pre_l[,v]<-ci[,"lwr"]
      pre_u[,v]<-ci[,"upr"]
      }
      pre_pattern[[p]]<-rbind(past,pre,pre_l,pre_u)
      rownames(pre_pattern[[p]])<-c(2010:2040,paste(2022:2040,"lower",sep="_"),paste(2022:2040,"upper",sep="_"))
    }
   
  rm(glmdata,p,v,gmodel,index,indexx,indexy,varname,glmdata2010,ci,id,pre,pre_l,pre_u,newdata,past)
}

##------------------------------Spatiotemporal evolution of high-risk factors from 2021 to 2040-------------------------------
{
  gbdselect<-cbind(gbdselect,gbdselect[,namex])
  colnames(gbdselect)<-c("location_id","location_name",namex,namey,paste(namex,"P50",sep="_"))
  for (c in 1: length(namex))
  {
    gbdselect[,paste(namex[c],"P50",sep="_")]<-ifelse(gbdselect[,namex[c]]>quantile(gbdselect[,namex[c]],0.5),1,0)
  }
  rm(c)
  gbdselect[,"h_number"]<-apply(gbdselect[,paste(namex,"P50",sep="_")],MARGIN=1,FUN=sum)
  
  gbdselect <- gbdselect %>%
    mutate(h_grade = case_when(
      h_number %in% 1:5 ~ 1,
      h_number %in% 6:8 ~ 2,
      h_number %in% 9:11 ~ 3,
      h_number %in% 12:16 ~ 4
    ))
  risk_2021<-merge(arcgis,gbdselect,by="location_id",all.y=TRUE)
}
{
gbdselect<-pres[["2040"]]
colnames(gbdselect)<-c("location_name","location_id",namex,namey)

gbdselect<-cbind(gbdselect,gbdselect[,namex])
colnames(gbdselect)<-c("location_name","location_id",namex,namey,paste(namex,"P50",sep="_"))
for (c in 1: length(namex))
{
  gbdselect[,paste(namex[c],"P50",sep="_")]<-ifelse(gbdselect[,namex[c]]>quantile(risk_2021[,namex[c]],0.5),1,0)
}
rm(c)
gbdselect[,"h_number"]<-apply(gbdselect[,paste(namex,"P50",sep="_")],MARGIN=1,FUN=sum)

gbdselect <- gbdselect %>%
  mutate(h_grade = case_when(
    h_number %in% 1:5 ~ 1,
    h_number %in% 6:8 ~ 2,
    h_number %in% 9:11 ~ 3,
    h_number %in% 12:16 ~ 4
  ))
risk_2040<-merge(arcgis,gbdselect,by="location_id",all.y =TRUE)
}

risk<-merge(risk_2021[,c("FID","location_id","h_number")],risk_2040[c("location_id","h_number")],by="location_id")
colnames(risk)<-c("location_id","FID","h_number_2021","h_number_2040")
