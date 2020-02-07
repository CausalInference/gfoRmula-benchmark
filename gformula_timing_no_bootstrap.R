rm(list=ls())
library(data.table)
library(Hmisc)
library(R.utils)
library(gfoRmula)

setDTthreads(threads = 1)
tsswitch1 <- function(pool, histvars, time_name, t, id_name){
  current_ids <- unique(pool[month2 == t,get(id_name)])
  if (t  == 0 ){
    pool[,tseverhaart_inter := everhaart]
    pool[, lag1_tseverhaart_inter := as.integer(0)]
    
    lineholder <-0    
  }
  else {                  
    
    pool[month2 == t & get(id_name) %in% current_ids, lag1_tseverhaart_inter :=
           as.integer(pool[month2  == t - 1 & get(id_name) %in% current_ids ,tseverhaart_inter])]
    
    pool[month2 == t & get(id_name) %in% current_ids , tseverhaart_inter := as.integer(lag1_tseverhaart_inter + everhaart) ]
  }
}

coris <- fread(file="smaller5000.csv.gz")
myvars <- c("id","month2","event","cd4_v","rna_v","everhaart","aids",'SEX',
            'yrshiv','year_0','age_0','origin','mode','rna_0','cd4_0','visit_rna' ,'visit_cd4' )
coris <- coris[,..myvars]

# to get some 0s in rna_v , simulated data has no 0s
coris[cd4_v < 5, 'cd4_v' := 0 ]
coris[rna_v < 5, 'rna_v' := 0]

coris[month2 == 0 , c('visit_cd4','visit_rna'):= list(1,1)]

coris[,'lncd4_v':=log(cd4_v + 1)]
coris[,'lnrna_v':=log(rna_v+1)]

coris[,'lncd4_0':=log(cd4_0 + 1)]
coris[,'lnrna_0':=log(rna_0 + 1)]

coris[,'mymode' :=  as.factor(cut(mode,breaks=c(0,1,2,6,100)))]
coris[,year_0 := relevel(as.factor(cut(year_0, breaks=c(0,2007,2010,2200),labels=1:3) ),ref=3)]
coris[,'myorigin' := relevel(as.factor(origin),ref=4)]

cd4p <- "lncd4_v+Hmisc::rcspline.eval(lncd4_v,knots=c( 3.912, 4.605, 5.298 ,5.858 ,6.215))"
lagcd4p <- "lag1_lncd4_v+Hmisc::rcspline.eval(lag1_lncd4_v,knots=c( 3.912, 4.605, 5.298 ,5.858 ,6.215))"
rnap <- "lnrna_v+Hmisc::rcspline.eval(lnrna_v,knots=c( 6.215, 7.601, 9.211 ,10.597 ,11.513))"
lagrnap <- "lag1_lnrna_v+Hmisc::rcspline.eval(lag1_lnrna_v,knots=c( 6.215, 7.601, 9.211 ,10.597 ,11.513))" 

treatp <-"everhaart+tseverhaart_inter"
lagtreatp<-"lag1_everhaart+lag1_tseverhaart_inter"

basecovmod <- 'month2 + SEX + year_0 + yrshiv + mymode  + lnrna_0 +lncd4_0 + myorigin '

visit_rnam <- as.formula(paste("visit_rna~",paste(basecovmod,lagrnap, 'lag1_ts_visit_rna', lagcd4p,'lag1_ts_visit_cd4', 'lag1_aids',lagtreatp,sep = '+')))
rnam <-  as.formula(paste("lnrna_v~",paste(basecovmod,lagrnap, 'lag1_ts_visit_rna', 
                                           lagcd4p,'lag1_ts_visit_cd4' , 'lag1_aids',lagtreatp,sep = '+')))

visit_cd4m <- as.formula(paste("visit_cd4~",paste(paste(basecovmod,lagrnap, 
                                                        'ts_visit_rna', lagcd4p,'lag1_ts_visit_cd4',  'lag1_aids',lagtreatp,rnap, sep = '+'))))
cd4m <- as.formula(paste("lncd4_v~",paste(paste(basecovmod,
                                                lagrnap, 'ts_visit_rna', lagcd4p,'lag1_ts_visit_cd4', 'lag1_aids',lagtreatp,rnap, sep = '+'))))

aidsm <- as.formula(paste("aids~",paste(basecovmod,
                                        lagrnap, 'ts_visit_rna', lagcd4p,'ts_visit_cd4',lagtreatp, rnap, cd4p, sep ='+')))
treatm <- as.formula(paste('everhaart~',paste(basecovmod,
                                              lagrnap,'ts_visit_rna', lagcd4p,'ts_visit_cd4','lag1_aids',rnap,cd4p,'aids',sep = '+')))

ymodelm <- as.formula(paste('event~',paste(basecovmod,rnap ,
                                           'ts_visit_rna' ,cd4p ,'ts_visit_cd4', 'aids' , treatp ,sep="+" )))


outcome_name <- 'event'
covparams <- list(covmodels = c( visit_rnam, rnam, visit_cd4m, cd4m , aidsm, treatm))  
histvars <- c( 'visit_rna','lnrna_v' ,'visit_cd4','lncd4_v', 'aids' ,'everhaart')
extracovs <- c( "SEX" , 'year_0' , 'yrshiv','mymode','lnrna_0','lncd4_0' ,'myorigin') # for initializing counter

myvars <- c("id","month2","event","lncd4_v","visit_cd4", "lnrna_v","visit_rna",
            "aids","everhaart",'SEX','year_0','yrshiv','mymode','lnrna_0','lncd4_0' ,'myorigin')


coris <- coris[,..myvars]


## Call to the g-formula

mytime <- system.time(
  gform_test <- gformula(obs_data = coris, 
                         outcome_type = 'survival',
                         id = 'id',
                         time_points = 60, 
                         time_name = 'month2', 
                         basecovs = extracovs ,
                         covnames = c('visit_rna','lnrna_v','visit_cd4', 'lncd4_v','aids','everhaart'), 
                         covtypes = c('binary', 'normal', 'binary', 'normal','absorbing','absorbing'),
                         covparams = covparams,
                         visitprocess = list(c('visit_rna','lnrna_v',12),c('visit_cd4','lncd4_v',12) ) ,
                         ymodel = ymodelm, 
                         outcome_name = 'event',
                         intvars = c('everhaart','everhaart'), 
                         interventions = list(list(c(static, rep(0, 60))), list( c(static, rep(1, 60))) ),
                         int_descript = c('Never treat', 'always treated') ,
                         histories = c(lagged,tsswitch1), 
                         histvars = list(c('lnrna_v','lncd4_v','aids','everhaart'),c('everhaart')),
                         seed = 1234)
)
print(mytime)

print(gform_test)
