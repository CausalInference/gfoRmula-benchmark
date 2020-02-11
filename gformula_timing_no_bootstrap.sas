%include '/home/rwlogan/Rgformula/gformula3.sas';


proc import datafile='/home/rwlogan/Rgformula/smaller5000.csv' 
 out=test 
 dbms=csv 
 replace;
 getnames=yes ;
 guessingrows= 1000;
 run;


options cpucount=1 nonotes nomprint ;

data mytmp ;
set test /*(keep = id month2 event cd4_v last_valid_cd4 visit_cd4 everhaart 
                rename = (last_valid_cd4 = ts_cd4 )) */; 
 
if cd4_v < 5 then cd4_v = 0 ;
if rna_v < 5 then rna_v = 0 ;


lncd4_v = log(cd4_v + 1);
lncd4_v_l1 = log(lag(cd4_v + 1));
lncd4_v_l2 = log(lag2(cd4_v+1));

lnrna_v = log(rna_v + 1) ;
lnrna_v_l1 = lag(lnrna_v);
lnrna_v_l2 = lag2(lnrna_v);


if month2 = 0 then do;
   lncd4_v_l1 = lncd4_v ;
   lncd4_v_l2 = lncd4_v ;

   lnrna_v_l1 = lnrna_v ;
   lnrna_v_l2 = lnrna_v ;
end;
if month2 = 1 then do;
   lncd4_v_l2 = lncd4_v_l1 ;
   lnrna_v_l2 = lnrna_v_l1;
end;


everhaart_l1 = lag(everhaart);
if month2 = 0 then everhaart_l1 = 0 ;
 
aids_l1 = lag(aids);
if month2 = 0 then aids_l1 = 0 ;

if month2 = 0 then do ;
   visit_cd4 = 1 ;
   ts_cd4 = 0 ;
 
   visit_rna = 1 ;
   ts_rna = 0 ;
end;

/* year_01 year_02  mymode1 mymode2 mymode3 rna_01 rna_02 rna_03 rna_04 cd4_0  */
/* with smaller data set mode only takes on values in (1,3,4). In the R version the reference level was taken to be
   mode = 1 */

year_01 =  ( year_0 <= 2007) ;
year_02 =  ( 2007 < year_0 <= 2010 );
mymode1 = (mode in (3,4)) ;
 

lnrna_0 = log(rna_0 + 1);
lncd4_0 = log(cd4_0 + 1);

myorigin1 = (origin = 1) ;
myorigin2 = (origin = 2) ;
myorigin3 = (origin = 3) ;

run;



%let interv1 =
intno=1,           intlabel='never treat', nintvar=1,
intvar1=everhaart, inttype1=1, intvalue1 = 0 ,
        inttimes1= 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
          31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 ;


%let interv2 =
intno=2,           intlabel='always treat', nintvar=1,
intvar1=everhaart, inttype1=1, intvalue1 = 1 ,
        inttimes1= 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
          31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 ;



%macro myupdate_myts_l1 ;
   /* this is called from genpred */
/* lagtype = 1 call from genpred at end of loop over time variable */

 %if &lagtype = 1 %then %do;
  * ;
  * this should replace the updating done at the end of time loop ;
  * myts_l1 is only used in simulating myvisit,  ;
   myts_l1 = myts ;
  
 %end;
 



%mend;
%macro myvisit ;
  myvisit = 1 ; 
  myts = 0 ;
%mend;



%macro mycarry ;
  * no visit carry forward previous value and increment counter ;
  lncd4_v = lncd4_v_l1 ;
  myts = myts + 1 ;
%mend ;
**options mprintnest notes fullstimer ;
%gformula(data = mytmp,
          id = id ,
          time=month2 ,
          timeptype = conbin ,
          timepoints = 60,
          outc = event ,
          outctype = binsurv,
numint=2,
nsamples=0,
fixedcov=   sex   lnrna_0 lncd4_0 yrshiv  mymode1 year_01 year_02 myorigin1 myorigin2 myorigin3 ,
print_stats = 0,
simuldata=mysim ,
usespline=0 ,
betadata = mybetas,
refint = 1 ,
/***/

ncov=4,
cov1 = lnrna_v , cov1otype = 3 , cov1ptype = lag1spl , cov1knots = 6.215 7.601 9.211 10.597 11.513  ,
cov1randomvisitp = visit_rna , cov1visitpmaxgap = 12 , cov1visitpcount = ts_rna ,
cov2 = lncd4_v , cov2otype = 3 , cov2ptype = lag1spl , cov2knots = 3.912 4.605 5.298 5.858 6.215 ,
cov2randomvisitp = visit_cd4 , cov2visitpmaxgap = 12 , cov2visitpcount = ts_cd4,
cov3 = aids , cov3otype = 2 , cov3ptype = lag1bin,
cov4 = everhaart , cov4otype = 2 , cov4ptype = tsswitch1


);


options notes fullstimer ;
proc print data = survdata_all ;
var int2 risk60 surv60 ;
run;



