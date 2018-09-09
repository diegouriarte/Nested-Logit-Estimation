cd "C:\Users\diego\Dropbox\projects\maestria\Ciclo 4\Empirical Industrial Organization\assignment"
import delimited "analysis_data"

***************************************** Preparing the data base********************************************

*Only use really choices about health plans
keep if (choice==1)

/*Collapse requieres count the total individuals by market and plan. Additionally, 
the others variables are equals by plan. Within a market, collapse is to the mean.  */

collapse (count) individual (mean) price xsi x_constant x_coinsurance x_deductible x_oopmax, by (plan market) 
sort market plan
br

* Generating a variable called 'm' that represent the total of individuals in each market  
egen double m=total(individual), by(market)

* Creating a variable called good_share represent the share of plan 'j' in each market
gen double market_shares=individual/m
drop m individual

*Variable s0 extract the value of variable good_share, by outside good.
g double s0=market_shares if (plan==0)

/* To complete variable s0 with the value previously obtained in each market (new variable is called 
outside_good_share), and drop s0. Then, delete values of variables if plan is equal to zero  */
egen double outside_good_share=mean(s0), by(market)
drop s0
drop if(plan==0)


***************************************** Part A of question 2 ************************************************
set more off

forv z=1/5 {
su market_shares outside_good_share if (plan==`z')

}

su market_shares outside_good_share

/*  Report of results:

**Plan 1
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
market_sha~s |       100    .1856738     .092138   .0085653   .4682779
outside_go~e |       100    .4700687    .0932901   .2858672   .7081448


**Plan 2
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
market_sha~s |       100    .1831786    .1093933   .0092272   .4912099
outside_go~e |       100    .4700687    .0932901   .2858672   .7081448


**Plan 3
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
market_sha~s |        66    .1536426    .1023411   .0023585   .4379015
outside_go~e |        66    .4311289    .0774895   .2858672   .5986622


**Plan 4
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
market_sha~s |        39    .1035197    .0705777   .0091185   .2557173
outside_go~e |        39    .3990424    .0629378   .2858672   .5496183


**Plan 5
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
market_sha~s |        20    .0965109    .0895796   .0091324   .3176179
outside_go~e |        20    .3708801    .0550381   .2858672   .5191676

**Total of plans:
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
market_sha~s |       325    .1630558     .102017   .0023585   .4912099
outside_go~e |       325    .4475338    .0904401   .2858672   .7081448
*/


forv z=1/5 {
gen double share_`z'=market_shares if (plan==`z')
replace share_`z'=0 if share_`z'==.
}

set more off
pwcorr price x_coinsurance x_deductible x_oopmax share_1 share_2 share_3 share_4 share_5, sig

/*
*Reporte:

             |    price x_coin~e x_dedu~e x_oopmax  share_1  share_2  share_3  share_4  share_5
-------------+---------------------------------------------------------------------------------
       price |   1.0000 
             |
             |
x_coinsura~e |  -0.1895   1.0000 
             |   0.0006
             |
x_deductible |  -0.0879  -0.0121   1.0000 
             |   0.1137   0.8274
             |
    x_oopmax |  -0.0320   0.0590  -0.0209   1.0000 
             |   0.5655   0.2887   0.7080
             |
     share_1 |   0.3891  -0.0434   0.0082   0.0744   1.0000 
             |   0.0000   0.4359   0.8832   0.1809
             |
     share_2 |   0.4636  -0.1090  -0.0517   0.0056  -0.3110   1.0000 
             |   0.0000   0.0497   0.3532   0.9206   0.0000
             |
     share_3 |   0.1483  -0.0891  -0.0557  -0.0952  -0.2326  -0.2201   1.0000 
             |   0.0074   0.1089   0.3170   0.0868   0.0000   0.0001
             |
     share_4 |  -0.0863   0.0635   0.0081   0.0673  -0.1720  -0.1628  -0.1217  1.0000
             |   0.1206   0.2536   0.8838   0.2263   0.0019   0.0033   0.0282
             |
     share_5 |  -0.0139   0.0124  -0.0351  -0.0250  -0.1073  -0.1015  -0.0759  -0.0562  1.0000
             |   0.8033   0.8235   0.5288   0.6530   0.0533   0.0675   0.1721   0.3129

*/
***************************************** Part B of question 2 ************************************************
*Generating instruments:

egen double inst_coinsurance=total(x_coinsurance), by(market)
egen double inst_deductible=total(x_deductible), by(market)
egen double inst_oopmax=total(x_oopmax), by(market)

summarize inst_coinsurance inst_deductible inst_oopmax

***************************************** Part C of question 2 ************************************************
gen double ln_sj_s0=ln(market_shares/outside_good_share)
gen double ln_sj_g=market_shares/(1-outside_good_share)

reg ln_sj_s0 price x_coinsurance x_deductible x_oopmax ln_sj_g

/*

      Source |       SS       df       MS              Number of obs =     325
-------------+------------------------------           F(  5,   319) =  123.76
       Model |  154.397825     5  30.8795651           Prob > F      =  0.0000
    Residual |  79.5962298   319  .249517962           R-squared     =  0.6598
-------------+------------------------------           Adj R-squared =  0.6545
       Total |  233.994055   324  .722203874           Root MSE      =  .49952

-------------------------------------------------------------------------------
     ln_sj_s0 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
        price |  -1.345578   .3124969    -4.31   0.000    -1.960393   -.7307624
x_coinsurance |  -.2026159   .2069634    -0.98   0.328    -.6098016    .2045697
 x_deductible |  -.2185973   .1971488    -1.11   0.268    -.6064736    .1692789
     x_oopmax |   .3699687   .1937491     1.91   0.057    -.0112188    .7511562
      ln_sj_g |   5.301552   .4611382    11.50   0.000     4.394296    6.208809
        _cons |  -1.367807   .2538013    -5.39   0.000    -1.867143   -.8684711
-------------------------------------------------------------------------------


*/



***************************************** Part D of question 2 ************************************************

ivregress 2sls ln_sj_s0 price x_coinsurance x_deductible x_oopmax ///
(ln_sj_g = inst_coinsurance inst_deductible inst_oopmax)



