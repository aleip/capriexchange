
********************************************************************************
$ontext

   CAPRI project

   GAMS file : health.gms

   @purpose  :  To estimate final health outcomes based on Springmann et al. (2018).

      See Springmann, M., Wiebe, K., Mason-D'Croz, D., Sulser, T. B., Rayner, M., & Scarborough, P. (2018).
      Health and nutritional aspects of sustainable diet strategies and their association with environmental impacts: a global modelling analysis with country-level detail
      The Lancet Planetary Health Volume 2, Issue 10, October 2018, Pages e451-e461
      https://www.sciencedirect.com/science/article/pii/S2542519618302067

   @author   :
   @date     :
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy : data_prep.gms (TBC)

$offtext
********************************************************************************


set stats /mean, low, high, std/;
set cause  "causes of death"  /CHD, stroke, cancer, T2DM, resp_dis/  ;
set age "age groups"  /20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54,55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85+/;
set age_prm age groups for which deaths is premature (age 30-70 according to WHO) /30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-64, 65-69/;
set health_ind  "indicators for health analysis"  /deaths_avd, deaths_avd_prm, YLL_avd, deaths, deaths_prm, YLL/   ;
set life_exp  /life_exp/

parameter p_health(*,*,*,*,*,*)health analysis of diets;
parameter p_dr(age,cause,*,*)  mortality rate (deaths per pop);
parameter p_pop(age,*,*)       population statistics (thousands);
parameter p_life_exp(age,*)    GBD 2010 standard abridged life table;
parameter p_diet_eatcategories(*,*,*,*)
;

TABLE p_life_exp(age,*)
age        life_exp
0-1        86.02
100+       2.23
100-104     2.23
10-14      76.27
105+       1.63
1-4        85.21
15-19      71.29
20-24      66.35
25-29      61.4
30-34      54.46
35-39      51.53
40-44      46.64
45-49      41.8
50-54      37.05
55-59      32.38
5-9        81.25
60-64      27.81
65-69      23.29
70-74      18.93
75-79      14.8
80-84      10.99
85+        7.64
85-89      7.64
90-94      5.05
95-99      3.31
;


$gdxin dr.gdx
$load p_dr, p_pop
$gdxin

TBC: p_r_data $r_data("all",r) - need to change the gdx file

*-------------------------------------------------------------------------------

*        select regions with data for health analysis:


*p_r_data("cons",r)$sum(fg, cons_scn_data("g/d_w",fg,r,"2010")) = yes;
*r_data("mort",r)$dr("35-39","CHD",r,"2010") = yes;
*r_data("bmi",r)$sum(weight, weight_scn("BMK",weight,r,"2010")) = yes;

*r_data("all",r)$r_data("mort",r) = yes;
*r_data("all",r)$(not r_data("cons",r)) = no;
*r_data("all",r)$(not r_data("bmi",r)) = no;

*-------------------------------------------------------------------------------


*        - avoided deaths:
p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","mean")$r_data("all",r)
         = sum(age, p_PAF(foodagg,cause,rall,"%SIMY%","mean",age)
         * p_dr(age,cause,rall,"%SIMY%") * p_pop(age,rall,"%SIMY%") );

p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","std")$r_data("all",r)
         = sum(age, p_PAF(foodagg,cause,rall,"%SIMY%","std",age)
         *  p_dr(age,cause,rall,"%SIMY%") * p_pop(age,rall,"%SIMY%") );

p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","low")
         = p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","mean")
         - p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","std");

p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","high")
         = p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","mean")
         + p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","std");

*p_health("deaths_avd",foodagg,"all-c",rall,"%SIMY%",stats)
*         = sum(cause, health_scn("deaths_avd", foodagg,cause,rall,"%SIMY%",stats));

*p_health("deaths_avd",diet_scn,riskf_p,cause_p,"all-r",yrs_scn,stats)
*         = sum(r, p_health("deaths_avd",diet_scn,riskf_p,cause_p,r,yrs_scn,stats));


*        - avoided premature deaths:
p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","mean")$r_data("all",r)
         = sum(age_prm, p_PAF(foodagg,cause,rall,"%SIMY%","mean",age_prm)
         * p_dr(age_prm,cause,rall,"%SIMY%") * p_pop(age_prm,rall,"%SIMY%") );

p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","std")$r_data("all",r)
         = sum(age_prm, p_PAF(foodagg,cause,rall,"%SIMY%","std",age_prm)
         * p_dr(age_prm,cause,rall,"%SIMY%") * p_pop(age_prm,rall,"%SIMY%") );

p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","low")
         = p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","mean")
         - p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","std");

p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","high")
         = p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","mean")
         + p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","std");

*p_health("deaths_avd_prm",foodagg,"all-c",rall,"%SIMY%",stats)
*         = sum(cause, p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%",stats));

*p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%",stats)
*         = sum(rall, p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%",stats));


*        - Years of Life Lost (YLL):
p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","mean")$r_data("all",r)
         = sum(age, p_PAF(foodagg,cause,rall,"%SIMY%","mean",age)
         * p_dr(age,cause,rall,"%SIMY%") * p_pop(age,rall,"%SIMY%")
         * p_life_exp(age,"life_exp") );

p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","std")$r_data("all",r)
         = sum(age, p_PAF(foodagg,cause,rall,"%SIMY%","std",age)
         * p_dr(age,cause,rall,"%SIMY%") * p_pop(age,rall,"%SIMY%")
         * p_life_exp(age,"life_exp") );

p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","low")
         = health_scn("YLL_avd",foodagg,cause,rall,"%SIMY%","mean")
         - health_scn("YLL_avd",foodagg,cause,rall,"%SIMY%","std");

p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","high")
         = p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","mean")
         + p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","std");

*p_health("YLL_avd",foodagg,"all-c",r,yrs_scn,stats)
*         = sum(cause, p_health("YLL_avd",foodagg,cause,r,yrs_scn,stats));

*p_health("YLL_avd",foodagg,cause,"all-r",yrs_scn,stats)
*         = sum(r, p_health("YLL_avd",diet_scn,riskf_p,cause_p,r,yrs_scn,stats));


*        - allocate colorectal cancer to all cancers:
*health_scn(health_itm,diet_scn_p,"redmeat","cancer",rgs,yrs_scn,stats)
*         = health_scn(health_itm,diet_scn_p,"redmeat","Colon and rectum cancers",rgs,yrs_scn,stats);

*health_scn(health_itm,diet_scn_p,"all-rf","cancer",rgs,yrs_scn,stats)
*         = health_scn(health_itm,diet_scn_p,"all-rf","cancer",rgs,yrs_scn,stats)
*         + health_scn(health_itm,diet_scn_p,"all-rf","Colon and rectum cancers",rgs,yrs_scn,stats);

*health_scn(health_itm,diet_scn_p,riskf_p,"Colon and rectum cancers",rgs,yrs_scn,stats) = 0;


*-------------------------------------------------------------------------------
*        Analysis:
*-------------------------------------------------------------------------------

*        - compare to all deaths and premature deaths:

*health_scn("deaths","BMK","all-rf",cause,r,yrs_scn,"mean")$r_data("all",r)
*         = sum(age, dr(age,cause,r,yrs_scn) * pop(age,r,yrs_scn) );

*health_scn("deaths","BMK","all-rf","all-c",r,yrs_scn,"mean")
*         = sum(cause, health_scn("deaths","BMK","all-rf",cause,r,yrs_scn,"mean"));

*health_scn("deaths","BMK","all-rf",cause_p,"all-r",yrs_scn,stats)
*         = sum(r, health_scn("deaths","BMK","all-rf",cause_p,r,yrs_scn,"mean"));


*health_scn("deaths_prm","BMK","all-rf",cause,r,yrs_scn,"mean")$r_data("all",r)
*         = sum(age_prm, dr(age_prm,cause,r,yrs_scn) * pop(age_prm,r,yrs_scn) );

*health_scn("deaths_prm","BMK","all-rf","all-c",r,yrs_scn,"mean")
*         = sum(cause, health_scn("deaths_prm","BMK","all-rf",cause,r,yrs_scn,"mean"));

*health_scn("deaths_prm","BMK","all-rf",cause_p,"all-r",yrs_scn,"mean")
*        = sum(r, health_scn("deaths_prm","BMK","all-rf",cause_p,r,yrs_scn,"mean"));


*health_scn("YLL","BMK","all-rf",cause,r,yrs_scn,"mean")$r_data("all",r)
*         = sum(age, dr(age,cause,r,yrs_scn) * pop(age,r,yrs_scn)
*         * p_life_exp(age,"life_exp") );

*health_scn("YLL","BMK","all-rf","all-c",r,yrs_scn,"mean")
*         = sum(cause, health_scn("YLL","BMK","all-rf",cause,r,yrs_scn,"mean"));

*health_scn("YLL","BMK","all-rf",cause_p,"all-r",yrs_scn,"mean")
*         = sum(r, health_scn("YLL","BMK","all-rf",cause_p,r,yrs_scn,"mean"));


*        - aggregate regions:

*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"all-r",yrs_scn,stats) = sum(r, health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"HIC",yrs_scn,stats) = sum(r$HIC(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"UMC",yrs_scn,stats) = sum(r$UMC(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"LMC",yrs_scn,stats) = sum(r$LMC(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"LIC",yrs_scn,stats) = sum(r$LIC(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"AMR",yrs_scn,stats) = sum(r$AMR(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"AFR",yrs_scn,stats) = sum(r$AFR(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"EMR",yrs_scn,stats) = sum(r$EMR(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"EUR",yrs_scn,stats) = sum(r$EUR(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"SEA",yrs_scn,stats) = sum(r$SEA(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));
*health_scn(health_itm,diet_scn_p,riskf_p,cause_p,"WPR",yrs_scn,stats) = sum(r$WPR(r), health_scn(health_itm,diet_scn_p,riskf_p,cause_p,r,yrs_scn,stats));


*        - calculate percentages:
*health_scn("%deaths_avd/all",diet_scn,riskf_p,cause_p,rgs,yrs_scn,stats)
*         $health_scn("deaths","BMK","all-rf",cause_p,rgs,yrs_scn,"mean")
*         = 100 * health_scn("deaths_avd",diet_scn,riskf_p,cause_p,rgs,yrs_scn,stats)
*               / health_scn("deaths","BMK","all-rf",cause_p,rgs,yrs_scn,"mean");

*health_scn("%deaths_avd_prm/all",diet_scn,riskf_p,cause_p,rgs,yrs_scn,stats)
*         $health_scn("deaths_prm","BMK","all-rf",cause_p,rgs,yrs_scn,"mean")
*         = 100 * health_scn("deaths_avd_prm",diet_scn,riskf_p,cause_p,rgs,yrs_scn,stats)
*               / health_scn("deaths_prm","BMK","all-rf",cause_p,rgs,yrs_scn,"mean");

*health_scn("%YLL_avd/all",diet_scn,riskf_p,cause_p,rgs,yrs_scn,stats)
*         $health_scn("YLL","BMK","all-rf",cause_p,rgs,yrs_scn,"mean")
*         = 100 * health_scn("YLL_avd",diet_scn,riskf_p,cause_p,rgs,yrs_scn,stats)
*               / health_scn("YLL","BMK","all-rf",cause_p,rgs,yrs_scn,"mean");

*-------------------------------------------------------------------------------
