
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
set age_prm "age groups for which deaths is premature (age 30-70 according to WHO)" /30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-64, 65-69/;
set health_ind  "indicators for health analysis"  /deaths_avd, deaths_avd_prm, YLL_avd, deaths, deaths_prm, YLL/   ;
set life_exp  /life_exp/;

parameter p_health(*,*,*,*,*,*) health analysis of diets;
parameter dr(*,*,*,*)           mortality rate (deaths per pop);
parameter pop(*,*,*)            population statistics (thousands);
parameter life_exp(*,*)         GBD 2010 standard abridged life table;
parameter p_total_death(*,*,*,*)total number of deaths;
parameter p_dr(*,*,*,*)         mortality rate (deaths per pop) adjusted for capri regions;
parameter p_pop(*,*,*)          population statistics (thousands) adjusted for capri regions;
*parameter p_r_data              ;
parameter p_diet_eatcategories(*,*,*,*);

$include '%datdir%\diet\mapIMPACTregions.gms'

execute_load '%datdir%\diet\dr_0203.gdx' dr, pop, life_exp;

p_total_death(age, cause, r, "%SIMY%") = dr(age,cause,r,"%SIMY%") * pop(age,r,"%SIMY%");

p_total_death(age, cause, rall, "%SIMY%") = sum(map_reg(r, rall), p_total_death(age, cause, r, "%SIMY%"));

p_pop(age,rall,"%SIMY%") = sum(map_reg(r, rall), pop(age,r,"%SIMY%"));

p_dr(age, cause, rall, "%SIMY%") = p_total_death(age, cause, rall, "%SIMY%")/p_pop(age,rall,"%SIMY%");

*p_r_data("mort",rall)$p_dr("35-39","CHD",rall,"%SIMY%") = yes;
*p_r_data("all",rall)$p_r_data("mort",rall) = yes;

*-------------------------------------------------------------------------------

*        select regions with data for health analysis:


*r_data("cons",r)$sum(fg, cons_scn_data("g/d_w",fg,r,"2010")) = yes;
*r_data("mort",r)$dr("35-39","CHD",r,"2010") = yes;
*r_data("bmi",r)$sum(weight, weight_scn("BMK",weight,r,"2010")) = yes;

*r_data("all",r)$r_data("mort",r) = yes;
*r_data("all",r)$(not r_data("cons",r)) = no;
*r_data("all",r)$(not r_data("bmi",r)) = no;

*-------------------------------------------------------------------------------


*        - avoided deaths:
p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","mean")
         = sum(age, p_PAF(foodagg,cause,rall,"%SIMY%","mean",age)
         * p_dr(age,cause,rall,"%SIMY%") * p_pop(age,rall,"%SIMY%") );

p_health("deaths_avd",foodagg,cause,rall,"%SIMY%","std")
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
p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","mean")
         = sum(age_prm, p_PAF(foodagg,cause,rall,"%SIMY%","mean",age_prm)
         * p_dr(age_prm,cause,rall,"%SIMY%") * p_pop(age_prm,rall,"%SIMY%") );

p_health("deaths_avd_prm",foodagg,cause,rall,"%SIMY%","std")
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
p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","mean")
         = sum(age, p_PAF(foodagg,cause,rall,"%SIMY%","mean",age)
         * p_dr(age,cause,rall,"%SIMY%") * p_pop(age,rall,"%SIMY%")
         * p_life_exp(age,"life_exp") );

p_health("YLL_avd",foodagg,cause,rall,"%SIMY%","std")
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
