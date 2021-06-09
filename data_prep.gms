
********************************************************************************
$ontext

   CAPRI project

   GAMS file : data_prep.gms

   @purpose  :  To match consumption scenarios from Springmann et al. (2018) to CAPRI's human intake outputs

      See Springmann, M., Wiebe, K., Mason-D'Croz, D., Sulser, T. B., Rayner, M., & Scarborough, P. (2018).
      Health and nutritional aspects of sustainable diet strategies and their association with environmental impacts: a global modelling analysis with country-level detail
      The Lancet Planetary Health Volume 2, Issue 10, October 2018, Pages e451-e461
      https://www.sciencedirect.com/science/article/pii/S2542519618302067

   @author   :
   @date     :
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************


* -----------------------------------------------------------------------------------------
* Product groups used as risk factors and its correspondence to the ones used by EAT-Lancet
* -----------------------------------------------------------------------------------------

set rows
      /redmeat "red meat products"
       fruvege "fruits and vegetables"
       frui "all fruits"
       vegs "all vegetables including non-dry pulses"
       legumes "legumes; includes dry beans, lentils, peas (172) + soy food (112) + peanuts (142)"
       nutseed "nuts and seeds"
      /;

set foodagg(rows) "food product aggregates"
      /redmeat "red meat products"
       fruvege "fruits and vegetables"
       frui "all fruits"
       vegs "all vegetables including non-dry pulses"
       legumes "legumes; includes dry beans, lentils, peas (172) + soy food (112) + peanuts (142)"
       nutseed "nuts and seeds"
      /;

set x_t_foodagg(rows,rows)
      /(BEEF,SGMT,PORK).redmeat
       (TOMA, OVEG, TABO, APPL, CITR, TAGR, TWIN, COCO).fruvege
       (TOMA, OVEG, TABO).vegs
       (APPL, CITR, TAGR, TWIN, COCO).frui
       (PULS, SOYA). legumes
       (OFRU, SUNF, RAPE).nutseed
        /;

*------------------------------------------------------------------------------
*   COLLECT HUMAN INTAKE REFERENCE FROM CAPRI
*------------------------------------------------------------------------------

parameter p_nutrientCont(rows,usda_cont)           "nutrient content per kg (mainly from USDA)";
parameter p_NCNC_CONT(rows,ncnc)                   "nutrient content per kg mapped from USDA texts to CAPRI codes";

$GDXIN %datdir%\arm\nutrient_cont_usda.gdx
$LOAD  p_nutrientCont
$LOAD  p_NCNC_CONT
$GDXIN

p_NCNC_CONT(rows,NCNC) = sum(ncnc_to_usda(ncnc,usda_cont),p_nutrientCont(rows,usda_cont));


* ------------------------------------------------------------------------------
* Parameter p_diet_eatcategories to check reference diets
* ------------------------------------------------------------------------------

parameter  p_diet_eatcategories(*,*,*,*) "data on human consumption";

* --- p_dataOutTemp is needed to define the reference diet:

execute_load "%results_in%\capmod\res_2_%BAS%%SIM%cap_after_2014_ref.gdx" p_DataOutTemp=dataout;

* --- For the calculation of relative risks, we need human consumption in grams per capita per day (net of waste) by food product in the reference scenario. (to be checked with Adrian: is all intake booked on 'N_CAL'?):

     p_DataOutTemp(rall, "", "INCE", rows, "%SIMY%")
     $ p_NCNC_CONT(rows, "N_CAL")
     = p_DataOutTemp(rall, "", "N_CAL", rows,"%SIMY%") / p_NCNC_CONT(rows, "N_CAL") * 1000;

* --- g per capita per day by food group in the reference scenario:

     p_diet_eatcategories(rall,"g",foodagg,"%SIMY%") = sum(x_t_foodagg(rows,foodagg), p_DataOutTemp(rall,"", "INCE", rows,"%SIMY%"));

* --- Save results:

   execute_unload "%datdir%\diet\diet_agg.gdx"  p_diet_eatcategories;

* --- calculate relative risks:
$include '%datdir%\diet\RR.gms'

* --- calculate population attributable fractions (PAFs):
$include '%datdir%\diet\PAF.gms'

* ---estimate final health outcomes (attributable deaths, etc)
$include '%datdir%\diet\health.gms'

* ---write report to database and spreadsheet:
$include '%datdir%\diet\report.gms'

execute_unload '%datdir%\diet\health_impacts.gdx' p_health;


$endif
$exit
*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------------




