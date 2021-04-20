********************************************************************************
$ontext

   CAPRI project

   GAMS file : merging_health.gms

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
   @calledBy : cons_0203.gms (note: Springmann et al. (2018) sets will be called by data_prep.gms (?))

$offtext
********************************************************************************

* --- auxiliary sets:

set diet_scn_p diet scenarios /"BMK", "FLX", "PSC", "VEG", "VGN" /;
set unit_wp consumption_unit(net of waste)  /"g/d_w"/;


*------------------------------------------------------------------------------
*   COLLECT HUMAN INTAKE REFERENCE FROM CAPRI
*------------------------------------------------------------------------------

parameter p_nutrientCont(rows,usda_cont)           "nutrient content per kg (mainly from USDA)";
parameter p_NCNC_CONT(rows,ncnc)                   "nutrient content per kg mapped from USDA texts to CAPRI codes";

$GDXIN %datdir%\arm\nutrient_cont_usda.gdx
$LOAD  p_nutrientCont
$GDXIN

p_NCNC_CONT(rows,NCNC) = sum(ncnc_to_usda(ncnc,usda_cont),p_nutrientCont(rows,usda_cont));

* --- files converting regions/countries and food groups from CAPRI into EAT-Lancet categorization (from A.LEIP):

$include '%curdir%\pol_input\epnf\tasteshift\mapIMPACTregions.gms'
$include '%curdir%\pol_input\epnf\tasteshift\EATmap2capri.gms'

* --- files from Springmann et al:
$include data_0203
$include parameters_0203

* --- Defining the parameter to integrate CAPRI and Springmann et al. model:

parameter cons_scn_data(*,*,*,*,*) "data on consumption scenarios";

* --- Consumption data from Springmann et al. relates to human intake in CAPRI, so we first need to import reference data for human intake from CAPRI:

execute_load "%results_in%\capmod\res_2_%BAS%%SIM%cap_after_2014_ref.gdx" p_DataOutTemp=dataout;

* --- Consumption data from Springmann et al. is presented in g/d_w, kcal/d_w, g/d and kcal/d in the file cons_scn_data.gdx, but only g/d_w is used in the calculation of relative risks (RR_0203.gms).
* --- Intake in g per capita per day (net of waste) by food product in the reference scenario (to be checked with Adrian: is all intake booked on 'N_CAL'?):

     p_DataOutTemp(rall, "", "INCE", rows, "%SIMY%")
     $ p_NCNC_CONT(rows, "N_CAL")
     = p_DataOutTemp(rall, "", "N_CAL", rows,"%SIMY%") / p_NCNC_CONT(rows, "N_CAL") * 1000;

* --- Food groups available in Springmann et al. are the following (26 fg): wheat, rice, maize, othr_grains, roots, sugar, legumes, soybeans, nuts_seeds, oil_veg, oil_palm, vegetables, fruits_trop, fruits_temp,
* --- fruits_starch, beef, lamb, pork, poultry, eggs, milk, shellfish, fish_freshw, fish_pelag, fish_demrs, othrcrp. However, relative risks are calculated based only based on fruits (fruits_temp, fruits_trop, fruits_starch),
* --- vegetables, nuts_seeds, legumes (legumes, soybeans) and red_meat (beef, lamb, pork)
* --- To be checked with A.LEIP: names and level of aggregation of the food groups. In Springmann et al., there is also RR attributable to processed meat, which will have to be disactivated in the code saved in the /dat.

      cons_scn_data(rall,"g/d_w","BMK",eatFoodGrp, "2010") = sum(eatFood2O(eatFoodGrp, rows), p_DataOutTemp(rall, "", "INCE", rows,"%SIMY%"));

* --- Mapping CAPRI regions to Springmann et al (180 regions):

      cons_scn_data(r,"g/d_w","BMK",eatFoodGrp, "2010") =  sum((map_reg(r, rall), cons_scn_data(rall,"g/d_w",eatFoodGrp, "2010"));

* --- To be checked with A.LEIP: Before generating the final file, we need to create different diet scenarios for "FLX", "PSC", "VEG" and "VGN". The flexitarian dietary patterns contain no processed meat, low amounts of
* --- red meat (including beef, lamb, and pork) and sugar, moderate amounts of poultry, dairy, and fish, and generous amounts of fruits, vegetables, legumes, and nuts. The other three dietary patterns replace meat
* --- (pescatarian or vegetarian) or all animal source foods (vegan) with two-thirds either fish and seafood (pescatarian diets) or legumes (vegetarian and vegan diets) and a third fruits and vegetables. The reorganization of the
* ---  different diet patterns should be done at the country level (to respect national preferences). How to do it in CAPRI?




* --- Save results to files that will be linked to Springmann et al consumption:

   execute_unload '%cons_scn_0203%.gdx' cons_scn_data;

* --- files from Springmann et al:
* --- load consumption data (< cons_scn_0203.gdx):
$include cons_0203

* --- load weight data (< weight_data_0203.gdx):
*$include weight_0203

* --- calculate relative risks:
$include RR_0203

* --- calculate population attributable fractions (PAFs):
$include PAF_0203

* ---estimate final health outcomes (attributable deaths, etc)
$include health_0203

* ---write report to database and spreadsheet:
$include report_0203

*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------------
