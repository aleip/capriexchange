

********************************************************************************
$ontext

   CAPRI project

   GAMS file : report.gms

   @purpose  :  To report health impacts based on Springmann et al. (2018)

      See Springmann, M., Wiebe, K., Mason-D'Croz, D., Sulser, T. B., Rayner, M., & Scarborough, P. (2018).
      Health and nutritional aspects of sustainable diet strategies and their association with environmental impacts: a global modelling analysis with country-level detail
      The Lancet Planetary Health Volume 2, Issue 10, October 2018, Pages e451-e461
      https://www.sciencedirect.com/science/article/pii/S2542519618302067

   @author   :
   @date     :
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy : data_prep.gms

$offtext
******************************


*-------------------------------------------------------------------------------
*        Compile report:
*-------------------------------------------------------------------------------


set        report_itm  items included in summary report /deaths_avd, deaths_avd_prm, YLL_avd/;

parameter  p_report_health(report_itm,foodagg,cause,rall,stats) report of health analysis;


*-------------------------------------------------------------------------------

*        compile report:

p_report_health(report_itm,foodagg,cause,rall,stats)
         = p_health(report_itm,foodagg,cause,rall,"%SIMY%",stats);

p_report_health(report_itm,foodagg,cause_p,rall,"std") = 0;

*-------------------------------------------------------------------------------
*        Unload data:
*-------------------------------------------------------------------------------

*        write to database:
execute_unload '%datdir%\diet\health_impacts.gdx' p_report_health, p_diet_eatcategories, p_RR_scn;

*-------------------------------------------------------------------------------

$exit

