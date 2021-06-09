
********************************************************************************
$ontext

   CAPRI project

   GAMS file : PAF.gms

   @purpose  :  To calculate population atttributable/impacts fractions (PAFs) based on Springmann et al. (2018).
                Note: original codes for the calculation of PAFs related to weight are deactivated.

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

set      stats   /mean, low, high, std/;
set      cause  "causes of death"  /CHD, stroke, cancer, T2DM, resp_dis/  ;
set      age "age groups"  /20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54,55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85+/;

parameters p_PAF(*,*,*,*,*) "population attributable fractions by scenario";
parameters p_diet_eatcategories(*,*,*,*) "data on human consumption";
parameters p_RR_scn_age(*,*,*,*,*, *) "relative risks by scenario and age";



*-------------------------------------------------------------------------------

*        PAF related to diet:
*        - for FV and meat, everybody in a country is exposed (p_food=1):
*          PAF_food = ( (p_food*RR_food)_ref -(p_food*RR_food)_scn )/(p_food*RR_food)_ref;
*                   = 1 - (p_food*RR_food)_scn/(p_food*RR_food)_ref;
*                   = 1 - ( power(RR_food,cns_chg_food)_scn/power(RR_food,cns_chg_food)_ref );


*-------------------------------------------------------------------------------
*       PAF related to diet (normalised serving size of 100 grams per day)
*-------------------------------------------------------------------------------

p_PAF(foodagg,cause,rall,"%SIMY%",stats,age)
         $p_RR_scn_age(foodagg,cause,rall,"%SIMY%",stats,age)
         = 1 - p_RR_scn_age(foodagg,cause,rall,"%SIMY%",stats,age)**(p_diet_eatcategories("g",foodagg,rall,"%SIMY%")/100)
             / p_RR_scn_age(foodagg,cause,rall,"%SIMY%",stats,age)**(p_diet_eatcategories("g",foodagg,rall,"%SIMY%")/100);

*        Uncertainty of PAF related to diet:
*        - d_PAF_food = PAF_food * sqrt(  power(d_RR_food/RR_food, 2)_ref
*                                       + power(d_RR_food/RR_food, 2)_scn );
*          with RR_food = RR**cns_chg; d_RR_food = RR**cns_chg * (cns_chg * d_RR/RR)
*          d_PAF_food = PAF_food * sqrt(  power(d_RR_food/RR_food,2)_ref
*                                       + power(d_RR_food/RR_food,2)_scn;

p_PAF(foodagg,cause,rall,"%SIMY%","std",age)
         $(p_RR_scn_age(foodagg,cause,rall,"%SIMY%","mean",age) and p_RR_scn_age(foodagg,cause,rall,"%SIMY%","mean",age))
         = p_PAF(foodagg,cause,rall,"%SIMY%","mean",age)
         * sqrt(power(p_RR_scn_age(foodagg,cause,rall,"%SIMY%","std",age)/p_RR_scn_age(foodagg,cause,rall,"%SIMY%","mean",age),2)
               + power(p_RR_scn_age(foodagg,cause,rall,"%SIMY%","std",age) /p_RR_scn_age(foodagg,cause,rall,"%SIMY%","mean",age),2) );


*        PAF for all dietary risks combined:
p_PAF("diet",cause,rall,"%SIMY%",stats,age)
         = 1 - prod(foodagg, 1-p_PAF(foodagg,cause,rall,"%SIMY%",stats,age));

*        Uncertainty of combined PAFs:
*        PAF_cns = 1 - (1-PAF_fvc)*(1-PAF_mtc)
*                = PAF_fvc + PAF_mtc - PAF_fvc*+PAF_mtc
*        -> d_PAF_cns = sqrt(  power(d_PAF_fvc,2) + power(d_PAF_mtc,2)
*                            + power(PAF_fvc*PAF_mtc,2)
*                              * (  power(d_PAF_fvc/PAF_fvc,2)
*                                 + power(d_PAF_mtc/PAF_mtc,2) ) );
*        PAF_all_scn is analogous;

p_PAF("diet",cause,rall,"%SIMY%","std",age)
         = sqrt(sum(foodagg,   power(p_PAF(foodagg,cause,rall,"%SIMY%","std",age) ,2))
                 + prod(foodagg,  power(p_PAF(foodagg,cause,rall,"%SIMY%","mean",age),2))
                 * ( sum(foodagg, power(p_PAF(foodagg,cause,rall,"%SIMY%","std",age)
                 /p_PAF(foodagg,cause,rall,"%SIMY%","mean",age),2)$p_PAF(foodagg,cause,rall,"%SIMY%","mean",age)) ) );

*-------------------------------------------------------------------------------

*        PAF related to weight:
*        - reformulate PAF for ease of calculation:
*          PAF_wgh = ( sum(p_weight*RR_weight)-sum(p'_weight*RR_weight) )/ sum(p_weight*RR_weight);
*                  = 1 - sum(p'_weight*RR_weight)/sum(p_weight*RR_weight);

*alias (weight, weightt);
*PAF_scn(diet_scn,"weight",cause,r,yrs_scn,"mean",age)
*         $sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age))
*         = 1- ( sum(weightt, weight_scn(diet_scn,weightt,r,yrs_scn)*RR_scn_age(diet_scn,weightt,cause,r,yrs_scn,"mean",age))
*               /sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age)) );

*        Uncertainty of PAF related to weight:
*        -   PAF_wgh = 1 - sum(p'_weight*RR_weight)/sum(p_weight*RR_weight);
*        - d_PAF_wgh = PAF_wgh * sqrt(  power(d_sum(p'_weight*RR_weight)/sum(p'_weight*RR_weight),2)
*                                     + power(d_sum(p_weight*RR_weight)/sum(p_weight*RR_weight),2) );
*                      with d_sum(p_weight*RR_weight) = sqrt( sum( power(p_weight*d_RR_weight,2) ) );
*          d_PAF_wgh = PAF_wgh * sqrt(  sum( power(p'_weight*d_RR_weight,2) )/power(sum(p'_weight*RR_weight),2)
*                                     + sum( power(p_weight*d_RR_weight,2) )/power(sum(p_weight*RR_weight),2) );

*PAF_scn(diet_scn,"weight",cause,r,yrs_scn,"std",age)
*         $sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age))
*         = PAF_scn(diet_scn,"weight",cause,r,yrs_scn,"mean",age)
*         * sqrt(   sum(weight, power(weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"std",age),2) )
*                 / power( sum(weight, weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"mean",age)),2)
*                 + sum(weight, power(weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"std",age),2) )
*                 / power( sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age)),2) );

*        PAF disagg for underweight:
*weight_scn(diet_scn,weight,r,yrs_scn)        = weight_scn("BMK",weight,r,yrs_scn);
*weight_scn(diet_scn,"underweight",r,yrs_scn) = weight_ref(diet_scn,"underweight",r,yrs_scn);
*weight_scn(diet_scn,"normal",r,yrs_scn)      = 1 - sum(weight_riskf, weight_scn(diet_scn,weight_riskf,r,yrs_scn));
*PAF_scn(diet_scn,"underweight",cause,r,yrs_scn,"mean",age)$sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age))
*         = 1- ( sum(weightt, weight_scn(diet_scn,weightt,r,yrs_scn)*RR_scn_age(diet_scn,weightt,cause,r,yrs_scn,"mean",age))
*               /sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age)) );

*PAF_scn(diet_scn,"underweight",cause,r,yrs_scn,"std",age)
*         $sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age))
*         = PAF_scn(diet_scn,"underweight",cause,r,yrs_scn,"mean",age)
*        * sqrt(   sum(weight, power(weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"std",age),2) )
*                / power( sum(weight, weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"mean",age)),2)
*                + sum(weight, power(weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"std",age),2) )
*                 / power( sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age)),2) );

*        PAF disagg for overweight:
*weight_scn(diet_scn,weight,r,yrs_scn)       = weight_scn("BMK",weight,r,yrs_scn);
*weight_scn(diet_scn,"overweight",r,yrs_scn) = weight_ref(diet_scn,"overweight",r,yrs_scn);
*weight_scn(diet_scn,"normal",r,yrs_scn)     = 1 - sum(weight_riskf, weight_scn(diet_scn,weight_riskf,r,yrs_scn));
*PAF_scn(diet_scn,"overweight",cause,r,yrs_scn,"mean",age)$sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age))
*         = 1- ( sum(weightt, weight_scn(diet_scn,weightt,r,yrs_scn)*RR_scn_age(diet_scn,weightt,cause,r,yrs_scn,"mean",age))
*               /sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age)) );

*PAF_scn(diet_scn,"overweight",cause,r,yrs_scn,"std",age)
*         $sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age))
*         = PAF_scn(diet_scn,"overweight",cause,r,yrs_scn,"mean",age)
*         * sqrt(   sum(weight, power(weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"std",age),2) )
*                 / power( sum(weight, weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"mean",age)),2)
*                 + sum(weight, power(weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"std",age),2) )
*                 / power( sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age)),2) );

*        PAF disagg for obesity:
*weight_scn(diet_scn,weight,r,yrs_scn)   = weight_scn("BMK",weight,r,yrs_scn);
*weight_scn(diet_scn,weight_obese,r,yrs_scn)  = weight_ref(diet_scn,weight_obese,r,yrs_scn);
*weight_scn(diet_scn,"normal",r,yrs_scn) = 1 - sum(weight_riskf, weight_scn(diet_scn,weight_riskf,r,yrs_scn));
*PAF_scn(diet_scn,"obese",cause,r,yrs_scn,"mean",age)$sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age))
*         = 1- ( sum(weightt, weight_scn(diet_scn,weightt,r,yrs_scn)*RR_scn_age(diet_scn,weightt,cause,r,yrs_scn,"mean",age))
*               /sum(weightt, weight_scn("BMK",weightt,r,yrs_scn)*RR_scn_age("BMK",weightt,cause,r,yrs_scn,"mean",age)) );

*PAF_scn(diet_scn,"obese",cause,r,yrs_scn,"std",age)
*         $sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age))
*         = PAF_scn(diet_scn,"obese",cause,r,yrs_scn,"mean",age)
*         * sqrt(   sum(weight, power(weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"std",age),2) )
*                 / power( sum(weight, weight_scn(diet_scn,weight,r,yrs_scn)*RR_scn_age(diet_scn,weight,cause,r,yrs_scn,"mean",age)),2)
*                + sum(weight, power(weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"std",age),2) )
*                / power( sum(weight, weight_scn("BMK",weight,r,yrs_scn)*RR_scn_age("BMK",weight,cause,r,yrs_scn,"mean",age)),2) );

*-------------------------------------------------------------------------------

*        - combined risk factors:
*PAF_scn(diet_scn,"all-rf",cause,r,yrs_scn,stats,age)
*         = 1 - prod(riskf_c, 1-PAF_scn(diet_scn,riskf_c,cause,r,yrs_scn,stats,age));

*PAF_scn(diet_scn,"all-rf",cause,r,yrs_scn,"std",age)
*         = sqrt(   sum(riskf_c,   power(PAF_scn(diet_scn,riskf_c,cause,r,yrs_scn,"std",age) ,2))
*                 + prod(riskf_c,  power(PAF_scn(diet_scn,riskf_c,cause,r,yrs_scn,"mean",age),2))
*                 * ( sum(riskf_c, power(PAF_scn(diet_scn,riskf_c,cause,r,yrs_scn,"std",age)
*                                 /PAF_scn(diet_scn,riskf_c,cause,r,yrs_scn,"mean",age),2)$PAF_scn(diet_scn,riskf_c,cause,r,yrs_scn,"mean",age)) ) );

*-------------------------------------------------------------------------------
