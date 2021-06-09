
********************************************************************************
$ontext

   CAPRI project

   GAMS file : RR.gms

   @purpose  :  To estimate relative risks based on Springmann et al. (2018)

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
set      intake  /0*900/;
set      age "age groups"  /20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54,55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85+/;
set      riskf_pos (rows) "risk factors with positive health impacts" / frui, vegs, legumes, nutseed/ ;
set      cause  "causes of death"  /CHD, stroke, cancer, T2DM, resp_dis/  ;


parameters p_diet_eatcategories(*,*,*,*) "data on human consumption";
parameters p_RR_data(*,*,*) "relative risks per 100 g";
parameters p_RR_scn(*,*,*,*,) "relative risks for human consumption";
parameters p_age_scaling(*,*) "age effects of RRs";
parameters p_RR_age(*,*,*,*) "relative risks scaled by age";
parameters p_RR_scn_age(*,*,*,*,*, *) "relative risks by scenario and age";
parameters p_TMREL(*) "theoretical minimum risk exposure level for risk factor";
parameters p_TMREL_max(*) "max intake after which no additional benefit occurs";

execute_load "%datdir%\diet\diet_agg.gdx"  p_diet_eatcategories;

*-------------------------------------------------------------------------------
*        relative risk factors from Springmann et al. (2018)
*-------------------------------------------------------------------------------

*        RRs for fruits and vegetables from Aune et al (2017):
*        - use RRs for mortality (instead of incidence or both)

*        no further increase in benefits beyond 800-900 g/d (due to no data):
p_TMREL("fruvege")  = 900;
p_TMREL("frui")     = 800;
p_TMREL("vegs")     = 600;

*        FV and CHD:

*        - non-linear until 850 g/d:
p_RR_data("fruvege","CHD","mean") = 0.96;
p_RR_data("fruvege","CHD","low")  = 0.94;
p_RR_data("fruvege","CHD","high") = 0.98;

*        - non-linear until 800 g/d
p_RR_data("frui","CHD","mean") = 0.95;
p_RR_data("frui","CHD","low")  = 0.92;
p_RR_data("frui","CHD","high") = 0.99;

*        - non-linear until 600 g/d:
p_RR_data("vegs","CHD","mean") = 0.84;
p_RR_data("vegs","CHD","low")  = 0.80;
p_RR_data("vegs","CHD","high") = 0.88;

*        FV and stroke:

*        - non-linear until 850 g/d:
p_RR_data("fruvege","stroke","mean") = 0.90;
p_RR_data("fruvege","stroke","low")  = 0.87;
p_RR_data("fruvege","stroke","high") = 0.93;

*        - non-linear until 800 g/d
p_RR_data("frui","stroke","mean") = 0.77;
p_RR_data("frui","stroke","low")  = 0.70;
p_RR_data("frui","stroke","high") = 0.84;

*        - non-linear until 500 g/d:
p_RR_data("vegs","stroke","mean") = 0.95;
p_RR_data("vegs","stroke","low")  = 0.87;
p_RR_data("vegs","stroke","high") = 1.03;

*        FV and cancer:

*        - non-linear until 900 g/d:
p_RR_data("fruvege","cancer","mean") = 0.97;
p_RR_data("fruvege","cancer","low")  = 0.96;
p_RR_data("fruvege","cancer","high") = 0.98;

*        - non-linear until 600 g/d
p_RR_data("frui","cancer","mean") = 0.94;
p_RR_data("frui","cancer","low")  = 0.91;
p_RR_data("frui","cancer","high") = 0.97;

*        - non-linear until 600 g/d:
p_RR_data("vegs","cancer","mean") = 0.93;
p_RR_data("vegs","cancer","low")  = 0.91;
p_RR_data("vegs","cancer","high") = 0.95;

*        non-linear RRs (Aune et al, 2016, 2017):

table    p_RR_fruvege_CHD(intake,stats)
           mean        low         high
36         1.00        1.00        1.00
50         0.99        0.98        0.99
100        0.96        0.94        0.98
150        0.93        0.90        0.96
200        0.91        0.87        0.94
250        0.88        0.84        0.93
300        0.86        0.82        0.91
350        0.84        0.79        0.90
400        0.83        0.78        0.88
450        0.81        0.76        0.87
500        0.80        0.75        0.85
550        0.79        0.73        0.84
600        0.77        0.72        0.83
650        0.76        0.71        0.82
700        0.75        0.70        0.81
750        0.74        0.69        0.80
800        0.73        0.68        0.79
850        0.72        0.67        0.78
;

table    p_RR_frui_CHD(intake,stats)
           mean        low         high
6          1.00        1.00        1.00
50         0.98        0.96        0.99
100        0.95        0.92        0.99
150        0.93        0.89        0.98
200        0.91        0.86        0.96
250        0.89        0.84        0.94
300        0.87        0.81        0.92
350        0.85        0.79        0.90
400        0.82        0.77        0.88
450        0.81        0.75        0.86
500        0.79        0.73        0.85
550        0.77        0.71        0.83
600        0.75        0.69        0.82
650        0.73        0.67        0.80
700        0.71        0.65        0.79
750        0.70        0.63        0.78
800        0.69        0.62        0.77
;

table p_RR_vegs_CHD(intake,stats)
           mean        low         high
11         1.00        1.00        1.00
50         0.92        0.90        0.95
100        0.84        0.80        0.88
150        0.78        0.72        0.83
200        0.73        0.67        0.79
250        0.70        0.64        0.77
300        0.68        0.62        0.75
350        0.67        0.60        0.73
400        0.66        0.60        0.73
450        0.65        0.59        0.72
500        0.65        0.59        0.71
550        0.64        0.58        0.71
600        0.64        0.58        0.70
;

table    p_RR_fruvege_stroke(intake,stats)
           mean        low         high
34         1.00        1.00        1.00
50         0.98        0.97        0.98
100        0.90        0.87        0.93
150        0.83        0.79        0.87
200        0.77        0.72        0.83
250        0.73        0.67        0.79
300        0.69        0.63        0.76
350        0.66        0.60        0.73
400        0.63        0.57        0.70
450        0.61        0.55        0.68
500        0.59        0.53        0.67
550        0.57        0.50        0.65
600        0.56        0.48        0.64
650        0.54        0.46        0.63
700        0.52        0.44        0.61
750        0.50        0.42        0.60
800        0.49        0.40        0.59
850        0.48        0.39        0.59
;

table    p_RR_frui_stroke(intake,stats)
           mean        low         high
6          1.00        1.00        1.00
50         0.88        0.84        0.92
100        0.77        0.70        0.84
150        0.70        0.61        0.79
200        0.64        0.55        0.75
250        0.61        0.52        0.72
300        0.59        0.49        0.70
350        0.57        0.48        0.69
400        0.57        0.47        0.69
450        0.56        0.46        0.69
500        0.56        0.46        0.69
550        0.56        0.45        0.69
600        0.56        0.45        0.69
650        0.55        0.44        0.70
700        0.55        0.43        0.70
750        0.55        0.43        0.71
800        0.55        0.42        0.71
;

*        no significant risk reduction of stroke for vegetables,
*        but keep in as significant effect in all other recent
*        meta-analyses (see, e.g., Micha et al, 2017)
*        [NB: incidence stroke is significant and combination of
*         incidence and mortality as well, which might also have
*         been used by Micha et al (2017)]:
table    p_RR_vegs_stroke(intake,stats)
           mean        low         high
11         1.00        1.00        1.00
50         0.98        0.94        1.02
100        0.95        0.87        1.03
150        0.92        0.82        1.04
200        0.90        0.78        1.04
250        0.88        0.76        1.03
300        0.86        0.74        1.01
350        0.85        0.72        1.00
400        0.83        0.71        0.98
450        0.82        0.69        0.97
500        0.81        0.68        0.96
;

table    p_RR_fruvege_cancer(intake,stats)
           mean        low         high
40         1.00        1.00        1.00
50         0.99        0.99        1.00
100        0.97        0.96        0.98
150        0.94        0.93        0.96
200        0.92        0.89        0.94
250        0.90        0.87        0.93
300        0.88        0.84        0.91
350        0.86        0.83        0.90
400        0.85        0.81        0.89
450        0.84        0.80        0.88
500        0.83        0.79        0.88
550        0.83        0.79        0.87
600        0.83        0.79        0.86
650        0.82        0.79        0.86
700        0.82        0.79        0.86
750        0.82        0.79        0.86
800        0.82        0.78        0.85
850        0.82        0.78        0.85
900        0.82        0.78        0.85
;

table    p_RR_frui_cancer(intake,stats)
           mean        low         high
6          1.00        1.00        1.00
50         0.97        0.95        0.98
100        0.94        0.91        0.97
150        0.92        0.88        0.96
200        0.90        0.86        0.95
250        0.89        0.84        0.94
300        0.88        0.83        0.93
350        0.88        0.83        0.93
400        0.87        0.83        0.92
450        0.87        0.83        0.92
500        0.87        0.83        0.91
550        0.87        0.82        0.91
600        0.86        0.82        0.91
;

table    p_RR_vegs_cancer(intake,stats)
           mean        low         high
20         1.00        1.00        1.00
50         0.96        0.95        0.98
100        0.93        0.91        0.95
150        0.90        0.86        0.93
200        0.87        0.83        0.91
250        0.85        0.80        0.89
300        0.83        0.78        0.88
350        0.82        0.77        0.88
400        0.82        0.76        0.87
450        0.82        0.76        0.87
500        0.81        0.76        0.88
550        0.81        0.75        0.88
600        0.81        0.75        0.88
;

*-------------------------------------------------------------------------------

*        RRs for nuts and seeds from Aune et al (2016):
*        - use RRs for mortality (instead of incidence or both)
*        - for increase of 28 g/d;

*        no further increase in benefits beyond 20 g/d:
*        but non-linear RRs are defined until 28:
p_TMREL("nutseed") = 28;

*        nuts_seeds and CHD:

*        - non-linear until 28 g/d:
p_RR_data("nutseed","CHD","mean") = 0.71**(1/0.28);
p_RR_data("nutseed","CHD","low")  = 0.63**(1/0.28);
p_RR_data("nutseed","CHD","high") = 0.80**(1/0.28);

*        nuts_seeds and stroke:

*        - non-linear until 28 g/d:
p_RR_data("nutseed","stroke","mean") = 0.93**(1/0.28);
p_RR_data("nutseed","stroke","low")  = 0.83**(1/0.28);
p_RR_data("nutseed","stroke","high") = 1.05**(1/0.28);

*        nuts_seeds and cancer:

*        - non-linear until 28 g/d:
p_RR_data("nutseed","cancer","mean") = 0.85**(1/0.28);
p_RR_data("nutseed","cancer","low")  = 0.76**(1/0.28);
p_RR_data("nutseed","cancer","high") = 0.94**(1/0.28);

*        nuts_seeds and T2DM:

*        - non-linear until 28 g/d:
p_RR_data("nutseed","T2DM","mean") = 0.61**(1/0.28);
p_RR_data("nutseed","T2DM","low")  = 0.43**(1/0.28);
p_RR_data("nutseed","T2DM","high") = 0.88**(1/0.28);


table    p_RR_nutseed_CHD(intake,stats)
          mean        low         high
0         1.00        1.00        1.00
5         0.88        0.86        0.90
10        0.81        0.78        0.83
15        0.77        0.74        0.79
20        0.75        0.72        0.77
25        0.74        0.71        0.77
28        0.73        0.70        0.77
;

*        no statistically significant risk reduction of stroke for nuts:
table    p_RR_nutseed_stroke(intake,stats)
          mean        low         high
0         1.00        1.00        1.00
5         0.97        0.92        1.01
10        0.95        0.90        1.01
15        0.96        0.90        1.02
20        0.97        0.91        1.04
25        0.99        0.91        1.08
;

table    p_RR_nutseed_cancer(intake,stats)
          mean        low         high
0         1.00        1.00        1.00
5         0.93        0.91        0.96
10        0.90        0.86        0.94
15        0.88        0.85        0.92
20        0.88        0.85        0.91
25        0.89        0.85        0.92
;

table    p_RR_nutseed_T2DM(intake,stats)
          mean        low         high
0         1.00        1.00        1.00
5         0.80        0.69        0.93
10        0.72        0.60        0.86
15        0.71        0.62        0.82
20        0.74        0.63        0.86
25        0.76        0.60        0.97
;

*-------------------------------------------------------------------------------

*        RRs for legumes from Afshin et al (2014):
*        - serving size: 4 x 100 g/wk

*        - median intake ranged from 0 to 938 g/wk -> TMREL = 938/7 = 134
p_TMREL("legumes") = 138;

p_RR_data("legumes","CHD","mean") = 0.86**(1/(4/7));
p_RR_data("legumes","CHD","low")  = 0.78**(1/(4/7));
p_RR_data("legumes","CHD","high") = 0.94**(1/(4/7));

*-------------------------------------------------------------------------------

*        RR for red and processed meat:

*        - no lower bound for red meat consumption:
p_TMREL("readmeat") = 0.001;

*        Bechthold et al (2019),
*        "Food groups and risk of coronary heart disease,
*        stroke and heart failure: A systematic review
*        and dose-response meta-analysis of prospective
*        studies"

*        Schwingshackl et al (2017),
*        "Food groups and risk of type 2 diabetes mellitus: a systematic
*        review and meta-analysis of prospective studies"

*        - per 100 g/d increase
p_RR_data("redmeat","CHD","mean") = 1.15;
p_RR_data("redmeat","CHD","low")  = 1.08;
p_RR_data("redmeat","CHD","high") = 1.23;

p_RR_data("redmeat","stroke","mean") = 1.12;
p_RR_data("redmeat","stroke","low")  = 1.06;
p_RR_data("redmeat","stroke","high") = 1.17;

p_RR_data("redmeat","T2DM","mean") = 1.17;
p_RR_data("redmeat","T2DM","low")  = 1.08;
p_RR_data("redmeat","T2DM","high") = 1.26;


*-------------------------------------------------------------------------------
*        Assigning non-linear relative risks:
*-------------------------------------------------------------------------------

*        fruits:

p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")<=50) = p_RR_fruit_CHD("50",stats)**(1/0.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>50  and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=100) = p_RR_frui_CHD("100",stats);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>100 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=150) = p_RR_frui_CHD("150",stats)**(1/1.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>150 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=200) = p_RR_frui_CHD("200",stats)**(1/2.0);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>200 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=250) = p_RR_frui_CHD("250",stats)**(1/2.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>250 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=300) = p_RR_frui_CHD("300",stats)**(1/3.0);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>300 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=350) = p_RR_frui_CHD("350",stats)**(1/3.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>350 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=400) = p_RR_frui_CHD("400",stats)**(1/4.0);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>400 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=450) = p_RR_frui_CHD("450",stats)**(1/4.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>450 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=500) = p_RR_frui_CHD("500",stats)**(1/5.0);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>500 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=550) = p_RR_frui_CHD("550",stats)**(1/5.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>550 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=600) = p_RR_frui_CHD("600",stats)**(1/6.0);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>600 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=650) = p_RR_frui_CHD("650",stats)**(1/6.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>650 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=700) = p_RR_frui_CHD("700",stats)**(1/7.0);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>700 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=750) = p_RR_frui_CHD("750",stats)**(1/7.5);
p_RR_scn("frui","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>750) = p_RR_frui_CHD("800",stats)**(1/8.0);

p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")<=50) = p_RR_frui_stroke("50",stats)**(1/0.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>50  and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=100) = p_RR_frui_stroke("100",stats);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>100 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=150) = p_RR_frui_stroke("150",stats)**(1/1.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>150 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=200) = p_RR_frui_stroke("200",stats)**(1/2.0);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>200 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=250) = p_RR_frui_stroke("250",stats)**(1/2.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>250 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=300) = p_RR_frui_stroke("300",stats)**(1/3.0);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>300 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=350) = p_RR_frui_stroke("350",stats)**(1/3.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>350 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=400) = p_RR_frui_stroke("400",stats)**(1/4.0);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>400 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=450) = p_RR_frui_stroke("450",stats)**(1/4.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>450 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=500) = p_RR_frui_stroke("500",stats)**(1/5.0);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>500 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=550) = p_RR_frui_stroke("550",stats)**(1/5.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>550 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=600) = p_RR_frui_stroke("600",stats)**(1/6.0);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>600 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=650) = p_RR_frui_stroke("650",stats)**(1/6.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>650 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=700) = p_RR_frui_stroke("700",stats)**(1/7.0);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>700 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=750) = p_RR_frui_stroke("750",stats)**(1/7.5);
p_RR_scn("frui","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>750) = p_RR_frui_stroke("800",stats)**(1/8.0);

p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")<=50) = p_RR_frui_cancer("50",stats)**(1/0.5);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>50  and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=100) = p_RR_frui_cancer("100",stats);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>100 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=150) = p_RR_frui_cancer("150",stats)**(1/1.5);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>150 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=200) = p_RR_frui_cancer("200",stats)**(1/2.0);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>200 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=250) = p_RR_frui_cancer("250",stats)**(1/2.5);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>250 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=300) = p_RR_frui_cancer("300",stats)**(1/3.0);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>300 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=350) = p_RR_frui_cancer("350",stats)**(1/3.5);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>350 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=400) = p_RR_frui_cancer("400",stats)**(1/4.0);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>400 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=450) = p_RR_frui_cancer("450",stats)**(1/4.5);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>450 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=500) = p_RR_frui_cancer("500",stats)**(1/5.0);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>500 and p_diet_eatcategories("g","frui",rall,"%SIMY%")<=550) = p_RR_frui_cancer("550",stats)**(1/5.5);
p_RR_scn("frui","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","frui",rall,"%SIMY%")>550) = p_RR_fruits_cancer("600",stats)**(1/6.0);

*        vegetables (no impact on stroke):

p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=50) = p_RR_vegs_CHD("50",stats)**(1/0.5);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>50  and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=100) = p_RR_vegs_CHD("100",stats);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>100 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=150) = p_RR_vegs_CHD("150",stats)**(1/1.5);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>150 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=200) = p_RR_vegs_CHD("200",stats)**(1/2.0);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>200 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=250) = p_RR_vegs_CHD("250",stats)**(1/2.5);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>250 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=300) = p_RR_vegs_CHD("300",stats)**(1/3.0);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>300 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=350) = p_RR_vegs_CHD("350",stats)**(1/3.5);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>350 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=400) = p_RR_vegs_CHD("400",stats)**(1/4.0);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>400 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=450) = p_RR_vegs_CHD("450",stats)**(1/4.5);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>450 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=500) = p_RR_vegs_CHD("500",stats)**(1/5.0);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>500 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=550) = p_RR_vegs_CHD("550",stats)**(1/5.5);
p_RR_scn("vegs","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>550) = p_RR_vegs_CHD("600",stats)**(1/6.0);

p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")<=50) = p_RR_vegs_stroke("50",stats)**(1/0.5);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>50  and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=100) = p_RR_vegs_stroke("100",stats);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>100 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=150) = p_RR_vegs_stroke("150",stats)**(1/1.5);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>150 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=200) = p_RR_vegs_stroke("200",stats)**(1/2.0);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>200 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=250) = p_RR_vegs_stroke("250",stats)**(1/2.5);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>250 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=300) = p_RR_vegs_stroke("300",stats)**(1/3.0);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>300 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=350) = p_RR_vegs_stroke("350",stats)**(1/3.5);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>350 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=400) = p_RR_vegs_stroke("400",stats)**(1/4.0);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>400 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=450) = p_RR_vegs_stroke("450",stats)**(1/4.5);
p_RR_scn("vegs","stroke",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegetables",rall,"%SIMY%")>450) = p_RR_vegs_stroke("500",stats)**(1/5.0);

p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=50) = p_RR_vegs_cancer("50",stats)**(1/0.5);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>50  and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=100) = p_RR_vegs_cancer("100",stats);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>100 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=150) = p_RR_vegs_cancer("150",stats)**(1/1.5);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>150 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=200) = p_RR_vegs_cancer("200",stats)**(1/2.0);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>200 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=250) = p_RR_vegs_cancer("250",stats)**(1/2.5);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>250 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=300) = p_RR_vegs_cancer("300",stats)**(1/3.0);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>300 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=350) = p_RR_vegs_cancer("350",stats)**(1/3.5);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>350 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=400) = p_RR_vegs_cancer("400",stats)**(1/4.0);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>400 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=450) = p_RR_vegs_cancer("450",stats)**(1/4.5);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>450 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=500) = p_RR_vegs_cancer("500",stats)**(1/5.0);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>500 and p_diet_eatcategories("g","vegs",rall,"%SIMY%")<=550) = p_RR_vegs_cancer("550",stats)**(1/5.5);
p_RR_scn("vegs","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","vegs",rall,"%SIMY%")>550) = p_RR_vegs_cancer("600",stats)**(1/6.0);

*        nuts and seeds (no impact on stroke) [scale to 10 g/d]:

p_RR_scn("nutseed","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=5) = p_RR_nutseed_CHD("5",stats)**(0.10/0.05);
p_RR_scn("nutseed","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>5  and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=10) = p_RR_nutseed_CHD("10",stats)**(0.10/0.10);
p_RR_scn("nutseed","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>10 and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=15) = p_RR_nutseed_CHD("15",stats)**(0.10/0.15);
p_RR_scn("nutseed","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>15 and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=20) = p_RR_nutseed_CHD("20",stats)**(0.10/0.20);
p_RR_scn("nutseed","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>20 and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=25) = p_RR_nutseed_CHD("25",stats)**(0.10/0.25);
p_RR_scn("nutseed","CHD",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>28 ) = p_RR_nutseed_CHD("28",stats)**(0.10/0.28);

p_RR_scn("nutseed","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=5) = p_RR_nutseed_cancer("5",stats)**(0.10/0.05);
p_RR_scn("nutseed","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>5  and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=10) = p_RR_nutseed_cancer("10",stats)**(0.10/0.10);
p_RR_scn("nutseed","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>10 and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=15) = p_RR_nutseed_cancer("15",stats)**(0.10/0.15);
p_RR_scn("nutseed","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>15 and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=20) = p_RR_nutseed_cancer("20",stats)**(0.10/0.20);
p_RR_scn("nutseed","cancer",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>20) = p_RR_nutseed_cancer("25",stats)**(0.10/0.25);

p_RR_scn("nutseed","T2DM",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=5) = p_RR_nutseed_T2DM("5",stats)**(0.10/0.05);
p_RR_scn("nutseed","T2DM",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>5  and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=10) = p_RR_nutseed_T2DM("10",stats)**(0.10/0.10);
p_RR_scn("nutseed","T2DM",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>10 and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=15) = p_RR_nutseed_T2DM("15",stats)**(0.10/0.15);
p_RR_scn("nutseed","T2DM",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>15 and p_diet_eatcategories("g","nutseed",rall,"%SIMY%")<=20) = p_RR_nutseed_T2DM("20",stats)**(0.10/0.20);
p_RR_scn("nutseed","T2DM",rall,"%SIMY%",stats)$(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>20) = p_RR_nutseed_T2DM("25",stats)**(0.10/0.25);

*        assign others linearly:

p_RR_scn("redmeat",cause,rall,"%SIMY%",stats) = p_RR_scn("redmeat",cause,stats);
*RR_scn(diet_scn_p,"prc_meat",cause,rgs,yrs_scn,stats) = RR_data("prc_meat",cause,stats);
p_RR_scn("legumes",cause,rall,"%SIMY%",stats)  = p_RR_scn("legumes",cause,stats);

*p_RR_scn(riskf_w,cause,rall,"%SIMY%",stats) = p_RR_data(riskf_w,cause,stats);
p_RR_scn(foodagg,cause,rall,"%SIMY%",stats) = 1;

*-------------------------------------------------------------------------------

*        age effects of RRs:
*        - RRs decrease with age (see Singh et al, PLOS ONE 2013)
*        > adjust age-adjusted RRs for that by scaling in a similar fashion
*          as Micha et al (PLOS ONE 2017) and GBD 2015 (see age_effects.xls):

table    p_age_scaling(age,stats)
         mean    std
20-24    0.035   1.25
25-29    0.030   1.20
30-34    0.025   1.25
35-39    0.020   1.20
40-44    0.015   1.15
45-49    0.010   1.10
50-54    0.005   1.05
55-59    0.000   1.00
60-64    0.005   0.95
65-69   -0.010   0.90
70-74   -0.015   0.85
75-79   -0.020   0.70
80-84   -0.025   0.60
85+     -0.030   0.50
;

*        low and high CI intervals as pct deviation from mean:
p_RR_data(foodagg,cause,"std_low")$p_RR_data(foodagg,cause,"mean")
         = p_RR_data(foodagg,cause,"low")/p_RR_data(foodagg,cause,"mean")-1;

p_RR_data(foodagg,cause,"std_high")$p_RR_data(foodagg,cause,"mean")
         = p_RR_data(foodagg,cause,"high")/p_RR_data(foodagg,cause,"mean")-1;

*parameter p_RR_age;

*        - for positive risk factors:
p_RR_age(riskf_pos,cause,"mean",age)
         = p_RR_data(riskf_pos,cause,"mean") * (1-p_age_scaling(age,"mean"));

p_RR_age(riskf_pos,cause,"low",age)
         = p_RR_age(riskf_pos,cause,"mean",age)
         * (1+ p_RR_data(riskf_pos,cause,"std_low") * p_age_scaling(age,"std"));

p_RR_age(riskf_pos,cause,"high",age)
         = p_RR_age(riskf_pos,cause,"mean",age)
         * (1+ p_RR_data(riskf_pos,cause,"std_high") * p_age_scaling(age,"std"));

*        - for negative risk factors (only redmeat):
p_RR_age("redmeat",cause,"mean",age)
         = p_RR_data("redmeat",cause,"mean") * (1+p_age_scaling(age,"mean"));

p_RR_age("redmeat",cause,"low",age)
         = p_RR_age("redmeat",cause,"mean",age)
         * (1+ p_RR_data("redmeat",cause,"std_low") * p_age_scaling(age,"std"));

p_RR_age("redmeat",cause,"high",age)
         = p_RR_age("redmeat",cause,"mean",age)
         * (1+ p_RR_data("redmeat",cause,"std_high") * p_age_scaling(age,"std"));

*-------------------------------------------------------------------------------

*        age effects of RRs:
*        - RRs decrease with age (see Singh et al, PLOS ONE 2013)
*        > adjust age-adjusted RRs for that by scaling in a similar fashion
*          as Micha et al (PLOS ONE 2017) and GBD 2015 (see age_effects.xls):

*        low and high CI intervals as pct deviation from mean:
p_RR_scn(foodagg,cause,rall,"%SIMY%","std_low")$p_RR_scn(foodagg,cause,rall,"%SIMY%","mean")
         = p_RR_scn(foodagg,cause,rall,"%SIMY%","low")
         / p_RR_scn(foodagg,cause,rall,"%SIMY%","mean") - 1;

p_RR_scn(foodagg,cause,rall,"%SIMY%","std_high")$p_RR_scn(foodagg,cause,rall,"%SIMY%","mean")
         = p_RR_scn(foodagg,cause,rall,"%SIMY%","high")
         / p_RR_scn(foodagg,cause,rall,"%SIMY%","mean") - 1;

*parameter p_RR_scn_age;

*        - for positive risk factors:
p_RR_scn_age(riskf_pos,cause,rall,"%SIMY%","mean",age)
         = p_RR_scn(riskf_pos,cause,rall,"%SIMY%","mean") * (1-p_age_scaling(age,"mean"));

p_RR_scn_age(riskf_pos,cause,rall,"%SIMY%","low",age)
         = p_RR_scn_age(riskf_pos,cause,rall,"%SIMY%","mean",age)
         * (1+ p_RR_scn(riskf_pos,cause,rall,"%SIMY%","std_low") * p_age_scaling(age,"std"));

p_RR_scn_age(riskf_pos,cause,rall,"%SIMY%","high",age)
         = p_RR_scn_age(riskf_pos,cause,rall,"%SIMY%","mean",age)
         * (1+ p_RR_scn(riskf_pos,cause,rall,"%SIMY%","std_high") * p_age_scaling(age,"std"));

*        - for negative risk factors (only redmeat):
p_RR_scn_age("redmeat",cause,rall,"%SIMY%","mean",age)
         = p_RR_scn("redmeat",cause,rall,"%SIMY%","mean") * (1+p_age_scaling(age,"mean"));

p_RR_scn_age("redmeat",cause,rall,"%SIMY%","low",age)
         = p_RR_scn_age("redmeat",cause,rall,"%SIMY%","mean",age)
         * (1+ p_RR_scn("redmeat",cause,rall,"%SIMY%","std_low") * p_age_scaling(age,"std"));

p_RR_scn_age("redmeat",cause,rall,"%SIMY%","high",age)
         = p_RR_scn_age("redmeat",cause,rall,"%SIMY%","mean",age)
         * (1+ p_RR_scn("redmeat",cause,rall,"%SIMY%","std_high") * p_age_scaling(age,"std"));

*        RRs for nuts and seeds become negative if scaled to 100 g portion
*        initially -> scale now back to 100 g portion:

p_RR_scn_age("nutseed",cause,rall,"%SIMY%","std_low",age)$p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)
         = p_RR_scn_age("nutseed",cause,rall,"%SIMY%","low",age)
          /p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)-1;

p_RR_scn_age("nutseed",cause,rall,"%SIMY%","std_high",age)$p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)
         = p_RR_scn_age("nutseed",cause,rall,"%SIMY%","high",age)
          /p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)-1;

p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)
         = p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)**(1/0.1);

p_RR_scn_age("nutseed",cause,rall,"%SIMY%""low",age)
         = p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)
         * (1+ p_RR_scn_age("nutseed",cause,rall,"%SIMY%","std_low",age));

p_RR_scn_age("nutseed",cause,rall,"%SIMY%","high",age)
         = p_RR_scn_age("nutseed",cause,rall,"%SIMY%","mean",age)
         * (1+ p_RR_scn_age("nutseed",cause,rall,"%SIMY%","std_high",age));

*        calc standard deviation for PAF calculations:

p_RR_scn_age(foodagg,cause,rall,"%SIMY%","left_CI",age)
         = p_RR_scn_age(foodagg,cause,rall,"%SIMY%","mean",age)
         - p_RR_scn_age(foodagg,cause,rall,"%SIMY%","low",age);

p_RR_scn_age(foodagg,cause,rall,"%SIMY%","right_CI",age)
         = p_RR_scn_age(foodagg,cause,rall,"%SIMY%","high",age)
         - p_RR_scn_age(foodagg,cause,rall,"%SIMY%","mean",age);

p_RR_scn_age(foodagg,cause,rall,"%SIMY%","std",age)
         = p_RR_scn_age(foodagg,cause,rall,"%SIMY%","right_CI",age)$(p_RR_scn_age(foodagg,cause,rall,"%SIMY%","right_CI",age)>p_RR_scn_age(foodagg,cause,rall,"%SIMY%","left_CI",age))
         + p_RR_scn_age(foodagg,cause,rall,"%SIMY%","left_CI",age)$(p_RR_scn_age(foodagg,cause,rall,"%SIMY%","right_CI",age)<p_RR_scn_age(foodagg,cause,rall,"%SIMY%","left_CI",age));

*        alternative: use of average
*RR_scn_age(diet_scn_p,riskf,cause,rgs,yrs_scn,"std",age)
*         = (RR_scn_age(diet_scn_p,riskf,cause,rgs,yrs_scn,"right_CI",age)
*          + RR_scn_age(diet_scn_p,riskf,cause,rgs,yrs_scn,"left_CI",age))/2;

*-------------------------------------------------------------------------------

*        for foods which have no benefits after a certain point,
*        keep cons at that point to not overestimate impacts:

*set      riskf_max(riskf_d)  food-related risk factors with TMREL
*         /nuts_seeds/;
*parameter TMREL_max;

p_TMREL_max("nutseed")   = 28;

p_diet_eatcategories("g","nutseed",rall,"%SIMY%")
         $(p_diet_eatcategories("g","nutseed",rall,"%SIMY%")>p_TMREL_max("nutseed"))
         = p_TMREL_max("nutseed");

*-------------------------------------------------------------------------------

*        control for spurious values:

*        insert RR of 1 for normal weight:
*RR_scn_age(diet_scn_p,"normal",cause,r,yrs_scn,stats,age) = 1;
*RR_scn_age(diet_scn_p,"normal",cause,r,yrs_scn,"std",age) = 0;
*        - reset to one (as it got off 1 from age scaling):
*RR_scn_age(diet_scn_p,"underweight","T2DM",r,yrs_scn,stats,age) = 1;
*RR_scn_age(diet_scn_p,"underweight","T2DM",r,yrs_scn,"std",age) = 0;
*        - zero other risks (not included anymore):
*p_RR_scn_age(riskf_w,"Other",r,yrs_scn,stats,age) = 1;
*p_RR_scn_age(riskf_w,"Other",r,yrs_scn,"std",age) = 0;
*p_RR_scn_age(riskf_w,"Colon and rectum cancers",r,yrs_scn,stats,age) = 1;
*p_RR_scn_age(riskf_w,"Colon and rectum cancers",r,yrs_scn,"std",age) = 0;

p_RR_scn_age("nutseed","resp_dis",rall,"%SIMY%",stats,age) = 1;
p_RR_scn_age("nutseed","resp_dis",rall,"%SIMY%","std",age) = 0;

*-------------------------------------------------------------------------------



