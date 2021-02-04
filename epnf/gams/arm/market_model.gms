********************************************************************************
$ontext

   CAPRI project

   GAMS file : MARKET_MODEL.GMS

   @purpose  : Definition and declaration of variables,
               equations in the Armington based market model of CAPRI

   @module   : market_model

   @author   : W.Britz, Uni Bonn
   @date     : 27.10.09
   @since    : September 2002
   @refDoc   :
   @seeAlso  : arm\simu_market.gms (actual simulation with model)
               arm\market1.gms (initialization)
   @calledBy : arm\market1.gms

$offtext
********************************************************************************
* --------------------------------------------------------------------------------------
*
*     Definition of model parameters, variables, equations and models
*
* --------------------------------------------------------------------------------------
*
 SET ABC(COLS)  / UvagA,UvagB,UvagC /;
 set calCurTrdYSimy / TRD,Y /;
 SET basCalCurLoLUp "frequently used positions in debugging efforts" / BAS,CAL,CUR,Lo,L,Up /;

 Set infesReportPos(COLS)  / grof,prod,feed,proc,hcon,biof,Arm1,domSales,Arm2,arm1V,arm2V,Imports,uvai,tars,tarv,ppri,cpri,procMarg,trqImports,minBordP,triggerP/;
*
 SET someItemsForShFstInSec "Sume items used in p_shFstInSecRmall which are partly loaded from gdx" / foreOrUaar,CorrForeOlndArtif  /;

*     May permit "exact" implemetation of taste shifts (without deviations due to second round price effects)
*     but was used in EPNF to resolve unclear infeasibilties for ETH.RAPO in some scenarios
*     (via "closure swap" between v_consqunat and v_commitPar))
 set xxFreeCommitments(rms,xx1)  "Items with fixed consumption and free committment terms";
 xxFreeCommitments(rms,xx1) = no;

 PARAMETER

           p_tradeFlows(*,Rmall,*,*)                "Physical import flows, destination first"
           p_tradeFlowWarning(*,Rmall,*,*)          "Warning parameter for inadmissible calibration settings for trade flows that will be ignored"
           p_impPrice(*,RMall,*,*)                  "Import prices  by origin, after transport costs and border protection measures"
           p_fobPrice(*,RMall,*,*)                  "F.O.B. prices"
           p_flexLevy(RMall,RMall,XX,*)

           p_entryPriceFac(RMall,RMall1,XX,basCalCur)     "Parameter to calibrate the function setting tarfifs under entry price regimes"

           p_pdGL(RMS,XX1,basCalCur)                "Constant terms in Generalised Leontief Expenditure function"
           p_pbGL(RMS,XX1,YY1,basCalCur)            "Terms in form of square roots in Generalised Leontief Expenditure function"

           p_hessNQSupp(RMS,*,*,basCalCur)          "Hessian of normalised quadratic profit function"
           p_cnstNQSupp(RMS,*)                      "Constant of normalised quadratic profit function"

           p_hessNQFeed(RMS,XX1,YY1,basCalCur)      "Hessian of normalised quadratic feed cost function"
           p_cnstNQFeed(RMS,XX1)                    "Constant of normalised quadratic feed cost function"

           p_hessNQDairy(RMS,XX1,YY1,basCalCur)     "Hessian of normalised quadratic profit function for dairies"
           p_cnstNQDairy(RMS,XX1)                   "Constant of normalised quadratic profit function for dairies"

           p_hessNQProc(RMS,XX1,YY1,basCalCur)      "Hessian of normalised quadratic profit function for processing"
           p_cnstNQProc(RMS,XX1)                    "Constant of normalised quadratic profit function for processing"

           p_transpCost(RMall,RMall,XX1,*)          "Transport costs estimates"
           p_ntm(rall,rall,xx)                    "explicit NTMs"
           p_feedByProdShare(RMS,XX)                "By product share from feed/food processing"

           p_rhoArm1(RM,XX1)                         "Substitution parameter first stage"
           p_rhoArm2(RM,XX1)                         "Substitution parameter second stage"
           p_rhoBioFuel(RMS,XXBIOF)                 "Substitution parameter between biofuel feedstocks"

           p_adjustConsPriceMarg(RMS,XX)         "Margins between consumer prices and Arm1P updated based on net trade position of country"


* --- possibly obsolete parameter
*     note that this correction factor does not enter the feedstock demand eqation for biofuels (Biof_) anymore
*     the correction factor for the Armington1 price is practically replaced by the market clearing price for out-of-quota sugar (v_CSugarPrice)

           p_rhoFeed(RMS,FEED)                      "Substitution between feed inside the blocks used in the supply model"
           p_rhoProc(RMS,SED)                       "Transformation elasticity for oilseed processing"

           p_calContent(XX1)                        "calorie content per kg or kcal per ton"
           p_nutrientCont(rows,usda_cont)           "nutrient content per kg (mainly from USDA)"
           p_NCNC_CONT(rows,ncnc)                   "nutrient content per kg mapped from USDA texts to CAPRI codes"

           p_feedConvBlk(RMS,ROWS,FEED_TRD)         "Per unit of output requirement of bulk of feedstuff such as cereals"
           p_feedBlkElas(RMS,FEED_TRD,ROWS)
           p_stdDevWorldMrkPrices(ROWS)             "Standard deviation of world market prices"

           p_MaxImp(RM,XX)                          "Maximum imports"

           p_weight(RMAll,CalMarketWgtCols)         "Weight for specific trade blocks and countries in objective"
           p_addWgtDifImpP(RMall,XX)                "Add weight for diff of import prices from destination market price to curb Armington effects"

           p_trqBilat(*,*,ROWS,TRQSET,*)            "Bilateral TRQ data"
           p_trqGlobl(*,ROWS,TRQSET,*)              "TRQ data for all trading partners"
           p_doubleZero(*,*,*,*)                    "Double zero aggreements"


           p_checkI(*,*,*)                          "Parameter listing infeasible equations in the market model, and variables hitting bounds"
           p_checkSquare(RMall,*,*)                 "Parameter listing non paired variables (= not fixed, but not linked to an equation)"
           p_checkSquare2(RMall,RMALL,*,*)          "Parameter listing non paired variables (= not fixed, but not linked to an equation), for variables with more geographical dimensions"

           p_tempStorageBounds                      "Temporary storage for bounds during pre-steps of market model"
           p_trqSigmoidSlope(RMall,XX)                 "Steepness of Sigmoid fudging function for tariffs under TRQs"

           PPSQAutoFin(*)                           "Switch for auto-financing of sugar quota regime: Marcel to add more!"
* --- PPSQAutoFin switches between price transmission implementations according to pre- and post sugar reform (2006 reform); see set_prices.gms
* (i) in calibration mode
*     pre-reform:  UVAGc is determined by the unit value of export (UVAE)
*     post-reform: UVAGc depends on bioethanol prices (as out-of-quota production goes to the industry and not anymore dumped on the world markets)
* (ii) in simulation mode
*     UVAGc follows the development in the market clearing price of out-of-quota sugar (v_CSugarPrice)

           LevyDist(*,*)
           Levy(*,*)
           p_exchgRateChangeFactor(*,*)             "Factor is > 1 if exporting region revalues compared to importing region relative to calibration point"
           p_lstIters(*)                            "Flag to indicate if a group of products or a single products need to solved in the pre-step of the market model, typcially equal to the last iterations used"
           p_lstPreStep                             "Flag to tell the model to solve all groups again to provide a good starting point for the market model"
           p_maxEU27Exports(XX)                     "Maximum exports from EU27 in calibration point"
           p_newEnergyCropYield(*)                  "biomass yields for new energy crops"
           p_secgConv(*)                            "Conversion coefficients for biomass to biofuel"


           p_landSupplyElas(RMS)                    "Land supply elasticity"
           p_shFstInSecRmall(rmAll,*,*)             "Shares of first (non-region) item in a second item mostly related to land use"
           p_elasSupp(RMALL,XX_ALL,XX_ALL,BASCAL)
           p_arm1PriceCnst(RM,XX)
           p_stockChgSlope(RM,XX)
           p_stockChgShare(RM,XX)


           p_sugbMarginShift(Rall,ABC)         "Shift of sugar beet margins due to sugar reform 2006"

           p_bioSupPar                         "biofuel supply function parameters"
           p_bioDemPar                         "biofuel demand function parameters"

           p_cSugarCalPar(RMS)
           p_feedBlkCnst(RMS,FEED_TRD)

           p_infesReport(rall,infesReportPos,rows)

           p_fixedTradeflows(RM,RM,XX)             "Exogenously fixed trade flows"


* --- elasticity parameters of the intervention buying/releases share equations (always relative to a numeraire)
           p_elasBuying(Rmall,XX)
           p_elasRelease(Rmall,XX)
* --- numeraires for interv. shares (rm in rmtp)
           p_numeraireBuying(RMall,XX)
           p_numeraireRelease(RMall,XX)

           p_oilCostShare(RMS,XX1)
           p_infesInMarket(*,*,*,*)     "Infeasible equations in market model"


*
*   ---    relevant for the modified Armington approach(es)
*
           p_arm2Commit(RMall,RMall,XX)       "Commitment parameter for the modified Armington specification (Armington 2 nest)"
           p_realRhoArm2(rmall, xx)           "Rho parameter in the CES utility aggregator"
           p_deltaCES(rmall, rmall, xx)       "CES share parameters (deltas), in fact delta**sigma = p_dpCESTrade"

           p_transitA(rmall, rmall, xx)       "parameter of the transition function for the KVT approach"
           p_transitB(rmall, rmall, xx)       "parameter of the transition function for the KVT approach"

           ;
$setglobal markupBiofFeedCost off
$iftheni.mark1 %markupBiofFeedCost%==on
Parameter p_markupBiofFeedCost(RMS,XX)          "price mark up on biofuel feedstock prices";
 p_markupBiofFeedCost(RMS,XBioStock) = 0;
$endif.mark1

p_lstPreStep = 0;

 p_arm2Commit(RMall,RMall1,XX) = 0;
 p_deltaCES(rm, rm1, xx)       = 1;
 p_transitA(rm, rm1, xx)       = 0;
 p_transitB(rm, rm1, xx)       = 1;

 p_oilCostShare(RMS,XCERE) = 0.15;
 p_oilCostShare(RMS,SED)   = 0.10;
 p_oilCostShare(RMS,"PLMO") = 0.05;
 p_oilCostShare(RMS,"OTHO") = 0.05;
 p_oilCostShare(RMS,"PULS") = 0.05;
 p_oilCostShare(RMS,"POTA") = 0.10;
 p_oilCostShare(RMS,"SUGA") = 0.10;
 p_oilCostShare(RMS,"TEXT") = 0.10;
 p_oilCostShare(RMS,"TOBA") = 0.10;
 p_oilCostShare(RMS,"TOMA") = 0.30;
 p_oilCostShare(RMS,"OVEG") = 0.15;
 p_oilCostShare(RMS,"OLIO") = 0.05;
 p_oilCostShare(RMS,"TWIN") = 0.05;
 p_oilCostShare(RMS,"APPL") = 0.05;
 p_oilCostShare(RMS,"OFRU") = 0.05;
 p_oilCostShare(RMS,"CITR") = 0.05;
 p_oilCostShare(RMS,"TAGR") = 0.05;
 p_oilCostShare(RMS,"TABO") = 0.05;
 p_oilCostShare(RMS,"COCO") = 0.02;
 p_oilCostShare(RMS,"COFF") = 0.02;
 p_oilCostShare(RMS,"TEAS") = 0.02;

 p_oilCostShare(RMS,"MILK") = 0.10;
 p_oilCostShare(RMS,"BEEF") = 0.01;
 p_oilCostShare(RMS,"SGMT") = 0.01;
 p_oilCostShare(RMS,"POUM") = 0.02;
 p_oilCostShare(RMS,"PORK") = 0.02;
 p_oilCostShare(RMS,"EGGS") = 0.05;

 p_oilCostShare(RMS,"FFIS") = 0.01;
 p_oilCostShare(RMS,"SFIS") = 0.02;
 p_oilCostShare(RMS,"OAQU") = 0.02;


 p_oilCostShare(RMS,"INPE") = 0;
 p_oilCostShare(RMS,"LAND") = 0;
 p_oilCostShare(RMS,"FEDE") = 0;

*
* --- effect of crude oil price on production costs is already
*     captured in supply models, put to zero in market model
*     if supply model are switched on
*
$ifi not %supply_model% == off p_oilCostShare(RMSSUP,XX1) = 0;

* --- two ad hoc options for fine tuning of scenarios or baseline (used e.g. by Swiss team)

 option kill= p_addWgtDifImpP;
 option kill= p_fixedTradeFlows;

 scalar
           p_trim                                "Switches on endogenous determination of processing yields and switches of test on share parameters dp.l in Armington equations"
           p_hasPreStep                          "Determines if the market model is sloved with a pre-step or not"
           p_endoBioMarket                       "Flag to switch endogenous biofuel market on(1) or off(0)"
           p_lstPreStep                          "Flag to tell the model to solve all groups again to provide a good starting point for the market model";


* --- Nutrient contents from USDA plus very few exceptions
*
*$CALL "gdxxrw %datdir%\arm\nutrient_cont_usda.xlsx O=%datdir%\arm\nutrient_cont_usda.gdx  par=p_nutrientCont rdim=1 cdim=1 rng=for_reading!a1:aa74 ignorecolumns=b";
$GDXIN %datdir%\arm\nutrient_cont_usda.gdx
$LOAD  p_nutrientCont
$GDXIN
   p_NCNC_CONT(rows,NCNC) = sum(ncnc_to_usda(ncnc,usda_cont),p_nutrientCont(rows,usda_cont));
*display p_nutrientCont,p_NCNC_CONT;
*
*
* --- Take calory content of CAPRI market model products (declared over XX1) from p_NCNC_CONT:
  p_calContent(XX1) = sum(sameas(XX1,rows),p_NCNC_CONT(rows,"N_CAL"));

 parameter p_additivePPriMargin(XX);
 option kill=p_additivePPriMargin;

 scalar p_trimlnd            "Switch for calibration mode of land allocation" /0/;

 scalar p_useStepWeightsForQuantities / 0 /;

 Scalar P_lowLimitFactQuant  "Multiplication factor to derive lower limits for quantity variables" / 1.E-5 /;
 Scalar p_uppLimitFactQuant  "Multiplication factor to derive upper limits for quantity variables" / 1.E+3 /;
*
 Scalar P_lowLimitFactPrice  "Multiplication factor to derive lower limits for price variables"   / 0.10 /;
 Scalar p_uppLimitFactPrice  "Multiplication factor to derive upper limits for price variables"   / 10.00 /;

 Scalar p_cSugarSupElas      "C sugar supply elasticity set very high to stabilise C prices"  /20/;



*    p_bioDemPar(RMS,"BIOE","add","MAX") = 0;



** TRQSLOPE is chosen such that the sigmoid function jumps from zero to one within the range of +-3% of the TRQ
** A Steeper slope would be more realistic, but potentially produces more infeasibilities in market model
** Note that this number must re-appear in arm\trim_trqs.gms
 p_trqSigmoidSlope(RMall,XX) = 200;

 set abcd / a,b,c,d /;
**

  Parameter p_oilEqu(ROWS) "Crude fossil oil equivalents"/
* From:
* Different sources, saved in U:\IPTS_CAPRI_BIOFUEL\Analysis\Data\scenario_ass (by P. Witzke)
* 1t =   toe
  DISL  1.01
  GASL  1.05
  BIOD  0.86
  BIOE  0.64
        /;

 VARIABLES
           v_landSupply(RMS)                    "Land supply [kha] to agriculture"
           v_landAgr(RMS,xx_all)                "land use [kha] of agricultural products in market model"
           v_landAgrTmp(RMS,xx_all)             "prelim estimate of land use [kha] by product before imposing agricultural land balance"
           v_tcrpWgt(RMS,xx_all)                "weight for allocating OCRO and NECR to TCRP or FRUN"
           v_land(RMS,cols)                     "land use aggregates [kha] are not directly linked to products in market model"
           v_landTmp(RMS,cols)                  "prelim estimate of land use aggregates [kha] before imposing agricultural land balance"
           v_yield(RMS,XX_all)                  "yield [t/ha] is reciprocal of requirement of harvested area for products in market model"

           v_tradeFlows(RMall,RMall,XX_ALL)     "Imports matrix by origin [1000 tons]"
           v_tradeFlowNeg(RMall,RMall,XX_ALL)   "Import flows allowed tobe negative before fudging into the positive domain"
           v_transpCost(*,RMALL,*)              "Transport cost matrix [curr Euro/ton]"
           v_domSales(RM,XX_ALL)                "Domestic sales [1000 tons]"
           v_stockChg(RM,XX)                    "Stock changes [1000 tons]"

           v_prodQuantNeg(RMAll,XX1)            "Production, negative values allowed [1000 tons]"
           v_prodQuant(RMAll,Rows)              "Production [1000 tons]"

           v_consQuant(RMAll,XX1)              "Human consumption [1000 tons]"
           v_consQuantNeg(RMAll,XX1)           "Human consumption, negative values allowed [1000 tons]"
           v_caloPerCap(RMall)                  "Calory consumption per capita [cal/head]"
           v_commitPar(RMS,XX1)                 "Commitment parameter of Generalized Leontief Demand System"
           v_feedQuantNeg(RMAll,XX)             "Feed use, negative values allowed, [1000 tons]"
           v_feedQuant(RMAll,XX)                "Feed use [1000 tons]"
           v_feedBlk(RMS,FEED_TRD)              "Feed bulk, such as cereals, [1000 tons]"
           v_feedBlkPrice(RMS,FEED_TRD)         "Feed bulk price, [curr EU/ton]"
           v_feedShift(RMS)

           v_procQuantNeg(RMAll,XX1)            "Processing, negative values allowed [1000 tons]"
           v_procQuant(RMAll,XX1)               "Processing [1000 tons"

           v_biofFeedCost(RAll,ROWS)            "Per unit feedstock input costs argins for biofuel processing (input costs minus by-product revenues per unit of output)"
           v_biofPriceRel(RAll,XXBIOF)          "Price relation for biofuel (output value over input value)"
           v_prodBiof(RMAll,XX1)                "Production of Bio-fuels"
           v_biofProcQuant(RMAll,Rows)          "Processing to Bio-fuels from first generation technologies - Or biofuels in total fuel mix"
           v_consShareBioF(RMS,XXBioF)               "biofuel consumption share in total fuel consumption"



           v_arm1Quant(RM,XX)                   "Armington first stage aggregate, beware: uility points, in calibration point [1000 tons]"
           v_arm2Quant(RM,XX1)                  "Armington second stage aggregate, beware: uility points, in calibration point [1000 tons]"

           v_wldPrice(XX)                       "World market price"
           v_uvae(RM,XX)                        "Unit value exports"
           v_netTrade(RM,XX)                    "Net trade"

           v_arm1Price(RM,XX1)                  "Armington first stage price [Euro/utility point]"
           v_cSugarPrice(RMS)
           v_arm1Val(RM,XX1)                    "Armington first stage value [current Euro]"
           v_arm2Price(RM,XX)                   "Armington second stage price [Euro/uility point"
           v_arm2Val(RM,XX)                     "Armington second stage value [current Euro]"

           v_marketPrice(RAll,XX)               "Domestic maret price [Euro/ton], anchor price for producer/consumer prices"
           v_prodPrice(RMS,Rows)                "Producer price [Euro/ton]"
           v_prodPricePos(RMS,XX_all)           "Producer prices[Euro/ton]  (determining yields) fudged to the positive domain"
           v_consPrice(RMS,XX1)                 "Consumer price [Euro/ton]"
           v_consPriceNeg(RMS,XX1)              "Consumer price [Euro/ton] (before fudging to the positive domain)"
           v_caloTax(RMall)                     "Calory tax or subsidy [Euro/kcal]"
           v_priceFatProt(RMS,FatsProt)         "Milk fat resp. protein price, [Euro/ton]"
           v_procMarg(RAll,ROWS)                "Processing margins for milk products and oilseeds, [Euro/ton]"
           v_procYield(RAll,*)                  "Processing yields for oil and cakes from oilseed crushing [ton/ton]"
           pv_prodPriceMarg(RMS,XX)             "Producer prices margin"

           v_impQuant(RM,XX1)                   "Sum of imports [1000 tons]"
           v_expQuant(RAll,XX1)                 "Sum of exports [1000 tons]"
           v_nonDoubleZeroExports(RAll,XX)      "Sum of export out of bilateral preferences [1000 tons]"

           v_TRQImports(RMall,XX)                  "Sum of imports under a non allocated TRQ, [1000 tons]"
           v_valSubsExports(Rall,XX)            "Value of subdidised exports [Mio curr EU"
           pv_feedConv(RMS,XX_ALL,*)            "Feed conversion factors for animal products - fixed during solution of market model, [ton/ton]"
*
* --- intermediate variables of GL demand function
*
           v_GLDemandFS(RMS)
           v_GLDemandGS(RMS)
           v_GLDemandGiS(RMS,XX1)
*
* --- endogenous MCP variables (if not fixed)
*
           v_tarSpec(*,*,*)                         "Endogenous specific tariff, fixed or under TRQ/min. boder price regime, [curr Euro/ton]"
           v_tarAdval(*,*,*)                        "Endogenous ad valorem tariff, fixed or under TRQ/min. boder price regime, [%]"
           v_trqSigmoidFunc(*,*,*)                  "Functional value of sigmoid function driving TRQs"

           v_flexLevyNotCut(*,*,*)                  "Difference between import and minimum import price"
           v_flexLevy(*,*,*)                        "Specific tariff under minimum border price regmine, [current Euro/ton]"
           v_entryPrice(*,*,*)                      "Reduction of specific tariffs in case of flexible levies"
           v_entryPriceDriver(*,*,*)                "Reduction of specific tariffs in case of flexible levies"

* --- intervention system
           v_probMarketPriceUnderSafetyNet(RMALL,XX) "Probability of market prices to undercut adminstrative ones"
           v_unitValueExports(RMall,XX)                "Unit value exports [curr Euro/ton]"
           v_buyingToIntervStock(RMall,XX)           "Building up of intervention stocks [1000 tons]"
           v_releaseFromIntervStock1(RMALL,XX)           "Difference between market prices and adjusted unit value"
           v_releaseFromIntervStock(RMall,XX)        "Releases from intervention stocks [1000 tons]"
           v_intervStockChange(RMall,XX)             "Change in intervention stocks [1000 tons]"
           v_intervStockLevel(RMall,XX)              "Intervention end stocks [1000 tons"
           v_shareBuying(RMall,XX)                  "RM shares of intervention buy-ins"
           v_shareRelease(RMall,XX)                  "RM shares of intervention releases"

           v_impPrice(RM,RM1,XX)                    "Import price for goods from RM1 in RM after border, including tariffs, [curr Euro/ton]"
           v_impShadowPrice(RM,RM1,XX)              "Shadow import price in case of nonzero fixed import flows"
           v_cifPrice(RM,RM1,XX)                    "CIF import price for goods from RM1 in RM [Euro/ton]"
           v_procMarginSubg(RMALL,ABC,*)

           v_perUnitExportSub(RMall,XX)
           v_dummy              "Dummy objective value to use NLP"
           SSQ                  "Sum of squares to minimise"
*
* --- trimming of subsidised exports and to intervention stocks
*
           pv_bevFuncSubsExpCorrFact(RMall,XX)
           pv_bevFuncIntCorrFac(RMall,*)
           pv_bevFuncIntAddFac(RMall,*)
           pv_bevFuncIntMultFac(RMall,*)

* --- calibration parameters of the intervention release/buying share equations (constant terms)
           pv_constShareBuying(RMALL,XX)
           pv_constShareRelease(rmall,xx)

           pv_netTrade(RM,XX,abcd)

           pv_sigmParSubsExports(RMall,*,XX) "Parameter steers logistic function for subsidized exports"


*
*   ---    Variables for the modified Armington approach(es)
*
           v_gammaCES(rmall, xx)                    "CES level parameter"
           pv_theta(rmall, rmall, xx)               "Hicks non-neutral pref. change parameter (Armington shifter)"
           v_Hicks(rmall, xx)                       "Hicks neutral pref. change parameter"

      ;

*
* WB: 13.01.14: intermediate solution until the statement can me moved to arm\data_cal.gms in front of
*               arm\store_prices
*
 option kill=v_biofPriceRel;

 parameter

           p_dpCESTrade(RM,RmAll,XX)               "CES distribution parameter"
           p_dpCESBiof(RMS,XX)                     "CES distribution biofuels"
           p_dpCESFeed(RMS,XX)                     "Distribution parameter of CES feed"
 ;

 set XX_no_prodNQ_equation(XX1,RALL) "Product without a NQ level equation in the market model" /
                                         (DDGS,SET.XXBIOF,SET.CAK,SET.OIL,set.xxOmYAni).(set.RMS) /;
*

 set NonZeroFixedImportFlow(RM,RM1,XX) "Nonzero fixed import flows" ;
 option kill = NonZeroFixedImportFlow;

* --- some additional products without NQ production equation for the regions with supply models
*
 xx_no_prodNQ_equation(MLK,RMS)       = YES;
 xx_no_prodNQ_equation("MILK",RMS)    = NO;
 xx_no_prodNQ_equation("FPRI",RMSSUP) = YES;
 xx_no_prodNQ_equation("FENI",RMSSUP) = YES;
 xx_no_prodNQ_equation("FEDE",RMSSUP) = YES;

 set XX_no_expQuant_equ(XX1,RALL) "Products where the export quantity equation is switched off (during trim_trqs)" ;
 XX_no_expQuant_equ(XX1,RALL) = no;
 set XX_no_impQuant_equ(XX1,RALL) "Products where the import quantity equation is switched off (during trim_trqs)" ;
 XX_no_impQuant_equ(XX1,RALL) = no;
*
* --- Scenarios with global emission taxes (via negative PSEI) may create negative prices for some products.
*     This is allowed for feasibility in widen_bounds.gms (technically negative prices do not create trouble with linear functions)
*     The set may be assigned on demand in scenario files
 set negProdPriceAllowed(RMS,rows)    "Combinations of regions and products where negative producer prices are allowed";
 option kill = negProdPriceAllowed;

* --- Scenarios with calorie taxes or subsidies may create negative consumer prices for some products.
*     This is allowed for feasibility on an intermediate price variable v_consPriceNeg
*     The set may be assigned on demand in scenario files
 set negConsPriceAllowed(RMS,rows)    "Combinations of regions and products where negative consumer prices are allowed";
 option kill = negConsPriceAllowed;


* --- Scenarios with calorie taxes or subsidies may create negative consumer qunatities for some products.
*     This is allowed for feasibility on an intermediate price variable v_consQuantNeg
*     The set may be assigned on demand in scenario files
 set negConsQuantAllowed(xx)    "Combinations of regions and products where negative consumer quantities are allowed"
$ontext
 set negConsQuantAllowed(rows)    "Combinations of regions and products where negative consumer quantities are allowed"
   /
   SET.XX_CROPS
   SET.MLK
   SET.OIL
   SET.XMEAT
     /
$offtext
     ;
 option kill = negConsQuantAllowed;


 EQUATIONS

*    --------- behavioural functions

           ProcNQ_(RMS,XX)               "NQ function processing"
           ProcO_(RMS,XX)                "Output of oil and cake"
           Biof_(RMS,XX)                 "CES share equations for biofuel processing demand"
           prodQuantBiofBy_(RMS,bio_by)   "Output from DGGS and Glycerin from bio-fuel production"
           MaprFeed_(RMS,RESIMPX)        "Output of feed by products from milling and processing"
           procYield_(RMS,XX)            "Outout of oil and cake"
           stockChg_(RM,XX)             "Behavioural equation for stock changes"

           Proda_(RM,XX)                 "Aggregation to trade blocks for production"
           FeedUsea_                     "Aggregation to trade blocks for feed use"
           HCona_                        "Aggregation to trade blocks for human consumption"
           Proca_                        "Aggregation to trade blocks for processing"
           Biofa_                        "Aggregation to trade blocks for processing to bio-fuels"


           GlDemandFS_(RMS)              "Part of GL demand function"
           GLDemandGS_(RMS)              "Part of GL demand function"
           GLDemandGiS_(RMS,XX)          "Part of GL demand function"
           XiS_(RMS,XX)                  "Part of GL demand function"
           ConsFudge_(RMS,XX)            "Fudging on positive range"
           caloPerCap_(RMall)            "Average calory consumption per capita"
*
           ProdNQ_(RMS,XX1)              "Supply function from NQ profit function"
           ProdFudge_(RMS,XX1)           "Ensure positive production"

           LandSupply_(RMS)              "Land supply function"
           LandAgrFromXX_(RMS,cols)      "Aggregating different types of agric land from land requirements by product"
           Yield_(RMS,XX_all)            "Yield responds to producer price"
           LandTypesNagrTmp_(RMS,cols)   "Prelim estimate for foreOlndArtif landtypes before imposing country land balance"
           LandTypesNagr_(RMS,cols)      "Estimate for foreOlndArtif landtypes after imposing land balance"
           ProdPriceFudge_(RMS,rows)     "Fudging function for producer prices determining yields"
           LandAgrTmp_(RMS,XX_all)       "Prelim estimate of land use by product before imposing agricultural land balance"
           LandAgr_(RMS,XX_all)          "Estimate of land use by product after imposing agricultural land balance"
           LandNagr_(RMS)                "Land available for nonagricultural land uses"

           yaniMarket_(RM,xxOmYani)

           FeedNQ_(RMS,XX)               "Feed demand from NQ feed function"
           FeedFudge_(RMS,XX)            "Ensure positive feed demandn"

           FeedBal_(RMS)                 "Feed shift"
           feedBlk_(RMS,FEED_TRD)        "Feed bulk definition"
           FeedShift_(RMS)
           feedBlkPrice_(RMS,FEED_TRD)   "Feed shift for bulk, for regions linked to supply models"
           FeedShareEqu_(RMS,XX)         "Feed demand derived from CES"
           DairyNQ_(RMS,MLK)             "Normalised quadratic dairy production function"
           ProcMargM_(RMS,MLK)           "Definition of processing margin for dairy products"
           ProcMargO_(RMS,SED)           "Definition of processing margin for oilseeds"
           ProcFudge_(RMS,XX)            "Ensure positive processing"

*          --- policy presentation


           Levies_(RM,RM1,XX)              "Ensure minimum import price"

           tarSpec_(RMall,RMall1,XX)             "Specific tariff as function of imports and TRQ"
           prefTriggerPrice_(RMall,RMall,XX)     "Preferential trigger price under TRQ"
           tarSpecIfEntryPrice_(RMall,RMall1,XX) "Specific tariff as function of import price for fruits and vegs"
           EntryPriceDriver_(RMall,RMall1,XX)    "Specific tariff as function of import price for fruits and vegs"
           FlexLevy_(RMall,RMall1,XX)            "Difference between bound rate applied and minimum import price"
           FlexLevyNotCut_(RMall,RMall1,XX)      "Ensures that levies don not exceed tariffs in calibration point"
           tarSpecW_(RMall,*,XX)              "Specific tariff as function of imports and TRQ"
           tarAdval_(RMall,RMall1,XX)            "Ad valorem tariff as function of imports and TRQ"
           tarAdValW_(RMall,*,XX)             "Ad valorem tariff as function of imports and TRQ"
           trqSigmoidFunc_(*,*,*)          "Functional value of sigmoid function driving TRQs"

           TRQImports_(RMall,XX)              "Sum of imports under a non allocated TRQ"


           unitValueExports_(RMall,XX)                  "unit value exports"


* --- intervention system
           buyingToIntervStock_(RMall,XX)              "intervention stock purchases"
           probMarketPriceUnderSafetyNet_(RMall,XX)    "probablity of market price to undercut adminstrative one"
           releaseFromIntervStock1_(RMall,XX)          "release of interventions stocks"
           releaseFromIntervStock_(RMall,XX)           "release of interventions stocks"
           intervStockLevel_(RMall,XX)                 "intervention stock end size"
           intervStockChange_(RMall,XX)                "change in size of intervention stocks"

           buyingRM_(RMall,XX)                         "intervention buy-in for trading blocks, in case the RMTP level is active"
           buyingRMNum_(RMall,XX)                      "intervention buy-in for trading blocks (for numeraire country), in case the RMTP level is active"
           releaseRM_(RMall,XX)                        "intervention releases for trading blocks, in case the RMTP level is active"
           releaseRMNum_(RMall,XX)                     "intervention releases for trading blocks (for numeraire country), in case the RMTP level is active"
           stockChangeRM_(RMall,XX)                    "intervention stock changes for trading blocks, in case the RMTP level is active"
           buyingShares_(RMALL,XX)                     "share equations for intervention buy-ins"
           releaseShares_(RMALL,XX)                    "share equations for intervention releases"

           EXPs_                   "Behavioural function subsidised exports"
           valSubsExports_         "Value of subsidised exports before cut"
           EU27Exports_            "Exports from EU27 into nonEU in calibration point"

*          --- biofuel equations

           Biof_(RMS,XX)             "CES share equations for biofuel processing demand"
           CSugarPrice_(RMS)
           ProdBiof_(RMS,Rows)       "Output of first generation biofuels"
           MaprBiof_(RMS,XXBioF)     "Total output of biofuels"
           SupplyBiof_(RMS,XXBioF)   "Supply function for biofuels wrt processing margins"
           BiofPriceRel_(RMS,XXBIOF)   "Price relation for biofuel (output price over input costs)"
           BiofFeedCost_(RMS,Rows)     "processing margin of biofuel production per feedstock"
           BiofFeedCostAgg_(RMS,Rows)  "Aggregated processing margin of biofuel production per fuel"
           biofDemshare_               "share of Biofuel demand in total fuel demand"
           biofDem_                    "Total Biofuel Demand"

*          --- market and other balances, Armington system



           TradeFlowsAgg_(RMall,RMall1,XX) "trade flows aggregator at policy block level"



           wldMarket_(XX)              "World market balance"
           nTrd_(RM,XX)                "Behavioural function net trade"
           arm1PNetTrade_(RM,XX)       "Arm1Price in case of net trade model"


           ImpQuant_(RM,XX)            "Sum of imports of each region"
           expQuant_(RM,XX)            "Sum of exports of each region"
           nonDoubleZeroExports_       "Sum of exports of each region excluding bil. free trade"
           uvae_(RM,XX)                "Definition of unit value exports"

           FatsProtBal_(RMS,FatsProt)  "Fat and protein balance for countries with supply model representation"


           ArmFit1_(RM,XX)             "Quantity aggregation Armington first stage in base year"
           mrkBal_(RM,XX)              "Quantity aggregation Armington first stage in net trade modelr"
           ArmFit2_(RM,XX)             "Quantity aggregation Armington second stage in base year"

           ArmBal1_(RM,XX)             "Adding up of human consumption feed and processing"
           SupBalM_(RM,XX)             "Supply balance of all individual countries"

           arm2QuantShares_(RM,XX)     "Share equation for arm2Quant in total demand, CGE style"
           domSalesShares_(RM,XX)      "Share equation for doemstic sales in total demand, CGE style"
           importShares_(RM,RM1,XX)    "Share equations for imports flows in total imports, CGE style"
           tradeFlowFudge_(RM,RM1,XX)  "Fudging function for (potentially nregative) trade flows with Armington committments"

*          --- price linkage

           impPrice_(RM,RM1,XX)        "Definition of import price"
           cifPrice_(RM,RM1,XX)        "Definition of import price without flexible levy"

           arm1Price_(RM,XX)           "Price index first stage Armington"
           arm1PriceAgg_(RM,XX)        "Price index first stage Armington"
           arm1Val_(RM,XX)             "Value of domestic sales plus imports"
           arm2Price_(RM,XX)           "Import price index from rest of world"
           arm2PriceAgg_(RM,XX)        "Import price index from rest of world"
           arm2Val_(RM,XX)             "Value of imports"

           CPri_(RMS,XX)               "Consumer price linked with fixed margin to first stage price"
           consPriceFudge_(RMS,XX)     "Consumer price fudging function in case that cpri_ generates negative prices"
           PMrk_(RM,XX)                "Market price if no production"
           PPri_(RMS,XX)               "Producer price inside aggregates"


          MarketPriceAgg_(RMall,XX)     "market price aggregator at policy block level"
          TransportCostsAgg_(RMall,RMall,XX) "transport cost aggregator at policy block level (only in calibration)"

          wldPrice_(XX)                 "World market price definition (only in calibration)"

*          --- dummy objective and penalty definition of data calibration

           FLIPFLOP_
           NSSQ_                       "Normalised least squares definition for base year calibration"
           NSSQ1_                      "Normalised least squares definition for sim year calibration"

*
*   ---    Additional equations for the Kuiper van-Tongeren approach
*
          HicksNeutral_(rmall, xx)        "equation combining the Hicks-neutral pref. change parameter and the CES level parameter"
          CESAugmented_(rmall, xx)        "CES aggregator for the KVT approach (augmented with techn. change parameters)"
          transition_(rmall, rmall, xx)   "Transition function for the extended KVT approach"

           ;



    POSITIVE VARIABLES v_tarSpec,v_tarAdVal,v_transpCost,v_tradeFlows,v_consPrice,
                       v_prodPrice,v_marketPrice,v_arm1Price,v_Arm2Price,v_arm2Val,v_arm1Val
                       v_perUnitExportSub,
                       v_buyingToIntervStock,v_releaseFromIntervStock,
                       v_landAgr,v_landAgrTmp,v_tcrpWgt,v_land,v_landTmp,v_yield,v_prodPricePos
                       ;
*
* --------------- QUANTITIES -----------------------------------------------------------------------

*
* --------------- Behavioural functions
*$onlisting
*
* --- definition of F as sum of Di multiplied with prices for the Generalised Leontief expenditure funtion
*                      PD: commitments / linear terms in the individual demand functions
*                      (in kg per capita, therefore prices are expressed in Euro/kg <=> CPRI [Euro/ton] * 0.001)
*
 GLDemandFS_(RMS) $ (  sum(xxx $ (v_consQuant.range(RMS,XXX) or xxFreeCommitments(rms,xxx)),1))..
*
     v_GLDemandFS(RMS) /
*
*         --- the following term (also found o the RHS) is only for scaling purposes
*             to bring the equation around unity
*
          (SUM(XX1 $ p_pdGL(RMS,XX1,"CUR"),
                             DATA(RMS,"CPRI",XX1,"CUR") * p_pdGL(RMS,XX1,"CUR")*1.E-3) + 0.1)

      =E= SUM(XX1 $ p_pdGl(RMS,XX1,"CUR"),
                             v_consPrice(RMS,XX1)
                          * ( p_pdGL(RMS,XX1,"CUR") $ (not xxFreeCommitments(rms,xx1))
                            + v_commitpar(RMS,XX1)  $ (    xxFreeCommitments(rms,xx1)))*1.E-3)
        / (SUM(XX1 $ p_pdGL(RMS,XX1,"CUR"),
                             DATA(RMS,"CPRI",XX1,"CUR") * p_pdGL(RMS,XX1,"CUR")*1.E-3) + 0.1);
*
* --- definition of function G for the Generalised Leontief expenditure funtion
*      (per capita)
*
 GLDemandGS_(RMS) $ (  sum(xxx $ (v_consQuant.range(RMS,XXX) or xxFreeCommitments(rms,xxx)),1))..
*
*    --- due to DF in the calibration of the parameter set, it is possible
*        to fix v_GLDemandGS durign a value. That value is 10000. and
*        use in the equation for scaling to bring the equation to unity in the
*        calibration point
*
     v_GLDemandGS(RMS)/10000. =E= SUM( (XX1,YY1) $ p_pbGL(RMS,XX1,YY1,"CUR"),
                                        p_pbGL(RMS,XX1,YY1,"CUR")
                                          * SQRT(v_consPrice(RMS,XX1)*v_consPrice(RMS,YY1)*1.E-6) )/10000.;
*
* --- definition of first derivatives of G called Gi for the Generalised Leontief expenditure funtion (per capita)
*     Note: this term enters the demand function that serves to define v_consQuant or v_commitPar
*
 GLDemandGiS_(RMS,XXX) $ (  (v_consQuant.range(RMS,XXX) or xxFreeCommitments(rms,xxx))
                          $ DATA(RMS,"HCon",XXX,"CUR"))  ..

     v_GLDemandGis(RMS,XXX)

*
*                --- the following is only a scaling term to bring the equation around unity
*                    in  calibration point. It is also found on the RHS
*
                 /   (SUM( YY1 $ p_pbGl(RMS,XXX,YY1,"CUR"),
                                 p_pbGL(RMS,XXX,YY1,"CUR")
                                   * SQRT(DATA(RMS,"CPRI",YY1,"CUR")/DATA(RMS,"CPRI",XXX,"CUR"))) + 0.1)

                 =E= [SUM( YY1 $ p_pbGl(RMS,XXX,YY1,"CUR"),
                                     p_pbGL(RMS,XXX,YY1,"CUR")
*
*                                  ---- the division / multiplication by DATA(RMS,"CPRI",XXX,"CUR")
*                                       cancel out and are only introduced to stabilize
*                                       the numerical solution
*
                                   * SQRT(v_consPrice(RMS,YY1)/DATA(RMS,"CPRI",XXX,"CUR")))
                                   * SQRT(DATA(RMS,"CPRI",XXX,"CUR")/v_consPrice(RMS,XXX))
                     ]
*
                 /   (SUM( YY1 $ p_pbGl(RMS,XXX,YY1,"CUR"),
                                 p_pbGL(RMS,XXX,YY1,"CUR")
                                   * SQRT(DATA(RMS,"CPRI",YY1,"CUR")/DATA(RMS,"CPRI",XXX,"CUR"))) + 0.1);
*
* ----- definition of human consumption for the Generalised Leontief expenditure funtion
*       (or indirectly if v_consQuant is fixed: implicit definition of v_commitPar)
*
 XiS_(RMS,XXX)  $ (  (v_consQuant.range(RMS,XXX) or xxFreeCommitments(rms,xxx))
                   $ DATA(RMS,"HCon",XXX,"CUR")) ..
*
     (  v_consQuant(RMS,XXX)    $ ( v_consQuant.LO(RMS,XXX) GT 0)
      + v_consQuantNeg(RMS,XXX) $ ( v_consQuant.LO(RMS,XXX) LE 0)
     )


     /(DATA(RMS,"HCon",XXX,"CUR")+0.1) =E=
*
       ((v_GLDemandGis(RMS,XXX)/v_GLDemandGS(RMS)
           * ( DATA(RMS,"Ince","Levl","CUR")/DATA(RMS,"INHA","LEVL","CUR") - v_GLDemandFS(RMS))
*              + p_pdGL(RMS,XXX,"CUR")
              + ( p_pdGL(RMS,XXX,"CUR") $ (not xxFreeCommitments(rms,xxx))
                + v_commitpar(RMS,XXX)  $ (    xxFreeCommitments(rms,xxx))) )
      * DATA(RMS,"INHA","LEVL","CUR")/1000.)/(DATA(RMS,"HCon",XXX,"CUR")+0.1);

 consFudge_(RMS,XXX) $ (     (v_consQuant.lo(RMS,XXX) LE 0)
                           $ (v_consQuant.LO(RMS,XXX) ne v_consQuant.UP(RMS,XXX))
                           $ DATA(RMS,"Hcon",XXX,"CUR")) ..
*
* --- Fisher-Baumeister smoothing function (sqrt(sqr(x) + sqr(y) + 2*z)
*     in case Human consumption gets negative
*
     v_consQuant(RMS,XXX)
       /(abs(DATA(RMS,"Hcon",XXX,"CUR"))+0.1)
        =E=  (-ncpcm(-v_consQuantNeg(RMS,XXX),0,1.E-2*min(1000,max(1.E-11,data(RMS,"Hcon",XXX,"CUR")))) + 1.E-11)
      /(abs(DATA(RMS,"HCon",XXX,"CUR"))+0.1);


* --- Average per capita calory consumption in larger (RMTP) regions (like the whole EU)
*     NOTE: Regional definition at RMTP level to conveniently target EU, benefiting from existing cross-sets.
*           The specification of a common target for MER is an undesired side effect, accepted under SUSFANS
*
 caloPerCap_(RMS) $ (not p_lstPreStep) ..
*
     v_caloPerCap(RMS)

     =E=  sum( XX  , ( v_consQuant(RMS,XX)
*                                               (HCOM+LOSF).(COMI+SGMI) need to be added to HCON.MILK (empty so far!) for RMSSUP,
*                                                because direct sales and subsistence consumption are not booked into FRMI in COCO/CAPTRD:
                                              + sum(mlkO,DATA(RMS,"HCON",mlkO,"CUR")) $ sameas(XX,"MILK"))
                                              * max(0,1-sum(shareInHcon,DATA(RMS,shareInHcon,XX,"CUR"))
*                                               Note: BARL is used for beer making - ends up in final human consumption
                                                       +                DATA(RMS, "INDMsh", XX, "CUR") $sameas(XX, "BARL"))
                                              * p_calContent(XX) )
         /(DATA(RMS,"INHA","LEVL","CUR")/1000.) / 365    ;


* --- Shift of feed demand function according to changes in total energy demand from animal production
*
 FeedShift_(RMSSUP) ..

*
     v_feedShift(RMSSUP) =E=
*
*                     --- total energy requirement in feed from current production levels
*
       [
              (   (     SUM( YY $ DATA(RMSSUP,"Prod",YY,"CUR"), v_prodQuant(RMSSUP,YY) * pv_feedConv(RMSSUP,YY,"CUR"))
*
*                     --- total energy requirement in feed from production levels in calibration point
*
                       /SUM( YY,  DATA(RMSSUP,"Prod",YY,"CUR") * pv_feedConv(RMSSUP,YY,"CUR")))
               )
                 $ sum( YY,  DATA(RMSSUP,"Prod",YY,"CUR") * pv_feedConv.l(RMSSUP,YY,"CUR"))

             + 1 $ (not sum( YY,  DATA(RMSSUP,"Prod",YY,"CUR") * pv_feedConv.l(RMSSUP,YY,"CUR")))

       ]
      /  (          SUM(XX $ DATA(RMSSUP,"Feed",XX,"CUR"),
                           v_feedQuant(RMSSUP,XX)       * p_calContent(XX))
                /  (SUM(XX $ DATA(RMSSUP,"Feed",XX,"CUR"),
                           DATA(RMSSUP,"feed",XX,"CUR") * p_calContent(XX))));

*
* --- Determine price index for bulks v_feedBlkPrice for regions with supply models
*     (= average price of ingredients)
*
 feedBlkPrice_(RMSSUP,FEED_TRD) $ SUM(FEED_TO_XX(FEED_TRD,XXX) $ DATA(RMSSUP,"FEED",XXX,"CUR"), 1) ..


  v_feedBlkPrice(RMSSUP,feed_trd)
          / SUM(FEED_TO_XX(FEED_TRD,XX) $ DATA(RMSSUP,"FEED",XX,"CUR"), sum(RMS_TO_RM(RMSSUP,RM), DATA(RM,"Arm1P",XX,"CUR")+1))
          * SUM(FEED_TO_XX(FEED_TRD,XX) $ DATA(RMSSUP,"FEED",XX,"CUR"), 1)

   =E= SUM(FEED_TO_XX(FEED_TRD,XX) $ DATA(RMSSUP,"FEED",XX,"CUR"),
                                           p_dpCESFeed(RMSSUP,XX)
         * (  sum(RMS_TO_RM(RMSSUP,RM),v_arm1Price(RM,XX))
               / SUM(YY $ (FEED_TO_XX(FEED_TRD,YY) $ DATA(RMSSUP,"FEED",YY,"CUR")), sum(RMS_TO_RM(RMSSUP,RM), DATA(RM,"Arm1P",YY,"CUR")+1))
                * SUM(YY $ (FEED_TO_XX(FEED_TRD,YY) $ DATA(RMSSUP,"FEED",YY,"CUR")), 1)
            )
                                           **(1-p_rhoFeed(RMSSUP,FEED_TRD)))
                                                ** (1/(1-p_rhoFeed(RMSSUP,FEED_TRD)));

*
* --- Behavioural equation for demand for feed bulks with an elasticity determined from the
*     parameterization of the supply model
*
 feedBlk_(RMSSUP,FEED_TRD) $ ( SUM(FEED_TO_XX(FEED_TRD,XXX) $ DATA(RMSSUP,"FEED",XXX,"CUR"), 1) ) ..

     log(v_feedBlk(RMSSUP,FEED_TRD))
              / log((SUM(FEED_TO_XX(FEED_TRD,XX), DATA(RMSSUP,"FEED",XX,"CUR")) + 0.1))

          =E= (
*
*              ---- effect of own and cross prices of bulkd (FCER,FPRO ...)
*
               sum(FEED_TRD1 $ p_feedBlkElas(RMSSUP,FEED_TRD,FEED_TRD1),
                          p_feedBlkElas(RMSSUP,FEED_TRD,FEED_TRD1)
                                * log(v_feedBlkPrice(RMSSUP,FEED_TRD1)))
*
*              ---- cross effects with other prices in supply model
*
              +sum(XX $ (p_feedBlkElas(RMSSUP,FEED_TRD,XX) $ data(rmssup,"prod",xx,"cur")),
                          p_feedBlkElas(RMSSUP,FEED_TRD,XX)
                                * log(max(1.E-3,v_prodPrice(RMSSUP,XX))))

                                 + p_feedBlkCnst(RMSSUP,FEED_TRD))

              / log((SUM(FEED_TO_XX(FEED_TRD,XX), DATA(RMSSUP,"FEED",XX,"CUR")) + 0.1));

*
* --- Feed demand in share form derived from CES for countries comprised in the Supply part
*
 feedShareEqu_(RMSSUP,XXX) $ ( (v_feedQuant.LO(RMSSUP,XXX) ne v_feedQuant.UP(RMSSUP,XXX))
                                    $ DATA(RMSSUP,"Feed",XXX,"CUR") ) ..

     v_feedQuant(RMSSUP,XXX)/(abs(DATA(RMSSUP,"Feed",XXX,"CUR"))+0.1)

              =E= sum(FEED_TO_XX(FEED_TRD,XXX),

                  v_feedBlk(RMSSUP,FEED_TRD)/(abs(DATA(RMSSUP,"Feed",XXX,"CUR"))+0.1)

                 * p_dpCESFeed(RMSSUP,XXX)

*   ... times ratio of output price to factor price plus tax, to the power of the substitution elasticity

                 * [ v_feedBlkPrice(RMSSUP,FEED_TRD) / sum(RMS_TO_RM(RMSSUP,RM),v_arm1Price(RM,XXX)) ] ** p_rhoFeed(RMSSUP,FEED_TRD));
*
* --- Equilibrate energy need for tradable feed from animal products
*     with enery deliveries from feed. Drives the feed price "FEDE"
*     which is net put in the supply function prodNQ_
*
 FeedBal_(RMS) $ ( XXX1("FEDE") $ (not RMSSUP(RMS)) ) ..

       SUM(XX $ DATA(RMS,"Feed",XX,"CUR"),
           v_feedQuant(RMS,XX) * p_calContent(XX))
       /  (SUM(XX $ DATA(RMS,"Feed",XX,"CUR"),
               DATA(RMS,"feed",XX,"CUR") * p_calContent(XX))+0.1)

         =E= SUM( XX_ANIM_FEED $ DATA(RMS,"PROD",XX_ANIM_FEED,"CUR"),
                v_prodQuant(RMS,XX_ANIM_FEED) * pv_feedConv(RMS,XX_ANIM_FEED,"CUR"))
          /  (SUM(XX $ DATA(RMS,"Feed",XX,"CUR"),
                DATA(RMS,"feed",XX,"CUR") * p_calContent(XX))+0.1);

*
* --- Normalised quadratic feed cost function
*
 FeedNQ_(RMS,XX) $ (   (v_feedQuant.LO(RMS,XX) ne v_feedQuant.UP(RMS,XX))
                     $ (XXX(XX) or XXX1("FEDE"))
                     $ (NOT RMSSUP(RMS))
                     $ DATA(RMS,"Feed",XX,"CUR") ) ..

     ( v_feedQuantNeg(RMS,XX) $ ( v_feedQuant.LO(RMS,XX) LE 0)
 +     v_feedQuant(RMS,XX)    $ ( v_feedQuant.LO(RMS,XX) GT 0)) / (abs(DATA(RMS,"Feed",XX,"CUR"))+0.1)
*
*                 --- constant term of NQ function, p_hessNQFeed: symmetric Hessian
*
             =E=   ( p_cnstNQFeed(RMS,XX)
                    + SUM( (RMS_TO_RM(RMS,RM),YY1) $ ( DATA(RM,"arm1P",YY1,"CUR") or sameas(YY1,"FEDE")),
                                   p_hessNQFeed(RMS,XX,YY1,"CUR")
                                      * (  (v_arm1Price(RM,YY1)-DATA(RMS,"FSEi",YY1,"CUR")) $ (not sameas(YY1,"Fede"))
                                         + v_prodPrice(RMS,"Fede") $ sameas(YY1,"FEDE"))/v_arm1Price(RM,"Inpe")))
                   /  (abs(DATA(RMS,"Feed",XX,"CUR"))+0.1);


*
* --- Ensure positive feed (NQ function may generate negative quantities)
*
 FeedFudge_(RMS,XX) $ (      (v_feedQuant.lo(RMS,XX) LE 0)
                           $ (XXX(XX) or XXX1("FEDE"))
                           $ (v_feedQuant.LO(RMS,XX) ne v_feedQuant.UP(RMS,XX))
                           $ DATA(RMS,"Feed",XX,"CUR")) ..
*
*    --- Fisher-Baumeister smoothing function (sqrt(sqr(x) + sqr(y) + 2*z)
*
     v_feedQuant(RMS,XX)/(abs(DATA(RMS,"Feed",XX,"CUR"))+0.1)
        =E=  (-ncpcm(-v_feedQuantNeg(RMS,XX), 0,1.E-2*min(1000,max(1.E-11,data(RMS,"Feed",XX,"CUR"))))  + 1.E-11)
                        /(abs(DATA(RMS,"Feed",XX,"CUR"))+0.1);
*
* --- NQ dairy net put function for processed product aggregates,
*     Depend for dairy products on processing margins (= difference
*     between selling price and price for milk fat and other milk solids)
*
 DairyNQ_(RMS,MLK) $ ( (xxx(MLK) or (sameas(MLK,"MILK") $ sum(YYY(MLK1),1)) )
                          $ (v_prodQuant.lo(RMS,MLK) ne v_prodQuant.up(RMS,MLK))
                                      $ DATA(RMS,"Prod",MLK,"CUR"))..

     ( v_prodQuantNeg(RMS,MLK) $ ( v_prodQuant.LO(RMS,MLK) LE 0)
 +     v_prodQuant(RMS,MLK)    $ ( V_prodQuant.LO(RMS,MLK) GT 0) )
                                                              / (DATA(RMS,"Prod",MLK,"CUR")+0.1)

        =E= (p_cnstNQDairy(RMS,MLK)

           + SUM( YY $ (   DATA(RMS,"PPri",YY,"CUR")     $ (NOT MLK(YY))
                         + DATA(RMS,"ProcMarg",YY,"CUR") $ MLK(YY)),
                             p_hessNQDairy(RMS,MLK,YY,"CUR")
              * (   v_prodPrice(RMS,YY)  $ (NOT MLK(YY))
                  + v_procMarg(RMS,YY)   $  MLK(YY)      ) /v_prodPrice(RMS,"Inpe")))
                                                             / (DATA(RMS,"Prod",MLK,"CUR")+0.1);

*
* --- Processing margin for dairy products: producer price minus value of fat and protein content
*
 ProcMargM_(RMS,MLK) $ (SUM(YY, ABS(p_hessNQDairy(RMS,MLK,YY,"CUR")))
                           $ (XXX(MLK) or (sameas(MLK,"MILK") $ sum(YYY(MLK1),1)) )) ..
*
     v_procMarg(RMS,MLK)/(abs(DATA(RMS,"ProcMarg",MLK,"CUR"))+1)
      =E= (  v_prodPrice(RMS,MLK)
           - DATA(RMS,"Fats",MLK,"CUR")*v_priceFatProt(RMS,"Fats")
           - DATA(RMS,"Prot",MLK,"CUR")*v_priceFatProt(RMS,"Prot")  )
         /(abs(DATA(RMS,"ProcMarg",MLK,"CUR"))+1);
*
* --- Fat and protein balance between raw milk and processed product aggregates
*
 FatsProtBal_(RMS,FatsProt) $ ( (v_priceFatProt.lo(RMS,FatsProt) ne v_priceFatProt.up(RMS,FatsProt))
                                $ (sum(XXX(MLK),1) ge 1))..

     (v_prodQuant(RMS,"MILK") * DATA(RMS,FatsProt,"MILK","CUR"))
         / (DATA(RMS,"PROD","MILK","CUR")* DATA(RMS,FatsProt,"MILK","CUR")+1)
           =E= SUM(MLK $ (NOT SAMEAS(MLK,"MILK")),
                     (  v_prodQuant(RMS,MLK) $ DATA(RMS,"Prod",MLK,"CUR")
                      - v_procQuant(RMS,MLK) $ DATA(RMS,"Proc",MLK,"CUR"))
                                                               * DATA(RMS,FatsProt,MLK,"CUR"))
         / (DATA(RMS,"PROD","MILK","CUR")* DATA(RMS,FatsProt,"MILK","CUR")+1);

*
* --- Processing margin for oilseeds: value of cake plus oil per ton of oilseed minus price for oilseed
*                                     Leontief-styled "PrcY" processing yields
*
 ProcMargO_(RMS,SED) $ (
                             (v_procMarg.LO(RMS,SED) NE v_procMarg.UP(RMS,SED))
                          $  (    SUM( SED_TO_CAK(SED,CAK), DATA(RMS,"Prod",CAK,"CUR"))
                               or SUM( SED_TO_OIL(SED,OIL), DATA(RMS,"Prod",OIL,"CUR")))
                          $ DATA(RMS,"Proc",SED,"CUR")
                          $ XXX(SED)) ..
*
     v_procMarg(RMS,SED)/(DATA(RMS,"PPRI",SED,"CUR")+1) =E=

         (SUM(RMS_TO_RM(RMS,RM), -v_Arm1Price(RM,SED))

         + SUM( SED_TO_CAK(SED,CAK) $ DATA(RMS,"Prod",CAK,"CUR"),
                   v_prodPrice(RMS,CAK) * (   (v_prodQuant(RMS,CAK)/v_procQuant(RMS,SED)) $ p_trim
                                            +  v_procYield(RMS,CAK)                      $ (NOT p_trim)))

         + SUM( SED_TO_OIL(SED,OIL) $ DATA(RMS,"Prod",OIL,"CUR"),
                   v_prodPrice(RMS,OIL) * (   (v_prodQuant(RMS,OIL)/v_procQuant(RMS,SED)) $ p_trim
                                             +  v_procYield(RMS,OIL)                      $ (NOT p_trim))))
                       /(DATA(RMS,"PPRI",SED,"CUR")+1);
*
* --- Processing function from NQ profit function
*
 ProcNQ_(RMS,XXX) $ ( (v_procQuant.lo(RMS,XXX) ne v_procQuant.UP(RMS,XXX)) $ DATA(RMS,"Proc",XXX,"CUR") ) ..

      (v_procQuantNeg(RMS,XXX) $ ( v_procQuant.LO(RMS,XXX) LE 0)
 +     v_procQuant(RMS,XXX)    $ ( v_procQuant.LO(RMS,XXX) GT 0)) / (DATA(RMS,"Proc",XXX,"CUR")+0.1)

      =E= (p_cnstNQProc(RMS,XXX)

          + SUM( YY $ p_hessNQProc(RMS,XXX,YY,"CUR"),
                  p_hessNQProc(RMS,XXX,YY,"CUR")
                     * (  v_procMarg(RMS,YY)  $ SED(YY)
                        - SUM(RMS_TO_RM(RMS,RM),  v_arm1Price(RM,YY) $ (NOT SED(YY))
                                                - DATA(RMS,"PRCA",YY,"CUR"))))/DATA(RMS,"PPri","Inpe","CUR"))
                                                                / (DATA(RMS,"Proc",XXX,"CUR")+0.1);


*
* --- Ensure positive processing
*
 ProcFudge_(RMS,XXX) $ (      (v_procQuant.lo(RMS,XXX) LE 0)
                          $ (v_procQuant.lo(RMS,XXX) ne v_procQuant.UP(RMS,XXX)) $ DATA(RMS,"Proc",XXX,"CUR")) ..

     v_procQuant(RMS,XXX)/(abs(DATA(RMS,"Proc",XXX,"CUR"))+0.1)
        =E=  (-ncpcm(-v_procQuantNeg(RMS,XXX),0,1.E-2*min(1000,max(1.E-11,data(RMS,"Proc",XXX,"CUR")))) + 1.E-11)
             /(abs(DATA(RMS,"Proc",XXX,"CUR"))+0.1);
*
*
* --- Production of oils and cakes (Leontief)
*
 ProcO_(RMS,XXX) $ (   SUM(SED $ ( SED_TO_CAK(SED,XXX) OR SED_TO_OIL(SED,XXX)),
                              DATA(RMS,"Proc",SED,"CUR")*DATA(RMS,"PrcY",XXX,"CUR"))
                            $   (v_prodQuant.lo(RMS,XXX) ne v_prodQuant.up(RMS,XXX))
                            $ (CAK(XXX) or OIL(XXX))
                            $   DATA(RMS,"Prod",XXX,"CUR")) ..

     v_prodQuant(RMS,XXX) / (DATA(RMS,"Prod",XXX,"CUR")+0.1)
        =E= SUM(SED $ ( SED_TO_CAK(SED,XXX) OR SED_TO_OIL(SED,XXX)),
                                                               v_procQuant(RMS,SED) * v_procYield(RMS,XXX))
                          / (DATA(RMS,"Prod",XXX,"CUR")+0.1);

*
* --- Biofuel supply functions: Generally double log function wrt relative processing margin
*                               but steeper in a range where processing is supposed to start
*                               and close to zero for  processing margins lt 1
*
*                               Can be seen as being paired to v_prodBiof
*
 SupplyBiof_(RMS,XXBIOF) $     ( DATA(RMS,"BiofPriceRel",XXBIOF,"CUR")
                              $ (SUM(Stock_to_Fuel(XBioStock,XXBioF),
                                    DATA(RMS,"BIOF",XBioStock,"CUR")*DATA(RMS,"PRCB",XBioStock,"CUR")))
                              $ ((v_prodBiof.lo(RMS,XXBioF) ne v_prodBiof.up(RMS,XXBioF)) or p_trim)
                              $ (v_prodQuant.lo(RMS,XXBioF) ne v_prodQuant.up(RMS,XXBioF))
                              $ DATA(RMS,"PROD",XXBIOF,"CUR")
                              $ XXX1(XXBIOF)
                              $ p_endoBioMarket
                               ) ..

     v_prodBiof(RMS,XXBioF)/(DATA(RMS,"Prod",XXBioF,"CUR")+0.1)
      =E=
*              --- small linear term to avoid zero production and zero slope..
              (p_bioSupPar(RMS,XXBIOF,"scale")*v_biofPriceRel(RMS,XXBIOF)
*             --- double log part
              + exp(p_bioSupPar(RMS,XXBIOF,"alfa") + p_bioSupPar(RMS,XXBIOF,"beta")*log(v_biofPriceRel(RMS,XXBIOF)+1e-2))
*                      --- multipied to sigmoid function that guarantees steeper slope
                                   * SIGMOID(p_bioSupPar(RMS,XXBIOF,"SLOPE")
                                            *(v_biofPriceRel(RMS,XXBIOF)
                                              -p_bioSupPar (RMS,XXBIOF,"Turn"))))/(DATA(RMS,"Prod",XXBioF,"CUR")+0.1);

*
* --- Total Output of biofuels: 1st Generation (profBiof, endogenous)
*                              + exogenous:   second generation (SECG)
*                                          + non agricultural (NAGR)
*                                          + exogenous quantities
*
*
 MaprBiof_(RMS,XXBioF) $ ((SUM(SAMEAS(XXX,XXBIOF),1)
                          $ (v_prodQuant.lo(RMS,XXBioF) ne v_prodQuant.up(RMS,XXBioF)))
                          $ DATA(RMS,"Prod",XXBioF,"CUR")
                          $ XXX1(XXBIOF) )..

     v_prodQuant(RMS,XXBioF)/(DATA(RMS,"Prod",XXBioF,"CUR")+0.1)
       =E=                     (     v_prodBiof(RMS,XXBioF)
                                 +   DATA(RMS,"NAGR",XXBioF,"CUR")
                                 +   DATA(RMS,"SECG",XXBioF,"CUR")
                                 +   DATA(RMS,"EXOG",XXBioF,"CUR"))/(DATA(RMS,"Prod",XXBioF,"CUR")+0.1);
*
* --- 1st Generation Output of biofuels (v_prodBiof) is equal to the sum over the feedstocks of
*          biofuel processing times extraction rates (PRCB).
*
*           That equation might be seen as being paired to the processing quantity of
*           the numeraire (and NOT to the output quantity).
*
*           Attention (see equation MaprBiof above): not equal to total biofuel outpt
*
 prodBiof_(RMS,XXBioF) $ (
                   (SUM(Stock_to_fuel(YbioStock,XXBioF), DATA(RMS,"BioF",YbioStock,"CUR")) GT 1)
*                --- the numerarie is not fixed
                 $ (NOT SUM(BIOF_NUM(RMS,XXBioF,XX) $ (v_BiofProcQuant.LO(RMS,XX) EQ v_biofProcQuant.UP(RMS,XX)),1))

*                --- at least one feedstock in data base
                 $  SUM(Stock_to_fuel(XXX,XXBioF) $ (DATA(RMS,"PRCB",XXX,"CUR") $ DATA(RMS,"BioF",XXX,"CUR")),1)
                 $ DATA(RMS,"Prod",XXBioF,"CUR")) ..

     v_prodBiof(RMS,XXBioF)/(DATA(RMS,"Prod",XXBioF,"CUR")+0.1) =E=

          SUM(Stock_to_fuel(XX,XXBioF) $ (DATA(RMS,"PRCB",XX,"CUR") $ DATA(RMS,"BioF",XX,"CUR")),
                                       v_biofProcQuant(RMS,XX) * DATA(RMS,"PRCB",XX,"CUR"))
                            /(DATA(RMS,"Prod",XXBioF,"CUR")+0.1);
*
*  --- Equation defines the c-sugar price from the implicit market clearing condition for c-sugar:
*      c-sugar produced = sugar used for bio-fuel processing. The right hand side
*      let the production quantity (= by definition equal to the biofuel processing quantity)
*      react to the c-sugar price.
*
 CSugarPrice_(RMS) $ (
                         (       (v_biofProcQuant.LO(RMS,"SUGA")  NE v_biofProcQuant.UP(RMS,"SUGA"))
                              or (v_biofFeedCost.lo(RMS,"SUGA")    NE v_biofFeedCost.up(RMS,"SUGA")))
                       $  (data(RMS,"QUTS","SUGA","CUR") gt eps)
                       $   DATA(RMS,"Biof","SUGA","CUR")  ) ..

     log(v_biofProcQuant(RMS,"SUGA")/DATA(RMS,"Biof","SUGA","CUR"))

            =E= p_cSugarSupElas * log( v_cSugarPrice(RMS) /sum(RMS_TO_RM(RMS,RM),DATA(RM,"Arm1P","SUGA","CUR") ))
                + p_cSugarCalPar(RMS);
*
* --- Production of DDGS/Glycerin from bio-fuel processing of cereals resp. vegetable oils
*           derived from by-product extraction rate (PRCBY)
*
 prodQuantBiofBy_(RMS,bio_by) $ (
                                  ((v_prodQuant.lo(RMS,bio_by) ne v_prodQuant.up(RMS,bio_by)))
                                  $ SUM(Stock_to_by(XXX,bio_by) $ (DATA(RMS,"BioF",XXX,"CUR") * DATA(RMS,"PRCBY",XXX,"CUR")),1)
                                  $ DATA(RMS,"Prod",bio_by,"CUR")) ..

     v_prodQuant(RMS,bio_by)/(DATA(RMS,"Prod",bio_by,"CUR")+0.1)
             =E= SUM(Stock_to_by(XX,bio_by) $ (DATA(RMS,"BioF",XX,"CUR") * DATA(RMS,"PRCBY",XX,"CUR")),
                                                      v_biofProcQuant(RMS,XX) * DATA(RMS,"PRCBY",XX,"CUR"))
                                                                      /(DATA(RMS,"Prod",bio_by,"CUR")+0.1);


*
* --- Feedstock costs per unit of input, minus secondary product revenues
*
*     Note: for sugar the input costs are defined by the market clearing price for out-of-quota sugar (v_cSugarPrice)
*
  biofFeedCost_(RMS,XBioStock) $ (   DATA(RMS,"Biof",XBioStock,"CUR")
                                  $ DATA(RMS,"PRCB",XBioStock,"CUR")
*
*                              --- input cost is not fixed
*
                                  $ (v_biofFeedCost.lo(RMS,XBioStock) ne v_biofFeedCost.up(RMS,XBioStock))
*
                               ) ..

     v_biofFeedCost(RMS,XBioStock)/(DATA(RMS,"BiofFeedCost",XBioStock,"CUR")+1) =E=

                [
*                  --- by feedstock cost per unit of input
                        SUM((RMS_TO_RM(RMS,RM), SAMEAS(XBioStock,XX)),
                                     v_Arm1Price(RM,XX)       $ ((NOT sameas(XX,"SUGA")) or (data(RMS,"QUTS","SUGA","CUR") le eps))
                                   + v_cSugarPrice(RMS)       $ (SAMEAS(XX,"SUGA") and (data(RMS,"QUTS","SUGA","CUR") gt eps))
                         )

*                  -- minus value of by product per ton of input
                   - SUM(Stock_to_By(XBioStock,bio_By), DATA(RMS,"PRCBY",XBioStock,"CUR")*v_prodPrice(RMS,bio_By))
             ]
$iftheni.mark2 %markupBiofFeedCost%==on
             * (1+p_markupBiofFeedCost(RMS,XBioStock))
$endif.mark2
            /(DATA(RMS,"BiofFeedCost",XBioStock,"CUR")+1) ;

*
*
* --- Average feedstock costs per unit of output, minus secondary product revenues
*
  biofFeedCostAgg_(RMS,XXBIOF) $ (
*
*                              --- some first generation output produced in calibration point
*
                                 (SUM(Stock_to_Fuel(XBioStock,XXBioF),
                                    DATA(RMS,"BIOF",XBioStock,"CUR")*DATA(RMS,"PRCB",XBioStock,"CUR")))
*
*                              --- margin not fixed
*
                               $ (v_biofFeedCost.lo(RMS,XXBIOF) ne v_biofFeedCost.up(RMS,XXBIOF))

                               $ DATA(RMS,"PROD",XXBIOF,"CUR")

                               ) ..

    v_biofFeedCost(RMS,XXBIOF)
      /(DATA(RMS,"BiofFeedCost",XXBIOF,"CUR")+1)
        =E=

              SUM(Stock_to_fuel(XBioStock,XXBiof) $ p_dpCesBiof(RMS,XBioStock),
                     p_dpCesBiof(RMS,XBioStock)  * (v_biofFeedCost(RMS,XBioStock)/(data(RMS,"BiofFeedCost",XXBIOF,"CUR")+1)) ** (1-p_rhoBioFuel(RMS,XXBiof))
                 ) ** (1/(1-p_rhoBioFuel(RMS,XXBiof)));
*
* --- CES share equations for feedstock demand (not fully consistent as we assume above fixed extracion rates ...)
*          The quantity of the feedstock used is equal to the quantity of the numeraire times a function the
*          price relation. The overall input level is derived from the prodBiof_ equation which hence drives the
*          numeraire quantity.
*
 Biof_(RMS,XX) $ (

*
*          Can be seen as paired to v_bioProcQuant with the exemption of the numeraire
*
*
*       --- don't enforce share equation for fixed quantities (arm\widen_bounds.gms)
*
                ( v_biofProcQuant.LO(RMS,XX) NE v_biofProcQuant.UP(RMS,XX))
                $ (v_biofProcQuant.UP(RMS,XX) ne 0)
*
*       --- introduce if XX is matched to a biofuel XXX (BIOE/BIOD) in the current solve
*           or if XX is in the current solve (XXX), in both cases, processing coefficient must ne non-zero
*
                $ (    SUM(Stock_to_fuel(XX,XXX), DATA(RMS,"PRCB",XX,"CUR"))
                    or SUM(Stock_to_fuel(XXX,XXBIOF) $ SAMEAS(XX,XXX), DATA(RMS,"PRCB",XXX,"CUR"))
                  )
*
*       --- not working for the numeraire feedstock which is used to define relative prices
*           and serves as the anchor quantity
*
                $ (NOT SUM(BIOF_NUM(RMS,XXBioF,XX),1))
                $ DATA(RMS,"Biof",XX,"CUR")

              )  ..
*
*
       v_biofProcQuant(RMS,XX)/(DATA(RMS,"Biof",XX,"CUR")+0.1)

        =E= [
             SUM(Stock_to_fuel(XX,XXBiof),
*
*                 ---- a share of output production quantity
*
                   p_dpCESBiof(RMS,XX) * v_prodBiof(RMS,XXBioF)
*
*                ---- times output prices divided by feedcost cost
*                     exponent the substiution elasticity
*
                 * (v_biofFeedCost(RMS,XXBIOF) / v_biofFeedCost(RMS,XX)) ** p_rhoBioFuel(RMS,XXBIOF)
               )
             ]/(DATA(RMS,"Biof",XX,"CUR")+0.1);

 biofPriceRel_(RMS,XXBIOF) $ (DATA(RMS,"PROD",XXBIOF,"CUR") $ DATA(RMS,"biofPriceRel",XXBIOF,"CUR") $ XXX1(XXBIOF)) ..

   v_biofPriceRel(RMS,XXBIOF)/(DATA(RMS,"biofPriceRel",XXBIOF,"CUR")+1)
       =E= [v_prodPrice(RMS,XXBiof) / v_biofFeedCost(RMS,XXBIOF)]
           /(DATA(RMS,"biofPriceRel",XXBIOF,"CUR")+1);


* --- Share of in total fuel consumption of biofuels used in the transport sector in total fuel consumption

 biofDemShare_(RMS,XXBIOF) $ (
                                     SUM(fuelMatch(XXBioF,Fuel_Rows), Data(RMS,"CPRI",fuel_rows,"CUR"))
                                   $ p_bioDemPar(RMS,XXBioF,"Slope")
                                   $ sum(sameas(xxx,xxBiof),1)
                                   $ (v_biofProcQuant.range(RMS,XXBIOF) ne 0)
                                   $ ((v_consShareBioF.range(RMS,XXBIOF) NE 0) or p_trim)
                                   $ xxx1(XXBIOF)
                                   $ DATA(RMS,"BIOF",XXBIOF,"CUR")
                                   $ p_endoBioMarket)..

     v_consShareBioF(RMS,XXBioF)  =E=
*
*         --- fixed share (= blending quota relative)
*
          Data(RMS,"QUTS",XXBIOF,"CUR")

*
*         -- sigmoid function, assumes that rather small shares are used even at high prices,
*            "TURN" reflects the quota
*
       +  (SIGMOID(p_bioDemPar(RMS,XXBioF,"Slope")
                                *(v_consprice(RMS,XXBIOF) / SUM(fuelMatch(XXBioF,Fuel_Rows), Data(RMS,"CPRI",fuel_rows,"CUR"))
                                  -p_bioDemPar(RMS,XXBioF,"Turn"))))*p_bioDemPar(RMS,XXBioF,"Max");


* --- Total biofuel demand. the bigger of the sum of the three components above or the obligation quota muktplied with total fuel use


 biofDem_(RMS,XXBioF) $ (DATA(RMS,"BIOF",XXBIOF,"CUR")
                        $ XXX1(xxBiof)
                        $ (v_biofProcQuant.range(RMS,XXBIOF) ne 0)
                        $ p_endoBioMarket )..

    v_biofProcQuant(RMS,XXBIOF)/(DATA(RMS,"BIOF",XXBIOF,"CUR")+0.1)

       =E=
*
*          --- share of biofuels in total fossil fuel demand,
*              reflecting oil equivalent differences
*
           v_consShareBioF(RMS,XXBioF)/p_oilEqu(XXbioF)
*
*          --- total demand for fossil fuels (diesel, gasoline) in oil equivalents
*
            * SUM(fuelMatch(XXBioF,Fuel_Rows), Data(RMS,"HCON",fuel_rows,"CUR") * p_oilEqu(fuel_Rows))
                   /(DATA(RMS,"BIOF",XXBIOF,"CUR")+0.1);
*
* --- "Output of feed by products from milling and processing" (FENI, FPRI)
*
  MaprFeed_(RMSSUP,RESIMPX) $ ( SUM(SAMEAS(XX,RESIMPX),1)
                                $ (v_prodQuant.lo(RMSSUP,RESIMPX) ne v_prodQuant.up(RMSSUP,RESIMPX))
                                $ DATA(RMSSUP,"Prod",RESIMPX,"CUR")) ..

     v_prodQuant(RMSSUP,RESIMPX) / (DATA(RMSSUP,"Prod",RESIMPX,"CUR")+0.1)
          =E= (SUM(RESIMP_T_O(RESIMPX,XX),
                      v_consQuant(RMSSUP,XX) $ DATA(RMSSUP,"HCon",XX,"CUR")
                    + v_procQuant(RMSSUP,XX) $ DATA(RMSSUP,"Proc",XX,"CUR")
                    + v_biofProcQuant(RMSSUP,XX) $ (DATA(RMSSUP,"Biof",XX,"CUR") $ SAMEAS(XX,"SUGA")))
                            * p_feedByProdShare(RMSSUP,RESIMPX)
                                           ) / (DATA(RMSSUP,"Prod",RESIMPX,"CUR")+0.1);
*
$ONTEXT
    Define processing yields. Beware, the first two terms are used for
    computing v_procYield in the calibration stage. The second of thos is for defining
    a processing yield in the case that production of oil or cake is missing.

    The last two terms are used in simulation. The first defines the processing
    yield of oil as a CET function of the price ratio oil/cake. The last term
    defines the processing yield of cake so that the mass (dis-)equilibrium of
    the calibration point is maintained.

    The third term in the expression is really looking as follows, for any seed
    and region and with _o meaning "oil" and _c meaning "cake":

    v_procYield_o = v_procYield_c * [CET_DP_o/CET_DP_c * ppri_o/ppri_c]^p_rhoProc

    Those are first order conditions from an optimization problem constrained
    by CET. Now, only the ratio CET_DP_o/CET_DP_c matters, so we may choose
    one of the parameters arbitrarily. For scaling, we set CET_DP_c = 10000.

    The parameter CET_DP is computed in cal_armington.gms. It could just as
    well be solved for analytically, but would then require redundant code.

    (T. Jansson, LEI)
$OFFTEXT
*
*
 procYield_(RMS,XXX) $ (
                         SUM(SED $ ( SED_TO_CAK(SED,XXX) OR SED_TO_OIL(SED,XXX)),
                                          DATA(RMS,"Proc",SED,"CUR")*DATA(RMS,"Prod",XXX,"CUR"))
                         $ ((v_procYield.LO(RMS,XXX) ne v_procYield.UP(RMS,XXX)) or p_trim)
                         $ DATA(RMS,"Prod",XXX,"CUR")
                         $ (CAK(XXX) or OIL(XXX))) ..

     v_procYield(RMS,XXX)
          =E=
*
*              --- Define processing yield consistent during calibration of market balances
*                  from consistent output of cake / oild and processing quantity of seed
*
*
               SUM(SED $ (SED_TO_CAK(SED,XXX) OR SED_TO_OIL(SED,XXX)),
                                                  v_prodQuant(RMS,XXX)/v_procQuant(RMS,SED)) $ p_trim


            +  [
*
*               --- Yield of oil in simulation: derived from CET technology
*
                +  DATA(RMS,"PrcY",XXX,"CUR") * SUM(SED_TO_CAK(SED,CAK) $ SED_TO_OIL(SED,XXX),
                        EXP( p_rhoProc(RMS,SED) * log (  ((v_prodPrice(RMS,XXX)+1.E-3)/(DATA(RMS,"PPri",XXX,"CUR")+1.E-3))
                                                        /((v_prodPrice(RMS,CAK)+1.E-3)/(DATA(RMS,"PPri",CAK,"CUR")+1.E-3)))))
*
*               --- Yield of cake in simulation: derived from CET technology
*
                +  DATA(RMS,"PrcY",XXX,"CUR") * SUM(SED_TO_OIL(SED,OIL) $ SED_TO_CAK(SED,XXX),
                        EXP( p_rhoProc(RMS,SED) * log (  ((v_prodPrice(RMS,XXX)+1.E-3)/(DATA(RMS,"PPri",XXX,"CUR")+1.E-3))
                                                        /((v_prodPrice(RMS,OIL)+1.E-3)/(DATA(RMS,"PPri",OIL,"CUR")+1.E-3)))))
*
*               --- correct to keep mass flow balance
*                   (if the sum of the simulated processing yields exceed the
*                    per unit output of cake and oil in the calibration point,
*                    correct both by 50% of error)
*
                + SUM((SED,YYY) $ (  (SED_TO_CAK(SED,YYY) or SED_TO_OIL(SED,YYY))
                                   $ (SED_TO_CAK(SED,XXX) or SED_TO_OIL(SED,XXX))),
                             DATA(RMS,"PrcY",YYY,"CUR")-v_procYield(RMS,YYY))/2

               ] $ (not p_trim);
*
*
* --- Net put function from NQ profit function, linear in normaized prices
*     Outputs: all agricultural primary (hence not included: oil, cakes, dairy products, bio-fuels ..)
*     Inputs: land and feed energy (FEDE)
*
*
 ProdNQ_(RMS,XX1) $ (  (    (NOT xx_no_prodNQ_equation(XX1,RMS))
                          $ (not sameas(XX1,"FEDE")))
                          $  (XXX1(XX1) or XXX1("Fede") or xxx1("land")
                       )
*
*                     --- production must be non-fixed
*
                      $ (v_prodQuant.lo(RMS,XX1) ne v_prodQuant.up(RMS,XX1))
*
*                     --- production must be given in calibration point
*
                      $ DATA(RMS,"Prod",XX1,"CUR"))  ..

      (v_prodQuantNeg(RMS,XX1) $ ( v_prodQuant.LO(RMS,XX1) LE 0)
 +     v_prodQuant(RMS,XX1)    $ ( V_prodQuant.LO(RMS,XX1) GT 0)) / (DATA(RMS,"Prod",XX1,"CUR")+0.1)

       =E= (p_cnstNQSupp(RMS,XX1)
            + SUM( YY1 $ (DATA(RMS,"PPri",YY1,"CUR") $ (NOT SAMEAS(YY1,"INPE")) $ DATA(RMS,"PROD",YY1,"CUR")),
                           p_hessNQSupp(RMS,XX1,YY1,"CUR")
*
*                         -- no v_marketPrice and PPRI_ for MILK => direct support for milk producers via PSEd is introduced here
*                            NOTE1: v_prodPrice(RMS,"MILK") in ProcMargM_ excludes this PSEd, because it is not a cost to dairies
*                            NOTE2: Support for milk production in RMSSUP would be covered in the supply models
*
                         * (v_prodPrice(RMS,YY1)
                             - DATA(RMS,"PPri",YY1,"CUR") * p_oilCostShare(RMS,YY1) * DATA(RMS,"UVAG","CRDO","PercentageChange")/100
                             + (DATA(RMS,"PSEd",YY1,"CUR")
                             +  DATA(RMS,"PSEI",YY1,"CUR")
                             + DATA(RMS,"AREP",YY1,"CUR")*(1-DATA(RMS,"DECP",YY1,"CUR"))) $ SAMEAS(YY1,"MILK")
*                         -- no v_marketPrice and PPRI_ for LAND => direct support via PSEd is introduced here
*                            But we assume that the PSEd.LAND has been stored as a positive number (subsidy) => need to deduct from price of land
                             - DATA(RMS,"PSEd",YY1,"CUR") $ SAMEAS(YY1,"LAND")
                           )/v_prodPrice(RMS,"Inpe")
                  )


            + SUM( feed_trd $ p_hessNQSupp(RMS,XX1,feed_trd,"Cur"),
                           p_hessNQSupp(RMS,XX1,feed_trd,"CUR")
                         * v_feedBlkPrice(RMS,feed_trd)/v_prodPrice(RMS,"Inpe"))
                         )
                         /(DATA(RMS,"Prod",XX1,"CUR")+0.1);
*
*
*
 yaniMarket_(RM,xxOmYani) $ ( sum(RM_TO_RMS(RM,RMSSUP),v_prodPrice.range(RMSSUP,xxOmYAni)) $ XXX(xxOmYani) ) ..
*
       sum(RM_TO_RMS(RM,RMSSUP),

            p_cnstNQSupp(RMSSUP,xxOmYani)

              + sum(YY1 $ (DATA(RMSSUP,"PPri",YY1,"CUR") $ (NOT SAMEAS(YY1,"INPE")) $ DATA(RMSSUP,"PROD",YY1,"CUR")),
                           p_hessNQSupp(RMSSUP,xxOmYani,YY1,"CUR")
                             * v_prodPrice(RMSSUP,YY1)/v_prodPrice(RMSSUP,"Inpe"))

            + SUM( feed_trd $ p_hessNQSupp(RMSSUP,xxOmYani,feed_trd,"Cur"),
                           p_hessNQSupp(RMSSUP,xxOmYani,feed_trd,"CUR")
                             * v_feedBlkPrice(RMSSUP,feed_trd)/v_prodPrice(RMSSUP,"Inpe"))
        )
             / (sum(RM_TO_RMS(RM,RMSSUP),
                 DATA(RMSSUP,"GROF",xxOmYani,"CUR")
                  +SUM(O_TO_YANI(xxOmYani,IYANI), DATA(RMSSUP,"GROF",IYANI,"CUR")))*0.5 + 1)

          =E=  sum(RM_TO_RMS(RM,RMSSUP),
                DATA(RMSSUP,"GROF",xxOmYani,"CUR")
                 -SUM(O_TO_YANI(xxOmYani,IYANI), DATA(RMSSUP,"GROF",IYANI,"CUR")))
             / (sum(RM_TO_RMS(RM,RMSSUP),
                 DATA(RMSSUP,"GROF",xxOmYani,"CUR")
                  +SUM(O_TO_YANI(xxOmYani,IYANI), DATA(RMSSUP,"GROF",IYANI,"CUR")))*0.5 + 1);
*
* --- Land supply equation in semi-log form (= land supply is depending on the land price (change
*     relative to the calibration point) times a elasticity
*

 LandSupply_(RMS) $ (    DATA(RMS,"Prod","LAND","CUR")
                      $ (v_prodQuant.LO(RMS,"Land") ne v_prodQuant.UP(RMS,"Land")) ) ..

      (v_prodQuantNeg(RMS,"Land") $ ( v_prodQuant.LO(RMS,"Land") LE 0)
 +     v_prodQuant(RMS,"Land")    $ ( V_prodQuant.LO(RMS,"Land") GT 0)) /DATA(RMS,"Prod","Land","CUR")

            =E= p_landSupplyElas(RMS)
*                 As the log only tolerates positive numbers we use a fudged producer price here
*                 to give a near zero land supply if the land rent is "approaching" zero
                * log( ( v_prodPricePos(RMS,"Land") $ (v_prodPrice.lo(RMS,"Land") le 0)
                       + v_prodPrice(RMS,"Land")    $ (v_prodPrice.lo(RMS,"Land") gt 0))
                     / DATA(RMS,"PPRI","LAND","CUR")) + 1;
*


* --- Three agric land types in market model (temp crops = TCRP, fruits+nurseries = FRUN, fodder+fallow = FODFAL=TGRS+GRAS)
*
 LandAgrFromXX_(RMS,luAgrMrk) $ (  data(RMS,luAgrMrk,"levl","cur")
*                                explicit exclusion of rmssup is redunddant if we ensure v_prodQuant.range(RMSSUP,"land")=0
                                 $ ((not RMSSUP(RMS)) or p_trim)
                                 $ (v_prodQuant.range(RMS,"land") or p_trimlnd)
                                 )..
*
  (
    SUM( luAgr_to_xx(luAgrMrk,XX_landuse) $ DATA(RMS,"levl",XX_landuse,"CUR") , v_landAgr(RMS,XX_landuse))
   +SUM( luAgr_to_ocnec(luAgrMrk,ocnec) $ v_landAgr.l(RMS,ocnec) ,
        v_landAgr(RMS,ocnec)
      * (v_tcrpWgt(RMS,ocnec) $ sameas(luAgrMrk,"TCRP") + (1-v_tcrpWgt(RMS,ocnec)) $ (not sameas(luAgrMrk,"TCRP"))) )
   + 1.E-6)

     / (data(RMS,luAgrMrk,"levl","cur")+ 1.E-6)
  =E= (v_land(RMS,luAgrMrk)+ 1.E-6)
     / (data(RMS,luAgrMrk,"levl","cur")+ 1.E-6);

*
 Yield_(RMS,XX_landuse) $ ( DATA(RMS,"Prod",XX_landuse,"CUR")
                          $ data(RMS,"yild",XX_landuse,"cur")
                          $ (v_prodQuant.range(RMS,"land") or p_trimlnd)
                          $ DATA(RMS,"levl",XX_landuse,"CUR")) ..
*
        (v_yield(RMS,XX_landuse)+ 1.E-6)
      / (DATA(RMS,"yild",XX_landuse,"CUR")+ 1.E-6)
   =E=   (  ( v_prodPricePos(RMS,XX_landuse) $ (v_prodPrice.lo(RMS,XX_landuse) le 0)
            + v_prodPrice(RMS,XX_landuse)    $ (v_prodPrice.lo(RMS,XX_landuse) gt 0)
            + 1.E-6)
          / (DATA(RMS,"PPri",XX_landuse,"CUR")+1.E-6))**(  p_elasSupp(RMS,XX_landuse,"Yild","CAL")
                                                          +1 $ (not p_elasSupp(RMS,XX_landuse,"Yild","CAL")) )
              ;

*
* --- Prelim land use by product follows from a yield elasticity and price changes, still ignoring consistency with prodNQ_ ...

 LandAgrTmp_(RMS,XX_landuse) $ (DATA(RMS,"Prod",XX_landuse,"CUR")
                                $ data(RMS,"yild",XX_landuse,"cur")
                                $ (v_prodQuant.range(RMS,"land") or p_trimlnd)
                                $ DATA(RMS,"levl",XX_landuse,"CUR")) ..
*
       (v_landAgrTmp(RMS,XX_landuse) $ (not p_trim) + v_landAgr(RMS,XX_landuse) $ p_trim +1.e-6)
      /(DATA(RMS,"LEVL",XX_landuse,"CUR") + 1.e-6)
   =E=   (   v_prodQuant(RMS,XX_landuse)
         /   v_yield(RMS,XX_landuse) / DATA(RMS,"cropix",XX_landuse,"CUR")+1.e-6)
      /(DATA(RMS,"LEVL",XX_landuse,"CUR") + 1.e-6)
              ;

*     ... which is imposed by scaling:

 LandAgr_(RMS,XX_landuse) $ ( DATA(RMS,"levl",XX_landuse,"CUR")
                            $ (v_prodQuant.range(RMS,"land") or p_trimlnd)
                            ) ..

   (v_landAgr(RMS,XX_landuse) + 1.E-6)
      /(DATA(RMS,"levl",XX_landuse,"CUR") + 1.E-6)
   =E=   (v_prodQuant(RMS,"land")
       * v_landAgrTmp(RMS,XX_landuse)/sum(XX_landuse1 $ DATA(RMS,"levl",XX_landuse1,"CUR"),v_landAgrTmp(RMS,XX_landuse1))+ 1.E-6)
      /(DATA(RMS,"levl",XX_landuse,"CUR")  + 1.E-6)
           ;


* --- compute residual land for the sum of all NAGR land types (= foreOlndArtif):
*     Note defining this as positive ensures some land remaining
 LandNagr_(RMS)  $ ( v_land.range(RMS,"NAGR")
*                                        explicit exclusion of rmssup is redunddant if we ensure v_prodQuant.range(RMSSUP,"land")=0
                                         $ (not RMSSUP(RMS))
                                         $ (v_prodQuant.range(RMS,"land") or p_trimlnd)
                                         )..
*
   (v_land(RMS,"NAGR"))
      /sum(foreOlndArtif1, data(RMS,foreOlndArtif1,"levl","cur"))
   =E= ((data(RMS,"arto","levl","cur")-sum(luAgrMrk $ data(RMS,luAgrMrk,"levl","cur"), v_land(RMS,luAgrMrk))-v_land(RMS,"inlw")))
      /sum(foreOlndArtif1, data(RMS,foreOlndArtif1,"levl","cur"))
      ;

* --- Land types ForeOlndArtif are allocated based on modified trends but squeezed into the total land balance

*     -- first step mainly corrects OLND, less so FORE and even less ARTIF (via ad hoc "adjustment elasticities"):
*
 LandTypesNagrTmp_(RMS,foreOlndArtif)  $ ( v_landTmp.range(RMS,foreOlndArtif)
*                                        explicit exclusion of rmssup is redunddant if we ensure v_prodQuant.range(RMSSUP,"land")=0
                                         $ (not RMSSUP(RMS))
                                         $ (v_prodQuant.range(RMS,"land") or p_trimlnd)
                                         )..
*
   (v_landTmp(RMS,foreOlndArtif)+ 1.E-6)
     / (data(RMS,foreOlndArtif,"levl","cur")+ 1.E-6)
   =E= (v_land(RMS,"NAGR")
      /sum(foreOlndArtif1, data(RMS,foreOlndArtif1,"levl","cur")))**p_shFstInSecRmall(RMS,foreOlndArtif,"CorrForeOlndArtif");

*     -- but a second step scaling is needed to ensure that the total area balance really holds:

*                                      explicit exclusion of rmssup is redunddant if we ensure v_prodQuant.range(RMSSUP,"land")=0
 LandTypesNagr_(RMS,foreOlndArtif)   $ ( (not RMSSUP(RMS))
                                       $ (v_prodQuant.range(RMS,"land") or p_trimlnd)
                                       )..
*
   (v_land(RMS,foreOlndArtif)+ 1.E-6)
     /(data(RMS,foreOlndArtif,"levl","CUR")+ 1.E-6)
      =E= (v_landTmp(RMS,foreOlndArtif)
          * v_land(RMS,"NAGR")
       /  sum(foreOlndArtif1, v_landTmp(RMS,foreOlndArtif1))+ 1.E-6)
     /(data(RMS,foreOlndArtif,"levl","CUR")+ 1.E-6);

* --- Fudging function for producer prices determining yields

 ProdPriceFudge_(RMS,XX_landuseOrLand) $ ( (v_prodQuant.range(RMS,"land") or p_trimlnd)
                                         $ XXX_all(XX_landuseOrLand)
                                         $ (v_prodPrice.lo(RMS,XX_landuseOrLand) LE 0)
                                         $ DATA(RMS,"PPri",XX_landuseOrLand,"CUR") )  ..

    v_prodPricePos(RMS,XX_landuseOrLand)
           /(abs(DATA(RMS,"PPri",XX_landuseOrLand,"CUR") )+0.1)
        =E=  (-ncpcm(-v_prodPrice(RMS,XX_landuseOrLand),0,
                    .1*min(10000,Max(1.E-2,data(RMS,"PPri",XX_landuseOrLand,"CUR"))))
              + 1.E-2*max(1,data(RMS,"PPri",XX_landuseOrLand,"CUR")))
           /(abs(DATA(RMS,"PPri",XX_landuseOrLand,"CUR"))+0.1);



* --- Ensure positive production quantities, as the NQ functional form may
*          simulate negative quantities. If that happens, the square system will
*          become infeasible. Arm\widen_bound will introduce a lower
*          bound below zero which will then trigger introduction of the following equation
*          in the model structure to allow for a feasible solution
*
*          The same approach is used for feed and processing demand.
*
*
 ProdFudge_(RMS,XX1) $ (
                              ((v_prodQuant.lo(RMS,XX1) ne v_prodQuant.up(RMS,XX1)))
                            $ (XXX1(XX1) or XXX1("Fede"))
                            $ (not XXBIOF(XX1))
                            $ (not sum(sameas(xxOmYani,XX1),1))
                            $ (v_prodQuant.lo(RMS,XX1) LE 0)
                            $ DATA(RMS,"Prod",XX1,"CUR")
                            $ (NOT SAMEAS(XX1,"DDGS")) ) ..

       v_prodQuant(RMS,XX1)
           /(abs(DATA(RMS,"Prod",XX1,"CUR"))+0.1)
        =E=  (-ncpcm(-v_prodQuantNeg(RMS,XX1),0,1.E-2*min(1000,Max(1.E-11,data(RMS,"Prod",XX1,"CUR")))) + 1.E-11)
           /(abs(DATA(RMS,"Prod",XX1,"CUR"))+0.1);

*
* -- Blocks of equation which add quantities from single countries (or block of countries) to
*    trade blocks
*
 ProdA_(RM,XXX)    $ (   (SUM( RMS_TO_RM(RMS,RM), 1.) GT 1.) $ (not xxOmYani(XXX))
                       $ (v_prodQuant.lo(RM,XXX) ne v_prodQuant.up(RM,XXX))) ..
*
    v_prodQuant(RM,XXX)
       /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"PROD",XXX,"CUR")+1.E-1)

    =E= SUM(RMS_TO_RM(RMS,RM) $ DATA(RMS,"Prod",XXX,"CUR"), v_prodQuant(RMS,XXX)
                           - v_biofProcQuant(RMS,"SUGA")    $ (sameas(XXX,"SUGA") $ DATA(RMS,"Biof","SUGA","CUR")
*
*                                                          --- in case of sguar market quotas, C-sugar is supposed to
*                                                              be used for biofuels and has its own market clearing
                                                            $ (data(RMS,"QUTS","SUGA","CUR") gt eps))
       )
       /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"PROD",XXX,"CUR")+1.E-1);
*
 ProcA_(RM,XXX)    $ (   (SUM( RMS_TO_RM(RMS,RM), 1.) GT 1.)
                       $ (v_procQuant.lo(RM,XXX) ne v_procQuant.up(RM,XXX)))..
*
    v_procQuant(RM,XXX)
          /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Proc",XXX,"CUR")+1.E-1)
       =E= SUM(RMS_TO_RM(RMS,RM) $ DATA(RMS,"Proc",XXX,"CUR"), v_procQuant(RMS,XXX))
          /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Proc",XXX,"CUR")+1.E-1);
*
 BiofA_(RM,XXX) $ (   (SUM( RMS_TO_RM(RMS,RM), 1.) GT 1.)
                    $ (v_biofProcQuant.lo(RM,XXX) ne v_biofProcQuant.up(RM,XXX))) ..
*
    v_biofProcQuant(RM,XXX)
       /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Biof",XXX,"CUR")+1.E-1)
     =E= SUM(RMS_TO_RM(RMS,RM) $ DATA(RMS,"Biof",XXX,"CUR"), v_biofProcQuant(RMS,XXX))
       /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Biof",XXX,"CUR")+1.E-1);
*
 HconA_(RM,XXX) $ (   (SUM( RMS_TO_RM(RMS,RM), 1.) GT 1.)
                    $ (v_consQuant.lo(RM,XXX) ne v_consQuant.up(RM,XXX))) ..
*
    v_consQuant(RM,XXX)
       /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Hcon",XXX,"CUR")+1.E-1)
     =E= SUM(RMS_TO_RM(RMS,RM) $ DATA(RMS,"HCon",XXX,"CUR"), v_consQuant(RMS,XXX))
       /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Hcon",XXX,"CUR")+1.E-1);
*
 FeedUseA_(RM,XXX) $ (   (SUM( RMS_TO_RM(RMS,RM), 1.) GT 1. )
                       $ (v_feedQuant.lo(RM,XXX) ne v_feedQuant.up(RM,XXX))) ..
*
    v_feedQuant(RM,XXX)
        /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Feed",XXX,"CUR")+1.E-1)
     =E= SUM(RMS_TO_RM(RMS,RM) $ DATA(RMS,"Feed",XXX,"CUR"), v_feedQuant(RMS,XXX))
       /SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Feed",XXX,"CUR")+1.E-1);
*
* --------------- equations only found in net trade model
*
*
* --- world market balance (paired with world market price)
*
 wldMarket_(XXX) ..

          sum(RM $ DATA(RM,"NTRD",XXX,"CUR"), v_netTrade(RM,XXX))
         /sum(RM, abs(DATA(RM,"NTRD",XXX,"CUR")))
      =E= 0;

*
* --- Behavioral equation net trade
*
 ntrd_(RM,XXX) $ (    DATA(RM,"NTRD",XXX,"CUR") $ DATA("WOR","PMRK",XXX,"CUR")
                  $ (    (v_netTrade.lo(RM,XXX) ne v_netTrade.up(RM,XXX))
                      or (pv_NetTrade.lo(RM,XXX,"a") ne pv_NetTrade.up(RM,XXX,"a")) )) ..

     v_netTrade(RM,XXX) / (abs(DATA(RM,"NTRD",XXX,"CUR")) + 1.E-3)
                =E=  (  pv_NetTrade(RM,XXX,"a")
                      + pv_NetTrade(RM,XXX,"b") * (v_marketPrice(RM,XXX)/v_wldPrice(XXX)
                       * DATA("WOR","PMRK",XXX,"CUR")/DATA(RM,"PMRK",XXX,"CUR"))

                      + pv_NetTrade(RM,XXX,"c") * (v_marketPrice(RM,XXX)/v_wldPrice(XXX)
                       * DATA("WOR","PMRK",XXX,"CUR")/DATA(RM,"PMRK",XXX,"CUR"))**3

                      + pv_NetTrade(RM,XXX,"d") * (v_marketPrice(RM,XXX)/v_wldPrice(XXX)
                       * DATA("WOR","PMRK",XXX,"CUR")/DATA(RM,"PMRK",XXX,"CUR"))**5);
*
* --- Net-trade market balance
*
 mrkBal_(RM,XXX) $ ( v_marketPrice.lo(RM,XXX) ne v_marketPrice.up(RM,XXX) ) ..
*

   (
        v_prodQuant(RM,XXX) $ DATA(RM,"Prod",XXX,"CUR")
      + v_stockChg(RM,XXX) )
       / ( Data(RM,"Arm1",XXX,"CUR") + DATA(RM,"Prod",XXX,"CUR") + 1)

       =E=
   (
     + v_arm1Quant(RM,XXX)         $ DATA(RM,"Arm1",XXX,"CUR")
     + v_netTrade(RM,XXX)          $ DATA(RM,"Ntrd",XXX,"CUR")
$ifi %RP% == RMTP         + v_intervStockChange(RM,XXX) $ { [ sum(rm_to_rmtp(rm,rmtp),ABS(DATA(RMTP,"PADM",XXX,"CUR")*DATA(RMTP,"STKS",XXX,"CUR"))) gt eps]}
$ifi not %RP% == RMTP     + v_intervStockChange(RM,XXX) $ ( (DATA(RM,"PADM",XXX,"CUR") gt eps) $ (DATA(RM,"STKS",XXX,"CUR") gt eps) )
    )   / ( Data(RM,"Arm1",XXX,"CUR") + DATA(RM,"Prod",XXX,"CUR") + 1);
*
* --- Price transmission in net trade model
*
  arm1PNetTrade_(RM,XXX) $ (    DATA(RM,"ARM1P",XXX,"CUR")
                             $ (v_arm1Price.lo(RM,XXX) ne v_arm1Price.up(RM,XXX))) ..

    log(v_arm1Price(RM,XXX)/DATA(RM,"ARM1P",XXX,"CUR"))
        =E= p_arm1PriceCnst(RM,XXX) + log(v_marketPrice(RM,XXX)/DATA(RM,"ARM1P",XXX,"CUR"));
*
* --- Stock change: logistic function depending on current price and expected market price
*
 stockChg_(RM,XXX) $ ( v_marketPrice.lo(RM,XXX) ne v_marketPrice.up(RM,XXX) ) ..

    v_stockChg(RM,XXX)/(DATA(RM,"Arm1",XXX,"CUR") + DATA(RM,"Exports",XXX,"CUR") + 0.001)
       =E= (sigmoid( (v_marketPrice(RM,XXX)/DATA(RM,"PMrk",XXX,"CUR")-1)*p_stockChgSlope(RM,XXX)) - 0.5)
              * p_stockChgShare(RM,XXX);
*
*  -- Unit value exports: theoretical border prices to end up with market price of trading partner
*                         (excluding double-zero agreements as EU15 - EU10)
*
 uvae_(RM,XXX) $ ( DATA(RM,"Exports",XXX,"CUR") )..
*
       v_uvae(RM,XXX)/v_wldPrice.l(XXX)

        =E= (SUM( RM1 $ (   (NOT SAMEAS(RM,RM1))
                          $ p_tradeFlows(RM1,RM,XXX,"CUR")),

               (  ( v_marketPrice(RM1,XXX) * p_exchgRateChangeFactor(RM1,RM)

*
*                    --- export subsidies (in case of adminstrative price PADM, a budget
*                        for export subsidies FEOE_MAX), but not in case of a quota/duty
*                        access relation to the exporter or for the domestic sales
*
                    + v_perUnitExportSub(RM,XXX)  $ (   (DATA(RM,"PADM",XXX,"CUR") gt eps)
                                                      $ (DATA(RM,"FEOE_max",XXX,"CUR") GT eps)
                                                      $ (NOT p_doubleZero(RM1,RM,XXX,"CUR"))
                                                      $ (NOT SAMEAS(RM,RM1))
                                                    )
*
*                  --- per unit transport costs
*                      (no exchange rate factor if the exporting region RM handles transport)
                    - v_transpCost(RM1,RM,XXX) )
*
*                  --- minus ad valorem tariff, applied to offer price (f.o.b - export subsidies plus
*                      + plus per unit transport costs)
*
                     /  ( 1. + 0.01*v_tarAdVal(RM1,RM,XXX) $ (  (NOT p_doubleZero(RM1,RM,XXX,"CUR"))
                                                              $ (NOT SAMEAS(RM,RM1))
                                                             )
                        )
*
*                  --- minus specific tariffs
*
                      -  (

*                       --- fixed according to tariff schedule or endogenous under TRQ
                         v_tarSpec(RM1,RM,XXX) $ (   (DATA(RM1,"MinBordP",XXX,"CUR") LE eps)
                                                  or (p_trqBilat(RM1,RM,XXX,"TrqNT","CUR") eq prohibitive) )

*                         --- or flexible levy in case of minimum border prices
                         + v_flexLevy(RM1,RM,XXX)  $ (     (DATA(RM1,"MinBordP",XXX,"CUR") GT eps)
                                                      $ (p_trqBilat(RM1,RM,XXX,"TrqNT","CUR") ne prohibitive) )

                       ) $ (   (NOT p_doubleZero(RM1,RM,XXX,"CUR"))
                             $ (NOT SAMEAS(RM,RM1))
                           )

                   )  * v_tradeFlows(RM1,RM,XXX)) / v_expQuant(RM,XXX))
                       / v_wldPrice.l(XXX);
*
* --------------- adding up conditions (partly used to balance the base year, only)
*
* --- Adding up inside of the Armginton aggregate (total domestic consumption)
*
*      Paired with v_Arm1Quant
*

 ArmBal1_(RM,XXX) $ (v_arm1Quant.lo(RM,XXX) ne v_arm1Quant.up(RM,XXX)) ..
*
*    --- total domestic consumption for a trade block
*
     v_arm1Quant(RM,XXX) / (DATA(RM,"arm1",XXX,"CUR") $ (not p_trim) + 1 $ p_trim + 0.1)

            =E=  SUM(RMS_TO_RM(RMS,RM),
*                     --- human consumption
                             v_consQuant(RMS,XXX)        $ DATA(RMS,"HCon",XXX,"CUR")
*                     --- feed use
                           + v_feedQuant(RMS,XXX)        $ DATA(RMS,"Feed",XXX,"CUR")
*                     --- explicit processing demand (cakes)
                           + v_procQuant(RMS,XXX)        $ DATA(RMS,"Proc",XXX,"CUR")
*                     --- demand for biofuel processing
                           + v_biofProcQuant(RMS,XXX)    $ (DATA(RMS,"Biof",XXX,"CUR")
*
*                                                          --- in case of sguar market quotas, C-sugar is supposed to
*                                                              be used for biofuels, and has its own market clearing
                                                            $ ( (not sameas(XXX,"SUGA")) or (data(RMS,"QUTS","SUGA","CUR") le eps)))

                     ) / (  DATA(RM,"Arm1",XXX,"CUR")    $ (not p_trim)
                                                     + 1 $ p_trim + 0.1);

*
* --- Supply balance:  supply is equal to outgoing flows (domestic sales + exports)
*                     + intervention stock change
*
*      Paired with market price v_marketPrice

*
 SupBalM_(RM,XXX) $ (not xxOMYani(XXX)) ..

     v_domSales(RM,XXX)
        /( (DATA(RM,"DSales",XXX,"CUR")+DATA(RM,"Prod",XXX,"CUR"))*0.5 $ (not p_trim)  + 1 $ p_trim + 0.1)

         =E=

        (   v_prodQuant(RM,XXX) $ DATA(RM,"Prod",XXX,"CUR")
         - v_expQuant(RM,XXX)   $ (DATA(RM,"Exports",XXX,"CUR") or p_trim)
$ifi %RP% == RMTP   - V_intervStockChange(RM,XXX) $ sum(rm_to_rmtp(rm,rmtp) $ ((DATA(RMTP,"PADM",XXX,"CUR") gt eps) $ (DATA(RMTP,"STKS",XXX,"CUR") gt eps)),1)
$ifi not %RP% == RMTP   - V_intervStockChange(RM,XXX) $ ( (DATA(RM,"PADM",XXX,"CUR") gt eps) $ (DATA(RM,"STKS",XXX,"CUR") gt eps) )
        )
        /( (DATA(RM,"DSales",XXX,"CUR")+DATA(RM,"Prod",XXX,"CUR"))*0.5 $ (not p_trim)  + 1 $ p_trim + 0.1);


* --- Imports & Exports added up of bilateral trade flows (exluding diagonal element)
*
*   paired with v_impQuant
*
 impQuant_(RM,XXX)  ..

       v_impQuant(RM,XXX)
            /(DATA(RM,"Imports",XXX,"CUR")    + 1. $ (NOT DATA(RM,"Imports",XXX,"CUR")) + 0.1  + 1 $ p_trim)

       =E= SUM(RM1 $ ((NOT SAMEAS(RM,RM1)) $ p_tradeFlows(RM,RM1,XXX,"CUR")), v_tradeFlows(RM,RM1,XXX))
            /(DATA(RM,"Imports",XXX,"CUR")    + 1. $ (NOT DATA(RM,"Imports",XXX,"CUR")) + 0.1  + 1 $ p_trim);
*
* --- First stage armington quantity aggregate in the simulation year
*     (not used in simulation model)
*
 ArmFit1_(RM,XXX) ..
*
     v_arm1Quant(RM,XXX) =E= v_arm2Quant(RM,XXX) + v_domSales(RM,XXX);
*

* --- Second stage armington quantity aggregate as used in the data
*     data balancing problems (not active in simulation model)

 ArmFit2_(RM,XXX)   ..

     v_Arm2Quant(RM,XXX) =E=

         SUM(RM1 $ ((NOT SAMEAS(RM,RM1)) $ p_tradeFlows(RM,RM1,XXX,"CUR")), v_tradeFlows(RM,RM1,XXX));


 arm2QuantShares_(RM,XXX) $ ( v_arm2Quant.up(RM,XXX) $ (v_arm2Quant.LO(RM,XXX) ne v_arm2Quant.UP(RM,XXX))) ..


     v_arm2Quant(RM,XXX)/(DATA(RM,"ARM2",XXX,"CUR")+1.)

              =E=  v_arm1Quant(RM,XXX)/(DATA(RM,"ARM2",XXX,"CUR")+1.)

                 * p_dpCESTrade(RM,"RW",XXX)

                 * [ v_arm1Price(RM,XXX) / v_arm2Price(RM,XXX) ] ** p_rhoArm1(RM,XXX) ;


 domSalesShares_(RM,XXX) $ ( v_domSales.up(RM,XXX) $ (v_domSales.LO(RM,XXX) ne v_domSales.UP(RM,XXX))) ..


     v_domSales(RM,XXX)/(p_tradeFlows(RM,RM,XXX,"CUR")+1.)

              =E=  v_arm1Quant(RM,XXX)/(p_tradeFlows(RM,RM,XXX,"CUR")+1.)

                 * p_dpCESTrade(RM,RM,XXX)

                 * [ v_arm1Price(RM,XXX) / v_marketPrice(RM,XXX) ] ** p_rhoArm1(RM,XXX);


*
*
*   ---    Demand function derived from an augmented CES formulation
*          [augmented with technological change parameters]
*

*   ---    The original CES formulation from the 'toy model'
*     demand_aug(i) ..      x(i) =e= (gamma**(sigma-1)) * (delta(i)**sigma) * (theta(i)**(rho*sigma)) * (pindex/p(i))**sigma * u;
*
*          Remarks:
*                    1. p_rhoArm2 is denoted with sigma above (same old confusion over sigma or rho...)
*                    2. the share parameters in CAPRI (p_dpCesTrade) are already on the power of sigma
*                    3. the utility 'u' above is simply scaled to total imports v_Arm2Quant
*                    4. By imposing v_gamma.fx=1 and pv_theta.fx=1 we simply fall back to the standard CES formulation in CAPRI
*


 importShares_(RM,RM1,XXX) $ ( (p_tradeFlows(RM,RM1,XXX,"Cur") or p_arm2Commit(rm,rm1,xxx)) $ (not SAMEAS(RM,RM1))
                               $  (v_tradeFlows.range(RM,RM1,XXX) or NonZeroFixedImportFlow(RM,RM1,XXX) or pv_theta.range(rm,rm1,xxx))) ..


     ( v_tradeFlows(RM,RM1,XXX)   $ (v_tradeFlows.lo(RM,RM1,XXX) gt 0)
     + v_tradeFlowNeg(RM,RM1,XXX) $ (v_tradeFlows.lo(RM,RM1,XXX) le 0))

*                 scaling
                  /(p_tradeFlows(RM,RM1,XXX,"Cur")+1.)

              =E= [v_arm2Quant(RM,XXX)

*                 Hicks neutral techn. change   (or better preference change)  combined with the CES level param.
*                 [see equation HicksNeutral_]
                 *  v_gammaCES(rm, xxx) ** (p_rhoArm2(rm, xxx) - 1)

*                 share params. (already at the power of sigma)
                 * p_dpCESTrade(RM,RM1,XXX)

*                 Hicks non-neutral techn. change    (or better preference change)
                 *  pv_theta(rm,rm1,xxx) ** (p_realRhoArm2(rm,xxx) * p_rhoArm2(rm,xxx))

*                 change in shares driven by changes in relative prices
                 * (  v_arm2Price(RM,XXX)
                   / (   v_impPrice(RM,RM1,XXX)       $ (NOT NonZeroFixedImportFlow(RM,RM1,XXX))
                     +   v_impShadowPrice(RM,RM1,XXX) $ (    NonZeroFixedImportFlow(RM,RM1,XXX)) )) ** p_rhoArm2(RM,XXX)

*                 commitment term
                 + p_arm2Commit(RM,RM1,XXX)]

*                 scaling
                  /(p_tradeFlows(RM,RM1,XXX,"Cur")+1.)
    ;


 tradeFlowFudge_(RM,RM1,XXX) $ ( (p_arm2Commit(rm,rm1,xxx)) $ (not SAMEAS(RM,RM1))
                               $ (v_tradeFlows.range(RM,RM1,XXX) or NonZeroFixedImportFlow(RM,RM1,XXX) or pv_theta.range(rm,rm1,xxx))) ..
*
* --- Fisher-Baumeister smoothing function (sqrt(sqr(x) + sqr(y) + 2*z)
*     in case Human consumption gets negative
*
     v_tradeFlows(RM,RM1,XXX)
            /(p_tradeFlows(RM,RM1,XXX,"Cur")+1.)
        =E=  (-ncpcm(-v_tradeFlowNeg(RM,RM1,XXX),0,1.E-2*min(1000,max(1.E-11,p_tradeFlows(RM,RM1,XXX,"Cur")))) + 1.E-11)
            /(p_tradeFlows(RM,RM1,XXX,"Cur")+1.);

*
*   ---    Additional equations for the Kuiper van-Tongeren approach
*          Note that fixing by v_gammaCES (e.g. at unity) the equation drops out
*
 HicksNeutral_(rm, xxx)  $  ( (sum(rm1, p_tradeFlows(RM,RM1,XXX,"Cur")) or sum(rm1, p_arm2Commit(rm,rm1,xxx)))
                         $  (sum(rm1, v_tradeFlows.range(RM,RM1,XXX))  or sum(rm1, NonZeroFixedImportFlow(RM,RM1,XXX)))
                         $  v_gammaCES.range(rm, xxx) )..

    v_gammaCES(rm, xxx) =e= v_Hicks(rm, xxx) * v_gammaCES.l(rm, xxx);


*
*   ---    Transition function for the extended KVT approach
*
 transition_(rm, rm1, xxx) $ pv_theta.range(rm,rm1,xxx)  ..

   pv_theta(rm, rm1, xxx) =e= p_transitB(rm, rm1, xxx) * [(v_impPrice(rm, rm1, xxx) / v_arm2Price(rm, xxx))
                                       / ( p_impPrice(rm, rm1, xxx, "cur") / data(rm, "Arm2P", xxx, "cur"))]
                              + p_transitA(rm, rm1, xxx) * [(v_impPrice(rm, rm1, xxx) / v_arm2Price(rm, xxx))
                                        / (p_impPrice(rm, rm1, xxx, "cur") / data(rm, "Arm2P", xxx, "cur")) - 1] ;


*
*   ---    CES aggregator for the KVT approach (included here only for testing purposes, i.e. not part of the demand system)
*
*   ces_aug ..   u =e= (gamma * Hicks) * sum(i, delta(i) * ((x(i)*theta(i))**rho))**(1/rho);
*
*         Note that as v_Arm2Quant is fixed in normal simulations, this equation is not part of the 'normal' simulation model
*


  CESAugmented_(rm, xxx)  $  ( (sum(rm1, p_tradeFlows(RM,RM1,XXX,"Cur")) or sum(rm1, p_arm2Commit(rm,rm1,xxx)))
                          $   v_Arm2Quant.range(rm, xxx) )..

     v_Arm2Quant(rm, xxx)

      =e=   (v_gammaCES(rm, xxx) * v_Hicks(rm, xxx))

             * sum(rm1,  p_deltaCES(rm, rm1, xxx)
                         * (v_tradeFlows(rm, rm1, xxx) * pv_theta(rm, rm1, xxx)) ** p_realRhoArm2(rm, xxx))

                 **(1/p_realRhoArm2(rm, xxx));




*
* --------------- PRICES ------------------------------------------------------
*
* --- Import price relation to producer price
*      (attention: ExpSub and Tariff may be endogenous variables)
*
*      paired with v_impPrice
*
 impPrice_(RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"CUR") or p_arm2Commit(rm,rm1,xxx)) $ (not SAMEAS(RM,RM1))) ..

     v_impPrice(RM,RM1,XXX)/(p_impPrice(RM,RM1,XXX,"CUR")+1)   =E=
*

*   bracket of the scaling divisor
 {


       [v_marketPrice(RM1,XXX)
*
*       --- transport costs
*
       + v_transpCost(RM,RM1,XXX)



$iftheni.impprice %RP% == RMTP


*       --- export subsidies
*
       - sum(RM_TO_RMTP(rm1,rmtp1), v_perUnitExportSub(RMtp1,XXX)  $ ( (DATA(RMtp1,"PADM",XXX,"CUR") gt eps) $ (DATA(RMtp1,"FEOE_max",XXX,"CUR") GT eps)
                               $ (NOT sum(RM1_TO_RMTP(rm,rmtp)  , p_doubleZero(RMtp,RMtp1,XXX,"CUR") ))))
*      first closing bracket
*             (exchange rate adjustment applies to marketPrice, transpCost [if handled by exporting country] and ExportSub)
            ] * p_exchgRateChangeFactor(RM,RM1)

*
*       --- add valorem tariff
*
          *  [ 1. + 0.01 * sum( (rmtp,rmtp1) $ (RM_TO_RMTP(rm,rmtp) $ RM_TO_RMTP(rm1,rmtp1)), v_tarAdVal(RMTP,RMTP1,XXX)
                                                       $ ( (NOT  p_doubleZero(RMTP,RMTP1,XXX,"CUR")) $ (NOT SAMEAS(RMTP,RMTP1))))]
*
*       --- specific tariffs
*
          +  [

*              --- fixed according to tariff schedule or endogenous under TRQ
              sum((RM_TO_RMTP(rm,rmtp),RM1_TO_RMTP(rm1,rmtp1)), v_tarSpec(RMTP,RMTP1,XXX)
                                                        $ (   (DATA(RMTP,"MinBordP",XXX,"CUR") LE eps)
                                                           or (p_trqBilat(RMTP,RMTP1,XXX,"TrqNT","CUR") eq prohibitive) ))

*                --- or flexible levy in case of minimum border prices
              +  sum((RM_TO_RMTP(rm,rmtp),RM1_TO_RMTP(rm1,rmtp1)), v_flexLevy(RMTP,RMTP1,XXX)
                                                        $ (   (DATA(RMtp,"MinBordP",XXX,"CUR") GT eps)
                                                            $ (p_trqBilat(RMtp,RMtp1,XXX,"TrqNT","CUR") ne prohibitive) ))

              ] $ [ (NOT sum((RM_TO_RMTP(rm,rmtp),RM1_TO_RMTP(rm1,rmtp1)),p_doubleZero(RMTP,RMTP1,XXX,"CUR")))
                                             $ (not same_pblock(rm,rm1))]



$else.impprice


       - (v_perUnitExportSub(RM1,XXX) )  $ ( (DATA(RM1,"PADM",XXX,"CUR") gt eps) $ (DATA(RM1,"FEOE_max",XXX,"CUR") GT eps)
                               $ (NOT p_doubleZero(RM,RM1,XXX,"CUR")) $ (NOT SAMEAS(RM,RM1) ))
*      first closing bracket
*             (exchange rate adjustment applies to marketPrice, transpCost [if handled by exporting country] and ExportSub)
            ] * p_exchgRateChangeFactor(RM,RM1)

          *  ( 1. + 0.01*v_tarAdVal(RM,RM1,XXX) $ ( (NOT p_doubleZero(RM,RM1,XXX,"CUR")) $ (NOT SAMEAS(RM,RM1))))

          +  [

*              --- fixed according to tariff schedule or endogenous under TRQ
                v_tarSpec(RM,RM1,XXX) $ (   (DATA(RM,"MinBordP",XXX,"CUR") LE eps)
                                         or (p_trqBilat(RM,RM1,XXX,"TrqNT","CUR") eq prohibitive) )

*                --- or flexible levy in case of minimum border prices
              + v_flexLevy(RM,RM1,XXX)  $ (     (DATA(RM,"MinBordP",XXX,"CUR") GT eps)
                                            $ (p_trqBilat(RM,RM1,XXX,"TrqNT","CUR") ne prohibitive) )

              ] $ ((NOT p_doubleZero(RM,RM1,XXX,"CUR")) $ (NOT SAMEAS(RM,RM1)))


$endif.impprice

*   closing bracket for the scaling
 }

                  /(p_impPrice(RM,RM1,XXX,"CUR")+1) ;
*
* --- Consumer price as average of domestic and import prices
*     weighted with import Armington utility aggregate or domestic sales, divided by Armington utility aggregator
*
*      TrimFitit = 1 will put the aggregate Arm1 on the left hand side of the equation. That leads to a somewhat
*      less fortunate formulation for the solver bu prevents division by zero of Arm1 is moved to zero. Used
*      wil trimming the framework (FITIT1 and FITIT6 models)
*
*      paired with v_arm1Price - not used in simulation model any longer
*
*
 arm1Price_(RM,XXX) $ ( (v_domSales.up(RM,XXX) ne 0) or (v_Arm2Quant.up(RM,XXX) ne 0) ) ..
*

      v_arm1Price(RM,XXX) * v_Arm1Quant(RM,XXX)/1000.
           / (DATA(RM,"arm1P",XXX,"CUR")+1)
*
       =E=  (
                     v_marketPrice(RM,XXX) * v_domSales(RM,XXX) /1000
                   + v_arm2Price(RM,XXX)   * v_arm2Quant(RM,XXX)/1000
            )
            /(DATA(RM,"arm1P",XXX,"CUR")+1) ;

*
* --- Definition of arm1Price
*
*
*      paired with v_arm1Price
*
 arm1Val_(RM,XXX) $ ( (v_domSales.up(RM,XXX) ne 0) or (v_Arm2Quant.up(RM,XXX) ne 0) )..
*


       v_arm1Price(RM,XXX) * v_arm1Quant(RM,XXX) * 0.001
     / (data(RM,"Arm1P",XXX,"CUR") * DATA(RM,"Arm1",XXX,"CUR") * 0.001 + 1)

       =E= (   v_marketPrice(RM,XXX) * v_domSales(RM,XXX)  $ DATA(RM,"DSales",XXX,"CUR")
             + v_arm2Price(RM,XXX)   * v_arm2Quant(RM,XXX) $ DATA(RM,"Arm2",XXX,"CUR")   ) * 0.001
     / (data(RM,"Arm1P",XXX,"CUR") * DATA(RM,"Arm1",XXX,"CUR") * 0.001 + 1);

 arm1PriceAgg_(RM,XXX) $ ( (v_domSales.up(RM,XXX) ne 0) or (v_Arm2Quant.up(RM,XXX) ne 0) )..

     v_arm1Price(RM,XXX) / (data(RM,"Arm1P",XXX,"CUR")+1) =E=

       (  (p_dpCesTrade(RM,RM,XXX)   * (v_marketPrice(RM,XXX)/ (data(RM,"Arm1P",XXX,"CUR")+1)) ** (1-p_rhoArm1(RM,XXX))) $ p_dpCesTrade(RM,RM,XXX)
        + (p_dpCesTrade(RM,"RW",XXX) * (v_arm2Price(RM,XXX)  / (data(RM,"Arm1P",XXX,"CUR")+1)) ** (1-p_rhoArm1(RM,XXX))) $ p_dpCesTrade(RM,"RW",XXX)
        ) ** (1/(1-p_rhoArm1(RM,XXX)));

*
* --- Average price as average of different import prices (excluding domestic)
*      weighted with imported quantities divided by Armington utility aggregate
*
*      TrimFitit = 1 will put the aggregate Arm2 on the left hand side of the equation. That leads to a somewhat
*      less fortunate formulation for the solver bu prevents division by zero of Arm1 is moved to zero. Used
*      wil trimming the framework (FITIT1 and FITIT6 models)
*
*      Paired with v_arm2Price - not used in simulation model any longer
*
 arm2Price_(RM,XXX) $ ( v_Arm2Price.lo(RM,XXX) ne v_Arm2Price.up(RM,XXX)) ..


      v_arm2Price(RM,XXX) * v_Arm2Quant(RM,XXX)/1000.
          / (DATA(RM,"arm2P",XXX,"CUR") + 1)

         =E=     v_arm2Val(RM,XXX)
          / (DATA(RM,"arm2P",XXX,"CUR")+1);

*
*
* --- arm2Val_ defines (after a long and winding road of changed formuations)
*     now the Armington two price =average import prices) by dviding the value of imports
*     (= trade flows times attached import prices) by the Armington 2 aggregate (= utility aggregate)
*
*     Two notes: (1) The 1.E-30 are the leftovers of trials when the model went hayward to keep some
*                    weight for import prices entering the equation when the trade flows were really small.
*                    As it cannot be excluded that it is needed, the term is still there but should not have
*                    an effect on the numerics.
*
*                 (2) Note the 1.E-10 added to be divisor to avoid zero division during calibration where
*                     the ARM2Quant variable is allowed to become zero.
*
*
 arm2Val_(RM,XXX) $ ( v_Arm2Val.lo(RM,XXX) ne v_Arm2Val.up(RM,XXX)) ..

    v_arm2Price(RM,XXX) * v_arm2Quant(RM,XXX) * 0.001
    / ( DATA(RM,"Arm2P",XXX,"CUR") * DATA(RM,"Arm2",XXX,"CUR") * 0.001 + 1)

       =E=
             [SUM(RM1 $ ((NOT SAMEAS(RM,RM1)) $ p_tradeFlows(RM,RM1,XXX,"CUR")),
                      v_impPrice(RM,RM1,XXX)*v_tradeFlows(RM,RM1,XXX))/1000.]
    / ( DATA(RM,"Arm2P",XXX,"CUR") * DATA(RM,"Arm2",XXX,"CUR") * 0.001 + 1);
*



*
*   ---    some add-ons to the price index equations for implementing the KVT approach
*
*          Note that by fixing both v_gammaCES.fx=1 and pv_theta.fx=1 we get back the standard CAPRI price index equation
*
*   For reference, see the price index equation from the 'toy model':
*    index_aug ..        pindex =e= (1/gamma) * sum(i, (delta(i)**sigma) * (theta(i)**(sigma-1)) * (p(i)**(1-sigma)))**(1/(1-sigma));


 arm2PriceAgg_(RM,XXX) $ ( v_Arm2Val.lo(RM,XXX) ne v_Arm2Val.up(RM,XXX)) ..
     v_arm2Price(RM,XXX) / (data(RM,"Arm2P",XXX,"CUR")+1) =E=

*    Pref. change prameter from the augmented CES:
     (1 / v_gammaCES(rm, xxx)) *


     sum(RM1 $ ((p_tradeFlows(RM,RM1,XXX,"Cur") or p_arm2Commit(rm,rm1,xxx)) $ (not SAMEAS(RM,RM1)) $  p_dpCesTrade(RM,RM1,XXX)),

*       the Armington shifters:
             (pv_theta(rm, rm1, xxx)**(p_rhoArm2(rm, xxx) - 1)) *

*       the usual CES price index formulation:
             p_dpCesTrade(RM,RM1,XXX) * ((   v_impPrice(RM,RM1,XXX)       $ (NOT NonZeroFixedImportFlow(RM,RM1,XXX))
                                     +   v_impShadowPrice(RM,RM1,XXX) $ (    NonZeroFixedImportFlow(RM,RM1,XXX)) )
                                    /(data(RM,"Arm2P",XXX,"CUR")+1)   ) ** (1-p_rhoArm2(RM,XXX))
        ) ** (1/(1-p_rhoArm2(RM,XXX)));
*
* --- Consumer prices equal Armington price index plus Armington minus policy support via CSE
*
 Cpri_(RMS,XXX) $ ( (DATA(RMS,"HCon",XXX,"CUR") or DATA(RMS,"BIOF",XXX,"CUR") $XXBIOF(XXX)) ) ..

     (  v_consPrice(RMS,XXX)    $ (not v_consPriceNeg.range(RMS,XXX))
      + v_consPriceNeg(RMS,XXX) $ (    v_consPriceNeg.range(RMS,XXX)))
    /(DATA(RMS,"CPRI",XXX,"CUR")+1)
                           =E= SUM(RMS_TO_RM(RMS,RM), v_arm1Price(RM,XXX)
                             + DATA(RMS,"CTAX",XXX,"CUR") $ XXBIOF(XXX)
                             + DATA(RMS,"CMrg",XXX,"CUR")
*                              Calory tax as a hypothetical instrument to influence calory intake (possibly fixed to zero)
                             + v_caloTax(RMS) * p_calContent(XXX) $ (DATA(RMS,"LOSMsh",XXX,"CUR") lt 1)
                             - DATA(RMS,"CSEd",XXX,"CUR")
                             - DATA(RMS,"CSEi",XXX,"CUR"))/(DATA(RMS,"CPRI",XXX,"CUR")+1);

* --- Fudging function that transforms negative numbers into small positive numbers (smooth max(0,consPriceNeg) operator)
*     in case consumer prices from CPRI_ get negative due to the calory tax (possibly < 0, so subsidies)
*     Note: currently v_consPriceNeg.range > 0 only for {BARL,RAPE,set.CAK}, see set_start_val.gms
*
 consPriceFudge_(RMS,XXX) $ ( v_consPriceNeg.range(RMS,XXX)
                            $ ( (DATA(RMS,"HCon",XXX,"CUR") or DATA(RMS,"BIOF",XXX,"CUR") $XXBIOF(XXX)) ) )..
     v_consPrice(RMS,XXX)
       /(abs(DATA(RMS,"Cpri",XXX,"CUR"))+0.1)
        =E=  (-ncpcm(-v_consPriceNeg(RMS,XXX),0,1.E-2*min(10000,max(1.E-2,data(RMS,"Cpri",XXX,"CUR")))) + 1.E-2)
      /(abs(DATA(RMS,"Cpri",XXX,"CUR"))+0.1);


*
* --- Producer price
*
 PPri_(RMS,XXX) $ ( (DATA(RMS,"Prod",XXX,"CUR")) or xxOmYani(XXX))  ..

      v_prodPrice(RMS,XXX)/(DATA(RMS,"PPRI",XXX,"CUR")+1)
         =E=
*     ---  market prices
          (SUM(RMS_TO_RM(RMS,RM),v_marketPrice(RM,XXX))
*     --- producer price margin - either additive or relative - default is relative
             * (pv_prodPriceMarg(RMS,XXX) $ (not p_additivePPriMargin(XXX)) + 1 $ p_additivePPriMargin(XXX))
              + pv_prodPriceMarg(RMS,XXX) $      p_additivePPriMargin(XXX)

*     --- plus support meassures - all transfered into per ton definition
             + DATA(RMS,"PSEi",XXX,"CUR")
             + DATA(RMS,"PSEd",XXX,"CUR")
*     --- and miltiplied with coupling degree for area payments
             + DATA(RMS,"AREP",XXX,"CUR")*(1-DATA(RMS,"DECP",XXX,"CUR")))/(DATA(RMS,"PPRI",XXX,"CUR")+1);
*
* --- Market price = calibrated to average domestic farm gate price,
*                    the f.o.b. price for region RM
*
*                    producer prices are linked with multiplicative margins
*                    (= Price transmission elasticity equal to unity)
*                    to that price, see PPRI_ equation
*
*                    Beware: equation only active if no production (and hence no dom sales). Normal case
*                    is that the supbalm_ equation drives the v_marketPrice
*
*
 PMrk_(RM,XXX)  $  (   ( (NOT v_prodQuant.UP(RM,XXX))
                       $ (not v_domSales.up(RM,XXX)))
*                    Sometimes widen_bounds decreases lower bounds until variables are close to zero
*                    and Gams fixes them but does not activate PMRK_ because fixation is "near zero" (<1E-10)
*                    To prevent non-square systems we explicitly introduce the case of fixed (or dropping out) variables
*                    in supbalm_ (which is dropped then) in order to activate PMRK_ then:
                    or ( (not v_prodQuant.range(RM,XXX))
                       $ (not v_domSales.range(RM,XXX))
                       $ (not (DATA(RM,"Exports",XXX,"CUR") or p_trim))) ) ..
*PMrk_(RM,XXX)  $  (NOT (v_prodQuant.UP(RM,XXX) or (v_domSales.up(RM,XXX)))) ..
*PMrk_(RM,XXX)  $ ((NOT v_prodQuant.UP(RM,XXX)) or (    v_domSales.lo(RM,XXX) eq v_domSales.up(RM,XXX))
*                                                   and v_prodQuant.lo(RM,XXX) eq v_prodQuant.up(RM,XXX)) ..
*
      v_marketPrice(RM,XXX)/(DATA(RM,"PMrk",XXX,"CUR")+1)
           =E= ( v_arm1Price(RM,XXX) + DATA(RM,"PMrkMrg",XXX,"CUR") $ (NOT v_prodQuant.UP(RM,XXX)))
               /(DATA(RM,"PMrk",XXX,"CUR")+1);

*
$iftheni.aggregators2 %RP% == RMTP
*
* --- average market price at RMTP level (over the ARM1 quantities)
*     [there are some countries that only do re-export, in this case there is no ARM1 but usually there is some PROD]
*     check if v_marketPrice is fixed in order to avoid activating non-paired market price variables => that would result in a non-squared system

MarketPriceAgg_(RMTP,XXX) $ { (not xxOmYani(XXX))
                            $ SUM(RM $  RMTP_TO_RM(RMTP,RM),  v_marketPrice.range(RM,XXX) )
                            $ [ SUM(RM $  RMTP_TO_RM(RMTP,RM),  (v_Arm1Quant.UP(RM,XXX) gt eps) )
                              + SUM(RM $  RMTP_TO_RM(RMTP,RM),  (v_prodQuant.UP(RM,XXX) gt eps) ) ]
                            $ RMTPagg(RMTP)
                            }..
*
      v_marketPrice(RMTP,XXX)  =E=
*
        {  SUM[RMTP_TO_RM(RMTP,RM),
              v_marketPrice(RM,XXX) * (1 + v_Arm1Quant(RM,XXX) + v_prodQuant(rm,xxx) $ (not DATA(RM,"ARM1",XXX,"CUR")))]
*
        /  SUM[RMTP_TO_RM(RMTP,RM),    1 + v_Arm1Quant(RM,XXX) + v_prodQuant(rm,xxx) $ (not DATA(RM,"ARM1",XXX,"CUR"))] } ;

*
* --- aggregates trade flows from RM to RMTP level (intra trade is excluded by definition)
*
*
 TradeFlowsAgg_(RMTP,RMTP1,XXX) $  {  (not sameas(RMTP,RMTP1))
                                   $  (RMTPagg(RMTP) or RMTPagg(RMTP1))
                                   $  p_tradeFlows(RMTP,RMTP1,XXX,"CUR")
                                   }..

      v_tradeFlows(RMTP,RMTP1,XXX)   =E=

        {   SUM((RMTP_TO_RM(RMTP,RM),RMTP_TO_RM1(RMTP1,RM1)) $ p_tradeFlows(RM,RM1,XXX,"CUR"),
                                                               v_tradeFlows(RM,RM1,XXX)) }   ;

*
* --- average transportation costs over RMTPs
*
 TransportCostsAgg_(RMTP,RMTP1,XXX) $ [   (not sameas(RMTP,RMTP1))
                                        $ (RMTPagg(RMTP) or RMTPagg(RMTP1))
                                        $ p_tradeFlows(RMTP,RMTP1,XXX,"CUR") ]..

      v_transpCost(RMTP,RMTP1,XXX) =E=

        { SUM((RMTP_TO_RM(RMTP,RM),RMTP_TO_RM1(RMTP1,RM1)), v_transpCost(RM,RM1,XXX) * p_tradeFlows(RM,RM1,XXX,"CUR"))
        / SUM((RMTP_TO_RM(RMTP,RM),RMTP_TO_RM1(RMTP1,RM1)), p_tradeFlows(RM,RM1,XXX,"CUR")) }  ;


$endif.aggregators2
*
*
* --------------- Policy representation ---------------------------------------
*
* --- TRQS: MCP formulation endogenous tariff linked to import quota

*      => paired with tariffs Tariff(RM,RM1,XXX) (o.k.)

*
* --- Sum of imports under a non allocated TRQ
*     The underlying assumption here is that bilateral quotas are filled first
*     Multilateral TRQs are used only after bilateral ones are already filled
*
TRQImports_ (%RP%,XXX) $ ( p_trqGlobl(%RP%,XXX,"TrqNT","Cur") gt eps) ..

    v_TRQImports(%RP%,XXX)/(p_trqGlobl(%RP%,XXX,"TrqNT","Cur")+1)
        =E=  SUM(%RP%1 $ ( (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR"))
                         $ (NOT SAMEAS(%RP%,%RP%1))
                         $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                         $ (NOT p_trqBilat(%RP%,%RP%1,XXX,"TrqNt","CUR") eq prohibitive)),


*
*         --- trade flows in case there is no bi-lateral TRQ defined
*
              v_tradeFlows(%RP%,%RP%1,XXX)
               $ ( (p_trqBilat(%RP%,%RP%1,XXX,"TrqNt","CUR") le eps) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx)))
*
*         --- overfill of bilateral imports: trade flows exceeding the bilateral quota
*
          + (-ncpcm(-(v_tradeFlows(%RP%,%RP%1,XXX)-p_trqBilat(%RP%,%RP%1,XXX,"TrqNt","CUR")),
                                          0,1.E-3*p_trqBilat(%RP%,%RP%1,XXX,"TrqNt","CUR")))
               $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNt","CUR") gt eps)
                                                              )/(p_trqGlobl(%RP%,XXX,"TrqNT","Cur")+1);

*
* --- Complex sigmoid function which determines the share of the tariff applied for the EU entry
*     price system for fruits and vegetables
*
*     Entry price system of the EU for fruits and vegetables:
*
*        Tariff is zero at 98% of trigger prices, reduces linear along CIF+Tariff
*         = Triggerprice until CIF=92% of trigger price,
*        and then jumps to MFN bound rate.
*        That is smoothed by using a non-symmetric sigmoid function
*

 EntryPriceDriver_(%RP%,%RP%1,XXX) $ (  (DATA(%RP%,"TriggerP",XXX,"CUR") gt eps)
                                   $ ((v_entryPriceDriver.LO(%RP%,%RP%1,XXX) NE v_entryPriceDriver.up(%RP%,%RP%1,XXX)) or p_Trim)
                                   $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                                   $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR"))
                                   $ (NOT SAMEAS(%RP%,%RP%1))) ..

  v_entryPriceDriver(%RP%,%RP%1,XXX) =E=
*
             (v_entryPrice(%RP%,%RP%1,XXX) *(0.98+0.92)/2

*                                            if transport is handled by exporter exchange rate adjustment applies to both
                 - (v_marketPrice(%RP%1,XXX)+v_transpCost(%RP%,%RP%1,XXX))*p_exchgRateChangeFactor(%RP%,%RP%1))

                        / DATA(%RP%,"TriggerP",XXX,"CUR") * p_entryPriceFac(%RP%,%RP%1,XXX,"CUR");


*
* --- Apply the driver defined above via a second sigmoid function to the bound rate
*

*
*     Entry price system of the EU for fruits and vegetables:
*
*        Tariff is zero at 98% of trigger prices, reduces linear along CIF+Tariff
*         = Triggerprice until CIF=92% of trigger price,
*        and then jumps to MFN bound rate. That is smoothed by using a non-symmetric sigmoid function
*

 tarSpecIfEntryPrice_(%RP%,%RP%1,XXX) $ (    (DATA(%RP%,"TriggerP",XXX,"CUR") gt eps)
                            $ ((v_tarSpec.LO(%RP%,%RP%1,XXX) NE v_tarSpec.up(%RP%,%RP%1,XXX)) or p_Trim)
                            $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                            $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (NOT SAMEAS(%RP%,%RP%1))) ..


   v_tarSpec(%RP%,%RP%1,XXX)/DATA(%RP%,"TARS",XXX,"CUR") =E=
*
       (EXP( MIN(0,v_entryPriceDriver(%RP%,%RP%1,XXX)))/(1+20*EXP(-ABS(v_entryPriceDriver(%RP%,%RP%1,XXX)))))
                        /DATA(%RP%,"TARS",XXX,"CUR")

             * (  DATA(%RP%,"TARS",XXX,"CUR")              $   (  P_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR") le eps)
                + p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR")  $   (  p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR") gt eps));


*
* --- Cif price (not used in model)
*     Note: We assume that transport is handled by the exporter and transport costs are incurred in the exporters currency

 cifPrice_(RM,RM1,XXX) $ ((DATA(RM,"MinBordP",XXX,"CUR") gt eps)
                                    $ ( v_flexLevy.lo(RM,RM1,XXX) ne v_flexLevy.up(RM,RM1,XXX))
                                    $ (p_tradeFlows(RM,RM1,XXX,"CUR") or p_arm2Commit(rm,rm1,xxx))
                                    $ (NOT p_doubleZero(RM,RM1,XXX,"CUR")) $ (NOT SAMEAS(RM,RM1))) ..
*
    v_cifPrice(RM,RM1,XXX) =E= (v_marketPrice(RM1,XXX)+v_transpCost(RM,RM1,XXX))*p_exchgRateChangeFactor(RM,RM1);

*
* --- Define levy (replaces tariff) in case of minimal border prices, ensure that it does not get negative
*     (may be turned into a "classical levy" without a bound rate by fixing "tariffs" to a high number)
*


 FlexLevyNotCut_(%RP%,%RP%1,XXX) $ ((DATA(%RP%,"MinBordP",XXX,"CUR") gt eps)
                                    $ ( v_flexLevy.lo(%RP%,%RP%1,XXX) ne v_flexLevy.up(%RP%,%RP%1,XXX))
                                    $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                                    $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (NOT SAMEAS(%RP%,%RP%1))
                                    ) ..
*
*    MAX(0,Min. Border price - CIF)
*
   v_flexLevyNotCut(%RP%,%RP%1,XXX)/DATA(%RP%,"MinBordP",XXX,"CUR") =E=

          -ncpcm( -(  DATA(%RP%,"MinBordP",XXX,"CUR")

*                                              (assume that transport is handled by exporter)
                    -(v_marketPrice(%RP%1,XXX)+v_transpCost(%RP%,%RP%1,XXX))*p_exchgRateChangeFactor(%RP%,%RP%1)),

                         0,1.E-3*DATA(%RP%,"MinBordP",XXX,"CUR"))
                               /DATA(%RP%,"MinBordP",XXX,"CUR");
*
*
* --- Cut flexible levy by specific tariff if it exceeds the bound rate
*     (may be turned into a "classical levy" without a bound rate by fixing "tariffs" to a high number)
*
 FlexLevy_(%RP%,%RP%1,XXX) $ ( (DATA(%RP%,"MinBordP",XXX,"CUR") gt eps)
                                    $ ( v_flexLevy.lo(%RP%,%RP%1,XXX) ne v_flexLevy.up(%RP%,%RP%1,XXX))
                                    $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                                    $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (NOT SAMEAS(%RP%,%RP%1))
                                    ) ..
*
*   MIN( DiffLevies1 =  MAX(0, Min. Border price - CIF), Bound Rate resp. tariff under TRQ)
*
        v_flexLevy(%RP%,%RP%1,XXX)/DATA(%RP%,"MinBordP",XXX,"CUR")
           =E= ncpcm(v_flexLevyNotCut(%RP%,%RP%1,XXX),v_tarSpec(%RP%,%RP%1,XXX),1.E-3*DATA(%RP%,"MinBordP",XXX,"CUR"))
                              /DATA(%RP%,"MinBordP",XXX,"CUR");

*
* --- Sigmoid function to drive tariffs / entry prices under bi-lateral TRQs
*     The 0.97 define the point where the function is at
*
*
 trqSigmoidFunc_(%RP%,%RP%1,XXX) $

             {
               [ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") GT eps) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE 1.E+10)
                 $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE prohibitive) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))]
             } ..


         v_trqSigmoidFunc(%RP%,%RP%1,XXX) =E= Sigmoid(p_trqSigmoidSlope(%RP%,XXX)/p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR")
                                                   * (v_tradeFlows(%RP%,%RP%1,XXX)-p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR")*0.97));


*
*
* --- Specific tariffs as function of import quantities if TRQ is present
*

 tarSpec_(%RP%,%RP%1,XXX) $ ( ((v_tarSpec.LO(%RP%,%RP%1,XXX) NE v_tarSpec.up(%RP%,%RP%1,XXX)) or p_trim) $
*
*               --- bilateral TRQ (excluding the case of unlimited access at no or reduced tariff)
*
        ((       ( (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") GT eps) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE 1.E+10))
              $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE prohibitive)

             $   (       ( (p_trqBilat(%RP%,%RP%1,XXX,"TsMFN","CUR") gt EPS) OR (p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR") gt eps))
                        $ (p_trqBilat(%RP%,%RP%1,XXX,"TsMFN","CUR") ne p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR"))
                        $ (DATA(%RP%,"TriggerP",XXX,"CUR") le eps) ) )

*
*               --- global TRQ (but excluding preferrential access)
*
           or ( (p_trqGlobl(%RP%,XXX,"TrqNT","CUR") gt eps) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE 1.E+10)
               $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE prohibitive)
               $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (DATA(%RP%,"TriggerP",XXX,"CUR") le eps)
               $ ( (p_trqGlobl(%RP%,XXX,"TsMFN","CUR") gt eps)  OR (p_trqGlobl(%RP%,XXX,"TsPref","CUR") gt eps )))
         )

                $ (not Sameas(%RP%,%RP%1)) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))) ..

            v_tarSpec(%RP%,%RP%1,XXX)
         /(   MAX(p_trqBilat(%RP%,%RP%1,XXX,"TsMFN","CUR"),p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR"))
            + MAX(p_trqGlobl(%RP%,XXX,"TsMFN","CUR"),p_trqGlobl(%RP%,XXX,"TsPref","CUR")))

*
         =E=
*
        (
*
*         --- bilaterally allocated TRQs: the applied tariffs is the smaller one
*                                         from the TRQ and the global TRQ
*
       + (    ncpcm(   v_tarSpec(%RP%,"RW",XXX),
                       p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR")

                     + (  p_trqBilat(%RP%,%RP%1,XXX,"TsMFN","CUR") - p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR"))

                        * v_trqSigmoidFunc(%RP%,%RP%1,XXX),max(1.E-2,1.E-3*ABS( p_trqBilat(%RP%,%RP%1,XXX,"TsMFN","CUR")
                                                                              - p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR")))))
                                        $ (     (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") gt eps)
                                              $ (p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR") le EPS)
                                              $ (p_trqGlobl(%RP%,XXX,"TrqNt","CUR") gt eps)
                                              $ ( (p_trqGlobl(%RP%,XXX,"TsPref","CUR") gt eps) or  (p_trqGlobl(%RP%,XXX,"TsMFN","CUR") gt eps)))


*
*       --- bilaterally allocated TRQs and no global TRQ
*
       + (  p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR")
             + (  p_trqBilat(%RP%,%RP%1,XXX,"TsMFN","CUR") - p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR"))
                    * v_trqSigmoidFunc(%RP%,%RP%1,XXX))
                                        $ (     (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") gt eps)
                                              $ (p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR") le EPS)
                                              $ (       (p_trqGlobl(%RP%,XXX,"TrqNt","CUR") le eps)
                                                    or  ( (p_trqGlobl(%RP%,XXX,"TsPref","CUR") le eps)
                                                        $ (p_trqGlobl(%RP%,XXX,"TsMFN","CUR") le eps))))


*
*       --- not bilaterally allocated TRQs
*
       + v_tarSpec(%RP%,"RW",XXX) $ ( (p_trqGlobl(%RP%,XXX,"TrqNt","CUR") gt eps)
                                    $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") le eps))

         )
         /(   MAX(p_trqBilat(%RP%,%RP%1,XXX,"TsMFN","CUR"),p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR"))
            + MAX(p_trqGlobl(%RP%,XXX,"TsMFN","CUR"),p_trqGlobl(%RP%,XXX,"TsPref","CUR")))
       ;
*
*
* --- Specific tariffs as function of import quantities if TRQ is present
*

 prefTriggerPrice_(%RP%,%RP%1,XXX) $ ( ((v_tarSpec.LO(%RP%,%RP%1,XXX) NE v_tarSpec.up(%RP%,%RP%1,XXX)) or p_trim) $
*
*               --- bilateral TRQ (excluding the case of unlimited access at no or reduced tariff)
*
        (       ( (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") GT eps) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE 1.E+10))
               $  (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE prohibitive)
               $  (p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR") gt EPS))

                $ (not Sameas(%RP%,%RP%1)) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))) ..


            v_entryPrice(%RP%,%RP%1,XXX)
         /  p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR")

*
         =E=
*

*
*        --- bilaterally allocated TRQs linked to preferrential trigger price
*
         [  p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR")
           + (DATA(%RP%,"TriggerP",XXX,"CUR")  - p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR"))
              * v_trqSigmoidFunc(%RP%,%RP%1,XXX) ]
         /  p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR")
       ;

*
* --- Tariffs under globally open (= not bilaterally allocated) TRQs
*
 tarSpecW_(%RP%,"Rw",XXX) $ (  (p_trqGlobl(%RP%,XXX,"TrqNt","CUR")   gt eps)
                           $   (p_trqGlobl(%RP%,XXX,"TsPref","CUR")  lt p_trqGlobl(%RP%,XXX,"TsMFN","CUR"))
                           $   (   (p_trqGlobl(%RP%,XXX,"TsPref","CUR")  gt eps)
                                or (p_trqGlobl(%RP%,XXX,"TsMFN","CUR")   gt eps)) ) ..
*
        v_tarSpec(%RP%,"RW",XXX)/(p_trqGlobl(%RP%,XXX,"TsMFN","CUR")+p_trqGlobl(%RP%,XXX,"TsPref","CUR")) =E=

          ( p_trqGlobl(%RP%,XXX,"TsPref","CUR")
            + (   p_trqGlobl(%RP%,XXX,"TsMFN","CUR")
                 -p_trqGlobl(%RP%,XXX,"TsPref","CUR"))

               * sigmoid( p_trqSigmoidSlope(%RP%,XXX)/p_trqGlobl(%RP%,XXX,"TrqNT","CUR")
                        * (v_TRQImports(%RP%,XXX)- p_trqGlobl(%RP%,XXX,"TrqNT","CUR")*0.97)))
                        /(p_trqGlobl(%RP%,XXX,"TsMFN","CUR")+p_trqGlobl(%RP%,XXX,"TsPref","CUR"));

*
* --- Ad valorem tariffs as function of import quantities if TRQ is present
*
 tarAdval_(%RP%,%RP%1,XXX) $ (  ((v_tarAdval.LO(%RP%,%RP%1,XXX) NE v_tarAdval.up(%RP%,%RP%1,XXX)) or p_trim)
*
*               --- bilateral TRQ (excluding the case of unlimited access at no or reduced tariff)
*
                            $   (( ( (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") GT eps) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE 1.E+10))
                                 $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE prohibitive)

                                 $ ( ((p_trqBilat(%RP%,%RP%1,XXX,"TaMFN","CUR") gt eps) OR (p_trqBilat(%RP%,%RP%1,XXX,"TaPref","CUR") gt eps))
                                    $ (p_trqBilat(%RP%,%RP%1,XXX,"TaMFN","CUR") ne p_trqBilat(%RP%,%RP%1,XXX,"TaPref","CUR")) )
                                 $ ( not (p_trqBilat(%RP%,%RP%1,XXX,"PrefTrigPrice","CUR") gt EPS)) )
*
*               --- global TRQ
*
                              or ( (p_trqGlobl(%RP%,XXX,"TrqNT","CUR") gt eps) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE 1.E+10)
                                  $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE prohibitive)

                                  $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR"))
                                  $ ( (p_trqGlobl(%RP%,XXX,"TaMFN","CUR") gt eps)    OR (p_trqGlobl(%RP%,XXX,"TaPref","CUR") gt eps))    ))
*
                            $   (not Sameas(%RP%,%RP%1)) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))) ..

         v_tarAdval(%RP%,%RP%1,XXX)/100 =E=
*
*      --- bilaterally allocated TRQs: the smaller of the global or bilateral rent / tariff
*
         ((    ncpcm( v_tarAdVal(%RP%,"RW",XXX),

              p_trqBilat(%RP%,%RP%1,XXX,"TaPref","CUR")

           + (p_trqBilat(%RP%,%RP%1,XXX,"TaMFN","CUR")-p_trqBilat(%RP%,%RP%1,XXX,"TaPref","CUR"))

               * v_trqSigmoidFunc(%RP%,%RP%1,XXX),max(1.E-2,1.E-3*abs(p_trqBilat(%RP%,%RP%1,XXX,"TaMFN","CUR")
                                                                     -p_trqBilat(%RP%,%RP%1,XXX,"TaPref","CUR")))))
                        $ (   (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") gt eps)
                            $ (p_trqGlobl(%RP%,XXX,"TrqNt","CUR") gt eps)
                            $ ( (p_trqGlobl(%RP%,XXX,"TaPref","CUR") gt eps) or  (p_trqGlobl(%RP%,XXX,"TaMFN","CUR") gt eps)))
*
*      --- bilaterally allocated TRQs and not global one
*
       + (
              p_trqBilat(%RP%,%RP%1,XXX,"TaPref","CUR")

           + (p_trqBilat(%RP%,%RP%1,XXX,"TaMFN","CUR")-p_trqBilat(%RP%,%RP%1,XXX,"TaPref","CUR"))

               * v_trqSigmoidFunc(%RP%,%RP%1,XXX))
                      $  (    (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") gt eps)
                           $  (    (p_trqGlobl(%RP%,XXX,"TrqNt","CUR") le eps)
                               or  ( (p_trqGlobl(%RP%,XXX,"TaPref","CUR") le eps) $ (p_trqGlobl(%RP%,XXX,"TaMFN","CUR") le eps))))
*
*      --- not bilaterally allocated TRQs
*
       + v_tarAdval(%RP%,"RW",XXX) $ ( (p_trqGlobl(%RP%,XXX,"TrqNt","CUR") gt eps) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") le eps)))/100

        ;

*
* --- Ad valorem tariff under not bilaterally allocated TRQs
*
 tarAdValW_(%RP%,"Rw",XXX) $ (  (p_trqGlobl(%RP%,XXX,"TrqNt","CUR") gt eps)
                              $ (p_trqGlobl(%RP%,XXX,"TaPref","CUR")  lt p_trqGlobl(%RP%,XXX,"TaMFN","CUR"))
                              $ ((p_trqGlobl(%RP%,XXX,"TaPref","CUR") gt eps) or (p_trqGlobl(%RP%,XXX,"TaMFN","CUR") gt eps)) ) ..
*
        v_tarAdval(%RP%,"RW",XXX)/(p_trqGlobl(%RP%,XXX,"TaMFN","CUR")+p_trqGlobl(%RP%,XXX,"TaPref","CUR"))
            =E=
*
         (p_trqGlobl(%RP%,XXX,"TaPref","CUR")
           + (p_trqGlobl(%RP%,XXX,"TaMFN","CUR")-p_trqGlobl(%RP%,XXX,"TaPref","CUR"))

                * sigmoid(  p_trqSigmoidSlope(%RP%,XXX)*(v_TRQImports(%RP%,XXX)
                          - p_trqGlobl(%RP%,XXX,"TrqNT","CUR")*0.97)/p_trqGlobl(%RP%,XXX,"TrqNT","CUR")))
                            /(p_trqGlobl(%RP%,XXX,"TaMFN","CUR")+p_trqGlobl(%RP%,XXX,"TaPref","CUR"));
*
* --- Intervention sales: probability weight for an undercut of administrative price
*                          multiplied with maximum increase of intervention stocks oberserved
*
 probMarketPriceUnderSafetyNet_(%RP%,XXX) $ { (DATA(%RP%,"PADM",XXX,"CUR") gt eps)
                                            $ (DATA(%RP%,"INTM",XXX,"CUR") GT eps)
                                            $  p_stdDevWorldMrkPrices(XXX) }..
*
        V_probMarketPriceUnderSafetyNet(%RP%,XXX) =E=
*
*        --- probability to undercut administrative price
*
             errorf( (DATA(%RP%,"PADM",XXX,"CUR")
                          - v_marketPrice(%RP%,XXX)
                       + pv_bevFuncIntAddFac(%RP%,XXX))*pv_bevFuncIntMultFac(%RP%,XXX)
                                                                               /p_stdDevWorldMrkPrices(XXX));
*
* --- Intervention sales
*
 buyingToIntervStock_(%RP%,XXX) $ ( (DATA(%RP%,"PADM",XXX,"CUR") gt eps)
                                  $ (DATA(%RP%,"INTM",XXX,"CUR") GT eps)
                                  $ p_stdDevWorldMrkPrices(XXX) ) ..
*

        v_buyingToIntervStock(%RP%,XXX)/DATA(%RP%,"INTM",XXX,"CUR") =E=
*
*            --- probability to undercut administrative price times maximum intervention purchases
*                (note that the maximum interv. purchases is put to the left hand side)
*
                  v_probMarketPriceUnderSafetyNet(%RP%,XXX);

$iftheni.interv_aggregators %RP% == RMTP

  buyingRM_(RM_not_RMTP,XXX)  $ (  (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"PADM",XXX,"CUR")) gt eps)
                                $  (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"INTM",XXX,"CUR")) gt eps)
                                $  p_stdDevWorldMrkPrices(XXX)
                                $  (not p_numeraireBuying(RM_not_RMTP,XXX)) )..

         v_buyingToIntervStock(RM_not_RMTP,XXX) =e=   sum(rm_to_rmtp(RM_not_RMTP,rmtp), v_buyingToIntervStock(RMTP,xxx))
                                                    * v_shareBuying(RM_not_RMTP,XXX) ;

  buyingRMNum_(RM_not_RMTP,XXX)  $ (  (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"PADM",XXX,"CUR")) gt eps)
                                   $  (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"INTM",XXX,"CUR")) gt eps)
                                   $  p_stdDevWorldMrkPrices(XXX)
                                   $  p_numeraireBuying(RM_not_RMTP,XXX) ) ..

         v_buyingToIntervStock(RM_not_RMTP,XXX) =e= sum(rm_to_rmtp(RM_not_RMTP,rmtp),
                                                       v_buyingToIntervStock(RMTP,xxx)
                                                     - sum(same_pblock(RM_not_RMTP,rm1) $ (not p_numeraireBuying(RM1,XXX)),
                                                       v_buyingToIntervStock(RM1,XXX))  ) ;

* --- shares among RMs in an RMTP, double log form
  buyingShares_(RM_not_RMTP,XXX)   $  ( (sum(rm_to_rmtp(RM_not_RMTP,rmtp),DATA(RMTP,"PADM",XXX,"CUR")) gt eps)
                                      $ (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"INTM",XXX,"CUR")) gt eps)
                                      $ p_stdDevWorldMrkPrices(XXX)
                                      $ (not p_numeraireBuying(RM_not_RMTP,XXX)) )..

         log(v_shareBuying(RM_not_RMTP,XXX)) =e= p_elasBuying(RM_not_RMTP,XXX)
                                               * log( v_marketPrice(RM_not_RMTP,XXX)
                                                    / sum(same_pblock(RM_not_RMTP,rm1) $ p_numeraireBuying(RM1,XXX),
                                                      v_marketPrice(RM1,XXX)))
                                      + pv_constShareBuying(RM_not_RMTP,XXX);

*
*  --- intervention stock releases
*
  releaseRM_(RM_not_RMTP,XXX) $ [ (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"PADM",XXX,"CUR")) gt eps)
                                $ (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"STKS",XXX,"CUR")) gt eps)
                                $ p_stdDevWorldMrkPrices(XXX)
                                $ (not p_numeraireRelease(RM_not_RMTP,XXX)) ]..

         v_releaseFromIntervStock(RM_not_RMTP,XXX) =e=   sum(rm_to_rmtp(RM_not_RMTP,rmtp), v_releaseFromIntervStock(RMTP,xxx))
                                                       * v_shareRelease(RM_not_RMTP,XXX) ;

  releaseRMNum_(RM_not_RMTP,XXX)  $ [  (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"PADM",XXX,"CUR")) gt eps)
                                    $  (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"STKS",XXX,"CUR")) gt eps)
                                    $  p_stdDevWorldMrkPrices(XXX)
                                    $  p_numeraireRelease(RM_not_RMTP,XXX) ]..

         v_releaseFromIntervStock(RM_not_RMTP,XXX) =e=   sum(rm_to_rmtp(RM_not_RMTP,rmtp),
                                                           v_releaseFromIntervStock(RMTP,xxx)
                                                       - sum(same_pblock(RM_not_RMTP,rm1) $ (not p_numeraireRelease(RM1,XXX)),
                                                           v_releaseFromIntervStock(RM1,XXX))) ;

* --- shares among RMs in an RMTP, log-log formulation
  releaseShares_(RM_not_RMTP,XXX)   $ [  (sum(rm_to_rmtp(RM_not_RMTP,rmtp),DATA(RMTP,"PADM",XXX,"CUR")) gt eps)
                                      $  (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"STKS",XXX,"CUR")) gt eps)
                                      $  p_stdDevWorldMrkPrices(XXX)
                                      $  (not p_numeraireRelease(RM_not_RMTP,XXX)) ]..

         log(v_shareRelease(RM_not_RMTP,XXX)) =e= p_elasRelease(RM_not_RMTP,XXX)
                                                * log( v_marketPrice(RM_not_RMTP,XXX)
                                                     / sum(same_pblock(RM_not_RMTP,rm1) $ p_numeraireRelease(RM1,XXX),
                                                       v_marketPrice(RM1,XXX)))
                                         + pv_constShareRelease(RM_not_RMTP,XXX);

* --- change = buying - release
  stockChangeRM_(RM_not_RMTP,XXX) $ { (sum(rm_to_rmtp(RM_not_RMTP,rmtp), DATA(RMTP,"PADM",XXX,"CUR")) gt eps)
                                    $ sum(rm_to_rmtp(RM_not_RMTP,rmtp) $ (  (DATA(RMTP,"STKS",XXX,"CUR") gt eps)
                                                                         or (DATA(RMTP,"intm",XXX,"CUR") gt eps)),1) }..

      v_intervStockChange(RM_not_RMTP,xxx) =e=  v_buyingToIntervStock(RM_not_RMTP,xxx)
                                              - V_releaseFromIntervStock(RM_not_RMTP,xxx);


$endif.interv_aggregators
*
* --- Intervention stock end size (start size plus purchases minus releases)
*

 intervStockLevel_(%RP%,XXX) $ ( (DATA(%RP%,"PADM",XXX,"CUR") gt eps)
                               $ (DATA(%RP%,"STKS",XXX,"CUR") gt eps)   )..

        V_intervStockLevel(%RP%,XXX)/(DATA(%RP%,"STKS",XXX,"CUR"))
         =E= (DATA(%RP%,"STKS",XXX,"CUR") + V_intervStockChange(%RP%,XXX))/(DATA(%RP%,"STKS",XXX,"CUR"));

*
* --- Intervention stock changes : purchases minus releases
*
 intervStockChange_(%RP%,XXX) $ ( (DATA(%RP%,"PADM",XXX,"CUR") gt eps)
                                $ (DATA(%RP%,"STKS",XXX,"CUR") gt eps)   )..


        V_intervStockChange(%RP%,XXX)/(DATA(%RP%,"STKS",XXX,"CUR")+DATA(%RP%,"INTM",XXX,"CUR"))
          =E= (V_buyingToIntervStock(%RP%,XXX) $ (DATA(%RP%,"INTM",XXX,"CUR") gt eps)
              -V_releaseFromIntervStock(%RP%,XXX))/(DATA(%RP%,"STKS",XXX,"CUR")+DATA(%RP%,"INTM",XXX,"CUR"));

*
* --- Release from intervention stocks: probability weight to undercut unit value exports
*                                        multiplied with size of intervention stocks
*
 releaseFromIntervStock_(%RP%,XXX) $ ( (DATA(%RP%,"PADM",XXX,"CUR") gt eps)
                                     $ (DATA(%RP%,"STKS",XXX,"CUR") gt eps)
                                     $  p_stdDevWorldMrkPrices(XXX) )..


        V_releaseFromIntervStock(%RP%,XXX)
           /(DATA(%RP%,"STKS",XXX,"CUR")+DATA(%RP%,"INTM",XXX,"CUR"))

           =E= (errorf( V_releaseFromIntervStock1(%RP%,XXX) ))**0.25
                * (1.-v_probMarketPriceUnderSafetyNet(%RP%,XXX)) * v_intervStockLevel(%RP%,XXX)
                / (DATA(%RP%,"STKS",XXX,"CUR")+DATA(%RP%,"INTM",XXX,"CUR"));

*
* --- Intermediate variable: normalised (and corrected) difference
*                             between unit value exports and market prices
*
 releaseFromIntervStock1_(%RP%,XXX) $  ( (DATA(%RP%,"PADM",XXX,"CUR") gt eps)
                                       $ (DATA(%RP%,"STKS",XXX,"CUR") gt eps)
                                       $ p_stdDevWorldMrkPrices(XXX) )..

        V_releaseFromIntervStock1(%RP%,XXX)
           /DATA(%RP%,"PADM",XXX,"CUR")
*
          =E= [(V_unitValueExports(%RP%,XXX)
               - v_marketPrice(%RP%,XXX)
               + pv_bevFuncIntCorrFac(%RP%,XXX))*pv_bevFuncIntMultFac(%RP%,XXX) / p_stdDevWorldMrkPrices(XXX)]
               /DATA(%RP%,"PADM",XXX,"CUR");

*
*  --- calculate export quantities from bi-lateral trade flows
*      (activate in calibration even if zero on CUR to avoid free, potentially even negative results)
  expQuant_(RM,XXX) $ ( (v_expQuant.lo(RM,XXX) ne v_expQuant.up(RM,XXX)) $  (DATA(RM,"Exports",XXX,"CUR") or p_trim)) ..

        v_expQuant(RM,XXX)
        /(DATA(RM,"Exports",XXX,"CUR") + 1. $ (NOT DATA(RM,"Exports",XXX,"CUR")) + 1.E-1)
*
         =E= SUM(RM1 $ ((NOT SAMEAS(RM,RM1))$ p_tradeFlows(RM1,RM,XXX,"CUR")), v_tradeFlows(RM1,RM,XXX))
        /(DATA(RM,"Exports",XXX,"CUR") + 1. $ (NOT DATA(RM,"Exports",XXX,"CUR")) + 1.E-1);
*

  EU27Exports_(XXX)  $ p_maxEU27Exports(XXX) ..
*
*       --- NONEU generally, but CH is excluded from the restriction in case of sugar
*
        SUM((RM1,RM)  $ (RW_TO_RM("NONEU",RM1) $ ((NOT SAMEAS("CH",RM1)) or (NOT SAMEAS(XXX,"SUGA")))
                           $ RW_TO_RM("EU",RM) $ p_tradeFlows(RM1,RM,XXX,"CUR")), v_tradeFlows(RM1,RM,XXX))
         =L= p_maxEU27Exports(XXX);

*
* --- Exports included in the calculation of the export unit values exclude flows under double-zero aggrements
*
 nonDoubleZeroExports_(%RP%,XXX) $  (     (DATA(%RP%,"PADM",XXX,"CUR") gt eps)
                                      $ ((DATA(%RP%,"STKS",XXX,"CUR") gt eps) or (DATA(%RP%,"FEOE_max",XXX,"CUR") gt eps))) ..


       v_nonDoubleZeroExports(%RP%,XXX)
          /  (SUM(%RP%1 $ ((NOT SAMEAS(%RP%,%RP%1)) $ p_tradeFlows(%RP%1,%RP%,XXX,"CUR")
                                              $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR"))),p_tradeFlows(%RP%1,%RP%,XXX,"CUR"))+1)


         =E= SUM(%RP%1 $ ((NOT SAMEAS(%RP%,%RP%1)) $ p_tradeFlows(%RP%1,%RP%,XXX,"CUR")
                                             $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR"))),
                    v_tradeFlows(%RP%1,%RP%,XXX))
          /  (SUM(%RP%1 $ ((NOT SAMEAS(%RP%,%RP%1)) $ p_tradeFlows(%RP%1,%RP%,XXX,"CUR")
                                              $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR"))),p_tradeFlows(%RP%1,%RP%,XXX,"CUR"))+1);
*
* -- Unit value exports: theoretical border prices to end up with market price of trading partner
*                        (excluding double-zero agreements as EU15 - EU10)
*

 unitValueExports_(%RP%,XXX) $ ( (DATA(%RP%,"PADM",XXX,"CUR") gt eps) $ (DATA(%RP%,"STKS",XXX,"CUR") gt eps))..
*
       v_unitValueExports(%RP%,XXX)/DATA(%RP%,"PADM",XXX,"CUR")

        =E= (SUM( %RP%1 $ ( (NOT SAMEAS(%RP%,%RP%1))
                         $ p_tradeFlows(%RP%1,%RP%,XXX,"CUR") $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR"))),

                  ( v_marketPrice(%RP%1,XXX) * p_exchgRateChangeFactor(%RP%,%RP%1)
*                      (no exchange rate factor for transportCost if the exporting region %RP% handles transport)
                   -v_transpCost(%RP%1,%RP%,XXX))  * v_tradeFlows(%RP%1,%RP%,XXX)) / (v_nonDoubleZeroExports(%RP%,XXX)+0.1))
                        /DATA(%RP%,"PADM",XXX,"CUR");
*

 valSubsExports_(%RP%,XXX) $ ( (DATA(%RP%,"FEOE_max",XXX,"CUR") gt eps) $ (DATA(%RP%,"PADM",XXX,"CUR") gt eps)) ..

      v_valSubsExports(%RP%,XXX)/DATA(%RP%,"FEOE_max",XXX,"CUR")
       =E= (v_nonDoubleZeroExports(%RP%,XXX) * v_perUnitExportSub(%RP%,XXX)/1000.)/DATA(%RP%,"FEOE_max",XXX,"CUR");

*
* --- Behavioural sigmoid function for subsidised export values
*
 EXPs_(%RP%,XXX) $ ((DATA(%RP%,"FEOE_MAX",XXX,"CUR") gt eps) $ (DATA(%RP%,"PADM",XXX,"CUR") gt eps)) ..
*
      v_valSubsExports(%RP%,XXX)/DATA(%RP%,"FEOE_Max",XXX,"CUR") =E=
*
                 Sigmoid(   pv_sigmParSubsExports(%RP%,"EXPS",XXX)/DATA(%RP%,"PADM",XXX,"CUR")
                         * (DATA(%RP%,"PADM",XXX,"CUR")-pv_bevFuncSubsExpCorrFact(%RP%,XXX)*v_marketPrice(%RP%,XXX)));

 wldPrice_(XXX) ..

    v_wldPrice(XXX) =E=  sum(rm, v_marketPrice(RM,XXX) * ( v_prodQuant(RM,XXX) + v_arm1Quant(RM,XXX) + 0.1))
                       / sum(rm,                           v_prodQuant(RM,XXX) + v_arm1Quant(RM,XXX) + 0.1);


*
*
* --- In order to use NLP type model sure to problems with CNS

 FLIPFLOP_           .. v_dummy =E= 10;


* --- Minimise deviation of given supply and import prices (used to calibrate base year data)

 NSSQ_            .. SSQ *
                           (   SUM((RMS,XXX) $ DATA(RMS,"Prod",XXX,"BAS"), p_weight(RMS,"Prod")*3)
                            +   SUM((RMS,XXX) $ DATA(RMS,"HCon",XXX,"BAS"),p_weight(RMS,"HCon"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"Feed",XXX,"BAS"),p_weight(RMS,"Feed"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"Proc",XXX,"BAS"),p_weight(RMS,"Proc"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"BioF",XXX,"BAS"),p_weight(RMS,"BioF"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"Imports",XXX,"BAS"),p_weight(RMS,"Imports"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"Exports",XXX,"BAS"),p_weight(RMS,"Exports"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"PPri",XXX,"BAS"),p_weight(RMS,"PPri"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"Cpri",XXX,"BAS"), 1.)
                            +   SUM((RM,XXX)  $ DATA(RM,"PMrk",XXX,"BAS"),p_weight(RM,"PMrk"))
                            +   SUM((RM,RM1,XXX) $ (DATA(RM,"PMrk",XXX,"CUR") $ p_impPrice(RM,RM1,XXX,"CUR")),
                                    p_weight(RM,"ImportP")*(1+p_addWgtDifImpP(RM,XXX)))
                            +   SUM((RMS,XXX) $ DATA(RMS,"ProcMarg",XXX,"BAS"),1.)
                            +   SUM((RMS,XXX)  $ DATA(RMS,"PMrg",XXX,"BAS"), 1.)
                            +   SUM((RMS,XXX)  $ DATA(RMS,"ProcMarg",XXX,"BAS"), 1.)
                            +   SUM((RM,RM1,XXX) $ p_tradeFlows(RM,RM1,XXX,"BAS"), (p_weight(RM,"ImportQ")+p_weight(RM1,"ImportQ"))/2)
                            +   SUM((RM,RM1,XXX) $ p_transpCost(RM,RM1,XXX,"Mean"),1)
                            + 1)
*
                           =E=
*
*                        --- difference between consolidated production quantities and observed ones
*

                            +  SUM((RMS,XXX) $ DATA(RMS,"Prod",XXX,"BAS"),
                                   SQR( (v_prodQuant(RMS,XXX)-DATA(RMS,"Prod",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"Prod",XXX,"BAS"),0.1) * p_weight(RMS,"Prod")) )
*
*                        --- difference between consolidated human consumption quantities and observed ones
*

                            +   SUM((RMS,XXX) $ DATA(RMS,"HCon",XXX,"BAS"),
                                   SQR( (v_consQuant(RMS,XXX)-DATA(RMS,"HCon",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"Hcon",XXX,"BAS"),0.1) * p_weight(RMS,"HCon")) )
*
*                        --- difference between consolidated feed quantities and observed ones
*

                            +   SUM((RMS,XXX) $ DATA(RMS,"Feed",XXX,"BAS"),
                                   SQR( (v_feedQuant(RMS,XXX)-DATA(RMS,"Feed",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"Feed",XXX,"BAS"),0.1) * p_weight(RMS,"Feed")) )
*
*                        --- difference between consolidated processed quantities and observed ones
*
                            +   SUM((RMS,XXX) $ DATA(RMS,"Proc",XXX,"BAS"),
                                   SQR( (v_procQuant(RMS,XXX)-DATA(RMS,"Proc",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"Proc",XXX,"BAS"),0.1) * p_weight(RMS,"Proc")) )
*
*                        --- difference between consolidated processed quantities to biofuels and observed ones
*
                            +   SUM((RMS,XXX) $ DATA(RMS,"BioF",XXX,"BAS"),
                                   SQR( (v_biofprocQuant(RMS,XXX)-DATA(RMS,"BioF",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"BioF",XXX,"BAS"),0.1) * p_weight(RMS,"BioF")) )
*
*                        --- difference between consolidated import quantities and observed ones
*

                            +   SUM((RM,XXX) $ DATA(RM,"Imports",XXX,"BAS"),
                                   SQR( (v_impQuant(RM,XXX)-DATA(RM,"Imports",XXX,"BAS"))
                                                            /MAX(DATA(RM,"Imports",XXX,"BAS"),0.1) * p_weight(RM,"Imports")) )
*
*                        --- difference between consolidated export quantities and observed ones
*

                            +   SUM((RM,XXX) $ DATA(RM,"Exports",XXX,"BAS"),
                                   SQR( (v_expQuant(RM,XXX)-DATA(RM,"Exports",XXX,"BAS"))
                                                            /MAX(DATA(RM,"Exports",XXX,"BAS"),0.1) * p_weight(RM,"Exports")) )

*
*                        --- difference between consolidated producer prices and observed ones
*

                            +   0.1 * SUM((RMS,XXX) $ DATA(RMS,"PPri",XXX,"BAS"),
                                   SQR( (v_prodPrice(RMS,XXX)-DATA(RMS,"PPri",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"PPri",XXX,"BAS"),0.1)) * p_weight(RMS,"PPri"))
*
*                        --- difference between consolidated consumer prices and observed ones
*

                            +   0.1 * SUM((RMS,XXX) $ DATA(RMS,"CPri",XXX,"BAS"),
                                   SQR( (v_consPrice(RMS,XXX)-DATA(RMS,"CPri",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"CPri",XXX,"BAS"),0.1)))
*
*                        --- difference between consolidated market prices and observed ones
*

                            +   0.1 * SUM((RM,XXX) $ DATA(RM,"PMrk",XXX,"BAS"),
                                   SQR( (v_marketPrice(RM,XXX)-DATA(RM,"PMrk",XXX,"BAS"))
                                                            /MAX(DATA(RM,"PMrk",XXX,"BAS"),0.1)) * p_weight(RM,"PMrk"))
*
*                           --- difference between market prics and import prices to reduce
*                               differences between physical and Armington aggregation
*
                            +   SUM((RM,RM1,XXX) $ (DATA(RM,"PMrk",XXX,"CUR") $ p_impPrice(RM,RM1,XXX,"CUR") $ p_tradeFlows(RM,RM1,XXX,"BAS")),
                                   SQR( (v_marketPrice(RM,XXX) - v_impPrice(RM,RM1,XXX))
                                        /MAX(DATA(RM,"PMrk",XXX,"CUR"),0.1)) * p_weight(RM,"ImportP")*(1+p_addWgtDifImpP(RM,XXX)))
*
*
*                        --- difference between consolidated gap between producer and market prices and observed ones
*

                            +   0.1 * SUM((RMS,XXX) $ DATA(RMS,"PMrg",XXX,"CUR"),
                                   SQR( (pv_prodPriceMarg(RMS,XXX)-DATA(RMS,"PMrg",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"PMrg",XXX,"CUR"),0.1)) )
*
*                        --- difference between consolidated processing margin and observed ones
*

                            +  SUM((RMS,XXX) $ DATA(RMS,"ProcMarg",XXX,"BAS"),
                                   SQR( (v_procMarg(RMS,XXX)-DATA(RMS,"ProcMarg",XXX,"BAS"))
                                                            /MAX(DATA(RMS,"ProcMarg",XXX,"BAS"),0.1)))
*
*                        --- difference between consolidated trade flows and observed ones
*

                           +    SUM((RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")),
*
*                                 --- squared dev from given flows resp. domestic saleas
*
                                  SQR( (v_tradeFlows(RM,RM1,XXX)+v_domSales(RM,XXX) $ SAMEAS(RM,RM1) -p_tradeFlows(RM,RM1,XXX,"BAS"))
                                                           /MAX(p_tradeFlows(RM,RM1,XXX,"BAS"),0.1)) * (p_weight(RM,"importQ")+p_weight(RM1,"importQ"))/2)

                                        / MAX(0.001,(SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")),
                                              (p_weight(RM,"importQ")+p_weight(RM1,"importQ"))/2) + 0.001))

*
*                                   --- weighted with weights for importers
*
                                     * SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                              $ DATA(RM1,"Prod",XXX,"CUR")), p_weight(RM1,"importQ"))
                                     / (SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                              $ DATA(RM1,"Prod",XXX,"CUR")), 1) + 0.001)
*
*                                   --- weighted with weights for exporters
*
                                     * SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                               $ SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Hcon",XXX,"CUR") + DATA(RMS,"Feed",XXX,"BAS") + DATA(RMS,"Proc",XXX,"BAS"))),
                                                                              p_weight(RM,"importQ"))
                                     /( SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                               $ SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Hcon",XXX,"CUR") + DATA(RMS,"Feed",XXX,"BAS") + DATA(RMS,"Proc",XXX,"BAS"))),
                                                                              1) + 0.001)
*

                           +    10. * SUM((RM,RM1,XXX) $ ( (p_tradeFlows(RM,RM1,XXX,"BAS") le 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")),
                                             SQR( v_tradeFlows(RM,RM1,XXX)) * (p_weight(RM,"importQ")+p_weight(RM1,"importQ"))/2)

*
*                        --- difference between transport costs and econometrically estimated transport costs
*
                           +    SUM((RM,RM1,XXX) $ p_transpCost(RM,RM1,XXX,"Mean"),
                                  SQR( (v_transpCost(RM,RM1,XXX) - p_transpCost(RM,RM1,XXX,"Mean"))
                                                           / MAX(SQRT(p_transpCost(RM,RM1,XXX,"Var")),p_transpCost(RM,RM1,XXX,"Mean")*0.1)))
                           /    (SUM((RM,RM1,XXX) $ (p_tradeFlows(RM,RM1,XXX,"BAS") $ DATA(RM,"Pmrk",XXX,"CUR")),1) + 0.1);


* --- Minimise deviation from given baseline data, used in calibration point

 NSSQ1_            .. SSQ *
                           (   SUM((RMS,XXX) $ DATA(RMS,"Prod",XXX,"BAS"), p_weight(RMS,"Prod"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"HCon",XXX,"BAS"),p_weight(RMS,"HCon"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"Feed",XXX,"BAS"),p_weight(RMS,"Feed"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"Proc",XXX,"BAS"),p_weight(RMS,"Proc"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"BioF",XXX,"BAS"),p_weight(RMS,"BioF"))
                            +   SUM((RM,XXX) $ DATA(RM,"Imports",XXX,"CUR"),p_weight(RM,"Imports"))
                            +   SUM((RM,XXX) $ DATA(RM,"Exports",XXX,"CUR"),p_weight(RM,"Exports"))
                            +   SUM((RMS,XXX) $ DATA(RMS,"PPri",XXX,"CUR"),p_weight(RMS,"PPri"))
                            +   SUM((RM,XXX) $ DATA(RM,"PMrk",XXX,"CUR"),11.*p_weight(RM,"PMrk"))
                            +   SUM((RM,RM1,XXX) $ (DATA(RM,"PMrk",XXX,"CUR") $ p_impPrice(RM,RM1,XXX,"CUR")),
                                    p_weight(RM,"ImportP")*(1+p_addWgtDifImpP(RM,XXX)))
                            +   SUM((RMS,XXX) $ DATA(RMS,"ProcMarg",XXX,"CUR"),1.)
                            +   SUM((RM,RM1,XXX) $ p_tradeFlows(RM,RM1,XXX,"CUR"), (p_weight(RM,"importQ")+p_weight(RM1,"importQ"))/2)
                            +   SUM((RM,RM1,XXX) $ p_transpCost(RM,RM1,XXX,"Mean"),1)
                            + 1)
*
                           =E=

                            +   SUM((RMS,XXX) $ DATA(RMS,"Prod",XXX,"CUR"),
                                   SQR( (v_prodQuant(RMS,XXX)-DATA(RMS,"Prod",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"Prod",XXX,"CUR"),0.1)) * p_weight(RMS,"Prod"))

                            +   SUM((RMS,XXX) $ DATA(RMS,"HCon",XXX,"CUR"),
                                   SQR( (v_consQuant(RMS,XXX)-DATA(RMS,"HCon",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"Hcon",XXX,"CUR"),0.1)) * p_weight(RMS,"HCon"))

                            +   SUM((RMS,XXX) $ DATA(RMS,"Feed",XXX,"CUR"),
                                   SQR( (v_feedQuant(RMS,XXX)-DATA(RMS,"Feed",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"Feed",XXX,"CUR"),0.1)) * p_weight(RMS,"Feed"))

                            +   SUM((RMS,XXX) $ DATA(RMS,"Proc",XXX,"CUR"),
                                   SQR( (v_procQuant(RMS,XXX)-DATA(RMS,"Proc",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"Proc",XXX,"CUR"),0.1)) * p_weight(RMS,"Proc"))

                            +   SUM((RMS,XXX) $ DATA(RMS,"BioF",XXX,"CUR"),
                                   SQR( (v_biofprocQuant(RMS,XXX)-DATA(RMS,"BioF",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"BioF",XXX,"CUR"),0.1)) * p_weight(RMS,"BioF"))

*pw020813                   intervention stock changes are not tightly bounded in future calibration point
*                           => may deviate strongly from near zero target values unless penalised
*
                            +   SUM((RM,XXX) $ DATA(RM,"ISCH",XXX,"CUR"),
                                   SQR( (v_intervStockChange(RM,XXX) - DATA(RM,"ISCH",XXX,"CUR") )
                                                            /MAX(ABS(DATA(RM,"ISCH",XXX,"CUR")),0.01))* p_weight(RM,"prod"))

*pw050416                   Total imports and exports may be derived from Aglink data and should be reproduced if possible
*
                            +   SUM((RM,XXX) $ DATA(RM,"Imports",XXX,"CUR"),
                                   SQR( (v_impQuant(RM,XXX)-DATA(RM,"Imports",XXX,"CUR"))
                                                            /MAX(DATA(RM,"Imports",XXX,"CUR"),0.1) * p_weight(RM,"Imports")) )
*
                            +   SUM((RM,XXX) $ DATA(RM,"Exports",XXX,"CUR"),
                                   SQR( (v_ExpQuant(RM,XXX)-DATA(RM,"Exports",XXX,"CUR"))
                                                            /MAX(DATA(RM,"Exports",XXX,"CUR"),0.1) * p_weight(RM,"Exports")) )

                            +   SUM((RMS,XXX) $ DATA(RMS,"PPri",XXX,"CUR"),
                                   SQR( (v_prodPrice(RMS,XXX)-DATA(RMS,"PPri",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"PPri",XXX,"CUR"),0.1)) * p_weight(RMS,"PPri"))
*
                            +   SUM((RM,XXX) $ DATA(RM,"PMrk",XXX,"CUR"),
                                   SQR( (v_marketPrice(RM,XXX)-DATA(RM,"PMrk",XXX,"CUR"))
                                                            /MAX(DATA(RM,"PMrk",XXX,"CUR"),0.1)) * p_weight(RM,"PMrk"))

                            +   SUM((RM,XXX) $ DATA(RM,"PMrk",XXX,"CUR"),
                                   SQR( (v_marketPrice(RM,XXX)-v_wldPrice(XXX))
                                                            /MAX(DATA(RM,"PMrk",XXX,"CUR"),0.1))) * 10
*
*                           --- difference between market prics and import prices to reduce
*                               differences between physical and Armington aggregation
*
                            +   SUM((RM,RM1,XXX) $ (DATA(RM,"PMrk",XXX,"CUR") $ p_impPrice(RM,RM1,XXX,"CUR")),
                                   SQR( (v_marketPrice(RM,XXX) - v_impPrice(RM,RM1,XXX))
                                        /MAX(DATA(RM,"PMrk",XXX,"CUR"),0.1)) * p_weight(RM,"ImportP")*(1+p_addWgtDifImpP(RM,XXX)))

                            +   SUM((RMS,XXX) $ DATA(RMS,"ProcMarg",XXX,"CUR"),
                                   SQR( (v_procMarg(RMS,XXX)-DATA(RMS,"ProcMarg",XXX,"CUR"))
                                                            /MAX(DATA(RMS,"ProcMarg",XXX,"CUR"),0.1)))

                           +    SUM((RM,RM1,XXX) $ p_tradeFlows(RM,RM1,XXX,"CUR"),
                                  SQR( ( v_tradeFlows(RM,RM1,XXX) $ (NOT SAMEAS(RM,RM1))
                                        +v_domSales(RM,XXX) $ SAMEAS(RM,RM1) -p_tradeFlows(RM,RM1,XXX,"CUR"))
                                      / MAX(p_tradeFlows(RM,RM1,XXX,"CUR"),0.1) ) * (p_weight(RM,"importQ")+p_weight(RM1,"importQ"))/2)
                               / MAX(0.001,(SUM( (RM,RM1,XXX) $ p_tradeFlows(RM,RM1,XXX,"CUR"),
                                                                                    (p_weight(RM,"importQ")+p_weight(RM1,"importQ"))/2)+0.0001))
*
*                                   --- weighted with weights for importers
*
                                     * SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                              $ DATA(RM1,"Prod",XXX,"CUR")), p_weight(RM1,"importQ"))
                                     / (SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                              $ DATA(RM1,"Prod",XXX,"CUR")), 1) + 0.001)
*
*                                   --- weighted with weights for exporters
*
                                     * SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                               $ SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Hcon",XXX,"CUR") + DATA(RMS,"Feed",XXX,"BAS") + DATA(RMS,"Proc",XXX,"BAS"))),
                                                                              p_weight(RM,"importQ"))
                                     /( SUM( (RM,RM1,XXX) $ ((p_tradeFlows(RM,RM1,XXX,"BAS") gt 1.E-10) $ p_tradeFlows(RM,RM1,XXX,"CUR")
                                               $ SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"Hcon",XXX,"CUR") + DATA(RMS,"Feed",XXX,"BAS") + DATA(RMS,"Proc",XXX,"BAS"))),
                                                         1) + 0.001)
*
*                         --- strong penalty for newly introduced trade flows
*
                           +    10. * SUM((RM,RM1,XXX) $ ( (not (p_tradeFlows(RM,RM1,XXX,"CUR")) $ (v_tradeFlows.LO(RM,RM1,XXX) ne v_tradeFlows.UP(RM,RM1,XXX)))),
                                             SQR( v_tradeFlows(RM,RM1,XXX)))

*
*                        --- difference between transport costs and econometrically estimated transport costs
*
                           +    SUM((RM,RM1,XXX) $ p_transpCost(RM,RM1,XXX,"Mean"),
                                  SQR( (v_transpCost(RM,RM1,XXX) - p_transpCost(RM,RM1,XXX,"Mean"))
                                                           / MAX(SQRT(p_transpCost(RM,RM1,XXX,"Var")),p_transpCost(RM,RM1,XXX,"Mean")*0.1,0.1)))
                           /    (SUM((RM,RM1,XXX) $ (p_tradeFlows(RM,RM1,XXX,"BAS") $ DATA(RM,"Pmrk",XXX,"CUR")),1) +0.1);
*




 MODEL m_bevFunctionsMarket /
* --- behavioural functions
*               Prod_
*               FeedUseA_

                GLDemandFS_
                GLDemandGS_
                GLDemandGis_
                XiS_
                consFudge_
                caloPerCap_

                ProdNQ_
                LandSupply_
                yaniMarket_
                ProdFudge_

                ProcO_
                MaprFeed_
                ProcMargO_
                ProcNQ_
                ProcFudge_
                procYield_

                FeedNQ_
                FeedFudge_
                FeedBal_

                feedBlkPrice_
*               feedShift_
                feedBlk_
                FeedShareEqu_

                DairyNQ_
                ProcMargM_
                FatsProtBal_
                Biof_
                CSugarPrice_
                prodQuantBiofBy_
                ProdBiof_
                MaprBiof_
                SupplyBiof_
                BiofPriceRel_
                BiofFeedCost_
                BiofFeedCostAgg_
                biofDemshare_
                biofDem_
                LandAgrFromXX_
                Yield_
                LandAgrTmp_
                LandAgr_
                LandNagr_
                LandTypesNagrTmp_
                LandTypesNagr_
   /;


 MODEL m_netTrade /

                m_bevFunctionsMarket

                ProdA_
                ArmBal1_

                stockChg_
                mrkBal_
                cPri_
                pPri_
                ProdPriceFudge_
                arm1PNetTrade_
*
                wldMarket_
                ntrd_
*
                FlipFlop_
*
*  --- open: intervention system, subsidized exports
*
 /;



 MODEL m_globMarket /


                m_bevFunctionsMarket
                ProdA_

* --- Armington share equations

                arm2QuantShares_
                domSalesShares_
                importShares_
                tradeFlowFudge_
* --- KVT approach for the Armington import-demand system
                HicksNeutral_
* --- extended KVT approach with transition function
$ifi %modArmington% == extendedKVT        transition_

* --- Price linkage (Fitit1)
                arm1PriceAgg_
                arm2PriceAgg_
                impPrice_
*               v_cifPrice_
                CPri_
                consPriceFudge_
                PMrk_
                PPri_
                ProdPriceFudge_
$ifi %RP% == RMTP          MarketPriceAgg_
* transport costs are fixed in simulation $ifi %RP% == RMTP          TransportCostsAgg_


* --- Quantity balances (Fitit1)
                SupBalM_
                ArmBal1_
                expQuant_
                nonDoubleZeroExports_
$ifi %RP% == RMTP  TradeFlowsAgg_
                FlipFlop_


* --- Policy variables

                buyingToIntervStock_
                probMarketPriceUnderSafetyNet_
                releaseFromIntervStock_
                releaseFromIntervStock1_
                intervStockLevel_
                intervStockChange_
                unitValueExports_

*            aggregators for intervention RM ==> RMTP
$iftheni %RP% == RMTP
                buyingRM_
                buyingRMNum_
                releaseRM_
                releaseRMNum_
                buyingShares_
                releaseShares_
                stockChangeRM_
$endif

                EXPs_
                valSubsExports_

                TRQImports_
                trqSigmoidFunc_
                tarSpec_
                prefTriggerPrice_
                FlexLevyNotCut_
                FlexLevy_
                tarSpecIfEntryPrice_
                EntryPriceDriver_
                tarSpecW_
                tarAdVal_
                tarAdValW_
                /;

 m_globMarket.OPTFILE     = 7;
 m_globMarket.LIMCOL      = 0;
 m_globMarket.LIMROW      = 0;
 m_globMarket.LIMCOL      = 0;
 m_globMarket.HOLDFIXED   = 1;
 m_globMarket.ITERLIM     = 10000;
*m_globMarket.DOMLIM      = 10000;
 m_globMarket.Scaleopt    = 0;
 m_globMarket.Solprint    = 0;
 m_globMarket.dictfile    = 0;
 m_globMarket.Workfactor  = 2.0;
 m_globMarket.TOLINFEAS = 1.E-5;

 m_netTrade.OPTFILE     = 7;
 m_netTrade.LIMCOL      = 0;
 m_netTrade.LIMROW      = 0;
 m_netTrade.LIMCOL      = 0;
 m_netTrade.HOLDFIXED   = 1;
 m_netTrade.ITERLIM     = 10000;
*m_netTrade.DOMLIM      = 10000;
 m_netTrade.Scaleopt    = 0;
 m_netTrade.Solprint    = 0;
 m_netTrade.dictfile    = 0;
 m_netTrade.Workfactor  = 2.0;
 m_netTrade.TOLINFEAS = 1.E-5;

*
*
*     --- include calibration models
*
$include 'arm\trim_gl_commits.gms'

SET GlobMarketEq "equation names as set element to facilitate checking of infeasibilities" /

                GLDemandFS_
                GLDemandGS_
                GLDemandGis_
                XiS_
                consFudge_

                ProdNQ_
                LandSupply_
                yaniMarket_
                ProdFudge_

                ProcO_
                MaprFeed_
                ProcMargO_
                ProcNQ_
                ProcFudge_
                procYield_

                FeedNQ_
                FeedFudge_
                FeedBal_

                feedBlkPrice_
*               feedShift_
                feedBlk_
                FeedShareEqu_

                DairyNQ_
                ProcMargM_
                FatsProtBal_
                Biof_
                CSugarPrice_
                prodQuantBiofBy_
                ProdBiof_
                MaprBiof_
                SupplyBiof_
                BiofPriceRel_
                BiofFeedCost_
                BiofFeedCostAgg_
                biofDemshare_
                biofDem_



                m_bevFunctionsMarket
                ProdA_

* --- Armington share equations

                arm2QuantShares_
                domSalesShares_
                importShares_
* --- KVT approach for the Armington import-demand system
                HicksNeutral_



* --- Price linkage (Fitit1)
                arm1PriceAgg_
                arm2PriceAgg_
                impPrice_
*               v_cifPrice_
                CPri_
                PMrk_
                PPri_
$ifi %RP% == RMTP          MarketPriceAgg_
* transport costs are fixed in simulation $ifi %RP% == RMTP          TransportCostsAgg_


* --- Quantity balances (Fitit1)
                SupBalM_
                ArmBal1_
                expQuant_
                nonDoubleZeroExports_
$ifi %RP% == RMTP  TradeFlowsAgg_
                FlipFlop_

* --- Land balancing
                LandAgrFromXX_
                Yield_
                LandTypesNagrTmp_
                LandTypesNagr_
                LandAgrTmp_
                LandAgr_



* --- Policy variables

                buyingToIntervStock_
                probMarketPriceUnderSafetyNet_
                releaseFromIntervStock_
                releaseFromIntervStock1_
                intervStockLevel_
                intervStockChange_
                unitValueExports_

*            aggregators for intervention RM ==> RMTP
$iftheni %RP% == RMTP
                buyingRM_
                buyingRMNum_
                releaseRM_
                releaseRMNum_
                buyingShares_
                releaseShares_
                stockChangeRM_
$endif

                EXPs_
                valSubsExports_

                TRQImports_
                trqSigmoidFunc_
                tarSpec_
                prefTriggerPrice_
                FlexLevyNotCut_
                FlexLevy_
                tarSpecIfEntryPrice_
                EntryPriceDriver_
                tarSpecW_
                tarAdVal_
                tarAdValW_
                /;


*
*   ---    Calibration model for the KVT approach for the Armington import demand system
*

  model m_calibKVT /

  importShares_
  arm2PriceAgg_

  /;

* --- Declare a small model that contains the difficult sigmoid functions.
*     In the market calibration, this model can be solved in a pre-step, to get a feasible starting point for "fitit"

  equation e_fixTradeFlows "Keep trade flows at the initial level";
  e_fixTradeFlows(%RP%,%RP%1,XXX) .. v_tradeFlows(%RP%,%RP%1,XXX) =E= v_tradeFlows.l(%RP%,%RP%1,XXX);

  model m_onlyTariffs "Solve the sigmoid tariff functions for fixed trade flows"
  /tarSpec_,tarAdval_,tarAdValW_,tarSpecW_,trqSigmoidFunc_,TRQImports_,e_fixTradeFlows,flipFlop_/;
