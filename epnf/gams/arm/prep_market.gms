********************************************************************************
$ontext

   CAPRI project

   GAMS file : PREP_MARKET.GMS

   @purpose  : Define start values for market model
               for current simulation year.


   @author   : Wolfgang Britz, Marcel Aden�uer et.al.
   @date     : 28.10.09
   @since    : 2002
   @refDoc   :
   @seeAlso  : arm\market_model.gms
   @calledBy : capmod.gms

$offtext
********************************************************************************
*

** if endogenous biofuel market are switched on, do it now
p_endoBioMarket=0;
$ifi %BIOF_M% == ON p_endoBioMarket=1;

option kill=scen_store_during_market;
$batinclude 'supply\unload_supplymodel.gms' "'CUR'" "'Y'" "'TRD'" "'BAS'" "'CAL'"
$include 'util\kill_model1.gms'

 option kill=p_CnstNQSupp;


* ---- include all products
*
  XXX(XX)      = YES;
  XXX1(XX1)    = YES;
  XXX("MILK")  = NO;
  XXX1(XX)     = YES;
  XXX_all(XX_all) $ sum(sameas(XXX1,XX_all),1)=yes;
*
*     --- use as start values those from the calibration point
*
$batinclude 'util\title.gms' "'Set starting values'"

  DATA(RMall,SITEMS,XX1,"CUR") $ DATA(RMall,SITEMS,XX1,"CAL") = DATA(RMall,SITEMS,XX1,"CAL");
  DATA(RMall,"FEED",XX,"TRD")  = DATA(RMall,"FEED",XX,"CAL");
  DATA(RMall,"BIOF",XX,"TRD")  = DATA(RMall,"BIOF",XX,"CAL");

  DATA(RMSSUP,"FEEDDIFF",FEED_TRD,"TRD")
    =  DATA(RMSSUP,"GROF",FEED_TRD,"TRD") - SUM(FEED_TO_XX(FEED_TRD,XX), DATA(RMSSUP,"FEDM",XX,"TRD"));
*
  DATA(RMall,"INTM",XX1,"CUR") = DATA(RMall,"INTM",XX1,"CAL");

  DATA(RMall,"GROF","SUGB","CUR") = DATA(RMall,"GROF","SUGB","TRD");
  DATA(RMall,"Prod","SUGB","CUR") = DATA(RMall,"GROF","SUGB","CUR");

  DATA(RMall,"GROF","OLIV","CUR") = DATA(RMall,"GROF","OLIV","TRD");
  DATA(RMall,"Prod","OLIV","CUR") = DATA(RMall,"GROF","OLIV","CUR");

  DATA(RMall,"GROF","PARI","CUR") = DATA(RMall,"GROF","PARI","TRD");
  DATA(RMall,"Prod","PARI","CUR") = DATA(RMall,"GROF","PARI","CUR");

  DATA(RMSSUP,"GROF",xxOmYani,"CUR")  = DATA(RMSSUP,"GROF",xxOmYani,"TRD");
  DATA(RMSSUP,"GROF",IYANI,"CUR")     = DATA(RMSSUP,"GROF",IYANI,"TRD");

  DATA(RMSSUP,"Prod",xxOmYani,"Cur") = DATA(RMSSUP,"GROF",xxOmYani,"TRD")
                                      -SUM(O_TO_YANI(xxOmYani,IYANI), DATA(RMSSUP,"GROF",IYANI,"TRD")) + eps;
  DATA(RMSSUP,"Prod",xxOmYani,"TRD") = DATA(RMSSUP,"Prod",xxOmYani,"Cur");

  DATA(RMSSUP,"Prod",xxOmYani,"bas") = DATA(RMSSUP,"GROF",xxOmYani,"BAS")
                                      -SUM(O_TO_YANI(xxOmYani,IYANI), DATA(RMSSUP,"GROF",IYANI,"BAS"));

$iftheni.expost %EXPOST% == ON
  DATA(RMall,"PADM",ROWS,"CUR")     = DATA(RMall,"PADM",ROWS,"CAL");
  DATA(RMall,"MinBordP",ROWS,"CUR") = DATA(RMall,"MinBordP",ROWS,"CAL");

* DATA(RMSSUP,"Biof","SUGA","TRD")   = DATA(RMSSUP,"Biof","SUGA","BAS");
* DATA(RMSSUP,"GROF","SUGBc","TRD")  = DATA(RMSSUP,"GROF","SUGBc","BAS");

* DATA(RMSSUP,"PPRI","SUGBc","CUR")  = DATA(RMSSUP,"PPRI","SUGBc","BAS");

$endif.expost

If (p_endobioMarket NE 1,
    v_prodBiof.fx(RMS,XXBioF)
     =  SUM(Stock_to_fuel(XX,XXBioF) $ (DATA(RMS,"PRCB",XX,"CUR") $ DATA(RMS,"BioF",XX,"CUR")),
                                        DATA(RMS,"Biof",XX,"CUR") * DATA(RMS,"PRCB",XX,"CUR"));
    Data(RMS,"PROD",XXBIOF,"CUR") = v_prodBiof.l(RMS,XXBioF)
                                 +   DATA(RMS,"NAGR",XXBioF,"CUR")
                                 +   DATA(RMS,"SECG",XXBioF,"CUR")
                                 +   DATA(RMS,"EXOG",XXBioF,"CUR") ;
    );
*
* --- for land modelling
*
  DATA(RMS,luInMrk,"LEVL","CUR")    = DATA(RMS,luInMrk,"LEVL","CAL")    ;
  DATA(RMS,"share",ocNec,"CUR")     = DATA(RMS,"share",ocNec,"CAL")   ;

*     -- drive regions with supply models without land supply curve
*        and land prices and all land related equations:
*       (As these are covered in supply models the additional equations and variables may be avoided)
  DATA(RMSSUP,"PROD","LAND","CUR") = 0;
  DATA(RMSSUP,"PPRI","LAND","CUR") = 0;
  DATA(RMSSUP,"levl",XX_landuse,"CUR") = 0;
  DATA(RMSSUP,"yild",XX_landuse,"CUR") = 0;

  DATA(RM,"PROC","SUGA","CUR") = SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"PROC","SUGA","CUR"));
  DATA(RM,"PROC","SUGA","CAL") = SUM(RMS_TO_RM(RMS,RM), DATA(RMS,"PROC","SUGA","CAL"));

  DATA(RMSSUP,Quotas,"SUGB","CUR") = DATA(RMSSUP,Quotas,"SUGB","Y");
  DATA(RMSSUP,Quotas,"SUGA","CUR") = DATA(RMSSUP,Quotas,"SUGA","Y");

  data(rmssup,"ppri",omYani,"cur") = data(rmssup,"uvag",omYani,"Trd");

  data(rm,"prod",xxOmYani,"cur") $ sum(rm_to_rms(rm,rmssup),1)
     = sum(rm_to_rms(rm,rmssup), data(rmssup,"prod",xxOmYani,"cur")) + eps;

  data(rm,"grof",xxOmYani,"cur") $ sum(rm_to_rms(rm,rmssup),1)
     = sum(rm_to_rms(rm,rmssup), data(rmssup,"grof",xxOmYani,"cur"));

  data(rm,"pMrk",xxOmYani,"cur") $ (data(rm,"grof",xxOmYani,"cur") gt eps)
      = sum(rm_to_rms(rm,rmssup), data(rmssup,"grof",xxOmYani,"cur")*data(rmssup,"ppri",xxOmYani,"cur"))
        / data(rm,"grof",xxOmYani,"cur");

  data(RMSSUP,"Pmrg",xxOmYani,"CUR") $ sum(rms_to_rm(rmssup,rm), data(rm,"Pmrk",xxOmYani,"cur"))
    = data(rmssup,"ppri",xxOmYani,"cur")/sum(rms_to_rm(rmssup,rm), data(rm,"Pmrk",xxOmYani,"cur"));

*
*  --- take over processing aid per unit
*
  DATA("EU_West","PRCA","SUGA","Y") $ DATA("EU_West","PRCA","SUGA","%SIMY%")
       = DATA("EU_West","PRCA","SUGA","%SIMY%");

  DATA("EU_West","PRCA","SUGA","CUR") $ (DATA("EU_West","PRCA","SUGA","CUR") GT EPS)
     = MIN(200,(DATA("EU_West","PROC","SUGA","Y")*DATA("EU_West","PRCA","SUGA","Y"))/DATA("EU_West","PROC","SUGA","CUR"));

$setglobal additivePPriMargin off

$iftheni.addMargin  %additivePPriMargin% == on

  p_additivePPriMargin(XX) = 1;


  DATA(RMS,"PMrg",XX,"CUR") $ p_additivePPriMargin(XX) =

    DATA(RMS,"PPri",XX,"CUR") -
        (SUM(RMS_TO_RM(RMS,RM),

*     --- Internal market prices from OECD calculations
          (DATA(RM,"PMrk",XX,"CUR") + DATA(RM,"PSEi",XX,"CUR") + DATA(RM,"PSEd",XX,"CUR")))

*     --- plus area / per head payment
            + (DATA(RMS,"AREP",XX,"CUR")/DATA(RMS,"Yild",XX,"CUR")) $ DATA(RMS,"Yild",XX,"CUR"));


$endif.addMargin

*     --- prepare adjustments of producer margins depending on net trade index

$ifi %adjustMargins% == on $include 'arm\define_for_adjust_marg.gms'

*
*     --- set the calibrated constant terms of the intervention equations
*
  pv_prodPriceMarg.FX(RMS,XX)   = DATA(RMS,"PMrg",XX,"CUR");
  pv_bevFuncIntCorrFac.FX(RMALL,XX) = DATA(RMALL,"C_TOST",XX,"CUR");
  pv_bevFuncIntAddFac.FX(RMALL,XX)  = DATA(RMALL,"C_TOST1",XX,"CUR");
  pv_bevFuncIntMultFac.FX(RMALL,XX) = DATA(RMALL,"C_TOST2",XX,"CUR");
$iftheni.rmtp %RP% == RMTP
  pv_constShareBuying.FX(RM,XX) = data(RM,"buyin_const",XX,"CUR");
  pv_constShareRelease.FX(RM,XX) = data(RM,"release_const",XX,"CUR");
$endif.rmtp
*
* ---- GDP and population
*
  DATA(RMall,"PPRI","FEDE","CAL") = DATA(RMall,"PPRI","FEDE","CUR");
  DATA(RMall,"FEED","FEDE","CAL") = DATA(RMall,"PROD","FEDE","CAL");
  DATA(RMall,"FEED","FEDE","CUR") = DATA(RMall,"PROD","FEDE","CUR");
  DATA(RMall,"INCE","LEVL","CUR") = DATA(RMall,"INCE","LEVL","CAL");
  DATA(RMall,"INHA","LEVL","CUR") = DATA(RMall,"INHA","LEVL","CAL");
*
* ---- total amount of milk fat and protein, production of milk, fat and protein prices and contents
*
  DATA(RMS,"PRCM","MILK","CUR") = DATA(RMS,"PRCM","MILK","CAL");
  DATA(RMSSUP,"PROD","MILK","CUR") = DATA(RMSSUP,"PRCM","MILK","CAL");
  DATA(RMS,"Prod","COMI","CUR") = DATA(RMS,"Prod","COMI","CAL");
  DATA(RMS,"Prod","SGMI","CUR") = DATA(RMS,"Prod","SGMI","CAL");
  DATA(RMS,"Prod","PARI","CUR") = DATA(RMS,"Prod","PARI","CAL");
  DATA(RMS,"Prod","OLIV","CUR") = DATA(RMS,"Prod","OLIV","CAL");

  DATA(RMS,"PRCM","MILK","TRD") = DATA(RMS,"PRCM","MILK","CAL");
  DATA(RMS,"Prod","COMI","TRD") = DATA(RMS,"Prod","COMI","CAL");
  DATA(RMS,"Prod","SGMI","TRD") = DATA(RMS,"Prod","SGMI","CAL");
  DATA(RMS,"Prod","PARI","TRD") = DATA(RMS,"Prod","PARI","CAL");
  DATA(RMS,"Prod","OLIV","TRD") = DATA(RMS,"Prod","OLIV","CAL");

  DATA(RMS,"Prod",FatsProt_R,"CUR") = DATA(RMS,"Prod",FatsProt_R,"CAL");
  DATA(RMS,"PPri",FatsProt_R,"CUR") = DATA(RMS,"PPri",FatsProt_R,"CAL");
  DATA(RMS,FatsProt,MLK,"CUR")      = DATA(RMS,FatsProt,MLK,"CAL");


*
* ----- shift trade flows and related prices, and re-determine Armington parameters
*       [04.09.2014mih: this code chunk is shifted before set_tariffs, because set_tariff uses p_impPrice]
$batinclude 'util\title.gms' "'Shift trade flows and prices'"
*
  p_tradeFlows(RMALL,RMALL1,XX,"CUR")         = p_tradeFlows(RMALL,RMALL1,XX,"CAL");
  p_impPrice(RM,RM1,XX,"CUR")                 = p_impPrice(RM,RM1,XX,"CAL");
  p_entryPriceFac(RMALL,RMALL1,XX,"CUR")      = p_entryPriceFac(RMALL,RMALL1,XX,"CAL");
  p_flexLevy(RMALL,RMALL1,XX,"CUR")           = p_flexLevy(RMALL,RMALL1,XX,"CAL");

*
*  --- calculate average import prices for the policy blocks
*
$iftheni %RP% == RMTP
  p_impPrice(RMTP,RMTP1,XX,"CUR") $ ((not sameas(RMTP,RMTP1))
                                  $ sum((rm,rm1) $ (rm_to_rmtp(rm,rmtp) $ rm_to_rmtp(rm1,rmtp1)), p_tradeFlows(rm,rm1,xx,"cur")) )
    = sum((rm,rm1) $ (rm_to_rmtp(rm,rmtp) $ rm_to_rmtp(rm1,rmtp1)), p_impPrice(RM,RM1,XX,"CUR") * p_tradeFlows(rm,rm1,xx,"cur"))
      / sum((rm,rm1) $ (rm_to_rmtp(rm,rmtp) $ rm_to_rmtp(rm1,rmtp1)), p_tradeFlows(rm,rm1,xx,"cur"));

$endif


*
* ---- define MFN tariffs, in- and out-of-quota tariffs used in the model
*
$batinclude 'arm\set_tariffs.gms' %RP%
*
* ---- define start values for tariffs under TRQs, and imports under TRQs
*
  v_tarSpec.L(%RP%,%RP%1,XX)  $ p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR") = p_trqBilat(%RP%,%RP%1,XX,"TsAppl","CAL");
  v_tarAdVal.L(%RP%,%RP%1,XX) $ p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR") = p_trqBilat(%RP%,%RP%1,XX,"TaAppl","CAL");


  v_tarSpec.L(%RP%,"RW",XX)  $ p_trqGlobl(%RP%,XX,"TrqNT","CUR") = p_trqGlobl(%RP%,XX,"TsAppl","CAL");
  v_tarAdVal.L(%RP%,"RW",XX) $ p_trqGlobl(%RP%,XX,"TrqNT","CUR") = p_trqGlobl(%RP%,XX,"TaAppl","CAL");

  v_TRQImports.L(%RP%,XX) $ p_trqGlobl(%RP%,XX,"TrqNT","CUR") = p_trqGlobl(%RP%,XX,"ImportsR","CAL");

  v_tarSpec.L(%RP%,%RP%1,XX) $ (p_trqGlobl(%RP%,XX,"TrqNT","CUR") $ (NOT p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR")))
     = v_tarSpec.l(%RP%,"RW",XX);

  v_tarAdVal.L(%RP%,%RP%1,XX) $ (p_trqGlobl(%RP%,XX,"TrqNT","CUR") $ (NOT p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR")))
     = v_tarAdVal.L(%RP%,"RW",XX);

$batinclude 'arm\set_tariffs.gms' %RP%


*
* ---- price index for the non-agricultural good
*
  DATA(RmAll,"PPri","Inpe","CUR")   = DATA(RmAll,"PPri","Inpe","BAS") * p_inflationFactor(RmAll);
  DATA(RmAll,"Pmrk","Inpe","CUR")   = DATA(RmAll,"PMrk","Inpe","BAS") * p_inflationFactor(RmAll);
  DATA(RmAll,"CPri","Inpe","CUR")   = DATA(RmAll,"Cpri","Inpe","BAS") * p_inflationFactor(RmAll);
  DATA(RmAll,"Arm1","Inpe","CUR")   = DATA(RmAll,"Arm1","Inpe","BAS") * p_inflationFactor(RmAll);


  DATA(RMSSUP,"PPRI",feed_trd,"CAL") = DATA(RMSSUP,"PPRI",feed_trd,"TRD");
  DATA(RMSSUP,"PPRI",feed_trd,"CUR") = DATA(RMSSUP,"PPRI",feed_trd,"TRD");
*
* --- store specific variables which are used in the welfare calculation as reference point
*
  DATA(RMS,TrdItems,XX1,"TRD")              = DATA(RMS,TrdItems,XX1,"CUR");
  DATA(RM ,"Arm1P",XX1,"TRD")              = DATA(RM,"Arm1P",XX1,"CUR");
*
  DATA(RMS,"FEDM","WHEA","TRD")             = DATA(RMS,"FEDM","SWHE","TRD") + DATA(RMS,"FEDM","DWHE","TRD");
  DATA(RMS,"FEDM","WHEA","BAS")             = DATA(RMS,"FEDM","SWHE","BAS") + DATA(RMS,"FEDM","DWHE","BAS");
*
  DATA(RMS,"Prod","SUGB","TRD")             = DATA(RMS,"Prod","SUGB","CUR");
  DATA(RMS,"Prod","OLIV","TRD")             = DATA(RMS,"Prod","OLIV","CUR");
  DATA(RMS,"Prod","PARI","TRD")             = DATA(RMS,"Prod","PARI","CUR");
  DATA(RMSSUP,"GROF",FEED_TRD,"CUR") = DATA(RMSSUP,"GROF",FEED_TRD,"TRD");

  DATA(RMSSUP,"feed",feed_trd,"TRD") = SUM(FEED_TO_XX(FEED_TRD,XX), DATA(RMSSUP,"FEED",XX,"TRD"));
  DATA(RMSSUP,"feed",feed_trd,"CAL") = DATA(RMSSUP,"feed",feed_trd,"TRD");
  DATA(RMSSUP,"feed",feed_trd,"CUR") = DATA(RMSSUP,"feed",feed_trd,"TRD");

  DATA(RMSSUP,"PPRI",feed_trd,"TRD") $ DATA(RMSSUP,"feed",feed_trd,"TRD")
  = SUM(FEED_TO_XX(FEED_TRD,XX) $ DATA(RMSSUP,"FEED",XX,"TRD"),
                                               DATA(RMSSUP,"FEED",XX,"TRD") * sum(RMS_TO_RM(RMSSUP,RM), DATA(RM,"ARM1P",XX,"TRD")))
                                        / DATA(RMSSUP,"feed",feed_trd,"TRD");
*
*  --- set start values for endogenous variables of market model, and related bounds
*
$include 'arm\set_start_val.gms'


  p_HessNQSupp(RMS,XX,YY,"CUR") $ (not DATA(RMS,"Prod",YY,"CUR")) = 0;

*  --- Before calulating the constant, set p_feedBlkElas to zero
*      if the sum of quantities mapped to the driver in the market model is also zero
*      (This will eliminate equation feedBlkPrice_)

  p_feedBlkElas(RMSSUP,FEED_TRD,FEED_TRD1) $ (NOT sum(feed_to_xx(FEED_TRD1,XXX) $ data(RMSSUP,"FEED",XXX,"CUR"),1)) = 0;


  p_feedBlkCnst(RMSSUP,FEED_TRD) $ SUM(FEED_TO_XX(FEED_TRD,XX), DATA(RMSSUP,"FEED",XX,"CUR"))

     = log(v_feedBlk.l(RMSSUP,FEED_TRD))


          - sum(FEED_TRD1 $ sum(feed_to_xx(FEED_TRD1,XXX) $ data(RMSSUP,"FEED",XXX,"CUR"),1),
                          p_feedBlkElas(RMSSUP,FEED_TRD,FEED_TRD1)
                                * log(v_feedBlkPrice.l(RMSSUP,FEED_TRD1)))
*
*      ---- cross effects with other prices in supply model
*
       - sum(XX $ (p_feedBlkElas(RMSSUP,FEED_TRD,XX) $ DATA(RMSSUP,"prod",xx,"cur")),
                          p_feedBlkElas(RMSSUP,FEED_TRD,XX)
                                * log(max(1.E-3,v_prodPrice.l(RMSSUP,XX))));
*
  XXX(XX)     = YES;
  XXX("MILK") = NO;
  XXX(XX) $ (sum(sameas(xx,xxbiof),1) and not p_endoBioMarket) = NO;

  XXX1(XX1)    = YES;
  XXX1(XXBIOF)   $ (not p_endoBioMarket) = NO;

  XXX_all(XX_all) $ sum(sameas(XXX1,XX_all),1)=yes;

$batinclude 'util\title.gms' "'Introduce scenario shifters'"

  v_tarSpec.L(%RP%,"RW",XX)  $ (p_trqGlobl(%RP%,XX,"TrqNT","CUR") $ (p_trqGlobl(%RP%,XX,"TsMFN","CAL") gt eps))
     = p_trqGlobl(%RP%,XX,"TsAppl","CAL") * MAX(0.1,p_trqGlobl(%RP%,XX,"TsMFN","CUR")/p_trqGlobl(%RP%,XX,"TsMFN","CAL"));

  v_tarAdVal.L(%RP%,"RW",XX) $ (p_trqGlobl(%RP%,XX,"TrqNT","CUR") $ (p_trqGlobl(%RP%,XX,"TaMFN","CAL") gt eps))
     = p_trqGlobl(%RP%,XX,"TaAppl","CAL") * MAX(0.1,p_trqGlobl(%RP%,XX,"TaMFN","CUR")/p_trqGlobl(%RP%,XX,"TaMFN","CAL"));

  v_TRQImports.L(%RP%,XX) = p_trqGlobl(%RP%,XX,"ImportsR","CAL");
  v_TRQImports.L(%RP%,XX) $ p_trqGlobl(%RP%,XX,"TrqNT","CAL")
      = p_trqGlobl(%RP%,XX,"ImportsR","CAL") *  p_trqGlobl(%RP%,XX,"TrqNT","CUR")/p_trqGlobl(%RP%,XX,"TrqNT","CAL");


  v_tradeFlows.L(%RP%,%RP%1,XX) $ (p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CAL") $ (p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR") ne prohibitive))
      = v_tradeFlows.l(%RP%,%RP%1,XX) *  p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR")/p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CAL");

  v_tarSpec.L(%RP%,%RP%1,XX) $ (p_trqGlobl(%RP%,XX,"TrqNT","CUR") $ (NOT p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR")))
     = v_tarSpec.l(%RP%,"RW",XX);

  v_tarAdVal.L(%RP%,%RP%1,XX) $ (p_trqGlobl(%RP%,XX,"TrqNT","CUR") $ (NOT p_trqBilat(%RP%,%RP%1,XX,"TrqNT","CUR")))
      = v_tarAdVal.L(%RP%,"RW",XX);
*
*  ----- Check if user tries to set endogenous prices
*
  if ( sum( (RMSSUP,OMS_XX,ScenShifterPos)
                $ (DATA(RMSSUP,"PROD",OMS_XX,ScenShifterPos) or DATA(RMSSUP,"FEED",OMS_XX,ScenShifterPos)),1) ne 0,
     p_errorsInScenShifters(RMSSUP,"PROD",OMS_XX,ScenshifterPos ) = DATA(RMSSUP,"PROD",OMS_XX,ScenShifterPos);
     p_errorsInScenShifters(RMSSUP,"FEED",OMS_XX,ScenshifterPos ) = DATA(RMSSUP,"FEED",OMS_XX,ScenShifterPos);
     abort " Scenario definition comprises exogenous changes for production",
           " or feed of countries covered by supply side: ",p_errorsInScenShifters;
  );

*  ----- Check if user tries to fix areas in market model other than OCRO+NECR
*
  if ( sum( (RMS,ScenShifterPos,xx_all) $ (not ocNec(xx_all)),DATA(RMS,"LEVL",xx_all,ScenShifterPos)),
     p_errorsInScenShifters(RMS,"LEVL",xx_all,ScenshifterPos ) = DATA(RMS,"LEVL",xx_all,ScenShifterPos);
     execute_unload "%results_out%\capmod\abort_%system.fn%_%system.incline%.gdx";
     abort "Scenario definition tries to fix endogenous LEVLs from market model in %system.fn%, line %system.incline%";
  );

*  --- shift transport cost according to exogenously defined changefactors
*
  v_transpCost.fx(RM,RM1,XX) $ ((   DATAOUT("WORLD",RM1,"tCost","ALLP","AbsoluteChange")
                                 or DATAOUT(RM,"WORLD","tCost","ALLP","AbsoluteChange")
                                 or DATAOUT("WORLD",RM1,"tCost",XX,"AbsoluteChange")
                                 or DATAOUT(RM,"WORLD","tCost",XX,"AbsoluteChange")
                                 or DATAOUT(RM,RM1,"tCost",XX,"AbsoluteChange"))
                               $ v_transpCost.l(RM,RM1,XX))
     = v_transpCost.l(RM,RM1,XX)
     + DATAOUT("WORLD",RM1,"tCost","ALLP","AbsoluteChange")
     + DATAOUT(RM,"WORLD","tCost","ALLP","AbsoluteChange")
     + DATAOUT("WORLD",RM1,"tCost",XX,"AbsoluteChange")
     + DATAOUT(RM,"WORLD","tCost",XX,"AbsoluteChange")
     + DATAOUT(RM,RM1,"tCost",XX,"AbsoluteChange")
     ;

  v_transpCost.fx(RM,RM1,XX) $ ((   DATAOUT("WORLD",RM1,"tCost","ALLP","ChangeFactor")
                                 or DATAOUT(RM,"WORLD","tCost","ALLP","ChangeFactor")
                                 or DATAOUT("WORLD",RM1,"tCost",XX,"ChangeFactor")
                                 or DATAOUT(RM,"WORLD","tCost",XX,"ChangeFactor")
                                 or DATAOUT(RM,RM1,"tCost",XX,"ChangeFactor"))
                               $ v_transpCost.l(RM,RM1,XX))
     = v_transpCost.l(RM,RM1,XX)
     * (DATAOUT("WORLD",RM1,"tCost","ALLP","ChangeFactor")  + 1 $ (NOT DATAOUT("WORLD",RM1,"tCost","ALLP","ChangeFactor")))
     * (DATAOUT(RM,"WORLD","tCost","ALLP","ChangeFactor")   + 1 $ (NOT DATAOUT(RM,"WORLD","tCost","ALLP","ChangeFactor") ))
     * (DATAOUT("WORLD",RM1,"tCost",XX,"ChangeFactor")      + 1 $ (NOT DATAOUT("WORLD",RM1,"tCost",XX,"ChangeFactor")    ))
     * (DATAOUT(RM,"WORLD","tCost",XX,"ChangeFactor")       + 1 $ (NOT DATAOUT(RM,"WORLD","tCost",XX,"ChangeFactor")     ))
     * (DATAOUT(RM,RM1,"tCost",XX,"ChangeFactor")           + 1 $ (NOT DATAOUT(RM,RM1,"tCost",XX,"ChangeFactor")         ))
     ;

  v_transpCost.fx(RM,RM1,XX) $ ((   DATAOUT("WORLD",RM1,"tCost","ALLP","PercentageChange")
                                 or DATAOUT(RM,"WORLD","tCost","ALLP","PercentageChange")
                                 or DATAOUT("WORLD",RM1,"tCost",XX,"PercentageChange")
                                 or DATAOUT(RM,"WORLD","tCost",XX,"PercentageChange")
                                 or DATAOUT(RM,RM1,"tCost",XX,"PercentageChange"))
                               $ v_transpCost.l(RM,RM1,XX))
     = v_transpCost.l(RM,RM1,XX)
     * (1 + DATAOUT("WORLD",RM1,"tCost","ALLP","PercentageChange")/100)
     * (1 + DATAOUT(RM,"WORLD","tCost","ALLP","PercentageChange") /100)
     * (1 + DATAOUT("WORLD",RM1,"tCost",XX,"PercentageChange")    /100)
     * (1 + DATAOUT(RM,"WORLD","tCost",XX,"PercentageChange")     /100)
     * (1 + DATAOUT(RM,RM1,"tCost",XX,"PercentageChange")         /100)
     ;

*
*  --- shift human consumption, feed, processing, production according to exogenously defined amounts
*      (using XX1 permits to pick up shifters for PROD.LAND = land area for non-supply model regions)
*
*      -- First implement production changes in non-RMSSUP as yield shocks (before changing PROD(CUR)):
  DATA(RMS,"Yild",XX_landuse,"ChangeFactor") $ ( DATA(RMS,"PROD",XX_landuse,"CUR")
                                               $ DATA(RMS,"PROD",XX_landuse,"AbsoluteLevel") $ (NOT RMSSUP(RMS)))
     = DATA(RMS,"PROD",XX_landuse,"AbsoluteLevel")/DATA(RMS,"PROD",XX_landuse,"CUR");
  DATA(RMS,"Yild",XX_landuse,"ChangeFactor") $ ( DATA(RMS,"PROD",XX_landuse,"CUR")
                                               $ DATA(RMS,"PROD",XX_landuse,"AbsoluteChange") $ (NOT RMSSUP(RMS)))
     = (DATA(RMS,"PROD",XX_landuse,"CUR")+DATA(RMS,"PROD",XX_landuse,"AbsoluteChange"))/DATA(RMS,"PROD",XX_landuse,"CUR");
  DATA(RMS,"Yild",XX_landuse,"ChangeFactor")  $ (DATA(RMS,"PROD",XX_landuse,"ChangeFactor") $ (NOT RMSSUP(RMS)))
     =  DATA(RMS,"PROD",XX_landuse,"ChangeFactor");
  DATA(RMS,"Yild",XX_landuse,"ChangeFactor")  $ (DATA(RMS,"PROD",XX_landuse,"PercentageChange") $ (NOT RMSSUP(RMS)))
     =   (1. + DATA(RMS,"PROD",XX_landuse,"PercentageChange")/100);
  DATA(RMS,"Yild",XX_landuse,"CUR")  $ DATA(RMS,"Yild",XX_landuse,"ChangeFactor")
       = DATA(RMS,"Yild",XX_landuse,"CUR") * DATA(RMS,"Yild",XX_landuse,"ChangeFactor");

*      -- Second assume that 50% of a change in foreOlndArtif is matched by a change in agric area supply PROD.LAND:
*      -- Second shift shares in HCON

  DATA(RMS,ShareInHcon,XX,"CUR")  $ (DATA(RMS,ShareInHcon,XX,"AbsoluteLevel") gt eps)
      = DATA(RMS,ShareInHcon,XX,"AbsoluteLevel");
*
  DATA(RMS,ShareInHcon,XX,"CUR")  $ (DATA(RMS,ShareInHcon,XX,"AbsoluteChange") gt eps)
     = DATA(RMS,ShareInHcon,XX,"CUR") + DATA(RMS,ShareInHcon,XX,"AbsoluteChange");
*
  DATA(RMS,ShareInHcon,XX,"CUR")  $ DATA(RMS,ShareInHcon,XX,"ChangeFactor")
       = DATA(RMS,ShareInHcon,XX,"CUR") * DATA(RMS,ShareInHcon,XX,"ChangeFactor");
*
  DATA(RMS,ShareInHcon,XX,"CUR")  $ DATA(RMS,ShareInHcon,XX,"PercentageChange")
   = DATA(RMS,ShareInHcon,XX,"CUR") * (1. + DATA(RMS,ShareInHcon,XX,"PercentageChange")/100);


*
*      -- Now the general case:
  DATA(RMS,ScenItems,XX1,"CUR")  $ DATA(RMS,ScenItems,XX1,"AbsoluteLevel")
      = DATA(RMS,ScenItems,XX1,"AbsoluteLevel");
*
  DATA(RMS,ScenItems,XX1,"CUR")  $ DATA(RMS,ScenItems,XX1,"AbsoluteChange")
     = DATA(RMS,ScenItems,XX1,"CUR") + DATA(RMS,ScenItems,XX1,"AbsoluteChange");
*
  DATA(RMS,ScenItems,XX1,"CUR")  $ DATA(RMS,ScenItems,XX1,"ChangeFactor")
       = DATA(RMS,ScenItems,XX1,"CUR") * DATA(RMS,ScenItems,XX1,"ChangeFactor");
*
  DATA(RMS,ScenItems,XX1,"CUR")  $ DATA(RMS,ScenItems,XX1,"PercentageChange")
   = DATA(RMS,ScenItems,XX1,"CUR") * (1. + DATA(RMS,ScenItems,XX1,"PercentageChange")/100);
*##no change if activated as no constrained included yet that addresses the aggregate of HCON
*DATA(RMS,"HCON","INCE","CUR") $ DATA(RMS, "HCON", "ALLP", "PercentageChange")
* = sum(XX1, DATA(RMS,"HCON",XX1,"CUR"))*(1.+ DATA(RMS, "HCON", "ALLP", "PercentageChange")/100);

$ifi %E3IOT_1A%==ON $include 'policy\E3IOT_demand_shift_1A.gms'
$ifi %E3IOT_1B%==ON $include 'policy\E3IOT_demand_shift_1B.gms'
$ifi %E3IOT_2A%==ON $include 'policy\E3IOT_demand_shift_2A.gms'
$ifi %E3IOT_2B%==ON $include 'policy\E3IOT_demand_shift_2B.gms'
$ifi %E3IOT_3A%==ON $include 'policy\E3IOT_demand_shift_3A.gms'
$ifi %E3IOT_3B%==ON $include 'policy\E3IOT_demand_shift_3B.gms'

*al201808 - write out human consumption data for checks [use temporary location]
*           Attention! p_temp4d not generally defined, works currently only with policy file ./epnf/epnf_scenario.gms
*p_temp4d(rms,ScenItems,XX1,ScenShifterPos_COLS)=DATA(rms,ScenItems,XX1,ScenShifterPos_COLS);
*p_temp4d(rms,ScenItems,xx_landuse,ScenShifterPos_COLS)=DATA(rms,ScenItems,xx_landuse,ScenShifterPos_COLS);
*p_temp4d(rms,ScenItems,XX1,"cur")=DATA(rms,ScenItems,XX1,"cur");
*execute_unload '%curdir%/pol_input/epnf/prepmarkethumanconsumption.gdx' ScenItems,ScenShifterPos_COLS,xx1,xx_landuse,p_temp4d;
*$stop

** Adjust new energy crop areas according to second generation biofuels

*
*  First check that there are no exogenous settings for the new energy crop area
*
$SETGLOBAL exog_NECR no
  If( SUM((RU,ScenShifterPos)$ DATA(RU,"NECR","LEVL",ScenShifterPos),1),
$SETGLOBAL exog_NECR yes
  );
$ifi %exog_NECR% == no $include 'biofuel\adjust_newenergycrops.gms'

*
*     ---- recalibrate constant terms of behavioural function to match
*          quantity/price point
*
$batinclude 'util\title.gms' "'Calibrate constant terms of behavioural functions'"
*
$include 'arm\cal_cnst_terms.gms'



  p_cSugarCalPar(RMS) = 0;
  data(RMSSUP,"PPRI","SUGBc","CUR") = sum(rm_to_rms(rm,rmssup),
           data(rm,"Arm1P","SUGA","CUR"));
*
*  --- delete calibration point - will not longer be used
*
* ### MA: PROC SUGA CAL still needed in set prices...  DATA(RMall,SITEMS,XX1,"CAL")  = 0;
  p_tradeFlows(RMALL,RMALL1,XX,"CAL")        = 0;
  p_tradeFlows(RMALL,RMALL1,XX,"BAS")        = 0;
  p_tradeFlows(RMALL,"RW",XX,"CAL")       = 0;
  p_tradeFlows(RMALL,"RW",XX,"BAS")       = 0;
  p_impPrice(RM,RM1,XX,"CAL")        = 0;
  p_impPrice(RM,RM1,XX,"BAS")        = 0;
  p_impPrice(RM,RM1,XX,"Y")          = 0;
*
  p_trim = 0;

*
*  --- set start values for endogenous variables of market model, and related bounds
*
$include 'arm\set_start_val.gms'
*
*  --- readjust demand system
*      That might be scenario specific!
*
$include 'arm\recalibrate_commitments.gms'
*
*  --- set start values again (to re-define bounds of variables fixed in cal_armington)
*
$include 'arm\set_start_val.gms'

*
*  --- Apply market support shifters from scenario
*      (Using XX1 includes land in case that land subsidies PSED.LAND are changed)
*
  DATA(RMS,SupportMarketModel,XX1,"CUR")  $ DATA(RMS,SupportMarketModel,XX1,"AbsoluteLevel")
      = DATA(RMS,SupportMarketModel,XX1,"AbsoluteLevel");
*
  DATA(RMS,SupportMarketModel,XX1,"CUR")  $ DATA(RMS,SupportMarketModel,XX1,"AbsoluteChange")
     = DATA(RMS,SupportMarketModel,XX1,"CUR") + DATA(RMS,SupportMarketModel,XX1,"AbsoluteChange");
*
  DATA(RMS,SupportMarketModel,XX1,"CUR")  $ DATA(RMS,SupportMarketModel,XX1,"ChangeFactor")
       = DATA(RMS,SupportMarketModel,XX1,"CUR") * DATA(RMS,SupportMarketModel,XX1,"ChangeFactor");
*
  DATA(RMS,SupportMarketModel,XX1,"CUR")  $ DATA(RMS,SupportMarketModel,XX1,"PercentageChange")
   = DATA(RMS,SupportMarketModel,XX1,"CUR") * (1. + DATA(RMS,SupportMarketModel,XX1,"PercentageChange")/100);

*
*  --- fix trade flows to values given by scenario
*
   v_tradeFlows.fx(RM,RM1,XX) $ (     p_tradeFlows(RM,RM1,XX,"CUR")
                                 and  p_fixedTradeFlows(RM,RM1,XX))
                                    = p_fixedTradeFlows(RM,RM1,XX);
*
   NonZeroFixedImportFlow(RM,RM1,XX) $ (p_tradeFlows(RM,RM1,XX,"Cur") $ (not SAMEAS(RM,RM1)) $ (not v_tradeFlows.Range(RM,RM1,XX))) = yes;
*
   v_impShadowPrice.L (RM,RM1,XX) $ (p_impPrice(RM,RM1,XX,"CUR")  $ NonZeroFixedImportFlow(RM,RM1,XX)) = p_impPrice(RM,RM1,XX,"CUR");
   v_impShadowPrice.LO(RM,RM1,XX) $ (p_impPrice(RM,RM1,XX,"CUR")  $ NonZeroFixedImportFlow(RM,RM1,XX)) = MAX(1.E-3+1.E-5,v_impShadowPrice.L(RM,RM1,XX) * P_lowLimitFactPrice/10);
   v_impShadowPrice.UP(RM,RM1,XX) $ (p_impPrice(RM,RM1,XX,"CUR")  $ NonZeroFixedImportFlow(RM,RM1,XX)) = v_impShadowPrice.L(RM,RM1,XX) * 1000;


*  ---  Set biofuel demand to scenario value
*
*
IF(p_endoBioMarket NE 1,
   v_prodBiof.FX(RMS,XXBioF)    =    DATA(RMS,"PROD",XXBioF,"CUR")
                                -   DATA(RMS,"NAGR",XXBioF,"CUR")
                                -   DATA(RMS,"SECG",XXBioF,"CUR")
                                -   DATA(RMS,"EXOG",XXBioF,"CUR");


   );



*
*   --- Initialize trade policy variables
*
  v_entryPriceDriver.L(%RP%,%RP%1,XXX) $ ( (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                                        $ (NOT SAMEAS(%RP%,%RP%1)) $ (DATA(%RP%,"TriggerP",XXX,"CUR") gt eps))

       =  (v_entryPrice.L(%RP%,%RP%1,XXX) *(0.98+0.92)/2
*             (exchange rate adjustment applies to marketPrice and transpCost, assuming the latter is handled by exporting country)
             - (v_marketPrice.L(%RP%1,XXX)+v_transpCost.L(%RP%,%RP%1,XXX))*p_exchgRateChangeFactor(%RP%,%RP%1))
                    / DATA(%RP%,"TriggerP",XXX,"CUR") * p_entryPriceFac(%RP%,%RP%1,XXX,"CUR");
*
*
  v_tarSpec.L(%RP%,%RP%1,XXX) $ ((DATA(%RP%,"TriggerP",XXX,"CUR") gt eps) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                             $ ((v_tarSpec.LO(%RP%,%RP%1,XXX) NE v_tarSpec.up(%RP%,%RP%1,XXX)) or p_Trim)
                             $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (NOT SAMEAS(%RP%,%RP%1)))
*
   =  (EXP( MIN(0,v_entryPriceDriver.L(%RP%,%RP%1,XXX)))/(1+20*EXP(-ABS(v_entryPriceDriver.L(%RP%,%RP%1,XXX)))))

        * (  DATA(%RP%,"TARS",XXX,"CUR")          $ (NOT p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR"))
           + p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR")  $      p_trqBilat(%RP%,%RP%1,XXX,"TsPref","CUR"));


  v_flexLevyNotCut.L(%RP%,%RP%1,XXX) $ ((DATA(%RP%,"MinBordP",XXX,"CUR") gt eps) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                               $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (NOT SAMEAS(%RP%,%RP%1)))

    =   -ncpcm(-(DATA(%RP%,"MinBordP",XXX,"CUR")
*             (exchange rate adjustment applies to marketPrice and transpCost, assuming the latter is handled by exporting country)
               -(v_marketPrice.l(%RP%1,XXX)+v_transpCost.l(%RP%,%RP%1,XXX))*p_exchgRateChangeFactor(%RP%,%RP%1)),0,1.E-3*DATA(%RP%,"MinBordP",XXX,"CUR"));

  v_flexLevy.L(%RP%,%RP%1,XXX) $ ( (DATA(%RP%,"MinBordP",XXX,"CUR") gt eps) $ (p_tradeFlows(%RP%,%RP%1,XXX,"CUR") or p_arm2Commit(%rp%,%rp%1,xxx))
                                $ (NOT p_doubleZero(%RP%,%RP%1,XXX,"CUR")) $ (NOT SAMEAS(%RP%,%RP%1)))

    = ncpcm(v_flexLevyNotCut.l(%RP%,%RP%1,XXX),v_tarSpec.l(%RP%,%RP%1,XXX),1.E-3*DATA(%RP%,"MinBordP",XXX,"CUR"));



  v_trqSigmoidFunc.l(%RP%,%RP%1,XXX) $

               (   ( (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") GT eps)
                 $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE 1.E+10)) $ (p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR") NE prohibitive))

        = Sigmoid(p_trqSigmoidSlope(%RP%,XXX)/p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR")
                                                     * (v_tradeFlows.l(%RP%,%RP%1,XXX)-p_trqBilat(%RP%,%RP%1,XXX,"TrqNT","CUR")*0.97));


  v_trqSigmoidFunc.lo(%RP%,%RP%1,XXX) $  v_trqSigmoidFunc.l(%RP%,%RP%1,XXX) =  -0.001;
  v_trqSigmoidFunc.up(%RP%,%RP%1,XXX) $  v_trqSigmoidFunc.l(%RP%,%RP%1,XXX) =  1.5;
*
  v_consQuant.FX(RMS,XX) $ (v_GLDemandGs.L(RMS) $ (NOT SUM(YY1, ABS(p_pbGL(RMS,XX,YY1,"CUR"))))) = DATA(RMS,"Hcon",XX,"CUR");
*
*  ----- shift transport costs according to oil price shock and price transmission
*        elasticity

  v_transpCost.fx(RM,RM1,XX) $ (NOT sum(ScenShifterPos $ (   DATAOUT("WORLD",RM1,"tCost","ALLP",ScenShifterPos)
                                                          or DATAOUT(RM,"WORLD","tCost","ALLP",ScenShifterPos)
                                                          or DATAOUT("WORLD",RM1,"tCost",XX,ScenShifterPos)
                                                          or DATAOUT(RM,"WORLD","tCost",XX,ScenShifterPos)
                                                          or DATAOUT(RM,RM1,"tCost",XX,ScenShifterPos)         ),1))
       = v_transpCost.l(RM,RM1,XX)
       * EXP( (p_oilPriceTransMissElas(RM,"EFUL")+p_oilPriceTransMissElas(RM1,"EFUL"))/2
         * log( (1. + 0.5*(DATA(RM,"UVAG","CRDO","PercentageChange")+DATA(RM1,"UVAG","CRDO","PercentageChange"))/100)));

$iftheni.rmtp %RP% == RMTP
   v_transpCost.fx(RMTP,RMTP1,XX)
       $  { sum((RM_TO_RMTP(rm,rmtp),RM1_TO_RMTP(rm1,rmtp1)) $ (not sameas(rm,rm1)), DATA(RM,"UVAG","CRDO","PercentageChange")
                                                                                    +DATA(RM1,"UVAG","CRDO","PercentageChange"))
          $ SUM((RMTP_TO_RM(RMTP,RM),RMTP_TO_RM1(RMTP1,RM1)) $ (not sameas(rm,rm1)), p_tradeFlows(RM,RM1,XX,"CUR"))
          $ (not sameas(RMTP,RMTP1) $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1)))
          }
    = SUM((RMTP_TO_RM(RMTP,RM),RMTP_TO_RM1(RMTP1,RM1)) $ (not sameas(rm,rm1)),v_transpCost.L(RM,RM1,XX)*p_tradeFlows(RM,RM1,XX,"CUR"))
    / SUM((RMTP_TO_RM(RMTP,RM),RMTP_TO_RM1(RMTP1,RM1)) $ (not sameas(rm,rm1)),p_tradeFlows(RM,RM1,XX,"CUR"));
$endif.rmtp

*
*  --- Change in exchange rates according to scenario shifters
*      A revaluation of the currency of exporter RM1 (DATA(RM1,"UVAG","INPE","PercentageChange") > 0)
*      implies that import prices in region RM will increase (and become less attractive)
*      Conversely a revaluation of the currency of importer RM (DATA(RM,"UVAG","INPE","PercentageChange") > 0)
*      implies that import prices in region RM will decrease in the importers currency (and hence become more attractive)
*
*
  p_exchgRateChangeFactor(RM,RM1) $ (NOT SAMEAS(RM,RM1))
        = p_exchgRateChangeFactor(RM,RM1)
        * (1+DATA(RM1,"UVAG","INPE","PercentageChange")/100)
        / (1+DATA(RM,"UVAG","INPE","PercentageChange")/100);
$iftheni.rmtp %RP% == RMTP
  p_exchgRateChangeFactor(RMTP,RMTP1)
    $ (  (NOT SAMEAS(RMTP,RMTP1))
      $ sum(( RM_TO_RMTP(rm,rmtp), RM1_TO_RMTP(rm1,rmtp1)),  (p_exchgRateChangeFactor(RM,RM1) gt eps) )
      $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1)))
    = sum((RM_TO_RMTP(rm,rmtp),RM1_TO_RMTP(rm1,rmtp1)),  p_exchgRateChangeFactor(RM,RM1) )
    / sum((RM_TO_RMTP(rm,rmtp),RM1_TO_RMTP(rm1,rmtp1)),  (p_exchgRateChangeFactor(RM,RM1) gt eps) );
$endif.rmtp
*
*   --- shift production costs for agriculture (= price index of inputs)
*       with oil price shock
*
* DATA(RMS,"Ppri","INPE","CUR") $ (not RMSSUP(RMS))
* = DATA(RMS,"Ppri","INPE","CUR")
*
*        * (1. + (DATA(RMS,"UVAG","CRDO","PercentageChange")/100) * 0.17);
*
*  --- take additional shifts from scenario definition (INCE.LEVL = GDP, INHA.LEVL = Population)
*
   DATA(RMS,"INCE","LEVL","CUR") $ DATA(RMS,"INCE","LEVL","PercentageChange")
     =  DATA(RMS,"INCE","LEVL","CUR") * (1. + DATA(RMS,"INCE","LEVL","PercentageChange") /100);

   DATA(RMS,"INCE","LEVL","CUR") $ DATA(RMS,"INCE","LEVL","AbsoluteLevel")
     = DATA(RMS,"INCE","LEVL","AbsoluteLevel");

   DATA(RMS,"INHA","LEVL","CUR") $ DATA(RMS,"INHA","LEVL","PercentageChange")
     =  DATA(RMS,"INHA","LEVL","CUR") * (1. + DATA(RMS,"INHA","LEVL","PercentageChange") /100);

   DATA(RMS,"INHA","LEVL","CUR") $ DATA(RMS,"INHA","LEVL","AbsoluteLevel")
     = DATA(RMS,"INHA","LEVL","AbsoluteLevel");

*
*  --- If the scenario file includes a setting for v_caloPerCap then fix it here and release the calory tax as a paired price variable
*      NOTE:
*            The specification of a common target for MER is an undesired side effect, accepted under SUSFANS
    v_caloPerCap.fx(RMS) $ DATA(RMS,"INHA","n_cal","AbsoluteLevel") = DATA(RMS,"INHA","n_cal","AbsoluteLevel");
    v_caloTax.up(RMS) $ (not v_caloPerCap.range(RMS)) =  inf;
    v_caloTax.lo(RMS) $ (not v_caloPerCap.range(RMS)) = -inf;
*      The calory tax may drive consumption of energy rich and cheap food (like OTHO in our data) to zero and beyond
*      => for feasibility we activate the fudging function with bounds < 0:
    v_consQuant.LO(RMS,XX)
                              $ (DATA(RMS,"INHA","n_cal","AbsoluteLevel")
                            $  (DATA(RMS,"HCon",XX,"CUR") $ (xoil(xx) or sameas(xx,"otho") or sameas(xx,"olio")))
)
          = - max(10,DATA(RMS,"HCon",XX,"CUR"));
*      Consider that the price increase due to the calory tax may increase consumer prices up to their standard bounds =>
    v_consPrice.UP(RMS,XX)
                            $ (DATA(RMS,"INHA","n_cal","AbsoluteLevel")
                            $  v_consPrice.l(RMS,XX) $ v_consPrice.range(RMS,XX)) = 10*v_consPrice.UP(RMS,XX) ;

*

$batinclude 'util\title.gms' "'Solve market model at trend values, year %SIMY%'"
*
   option kill=v_cifPrice;
*
*  --- remove unused upper/lower bounds, level and marginals
*      (tenthousands of numbers)




$batinclude 'util\title.gms' "'price/quantity settings for the modified Armington lower nest'"

*
*   --- Price settings for enabled 'emerging' trade relations. There the commitment term is non-zero
*
option p_impPrice:0:3:1;

p_impPrice(rm,rm1,xx,"cur") $ (p_arm2Commit(rm,rm1,xx) $ (not p_impPrice(rm,rm1,xx,"cur")))
   = (data(rm1,"pmrk",xx,"cur") * p_exchgRateChangeFactor(rm,RM1) + v_transpCost.l(rm,rm1,xx) * p_exchgRateChangeFactor(rm,RM1))

*      ad valorem tariff
          *  ( 1. + 0.01*v_tarAdVal.l(rm,rm1,xx) $ ( (NOT p_doubleZero(rm,RM1,xx,"CUR")) $ (NOT SAMEAS(rm,RM1))))

*      specific tariff and flex. levy (for EU)
          +  [

*              --- fixed according to tariff schedule or endogenous under TRQ
                v_tarSpec.l(rm,RM1,xx) $ (   (DATA(rm,"MinBordP",xx,"CUR") LE eps)
                                         or (p_trqBilat(rm,RM1,xx,"TrqNT","CUR") eq prohibitive) )

*                --- or flexible levy in case of minimum border prices
              + v_flexLevy.l(rm,RM1,xx)  $ (     (DATA(rm,"MinBordP",xx,"CUR") GT eps)
                                            $ (p_trqBilat(rm,RM1,xx,"TrqNT","CUR") ne prohibitive) )

              ] $ ((NOT p_doubleZero(rm,RM1,xx,"CUR")) $ (NOT SAMEAS(rm,RM1)))
       ;



v_impPrice.l(rm,rm1,xx)     $ (p_arm2Commit(rm,rm1,xx) $ (not v_impPrice.l(rm,rm1,xx)))     = p_impPrice(rm,rm1,xx,"cur");
v_impPrice.up(rm,rm1,xx)    $ p_arm2Commit(rm,rm1,xx)                                       = v_impPrice.L(RM,RM1,XX) * 100;
v_impPrice.LO(RM,RM1,XX)    $ (p_arm2Commit(rm,rm1,xx) $ p_impPrice(RM,RM1,XX,"CUR"))       = MAX(1.E-3+1.E-5,v_impPrice.L(RM,RM1,XX) * P_lowLimitFactPrice);


  option P_trqBilat:0:4:1;
  option v_tarSpec:3:2:1;
  option v_tarAdval:3:2:1;
  option v_transpCost:3:2:1;

*display p_impPrice, P_trqBilat, v_tarSpec.l, v_tarAdVal.l, v_transpCost.l;
*abort "check import prices";


*
*   --- initialize trade flows with commitment terms
*       import price-dependent, that's why this calculation is shifted to here
*

 option v_tradeFlows:1:2:1;
 option v_impPrice:1:2:1;

 v_tradeFlows.l(rm,rm1,xx) $ p_arm2Commit(rm,rm1,xx)
    = v_arm2Quant.l(rm,xx) * p_dpCESTrade(rm,rm1,xx)
        *  (v_arm2Price.l(rm,xx) / v_impPrice.l(rm,rm1,xx)) ** p_rhoArm2(rm,xx)
        + p_arm2Commit(rm,rm1,xx)
    ;

*  test if tradeflows are correctly initialized
*  and the non-homothetic Armington system correctly calibrated
*  [uncomment for the reference run only]
* parameter p_iszero(rm,rm,xx) "is it really zero?";
* p_iszero(rm,rm1,xx) $ p_arm2Commit(rm,rm1,xx) = abs(v_tradeFlows.l(rm,rm1,xx) - p_tradeFlows(rm,rm1,xx,"cur"));
* if(sum( (rm,rm1,xx), p_iszero(rm,rm1,xx)) gt 1.E-2, abort "problem with initial trade flows: check calibration of commitment terms", p_iszero);



*
*mih --- include import share equations for emerging trade flows
*        i.e. allow emerging trade flows to become  non-zero
*     Technically we need to avoid that trade flows and related variables are fixed to zero in the code below:
*                 (1) define non-zero (tiny) trade flow in p_tradeFlows
*                 (2) set tiny flows back to zero before solve
*

v_tradeFlows.l(rm,rm1,xx) $ (p_arm2Commit(rm,rm1,xx) $ (not p_tradeFlows(rm,rm1,xx,"cur")))
  = max(1.E-3, v_tradeFlows.l(rm,rm1,xx));
p_tradeFlows(rm,rm1,xx,"cur") $ (p_arm2Commit(rm,rm1,xx) $ (not p_tradeFlows(rm,rm1,xx,"cur")))
  = v_tradeFlows.l(rm,rm1,xx);

option p_tradeFlows:0:3:1;
*abort "check flows", p_tradeFlows, v_tradeFlows.l, p_arm2Commit;

*
*   --- adjust bounds and levels at RMTP level accordingly
*
$iftheni.lower %RP% == RMTP

 v_tradeFlows.L(rmtp,rmtp1,XX) $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1))
   = sum((RM,RM1) $ (rm_to_rmtp(RM,rmtp) $ rm_to_rmtp(RM1,rmtp1) ), v_tradeFlows.L(RM,RM1,XX));
 v_tradeFlows.UP(rmtp,rmtp1,XX) $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1))
   = sum((RM,RM1) $ (rm_to_rmtp(RM,rmtp) $ rm_to_rmtp(RM1,rmtp1) ), v_tradeFlows.UP(RM,RM1,XX));

$endif.lower



$iftheni.taggmodule %tagg_module% == on
$batinclude 'util\title.gms' "'Call the tariff aggregation module (calibrate it and apply lib. scenario at tariff line level)'"

$ifi not %threads% == on abort "tariff aggregation module only works with threads on";

* this .gdx file contains all parameters/settings passed on to the tariff aggregation module by CAPMOD
    execute "if exist %scrdir%\to_tariff_aggregation.gdx del %scrdir%\to_tariff_aggregation.gdx";

* wrt. to this .gdx file:
*       1) a flag file, i.e. the parent process waits until its created at the end of the tariff aggregation module
*       2) contains all results passed back to the main CAPMOD process
    execute "if exist %scrdir%\from_tariff_aggregation.gdx del %scrdir%\from_tariff_aggregation.gdx";

    execute "if not exist %scrdir%\tagg_module mkdir %scrdir%\tagg_module";

    execute_unload "%scrdir%\to_tariff_aggregation.gdx" DATA,p_tradeFlows,p_impPrice,p_exchgRateChangeFactor,v_transpCost
                                                 v_tarSpec,v_tarAdVal,v_flexLevy
                                                 p_trqBilat,p_trqGlobl,p_doubleZero,
                                                 tline_prod,importer,exporter,hs8list
                                                 p_tline_qchange, p_tlinecuts, p_fixdomExpQ
                                                 p_rentShare0, p_tariff_R0, p_rent_null
                                                 p_rhoArm1, p_rhoArm2
                                                 ;
*
*   ---    Uncomment before using the test routines in test_tagg_module.gms
*          Creates the necessary .gdx file in the scratch directory
*
*  abort "scratch file created";

    put_utility batch 'shell'  / 'start /B /NORMAL %GAMSEXE%' ' %CURDIR%\arm\tagg_module.gms'
                                ' %GAMSARG%' ' -scrdir=%SCRDIR% --scrdir=%scrdir%'
                                ' --threads=on'  ' --tariffshs=%tariffshs%'
$ifi %generateRefExpFile%==ON         ' -rf=%expRefDir%\tagg_module.ref' ' -ef=%expRefDir%\tagg_module.exp'
                                ' -o=%scrdir%\tagg_module.lst' ' -procdir=%scrdir%\tagg_module >nul'    ' --Farm_SelRoutineVersion08=%Farm_SelRoutineVersion08%';

*   puts the statement in the log file and on the screen
    put_utility batch 'msglog' / 'start /B /NORMAL %GAMSEXE%' ' %CURDIR%\arm\tagg_module.gms'
                                ' %GAMSARG%' ' -scrdir=%SCRDIR%'
                                ' --threads=on'  ' --tariffshs=%tariffshs%'
$ifi %generateRefExpFile%==ON         ' -rf=%expRefDir%\tagg_module.ref' ' -ef=%expRefDir%\tagg_module.exp'
                                ' -o=%scrdir%\tagg_module.lst' ' -procdir=%scrdir%\tagg_module'    ' --Farm_SelRoutineVersion08=%Farm_SelRoutineVersion08%';


*

$batinclude 'util\title.gms' "' Waiting for results from tariff aggregation (done on separate thread)'"

   PARAMETER p_welfareRes(RALL,COLS,ROWS,*) "Report on welfare analysis from market model";
   execute "=%curdir%\util\taskSync.bat 1 120 %scrdir%\from_tariff_aggregation.gdx NOT";
   p_dummy = sleep(0.1);
   execute_load "%scrdir%\from_tariff_aggregation.gdx",  p_tagg;
*   abort "check tagg module";

$endif.taggmodule



$batinclude 'util\title.gms' "'Solve market model at trend values, year %SIMY%'"
*
*pw/mih090813 -- Temporary solution due to nonsquareness in some cases: fix unused variables to zero
*         => the holdfixed option eliminates them from the model
*         set margins to zero so that the widening bounds processes do not pick those variables
*
*
   v_tradeFlows.fx(rm,rm1,XX_all)  $ (not p_tradeFlows(rm,rm1,XX_all,"CUR")) = 0;
   v_tradeFlows.m(rm,rm1,XX_all)   $ (not p_tradeFlows(rm,rm1,XX_all,"CUR")) = 0;
*
$iftheni.rmtp %RP% == RMTP
   v_tradeFlows.fx(RMTP,RMTP1,XX_all)  $ ((not p_tradeFlows(RMTP,RMTP1,XX_all,"CUR")) $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1)))= 0;
   v_tradeFlows.m(RMTP,RMTP1,XX_all)   $ ((not p_tradeFlows(RMTP,RMTP1,XX_all,"CUR")) $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1)))= 0;
$endif.rmtp


*
* --- remove import prices without actual trade...
*
    p_impPrice(RM,RM1,XX,"CUR") $ [ (not p_tradeFlows(RM,RM1,XX,"CUR")) $ (not sameas(RM,RM1)) ] = 0;
    v_impPrice.m(RM,RM1,XX)   $ (not p_impPrice(RM,RM1,XX,"CUR"))   = 0;
    v_impPrice.fx(RM,RM1,XX)  $ (not p_tradeFlows(RM,RM1,XX,"CUR")) = 0;


*
*  --- eliminate tariffs if no trade
*
   v_tarSpec.fx(%RP%,%RP%1,XX)  $ (not p_tradeFlows(%RP%,%RP%1,XX,"CUR")) = 0;
   v_tarSpec.m(%RP%,%RP%1,XX)   $ (not p_tradeFlows(%RP%,%RP%1,XX,"CUR")) = 0;


*
*  --- eliminate tariffs under global TRQ if no TRQ quantity
*
   v_tarSpec.fx(%RP%,"RW",XX)   $ {not[  (p_trqGlobl(%RP%,XX,"TrqNt","CUR") gt eps)
                                $ ((p_trqGlobl(%RP%,XX,"TsPref","CUR") gt eps) or (p_trqGlobl(%RP%,XX,"TsMFN","CUR") gt eps)) ]}
                               = 0;
   v_tarSpec.m(%RP%,"RW",XX)    $ {not[  (p_trqGlobl(%RP%,XX,"TrqNt","CUR") gt eps)
                                $ ((p_trqGlobl(%RP%,XX,"TsPref","CUR") gt eps) or (p_trqGlobl(%RP%,XX,"TsMFN","CUR") gt eps)) ]}
                               = 0;

*
*  --- same elimination for ad valorem tariffs
*
   v_tarAdVal.fx(%RP%,%RP%1,XX)  $ (not p_tradeFlows(%RP%,%RP%1,XX,"CUR"))  = 0;
   v_tarAdVal.m(%RP%,%RP%1,XX)   $  (not p_tradeFlows(%RP%,%RP%1,XX,"CUR")) = 0;

   v_tarAdVal.fx(%RP%,"RW",XX)  $ {not[  (p_trqGlobl(%RP%,XX,"TrqNt","CUR") gt eps)
                                $ ((p_trqGlobl(%RP%,XX,"TaPref","CUR") gt eps) or (p_trqGlobl(%RP%,XX,"TaMFN","CUR") gt eps)) ]}
                           = 0;
   v_tarAdVal.m(%RP%,"RW",XX)  $ {not[  (p_trqGlobl(%RP%,XX,"TrqNt","CUR") gt eps)
                               $ ((p_trqGlobl(%RP%,XX,"TaPref","CUR") gt eps) or (p_trqGlobl(%RP%,XX,"TaMFN","CUR") gt eps)) ]}
                           = 0;
*
*   --- if the policy blocks are active, the following tariffs should not be defined for RM_not_RMTP
*       (only the aggregate tariffs for RMTP should be nonzero)
*
$iftheni.rmtp %RP% == RMTP
   v_tarSpec.fx(RM,RM1,XX) $ [ rm_not_rmtp(rm) or rm_not_rmtp(rm1) ] = 0;
   v_tarSpec.m (RM,RM1,XX) $ [ rm_not_rmtp(rm) or rm_not_rmtp(rm1) ] = 0;

   v_tarAdval.fx(RM,RM1,XX) $ [ rm_not_rmtp(rm) or rm_not_rmtp(rm1) ] = 0;
   v_tarAdval.m (RM,RM1,XX) $ [ rm_not_rmtp(rm) or rm_not_rmtp(rm1) ] = 0;
$endif.rmtp



*
*   --- no transport cost if no trade
*
   v_transpCost.fx(RM,RM1,XX) $ ((not p_tradeFlows(RM,RM1,XX,"CUR")) $ (NOT sameas(RM,RM1))) = 0;
   v_transpCost.m(RM,RM1,XX)  $ ((not p_tradeFlows(RM,RM1,XX,"CUR")) $ (NOT sameas(RM,RM1))) = 0;


$iftheni.rmtp %RP% == RMTP
   v_transpCost.fx(RMTP,RMTP1,XX) $ ( (not p_tradeFlows(RMTP,RMTP1,XX,"CUR"))
                                      $ (NOT sameas(RMTP,RMTP1)) $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1))) = 0;
   v_transpCost.m(RMTP,RMTP1,XX)  $ ( (not p_tradeFlows(RMTP,RMTP1,XX,"CUR"))
                                      $ (NOT sameas(RMTP,RMTP1)) $ (rmtpAgg(RMTP) or rmtpAgg(RMTP1))) = 0;
$endif.rmtp


*
* --- flexible levy only if minimum border price is defined and trade is observed/projected
*
   v_flexLevy.fx(%RP%,%RP%1,XXX) $ ( (DATA(%RP%,"MinBordP",XXX,"CUR") le eps) or (NOT p_tradeFlows(%RP%,%RP%1,XXX,"CUR"))) =  0;
   v_flexLevy.m(%RP%,%RP%1,XXX)  $ ( (DATA(%RP%,"MinBordP",XXX,"CUR") le eps) or (NOT p_tradeFlows(%RP%,%RP%1,XXX,"CUR")))  = 0;

   v_flexLevyNotCut.fx(%RP%,%RP%1,XXX) $ ( (DATA(%RP%,"MinBordP",XXX,"CUR") le eps) or (NOT p_tradeFlows(%RP%,%RP%1,XXX,"CUR")))  = 0;
   v_flexLevyNotCut.m(%RP%,%RP%1,XXX)  $ ( (DATA(%RP%,"MinBordP",XXX,"CUR") le eps) or (NOT p_tradeFlows(%RP%,%RP%1,XXX,"CUR")))  = 0;


*
*  -- fix the flexible levy to zero under unlimited access at zero specific tariff
*
   v_flexLevy.fx(%RP%,%RP%1,XX) $(p_doubleZero(%RP%,%RP%1,XX,"CUR"))

   = 0;

*
* --- transmission of crude oil price changes to fuel prices
*
$include biofuel\fuel_price_transmission.gms

*
*   --- modified ARmington:
*         - set artifical tiny flows back to zero (were only defined to avoid clearing trade policy variables above)
*         - fix tariff variables (otherwise accidentally activated by the ImpPrice_ equation)
*

p_tradeFlows(rm,rm1,xx,"cur") $ (p_arm2Commit(rm,rm1,xx) $ (p_tradeFlows(rm,rm1,xx,"cur") eq 1.E-3)) = 0;
v_tradeFlows.l(rm,rm1,xx)     $ (p_arm2Commit(rm,rm1,xx) $ (v_tradeFlows.l(rm,rm1,xx) eq 1.E-3))     = 0;



   p_trqBilat(RM,RM1,XX,TrqSet,"CAL") = 0;
   p_trqBilat(RM,RM1,XX,TrqSet,"BAS") = 0;
   p_trqBilat(RM,RM1,XX,TrqSet,"Y") = 0;
   p_trqGlobl(RM,XX,TrqSet,"CAL") = 0;
   p_trqGlobl(RM,XX,TrqSet,"BAS") = 0;
   p_trqGlobl(RM,XX,TrqSet,"Y") = 0;
*
   m_globMarket.optfile    = 2;
   m_globMarket.Solprint   = 1;
   m_globMarket.limrow     = 0;
   m_globMarket.limcol     = 0;

$iftheni.solpri1 %solprintFirstMarketSolve% == on
    m_globMarket.solprint = 1;
    m_globMarket.limrow   = 10000;
    m_globMarket.limcol   = 10000;
$endif.solpri1

$ifi %abortAfterFirstMarketSolve% == on m_globMarket.solprint = 1;

   m_globMarket.holdfixed  = 1;
   m_globMarket.Iterlim    = 0;
   m_globMarket.SOLVELINK  = 2;

   v_dummy.l = 10;

   SOLVE m_globMarket USING DNLP Minimizing v_dummy;
   IF ( (m_globMarket.ModelStat ge 11) $ (m_globMarket.ModelStat LE 13),
     abort "internal error in %system.fn%, %system.incline%, m_globMarket.modelstat = ",m_globMarket.modelstat,data,p_dpCesTrade);
   IF ( (m_globMarket.Solvestat ge 9) $ (m_globMarket.SolveStat LE 13),
     abort "internal error in %system.fn%, %system.incline%, m_globMarket.solvestat = ",m_globMarket.solvestat,data,p_dpCesTrade);


$batinclude 'arm\check_square.gms' %system.fn%

$iftheni.abort1 %abortAfterFirstMarketSolve% == on

*
* --- generate convert output to be accessed with interface
*
  option DNLP=Convert;
  execute "echo gams %scrdir%\convert_market_model.gms > convert.opt";
  execute "echo dict  %scrdir%\convert_market_model.txt >> convert.opt";

  m_globMarket.solprint   = 2;
  m_globMarket.optfile    = 1;
  m_globMarket.limrow     = 0;
  m_globMarket.limcol     = 0;
  SOLVE m_globMarket USING DNLP Minimizing v_dummy;
$batinclude "arm\report_market_infes.gms"
  execute_unload "%scrdir%\convert_market_model.gdx";


  abort "Program stopped after first test solve of market model, convert output generated",data,p_dpCesTrade;
$endif.abort1
*
$iftheni.basel %BASELINE% == on
*
$batinclude 'arm\set_prices.gms' '"BAS"'
*
*   --- define young animal prices from there
*
$include 'capreg\price_yani.gms'
*
*   --- store these prices when in baseline modus
*
$include 'arm\store_uvag.gms'
*
  p_itemsInIters(MS,"UVAG",XX,"TRD")    = DATA(MS,"PPRI",XX,"Y");
  p_itemsInIters(MS,"UVAG",IO,"TRD")    = DATA(MS,"UVAG",IO,"Y");
  p_itemsInIters(MS,"UVAG",ABC,"TRD")   = DATA(MS,ABC,"SUGB","Y");

  p_itemsInIters(MS,"UVAG",IO,"CAL")  = p_uvagStore(MS,IO);
  p_itemsInIters(MS,"UVAG",ABC,"CAL") = p_uvagStore(MS,ABC);
  p_itemsInIters(MS,"UVAG",IO,"CAL")  = p_uvagStore(MS,IO);
  p_itemsInIters(MS,"UVAG",ABC,"CAL") = p_uvagStore(MS,ABC);

  DATA(MS,"UVAG",IOE,"Y")    =  p_itemsInIters(MS,"UVAG",IOE,"TRD");
  DATA(MS,ABC,"SUGB","Y")    =  p_itemsInIters(MS,"UVAG",ABC,"TRD");
  DATA(MS,"UVAG",IOE,"TRD") $ (NOT DATA(MS,"UVAG",IOE,"TRD"))  =  p_itemsInIters(MS,"UVAG",IOE,"TRD");
*
$endif.basel
*
$ifi %BASELINE% == OFF $batinclude 'arm\set_prices.gms' "'TRD'"

$ifi %YANI_M% == OFF v_marketPrice.fx(RM,xxOmYAni) = v_marketPrice.l(RM,xxOMyani);
*
   p_itemsInIters(RMS,"grof",XX,"CAL")      = DATA(RMS,"Prod",XX,"CAL");
   p_itemsInIters(RMS,"grof",XXBioF,"CAL")  = DATA(RMS,"Prod",XXBioF,"CAL");
   p_itemsInIters(RMS,"prod",XX,"CAL")      = DATA(RMS,"Prod",XX,"CAL");
   p_itemsInIters(RMS,"prod",XXBioF,"CAL")  = DATA(RMS,"Prod",XXBioF,"CAL");

   p_itemsInIters(RMSSUP,"fedm",XX,"CAL")        = DATA(RMSSUP,"FEDM",XX,"CAL");
   p_itemsInIters(RMSSUP,"feed",XX,"CAL")        = DATA(RMSSUP,"Feed",XX,"CAL");
   p_itemsInIters(RMSSUP,"Feed",feed_trd,"CAL")  = DATA(RMSSUP,"FEED",feed_trd,"CAL");
   p_itemsInIters(RMSSUP,"PPri",feed_trd,"CAL")  = DATA(RMSSUP,"PPri",feed_trd,"CAL");
   p_itemsInIters(RMS,"feed",XX,"CAL") $ (NOT RMSSUP(RMS))  = DATA(RMS,"FEED",XX,"CAL");
   p_itemsInIters(RMS,"fedm",XX,"CAL") $ (NOT RMSSUP(RMS))  = DATA(RMS,"FEED",XX,"CAL");

*
   p_itemsInIters(RMS,"grof",XX,"TRD")      = DATA(RMS,"Prod",XX,"CAL");
   p_itemsInIters(RMS,"grof",XXBioF,"TRD")  = DATA(RMS,"Prod",XXBioF,"CAL");
   p_itemsInIters(RMS,"prod",XX,"TRD")      = DATA(RMS,"Prod",XX,"CAL");
   p_itemsInIters(RMS,"prod",XXBioF,"TRD")  = DATA(RMS,"Prod",XXBioF,"CAL");

   p_itemsInIters(RMS,"Feed",XX,"TRD")        = DATA(RMS,"FEED",XX,"CAL");
   p_itemsInIters(RMS,"Fedm",XX,"TRD")      = DATA(RMS,"Fedm",XX,"CAL");

   p_itemsInIters(RMSSUP,"FEDM","SWHE","TRD")    = DATA(RMSSUP,"FEDM","SWHE","CAL");
   p_itemsInIters(RMSSUP,"FEDM","DWHE","TRD")    = DATA(RMSSUP,"FEDM","DWHE","CAL");
   p_itemsInIters(RMSSUP,"Feed",feed_trd,"TRD")  = DATA(RMSSUP,"FEED",feed_trd,"TRD");
   p_itemsInIters(RMSSUP,"PPri",feed_trd,"TRD")  = DATA(RMSSUP,"PPri",feed_trd,"TRD");

   p_itemsInIters(RMS,"HCON",XX,"TRD")  = DATA(RMS,"Hcon",XX,"TRD");
   p_itemsInIters(RMS,"PROC",XX,"TRD")  = DATA(RMS,"Proc",XX,"TRD");
   p_itemsInIters(RMS,"BIOF",XX,"TRD")  = DATA(RMS,"Biof",XX,"TRD");
   p_itemsInIters(RM,"EXPT",XX,"TRD")   = v_expQuant.l(RM,XX);
   p_itemsInIters(RM,"IMPT",XX,"TRD")   = v_impQuant.l(RM,XX);
   p_itemsInIters(RM,"DSALES",XX,"TRD") = v_domSales.l(RM,XX);

   p_itemsInIters(RMS,"UVAG",XX,"TRD") $ (NOT RMSSUP(RMS)) = v_prodPrice.l(RMS,XX);
   p_itemsInIters(RMS,"UVAG",XX,"CAL") $ (NOT RMSSUP(RMS)) = DATA(RMS,"PPri",XX,"CAL");

   p_itemsInIters(RM,"PMRK",XX,"TRD")  = DATA(RM,"PMRK",XX,"CAL");
   p_itemsInIters(RM,"ARM1P",XX,"TRD") = DATA(RM,"ARM1P",XX,"CAL");
   p_itemsInIters(RM,"ARM2P",XX,"TRD") = DATA(RM,"ARM2P",XX,"CAL");

   p_itemsInIters(RMS,"PPRI",XX,"TRD")        = DATA(RMS,"PPRI",XX,"CAL");
   p_itemsInIters(RMS,"CPRI",XX,"TRD")        = DATA(RMS,"CPRI",XX,"CAL");
   p_itemsInIters(RMS,"ProcMarg",XX,"TRD")    = DATA(RMS,"ProcMarg",XX,"CAL");
   p_itemsInIters(RMS,"BiofFeedCost",XX,"TRD") = DATA(RMS,"BiofFeedCost",XX,"CAL");


   DATA(RMSSUP,"UVAG",XX,"Y") $ (not XX_SUPPLY(XX)) = v_prodPrice.L(RMSSUP,XX);

*
   p_lstIters(XXGRP) = 0;

*
$include 'arm\unload_market.gms'
$include 'supply\load_supplymodel.gms'
$include 'util\kill_model1.gms'
