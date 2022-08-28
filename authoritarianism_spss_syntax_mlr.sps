* Encoding: UTF-8.
**This file contains code for multinomial logistic regressions run to test whether a number of variables can predict membership to authoritarian groups (Class)
**See RScript for latent profile analysis which allowed to determine authoritarian groups

*open dataset

GET
  FILE='C:\Users\olgak\Desktop\masters project\aut_data_clean.sav'.
DATASET NAME aut_data_clean.

*rename varaibles

RENAME VARIABLES (W7_Age_year W7_Gender W7_EU_Ref = age gender EU_ref_vote).
EXECUTE.

*change measurement level -> categorical variables

VARIABLE LEVEL Class gender Education_binary EU_ref_vote (NOMINAL).
EXECUTE.

*label class variable 

VALUE LABELS Class 1 "Non-authoritarians" 2 "Authoritarians about almost anything" 3 "Anti-authoritarians" 4 "Left-wing authoritarians".
EXECUTE.

**Multinimial logistic regressions -> single predictors
*for each analysis "Authoritarians about almost anything" are set as the reference category

*predictor: age

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH age
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*predictor: bully_child

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH bully_child_TOTAL
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*predictor: bully_adult

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH bully_adult_TOATL
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*predictor: bully_others

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH bully_others
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*predictor: child_rearing

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH child_rearing_TOTAL
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*predictor: parent_rearing

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH parent_rearing_TOTAL
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*predictor: gender (only cases who reported being male and female included)

USE ALL.
COMPUTE filter_$=(gender  < 3).
VARIABLE LABELS filter_$ 'gender  < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH gender
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*predictor: EU_ref_vote (only those who voted included in the analysis)

USE ALL.
COMPUTE filter_$=(EU_ref_vote < 3).
VARIABLE LABELS filter_$ 'EU_ref_vote < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH EU_ref_vote
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*include all participants again

FILTER OFF.
USE ALL.
EXECUTE.

*predictor: education

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH Education_binary
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

**Multinomial logistic regressions -> predictors combined 

*Demographics model -> variables included: age, gender, education
*only those reporting being male or female included in the analysis

USE ALL.
COMPUTE filter_$=(gender < 3).
VARIABLE LABELS filter_$ 'gender < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH age gender Education_binary
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*Psycho-political model -> varaibles included: EU_ref_vote, bully_child, bully_adult, bully_others, child_rearing, parent_rearing
*only those who voted in the EU referendum were included in the analysis

USE ALL.
COMPUTE filter_$=(EU_ref_vote < 3).
VARIABLE LABELS filter_$ 'EU_ref_vote < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

NOMREG Class (BASE=FIRST ORDER=ASCENDING) WITH EU_ref_vote bully_child_TOTAL bully_adult_TOATL 
    bully_others child_rearing_TOTAL parent_rearing_TOTAL
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CELLPROB CLASSTABLE FIT PARAMETER SUMMARY LRT CPS STEP MFI.

*include all participants again

FILTER OFF.
USE ALL.
EXECUTE.

*save dataset

SAVE OUTFILE='C:\Users\olgak\Desktop\masters project\aut_data_clean_mlr.sav'
  /COMPRESSED.

