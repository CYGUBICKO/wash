### Hooks for the editor to set the default target
## https://cygubicko.github.io/wash/
## https://cygubicko.github.io/wash/simulations_writeup.html

current: target
-include target.mk

##################################################################

ms = makestuff

Sources += Makefile notes.md rmd.mk
Sources += $(wildcard *.md)

## Used by Steve to link data to right place
Ignore += local.mk
-include local.mk
## ln -fs ~/Dropbox/aphrc/wash/data  ##

######################################################################

Ignore += data docs temp_files 

Sources += $(wildcard *.R *.rmd *.tex *.sage)

# Define all important R-functions in one file
globalFunctions.Rout: globalFunctions.R

# Read data
## loadData.rda: loadData.R
loadData.Rout: data/NUHDSS_Wash.dta loadData.R

# Some cleaning
cleaning.Rout: cleaning.R

# Drop cases
complete.Rout: complete.R
# wash_codebook.csv: complete.Rout ;

# Select only variables for analysis
analysisdata.Rout: analysisdata.R

# Logistic PCA to create a single variable for WASH variables
logisticpca.Rout: logisticpca.R

# Descriptives plots
descriptives.Rout: descriptives.R

# households.Rout: households.R

# Simulating Response variable
simulateResponse.Rout: simulateResponse.R
simulatePoisson.Rout: simulatePoisson.R

## Just trying to include Cov in the simulations
simulateMvariate.Rout: simulateMvariate.R

## Fit rstanarm model to simulateMvariate.R data
rstanarmModelbinwash.Rout: rstanarmModelbinwash.R
simulations_analysis_writeup_wash.html: simulations_analysis_writeup_wash.rmd

rstanarmModelbin.Rout: rstanarmModelbin.R
# Fit simple GLM to recapture beta 
simpleGlm.Rout: simpleGlm.R

# Fit a more complex glmer based on long-format data
complexGlmer.Rout: complexGlmer.R

# Fit poisson GMLER
poissonGlmer.Rout: poissonGlmer.R

# Try MCMCglmm
binaryMcmcglmm.Rout: binaryMcmcglmm.R
multiMcmcglmm.Rout: multiMcmcglmm.R

# Brms models
brmsModel.Rout: brmsModel.R

# Some experimenting
oneGlmer.Rout: oneGlmer.R
oneMcmcglmm.Rout: oneMcmcglmm.R

# Extract summary stats of complexGlmer
glmerStats.Rout: glmerStats.R
mcmcglmmStats.Rout: mcmcglmmStats.R
brmsStats.Rout: brmsStats.R

# Summary plots
glmerSummary.Rout: glmerSummary.R
multimcmcglmmSummary.Rout: multimcmcglmmSummary.R
brmsSummary.Rout: brmsSummary.R

# Diagnostics
glmerDiagnostics.Rout: glmerDiagnostics.R
multimcmcDiagnostics.Rout: multimcmcDiagnostics.R
brmsDiagnostics.Rout: brmsDiagnostics.R

# Compare mcmc and glmer
compareModels.Rout: compareModels.R

## Report
wash_report.html: wash_report.rmd

## Presentation
presentation.Rout: presentation.R
wash_presentation.pdf: wash_presentation.tex

## Theory 
binary_random.Rout: binary_random.R 
beta_variance_report.html: beta_variance_report.rmd

## Integrating f(theta)*l(theta)
## Does not integrate!!
Ignore += *.sage.py
Ignore += integration.out
integration.out: integration.sage
	sage $< |tee > $@

## Back to basics: Bivariate, Multivariate normal response

# Simulations
simulatemvn.Rout: simulatemvn.R

# Models

## Multivariate normal
bivariateModel.Rout: bivariateModel.R

## Multivariate binomial
bivariateBinary.Rout: bivariateBinary.R

# Plot the coeffs
bivariateDiagnostics.Rout: bivariateDiagnostics.R

# Joint model
mvnjointModel.Rout: mvnjointModel.R
mvnjointDiagnostics.Rout: mvnjointDiagnostics.R

binjointModel.Rout: binjointModel.R
binjointDiagnostics.Rout: binjointDiagnostics.R

## Simulate Hierarchical Multivariate data
simulateHierarchicalmvn.Rout: simulateHierarchicalmvn.R

## Simulate Hierarchical Multivariate data with AR1 process
simulateHierarchicalmvnAR1.Rout: simulateHierarchicalmvnAR1.R
simulateHierarchicalmvnAR1_simple.Rout: simulateHierarchicalmvnAR1_simple.R

## Fit stan model using rstanarm
rstanarmModel.Rout: rstanarmModel.R

# Binary response
rstanarmModelbin.Rout: rstanarmModelbin.R
rstanarmModelbinDiagnostics.Rout: rstanarmModelbinDiagnostics.R

# Fit brms model
brmsModelbin.Rout: brmsModelbin.R
brmsModelbinAR1.Rout: brmsModelbinAR1.R
temp_summaries.Rout: temp_summaries.R

# Fit lme4 (glmer) model
glmerModelbin.Rout: glmerModelbin.R

## Simulations writeup
additional_summaries.Rout: additional_summaries.R
simulations_writeup.html: simulations_writeup.rmd

## https://cygubicko.github.io/wash/simulations_writeup.html
simulations_writeup.html.pages: simulations_writeup.rmd

## Simulations analyis

summary_plot_data.Rout: summary_plot_data.R 
extract_summaries.Rout: extract_summaries.R
simulations_analysis_writeup.html: simulations_analysis_writeup.rmd
simulations_plots_temp.Rout: simulations_plots_temp.R

## Extract posterior summaries from all the models for comparison
extract_posterior_summaries.Rout: extract_posterior_summaries.R

## Data lunch presentation
## https://cygubicko.github.io/wash/datalunch_wash_presentation.pdf
datalunch_wash_presentation.pdf: datalunch_wash_presentation.rmd
simulations_analysis_output.html: simulations_analysis_output.rmd

## AR1 based covariates simulations
simSwitch.Rout: simSwitch.R
switchModel.Rout: switchModel.R
switchSummary.Rout: switchSummary.R

### Comparing mech and stat switch simulation
simSwitch_compare.Rout: simSwitch_compare.R
switchCompare_summary.Rout: switchCompare_summary.R

switchInspect.Rout: switchInspect.R

### Exploring effects plots by JD
switchSingleModel.Rout: switchSingleModel.R
eplotsFuns.Rout: eplotsFuns.R
ordFuns.Rout: ordFuns.R

### Tidy estimates
switchTidy.Rout: switchTidy.R

### Iso effect plots - JD
switchEffect_plots.Rout: switchEffect_plots.R

### Effects plot effects package
switchPredEffects.Rout: switchPredEffects.R
switchPredInspect.Rout: switchPredInspect.R

## Complete simulation report
simulations_report.html: simulations_report.rmd

## Bottomline test with multivariate normal response
mvnSim.Rout: mvnSim.R

### Simple ar simulation
ar.Rout: ar.R

### Ben Bolker suggestions
corr_effs.html: corr_effs.rmd


######################################################################

## WASH Analysis of real data
washdataInspect.Rout: washdataInspect.R
washdataInspect_plots.Rout: washdataInspect_plots.R

householdSurveys.Rout: householdSurveys.R
householdSurveys_plots.Rout: householdSurveys_plots.R

## Restructured data for hhid-year-services
longDFunc.Rout: longDFunc.R
washModeldata.Rout: washModeldata.R

## Fitting models

## glme

### Year 1 model
#### Scaled variable (hhsize and year)
washModelfit_y1glmS.Rout: washModelfit_y1glmS.R

#### Unscaled
washModelfit_y1glmU.Rout: washModelfit_y1glmU.R

## glmer

### Previous year model
#### Scaled variable (hhsize and year)
washModelfit_pglmerS.Rout: washModelfit_pglmerS.R

#### Unscaled year variable
washModelfit_pglmerU.Rout: washModelfit_pglmerU.R

### All fit: to troubleshoot optimizer in lme4
washModel_allFit.Rout: washModel_allFit.R

### Interaction model
washModelfit_inter_pglmerS.Rout: washModelfit_inter_pglmerS.R

## BRMS model

### Scaled
washModelfit_brmsS.Rout: washModelfit_brmsS.R

## Tidy model estimates
washTidyestimates.Rout: washTidyestimates.R

## Effect size plots
washEffectsize_plots.Rout: washEffectsize_plots.R

### Wash predictor effects
washPredEffects.Rout: washPredEffects.R
washPredEffects_plots.Rout: washPredEffects_plots.R

### Analysis report
washdataAnalysis_report.html: washdataAnalysis_report.rmd

### Calculate variable level p-values
washModelPvalues.Rout: washModelPvalues.R

washModel_isoplots.Rout: washModel_isoplots.R

missing.Rout: missing.R

######################################################################

## Run and push
run_push:
	make simulations_analysis_writeup.html.pages
	make datalunch_wash_presentation.pdf.pages
	make simulations_analysis_output.html.pages
	cd pages && git push
	make sync
#	cd ~/grive && grive

## Why does Steve need so many? Should some be in stepR?
Ignore += *.rda
Ignore += *.Rhistory
Ignore += *.pdf *.html *.csv *.vrb *.png *.Rexit

sub += hh autopsy data docs temp_files 
Ignore += $(sub)

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog *.rdeps *.rda .*.rdeps .*.rda *.vrb *.toc *.out *.nav *.snm *.log *.aux

######################################################################

### Makestuff

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls $@

include rmd.mk
-include makestuff/os.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
-include makestuff/texdeps.mk
-include makestuff/pandoc.mk
## makestuff/stepR.md
-include makestuff/stepR.mk
-include makestuff/git.mk
