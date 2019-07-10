### Hooks for the editor to set the default target
## https://cygubicko.github.io/wash/
## https://cygubicko.github.io/wash/simulations_writeup.html

current: target
-include target.mk

##################################################################

## Defs

# stuff

Sources += Makefile notes.md

msrepo = https://github.com/dushoff
ms = makestuff
-include $(ms)/os.mk

## Used by Steve to link data to right place
Ignore += local.mk
-include local.mk

# -include $(ms)/perl.def

Ignore += $(ms)
## Sources += $(ms)
Makefile: $(ms)/Makefile
$(ms)/Makefile:
	git clone $(msrepo)/$(ms)

######################################################################

## Loading data and defining some important functions
## ln -fs ~/Dropbox/aphrc/wash/data  ##

Ignore += data docs temp_files 

Sources += $(wildcard *.R *.rmd *.tex *.sage)

# Define all important R-functions in one file
globalFunctions.Rout: globalFunctions.R

# Read data
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

## Fit stan model using rstanarm
rstanarmModel.Rout: rstanarmModel.R

# Binary response
rstanarmModelbin.Rout: rstanarmModelbin.R

## Simulations writeup
additional_summaries.Rout: additional_summaries.R
simulations_writeup.html: simulations_writeup.rmd

## https://cygubicko.github.io/wash/simulations_writeup.html
## simulations_writeup.html.pages: simulations_writeup.rmd

## Bottomline test with multivariate normal response
mvnSim.Rout: mvnSim.R

######################################################################

Ignore += *.rda
Ignore += *.Rhistory
Ignore += *.pdf *.html *.csv *.vrb *.png *.Rexit
sub += hh autopsy data docs temp_files 
Ignore += $(sub)

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog *.rdeps *.rda .*.rdeps .*.rda *.vrb *.toc *.out *.nav *.snm *.log *.aux

######################################################################

### Makestuff

-include $(ms)/texdeps.mk
-include $(ms)/pandoc.mk
-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/stepR.mk
