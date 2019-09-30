### Hooks for the editor to set the default target
## https://cygubicko.github.io/wash/
## https://cygubicko.github.io/wash/simulations_writeup.html

current: target
-include target.mk

##################################################################

## Kind of deprecated now ☺
ms = makestuff

Sources += Makefile notes.md rmd.mk

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

## Fit stan model using rstanarm
rstanarmModel.Rout: rstanarmModel.R

# Binary response
rstanarmModelbin.Rout: rstanarmModelbin.R
rstanarmModelbinDiagnostics.Rout: rstanarmModelbinDiagnostics.R

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

## Data lunch presentation
## https://cygubicko.github.io/wash/datalunch_wash_presentation.pdf
datalunch_wash_presentation.pdf: datalunch_wash_presentation.rmd
simulations_analysis_output.html: simulations_analysis_output.rmd

## Run and push
run_push:
	make simulations_analysis_writeup.html.pages
	make datalunch_wash_presentation.pdf.pages
	make simulations_analysis_output.html.pages
	cd pages && git push
	make sync
#	cd ~/grive && grive

## Bottomline test with multivariate normal response
mvnSim.Rout: mvnSim.R

######################################################################

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
