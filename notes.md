2019 May 10 (Fri)
================

* univariate simulation test works for logistic regression (only if we have a single beta (i.e. betas cannot have large variations))

* multivariate normal simulation test works perfectly

* multivariate logistic kinda works combining the two ideas above (but fails completely if we have variation in the betas)

2019 Apr 04 (Thu)
=================

Fitting
* Figure out how to fix variances in MCMCglmm (see vignette, try to understand rescaling part)
* Talk to BB about lme4?

Theory
* Write down and extend our idea about how REs on the intercept should also affect β_effect values
* Are linear relationships on the log-odds scale still linear?
 * Can we do the integral (logistic distributed over a Gaussian and then back to logit scale)?
 * What do simulations show? 

2019 Mar 14 (Thu)
=================

Next steps:

* Fit a model and recapture beta (just use serv1)
* Add hhid to the pipeline and fit a mixed model
* Add a household level confounder (unobs_capital)
	* Each household has some unobserved capital
	* simulate at random for each household 
	* merge into individual level data
	* add betas for unobs_capital
* refit the mixed model (one variable at a time)
* Make the long data frame
* fit the combined mixed model

* Make the pipeline a little less ugly. 

----------------------------------------------------------------------

* Steve: Variables for analysis
	
	* Selected
		
		* slumarea
		* interview year
		* age
		* gender
		* ethnicity
		* number of people in hh
		* Poverty line
		* wealthquintile
		* hh expenditure

	* Not sure

		* Hunger scale
		* Should I consider variations in hhid_anon
		* How do we treat years? Factors or continous

----------------------------------------------------------------------

JD feels it is too early to start fitting multivariate models: we don't understand the data structure, and we don't understand multivariate models ☺. We can attack both of these problems separately, and do whatever we want, as long as we don't look at the real response variables. For me, the next two steps are

* Look at the predictor variables and how they're structured and asked which ones to use and whether we can do the analysis at a household level instead of an individual level

* Simulate fake response variables with different simple assumptions, maybe using just a subset of the data set since the data set is large, and see if we can make sense of multivariate model fits. At the beginning, we should probably start with a pretty simple set of predictors.

We have to think about the form of predictor variables. Age should obviously be some sort of spline, and maybe intvwyear as well.

----------------------------------------------------------------------

Gender and age

* Staged cases with "Missing: Impute)" to dropi

Wealth index
* How was **wealthindex** computed? No missing in this but we have missing in essential variables like gender, expend_total_USD...

Ethnicity
* Comnined smaller ethnic groups to others

Number of people living in the structure
* What does 0 mean?
* Do we want to "missing:impute"?

Wash variables
* Sum them up and model as count

Topics
* The influence of demographic and economic factors on WASH indicators among the urban-poor
* Investigate relationships between poor sanitation and risk of adverse pregnancy outcomes.

2019 Feb 26 (Tue)
----------------------------------------------------------------------

* The question is too epidemiological. We approach this from a methedological point of view. 

	* Ideally the two approaches seem to arrive at the same answer but what's our audience?
	* Damazo wants to target epidimiologists and other researchers. Trying to compare what other researchers have been doing and our approach (how wash has been analysed before).
	* Joint model: Does it involve composite score or not.
	* Spikes in 2009

# (1/(1+exp(-x)))*(1/(2pi*d^2)*exp(-(x-m)^2/(2*d^2))) dx
