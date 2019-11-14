2019 Nov 14 (Thu)
=================

Simulation: 
- Simulate two AR1 process covariates
	- Unmeasured and measured
- Two services for the start
- 1000 households for 10 years of data 
	- We can simulate many years and `throw` away some at the beginning to take of the stationarity  

- The household services for the time period $t` = t + 1$ are generated using switch probabilities: These are either defined in a:

	- mechanistic way
		- If the value of service at current time is $1$ then the switch probability to the next year is 
			- $\beta_{lose}$ minus the value of linear predictor $L$ at the current time
		- Otherwise, if the HH has no service at the current time, then the switch probability is
			- $\beta_{gain}$ plus the value of linear predictor $L$ at the current time
	
	- simplified (or statistical) way
		- The probability of having service at time $t + 1$ is given by 
			- $y_{t+1} = L_{t+1} + \beta_{gain} + \beta_{add}y_t$

			- Where $\beta_{add} = -(\beta_{gain} + \beta_lose)$

	- We established that the two simulation approaches give the same results.


2019 Oct 23 (Wed)
=================

Simulating: Steve has made separate AR(1) series for each service within each household
* In future, maybe incorporate correlations with a household-level AR(1) series to be added to those

Next steps:
* fit new sims with old fitting machine
* fit new sims with new fitting machine

At some point the goal should be to see from simulations:
* when does the fancier method provide more accuracy or clarity 

2019 Oct 02 (Wed)
=================

Steve:
* Trying to figure out these examples
	* https://github.com/bbolker/mixedmodels-misc/blob/master/notes/corr_braindump.rmd
	* http://www.flutterbys.com.au/stats/tut/tut8.3a.html


Committee
* Steve will finalize the GD and we will send to committee in advance of meeting
* Steve will send CES comprehensive material; JD will send to committee with comments
* Steve may create an overarching repo and share with JD. This can contain anything about his graduate career and thesis that is not project-specific

WASH
* Steve to email BB, cc JD
* Take a look at some resources
	* https://mc-stan.org/docs/2_18/stan-users-guide/autoregressive-section.html
	* https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html
	* https://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model 
	* Maybe see ARIMA also


2019 Aug 29 (Thu)
=================

Make sure axis labels are readable (particularly if there is a pattern there)

For S3.5–3.6, compare random effects with secret (but knowable) simulation values. Since the link may be complicated, start with scatter plots:
* simulation value on the x axis and model inference on the y axis

Make a plot for 3.7 as well.

Add a few more words, in general, for these things. Especially if you are submitting a report. Your goal is that a busy, old, confused person can quickly figure out what your main points are before the meeting ☺

2019 Jul 18 (Thu)
=================

Figure out how to add more correlations to the simulation:
* Things should be correlated within households and across time
* Each household could have a mean wealth and a temporal autocorrelation
* Each household service intercept could also have a similar structure
* If we know that the household has a service this year, then even if we control for wealth and other variables, it should be more likely to have the same service next year

Figure out how to incorporate a household-level RE.
* This should be possible if we have more than one year per household, even if we also have a temporal random effect (10×100 > 10+100)
* It may just work in rstanarm, but I'm not sure

Figure out how to interpret and validate rstanarm (or other model) outputs

Experiment with changing simulation parameters and see how the changes are reflected in model fits

Are the correlations also good, or just the sd?

I am going to hang up, because I don't hear you ☹

2019 Jul 10 (Wed)
=================

Results of data analysis
* Services are reported one per household per year
* Wealth is always reported the same once we group by household and year
	* but it may be reported by more than one interviewee
* We don't really think that income is reported, it's wealth
* We are thinking that our "observation" level will be households (not individuals), with one observation per year per household

Simulation plan
* Need to add correlations between ε within a household

Analysis plan
* Steve to suggest

2019 Jul 04 (Thu)
=================

Steve wants to do hierarchical simulations, so we need to decide what simulations to do:
* is it realistic to aggregate within a household by year?
* is it realistic not to?

Do we have different services for different people in the household? How many people per household have different incomes? Is it easy to figure out an aggregate measure of househould income or household wealth? How do we adjust that for the number of people per household?

We can:
* Look at the data
* Make a clear simulation plan (in plain English)
* Do a simulation
* Adapt current rstanarm (and test to see if we can see the hierarchy)

Steve can email JD at the end of each step

2019 Jun 20 (Thu)
=================

There is a fair amount of confusion about the current plots

* bivariateDiagnostics.Rout.pdf just does univariate (!) normal stuff. Seems to work fine
* mvnjointDiagnostics.Rout.pdf does multivariate normal and also works fine
* mvnjointDiagnostics_grp.Rout.pdf does not work yet; possibly because it assumes groups but groups have no replication
* binjointDiagnostics.Rout.pdf FAILS to see correlations when we look at binary variables based on the mvnjoint normal variables. This is the current problem point

To do
* Go back to working mvn and add grouping variables _with_ multiple observations per group
* Try other approaches for fitting binary data
* Maybe: try other approaches for _making_ binary data. This is not really satisfactory, because we feel that the current binary data should be fittable.

2019 May 23 (Thu)
================

## Status

## Goals
Make a strong, simple example where the technique clearly works
* Compare the multivariate approach to a univariate approach
* Ideally, we have a scenario where multivariate is either more powerful or more valid
* Maybe make some checkplots

Write up our understanding of the beta/variance issue
* Refer to the beta/lines picture binary_random.R 

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
