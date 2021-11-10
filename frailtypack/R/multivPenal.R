#' Fit a multivariate frailty model for two types of recurrent events and a
#' terminal event.
#'
#' @description Fit a joint frailty model for a single recurrent event
#' and two terminal events using penalized splines for the hazard
#' function or a parametric weibull model.
#'
#' @aliases multivPenal transfo.table multivPenal for multivariate frailty
#' model
#'
#' @usage
#' multivPenal(formula, formula.Event2, formula.terminalEvent, formula.terminalEvent2,
#' data, data.Event2, initialize = TRUE, recurrentAG = FALSE, n.knots, kappa, maxit = 350,
#' hazard = "Weibull", nb.int, print.times = TRUE, GHpoints = 32, save.progress = F,
#' init.hazard = c(1,0.5,1,1,1,1), init.Theta = 0.5,
#' init.Alpha1 = 0.1, init.Alpha2 = -0.1, init.B = c(0,0,0))
#'
#' @param formula a formula object, with the response for the first recurrent
#' event on the left of a \eqn{\sim} operator, and the terms on the right. The
#' response must be in the format Surv(t0, t1, recurrentevent),
#' where t0 is the start time for an at-risk period for the recurrent event,
#' t1 is the end time for an at-risk period for the recurrent event, and
#' recurrentevent is a numeric indicator for whether an event was observed (1)
#' or was censored(2).
#'
#' @param formula.Event2 a formula object, with the response for the second
#' recurrent event on the left of a \eqn{\sim} operator, and the terms on the
#' right.
#'
#' @param formula.terminalEvent, a formula object, empty on the left of a \eqn{\sim} operator,
#' and the terms on the right.
#'
#' @param formula.terminalEvent2, a formula object, empty on the left of a \eqn{\sim} operator,
#' and the terms on the right.
#'
#' @param data a 'data.frame' with the variables used in 'formula',
#' 'formula.Event2', 'formula.terminalEvent', and 'formula.terminalEvent2'.
#'
#' @param initialize Logical value to internally initialize regression coefficients and
#' baseline hazard functions parameters using simpler models from frailtypack.
#' When the estimation is semi-parametric
#' with splines, this initialization produces also values for smoothing
#' parameters (by cross validation). When initialization is requested, the
#' program first fit shared frailty models for recurrent
#' events and Cox proportional hazards models for the terminal events.
#' Default is TRUE.
#' When FALSE, parameters are initialized via the arguments
#' init.hazard, init.Theta, init.Alpha1, init.Alpha2, init.B.
#'
#' @param recurrentAG Logical value. Is Andersen-Gill model fitted? If so
#' indicates that recurrent event times with the counting process approach of
#' Andersen and Gill is used. This formulation can be used for dealing with
#' time-dependent covariates. The default is FALSE.
#'
#' @param jointGeneral Logical. Should the model have separate (correlated)
#' frailty random effects that connect each terminal event to the recurrent
#' event? Default is FALSE.
#'
#' @param n.knots integer vector of length 3 giving
#' the number of knots to use in each spline hazard function.
#' First is for the recurrent of type 1, second is
#' for the terminal event 1, and third is for the terminal event 2.
#' Value required in the penalized likelihood estimation. It
#' corresponds to the (n.knots+2) splines functions for the approximation of
#' the hazard or the survival functions. Number of knots must be between 4 and
#' 20. (See Note)
#'
#' @param kappa vector of length 3 (for the three outcomes) for positive
#' smoothing parameters in the penalized likelihood estimation. First is for
#' the recurrent of type 1, second is for the terminal event 1, and third is for the terminal event 2.
#' The coefficient kappa of the
#' integral of the squared second derivative of hazard function in the fit
#' (penalized log likelihood). Initial values for the kappas can be obtained
#' with the option "initialize=TRUE". We advise the user to identify several
#' possible tuning parameters, note their defaults and look at the sensitivity
#' of the results to varying them. Value required. (See Note)
#'
#' @param maxit maximum number of iterations for the Marquardt algorithm.
#' Default is 350.
#'
#' @param hazard Type of hazard functions: "Splines" for semi-parametric hazard
#' functions with the penalized likelihood estimation and "Weibull"
#' for parametric Weibull function. Piecewise constant hazard functions are not
#' available for this function.
#'
#' @param nb.int Depreciated. An integer vector of length 3.
#'
#' @param print.times a logical parameter to print iteration process. Default
#' is TRUE.
#'
#' @param GHpoints Integer. Number of nodes for Gauss-Hermite integration
#' to marginalize random effects/frailties. Default is 32.
#'
#' @param save.progress Logical. Should the trajectory of parameter values from the optimizer be saved?
#'
#' @param init.hazard Numeric. = c(1,0.5,1,1,1,1),
#'
#' @param init.Theta Numeric, Initialization value for the variance of the
#' normally-distributed random effects.
#'
#' @param init.Alpha1 Numeric. Initialization value for the parameter alpha that
#' links the hazard function of the recurrent event to the first terminal event.
#'
#' @param init.Alpha2 Numeric. Initialization value for the parameter alpha that
#' links the hazard function of the recurrent event to the second terminal event.
#'
#' @param init.B Numeric vector, same length and order as the covariate vectors
#' for each the recurrent event, first terminal event, and second terminal event.
#' Initialization values for the independent variable coefficients.
#'
#' @details{
#' \if{html}{ Right-censored data are allowed.
#' Left-truncated data and stratified analysis are not possible.
#' Multivariate joint frailty models are applicable in mainly two settings.
#'
#' The multivariate frailty model for two types of recurrent events with a
#' terminal event is (in the calendar or time-to-event timescale):
#'
#' {\figure{multivmodel1.png}{options: width="100\%"}}
#'
#' where \eqn{r}\out{<sub>0</sub>}\out{<sup>l</sup>}(t), (l\out{&isin;}{1,2}) and \eqn{r}\out{<sub>0</sub>}(t) are
#' respectively the recurrent and terminal event baseline hazard functions, and
#' \eqn{\beta}\out{<sub>1</sub>},\eqn{\beta}\out{<sub>2</sub>},\eqn{\beta}\out{<sub>3</sub>} the regression coefficient vectors associated
#' with \eqn{Z}\out{<sub>i</sub>}(t) the covariate vector. The covariates could be different
#' for the different event hazard functions and may be time-dependent. We
#' consider that death stops new occurrences of recurrent events of any type,
#' hence given \eqn{t>D}, \eqn{dN}\out{<sup>R(l)*</sup>}(t), (l\out{&isin;}{1,2}) takes the value 0.
#' Thus, the terminal and the two recurrent event processes are not independent
#' or even conditional upon frailties and covariates. We consider the hazard
#' functions of recurrent events among individuals still alive.  % The three
#' components in the above multivariate frailty model are linked together by
#' two Gaussian and correlated random effects \eqn{u}\out{<sub>i</sub>},\eqn{v}\out{<sub>i</sub>}: %
#' (\eqn{u}\out{<sub>i</sub>},\eqn{v}\out{<sub>i</sub>})\out{<sup>T</sup>} \out{<span>&#126;</span>} \bold{\eqn{N}}(0,\eqn{\Sigma}\out{<sub>uv</sub>}), with
#'
#' {\figure{multivmodel2.png}{options: width="100\%"}}
#'
#' Dependencies between these three types of event are taken into account by
#' two correlated random effects and parameters \eqn{\theta}\out{<sub>1</sub>},\eqn{\theta}\out{<sub>2</sub>} the
#' variance of the random effects and \eqn{\alpha}\out{<sub>1</sub>},\eqn{\alpha}\out{<sub>2</sub>} the coefficients
#' for these random effects into the terminal event part. If \eqn{\alpha}\out{<sub>1</sub>} and
#' \eqn{\theta}\out{<sub>1</sub>} are both significantly different from 0, then the recurrent
#' events of type 1 and death are significantly associated (the sign of the
#' association is the sign of \eqn{\alpha}\out{<sub>1</sub>}). If \eqn{\alpha}\out{<sub>2</sub>} and
#' \eqn{\theta}\out{<sub>2</sub>} are both significantly different from 0, then the recurrent
#' events of type 2 and death are significantly associated (the sign of the
#' association is the sign of \eqn{\alpha}\out{<sub>2</sub>}). If \eqn{\rho}, the correlation
#' between the two random effects, is significantly different from 0, then the
#' recurrent events of type 1 and the recurrent events of type 2 are
#' significantly associated (the sign of the association is the sign of
#' \eqn{\rho}).
#' }
#'
#' \if{latex}{Fit a multivariate frailty model for two types of recurrent events with a
#' terminal event using a penalized likelihood estimation on the hazard
#' function or a parametric estimation. Right-censored data are allowed.
#' Left-truncated data and stratified analysis are not possible. Multivariate
#' frailty models allow studying, with a joint model, three survival dependent
#' processes for two types of recurrent events and a terminal event.
#' Multivariate joint frailty models are applicable in mainly two settings.
#' First, when focus is on the terminal event and we wish to account for the
#' effect of previous endogenous recurrent event. Second, when focus is on a
#' recurrent event and we wish to correct for informative censoring.
#'
#' The multivariate frailty model for two types of recurrent events with a
#' terminal event is (in the calendar or time-to-event timescale):
#'
#' \deqn{\left\{ \begin{array}{lll} r_{i}^{(1)}(t|u_i,v_i) &=
#' r_0^{(1)}(t)\exp({{\beta_1^{'}}}Z_{i}(t)+u_i) &\quad \mbox{(rec. of type
#' 1)}\\ r_{i}^{(2)}(t|u_i,v_i) &=
#' r_0^{(2)}(t)\exp({{\beta_2^{'}}}Z_{i}(t)+v_i) &\quad \mbox{(rec. of type
#' 2)}\\ \lambda_i(t|u_i,v_i) &=
#' \lambda_0(t)\exp({{\beta_3^{'}}}Z_{i}(t)+\alpha_1u_i+\alpha_2v_i) &\quad
#' \mbox{(death)}\\ \end{array} \right. }
#'
#' where \eqn{r_0^{(l)}(t)}, \eqn{l\in{1,2}} and \eqn{\lambda_0(t)} are
#' respectively the recurrent and terminal event baseline hazard functions, and
#' \eqn{\beta_1,\beta_2,\beta_3} the regression coefficient vectors associated
#' with \eqn{Z_{i}(t)} the covariate vector. The covariates could be different
#' for the different event hazard functions and may be time-dependent. We
#' consider that death stops new occurrences of recurrent events of any type,
#' hence given \eqn{t>D}, \eqn{dN^{R(l)*}(t), l\in{1,2}} takes the value 0.
#' Thus, the terminal and the two recurrent event processes are not independent
#' or even conditional upon frailties and covariates. We consider the hazard
#' functions of recurrent events among individuals still alive.  % The three
#' components in the above multivariate frailty model are linked together by
#' two Gaussian and correlated random effects \eqn{u_i,v_i}: %
#'
#' \eqn{(u_i,v_i)^{T}\sim\mathcal{N}\left({{0}},\Sigma_{uv}\right)}, with
#' \deqn{\Sigma_{uv}=\left(\begin{array}{cc} \theta_1 &
#' \rho\sqrt{\theta_1\theta_2} \\ \rho\sqrt{\theta_1\theta_2}&\theta_2
#' \end{array}\right)}
#'
#' Dependencies between these three types of event are taken into account by
#' two correlated random effects and parameters \eqn{\theta_1,\theta_2} the
#' variance of the random effects and \eqn{\alpha_1,\alpha_2} the coefficients
#' for these random effects into the terminal event part. If \eqn{\alpha_1} and
#' \eqn{\theta_1} are both significantly different from 0, then the recurrent
#' events of type 1 and death are significantly associated (the sign of the
#' association is the sign of \eqn{\alpha_1}). If \eqn{\alpha_2} and
#' \eqn{\theta_2} are both significantly different from 0, then the recurrent
#' events of type 2 and death are significantly associated (the sign of the
#' association is the sign of \eqn{\alpha_2}). If \eqn{\rho}, the correlation
#' between the two random effects, is significantly different from 0, then the
#' recurrent events of type 1 and the recurrent events of type 2 are
#' significantly associated (the sign of the association is the sign of
#' \eqn{\rho}).
#' }
#'
#' @return Parameters estimates of a multivariate joint frailty model, more
#' generally a 'multivPenal' object. Methods defined for 'multivPenal' objects
#' are provided for print, plot and summary. The following components are
#' included in a 'multivPenal' object for multivariate Joint frailty models.
#'
#' \item{b}{sequence of the corresponding estimation of the splines
#' coefficients, the random effects variances, the coefficients of the
#' frailties and the regression coefficients.}
#'
#' \item{call}{The code used for
#' fitting the model.}
#'
#' \item{n}{the number of observations used in the fit.}
#'
#' \item{groups}{the number of subjects used in the fit.}
#'
#' \item{n.events}{the number of recurrent events of type 1 observed in the fit.}
#'
#' \item{n.events2}{the number of the recurrent events of type 2 observed in
#' the fit.}
#'
#' \item{n.deaths}{the number of deaths observed in the fit.}
#'
#' \item{loglikPenal}{the complete marginal penalized log-likelihood in the
#' semi-parametric case.}
#'
#' \item{loglik}{the marginal log-likelihood in the
#' parametric case.}
#'
#' \item{LCV}{the approximated likelihood cross-validation
#' criterion in the semi parametric case (with H minus the converged Hessian
#' matrix, and l(.) the full
#' log-likelihood.\deqn{LCV=\frac{1}{n}(trace(H^{-1}_{pl}H) - l(.))})}
#'
#' \item{AIC}{the Akaike information Criterion for the parametric
#' case.\deqn{AIC=\frac{1}{n}(np - l(.))}}
#'
#' \item{theta1}{variance of the
#' frailty parameter for recurrences of type 1 \eqn{(\bold{Var}(u_i))}}
#'
#' \item{theta2}{variance of the frailty parameter for recurrences of type 2
#' \eqn{(\bold{Var}(v_i))}}
#'
#' \item{alpha1}{the coefficient associated with the
#' frailty parameter \eqn{u_i} in the terminal hazard function.}
#'
#' \item{alpha2}{the coefficient associated with the frailty parameter
#' \eqn{v_i} in the terminal hazard function.}
#'
#' \item{rho}{the correlation
#' coefficient between \eqn{u_i} and \eqn{v_i}}
#'
#' \item{npar}{number of
#' parameters.}
#'
#' \item{coef}{the regression coefficients.}
#'
#' \item{nvar}{A vector
#' with the number of covariates of each type of hazard function as
#' components.}
#'
#' \item{varH}{the variance matrix of all parameters before
#' positivity constraint transformation (theta, the regression coefficients and
#' the spline coefficients). Then, the delta method is needed to obtain the
#' estimated variance parameters.}
#'
#' \item{varHIH}{the robust estimation of the
#' variance matrix of all parameters (theta, the regression coefficients and
#' the spline coefficients).}
#'
#' \item{formula}{the formula part of the code used
#' for the model for the recurrent event.}
#'
#' \item{formula.Event2}{the formula
#' part of the code used for the model for the second recurrent event.}
#'
#' \item{formula.terminalEvent}{the formula part of the code used for the model
#' for the terminal event.}
#'
#' \item{x1}{vector of times for hazard functions of
#' the recurrent events of type 1 are estimated. By default
#' seq(0,max(time),length=99), where time is the vector of survival times.}
#'
#' \item{lam1}{matrix of hazard estimates and confidence bands for recurrent
#' events of type 1.}
#'
#' \item{xSu1}{vector of times for the survival function of
#' the recurrent event of type 1.}
#'
#' \item{surv1}{matrix of baseline survival
#' estimates and confidence bands for recurrent events of type 1.}
#'
#' \item{x2}{vector of times for the recurrent event of type 2 (see x1 value).}
#'
#' \item{lam2}{the same value as lam1 for the recurrent event of type 2.}
#'
#' \item{xSu2}{vector of times for the survival function of the recurrent event
#' of type 2}
#'
#' \item{surv2}{the same value as surv1 for the recurrent event of
#' type 2.}
#'
#' \item{xEnd}{vector of times for the terminal event (see x1 value).}
#'
#' \item{lamEnd}{the same value as lam1 for the terminal event.}
#'
#' \item{xSuEnd}{vector of times for the survival function of the terminal
#' event}
#'
#' \item{survEnd}{the same value as surv1 for the terminal event.}
#'
#' \item{median1}{The value of the median survival and its confidence bands for the recurrent event of type 1.}
#'
#' \item{median2}{The value of the median survival and its confidence bands for the recurrent event of type 2.}
#'
#' \item{medianEnd}{The value of the median survival and its confidence bands for the terminal event.}
#'
#' \item{type.of.Piecewise}{Type of Piecewise hazard functions (1:"percentile",
#' 0:"equidistant").}
#'
#' \item{n.iter}{number of iterations needed to converge.}
#'
#' \item{type.of.hazard}{Type of hazard functions (0:"Splines", "1:Piecewise",
#' "2:Weibull").}
#'
#' \item{n.knots}{a vector with number of knots for estimating
#' the baseline functions.}
#'
#' \item{kappa}{a vector with the smoothing parameters
#' in the penalized likelihood estimation corresponding to each baseline
#' function as components.}
#'
#' \item{n.knots.temp}{initial value for the number of knots.}
#'
#' \item{zi}{splines knots.}
#'
#' \item{time}{knots for Piecewise hazard
#' function for the recurrent event of type 1.}
#'
#' \item{timedc}{knots for
#' Piecewise hazard function for the terminal event.}
#'
#' \item{time2}{knots for
#' Piecewise hazard function for the recurrent event of type 2.}
#'
#' \item{noVar}{indicator vector for recurrent, death and recurrent 2
#' explanatory variables.}
#'
#' \item{nvarRec}{number of the recurrent of type 1
#' explanatory variables.}
#'
#' \item{nvarEnd}{number of death explanatory
#' variables.}
#'
#' \item{nvarRec2}{number of the recurrent of type 2 explanatory
#' variables.}
#'
#' \item{nbintervR}{Number of intervals (between 1 and 20) for the
#' the recurrent of type 1 parametric hazard functions ("Piecewise-per",
#' "Piecewise-equi").}
#'
#' \item{nbintervDC}{Number of intervals (between 1 and 20)
#' for the death parametric hazard functions ("Piecewise-per",
#' "Piecewise-equi").}
#'
#' \item{nbintervR2}{Number of intervals (between 1 and 20)
#' for the the recurrent of type 2 parametric hazard functions
#' ("Piecewise-per", "Piecewise-equi").}
#'
#' \item{istop}{Vector of the convergence criteria.}
#'
#' \item{shape.weib}{shape parameters for the Weibull hazard
#' function.}
#'
#' \item{scale.weib}{scale parameters for the Weibull hazard
#' function.}
#'
#' \item{martingale.res}{martingale residuals for each cluster (recurrent of
#' type 1).}
#'
#' \item{martingale2.res}{martingale residuals for each cluster
#' (recurrent of type 2).}
#'
#' \item{martingaledeath.res}{martingale residuals for
#' each cluster (death).}
#'
#' \item{frailty.pred}{empirical Bayes prediction of the
#' first frailty term.}
#'
#' \item{frailty2.pred}{empirical Bayes prediction of the
#' second frailty term.}
#'
#' \item{frailty.var}{variance of the empirical Bayes
#' prediction of the first frailty term.}
#'
#' \item{frailty2.var}{variance of the
#' empirical Bayes prediction of the second frailty term.}
#'
#' \item{frailty.corr}{Correlation between the empirical Bayes prediction of
#' the two frailty.}
#'
#' \item{linear.pred}{linear predictor: uses Beta'X + ui in
#' the multivariate frailty models.}
#'
#' \item{linear2.pred}{linear predictor: uses
#' Beta'X + vi in the multivariate frailty models.}
#'
#' \item{lineardeath.pred}{linear predictor for the terminal part form the
#' multivariate frailty models: Beta'X + alpha1 ui + alpha2 vi}
#'
#' \item{global_chisq}{Recurrent event of type 1: a vector with the values of
#' each multivariate Wald test.}
#'
#' \item{dof_chisq}{Recurrent event of type 1: a
#' vector with the degree of freedom for each multivariate Wald test.}
#'
#' \item{global_chisq.test}{Recurrent event of type 1: a binary variable equals
#' to 0 when no multivariate Wald is given, 1 otherwise.}
#'
#' \item{p.global_chisq}{Recurrent event of type 1: a vector with the p-values
#' for each global multivariate Wald test.}
#'
#' \item{names.factor}{Recurrent event
#' of type 1: Names of the "as.factor" variables.}
#'
#' \item{global_chisq2}{Recurrent event of type 2: a vector with the values of
#' each multivariate Wald test.}
#'
#' \item{dof_chisq2}{Recurrent event of type 2: a
#' vector with the degree of freedom for each multivariate Wald test.}
#'
#' \item{global_chisq.test2}{Recurrent event of type 2: a binary variable
#' equals to 0 when no multivariate Wald is given, 1 otherwise.}
#'
#' \item{p.global_chisq2}{Recurrent event of type 2: a vector with the p_values
#' for each global multivariate Wald test.}
#'
#' \item{names.factor2}{Recurrent
#' event of type 2: Names of the "as.factor" variables.}
#'
#' \item{global_chisq_d}{Terminal event: a vector with the values of each
#' multivariate Wald test.}
#'
#' \item{dof_chisq_d}{Terminal event: a vector with
#' the degree of freedom for each multivariate Wald test.}
#'
#' \item{global_chisq.test_d}{Terminal event: a binary variable equals to 0
#' when no multivariate Wald is given, 1 otherwise.}
#'
#' \item{p.global_chisq_d}{Terminal event: a vector with the p-values for each
#' global multivariate Wald test.}
#'
#' \item{names.factordc}{Terminal event: Names
#' of the "as.factor" variables.}
#'
#' @note "kappa" (kappa[1], kappa[2] and kappa[3]) and "n.knots" (n.knots[1],
#' n.knots[2] and n.knots[3]) are the arguments that the user has to change if
#' the fitted model does not converge.  "n.knots" takes integer values between
#' 4 and 20. But with n.knots=20, the model will take a long time to converge.
#' So, usually, begin first with n.knots=7, and increase it step by step until
#' it converges. "kappa" only takes positive values. So, choose a value for
#' kappa (for instance 10000), and if it does not converge, multiply or divide
#' this value by 10 or 5 until it converges.  Moreover, it may be useful to
#' change the value of the initialize argument.
#'
#' @seealso \code{\link{terminal}},\code{\link{event2}},
#' \code{\link{print.multivPenal}},\code{\link{summary.multivPenal}},\code{\link{plot.multivPenal}}
#'
#' @keywords models methods multiv
#' @export
#' @examples
#'
#'
#' \dontrun{
#'
#' ###--- Multivariate Frailty model ---###
#'
#'
#' }
#'
#'
"multivPenal" <-
  function(formula,
           formula.Event2,
           formula.terminalEvent,
           formula.terminalEvent2,
           data,
           data.Event2,
           initialize=TRUE,
           gapTimes=FALSE,
           jointGeneral=FALSE,
           n.knots, kappa, maxit=350,
           hazard="Weibull",
           nb.int,
           print.times=TRUE,
           GHpoints = 32,
           save.progress = F,
           init.hazard = NULL,
           init.Theta = 0.5,
           init.Alpha1 = 0.1,
           init.Alpha2 = -0.1,
           init.B = NULL)
{
if(save.progress){
    if(file.exists("multiv_model_progress.dat")) file.remove("multiv_model_progress.dat")
    if(file.exists("multiv_model_parameters.dat")) file.remove("multiv_model_parameters.dat")
    if(file.exists("multiv_model_individual_likelihoods.dat")) file.remove("multiv_model_individual_likelihoods.dat")
}


if (class(formula) != "formula")
  stop("The argument formula must be a formula")

if (nrow(data) == 0)
  stop("No (non-missing) observations")

############################ Verify Event 1 / Times
# NN =  names of time gap and recurrent event 1
Y1 <- get_all_vars(update(formula, "~1"), data)

if (ncol(Y1) != 3) {
  stop(
  	"Survival object outcome must be specified using \n
  	time when at risk period starts and stops, such as: \n
  	Surv(time.start, time.stop, event.indicator).
  	This is true for both calendar time and gap time
  	formulations."
  )
}

NN <- colnames(Y1)

EVENT1 <- NN[3]
TSTART <- NN[1]
TSTOP <- NN[2]

event1 <- Y1[, 3]
tt10 <- Y1[, 1]
tt11 <- Y1[, 2]


#if (type != "right" && type != "counting"){
#      stop(paste("Cox model doesn't support \"", type, "\" survival data", sep = ""))
#}

############################ Verify recurrent event 2
if (!missing(formula.Event2)) {
  Y2 <- get_all_vars(update(formula.Event2, "~1"), data.Event2)

  if (ncol(Y2) != 3) {
  	stop(
  		"Survival object outcome must be specified using \n
  		time when at risk period starts and stops, such as: \n
  		Surv(time.start, time.stop, event.indicator).
  		This is true for both calendar time and gap time
  		formulations."
  	)
  }
  NN <- colnames(Y2)
  EVENT2 <- NN[3]
  TSTART2 <- NN[1]
  TSTOP2 <- NN[2]

  event2 <- Y2[, 3]
  tt0meta0 <- Y2[, 1]
  tt1meta0 <- Y2[, 2]

  if (!all(event2 %in% c(1, 0))) {
  	stop(
  		"event2 must contain a variable coded 0-1 and a non-factor variable"
  	)
  }
  event2.ind <- 1
} else{
  event2.ind <- 0
  event2 <- 0
  tt0meta0 <- 0
  tt1meta0 <- 0
}

############################ Verify Terminal Event 1
TT <-
  survival::untangle.specials(terms(formula, c("terminal")), "terminal", 1:10)$vars
start <- grep("\\(", unlist(strsplit(TT, "")))
stop <- grep("\\)", unlist(strsplit(TT, "")))
TERMINAL1 <- substr(TT, start = start + 1, stop = stop - 1)
if (length(TERMINAL1) == 0) {
  stop("A term for a terminal event must be included in the formula.")
}
if (!all(data[[TERMINAL1]] %in% c(1, 0))) {
  stop("terminal must contain a variable coded 0-1 and a non-factor variable")
}

############################ Verify Terminal Event 2
TT <-survival::untangle.specials(terms(formula, c("terminal2")), "terminal2", 1:10)$vars
start <- grep("\\(", unlist(strsplit(TT, "")))
stop <- grep("\\)", unlist(strsplit(TT, "")))
TERMINAL2 <- substr(TT, start = start + 1, stop = stop - 1)
if (length(TERMINAL2) == 0) {
  terminal2.ind <- 0
} else{
  if (!all(data[[TERMINAL2]] %in% c(1, 0))) {
  	stop(
  		"terminal must contain a variable coded 0-1 and a non-factor variable"
  	)
  }
  terminal2.ind <- 1
}

################################ Verify Cluster Variable
# name of cluster variable
TT <-
  survival::untangle.specials(terms(formula, c("cluster")), "cluster", 1:10)$vars
start <- grep("\\(", unlist(strsplit(TT, "")))
stop <- grep("\\)", unlist(strsplit(TT, "")))
CLUSTER <- substr(TT, start = start + 1, stop = stop - 1)

if (length(data[[CLUSTER]])) {
  uni.cluster <- unique(data[[CLUSTER]])
} else{
  stop("grouping variable is needed")
}
if (length(uni.cluster) == 1) {
  stop("grouping variable must have more than 1 level")
}
if (event2.ind == 1){
	if(CLUSTER %in% colnames(data.Event2)) {
		stop("grouping variable must be present in data.Event2")
	}
	if (!all(uni.cluster %in% data.Event2[[CLUSTER]])) {
		stop("all groups must be represented in data.Event2")
	}
}

##########################################################################
# Hazard specification

haztemp <- hazard
if (!hazard %in% c("Weibull", "Splines")) {
  stop("Only 'Weibull' or 'Splines' hazard can be specified in hazard argument.")
}

# typeof is the numerical indicator for hazard
typeof <- switch(hazard,
  	     "Splines" = 0,
  	     "Weibull" = 2)
size <- c(100, 100, 100, 100)


if(typeof == 0) {
  	equidistant <- 1
}else if(typeof == 2) {
  	### Weibull
  	equidistant <- 1
}

#### Configure Splines Hazard (knots, pentalty)
if (typeof == 0) {
  crossVal <- 1 # always do cross validation for splines
  if (missing(kappa))
  	stop("smoothing parameter (kappa1) is required")
  if (missing(n.knots))
  	stop("number of knots are required")
  if (class(kappa) != "numeric")
  	stop("The argument kappa must be a numeric")
  if (class(n.knots) != "integer"){
  	warning("Converting n.knots to integers")
  	n.knots <- as.integer(n.knots)
  }
  if (length(n.knots) != 1) {
  	stop("length of knots must be 1.")
  }
  if (length(kappa) != 2 + event2.ind + terminal2.ind) {
  	stop(
  		"length of kappa must be equal to the number of formulas
  		for different event types (3 or 4) in the order
  		recurrent1, recurrent2, terminal1, terminal2."
  	)
  }
  if (event2.ind == 1 & terminal2.ind == 0) {
  	kappa = c(kappa0, 0)
  }
  if (event2.ind == 0 & terminal2.ind == 1) {
  	kappa = c(kappa[1:2], 0, kappa[3])
  }
  n.knots[n.knots < 4 & n.knots != 0] <- 4
  n.knots[n.knots > 20] <- 20
}else if(typeof == 2){
  # if the hazard is weibull
  if (!(missing(n.knots)) || !(missing(kappa))) {
  	warning("When parametric hazard is not 'Splines'
  		'kappa' and 'n.knots' arguments are ignored.")
  }
  n.knots <- 0
  kappa <- rep(0, 4)
  crossVal <- 0
}

########################### End Hazard Configuration

flush.console()
if (print.times) {
  ptm <- proc.time()
  #cat("\n")
  #cat("The program is computing ... \n")
}

#########################################################################
# Configure Model Matrices

noVarEvent = c(0,0,0,0) # need to fix this

# Recurrent1
specials = c("strata", "cluster", "terminal", "event2", "terminal2")
Terms = terms(formula, specials = specials)
modelmatrix1 =
  model.matrix(update(
  	drop.terms(
  		termobj = terms(formula),
  		unlist(attr(Terms, "specials")) - 1,
  		keep.response = TRUE
  	),
  	~ . - 1
  ),
  data) # need to delete specials from the formula

# need to get densely-ranked ids
group1 <- as.numeric(factor(data[[CLUSTER]]))

# Compute Event Counts
#nevents1 <- tapply(event1, group1, sum)

# Recurrent 2
if (event2.ind == 1) {
  group2 <- as.numeric(factor(data.Event2[[CLUSTER]]))
  # Compute Event Counts
  #nevents2 <- tapply(event2, group2, sum)
  Terms2 = terms(formula.Event2, specials = specials)
  if (!is.null(unlist(attr(Terms2, "specials")))) {
  	modelmatrix3 =
  		model.matrix(update(
  			drop.terms(
  				termobj = terms(formula.Event2),
  				unlist(attr(
  					Terms2, "specials"
  				)) - 1,
  				keep.response = TRUE
  			),
  			~ . - 1
  		),
  		data.Event2)
  } else{
  	modelmatrix3 =
  		model.matrix(update(formula.Event2, ~ . - 1),
  			 data.Event2)
  }
} else{
  modelmatrix3 = matrix(0)
  group2 = 0
}

# Terminal 1
data.terminal <- do.call(what = "rbind",
  	 lapply(split(x = data, f = data[[CLUSTER]]),
  	        function(df) {
  	        	subset(df, df[[TSTOP]] == max(df[[TSTOP]]))
  	        }))
groupdc <- as.numeric(factor(data.terminal[[CLUSTER]]))
tt1dc <- data.terminal[[TSTOP]]
terminal1 <- data.terminal[[TERMINAL1]]

modelmatrix2 = model.matrix(update(formula.terminalEvent, ~ . - 1), data.terminal)

# Terminal 2
if(terminal2.ind == 1) {
  terminal2 <- data.terminal[[TERMINAL2]]
  modelmatrix4 = model.matrix(update(formula.terminalEvent2, ~ . - 1),
  		    data.terminal)
} else{
  terminal2 <- data.terminal[[TERMINAL1]]*0
  modelmatrix4 = matrix(0)
}

############################################################
# Compute Gap Times (If Applicable)

if(gapTimes){
  tt11 <- tt11 - tt10
  tt10 <- 0 * tt10
  tt1meta0 <- tt1meta0 - tt0meta0
  tt0meta0 <- 0 * tt0meta0
}

#########################################################################
# Configure Parameters

### Total number of parameters
nvar = ncol(modelmatrix1) +
  ncol(modelmatrix2)  +
  ncol(modelmatrix3) * event2.ind+
  ncol(modelmatrix4) * terminal2.ind

nbvar = c(
	ncol(modelmatrix1),
	ncol(modelmatrix2),
	ncol(modelmatrix3),
	ncol(modelmatrix4)
)

# Total number of parameters
# This will need adjustment before incorporating a second recurrent event
if(typeof==0 ){ # splines, single random effect
	np = ((2 + n.knots) * (2 + event2.ind + terminal2.ind) + nvar + 3 + 2*jointGeneral)
}else if(typeof == 2){ # weibull, single random effect
	np = (2 * (2 + event2.ind + terminal2.ind) + nvar + 3 + 2*jointGeneral)
}

###################################################
# Define GH nodes Weights

gh <- statmod::gauss.quad(GHpoints, kind="hermite")
ghNodes = gh$nodes
ghWeights = gh$weights * exp(gh$nodes^2)

#####################################################################
# Fill Parameter Vector with User-Defined Values OR Initialize Models

# Check if user entered values for hazard, input 1s if not
if(is.null(init.hazard)) init.hazard <- rep(1, np - nvar - 3 - 2*jointGeneral)

# Check if user entered values for coefficients, input 0s if not
if(is.null(init.B)) init.B <- rep(0, nvar)


# Check lengths of inputs
if(typeof == "Weibull" & length(init.hazard != 2 * (2 + event2.ind + terminal2.ind))){
	stop("init.hazard must have length 6 for weibull for three
	     event types, or length 8 for four event types.")
}else if(typeof == "Splines" & length(init.hazard != (n.knots + 2) * (2 + event2.ind + terminal2.ind))){
		stop("init.hazard must have length (n.knots + 2) * number of event types (3 or 4) for splines.")
}
if(jointGeneral & length(init.Theta)!=3){
	stop("init.Theta must have length 3 when jointGeneral = T.\n
	     Order should be: c(frailtyVarianceTerminal1,  frailtyVarianceTerminal2, frailtyCorrelation)")
}else if(!jointGeneral & length(init.Theta)!=1){
	stop("init.Theta must have length 1 when jointGeneral = F.")
}
if(length(init.B) != nvar){
	stop("init.B must be the same length as the number of coefficients.")
}

# If initialization indicated, replace values
if(initialize){
	# ignore user-supplied initialization values if initialize == T
	init.hazard <- init.hazard*0 + 1
	init.B <- init.B*0

	# recreate time variable in original data set in case of gap times, create new formula
	if(gapTimes){
		data$gapTimes <- tt11
		initialization.formula <-
			paste("Surv(gapTimes, ", EVENT1, ")",
			      paste(gsub("Surv(.*)","", as.character(formula)), collapse = ""),
			      collapse = "")
	}else{
		initialization.formula <- formula
	}
	initialization.formula <- formula(initialization.formula)

	# create separate formulas for each initialization model
	initialization.formula1 <- drop.terms(terms(initialization.formula),
				  survival::untangle.specials(terms(initialization.formula, c("terminal2")), "terminal2", 1:10)$terms,
				  keep.response = T)
	initialization.formula1 <- formula(initialization.formula1)

	initialization.formula2 <- drop.terms(terms(initialization.formula),
				  survival::untangle.specials(terms(initialization.formula, c("terminal")), "terminal", 1:10)$terms,
				  keep.response = T)
	initialization.formula2 <- formula(initialization.formula2)
	initialization.formula2 <- sub("terminal2\\(","terminal\\(",initialization.formula2)
	initialization.formula2 <- formula(paste0(initialization.formula2[2:3], collapse = "~"))


	# fit two joint models for initialization
	mod.joint1<-
		frailtyPenal(formula = initialization.formula1,
			 # this line drops the "terminal2" term from the original model formula
			 formula.terminalEvent = formula.terminalEvent,
			 jointGeneral = F,
			 data = data,
			 recurrentAG= !gapTimes,
			 hazard = "Weibull",RandDist = "LogN",
			 maxit = 100, print.times = F)

	mod.joint2<-
		frailtyPenal(formula = initialization.formula2,
			 # this line drops the "terminal" term from the original model formula
			 formula.terminalEvent = formula.terminalEvent2,
			 jointGeneral = F,
			 data = data,
			 recurrentAG= !gapTimes,
			 hazard = "Weibull", RandDist = "LogN",
			 maxit = 100, print.times = F)

	# grab initialized values
	# Recurrent
	init.hazard[1:(2+n.knots)] <- (mod.joint1$b[1:(2+n.knots)]^2 + mod.joint2$b[1:(2+n.knots)]^2)/2
		# average estimates from the two models
	# Terminal 1
	init.hazard[(3+n.knots):(4+n.knots*2)] <- mod.joint1$b[(3+n.knots):(4+n.knots*2)]^2
	# Terminal 2
	init.hazard[(5+n.knots*2):(6+n.knots*3)] <- mod.joint2$b[(3+n.knots):(4+n.knots*2)]^2

	# Random Effect Variance
	if(!jointGeneral){
		init.Theta <- (mod.joint1$b[5+n.knots*2]^2 + mod.joint2$b[5+n.knots*2]^2)/2
			# average estimates from the two models
	}else{
		init.Theta[1] <- mod.joint1$b[5+n.knots*2]^2
		init.Theta[2] <- mod.joint2$b[5+n.knots*2]^2
		init.Theta[3] <- 0 # rho, covariance
	}

	# Alpha
	init.Alpha1 <- mod.joint1$b[6+n.knots*2]
	init.Alpha2 <- mod.joint2$b[6+n.knots*2]

	# Coefficients
	if(noVarEvent[1] == 0){
		# average two estimates
		init.B[1:nbvar[1]] <- (mod.joint1$b[(7+n.knots*2):(6+n.knots*2+nbvar[1])] + mod.joint2$b[(7+n.knots*2):(6+n.knots*2+nbvar[1])])/2
	}
	if(noVarEvent[2] == 0){
		init.B[(1+nbvar[1]):(nbvar[1]+nbvar[2])] <- mod.joint1$b[(7+n.knots*2+nbvar[1]):(6+n.knots*2+nbvar[1]+nbvar[2])]
	}
	if(noVarEvent[4] == 0){
		init.B[(1+nbvar[1]+nbvar[2]):(nbvar[1]+nbvar[2]+nbvar[4])] <- mod.joint2$b[(7+n.knots*2+nbvar[1]):(6+n.knots*2+nbvar[1]+nbvar[4])]
	}
}

# Fill parameter vector
if(!jointGeneral){
	b <- c(log(sqrt(init.hazard)),
	       log(sqrt(init.Theta)),
	       init.Alpha1, init.Alpha2,
	       init.B)
}else{
	b <- c(log(sqrt(init.hazard)),
	       log(sqrt(init.Theta[1:2])), # variance
	       log((init.Theta[3]+1)/(1-init.Theta[3])), # rho transformed using scale-logit
	       init.Alpha1, init.Alpha2,
	       init.B)
}

start.b <- b

if(length(b)!=np) stop("Parameter vector not the correct length.")
cat("\nmultivPenal.R:: length(b)=",length(b),
    "\nmultivPenal.R:: np = ",np,
    file='../package_tests/multiv_model_progress.dat',append=TRUE)


###################################################
# Check Dimensions of all variables for debugging
controls = c(maxit = maxit[1], # [1]
	 initialize = initialize, # [2]
	 typeof = typeof, # [3]
	 equidistant = 1, # [4]
	 irep = !crossVal, # [5] irep
	 gapTimes = gapTimes, # [6] ag0
	 nbIntervEvent = 0, # [7] nbIntervEvent
	 n.knots = n.knots, # [8]
	 event2.ind = event2.ind, # [9]
	 terminal2.ind = terminal2.ind, # [10]
	 GHpoints = GHpoints, # [11]
	 jointGeneral = as.integer(jointGeneral)) # [12] typeJoint0
if(length(controls) != 12) stop("Length of 'controls' not 12.")
cat("\nmultivPenal.R:: length(controls)=",length(controls),
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

nobsEvent = c(length(event1),
	  length(terminal1),
	  length(event2))
if(length(nobsEvent)!= 3) stop("Length of 'nobsEvent' not 3.")
cat("\n\nmultivPenal.R::nobsEvent=", nobsEvent,
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

if(length(kappa) != 4) stop("Length of 'kappa' not 4.")
cat("\nkappa=", kappa,file='../package_tests/multiv_model_progress.dat',append=TRUE)

### Recurrent 1
if(length(tt10) != nobsEvent[1] | length(tt10) !=  nobsEvent[1] | length(event1) !=  nobsEvent[1] | length(group1) !=  nobsEvent[1]){
	stop("Length of tt00, tt10, event1, group1 not nobsEvent[1]")
}
cat("\nmultivPenal.R:: length(tt10)=",length(tt10),
    "\nmultivPenal.R:: length(tt11)=",length(tt11),
    "\nmultivPenal.R:: length(event1)=",length(event1),
    "\nmultivPenal.R:: length(group1)=",length(group1),
    "\nmultivPenal.R:: max(group1)=",max(group1),
    "\nmultivPenal.R:: length(unique(group1))=",length(unique(group1)),
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

### Recurrent 2
if(length(tt0meta0) !=  nobsEvent[3]  | length(tt1meta0) !=  nobsEvent[3]  | length(group2) !=  nobsEvent[3]  | length(event2) !=  nobsEvent[3] ){
	stop("Length of tt0meta0, tt1meta0, group2, event2 not nobsEvent[3]")
}
cat("\nmultivPenal.R:: length(tt0meta0)=",length(tt0meta0),
    "\nmultivPenal.R:: length(tt1meta0)=",length(tt1meta0),
    "\nmultivPenal.R:: length(group2)=",length(group2),
    "\nmultivPenal.R:: length(event2)=",length(event2),
    "\nmultivPenal.R:: max(group2)=",max(group2),
    "\nmultivPenal.R:: length(unique(group2))=",length(unique(group2)),
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

if(length(event1) != nobsEvent[1]) stop("length(event1) != nobsEvent[1]")
if(length(event2) != nobsEvent[3]) stop("length(event2) != nobsEvent[3]")
if(length(group1) != nobsEvent[1]) stop("length(group1) != nobsEvent[1]")
if(length(group2) != nobsEvent[3]) stop("length(group2) != nobsEvent[3]")
if(length(groupdc) != nobsEvent[2]) stop("length(groupdc) != nobsEvent[2]")
if(max(group1) != nobsEvent[2]) stop("max(group1) != nobsEvent[2]")
if(max(groupdc) != nobsEvent[2]) stop("max(groupdc) != nobsEvent[2]")
if(length(tt1dc) != nobsEvent[2]) stop("length(tt1dc) != nobsEvent[2]")

### Terminal Events
if(length(terminal1) != nobsEvent[2]){
	cat("nobsEvent[2] = ", nobsEvent[2], " and length(terminal1) = ",length(terminal1))
	stop("length(terminal1) != nobsEvent[2]")
}
if(length(terminal2) != nobsEvent[2]){
	cat("nobsEvent[2] = ", nobsEvent[2], " and length(terminal2) = ",length(terminal2))
	stop("length(terminal2) != nobsEvent[2]")
}
cat("\nmultivPenal.R:: length(terminal1, terminal2, groupdc, tt1dc)=",
    length(terminal1),length(terminal2),length(groupdc),length(tt1dc),
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

if(length(nbvar) != 4) stop("length(nbvar) != 4")
cat("\nmultivPenal.R:: nbvar=",nbvar,
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

if(all(dim(modelmatrix1) != c(nobsEvent[1],nbvar[1]))) stop("all(dim(modelmatrix1) != c(nobsEvent[1],nbvar[1]))")
if(all(dim(modelmatrix2) != c(nobsEvent[2],nbvar[2]))) stop("all(dim(modelmatrix2) != c(nobsEvent[3],nbvar[2]))")
if(all(dim(modelmatrix3) != c(nobsEvent[3],nbvar[3]))) stop("all(dim(modelmatrix3) != c(nobsEvent[2],nbvar[3]))")
if(all(dim(modelmatrix4) != c(nobsEvent[2],nbvar[4]))) stop("all(dim(modelmatrix4) != c(nobsEvent[3],nbvar[4]))")
cat("\nmultivPenal.R:: dim(modelmatrix1, modelmatrix2, modelmatrix3, modelmatrix4)",
    dim(modelmatrix1),",", dim(modelmatrix2),",", dim(modelmatrix3),",", dim(modelmatrix4),
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

if(length(noVarEvent) != 4) stop("length(noVarEvent) != 4")
cat("\nmultivPenal.R:: noVarEvent=",noVarEvent,
    file='../package_tests/multiv_model_progress.dat',append=TRUE)

if(any(is.na(modelmatrix1))|any(is.na(modelmatrix2))|any(is.na(modelmatrix3))|any(is.na(modelmatrix4))){
	stop("NA values among covariates. Reconfigure Data.")
}

######################################################################################################
# Send to Fortran for optimization
    ans <- .Fortran(C_joint_multiv,
                controls = as.integer(controls),
                nobsEvent = as.integer(nobsEvent), #nobsEvent
                k0 = as.double(kappa),

                # Data Arguments
                tt00 = as.double(tt10),
                tt10 = as.double(tt11),
                tt0meta0 = as.double(tt0meta0),
                tt1meta0 = as.double(tt1meta0),
                ic0 = as.integer(event1), #ic0
                icmeta0 = as.integer(event2), #icmeta0

                groupe0 = as.integer(group1),#groupe0
                groupe0meta = as.integer(group2),#groupe0meta
                groupe0dc = as.integer(groupdc),#groupe0dc

                tt0dc0 = as.double(0*tt1dc), #tt0dc0
                tt1dc0 = as.double(tt1dc), #tt1dc0
                icdc0 = as.integer(terminal1), #icdc0
                icdc20 = as.integer(terminal2), #icdc20

                nbvar = as.integer(nbvar), #nbvar

                vax0 = as.double(modelmatrix1),
                vaxdc0 = as.double(modelmatrix2), #vaxdc0
                vaxmeta0 = as.double(modelmatrix3),
                vaxdc20 = as.double(modelmatrix4), #vaxdc20

                noVarEvent = as.integer(noVarEvent), #noVarEvent

                # Parameter Information
                np=as.integer(np),
                b=as.double(b),
                H_hessOut=as.double(matrix(0,nrow=np,ncol=np)),
                HIHOut=as.double(matrix(0,nrow=np,ncol=np)),
                resOut=as.double(0),
                LCV=as.double(rep(0,2)),
                critCV=as.integer(rep(0,6)),
                mtEvent = as.integer(rep(100,4)), #mtEvent
                mt1Event = as.integer(rep(100,4)), #mt1Event

                # Survival and Hazard Function Fits
                x1=as.double(rep(0,100)),                  #x1Out
                lam=as.double(matrix(0,nrow=100,ncol=3)),  #lamOut
                xSu1=as.double(rep(0,100)),                #xSu1
                surv=as.double(matrix(0,nrow=100,ncol=3)), #suOut
                x2=as.double(rep(0,100)),                  #x2Out
                lam2=as.double(matrix(0,nrow=100,ncol=3)), #lam2Out
                xSu2=as.double(rep(0,100)),                #xSu2
                surv2=as.double(matrix(0,nrow=100,ncol=3)),#su2Out
                x3=as.double(rep(0,100)),                  #x3Out
                lam3=as.double(matrix(0,nrow=100,ncol=3)), #lam3out
                xSu3=as.double(rep(0,100)),                #xSu3
                surv3=as.double(matrix(0,nrow=100,ncol=3)),#su3Out
                x4=as.double(rep(0,100)),                  #x4Out
    	    lam4=as.double(matrix(0,nrow=100,ncol=3)), #lam4Out
    	    xSu4=as.double(rep(0,100)),                #xSu4
    	    surv4=as.double(matrix(0,nrow=100,ncol=3)),#su4Out

                ni=as.integer(0),
                cptEvent=as.integer(rep(0,4)),
                ResMartingaleEvent=as.double(matrix(0,nrow=nobsEvent[2],ncol=3)),
                frailtyEstimates=as.double(matrix(0,nrow=nobsEvent[2],ncol=5)),

                linearpred=as.double(rep(0,nobsEvent[1])),
                linearpreddc=as.double(rep(0,nobsEvent[2])),
                linearpredM=as.double(rep(0,nobsEvent[3])),
                linearpreddc2=as.double(rep(0,nobsEvent[2])),
                ziOut1=as.double(rep(0,controls[8]+6)),
                ziOutdc=as.double(rep(0,controls[8]+6)),
                ziOutmeta=as.double(rep(0,controls[8]+6)),
                time=as.double(rep(0,controls[7]+1)),
                timedc=as.double(rep(0,controls[7]+1)),
                timeM=as.double(rep(0,controls[7]+1)),
                ghNodes = as.double(ghNodes),
                ghWeights = as.double(ghWeights)
    )
######################################################################################################
# Format Model Tables

if(initialize){
	ans$initialization$b <- start.b
	ans$initialization$joint1 <- mod.joint1
	ans$initialization$joint2 <- mod.joint2
}

# if(jointGeneral == F & hazard == "Weibull"){
# 	f <- function(b){
# 		c(exp(b[1:6])^2,
# 		  exp(b[7]),
# 		  b[(np-nvar-1):np])
# 	}
# 	f.prime <- function(b){
# 		diag(c(2*exp(2*b[1:6]),
# 		       exp(b[7]),
# 		       rep(1,nvar + 2)))
# 	}
# 	Parameter = c("Shape, Recurrent", "Scale, Recurrent",
# 		  "Shape, Terminal1", "Scale, Terminal1",
# 		  "Shape, Terminal2", "Scale, Terminal2",
# 		  "Sigma",
# 		  "Alpha, Terminal1", "Alpha, Terminal2",
# 		  paste0("Beta",(np-nvar+1):np))
# }else if(jointGeneral == T & hazard == "Weibull"){
# 	f <- function(b){
# 		c(exp(b[1:6])^2,
# 		  exp(b[7:8]),
# 		  (exp(b[9]) - 1)/(exp(b[9]) + 1),
# 		  b[(np-nvar-1):np])
# 	}
# 	f.prime <- function(b){
# 		diag(c(2*exp(2*b[1:6]),
# 		       exp(b[7:8]),
# 		       2*exp(2*b[9])/(1+exp(b[9]))^2,
# 		       rep(1,nvar+2)))
# 	}
# 	Parameter = c("Shape, Recurrent", "Scale, Recurrent",
# 		  "Shape, Terminal1", "Scale, Terminal1",
# 		  "Shape, Terminal2", "Scale, Terminal2",
# 		  "Sigma, Terminal1", "Sigma, Terminal2", "Rho",
# 		  "Alpha, Terminal1", "Alpha, Terminal2",
# 		  paste0("Beta",(np-nvar+1):np))
# }if(jointGeneral == F & hazard == "Weibull"){
# 	f <- function(b){
# 		c(exp(b[1:6])^2,
# 		  exp(b[7]),
# 		  b[(np-nvar-1):np])
# 	}
# 	f.prime <- function(b){
# 		diag(c(2*exp(2*b[1:6]),
# 		       exp(b[7]),
# 		       rep(1,nvar + 2)))
# 	}
# 	Parameter = c("Shape, Recurrent", "Scale, Recurrent",
# 		  "Shape, Terminal1", "Scale, Terminal1",
# 		  "Shape, Terminal2", "Scale, Terminal2",
# 		  "Sigma",
# 		  "Alpha, Terminal1", "Alpha, Terminal2",
# 		  paste0("Beta",(np-nvar+1):np))
# }else if(jointGeneral == T & hazard == "Splines"){
# 	f <- function(b){
# 		c(exp(b[1:6])^2,
# 		  exp(b[7:8]),
# 		  (exp(b[9]) - 1)/(exp(b[9]) + 1),
# 		  b[(np-nvar-1):np])
# 	}
# 	f.prime <- function(b){
# 		diag(c(2*exp(2*b[1:6]),
# 		       exp(b[7:8]),
# 		       2*exp(2*b[9])/(1+exp(b[9]))^2,
# 		       rep(1,nvar+2)))
# 	}
# 	Parameter = c("Shape, Recurrent", "Scale, Recurrent",
# 		  "Shape, Terminal1", "Scale, Terminal1",
# 		  "Shape, Terminal2", "Scale, Terminal2",
# 		  "Sigma, Terminal1", "Sigma, Terminal2", "Rho",
# 		  "Alpha, Terminal1", "Alpha, Terminal2",
# 		  paste0("Beta",(np-nvar+1):np))
# }
# ans$varH.Raw <- matrix(ans$H, nrow = np, ncol = np)
# ans$varH.Estimate <- f.prime(ans$b) %*% ans$varH.Raw %*% f.prime(ans$b)
# ans$summary.table <- tibble(
# 	Parameter = Parameter,
# 	Raw = ans$b,
# 	Raw.SE = sqrt(diag(ans$varH.Raw)),
# 	Estimate = f(ans$b),
# 	Estimate.SE = sqrt(diag(ans$varH.Estimate)),
# 	LB95 = f(ans$b - 2*Raw.SE),
# 	UB95 = f(ans$b + 2*Raw.SE),
# 	p = 2*pnorm(q = -abs(ans$b), mean = 0, sd = Raw.SE),
# 	H0 = paste(Parameter, " = ", f(rep(0, np)))
# )
return(ans)
}



