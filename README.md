# Delirium

## File Structure

### `frailtypack` Folder

`frailtypack` is the most important folder here. 
It contains a copy of the source files from CRAN from around February 2020, 
as well as changes I have made to a few of the files:

* `init.c` contains a list of the Fortran functions in the package, as well as the number of arguments in each function. In this file, I changed line 18 when I changed the number of arguments in the function. The current number of arguments is 64.

```
{"joint_multiv",         (DL_FUNC) &F77_SUB(joint_multiv),         64}
```

* `multivJoint.f90` used to contain the fortran routine that is called by R to perform the fitting of the the model for two recurrent and one terminal events (`joint_multiv`), but I have modified this to fit our model. Now, `joint_multiv` fits the model for two terminal events and one recurrent event. When we include our function in the frailtypack package, we will need to give this file (`multivJoint.f90`) and function (`joint_multiv`) new names and a new entry in `init.c`. I have also added a header within this file that gives a table of contents for this subroutine:

```
! Organizational Overview of:
! (0) Variable definitions
! (1) Unpack model controls
! (2) Parameter initialization (if specified by user)
! (3) Copy data
! (4) Sort Unique Dates for each event type
! (5) Construct Vector of Spline Nodes (Z_i)
! (6) Map dates to sequence of unique dates
! (7) Calculate penalties
! (8) Copy initialized parameters to "b"
! (9) Model Optimization
! (10) Organize Hessian Matrix
! (11) Evaluate hazard and survival estimates over a grid
! (12) Calculate AIC
! (13) Calculate Fitted Values
! (14) Deallocation and return

! Supporting Functions
! (a) vecspli
! (b) vecpenP
! (c) susp
! (d) cosp ! calcul les points pour les fonctions ! et leur bandes de confiance
! (e) conf1
! (f) conf
! (g) isp
! (h) mmsp
! (i) multi ! matrix multiply A by B with the result in C
! (j) func30 ! calcul de l integrant, pour un effet aleatoire donne frail et un groupe donne auxig (cf funcpa)

! istop codes:
!    1: convergence
!    2: maximum number of iterations
!    4: error (any calculation error)
```

* `funcpaMultivWeib.f90` contains the subroutine (`funcpaMultivWeib`) that calculates the likelihood given a parametric splines model for the baseline hazard. I modified this function to calculate the likelihood for our model, so this file and function will need new names before integrating it back into `frailtypack`. Note: I found it a bit confusing the way that the current package referenced a number of functions to compute the integral portion of the likelihood, and I wanted to refrain from modifying these existing functions or creating new ones, so I brought all of the code to do that integration into this likelihood file directly. That code is copied here:

```
    if(typeJoint.eq.0)then
        do k=1, ng
            ss=0.d0
            do j=1, ghPoints
                    weight = ghWeights(j)
                    frail = ghNodes(j)
                    ss = ss + &
                    (weight* dexp( & ! GQ Weights
                    frail * cpt(k) - dexp(frail)*(res1(k)-res3(k)) & ! recurrent event 1
                    + alpha1 * frail * cdc(k) - dexp(frail*alpha1) * aux1(k) & ! terminal event 1
                    + alpha2 * frail * cdc2(k) - dexp(frail*alpha2) * aux2(k) & ! terminal event 2
                    - (frail**2.d0)/(2.d0*theta)))! frailty distribution (normal)
            end do
            if((ss.eq.0.0d0)) ss = 1.0d-10 ! integral should not be too small when theta is large
            integrale3(k) = ss
            if ((integrale3(k).ne.integrale3(k)).or.(abs(integrale3(k)).ge.1.d30)) then
                funcpaMultivWeib=-1.d9
                goto 123
            end if
        end do
    else
```

* `funcpaMultivSpines.f90` contains a similar likelihood subroutine, but this function currently does not work. I found that when attempting to optimize the likelihood, one of the spline coefficients was not changing at all, which makes me believe I using incorrect indexes for the parameter vector somewhere in this function. This needs to be investigated before this function is useable.

* `aaOptim.f90` contains the optimizer routine, and got some added messages so I could examine errors during the optimization process.

* `AparamMultive.f90` contains some modules of "global" variables, which I added to.

* `multivPenal.R` contains the R function that the user calls to fit the competing joint model. Both the file and function will need to be re-named before integrating it back into frailtypack. This function performs some pre-processing and then calls the Fortran routine `joint_multiv` to fit the model.

### `delirium_package` Folder

This folder contains the code that I initially wrote in R to fit some of the models

## Developing with this package on JHPCE

Some Helpful hints for any future students working on this package

	1. Installing this package (or any compiled package) on the cluster
	
	2. Preparing to run batches on the cluster.
	
	3. Running batches on the cluster.
	
	4. Deleting unnecessary output files.
 
### Installing this Package on JHPCE

(1) Make sure to output and upload the package from your local computer
by going to Build > More > Build Source Package.  Otherwise type into terminal:

	R CMD BATCH packagename
	
This produces “packagename_VERSION.tar.gz, which is called the package tarball. 
Technically, it is different from just zipping up your package. 
This step cleans up your code and also processes your vignettes (if you have any). 
This package tarball is system-independent, 
ie. it can be installed anywhere (assuming your installation computer has the appropriate tools).

(2) Upload these source files to the cluster. I use scp like a n00b.

	scp LocationOfPackage/packagename_VERSION.tar.gz username@jhpce-transfer01.jhsph.edu:~/
	
	 #enter key 
	
	 #enter password 

(3) Install the package. (Make sure to qrsh onto a computing node and load R.)

	ssh username@jhpce01.jhsph.edu
	
	 #enter key 
	
	 #enter password 
	
	qrsh
	
	module load conda_R
	
	R CMD INSTALL packagename_VERSION.tar.gz

### Preparing to Run Simulations on the Cluster

This function will make running batches of R code easier, thanks to John Muschelli (https://johnmuschelli.com/cluster_rstudio/#5_Submitting_R_Jobs).

(1) Upload the file cluster_shit/Rsubmit.sh to your working directory on the cluster.

	scp Delirium/cluster_shit/Rsubmit.sh username@jhpce-transfer01.jhsph.edu:~/
	
	 #enter key 
	
	 #enter password 
	
(2) Add these three lines to the end of your bash profile (in this order):

	echo ‘if [ -f ~/Rsubmit.sh ]; then’ >> ~/.bash_profile
	
	echo ‘	. ~/Rsubmit.sh’ >> ~/.bash_profile
	
	echo ‘fi’ >> ~/.bash_profile:
	
### Running Some Simulations on the Cluster
	
Assuming you've also uploaded the following files:
	
	simulation_analyses/Fit_Model.R
	
	delirium_package/R/random_weibull.R
	
	delirium_package/R/random_lognormal.R
	
	delirium_package/R/joint_simulate_data.R
	
	simulation_analyses/0_Generate_Simulation_Values.R
	
and you've run 0_Generate_Simulation_Values.R, 
you should be able to execute the following code to 
run the first two models on simulated data sets:
	
	Rnosave Simulation_Scripts/Fit_Model.R -t 1-2 -tc 2 -N TestTwoJobs

### Removing output files:
	
This should remove the output files from your main directory:

	ls | grep -P "TestTwoJobs." | xargs -d"\n" rm
	
### Downloading Results

	scp username@jhpce-transfer01.jhsph.edu:~/YourResults.rdata ~/TargetDirectory



