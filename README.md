# Delirium
Some Helpful hints for any future students working on this package

	1. Installing this package (or any compiled package) on the cluster
	
	2. Preparing to run batches on the cluster.
 
## Installing this Package on JHPCE

(1) Make sure to output and upload the package from your local computer
by going to Build > More > Build Source Package.  Otherwise type into terminal:

	`R CMD BATCH packagename`
	
This produces “packagename_VERSION.tar.gz, which is called the package tarball. 
Technically, it is different from just zipping up your package. 
This step cleans up your code and also processes your vignettes (if you have any). 
This package tarball is system-independent, 
ie. it can be installed anywhere (assuming your installation computer has the appropriate tools).

(2) Upload these source files to the cluster. I use scp like a n00b.
	`scp LocationOfPackage/packagename_VERSION.tar.gz username@jhpce-transfer01.jhsph.edu:~/`
	* enter key *
	* enter password *

(3) Install the package. (Make sure to qrsh onto a computing node and load R.)
	`ssh username@jhpce01.jhsph.edu`
	* enter key *
	* enter password *
	`qrsh`
	`module load conda_R`
	`R CMD INSTALL packagename_VERSION.tar.gz`

## Preparing to Run Simulations on the Cluster

This function will make running batches of R code easier, thanks to John Muschelli (https://johnmuschelli.com/cluster_rstudio/#5_Submitting_R_Jobs).

(1) Upload the file cluster_shit/Rsubmit.sh to your working directory on the cluster.

	`scp Delirium/cluster_shit/Rsubmit.sh username@jhpce-transfer01.jhsph.edu:~/`
	* enter key *
	* enter password *
	
(2) Add these three lines to the end of your bash profile (in this order):

	`echo ‘if [ -f ~/Rsubmit.sh ]; then’ >> ~/.bash_profile`
	`echo ‘	. ~/Rsubmit.sh’ >> ~/.bash_profile`
	`echo ‘fi’ >> ~/.bash_profile:`
