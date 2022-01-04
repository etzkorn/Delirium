# Delirium
Some Helpful hints for any future students working on this package

	1. Installing this package (or any compiled package) on the cluster
 
############################################################
# Installing this Package on JHPCE
### (1) Make sure to output and upload the package from your local computer
### by going to Build > More > Build Source Package.  Otherwise type into terminal:
	R CMD BATCH packagename
### This produces “packagename_VERSION.tar.gz, which is called the package tarball. 
### Technically, it is different from just zipping up your package. 
### This step cleans up your code and also processes your vignettes (if you have any). 
### This package tarball is system-independent, 
### ie. it can be installed anywhere (assuming your installation computer has the appropriate tools).

### (2) Upload these source files to the cluster. I use scp like a n00b.
	scp LocationOfPackage/packagename_VERSION.tar.gz username@jhpce-transfer01.jhsph.edu:~/
	# enter key
	# enter password

### (3) Install the package. (Make sure to qrsh onto a computing node and load R.)
	ssh username@jhpce01.jhsph.edu
	# enter key
	# enter password
	qrsh
	module load conda_R
	R CMD INSTALL packagename_VERSION.tar.gz
