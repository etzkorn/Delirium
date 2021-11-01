library(tidyverse)
###########################################
# Find variables in allcoation statements
routine <- read_file("src/multiveJoint.f90")%>%
	str_split("\n") %>%
	unlist
allocate.statements <- grepl("allocate",routine)
and.statements <- grepl("&",routine)
allocate.count <- 0

while(sum(allocate.statements) > allocate.count){
	allocate.count <- sum(allocate.statements)
	#cat("allocate count: ", allocate.count, "\n")
	allocate.statements <- allocate.statements |
		c(F, (allocate.statements & and.statements)[-length(allocate.statements)])
}

allocates <- routine[allocate.statements]
rm(routine, allocate.statements, and.statements, allocate.count)

###########################################
# Find subgroup of allocate statements that are deallcoation statements
deallocate.statements <- grepl("deallocate",allocates)
and.statements <- grepl("&",allocates)
deallocate.count <- 0

while(sum(deallocate.statements) > deallocate.count){
	deallocate.count <- sum(deallocate.statements)
	#cat("deallocate count: ", deallocate.count, "\n")
	deallocate.statements <- deallocate.statements |
		c(F, (deallocate.statements & and.statements)[-length(deallocate.statements)])
}

###########################################
deallocates <- allocates[deallocate.statements]
allocates <- allocates[!deallocate.statements]

deallocates <- gsub("\\!.*","",deallocates) %>%
	trimws %>%
	gsub(pattern = "deallocate\\(",replacement = "") %>%
	gsub(pattern = "\\)",replacement = "") %>%
	gsub(pattern = "&",replacement = "") %>%
	str_split(",") %>%
	unlist
deallocates <- deallocates[nchar(deallocates)>0]

allocates <- gsub("\\!.*","",allocates) %>%
	trimws %>%
	gsub(pattern = "allocate\\(",replacement = "") %>%
	unlist %>%
	gsub(pattern = "\\([0-9 a-z A-Z \\, \\+ \\* \\/ \\: \\-]*\\)",replacement = "") %>%
	gsub(pattern = "\\([0-9 a-z A-Z \\, \\+ \\* \\/ \\: \\-]*\\)",replacement = "") %>%
	gsub(pattern = "\\([0-9 a-z A-Z \\, \\+ \\* \\/ \\: \\-]*\\)",replacement = "") %>%
	gsub(pattern = "\\)",replacement = "") %>%
	gsub(pattern = "&",replacement = "") %>%
	str_split(",") %>%
	unlist %>%
	trimws
allocates <- allocates[nchar(allocates)>0]

cat("Needing deallocation:", length(unique(allocates)) - length(unique(deallocates)),
    "\n",setdiff(allocates, deallocates),
    "\nNeeding Allocation:", setdiff(deallocates,allocates))

#(table(allocates) - table(deallocates))[table(allocates) - table(deallocates) != 0]
