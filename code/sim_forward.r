
## Simulate the reinitiation and extinction times of small places from the posterior, followed by inference ##
#############################################################################################################


require(parallel)
require(geosphere)
require(ggplot2)
require(Rcpp)
require(truncdist)
require(synchrony)
require(TSclust)
require(proxy)
require(gridExtra)

set.seed(1)



period <- "pre_vac"




path1<- "/Users/slau24/Google Drive/git projects/measles_competing_risks/data/formatted/prevac/" # data input
path2<- "/Users/slau24/Google Drive/git projects/measles_competing_risks/code/" # script directory
path3<- "/Users/slau24/Google Drive/git projects/measles_competing_risks/output/mcmc inference/pre_vac/" # mcmc inference results
path4<- "xx" # output



source(paste(path2,"sim_forward_functions.r",sep=""))


n_sim <- 100


## read the posterior samples ##
burn <- 1000

beta_coupling_mc <- (read.csv(file=paste(path3,"beta_coupling_mc.csv",sep="")))[,1][-(1:burn)]
rho_mc <- (read.csv(file=paste(path3,"rho_mc.csv",sep="")))[,1][-(1:burn)]
alpha_1_mc <- (read.csv(file=paste(path3,"alpha_1_mc.csv",sep="")))[,1][-(1:burn)]
alpha_2_mc <- (read.csv(file=paste(path3,"alpha_2_mc.csv",sep="")))[,1][-(1:burn)]
eta_mc <- (read.csv(file=paste(path3,"eta_mc.csv",sep="")))[,1][-(1:burn)]
b_mc <- (read.csv(file=paste(path3,"b_mc.csv",sep="")))[,1][-(1:burn)]
tau_mc <- (read.csv(file=paste(path3,"tau_mc.csv",sep="")))[,1][-(1:burn)]
psi_mc <- (read.csv(file=paste(path3,"psi_mc.csv",sep="")))[,1][-(1:burn)]
a_mc <- (read.csv(file=paste(path3,"a_mc.csv",sep="")))[,1][-(1:burn)]




#################################
## field data and extraction ##

##

# sort_ind <- 2 # 1= sort small cities according to distance to the big city; 2= .. according to population size 

## field data and extraction ##

##


# if (data_resol=="64_biweek") {
subset_data_row_start <- 1
subset_data_row_end <- 534 # number of weeks to analyze (max 534)
pop_bound <- 90000 # upper bound of population size of defined small cities 

# }



n_0 <- 1 # number of weeks of 0 before required for defining an re-initiations; see also function define_influx
n_1 <- 1 # number of weeks of 1 ahead ...

#
lon_lb <- -6.0
lon_ub <- 1.8

lat_lb <- 50
lat_ub <- 55.8


include_city <- function(coordinates, lon_lb, lon_ub, lat_lb, lat_ub){ # determine if to include a city based on a region defined
	1*(coordinates[1]>=lon_lb & coordinates[1]<=lon_ub & coordinates[2]>=lat_lb & coordinates[2]<=lat_ub)
}


##


lon_lb <- -6.0
lon_ub <- 1.8

lat_lb <- 50
lat_ub <- 55.8


include_city <- function(coordinates, lon_lb, lon_ub, lat_lb, lat_ub){ # determine if to include a city based on a region defined
	1*(coordinates[1]>=lon_lb & coordinates[1]<=lon_ub & coordinates[2]>=lat_lb & coordinates[2]<=lat_ub)
}

##
# scale_mix <- c(0.2, 0.3, 0.3, 0.3,  0.3, 0.3, 0.3, 0.3 ) # scaling constant for MH-samplers for beta_coupling, alpha_1, alpha_2, rho,  eta, phi, tau, psi
# sd_mix <- c(0.2, 0.7, 0.3, 0.3,  0.3, 0.3, 0.5, 0.7) # proposal SD for ...
## field data and extraction ##

cases_urban <- read.csv(paste(path1,"inferred_cases_urban.csv",sep=""))[subset_data_row_start:subset_data_row_end,] 
pop_urban <- read.csv(paste(path1,"inferred_pop_urban.csv",sep=""))[subset_data_row_start:subset_data_row_end,] 
coordinates_urban <- read.csv(paste(path1,"coordinates_urban.csv",sep=""))


susceptibles_urban <- read.csv(paste(path1,"susceptibles_urban.csv",sep=""))[subset_data_row_start:subset_data_row_end,]

cities_drop <- c( "Yeovil","Yeovil.RD","City.of.London") # places with duplicated coordinates 

cases_urban <-  cases_urban[, !(colnames(cases_urban)%in%cities_drop)]
pop_urban <-  pop_urban[, !(colnames(pop_urban)%in%cities_drop)]
# pop_urban_ref <-  pop_urban_ref[, !(colnames(pop_urban_ref)%in%cities_drop)]
susceptibles_urban <-  susceptibles_urban[, !(colnames(susceptibles_urban)%in%cities_drop)]


coordinates_urban <- subset(coordinates_urban, select=colnames(cases_urban))

names_cities <- colnames(pop_urban)[-1]



##
core_cities <- c("London","Birmingham","Liverpool","Manchester","Sheffield","Leeds","Newcastle","Bristol","Cardiff","Norwich","Swansea","Plymouth","Portsmouth")

cases_london <- subset(cases_urban, select=c("X","London"))
pop_london <- subset(pop_urban, select=c("X","London"))
#births_london <- subset(births_urban, select=c("X","London"))
coordinates_london <- subset(coordinates_urban, select=c("X","London"))
susceptibles_london <- subset(susceptibles_urban, select=c("X","London"))
# mean_susceptibles_london <- subset(mean_susceptibles_urban, select=c("X","London"))

##

cases_bmh <- subset(cases_urban, select=c("X","Birmingham"))
pop_bmh <- subset(pop_urban, select=c("X","Birmingham"))
#births_bmh <- subset(births_urban, select=c("X","Birmingham"))
coordinates_bmh <- subset(coordinates_urban, select=c("X","Birmingham"))
susceptibles_bmh <- subset(susceptibles_urban, select=c("X","Birmingham"))
# mean_susceptibles_bmh <- subset(mean_susceptibles_urban, select=c("X","Birmingham"))

##

cases_liv <- subset(cases_urban, select=c("X","Liverpool"))
pop_liv <- subset(pop_urban, select=c("X","Liverpool"))
#births_bmh <- subset(births_urban, select=c("X","Birmingham"))
coordinates_liv <- subset(coordinates_urban, select=c("X","Liverpool"))
susceptibles_liv <- subset(susceptibles_urban, select=c("X","Liverpool"))
# mean_susceptibles_liv <- subset(mean_susceptibles_urban, select=c("X","Liverpool"))

##

cases_man <- subset(cases_urban, select=c("X","Manchester"))
pop_man <- subset(pop_urban, select=c("X","Manchester"))
#births_bmh <- subset(births_urban, select=c("X","Birmingham"))
coordinates_man <- subset(coordinates_urban, select=c("X","Manchester"))
susceptibles_man <- subset(susceptibles_urban, select=c("X","Manchester"))
# mean_susceptibles_man <- subset(mean_susceptibles_urban, select=c("X","Manchester"))

##

cases_shef <- subset(cases_urban, select=c("X","Sheffield"))
pop_shef <- subset(pop_urban, select=c("X","Sheffield"))
#births_bmh <- subset(births_urban, select=c("X","Birmingham"))
coordinates_shef <- subset(coordinates_urban, select=c("X","Sheffield"))
susceptibles_shef <- subset(susceptibles_urban, select=c("X","Sheffield"))
# mean_susceptibles_shef <- subset(mean_susceptibles_urban, select=c("X","Sheffield"))

##

cases_leeds <- subset(cases_urban, select=c("X","Leeds"))
pop_leeds <- subset(pop_urban, select=c("X","Leeds"))
#births_bmh <- subset(births_urban, select=c("X","Birmingham"))
coordinates_leeds <- subset(coordinates_urban, select=c("X","Leeds"))
susceptibles_leeds <- subset(susceptibles_urban, select=c("X","Leeds"))
# mean_susceptibles_leeds <- subset(mean_susceptibles_urban, select=c("X","Leeds"))

##


##

cases_newcastle <- subset(cases_urban, select=c("X","Newcastle"))
pop_newcastle <- subset(pop_urban, select=c("X","Leeds"))
#births_new <- subset(births_urban, select=c("X","Newcastle"))
coordinates_newcastle <- subset(coordinates_urban, select=c("X","Newcastle"))
susceptibles_newcastle <- subset(susceptibles_urban, select=c("X","Newcastle"))
# mean_susceptibles_newcastle <- subset(mean_susceptibles_urban, select=c("X","Newcastle"))

## 

cases_bristol <- subset(cases_urban, select=c("X","Bristol"))
pop_bristol <- subset(pop_urban, select=c("X","Leeds"))
#births_new <- subset(births_urban, select=c("X","Bristol"))
coordinates_bristol <- subset(coordinates_urban, select=c("X","Bristol"))
susceptibles_bristol <- subset(susceptibles_urban, select=c("X","Bristol"))
# mean_susceptibles_bristol <- subset(mean_susceptibles_urban, select=c("X","Bristol"))

## 

cases_cardiff <- subset(cases_urban, select=c("X","Cardiff"))
pop_cardiff <- subset(pop_urban, select=c("X","Leeds"))
#births_new <- subset(births_urban, select=c("X","Cardiff"))
coordinates_cardiff <- subset(coordinates_urban, select=c("X","Cardiff"))
susceptibles_cardiff <- subset(susceptibles_urban, select=c("X","Cardiff"))
# mean_susceptibles_cardiff <- subset(mean_susceptibles_urban, select=c("X","Cardiff"))

## 

cases_norwich <- subset(cases_urban, select=c("X","Norwich"))
pop_norwich <- subset(pop_urban, select=c("X","Leeds"))
#births_new <- subset(births_urban, select=c("X","Norwich"))
coordinates_norwich <- subset(coordinates_urban, select=c("X","Norwich"))
susceptibles_norwich <- subset(susceptibles_urban, select=c("X","Norwich"))
# mean_susceptibles_norwich <- subset(mean_susceptibles_urban, select=c("X","Norwich"))

## 
cases_swansea <- subset(cases_urban, select=c("X","Swansea"))
pop_swansea <- subset(pop_urban, select=c("X","Leeds"))
#births_new <- subset(births_urban, select=c("X","Swansea"))
coordinates_swansea <- subset(coordinates_urban, select=c("X","Swansea"))
susceptibles_swansea <- subset(susceptibles_urban, select=c("X","Swansea"))
# mean_susceptibles_swansea <- subset(mean_susceptibles_urban, select=c("X","Swansea"))



cases_exeter <- subset(cases_urban, select=c("X","Exeter"))
pop_exeter <- subset(pop_urban, select=c("X","Exeter"))
#births_new <- subset(births_urban, select=c("X","Exeter"))
coordinates_exeter <- subset(coordinates_urban, select=c("X","Exeter"))
susceptibles_exeter <- subset(susceptibles_urban, select=c("X","Exeter"))
# mean_susceptibles_swansea <- subset(mean_susceptibles_urban, select=c("X","Swansea"))

cases_plymouth <- subset(cases_urban, select=c("X","Plymouth"))
pop_plymouth <- subset(pop_urban, select=c("X","Plymouth"))
#births_new <- subset(births_urban, select=c("X","Plymouth"))
coordinates_plymouth <- subset(coordinates_urban, select=c("X","Plymouth"))
susceptibles_plymouth <- subset(susceptibles_urban, select=c("X","Plymouth"))
# mean_susceptibles_swansea <- subset(mean_susceptibles_urban, select=c("X","Swansea"))

cases_portsmouth <- subset(cases_urban, select=c("X","Portsmouth"))
pop_portsmouth <- subset(pop_urban, select=c("X","Portsmouth"))
#births_new <- subset(births_urban, select=c("X","Portsmouth"))
coordinates_portsmouth <- subset(coordinates_urban, select=c("X","Portsmouth"))
susceptibles_portsmouth <- subset(susceptibles_urban, select=c("X","Portsmouth"))
# mean_susceptibles_swansea <- subset(mean_susceptibles_urban, select=c("X","Swansea"))


##
# density_cities <- as.numeric(read.csv(file=paste(path1,"density_cities.csv",sep=""))$x)


## 
small_cities <- {} # small cities 

dist_to_london_small_cities <- {} # distance to London
dist_to_bmh_small_cities <- {} # distance to Birmingham
dist_to_liv_small_cities <- {} # distance to Liverpool
dist_to_man_small_cities <- {} # distance to Manchester
dist_to_shef_small_cities <- {} # distance to Sheffield
dist_to_leeds_small_cities <- {} # distance to Leeds
dist_to_newcastle_small_cities <- {} # distance to Newcastle
dist_to_bristol_small_cities <- {} # distance to Bristol
dist_to_cardiff_small_cities <- {} # distance to Cardiff
dist_to_norwich_small_cities <- {} # distance to Norwich
dist_to_swansea_small_cities <- {} # distance to Norwich

dist_to_exeter_small_cities <- {} # distance to Norwich
dist_to_plymouth_small_cities <- {} # distance to Norwich
dist_to_portsmouth_small_cities <- {} # distance to Norwich



avg_pop_small_cities <- {}
# density_small_cities <- {}

# for (i in 2:ncol(coordinates_urban)){
for (i in 1:length(names_cities)){


	dist_to_london <- distCosine(coordinates_urban$London, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_bmh <- distCosine(coordinates_urban$Birmingham, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_liv <- distCosine(coordinates_urban$Liverpool, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_man <- distCosine(coordinates_urban$Manchester, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_shef <- distCosine(coordinates_urban$Sheffield, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_leeds <- distCosine(coordinates_urban$Leeds, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_newcastle <- distCosine(coordinates_urban$Newcastle, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_bristol <- distCosine(coordinates_urban$Bristol, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_cardiff <- distCosine(coordinates_urban$Cardiff, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_norwich <- distCosine(coordinates_urban$Norwich, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_swansea <- distCosine(coordinates_urban$Swansea, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000

	dist_to_exeter <- distCosine(coordinates_urban$Exeter, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_plymouth <- distCosine(coordinates_urban$Plymouth, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000
	dist_to_portsmouth <- distCosine(coordinates_urban$Portsmouth, coordinates_urban[,colnames(coordinates_urban)==names_cities[i]])/1000

	ind_include <- include_city(coordinates_urban[,colnames(coordinates_urban)==names_cities[i]], lon_lb, lon_ub, lat_lb, lat_ub)


	### if (ind_include==1 & mean(pop_urban_ref[,colnames(pop_urban_ref)==names_cities[i]])<=pop_bound & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])==0)>=1 & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])>1)>=1 & max(pop_urban_ref[,colnames(pop_urban_ref)==names_cities[i]])<=(pop_bound+10000)) { 

	# if (ind_include==1 & mean(pop_urban[,colnames(pop_urban)==names_cities[i]])<=pop_bound & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])==0)>=1 & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])>1)>=1 & max(pop_urban[,colnames(pop_urban)==names_cities[i]])<=(pop_bound+10000)) { # pop_bound=40000
	if (ind_include==1 & mean(pop_urban[,colnames(pop_urban)==names_cities[i]])<=pop_bound & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])==0)>=1 & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])>1)>=1 & max(pop_urban[,colnames(pop_urban)==names_cities[i]])<=(pop_bound+10000) & names_cities[i]!="Barnsley") { # pop_bound=90000

		small_cities <- c(small_cities, names_cities[i])

		dist_to_london_small_cities <- c(dist_to_london_small_cities, dist_to_london)
		dist_to_bmh_small_cities <- c(dist_to_bmh_small_cities, dist_to_bmh)
		dist_to_liv_small_cities <- c(dist_to_liv_small_cities, dist_to_liv)
		dist_to_man_small_cities <- c(dist_to_man_small_cities, dist_to_man)
		dist_to_shef_small_cities <- c(dist_to_shef_small_cities, dist_to_shef)
		dist_to_leeds_small_cities <- c(dist_to_leeds_small_cities, dist_to_leeds)
		dist_to_newcastle_small_cities <- c(dist_to_newcastle_small_cities, dist_to_newcastle)
		dist_to_bristol_small_cities <- c(dist_to_bristol_small_cities, dist_to_bristol)
		dist_to_cardiff_small_cities <- c(dist_to_cardiff_small_cities, dist_to_cardiff)
		dist_to_norwich_small_cities <- c(dist_to_norwich_small_cities, dist_to_norwich)
		dist_to_swansea_small_cities <- c(dist_to_swansea_small_cities, dist_to_swansea)

		dist_to_exeter_small_cities <- c(dist_to_exeter_small_cities, dist_to_exeter)
		dist_to_plymouth_small_cities <- c(dist_to_plymouth_small_cities, dist_to_plymouth)
		dist_to_portsmouth_small_cities <- c(dist_to_portsmouth_small_cities, dist_to_portsmouth)

		avg_pop_small_cities <- c(avg_pop_small_cities,mean(pop_urban[,colnames(pop_urban)==names_cities[i]]))

		# density_small_cities <- c(density_small_cities, density_cities[i])

		# print(c(dist_to_london, dist_to_bmh))
	}
}



## reorder according to dist_to_london ##
# if (sort_ind==1){
# 	df_temp <- data.frame(x=small_cities, y_1=dist_to_london_small_cities, y_2= dist_to_london_small_cities, z=avg_pop_small_cities)
# 	small_cities <- as.vector(df_temp[order(df_temp$y_1),]$x)
# 	dist_to_london_small_cities <- df_temp[order(df_temp$y_1),]$y_1
# 	dist_to_bmh_small_cities <- df_temp[order(df_temp$y_1),]$y_2
# 	avg_pop_small_cities <- df_temp[order(df_temp$y_1),]$z
# }
# ## reorder according to population size ##
# if (sort_ind==2){
# 	df_temp <- data.frame(x=small_cities,  y_1=dist_to_london_small_cities, y_2= dist_to_london_small_cities, z=avg_pop_small_cities)
# 	small_cities <- as.vector(df_temp[order(df_temp$z),]$x)
# 	dist_to_london_small_cities <- df_temp[order(df_temp$z),]$y_1
# 	dist_to_bmh_small_cities <- df_temp[order(df_temp$z),]$y_2
# 	avg_pop_small_cities <- df_temp[order(df_temp$z),]$z
# }

# small_cities


##


n_small_cities <- length(small_cities)

dist_mat_betweeen_small_cities <- matrix(NA, ncol= n_small_cities, nrow=n_small_cities)

for (i in 1:n_small_cities){
	for (j in 1:n_small_cities){
		
dist_mat_betweeen_small_cities[i,j]  <- distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[i]], coordinates_urban[,colnames(coordinates_urban)==small_cities[j]])/1000
	}

}


##
cases_small_cities <- subset(cases_urban,select=c("X",small_cities))
pop_small_cities <- subset(pop_urban,select=c("X",small_cities))
# births_small_cities <- subset(births_urban,select=c("X",small_cities))
coordinates_small_cities <- subset(coordinates_urban, select=c("X",small_cities))
susceptibles_small_cities <- subset(susceptibles_urban, select=c("X",small_cities))
# mean_susceptibles_small_cities <- as.numeric(subset(mean_susceptibles_urban, select=c("X",small_cities)))

total_Nt_small_cities <- {} # total pop of small cities over time
for (i in 1:nrow(pop_small_cities)){
	total_Nt_small_cities[i] <- sum(pop_small_cities[i,2:ncol(pop_small_cities)])
}

total_St_small_cities <- {} # total susceptibles of small cities over time
for (i in 1:nrow(susceptibles_small_cities)){
	total_St_small_cities[i] <- sum(susceptibles_small_cities[i,2:ncol(susceptibles_small_cities)])
}
#

n_small_cities <- length(small_cities)

t_step <- nrow(cases_small_cities) # time duration for simulation, started from t=0

list_influx_time_data <-  vector("list", n_small_cities) # each element of this list contains (user-defined) influx times of a small city
list_die_time_data <-  vector("list", n_small_cities) # each element of this list contains (user-defined) die out times of a small city
list_n_zero_data <-  vector("list", n_small_cities) 

list_immune_period_data <-  vector("list", n_small_cities) 

list_period_immune_data <-  vector("list", n_small_cities) 

list_cases_data <-  vector("list", n_small_cities) 


for (i in 1:n_small_cities){

	list_cases_data[[i]] <- as.numeric(cases_small_cities[,i+1])

	t_influx_data <- define_influx(cases_small_cities[,i+1], n_0=n_0, n_1=n_1) # return a vector indicating the user-defined influx time
	list_influx_time_data[[i]] <- t_influx_data

	t_die_data <- define_die(cases_small_cities[,i+1], n_0=n_0, n_1=n_1) # return a vector indicating the user-defined die time
	list_die_time_data[[i]] <- t_die_data

	list_n_zero_data[[i]] <- (t_step - length(which(cases_small_cities[,i+1]>0)))

	##
	t_influx_data_nna <- t_influx_data[is.na(t_influx_data)==FALSE]
	t_die_data_nna <- t_die_data[is.na(t_die_data)==FALSE]

	if((t_die_data_nna[1]-t_influx_data_nna[1])<2) t_die_data_nna <- t_die_data_nna[-1]

# print(min(t_die_data_nna[1]-t_influx_data_nna[1],na.rm=T))
	n_min <- min(length(t_influx_data_nna), length(t_die_data_nna))

	list_immune_period_data[[i]] <- t_die_data_nna[1:n_min] -  t_influx_data_nna[1:n_min]

	##
	period_immune <- {}
	for (j in 1:n_min){
		period_immune <- c(period_immune, (t_influx_data_nna[j]+1):(t_die_data_nna[j]-1))
	}
	list_period_immune_data[[i]]<- period_immune

	# #


	#
}

#########
## Simulation ##

##
cases_london_vector <- as.numeric(cases_london[,2])
cases_bmh_vector <- as.numeric(cases_bmh[,2])
cases_liv_vector <- as.numeric(cases_liv[,2])
cases_man_vector <- as.numeric(cases_man[,2])
cases_shef_vector <- as.numeric(cases_shef[,2])
cases_leeds_vector <- as.numeric(cases_leeds[,2])
cases_newcastle_vector <- as.numeric(cases_newcastle[,2])
cases_bristol_vector <- as.numeric(cases_bristol[,2])
cases_cardiff_vector <- as.numeric(cases_cardiff[,2])
cases_norwich_vector <- as.numeric(cases_norwich[,2])
cases_swansea_vector <- as.numeric(cases_swansea[,2])

cases_exeter_vector <- as.numeric(cases_exeter[,2])
cases_plymouth_vector <- as.numeric(cases_plymouth[,2])
cases_portsmouth_vector <- as.numeric(cases_portsmouth[,2])


pop_small_cities_mat <- matrix(as.matrix(pop_small_cities),dimnames=NULL, ncol=ncol(pop_small_cities))
susceptibles_small_cities_mat <- matrix(as.matrix(susceptibles_small_cities),dimnames=NULL, ncol=ncol(susceptibles_small_cities))
cases_small_cities_mat <- matrix(as.matrix(cases_small_cities),dimnames=NULL, ncol=ncol(cases_small_cities))

other_cities <- names_cities[!names_cities%in%c(core_cities,small_cities)]
cases_other_cities <- subset(cases_urban,select=c("X",other_cities))
cases_other_cities_mat <- matrix(as.matrix(cases_other_cities),dimnames=NULL, ncol=ncol(cases_other_cities))


n_other_cities <- length(other_cities)
dist_mat_to_other_cities <- matrix(NA, ncol= n_other_cities, nrow=n_small_cities) # distacne to other cities to each small city (row=small city, col=other cities)
for (i in 1:n_small_cities){
	for (j in 1:n_other_cities){		
	dist_mat_to_other_cities[i,j]  <- distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[i]], coordinates_urban[,colnames(coordinates_urban)==other_cities[j]])/1000
	}
}

##

J_iter <- sample(1:length(a_mc),n_sim)

# write.csv(J_iter,file=paste(path4,"J_iter.csv",sep=""),row.names=FALSE)	 


for (j in 1:n_sim){

	# write.csv(j,file=paste(path4,"j_sim",sep=""),row.names=FALSE)	 
	print(j)
	# beta_coupling <- mean(beta_coupling_mc)
	# alpha_1 <- mean(alpha_1_mc)
	# alpha_2 <- mean(alpha_2_mc)
	# rho <- mean(rho_mc)
	# eta <- mean(eta_mc)
	# b <- mean(b_mc)
	# tau <- mean(tau_mc)
	# psi <- mean(psi_mc)
	# a <- mean(a_mc)

	j_iter <- J_iter[j]
	beta_coupling <- beta_coupling_mc[j_iter]
	alpha_1 <- alpha_1_mc[j_iter]
	alpha_2 <- alpha_2_mc[j_iter]
	rho <- rho_mc[j_iter]
	eta <- eta_mc[j_iter]
	b <- b_mc[j_iter]
	tau <- tau_mc[j_iter]
	psi <- psi_mc[j_iter]
	a <- a_mc[j_iter]

	# beta_coupling <- beta_coupling_mc[sample(1:length(beta_coupling_mc),1)]
	# alpha_1 <- alpha_1_mc[sample(1:length(alpha_1_mc),1)]
	# alpha_2 <- alpha_2_mc[sample(1:length(alpha_2_mc),1)]
	# rho <- rho_mc[sample(1:length(rho_mc),1)]
	# eta <- eta_mc[sample(1:length(eta_mc),1)]
	# b <- b_mc[sample(1:length(b_mc),1)]
	# tau <- tau_mc[sample(1:length(tau_mc),1)]
	# psi <- psi_mc[sample(1:length(psi_mc),1)]
	# a <- a_mc[sample(1:length(a_mc),1)]


	para <- c(beta_coupling, alpha_1, alpha_2, rho, eta, b, tau, psi, a)


	if (j==1) epi_summary <- t(mapply(simulate_summary_single_city,1:n_small_cities,  avg_pop_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities,  dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities,  dist_to_cardiff_small_cities,  dist_to_norwich_small_cities,  dist_to_swansea_small_cities,dist_to_exeter_small_cities, dist_to_plymouth_small_cities,dist_to_portsmouth_small_cities, MoreArgs=list(pop_small_cities_mat, susceptibles_small_cities_mat, cases_london_vector, cases_bmh_vector, cases_liv_vector, cases_man_vector, cases_shef_vector, cases_leeds_vector,  cases_newcastle_vector,  cases_bristol_vector, cases_cardiff_vector,cases_norwich_vector,cases_swansea_vector, cases_exeter_vector,cases_plymouth_vector,cases_portsmouth_vector, para, t_step, dist_mat_betweeen_small_cities,cases_small_cities_mat,  dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities))) # every row corresponds to an output of a city

	if(j>1){

		temp_summary <-  t(mapply(simulate_summary_single_city,1:n_small_cities,  avg_pop_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities,  dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities,  dist_to_cardiff_small_cities,  dist_to_norwich_small_cities,  dist_to_swansea_small_cities, dist_to_exeter_small_cities,dist_to_plymouth_small_cities,dist_to_portsmouth_small_cities, MoreArgs=list(pop_small_cities_mat, susceptibles_small_cities_mat, cases_london_vector, cases_bmh_vector, cases_liv_vector, cases_man_vector, cases_shef_vector, cases_leeds_vector,  cases_newcastle_vector,  cases_bristol_vector, cases_cardiff_vector,cases_norwich_vector,cases_swansea_vector, cases_exeter_vector,cases_plymouth_vector,cases_portsmouth_vector, para, t_step, dist_mat_betweeen_small_cities,cases_small_cities_mat,  dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))
		epi_summary <- rbind(epi_summary,temp_summary)
	}



}

sim_forward <- as.data.frame(epi_summary)
# colnames(sim_forward)<- c("zero","count","mean_duration", "sd_duration","mean_abs","mean_immune","sd_immune", "mean_delta_london", "dispersion_immune")
colnames(sim_forward)<- c("zero","count","mean_duration", "sd_duration","mean_immune","sd_immune")



##
# save.image(file=paste(path4,"multi_city_sim_forward.RData",sep=""))



require(parallel)
require(geosphere)
require(ggplot2)
require(Rcpp)
require(truncdist)
require(synchrony)
require(TSclust)
require(proxy)
require(gridExtra)



### Set the plot level on x-axis ###
small_cities_name <- small_cities




count_class <- {}


m_class_cities <- 40
min_pop <- 0
class_cities <- seq(min_pop,pop_bound, trunc((pop_bound-min_pop)/m_class_cities))


for (i in 1:m_class_cities){
count_class[i] <- length(which(avg_pop_small_cities>class_cities[i] & avg_pop_small_cities<=class_cities[i+1]))
}



sim_forward$small_cities <- rep(small_cities,n_sim)

sim_forward$avg_pop_small_cities <- rep(avg_pop_small_cities,n_sim)

# sim_forward$pop_level <- rep(pop_level,n_sim)

# mean_scep_small_cities <- as.numeric(mean_susceptibles_small_cities[-1])
# sim_forward$mean_scep_small_cities <- rep(mean_scep_small_cities,n_sim)

sim_forward$dist_to_london <- rep(dist_to_london_small_cities,n_sim)



## the summaries from data ##
count_coupling_data <- sapply(list_influx_time_data, count_coupling)
mean_duration_coupling_data <- sapply(list_influx_time_data, mean_duration_coupling)
sd_duration_coupling_data <- sapply(list_influx_time_data, sd_duration_coupling)
mean_abs_coupling_data <- sapply(list_influx_time_data, mean_abs_coupling)
mean_immune_coupling_data <- sapply(list_immune_period_data, mean)
sd_immune_coupling_data <- sapply(list_immune_period_data, sd)
zero_coupling_data <- unlist(list_n_zero_data)



data_summary <- data.frame(small_cities=small_cities,dist_to_london=dist_to_london_small_cities, avg_pop_small_cities=avg_pop_small_cities, zero= zero_coupling_data, count=count_coupling_data, mean_duration=mean_duration_coupling_data, sd_duration=sd_duration_coupling_data, mean_abs=mean_abs_coupling_data, mean_immune=mean_immune_coupling_data, sd_immune=sd_immune_coupling_data)







#####

mean_abs_q_1 <- rep(NA, m_class_cities)
mean_abs_q_3 <- rep(NA, m_class_cities)
mean_abs_median <- rep(NA, m_class_cities)

small_cities_name <- small_cities


for (i in 1:(m_class_cities)){


	mean_abs_q_1[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_abs,probs=c(0.025), na.rm=T))
	mean_abs_q_3[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_abs,probs=c(0.975),na.rm=T))
	mean_abs_median[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_abs,probs=c(0.5),na.rm=T))	




}

mean_abs_x_polygon <- c(c(1:m_class_cities), rev(1:m_class_cities))
mean_abs_y_polygon <- c(mean_abs_q_1, rev(mean_abs_q_3))

mean_abs_data <- {}
for (i in 1:(m_class_cities)){

		mean_abs_data[i] <- as.numeric(quantile(subset(data_summary, data_summary$avg_pop_small_cities>class_cities[i] & data_summary$avg_pop_small_cities<=class_cities[i+1])$mean_abs,probs=c(0.5), na.rm=T))



}



##


#####

mean_duration_q_1 <- rep(NA, m_class_cities)
mean_duration_q_3 <- rep(NA, m_class_cities)
mean_duration_median <- rep(NA, m_class_cities)

small_cities_name <- small_cities


for (i in 1:(m_class_cities)){


	mean_duration_q_1[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_duration,probs=c(0.025), na.rm=T))
	mean_duration_q_3[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_duration,probs=c(0.975),na.rm=T))
	mean_duration_median[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_duration,probs=c(0.5),na.rm=T))		

}

mean_duration_x_polygon <- c(c(1:m_class_cities), rev(1:m_class_cities))
mean_duration_y_polygon <- c(mean_duration_q_1, rev(mean_duration_q_3))

mean_duration_data <- {}
for (i in 1:(m_class_cities)){

	mean_duration_data[i] <- as.numeric(quantile(subset(data_summary, data_summary$avg_pop_small_cities>class_cities[i] & data_summary$avg_pop_small_cities<=class_cities[i+1])$mean_duration,probs=c(0.5), na.rm=T))

}



##


#

sd_duration_q_1 <- rep(NA, m_class_cities)
sd_duration_q_3 <- rep(NA, m_class_cities)
sd_duration_median <- rep(NA, m_class_cities)

small_cities_name <- small_cities


for (i in 1:(m_class_cities)){


	sd_duration_q_1[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$sd_duration,probs=c(0.025), na.rm=T))
	sd_duration_q_3[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$sd_duration,probs=c(0.975),na.rm=T))
	sd_duration_median[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$sd_duration,probs=c(0.5),na.rm=T))		
	# sd_duration_median[i] <- as.numeric(mean(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$sd_duration,na.rm=T))		



}

sd_duration_x_polygon <- c(c(1:m_class_cities), rev(1:m_class_cities))
sd_duration_y_polygon <- c(sd_duration_q_1, rev(sd_duration_q_3))

sd_duration_data <- {}
for (i in 1:(m_class_cities)){

	sd_duration_data[i] <- as.numeric(quantile(subset(data_summary, data_summary$avg_pop_small_cities>class_cities[i] & data_summary$avg_pop_small_cities<=class_cities[i+1])$sd_duration,probs=c(0.5), na.rm=T))


}

##

count_q_1 <- rep(NA, m_class_cities)
count_q_3 <- rep(NA, m_class_cities)
count_median <- rep(NA, m_class_cities)

for (i in 1:m_class_cities){

	count_q_1[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$count,probs=c(0.025), na.rm=T))
	count_q_3[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$count,probs=c(0.975),na.rm=T))
	count_median[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$count,probs=c(0.5),na.rm=T))
	

}

count_x_polygon <- c(c(1:m_class_cities), rev(1:m_class_cities))
count_y_polygon <- c(count_q_1, rev(count_q_3))


count_data <- {}
for (i in 1:m_class_cities){

		count_data[i] <- as.numeric(quantile(subset(data_summary, data_summary$avg_pop_small_cities>class_cities[i] & data_summary$avg_pop_small_cities<=class_cities[i+1])$count,probs=c(0.5), na.rm=T))


}



#

zero_q_1 <- rep(NA, m_class_cities)
zero_q_3 <- rep(NA, m_class_cities)
zero_median <- rep(NA, m_class_cities)

for (i in 1:m_class_cities){


	zero_q_1[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$zero,probs=c(0.025), na.rm=T))
	zero_q_3[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$zero,probs=c(0.975),na.rm=T))
	zero_median[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$zero,probs=c(0.5),na.rm=T))
	
	


}

zero_x_polygon <- c(c(1:m_class_cities), rev(1:m_class_cities))
zero_y_polygon <- c(zero_q_1, rev(zero_q_3))


zero_data <- {}
for (i in 1:m_class_cities){

	zero_data[i] <- as.numeric(quantile(subset(data_summary, data_summary$avg_pop_small_cities>class_cities[i] & data_summary$avg_pop_small_cities<=class_cities[i+1])$zero,probs=c(0.5), na.rm=T))
	

}


##

mean_immune_q_1 <- rep(NA, m_class_cities)
mean_immune_q_3 <- rep(NA, m_class_cities)
mean_immune_median <- rep(NA, m_class_cities)

small_cities_name <- small_cities


for (i in 1:(m_class_cities)){


	mean_immune_q_1[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_immune,probs=c(0.025), na.rm=T))
	mean_immune_q_3[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_immune,probs=c(0.975),na.rm=T))
	mean_immune_median[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$mean_immune,probs=c(0.5),na.rm=T))		




}

mean_immune_x_polygon <- c(c(1:m_class_cities), rev(1:m_class_cities))
mean_immune_y_polygon <- c(mean_immune_q_1, rev(mean_immune_q_3))

mean_immune_data <- {}
for (i in 1:(m_class_cities)){

	mean_immune_data[i] <- as.numeric(quantile(subset(data_summary, data_summary$avg_pop_small_cities>class_cities[i] & data_summary$avg_pop_small_cities<=class_cities[i+1])$mean_immune,probs=c(0.5), na.rm=T))



}

##

sd_immune_q_1 <- rep(NA, m_class_cities)
sd_immune_q_3 <- rep(NA, m_class_cities)
sd_immune_median <- rep(NA, m_class_cities)

small_cities_name <- small_cities


for (i in 1:(m_class_cities)){


	sd_immune_q_1[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$sd_immune,probs=c(0.025), na.rm=T))
	sd_immune_q_3[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$sd_immune,probs=c(0.975),na.rm=T))
	sd_immune_median[i] <- as.numeric(quantile(subset(sim_forward, sim_forward$avg_pop_small_cities>class_cities[i] & sim_forward$avg_pop_small_cities<=class_cities[i+1])$sd_immune,probs=c(0.5),na.rm=T))		




}

sd_immune_x_polygon <- c(c(1:m_class_cities), rev(1:m_class_cities))
sd_immune_y_polygon <- c(sd_immune_q_1, rev(sd_immune_q_3))

sd_immune_data <- {}
for (i in 1:(m_class_cities)){

	sd_immune_data[i] <- as.numeric(quantile(subset(data_summary, data_summary$avg_pop_small_cities>class_cities[i] & data_summary$avg_pop_small_cities<=class_cities[i+1])$sd_immune,probs=c(0.5), na.rm=T))



}

##




layout(matrix(1:6, nrow=3, byrow=TRUE))
par ( mar=c ( 2,3,1,1),oma = c(1.4, 1.4, 0, 0) ,mgp=c(1.5, 0.5, 0))

plot(0, type="l",col=gray(0.9),lwd=0.2, xlim=c(1,m_class_cities), ylim=c(0,30),ylab="Epidemic Cycles (mean)", las=1, xlab="", cex.axis=0.9, cex.lab=0.9)
polygon(mean_duration_x_polygon, mean_duration_y_polygon, col=gray(0.9), border=NA)
lines(1:m_class_cities,mean_duration_data,type="o", pch=20, lty=3, cex=0.9, col="black")
lines(1:m_class_cities,mean_duration_median ,type="o", pch=20, lty=3, cex=0.9, col="red")



plot(0, type="l",col=gray(0.9),lwd=0.2, xlim=c(1,m_class_cities), ylim=c(0,40),ylab="Epidemic Cycles (SD)", las=1, xlab="", cex.axis=0.9, cex.lab=0.9)
polygon(sd_duration_x_polygon, sd_duration_y_polygon, col=gray(0.9), border=NA)
lines(1:m_class_cities,sd_duration_data,type="o", pch=20, lty=3, cex=0.9, col="black")
lines(1:m_class_cities,sd_duration_median ,type="o", pch=20, lty=3, cex=0.9, col="red")


plot(0, type="l",col=gray(0.9),lwd=0.2, xlim=c(1,m_class_cities), ylim=c(0,100),ylab="Number of Re-introductions", las=1, xlab="", cex.axis=0.9, cex.lab=0.9)
polygon(count_x_polygon, count_y_polygon, col=gray(0.9), border=NA)
lines(1:m_class_cities,count_data,type="o", pch=20, lty=3, cex=0.9, col="black")
lines(1:m_class_cities,count_median ,type="o", pch=20, lty=3, cex=0.9, col="red")
#points((1:m_class_cities)[which(data_summary$count<=count_limit)], data_summary$count[which(data_summary$count<=count_limit)], col="red" )


plot(0, type="l",col=gray(0.9),lwd=0.2, xlim=c(1,m_class_cities), ylim=c(0,500),ylab="Number of Zeros", las=1, xlab="", cex.axis=0.9, cex.lab=0.9)
polygon(zero_x_polygon, zero_y_polygon, col=gray(0.9), border=NA)
lines(1:m_class_cities,zero_data,type="o", pch=20, lty=3, cex=0.9, col="black")
lines(1:m_class_cities,zero_median ,type="o", pch=20, lty=3, cex=0.9, col="red")
#points((1:m_class_cities)[which(data_summary$zero<=zero_limit)], data_summary$zero[which(data_summary$zero<=zero_limit)], col="red" )

plot(0,  type="l",col=gray(0.9),lwd=0.2, xlim=c(1,m_class_cities),ylim=c(0,30),ylab="Epidemic Durations (Mean)", las=1, xlab="", cex.axis=0.9, cex.lab=0.9)
polygon(mean_immune_x_polygon, mean_immune_y_polygon, col=gray(0.9), border=NA)
lines(1:m_class_cities,mean_immune_data,type="o", pch=20, lty=3, cex=0.9, col="black")
lines(1:m_class_cities,mean_immune_median ,type="o", pch=20, lty=3, cex=0.9, col="red")

plot(0, type="l",col=gray(0.9),lwd=0.2, xlim=c(1,m_class_cities),ylim=c(0,40),ylab="Epidemic Cycles (SD)", las=1, xlab="", cex.axis=0.9, cex.lab=0.9)
polygon(sd_immune_x_polygon, sd_immune_y_polygon, col=gray(0.9), border=NA)
lines(1:m_class_cities,sd_immune_data,type="o", pch=20, lty=3, cex=0.9, col="black")
lines(1:m_class_cities,sd_immune_median ,type="o", pch=20, lty=3, cex=0.9, col="red")


mtext("Average Population level", side=1,outer=TRUE, col="black", cex=0.9)

