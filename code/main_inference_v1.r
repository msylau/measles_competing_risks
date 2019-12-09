## inference code for pre-vac data ##



require(geosphere)
require(truncdist)
require(Rcpp)
require(RcppParallel)
require(parallel)
require(MASS)
require(RcppArmadillo)
require(future.apply)


n_core = 7

# set.seed(1)

path1<- "/Users/slau24/Google Drive/git projects/measles_competing_risks/data/formatted/prevac/" # data input
path2<- "/Users/slau24/Google Drive/git projects/measles_competing_risks/code/" # script directory
path3<- "x" # mcmc samples output



source(paste(path2,"main_inference_src_functions_v1.r",sep="")) # src functions


##
ind_rand_small_cities <- 0 # 1= randomly choose a subset of small cities specified later
n_rand_small_cities <- 534
## field data and extraction ##


##

subset_data_row_start <- 1
subset_data_row_end <- 534 # number of weeks to analyze
pop_bound <- 90000 # upper bound of median population size of defined small cities 

# Cardiff has avg pop 120000

# radius <- 100 # radius of the circle (km)

# n_0 <- 2 # number of weeks of 0 before required for defining an re-initiations; see also function define_influx
# n_1 <- 2 # number of weeks of 1 ahead ...

#
lon_lb <- -6.0
lon_ub <- 1.8

lat_lb <- 50
lat_ub <- 55.8


include_city <- function(coordinates, lon_lb, lon_ub, lat_lb, lat_ub){ # determine if to include a city based on a region defined
	1*(coordinates[1]>=lon_lb & coordinates[1]<=lon_ub & coordinates[2]>=lat_lb & coordinates[2]<=lat_ub)
}


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

cases_newcastle <- subset(cases_urban, select=c("X","Newcastle"))
pop_newcastle <- subset(pop_urban, select=c("X","Newcastle"))
#births_new <- subset(births_urban, select=c("X","Newcastle"))
coordinates_newcastle <- subset(coordinates_urban, select=c("X","Newcastle"))
susceptibles_newcastle <- subset(susceptibles_urban, select=c("X","Newcastle"))
# mean_susceptibles_newcastle <- subset(mean_susceptibles_urban, select=c("X","Newcastle"))

## 

cases_bristol <- subset(cases_urban, select=c("X","Bristol"))
pop_bristol <- subset(pop_urban, select=c("X","Bristol"))
#births_new <- subset(births_urban, select=c("X","Bristol"))
coordinates_bristol <- subset(coordinates_urban, select=c("X","Bristol"))
susceptibles_bristol <- subset(susceptibles_urban, select=c("X","Bristol"))
# mean_susceptibles_bristol <- subset(mean_susceptibles_urban, select=c("X","Bristol"))

## 

cases_cardiff <- subset(cases_urban, select=c("X","Cardiff"))
pop_cardiff <- subset(pop_urban, select=c("X","Cardiff"))
#births_new <- subset(births_urban, select=c("X","Cardiff"))
coordinates_cardiff <- subset(coordinates_urban, select=c("X","Cardiff"))
susceptibles_cardiff <- subset(susceptibles_urban, select=c("X","Cardiff"))
# mean_susceptibles_cardiff <- subset(mean_susceptibles_urban, select=c("X","Cardiff"))

## 

cases_norwich <- subset(cases_urban, select=c("X","Norwich"))
pop_norwich <- subset(pop_urban, select=c("X","Norwich"))
#births_new <- subset(births_urban, select=c("X","Norwich"))
coordinates_norwich <- subset(coordinates_urban, select=c("X","Norwich"))
susceptibles_norwich <- subset(susceptibles_urban, select=c("X","Norwich"))
# mean_susceptibles_norwich <- subset(mean_susceptibles_urban, select=c("X","Norwich"))

## 
cases_swansea <- subset(cases_urban, select=c("X","Swansea"))
pop_swansea <- subset(pop_urban, select=c("X","Swansea"))
#births_new <- subset(births_urban, select=c("X","Swansea"))
coordinates_swansea <- subset(coordinates_urban, select=c("X","Swansea"))
susceptibles_swansea <- subset(susceptibles_urban, select=c("X","Swansea"))
# mean_susceptibles_swansea <- subset(mean_susceptibles_urban, select=c("X","Swansea"))


#  [1] "Birmingham" "Bradford"   "Bristol"    "Coventry"   "Croydon"    "Harrow"     "Hull"       "Leeds"      "Leicester"  "Liverpool"  "London"     "Manchester"
# [13] "Newcastle"  "Nottingham" "Plymouth"   "Portsmouth" "Sheffield"  "Stoke"      "Cardiff"   

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

###	if ( ind_include==1 & mean(pop_urban[,colnames(pop_urban)==names_cities[i]])<=pop_bound & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])==0)>=1 & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])>1)>=1) {

	# if (ind_include==1 & mean(pop_urban[,colnames(pop_urban)==names_cities[i]])<=pop_bound & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])==0)>=1 & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])>1)>=1 & max(pop_urban[,colnames(pop_urban)==names_cities[i]])<=(pop_bound+10000)) { # pop_bound=40000
	if (ind_include==1 & mean(pop_urban[,colnames(pop_urban)==names_cities[i]])<=pop_bound & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])==0)>=1 & sum((cases_urban[,colnames(cases_urban)==names_cities[i]])>1)>=1 & max(pop_urban[,colnames(pop_urban)==names_cities[i]])<=(pop_bound+10000) & names_cities[i]!="Barnsley") { # pop_bound=90000

# temp <- sample(0:1,1,prob=c(0.95,0.05))
# if(temp==1){
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
# }

		# print(c(dist_to_london, dist_to_bmh))
	}
}


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

list_influx_time_data <-  vector("list", n_small_cities) # each element of this list contains (user-defined) influx times of a small city
list_die_time_data <-  vector("list", n_small_cities) # each element of this list contains (user-defined) die out times of a small city

list_influx_time_data_unique <-  vector("list", n_small_cities) # (without NA) each element of this list contains (user-defined) influx times of a small city

# period_immune_list <- vector("list", n_small_cities)
 
for (i in 1:n_small_cities){
	t_influx_data <- define_influx(cases_small_cities[,i+1], n_0=n_0, n_1=n_1) # return a vector indicating the user-defined influx time
	list_influx_time_data[[i]] <- t_influx_data

	t_die_data <- define_die(cases_small_cities[,i+1], n_0=n_0, n_1=n_1) # return a vector indicating the user-defined die time
	list_die_time_data[[i]] <- t_die_data

	print(length(unique(list_influx_time_data[[i]]))- length(unique(list_die_time_data[[i]])))

	list_influx_time_data_unique[[i]] <- t_influx_data[which(is.na(t_influx_data)==FALSE)]


}


list_influx_time_data_with_ind_unique <- vector("list", n_small_cities)
for (i in 1:n_small_cities){
	list_influx_time_data_with_ind_unique[[i]] <- c(list_influx_time_data_unique[[i]], i)
}

list_all_time_data_with_ind <- vector("list", n_small_cities)
for (i in 1:n_small_cities){
	list_all_time_data_with_ind[[i]] <- c(list_influx_time_data[[i]], list_die_time_data[[i]], i)
}

# list_all_time_data_with_ind <- matrix(NA, nrow=n_small_cities, ncol=2*length(list_influx_time_data[[1]])+1)
# for (i in 1:n_small_cities){
# 	list_all_time_data_with_ind[i,] <- c(list_influx_time_data[[i]], list_die_time_data[[i]], i)
# }




##
cases_london_vector <- as.numeric(cases_london[,2])
# cases_london_vector <- rep(mean(cases_london_vector),length(cases_london_vector))
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



## MCMC ##

## Initialization ##


beta_coupling <-  0.000035 # universal coupling rate
alpha_1 <- 0.43 # power raised for imported infection
alpha_2 <- 0.58 # power, if used, raised for population size at city importing to
rho <- 0.85 # power raised for distance
eta <- 0.036 # power raised for number of susceptibles
b <- 0.4811028
tau <- 0.008 # background rate of infection
psi <- 2.912044
a <- 0.02988347


tau_2 <- tau

c <- 0.001
d <- 0.001

para <- c(beta_coupling, alpha_1, alpha_2, rho, eta, b, tau, psi, a, c, d)

t_step <- nrow(cases_small_cities) # time duration for simulation, started from t=0



para_current <- para

norm_const_for_beta <- 1



for (i in 1:100){

print(c(i,log_lh_single(list_all_time_data_with_ind[[i]], para_current, t_step, cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat,  dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

# print(c(i,log_lh_single_R(list_all_time_data_with_ind[[i]], para_current, t_step, cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta,cases_small_cities_mat,   cases_other_cities_mat, n_small_cities, n_other_cities,coordinates_urban, other_cities, small_cities)))

}



start_time <- Sys.time()

# log_lh_current <- sum(sapply(list_all_time_data_with_ind, log_lh_single,  para_current, t_step, cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities))

log_lh_current = sum(unlist(mclapply(list_all_time_data_with_ind, log_lh_single,  para_current, t_step, cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))


end_time <- Sys.time()

end_time - start_time


# log_lh_current <- sum(sapply(list_all_time_data_with_ind, log_lh_single_R,  para_current, t_step, cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta,cases_small_cities_mat,   cases_other_cities_mat, n_small_cities, n_other_cities,coordinates_urban, other_cities, small_cities))

#####



print(log_lh_current)

write.csv(log_lh_current,file=paste(path3,"lh_initial.csv",sep=""),row.names=FALSE)	 
write.csv(para,file=paste(path3,"para_initial.csv",sep=""),row.names=FALSE)	 

## M-H sampling ##

n_iter <- 500000 # number of iterations

# para_current_mat <- matrix(NA, nrow=n_iter,ncol=length(para))


for (i in 1:n_iter){


	mh_beta_rho_alphas_eta_out <- mh_beta_rho_alphas_eta (para_current[1:5], log_lh_current, para_current ,t_step,cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,   list_all_time_data_with_ind, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)
	para_current[1] <- mh_beta_rho_alphas_eta_out[1]
	para_current[2] <- mh_beta_rho_alphas_eta_out[2]
	para_current[3] <- mh_beta_rho_alphas_eta_out[3]
	para_current[4] <- mh_beta_rho_alphas_eta_out[4]
	para_current[5] <- mh_beta_rho_alphas_eta_out[5]
	log_lh_current <- mh_beta_rho_alphas_eta_out[6]


	mh_b_a_out <- mh_b_a(c(para_current[6],para_current[9]), log_lh_current, para_current ,t_step,cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,   list_all_time_data_with_ind, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)
	para_current[6] <- mh_b_a_out[1]
	para_current[9] <- mh_b_a_out[2]
	log_lh_current <- mh_b_a_out[3]



	mh_psi_out <- mh_psi (para_current[8], log_lh_current, para_current ,t_step, cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,   list_all_time_data_with_ind, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)
	para_current[8] <- mh_psi_out[1]
	log_lh_current <- mh_psi_out[2]


	mh_tau_out <- mh_tau (para_current[7], log_lh_current, para_current ,t_step, cases_london_vector, cases_bmh_vector,  cases_liv_vector,  cases_man_vector,cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector, cases_norwich_vector, cases_swansea_vector,cases_exeter_vector, cases_plymouth_vector, cases_portsmouth_vector,  pop_small_cities_mat, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  list_all_time_data_with_ind, susceptibles_small_cities_mat, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)
	para_current[7] <- mh_tau_out[1]
	log_lh_current <- mh_tau_out[2]


	

	write.table(para_current[2],file=paste(path3,"alpha_1_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)
	write.table(para_current[3],file=paste(path3,"alpha_2_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 	 
	write.table(para_current[5],file=paste(path3,"eta_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 
	write.table(para_current[6],file=paste(path3,"b_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 	 
	write.table(para_current[9],file=paste(path3,"a_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 
	write.table(para_current[1],file=paste(path3,"beta_coupling_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 
	write.table(para_current[4],file=paste(path3,"rho_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	
	write.table(para_current[8],file=paste(path3,"psi_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 
	write.table(para_current[7],file=paste(path3,"tau_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 



	write.table(para_current[10],file=paste(path3,"c_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 	 
 	write.table(para_current[11],file=paste(path3,"d_mc.csv",sep=""),row.names=FALSE,col.names=FALSE,append=T)	 	 




}



