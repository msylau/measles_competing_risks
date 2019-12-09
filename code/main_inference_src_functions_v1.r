
## function defining the influx time in field data ##

define_influx <- function(cases, n_0, n_1){
# n_0= number of 0 on the left required, n_1=number of 1 on the right required

	t_influx <- rep(NA, length(cases))

	for (i in 1:(length(cases)-1)){

		if(cases[i]==0 & cases[i+1]>0 ) t_influx[i] <- i-1

	}

	t_influx
}

## function defining the die time in field data ##

define_die <- function(cases, n_0, n_1){
# n_0= number of 0 on the left required, n_1=number of 1 on the right required

	t_die <- rep(NA, length(cases))

	for (i in 1:(length(cases)-1)){

		if(cases[i]>0 & cases[i+1]==0 ) t_die[i] <- i # i+1-1

	}

	t_die
}



## Compute the log-likelihood of beta_coupling given other parameters ##
lh_beta_coupling <- function(beta_coupling, list_all_time_with_ind, para_remain ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(beta_coupling,para_remain)

	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}



## Compute the log-likelihood of alpha_1 given other parameters ##
lh_alpha_1 <- function(alpha_1, list_all_time_with_ind, para_remain ,t_step,  cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(para_remain[1], alpha_1,para_remain[2:9])

	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities,susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}


## Compute the log-likelihood of alpha_2 given other parameters ##
lh_alpha_2 <- function(alpha_2, list_all_time_with_ind, para_remain ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(para_remain[1:2], alpha_2, para_remain[3:9])

	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}





## Compute the log-likelihood of rho given other parameters ##
lh_rho <- function(rho, list_all_time_with_ind, para_remain ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(para_remain[1:3], rho,para_remain[4:9])

	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities,susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}


## Compute the log-likelihood of eta given other parameters ##
lh_eta <- function(eta, list_all_time_with_ind, para_remain ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(para_remain[1:4], eta, para_remain[5:9])

	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}

## Compute the log-likelihood of b given other parameters ##
lh_b <- function(b, list_all_time_with_ind, para_remain ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(para_remain[1:5], b, para_remain[6:9])
	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}

## Compute the log-likelihood of tau given other parameters ##
lh_tau <- function(tau, list_all_time_with_ind, para_remain ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(para_remain[1:6], tau, para_remain[7:9])
	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}


## Compute the log-likelihood of psi given other parameters ##
lh_psi <- function(psi, list_all_time_with_ind, para_remain ,t_step,  cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, susceptibles_small_cities, avg_pop_small_cities){ 

	para <- c(para_remain[1:7], psi, para_remain[8:9])
	# list_all_time_with_ind <- vector("list", n_small_cities)
	# for (i in 1:n_small_cities){
	# 	list_all_time_with_ind[[i]] <- c(list_all_time[[i]], i)
	# }

	sum(sapply(list_all_time_with_ind, log_lh_single,  para, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,susceptibles_small_cities, avg_pop_small_cities))

	#log_lh_all_single (c(list_all_time[[1]],1), para, t_step, cases_london, pop_small_cities_expanded, dist_small_cities)

}





## Joint M-H sampling of beta and rho and alpha_1 and alpha_2 and eta ##

mh_beta_rho_alphas_eta <- function(beta_rho_alphas_eta_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1, -1, -1, -1)
 	while(y[1]<=0 | y[2]<=0 | y[3]<0 | y[4]<0| y[5]<0){
	y <-  mvrnorm(1,mu=beta_rho_alphas_eta_current,Sigma=0.2*matrix(c(1e-10,0,0,0,0, 0,1e-3,0,0,0, 0,0,1e-4,0,0, 0,0,0,1e-4,0, 0,0,0,0,1e-4),byrow=TRUE,ncol=5,nrow=5))
	# y <-  mvrnorm(1,mu=beta_rho_alphas_eta_current,Sigma=0.2*matrix(c(1e-10,0,0,0,0, 0,1e-3,0,0,0, 0,0,1e-4,0,0, 0,0,0,1e-4,0, 0,0,0,0,1e-4),byrow=TRUE,ncol=5,nrow=5))

	}

	para_next <- c(y[1:5], para_current[6:11])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind, 1,log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		beta_rho_alphas_eta_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		beta_rho_alphas_eta_next <- beta_rho_alphas_eta_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(beta_rho_alphas_eta_next, log_lh_current)
}

## Joint M-H sampling of beta and rho and alpha_1 and alpha_2 ##

mh_beta_rho_alphas <- function(beta_rho_alphas_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1, -1, -1)
 	while(y[1]<=0 | y[2]<=0 | y[3]<0 | y[4]<0){
	y <-  mvrnorm(1,mu=beta_rho_alphas_current,Sigma=0.2*matrix(c(4e-10,0,0,0, 0,1e-4,0,0, 0,0,1e-4,0, 0,0,0,1e-3),byrow=TRUE,ncol=4,nrow=4))
	}

	para_next <- c(y[1:4], para_current[5:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta)))


	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		beta_rho_alphas_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		beta_rho_alphas_next <- beta_rho_alphas_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(beta_rho_alphas_next, log_lh_current)
}




## Joint M-H sampling of beta and rho ##

mh_beta_rho <- function(beta_rho_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1)
 	while(y[1]<=0 | y[2]<=0){
	# y <-  mvrnorm(1,mu=beta_rho_current,Sigma=0.02*matrix(c(5e-9,0,0,0.001),byrow=TRUE,ncol=2,nrow=2))
	y <-  mvrnorm(1,mu=beta_rho_current,Sigma=0.2*matrix(c(5e-9,0,0,0.001),byrow=TRUE,ncol=2,nrow=2))
	}



	para_next <- c(y[1],para_current[2:3], y[2], para_current[5:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta)))


	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		beta_rho_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		beta_rho_next <- beta_rho_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(beta_rho_next, log_lh_current)
}





## Joint M-H sampling of alpha_1 and tau ##

mh_alpha_1_tau <- function(alpha_1_tau_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1)
 	while(y[1]<=0 | y[2]<=0){
	y <-  mvrnorm(1,mu=alpha_1_tau_current,Sigma=0.5*matrix(c(0.8e-3,0,0,0.3e-4),byrow=TRUE,ncol=2,nrow=2))
	}


	para_next <- c(para_current[1], y[1], para_current[3:6], y[2], para_current[8:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind, 1,log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_sm/all_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta)))


	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		alpha_1_tau_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		alpha_1_tau_next <- alpha_1_tau_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(alpha_1_tau_next, log_lh_current)
}





## Joint M-H sampling of alpha_2 and eta ##

mh_alpha_2_eta <- function(alpha_2_eta_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1)
 	while(y[1]<=0 | y[2]<=0){
	# y <-  mvrnorm(1,mu=alpha_2_eta_current,Sigma=0.5*matrix(c(1e-3,0,0,0.6e-3),byrow=TRUE,ncol=2,nrow=2))
	y <-  mvrnorm(1,mu=alpha_2_eta_current,Sigma=0.5*matrix(c(2e-3,0,0,1e-3),byrow=TRUE,ncol=2,nrow=2))

	}

	
	para_next <- c(para_current[1:2], y[1], para_current[4], y[2], para_current[6:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind, 1,log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		alpha_2_eta_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		alpha_2_eta_next <- alpha_2_eta_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(alpha_2_eta_next, log_lh_current)
}



## Joint M-H sampling of alpha_1 and eta ##

mh_alphas_eta <- function(alphas_eta_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1, -1)
 	while(y[1]<=0 | y[2]<=0 | y[3]<=0){
        # y <-  mvrnorm(1,mu=alphas_eta_current,Sigma=0.1*matrix(c(0.6e-3,0,0, 0, 0.8e-3,0, 0,0,0.3e-3),byrow=TRUE,ncol=3,nrow=3))
        y <-  mvrnorm(1,mu=alphas_eta_current,Sigma=0.5*matrix(c(0.1e-3,0,0, 0, 0.1e-3,0, 0,0,0.1e-3),byrow=TRUE,ncol=3,nrow=3))
	}

	
	para_next <- c(para_current[1], y[1:2], para_current[4],y[3], para_current[6:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind, 1,log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		alphas_eta_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		alphas_eta_next <- alphas_eta_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(alphas_eta_next, log_lh_current)
}




## Joint M-H sampling of alpha_1 and eta  and tau ##

mh_alphas_eta_tau <- function(alphas_eta_tau_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1, -1, -1)
 	while(y[1]<=0 | y[2]<=0 | y[3]<=0 | y[4]<=0){
	y <-  mvrnorm(1,mu=alphas_eta_tau_current,Sigma=0.1*matrix(c(0.8e-3,0,0,0, 0, 1e-3,0,0, 0,0,0.6e-3,0, 0,0,0,0.3e-4),byrow=TRUE,ncol=4,nrow=4))
	}

	
	para_next <- c(para_current[1], y[1:2], para_current[4],y[3], para_current[6], y[4], para_current[8:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		alphas_eta_tau_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		alphas_eta_tau_next <- alphas_eta_tau_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(alphas_eta_tau_next, log_lh_current)
}



## Joint M-H sampling of b and a##

mh_b_a <- function(b_a_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1)
 	while(y[1]<=0 | y[2]<=0){
	y <-  mvrnorm(1,mu=b_a_current,Sigma=0.5*matrix(c(6e-5,0,0,1e-4),byrow=TRUE,ncol=2,nrow=2))
	}

	para_next <- c(para_current[1:5],y[1],para_current[7:8],y[2],para_current[10:11])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum(unlist(future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		b_a_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		b_a_next <- b_a_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(b_a_next, log_lh_current)
}



## Joint M-H sampling of b and a##

mh_c_d <- function(c_d_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){
# log_lh_current = the log-likelihood at currrent state

	y <- c(-1,-1)
 	while(y[1]<=0 | y[2]<=0){
	y <-  mvrnorm(1,mu=c_d_current,Sigma=0.5*matrix(c(1e-5,0,0,1e-5),byrow=TRUE,ncol=2,nrow=2))
	}

	para_next <- c(para_current[1:9],y)

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		c_d_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		c_d_next <- c_d_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(c_d_next, log_lh_current)
}




## M-H sampling of beta_coupling ##

mh_beta_coupling <- function(beta_coupling_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- beta_coupling_current + 0.001*rnorm(1, mean=0, sd=0.5)

	# log_y <- log(beta_coupling_current) + 0.2*rnorm(1, mean=0, sd=1) # propose on log-scale
	# y <- exp(log_y)

	low <- 0.0
	up <- 1000

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(y,para_current[-1])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	# prior_next <- dexp(y, 0.001)
	# prior_current <- dexp(beta_coupling_current, 0.001)


 	acp <- min(1, exp(log_lh_next - log_lh_current))
	#acp <- min(1, exp((log_lh_next - log_lh_current)+(log(beta_coupling_current)-log(y)))); # propose on log_scale
	 # acp <- min(1, exp( (log_lh_next - log_lh_current)+(log(y)-log(beta_coupling_current)) ) ); # propose on log_scale

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		beta_coupling_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		beta_coupling_next <- beta_coupling_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(beta_coupling_next, log_lh_current)
}







## M-H sampling of rho ##

mh_rho <- function(rho_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){
# log_lh_current = the log-likelihood at currrent state

	y <- rho_current + 0.01*rnorm(1, mean=0, sd=1)

	low <- 0.0001 
	up <- 10

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(para_current[1:3], y, para_current[5:11])

	# log_prior_next <- dgamma(y, 2,1,log=TRUE)
	# log_prior_current <- dgamma(rho_current, 2,1,log=TRUE)


	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))
	# acp <- min(1, exp( (log_lh_next - log_lh_current)+(log_prior_next-log_prior_current) ))


	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		rho_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		rho_next <- rho_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(rho_next, log_lh_current)
}



## M-H sampling of alpha_1 ##

mh_alpha_1 <- function(alpha_1_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- alpha_1_current + 0.01*rnorm(1, mean=0, sd=1)

	low <- 0.0001 
	up <- 1.0

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(para_current[1], y, para_current[3:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		alpha_1_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		alpha_1_next <- alpha_1_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(alpha_1_next, log_lh_current)
}




## M-H sampling of alpha_2 ##

mh_alpha_2 <- function(alpha_2_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- alpha_2_current + 0.01*rnorm(1, mean=0, sd=1)

	low <- 0.0001 
	up <- 1.0

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(para_current[1:2], y, para_current[4:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		alpha_2_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		alpha_2_next <- alpha_2_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(alpha_2_next, log_lh_current)
}



## M-H sampling of eta ##

mh_eta <- function(eta_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- eta_current + 0.2*rnorm(1, mean=0, sd=1)

	low <- 0.0001 
	up <- 10.0

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(para_current[1:4],y, para_current[6:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))


	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		eta_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		eta_next <- eta_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(eta_next, log_lh_current)
}




## M-H sampling of b ##

mh_b <- function(b_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- b_current + 0.1*rnorm(1, mean=0, sd=0.5)

	low <- 0.0
	up <- 10.0

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(para_current[1:5],y, para_current[7:9])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		b_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		b_next <- b_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(b_next, log_lh_current)
}




## M-H sampling of tau ##

mh_tau <- function(tau_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){
# log_lh_current = the log-likelihood at currrent state

	y <- -1
 	while(y<=0){
	y <- tau_current + 0.005*rnorm(1, mean=0, sd=1)
	}

	# log_y <- log(tau_current) + 1*rnorm(1, mean=0, sd=1) # propose on log-scale
	# y <- exp(log_y)

	# low <- 0.0
	# up <- 10

	# if (y<=low) y <- low + abs(y-low)
	# if (y>=up) y <- up - abs(y-up)


	para_next <- c(para_current[1:6],y, para_current[8:11])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	# log_prior_next <- dexp(y, 0.001, log=TRUE)
	# log_prior_current <- dexp(tau_current, 0.001, log=TRUE)


	acp <- min(1, exp(log_lh_next - log_lh_current))
	# acp <- min(1, exp((log_lh_next - log_lh_current)+(log(y)-log(tau_current)))); # propose on log_scale
	# acp <- min(1, exp((log_lh_next - log_lh_current)+(log(y)-log(tau_current))+(log_prior_next-log_prior_current) )); # propose on log_scale

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		tau_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		tau_next <- tau_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(tau_next, log_lh_current)
}






## M-H sampling of tau ##

mh_tau_2 <- function(tau_2_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){
# log_lh_current = the log-likelihood at currrent state

	y <- -1
 	while(y<=0){
	y <- tau_2_current + 0.002*rnorm(1, mean=0, sd=1)
	}

	# log_y <- log(tau_current) + 1*rnorm(1, mean=0, sd=1) # propose on log-scale
	# y <- exp(log_y)

	# low <- 0.0
	# up <- 10

	# if (y<=low) y <- low + abs(y-low)
	# if (y>=up) y <- up - abs(y-up)


	para_next <- c(para_current[1:9],y, para_current[11])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	# log_prior_next <- dexp(y, 0.001, log=TRUE)
	# log_prior_current <- dexp(tau_current, 0.001, log=TRUE)


	acp <- min(1, exp(log_lh_next - log_lh_current))
	# acp <- min(1, exp((log_lh_next - log_lh_current)+(log(y)-log(tau_current)))); # propose on log_scale
	# acp <- min(1, exp((log_lh_next - log_lh_current)+(log(y)-log(tau_current))+(log_prior_next-log_prior_current) )); # propose on log_scale

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		tau_2_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		tau_2_next <- tau_2_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(tau_2_next, log_lh_current)
}



## M-H sampling of tau ##

mh_tau_3 <- function(tau_3_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- -1
 	while(y<=0){
	y <- tau_3_current + 0.01*rnorm(1, mean=0, sd=1)
	}

	# log_y <- log(tau_current) + 1*rnorm(1, mean=0, sd=1) # propose on log-scale
	# y <- exp(log_y)

	# low <- 0.0
	# up <- 10

	# if (y<=low) y <- low + abs(y-low)
	# if (y>=up) y <- up - abs(y-up)


	para_next <- c(para_current[1:10],y)

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	# log_prior_next <- dexp(y, 0.001, log=TRUE)
	# log_prior_current <- dexp(tau_current, 0.001, log=TRUE)


	acp <- min(1, exp(log_lh_next - log_lh_current))
	# acp <- min(1, exp((log_lh_next - log_lh_current)+(log(y)-log(tau_current)))); # propose on log_scale
	# acp <- min(1, exp((log_lh_next - log_lh_current)+(log(y)-log(tau_current))+(log_prior_next-log_prior_current) )); # propose on log_scale

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		tau_3_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		tau_3_next <- tau_3_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(tau_3_next, log_lh_current)
}




## M-H sampling of psi ##

mh_psi <- function(psi_current, log_lh_current, para_current ,t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){
# log_lh_current = the log-likelihood at currrent state

	y <- psi_current + 1*rnorm(1, mean=0, sd=1)


	low <- 0.0
	up <- 1000

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(para_current[1:7],y, para_current[9:11])

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta, dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		psi_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		psi_next <- psi_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(psi_next, log_lh_current)
}


## M-H sampling of a ##

mh_a <- function(a_current, log_lh_current, para_current ,t_step,cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,list_all_time_with_ind,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta){
# log_lh_current = the log-likelihood at currrent state

	y <- a_current + 0.1*rnorm(1, mean=0, sd=0.5)

	low <- 0.0
	up <- 100.0

	if (y<=low) y <- low + abs(y-low)
	if (y>=up) y <- up - abs(y-up)

	para_next <- c(para_current[1:8],y)

	#log_lh_next <- sum(sapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta))

	log_lh_next <- sum(unlist(mclapply(list_all_time_with_ind, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,  susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,mc.cores=n_core,mc.preschedule=TRUE)))

	# log_lh_next <- sum((future_apply(list_all_time_with_ind,1, log_lh_single,  para_next, t_step, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds, cases_newcastle, cases_bristol, cases_cardiff, cases_norwich, cases_swansea, cases_exeter, cases_plymouth, cases_portsmouth,pop_small_cities, total_Nt_small_cities,dist_to_london_small_cities, dist_to_bmh_small_cities, dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities, dist_to_cardiff_small_cities, dist_to_norwich_small_cities, dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities,susceptibles_small_cities, avg_pop_small_cities, norm_const_for_beta,dist_mat_betweeen_small_cities,cases_small_cities_mat, dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities)))

	acp <- min(1, exp(log_lh_next - log_lh_current))

	#print(c(y, acp))

	uniform_draw <- runif(1)

	if (uniform_draw<=acp) {
		a_next <- y
		log_lh_current <- log_lh_next
		#para_current <- para_next
	}

	if (uniform_draw>acp) {
		a_next <- a_current
		log_lh_current <- log_lh_current
		#para_current <- para_current
	}


	c(a_next, log_lh_current)
}






## log-likelihood for a small city (Rcpp); same as log_lh_single but hand-keyed dnbinom##
cppFunction('


	double log_lh_single (IntegerVector list_all_time_single, NumericVector para, int t_step,  Rcpp::NumericVector& cases_london_vector,  Rcpp::NumericVector& cases_bmh_vector,  Rcpp::NumericVector& cases_liv_vector, Rcpp::NumericVector& cases_man_vector, Rcpp::NumericVector& cases_shef_vector, Rcpp::NumericVector& cases_leeds_vector, Rcpp::NumericVector& cases_newcastle_vector, Rcpp::NumericVector& cases_bristol_vector, Rcpp::NumericVector& cases_cardiff_vector, Rcpp::NumericVector& cases_norwich_vector, Rcpp::NumericVector& cases_swansea_vector, Rcpp::NumericVector& cases_exeter_vector,Rcpp::NumericVector& cases_plymouth_vector,Rcpp::NumericVector& cases_portsmouth_vector,Rcpp::NumericMatrix& pop_small_cities_mat, NumericVector& total_Nt_small_cities, NumericVector& dist_to_london_small_cities, NumericVector& dist_to_bmh_small_cities,  NumericVector& dist_to_liv_small_cities, NumericVector& dist_to_man_small_cities, NumericVector& dist_to_shef_small_cities, NumericVector& dist_to_leeds_small_cities,  NumericVector& dist_to_newcastle_small_cities,  NumericVector& dist_to_bristol_small_cities,  NumericVector& dist_to_cardiff_small_cities,  NumericVector& dist_to_norwich_small_cities, NumericVector& dist_to_swansea_small_cities, NumericVector& dist_to_exeter_small_cities, NumericVector& dist_to_plymouth_small_cities, NumericVector& dist_to_portsmouth_small_cities, Rcpp::NumericMatrix& susceptibles_small_cities_mat, NumericVector& avg_pop_small_cities, double& norm_const_for_beta, Rcpp::NumericMatrix& dist_mat_betweeen_small_cities, Rcpp::NumericMatrix& cases_small_cities_mat, Rcpp::NumericMatrix dist_mat_to_other_cities, Rcpp::NumericMatrix& cases_other_cities_mat, int n_small_cities, int n_other_cities) {

//	Function dtrunc("dtrunc") ;
//	Function dnbinom("dnbinom") ;


	double log_lh_single = 0.0;

	int ind_small_city = list_all_time_single[list_all_time_single.size()-1]; //ind_small_city = i^th small city

	IntegerVector list_influx_time_single_with_NA (list_all_time_single.begin(), list_all_time_single.begin() + t_step);
	IntegerVector list_die_time_single_with_NA (list_all_time_single.begin() + t_step , list_all_time_single.end()-1);

	//double beta_norm = sum(total_Nt_small_cities);

//Rcpp::Rcout << beta_norm  << std::endl;

	IntegerVector list_influx_time_single = list_influx_time_single_with_NA[is_na(list_influx_time_single_with_NA)==FALSE];
	IntegerVector list_die_time_single = list_die_time_single_with_NA[is_na(list_die_time_single_with_NA)==FALSE];

	//IntegerVector list_influx_time_single = list_influx_time_single_with_NA[list_influx_time_single_with_NA!=9999];
	//IntegerVector list_die_time_single = list_die_time_single_with_NA[list_die_time_single_with_NA!=9999];


	int t_0 = 0;

	//int n_influx = list_influx_time_single.size();
	int n_die =list_die_time_single.size();


	//if (list_die_time_single[0]<list_influx_time_single[0]) {
	if ((list_die_time_single[0]-list_influx_time_single[0])<2) {
		t_0 = list_die_time_single[0];
		list_die_time_single.erase(list_die_time_single.begin());
		n_die = n_die - 1;
	}

	IntegerVector period_immune;

	//NumericVector log_lh_die_step (n_die);

	for (int i=0; i<=(n_die-1); i++) {
		
		for (int j=(list_influx_time_single[i]+1); j<=(list_die_time_single[i]-1); j++) {
			period_immune.push_back(j);

		}


	double mu_covariate = avg_pop_small_cities[ind_small_city-1];
	//double mu_covariate = pop_small_cities_mat(list_influx_time_single[i],ind_small_city);

//Rcpp::Rcout << mu_covariate << std::endl;


//    double p =  para[7]/( para[7]+ para[8]*exp(para[5]*log(mu_covariate)) );
    double p =  (para[7]/log(mu_covariate))/( (para[7]/log(mu_covariate))+ para[8]*exp(para[5]*log(mu_covariate)) );

    int x = list_die_time_single[i] - list_influx_time_single[i] -2;
    double y = x;
//    double log_lh_die_step = log( (Rf_gammafn(y+para[7])/(Rf_gammafn(para[7])*Rf_gammafn(y+1.0)))*pow(p,para[7])*pow(1.0-p,y) );
    double log_lh_die_step = log( (Rf_gammafn(y+para[7]/log(mu_covariate))/(Rf_gammafn(para[7]/log(mu_covariate))*Rf_gammafn(y+1.0)))*pow(p,para[7]/log(mu_covariate))*pow(1.0-p,y) );


	//Rcpp::Rcout << list_die_time_single[i] << "," << list_influx_time_single[i] << "," << log_lh_die_step << std::endl;

    log_lh_single  = log_lh_single + log_lh_die_step;
	//Rcpp::Rcout << list_die_time_single[i] << "," << list_influx_time_single[i] << "," << n_die << std::endl;

	}



	//IntegerVector list_influx_time_single_with_NA(list_all_time_single.begin(), list_all_time_single.begin() + t_step +1);

	//NumericVector It_london_vector = cases_london[1]; // second column of dataframe cases_london
	double dist_to_london = dist_to_london_small_cities[ind_small_city-1];

	//NumericVector It_bmh_vector = cases_bmh[1]; // second column of dataframe cases_bmh
	double dist_to_bmh = dist_to_bmh_small_cities[ind_small_city-1];

	double dist_to_liv = dist_to_liv_small_cities[ind_small_city-1];
	double dist_to_man = dist_to_man_small_cities[ind_small_city-1];
	double dist_to_shef = dist_to_shef_small_cities[ind_small_city-1];
	double dist_to_leeds = dist_to_leeds_small_cities[ind_small_city-1];

	double dist_to_newcastle = dist_to_newcastle_small_cities[ind_small_city-1];
	double dist_to_bristol = dist_to_bristol_small_cities[ind_small_city-1];
	double dist_to_cardiff = dist_to_cardiff_small_cities[ind_small_city-1];
	double dist_to_norwich = dist_to_norwich_small_cities[ind_small_city-1];
	double dist_to_swansea = dist_to_swansea_small_cities[ind_small_city-1];

	double dist_to_exeter = dist_to_exeter_small_cities[ind_small_city-1];
	double dist_to_plymouth = dist_to_plymouth_small_cities[ind_small_city-1];
	double dist_to_portsmouth = dist_to_portsmouth_small_cities[ind_small_city-1];

	NumericVector dist_between_small_cities_t = wrap(dist_mat_betweeen_small_cities(ind_small_city-1,_));
	dist_between_small_cities_t.erase(ind_small_city-1);

	//double den = density_small_cities[ind_small_city-1];


	//NumericVector Nt_vector = pop_small_cities_mat[,ind_small_city];
	//NumericVector St_vector = susceptibles_small_cities_mat[,ind_small_city];


//		double norm_const_for_beta = 1.0;

//		NumericVector pop_t = pop_small_cities_mat(t,_);
//		double norm_const_for_beta = sum(pop_t);

//Rcpp::Rcout << norm_const_for_beta << std::endl;


	for (int t=t_0; t<=(t_step-1); t++){

//		double It_london = It_london_vector[t];
		double It_london = cases_london_vector[t];

//		double It_bmh = It_bmh_vector[t];
		double It_bmh = cases_bmh_vector[t];

		double It_liv = cases_liv_vector[t];
		double It_man = cases_man_vector[t];
		double It_shef = cases_shef_vector[t];
		double It_leeds = cases_leeds_vector[t];

		double It_newcastle = cases_newcastle_vector[t];
		double It_bristol = cases_bristol_vector[t];
		double It_cardiff = cases_cardiff_vector[t];
		double It_norwich = cases_norwich_vector[t];
		double It_swansea = cases_swansea_vector[t];

		double It_exeter = cases_exeter_vector[t];
		double It_plymouth = cases_plymouth_vector[t];
		double It_portsmouth = cases_portsmouth_vector[t];

//		double Nt = Nt_vector[t];
//		double St = St_vector[t];

		double Nt = pop_small_cities_mat(t,ind_small_city);
		double St = susceptibles_small_cities_mat(t,ind_small_city);

		//double tau_i =0;
		//if(avg_pop_small_cities[ind_small_city-1]<=10000) tau_i = para[6];
		//if(avg_pop_small_cities[ind_small_city-1]>10000 & avg_pop_small_cities[ind_small_city-1]<=30000) tau_i = para[9];
		//if(avg_pop_small_cities[ind_small_city-1]>30000 & avg_pop_small_cities[ind_small_city-1]<=100000) tau_i = para[10];

		//-------
		// double risk_region = 0.0;

		
		// //int n_small_cities = dist_mat_betweeen_small_cities.ncol();

		// for (int i=1;i<=n_small_cities;i++){

		// 	/*
		// 	switch(1*(i==ind_small_city)){
		// 		case 0:{
		// 			risk_region = risk_region + (para[0])*pow(cases_small_cities_mat(t,i),para[1])*pow(Nt,para[2])/pow(dist_mat_betweeen_small_cities(ind_small_city-1,i-1),para[3]);
		// 		break;
		// 		}
		// 		case 1:{
		// 			risk_region = risk_region + 0;
		// 		break;
		// 		}
		// 	}
		// 	*/

			
		// 	if (i==ind_small_city) risk_region = risk_region + 0;
		// 	//if (i!=ind_small_city & cases_small_cities_mat(t,i)!=0 & dist_mat_betweeen_small_cities(ind_small_city-1,i-1)<=50){
		// 	if (i!=ind_small_city & cases_small_cities_mat(t,i)!=0){
		// 		//risk_region = risk_region + para[9];
		// 		//risk_region = risk_region + para[9]*pow(pop_small_cities_mat(t,i),para[10]);
		// 		//risk_region = risk_region + para[9]/pow(dist_mat_betweeen_small_cities(ind_small_city-1,i-1), para[3]);
		// 		//risk_region = risk_region + para[9]*pow(avg_pop_small_cities[i-1],para[10])/pow(dist_mat_betweeen_small_cities(ind_small_city-1,i-1), para[3]);
		// 		risk_region = risk_region + (para[0])*pow(cases_small_cities_mat(t,i),para[1])*pow(Nt,para[2])/pow(dist_mat_betweeen_small_cities(ind_small_city-1,i-1),para[3]);

		// 	}
			

		// }	

		//----------

		NumericVector cases_small_cities_t = wrap(cases_small_cities_mat(t,_));
		cases_small_cities_t.erase(0);
		cases_small_cities_t.erase(ind_small_city-1);


		NumericVector risk_region_small_cities_vec = (para[0]/norm_const_for_beta)*pow(cases_small_cities_t,para[1])*pow(Nt,para[2])/pow(dist_between_small_cities_t,para[3]);
		double risk_region_small_cities = std::accumulate(risk_region_small_cities_vec.begin(), risk_region_small_cities_vec.end(), 0.0);


		NumericVector cases_other_cities_t = wrap(cases_other_cities_mat(t,_));
		cases_other_cities_t.erase(0);
		NumericVector risk_region_other_cities_vec = (para[0]/norm_const_for_beta)*pow(cases_other_cities_t,para[1])*pow(Nt,para[2])/pow(dist_mat_to_other_cities(ind_small_city-1,_),para[3]);
		double risk_region_other_cities = std::accumulate(risk_region_other_cities_vec.begin(), risk_region_other_cities_vec.end(), 0.0);

		double risk_region = risk_region_small_cities + risk_region_other_cities;

//		for (int i=1;i<=n_other_cities;i++){
//			risk_region = risk_region + (para[0])*pow(cases_other_cities_mat(t,i),para[1])*pow(Nt,para[2])/pow(dist_mat_to_other_cities(ind_small_city-1,i-1),para[3]);
//		}

		//----------

		double rate_influx =  risk_region*pow(St,para[4])  + para[6]*pow(St,para[4]) + (para[0]/norm_const_for_beta)*pow(It_london,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_london,para[3]) 
		+ (para[0]/norm_const_for_beta)*pow(It_bmh,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_bmh,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_liv,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_liv,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_man,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_man,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_shef,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_shef,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_leeds,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_leeds,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_newcastle,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_newcastle,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_bristol,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_bristol,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_cardiff,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_cardiff,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_norwich,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_norwich,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_swansea,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_swansea,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_plymouth,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_plymouth,para[3])
		+ (para[0]/norm_const_for_beta)*pow(It_portsmouth,para[1])*pow(Nt,para[2])*pow(St,para[4])/pow(dist_to_portsmouth,para[3]);




//		double rate_influx = para[6] + (para[0])*pow(It,para[1])*pow(Nt,para[2])*pow(log(St),para[4])/pow(dist,para[3]);

		double pr_influx = 1 - exp(-rate_influx);

		double log_lh_influx_step = 0;

		//if ( NumericVector::is_na(list_influx_time_single_with_NA[t])==TRUE & std::find(period_immune.begin(), period_immune.end(), t)==period_immune.end() ) log_lh_influx_step= log(1-pr_influx); //no influx and not in period_immune
		//if (NumericVector::is_na(list_influx_time_single_with_NA[t])==FALSE ) log_lh_influx_step = log(pr_influx); //influx

		if ( list_influx_time_single_with_NA[t]<t_0 & std::find(period_immune.begin(), period_immune.end(), t)==period_immune.end() ) log_lh_influx_step= log(1-pr_influx); //no influx and not in period_immune
		if (list_influx_time_single_with_NA[t]>=t_0) log_lh_influx_step = log(pr_influx); //influx
		//if (list_influx_time_single_with_NA[t]>=t_0 &  std::find(period_immune.begin(), period_immune.end(), t)!=period_immune.end()) Rcpp::Rcout << "impossible"<< std::endl;

//Rcpp::Rcout << ( list_influx_time_single_with_NA[t]  )<< std::endl;

		log_lh_single = log_lh_single + log_lh_influx_step;

	}





return log_lh_single;
}')


##############


# log-likelihood for one small city (R)##

# log_lh_single_R <- function(list_all_time_single, para, t_step, cases_london, cases_bmh, pop_small_cities, total_Nt_small_cities, dist_to_london_small_cities, dist_to_bmh_small_cities, susceptibles_small_cities, mean_susceptibles_small_cities){ 
log_lh_single_R <- function( list_all_time_single,  para,  t_step,   cases_london_vector,  cases_bmh_vector,   cases_liv_vector, cases_man_vector, cases_shef_vector, cases_leeds_vector, cases_newcastle_vector, cases_bristol_vector, cases_cardiff_vector,  cases_norwich_vector,  cases_swansea_vector, cases_exeter_vector, cases_plymouth_vector,cases_portsmouth_vector, pop_small_cities_mat, total_Nt_small_cities, susceptibles_small_cities_mat,  avg_pop_small_cities,  norm_const_for_beta,  cases_small_cities_mat,cases_other_cities_mat,  n_small_cities, n_other_cities,coordinates_urban, other_cities, small_cities){ 

	log_lh_single <- 0

	ind_small_city <- list_all_time_single[length(list_all_time_single)] #ind_small_city = i^th small city
	#list_influx_time_single <-  list_influx_time_single[-(length(list_influx_time_single))] # remove the last entry which is the ind_small_city
	list_influx_time_single <-  unique(list_all_time_single[1:t_step])
	list_die_time_single <-  unique(list_all_time_single[(t_step+1):(t_step+t_step)])

	list_influx_time_single <- subset(list_influx_time_single, is.na(list_influx_time_single)==F)
	list_die_time_single <- subset(list_die_time_single, is.na(list_die_time_single)==F)

	t_0 <- 0

	n_influx <- length(list_influx_time_single)
	n_die <- length(list_die_time_single)

#	if (list_die_time_single[1]<list_influx_time_single[1]) {
	if ((list_die_time_single[1]-list_influx_time_single[1])<2) {
		t_0 <- list_die_time_single[1]
		list_die_time_single <- list_die_time_single[-1]
		n_die <- n_die - 1
	}
   

	period_immune <- {}
	for (i in 1:n_die){
		period_immune <- c(period_immune, (list_influx_time_single[i]+1):(list_die_time_single[i]-1))
		
		mu_covariate = avg_pop_small_cities[ind_small_city]
		p =  (para[8]/log(mu_covariate))/( (para[8]/log(mu_covariate))+ para[9]*exp(para[6]*log(mu_covariate)) )
		y = list_die_time_single[i] - list_influx_time_single[i] -2
 		# log_lh_die_step = log( (gamma(y+para[8]/log(mu_covariate))/(gamma(para[8]/log(mu_covariate))*gamma(y+1.0)))*pow(p,para[8]/log(mu_covariate))*pow(1.0-p,y) )
 		log_lh_die_step = dnbinom(y, size=para[8], mu=mu_covariate, log = TRUE)

		#log_lh_die_step <- log( dgamma(list_die_time_single[i] - list_influx_time_single[i],shape=para[6]*log(as.numeric(mean_susceptibles_small_cities[ind_small_city+1])), scale=1) )
		#log_lh_die_step <- log( dtrunc(list_die_time_single[i] - list_influx_time_single[i],spec="pois", lambda=para[6]*log(as.numeric(mean_susceptibles_small_cities[ind_small_city+1])), a=2, b=Inf) )

		#print(c(list_die_time_single[i], list_influx_time_single[i], n_die))

		log_lh_single <- log_lh_single + log_lh_die_step
	}

#print(log_lh_single)

#  [1] "Birmingham" "Bradford"   "Bristol"    "Coventry"   "Croydon"    "Harrow"     "Hull"       "Leeds"      "Leicester"  "Liverpool"  "London"     "Manchester"
# [13] "Newcastle"  "Nottingham" "Plymouth"   "Portsmouth" "Sheffield"  "Stoke"      "Cardiff"   

	dist_between_small_cities = sapply(small_cities[-ind_small_city],function(x_city){distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)==x_city])/1000})

	# print(length(dist_between_small_cities))

	dist_to_other_cities = sapply(other_cities,function(x_city){distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)==x_city])/1000})

	dist_to_london <-  distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="London"])/1000
	dist_to_bmh <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Birmingham"])/1000
	dist_to_liv <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Liverpool"])/1000

	dist_to_shef <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Sheffield"])/1000
	dist_to_leeds <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Leeds"])/1000
	dist_to_man <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Manchester"])/1000
	dist_to_newcastle <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Newcastle"])/1000
	dist_to_bristol <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Bristol"])/1000
	dist_to_cardiff <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Cardiff"])/1000

	dist_to_norwich <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Norwich"])/1000
	dist_to_swansea <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Swansea"])/1000
	dist_to_exeter <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Exeter"])/1000
	dist_to_plymouth <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Plymouth"])/1000
	dist_to_portsmouth <-distCosine(coordinates_urban[,colnames(coordinates_urban)==small_cities[ind_small_city]], coordinates_urban[,colnames(coordinates_urban)=="Portsmouth"])/1000


	list_influx_time_single_with_NA <- list_all_time_single[1:t_step]
	for (t in t_0:(t_step-1)){

		It_london <- cases_london[,2][t+1]
		# dist_to_london <- dist_to_london_small_cities[ind_small_city]
	

		It_bmh <- cases_bmh[,2][t+1]


		It_liv <- cases_liv[,2][t+1]

		It_man <- cases_man[,2][t+1]

		It_shef <- cases_shef[,2][t+1]

		It_leeds <- cases_leeds[,2][t+1]

		It_newcastle <- cases_newcastle[,2][t+1]

		It_bristol <- cases_bristol[,2][t+1]

		It_cardiff <- cases_cardiff[,2][t+1]

		It_norwich <- cases_norwich[,2][t+1]

		It_swansea <- cases_swansea[,2][t+1]

		It_exeter <- cases_exeter[,2][t+1]

		It_plymouth<- cases_plymouth[,2][t+1]

		It_portsmouth<- cases_portsmouth[,2][t+1]


		Nt <- pop_small_cities[,ind_small_city+1][t+1]
		St <- susceptibles_small_cities[,ind_small_city+1][t+1]



		cases_small_cities_t = cases_small_cities_mat[t+1,][-c(1,ind_small_city+1)]
		risk_region_small_cities= sum( (para[1]/norm_const_for_beta)*(cases_small_cities_t^para[2])*(Nt^para[3])/(dist_between_small_cities^para[4]))

		cases_other_cities_t = cases_other_cities_mat[t+1,][-c(1)]
		risk_region_other_cities= sum( (para[1]/norm_const_for_beta)*(cases_other_cities_t^para[2])*(Nt^para[3])/(dist_to_other_cities^para[4]))

		risk_region = risk_region_small_cities + risk_region_other_cities

		rate_influx =  risk_region*(St^para[5])  + para[7]*(St^para[5]) + (para[1]/norm_const_for_beta)*(It_london^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_london^para[4]) 
				+ (para[1]/norm_const_for_beta)*(It_bmh^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_bmh^para[4])
				+ (para[1]/norm_const_for_beta)*(It_liv^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_liv^para[4])
				+ (para[1]/norm_const_for_beta)*(It_man^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_man^para[4])
				+ (para[1]/norm_const_for_beta)*(It_shef^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_shef^para[4])
				+ (para[1]/norm_const_for_beta)*(It_leeds^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_leeds^para[4])
				+ (para[1]/norm_const_for_beta)*(It_newcastle^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_newcastle^para[4])
				+ (para[1]/norm_const_for_beta)*(It_bristol^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_bristol^para[4])
				+ (para[1]/norm_const_for_beta)*(It_cardiff^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_cardiff^para[4])
				+ (para[1]/norm_const_for_beta)*(It_norwich^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_norwich^para[4])
				+ (para[1]/norm_const_for_beta)*(It_swansea^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_swansea^para[4])
				+ (para[1]/norm_const_for_beta)*(It_plymouth^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_plymouth^para[4])
				+ (para[1]/norm_const_for_beta)*(It_portsmouth^para[2])*(Nt^para[3])*(St^para[5])/(dist_to_portsmouth^para[4])


		# rate_influx <- risk_region_small_cities

		pr_influx <- 1 - exp(-rate_influx)

		log_lh_influx_step <- 0
		if (is.na(list_influx_time_single_with_NA[t+1])==TRUE & (t%in%period_immune)==FALSE) log_lh_influx_step <- log(1-pr_influx) # no influx and not in period_immune
		if (is.na(list_influx_time_single_with_NA[t+1])==FALSE ) log_lh_influx_step <- log(pr_influx) # influx
		#if (is.na(list_all_time_single_with_NA[t+1])==FALSE & (t%in%period_immune)==TRUE) print(c("impossible",ind_small_city))

#print((is.na(list_influx_time_single_with_NA[t+1])==TRUE & (t%in%period_immune)==FALSE))

		log_lh_single <- log_lh_single + log_lh_influx_step
	}	



	log_lh_single
}



