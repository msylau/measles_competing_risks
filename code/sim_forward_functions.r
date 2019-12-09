
## simulate the (summary) of epidemics for many cities

simulate_summary_single_city <- function(ind_small_city,  avg_pop, dist_to_london_small_cities, dist_to_bmh_small_cities,  dist_to_liv_small_cities, dist_to_man_small_cities, dist_to_shef_small_cities, dist_to_leeds_small_cities, dist_to_newcastle_small_cities, dist_to_bristol_small_cities,  dist_to_cardiff_small_cities,  dist_to_norwich_small_cities,  dist_to_swansea_small_cities, dist_to_exeter_small_cities, dist_to_plymouth_small_cities, dist_to_portsmouth_small_cities, pop_small_cities_mat, susceptibles_small_cities_mat, cases_london, cases_bmh, cases_liv, cases_man, cases_shef, cases_leeds,  cases_newcastle,  cases_bristol, cases_cardiff,cases_norwich,cases_swansea, cases_exeter,cases_plymouth,cases_portsmouth, para, t_step, dist_mat_betweeen_small_cities,cases_small_cities_mat,  dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities){



		influx_times <- die_times <- rep(NA, t_step)

		duration_immune <- {}
		period_immune <- {}

		t_0 <- which(cases_small_cities_mat[,ind_small_city+1]==0)[1] - 1  # simulate from the first time with 0
		if (t_0>0) period_immune <- seq(0,t_0,1)

		# t_0 <- 0

# count <- 1000
# while(abs(count-count_coupling_data[ind_small_city])>=5){

		for (t in 0:(t_step-1)){

	
			t_influx <- NA
			if (t>=t_0){		
			t_influx <- sim_influx(c(cases_london[t+1], pop_small_cities_mat[t+1,ind_small_city+1], dist_to_london_small_cities,susceptibles_small_cities_mat[t+1,ind_small_city+1], cases_bmh[t+1], dist_to_bmh_small_cities, cases_liv[t+1], dist_to_liv_small_cities, cases_man[t+1], dist_to_man_small_cities, cases_shef[t+1], dist_to_shef_small_cities, cases_leeds[t+1], dist_to_leeds_small_cities, cases_newcastle[t+1], dist_to_newcastle_small_cities, cases_bristol[t+1], dist_to_bristol_small_cities,cases_cardiff[t+1], dist_to_cardiff_small_cities,cases_norwich[t+1], dist_to_norwich_small_cities,cases_swansea[t+1], dist_to_swansea_small_cities,cases_exeter[t+1], dist_to_exeter_small_cities,cases_plymouth[t+1], dist_to_plymouth_small_cities,cases_portsmouth[t+1], dist_to_portsmouth_small_cities),para=para, t=t, period_immune=period_immune,dist_mat_betweeen_small_cities,cases_small_cities_mat,  dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,ind_small_city)
			}


			influx_times[t+1] <- t_influx

			t_die_vector <- sim_die(c(t_influx, as.numeric(avg_pop)), para=para, t=t, period_immune=period_immune) # use avg_pop as ...
			#t_die_vector <- sim_die(c(t_influx, as.numeric(pop_small_cities_mat[t+1,ind_small_city+1])), para=para, t=t, period_immune=period_immune) # use N_t as ...

			die_times [t+1] <- t_die_vector[1]


			period_immune <- t_die_vector[-1]

			duration_immune <- c(duration_immune, t_die_vector[1] -  t_influx)


		}

	n_zero <- t_step - length(period_immune) 
	
	count<- count_coupling(influx_times)
# }

	mean_duration <- mean_duration_coupling(influx_times)
	sd_duration <- sd_duration_coupling(influx_times)

	mean_abs <- mean_abs_coupling(influx_times)

	mean_immune <- mean(duration_immune,na.rm=T)
	sd_immune <- sd(duration_immune,na.rm=T)

	dispersion_immune <- mean_immune^2/(sd_immune^2-mean_immune)
	dispersion_immune[which(dispersion_immune<0 | dispersion_immune>1)] <- NA

	##
	# mean_delta_influx_london <- mean_delta (influx_times, 20, 55, 80, 110, 148, 185)
	# mean_delta_influx_london <- mean_delta (period_immune, cases_london_vector, t_step)
	mean_delta_influx_london <- mean_delta (influx_times, cases_london_vector, t_step)


	##

	# c(n_zero,count, mean_duration,sd_duration,mean_abs,mean_immune,sd_immune)
	c(n_zero,count, mean_duration,sd_duration,mean_immune,sd_immune)

}


## simulate the time of influx at a single step ##

sim_influx <- function(var, para, t, period_immune,dist_mat_betweeen_small_cities,cases_small_cities_mat,  dist_mat_to_other_cities, cases_other_cities_mat, n_small_cities, n_other_cities,ind_small_city){
	
	t_influx <- NA

	It_london <- var[1]
	Nt <- var[2]
	dist_to_london <- var[3]
	St <- var[4]
	
	It_bmh <- var[5]
	dist_to_bmh <- var[6]

	It_liv <- var[7]
	dist_to_liv <- var[8]

	It_man <- var[9]
	dist_to_man <- var[10]

	It_shef <- var[11]
	dist_to_shef <- var[12]

	It_leeds <- var[13]
	dist_to_leeds <- var[14]

	It_newcastle <- var[15]
	dist_to_newcastle <- var[16]

	It_bristol <- var[17]
	dist_to_bristol <- var[18]

	It_cardiff <- var[19]
	dist_to_cardiff <- var[20]

	It_norwich <- var[21]
	dist_to_norwich <- var[22]

	It_swansea <- var[23]
	dist_to_swansea <- var[24]

	It_exeter <- var[25]
	dist_to_exeter <- var[26]

	It_plymouth <- var[27]
	dist_to_plymouth <- var[28]

	It_portsmouth <- var[29]
	dist_to_portsmouth <- var[30]


	#sta <- var[5] # 1= class S, 2= class I (immune)


	risk_region <- 0.0
	for (i in 1:n_small_cities){
		if (i==ind_small_city) risk_region <- risk_region + 0
		if (i!=ind_small_city & cases_small_cities_mat[t+1,i+1]!=0){
			risk_region <- risk_region +  (para[1])*(cases_small_cities_mat[t+1,i+1]^para[2])*(Nt^para[3])/(dist_mat_betweeen_small_cities[ind_small_city,i]^para[4])
		}
	}
	
	for (i in 1:n_other_cities){
		risk_region <- risk_region +  (para[1])*(cases_other_cities_mat[t+1,i+1]^para[2])*(Nt^para[3])/(dist_mat_to_other_cities[ind_small_city,i]^para[4])
	}
	

	risk_region <- risk_region*((St)^para[5])
	
	rate_influx <- risk_region+ para[7]*((St)^para[5]) + para[1]*(It_london^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_london^para[4]) + para[1]*(It_bmh^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_bmh^para[4]) + para[1]*(It_liv^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_liv^para[4]) + para[1]*(It_man^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_man^para[4]) + para[1]*(It_shef^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_shef^para[4]) + para[1]*(It_leeds^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_leeds^para[4]) + para[1]*(It_newcastle^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_newcastle^para[4]) + para[1]*(It_bristol^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_bristol^para[4]) + para[1]*(It_cardiff^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_cardiff^para[4]) + para[1]*(It_norwich^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_norwich^para[4]) + para[1]*(It_swansea^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_swansea^para[4]) + para[1]*(It_plymouth^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_plymouth^para[4])+ para[1]*(It_portsmouth^para[2])*(Nt^para[3])*((St)^para[5])/(dist_to_portsmouth^para[4])



	#rate_influx <- para[1]*(It^para[2])*(Nt^para[3])*(log(St)^para[5])/(dist^para[4])

	pr_influx <- 1 - exp(-rate_influx)

	#print(c(pr_influx, It, Nt, dist, St))

	if(t%in%period_immune==FALSE){
		sim_influx <- sample(c(1,0), size=1, prob=c(pr_influx, 1-pr_influx)) # 1= an influx event happens

		if (sim_influx==1) t_influx <- t
	}

	t_influx
}

## simulate the time of die out at a single step (and update period_immune) ##

sim_die <- function(var, para, t, period_immune){
	
	t_die <- NA

	t_influx <- var[1] # 1 = an influx at time t
	avg_pop <- var[2] # mean number of susceptibles of a city

	if(is.na(t_influx)==FALSE) {

		##t_die <- t_influx+ rtrunc(1, spec="pois", lambda=(para[6]*log(mean_susceptibles)), a=2, b=Inf)
		#t_die <- t_influx+ rtrunc(1, spec="pois", lambda=exp(para[6]*log(mean_susceptibles)), a=2, b=Inf)

		# #t_die <- t_influx+ rtrunc(1, spec="geom", prob=1/ (1 + (para[6]*log(mean_susceptibles))), a=2, b=Inf)
		# t_die <- t_influx+ rtrunc(1, spec="geom", prob=1/ (1 + exp(para[6]*log(mean_susceptibles))), a=2, b=Inf)

		#t_die <- t_influx+ 2 + rnbinom(1, mu=(para[6]*log(mean_susceptibles)), size=para[8])


		# t_die <- t_influx+ 2 + rnbinom(1, mu=para[9]*exp(para[6]*log(avg_pop)), size=para[8])
		t_die <- t_influx+ 2 + rnbinom(1, mu=para[9]*exp(para[6]*log(avg_pop)), size=para[8]/log(avg_pop))

		period_immune <- c(period_immune, (t_influx+1):(t_die-1))

	}

	c(t_die, period_immune)
}


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


Find_Max_CCF<- function(a,b)
{
 d <- ccf(a, b, plot = FALSE)
 cor = d$acf[,,1]
 lag = d$lag[,,1]
 res = data.frame(cor,lag)
 res_max = res[which.max(res$cor),]
 return(res_max)
} 

localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}


## mean delay of take-off times to peak times of e,g London ##

# mean_delta <- function(influx_times, time_ref_1, time_ref_2, time_ref_3, time_ref_4, time_ref_5, time_ref_6){

# 	influx_times_unique <- sort(unique(influx_times))

# 	delta_influx_1 <- influx_times_unique[which(influx_times_unique>=time_ref_1)][1] - time_ref_1 # the first influx time after time_ref_1 (few weeks before the first major peak of e.g London)

# 	delta_influx_2 <- influx_times_unique[which(influx_times_unique>=time_ref_2)][1] - time_ref_2 # the first influx time after _2 (few weeks before the 2nd major peak of e.g London)

# 	delta_influx_3 <- influx_times_unique[which(influx_times_unique>=time_ref_3)][1] - time_ref_3 # the first influx time after _3 (few weeks before the 3rd major peak of e.g London)

# 	delta_influx_4 <- influx_times_unique[which(influx_times_unique>=time_ref_4)][1] - time_ref_4
# 	delta_influx_5 <- influx_times_unique[which(influx_times_unique>=time_ref_5)][1] - time_ref_5
# 	delta_influx_6 <- influx_times_unique[which(influx_times_unique>=time_ref_6)][1] - time_ref_6

# 	mean_delta_influx <- mean(c(delta_influx_1, delta_influx_2, delta_influx_3, delta_influx_4, delta_influx_5, delta_influx_6))
# 	##

# 	mean_delta_influx
# }



mean_delta <- function(influx_times, cases_vector, t_step){

	mean_delta_influx <- NA

	influx_times_unique <- sort(unique(influx_times))

	##r <- rle(cases_vector)
	##maxima_position_cases_vector <- which(rep(x = diff(sign(diff(c(-Inf, r$values, -Inf)))) == -2, times = r$lengths))
	##peak_cases_vector <- maxima_position_cases_vector - 1

	# maxima_position_cases_vector <- peaks(cases_vector, span=3, strict=TRUE)
	# peak_cases_vector <- which(maxima_position_cases_vector==TRUE) - 1
	# peak_cases_vector <- peak_cases_vector - 3 # shift backward the peaks manually 
	# x <- peak_cases_vector
	# y <- influx_times_unique
	# n_min <- min(length(x), length(y))
	# l <- list(x[1:n_min],y[1:n_min])
	# mean_delta_influx <- as.numeric(proxy::dist(l))

	# maxima_position_cases_vector <- peaks(cases_vector, span=3, strict=TRUE)
	# peak_cases_vector <- which(maxima_position_cases_vector==TRUE) - 1
	# peak_cases_vector <- peak_cases_vector - 3 # shift backward the peaks manually 
	# x <- peak_cases_vector
	# y <- influx_times_unique
	# x_subset <- y_subset <-  {}
	# current_nearest_influx <- -1
	# for (i in 1:length(x)){
	# 	nearest_influx <- y[which((y-x[i])>=0 & (y-x[i])<50)[1]]
	# 	if (is.na(nearest_influx)==FALSE & nearest_influx!=current_nearest_influx) {
	# 		y_subset <- c(y_subset,nearest_influx)
	# 		x_subset <- c(x_subset, x[i])
	# 		current_nearest_influx <- nearest_influx
	# 	}
	# }
	# l <- list(x_subset,y_subset)
	# mean_delta_influx <- as.numeric(proxy::dist(l))


	# valleys <- c(1,17,48,71,97,124,153,170, 207, 227, 260, 324,331, 366, 383, 418, 435, 469, 488) - 1
	# valleys <- c(1,17,48,71,97,124,153,170, 207, 227, 260) - 1
	valleys <- localMinima(cases_vector) - 1

	x <- valleys
	y <- influx_times_unique[which(influx_times_unique<=500)]
	# y <- influx_times_unique
	delta_influx <- {}
	for (i in 1:length(x)){

		if (i!=length(x)) y_temp <- y[which(y>=x[i] & y<x[i+1])]
		if (i==length(x)) y_temp <- y[which(y>=x[i])]

		if(length(y_temp)>=1){
			delta_influx <- c(delta_influx, sort(y_temp)[1]-x[i]) # consider the nearest influx
			# delta_influx <- c(delta_influx, mean(y_temp-x[i])) # consider all influx within the range and take the mean
		}
	}
	# mean_delta_influx <- mean(delta_influx)
	mean_delta_influx <- mean(delta_influx[which(delta_influx<=15)])


	mean_delta_influx 

}


# mean_delta <- function(period_immune, cases_vector, t_step){

# 	binary_epi <- rep(0,t_step)
# 	binary_epi[period_immune[which(period_immune<=(t_step-1))]+1] <- 1

# 	binary_epi <- trans_to_tri_wave(binary_epi)


# 	# r <- rle(cases_vector)
# 	# maxima_position_cases_vector <- which(rep(x = diff(sign(diff(c(-Inf, r$values, -Inf)))) == -2, times = r$lengths))
# 	# binary_cases_vector <- rep(0,t_step)
# 	# binary_cases_vector[maxima_position_cases_vector] <- 1

# 	maxima_position_cases_vector <- peaks(cases_vector, span=3, strict=TRUE)
# 	binary_cases_vector <- rep(0,t_step)
# 	binary_cases_vector[maxima_position_cases_vector==TRUE] <- 1


# 	mean_delta_influx <- mean(phase.sync (binary_epi, binary_cases_vector)[[3]]$phasediff, na.rm=T)
# 	# mean_delta_influx <- synchrony::peaks(t1=binary_cases_vector, t2=binary_epi)[[1]]
# 	# mean_delta_influx <- cor(cases_vector, binary_epi)
# 	# mean_delta_influx <- as.numeric(Find_Max_CCF(binary_cases_vector, binary_epi)[1])

# 	mean_delta_influx 
# }


# mean_delta <- function(period_immune, period_immune_data, t_step){

# 	binary_epi <- rep(0,t_step)
# 	binary_epi[period_immune[which(period_immune<=(t_step))]] <- 1

# 	binary_epi_data <- rep(0,t_step)
# 	binary_epi_data[period_immune_data] <- 1

# 	mean_delta_influx <- mean(phase.sync (binary_epi_data, binary_epi)[[3]]$phasediff, na.rm=T)
# 	# mean_delta_influx <- as.numeric(Find_Abs_Max_CCF (binary_epi_data, binary_epi)[1])

# 	mean_delta_influx 
# }






## function calculate the mean of absolute re-initiation times ##

mean_abs_coupling <- function(v){

	v <- v[is.na(v)==FALSE]
	# mean(v, na.rm=T)
	v[1]

	# first_influx <- {}
	# n_year <- length(week_dist)
	# for (i in 1:n_year){
	# 	if (i==1){
	# 		a <- 1
	# 		b <- week_dist[1]
	# 	}
	# 	if (i>1){
	# 		a <- sum(week_dist[1:(i-1)]) + 1
	# 		b <- sum(week_dist[1:i])
	# 	}
	# 	v_year <- v[a:b]
	# 	v_year <- v_year[is.na(v_year)==FALSE]
	# 	if (length(v_year)>=1) first_influx <- c(first_influx, which(v[a:b]==min(v_year)))
	# }
	# mean(first_influx)
}

## function calculate the mean of difference between re-initiations ##

mean_duration_coupling <- function(v){

	mean_duration <- NA

	v <- v[is.na(v)==FALSE]

	if (length(v)>1) mean_duration <- mean(v[-1] - v[1:(length(v)-1)]) # plot mean

	mean_duration
}

## function calculate the sd of difference between re-initiations ##

sd_duration_coupling <- function(v){

	sd_duration <- NA

	v <- v[is.na(v)==FALSE]

	if (length(v)>1) sd_duration <- sd(v[-1] - v[1:(length(v)-1)]) # plot sd

	sd_duration
}


## function counting the number of re-initiations ##

count_coupling <- function(v){

	length(v[is.na(v)==FALSE])

}
