# measles functions Aug 1998

# 1 read in all the databases
#options("memory"=200000000)
#options("object.size"=20000000)

setwd("e:\\research\\ThesisMeaslesGandCRED\\measles\\datasets")

dqr1<-function(yrf,yrf2)
{
options(warn=2)
if(yrf<73.5)
	{
	if(yrf==65)
		{
		if(yrf2<1.5)
			yrf2<-"i"
		else
			yrf2<-"ii"
		}
	else
		{
		if(yrf2<1.5)
			yrf2<-"a"
		else
			yrf2<-"b"
		}
	m<-read.csv(paste("meas",yrf,yrf2,".csv",sep=""),header=T)
	}
if(yrf==74)
	{
	if(yrf2<1.5)
		yrf2<-"i"
	else
		yrf2<-"ii"
	m<-read.csv(paste("meas",yrf,yrf2,".csv",sep=""),header=T)
	}
if(yrf>74.5)
	m<-read.csv(paste("meas",yrf,".csv",sep=""),header=T)
m
}

#read in all the spreadsheets

dq44a<-dqr1(44,1)
dq44b<-dqr1(44,2)
dq45a<-dqr1(45,1)
dq45b<-dqr1(45,2)
dq46a<-dqr1(46,1)
dq46b<-dqr1(46,2)
dq47a<-dqr1(47,1)
dq47b<-dqr1(47,2)
dq48a<-dqr1(48,1)
dq48b<-dqr1(48,2)
dq49a<-dqr1(49,1)
dq49b<-dqr1(49,2)
dq50a<-dqr1(50,1)
dq50b<-dqr1(50,2)
dq51a<-dqr1(51,1)
dq51b<-dqr1(51,2)
dq52a<-dqr1(52,1)
dq52b<-dqr1(52,2)
dq53a<-dqr1(53,1)
dq53b<-dqr1(53,2)
dq54a<-dqr1(54,1)
dq54b<-dqr1(54,2)
dq55a<-dqr1(55,1)
dq55b<-dqr1(55,2)
dq56a<-dqr1(56,1)
dq56b<-dqr1(56,2)
dq57a<-dqr1(57,1)
dq57b<-dqr1(57,2)
dq58a<-dqr1(58,1)
dq58b<-dqr1(58,2)
dq59a<-dqr1(59,1)
dq59b<-dqr1(59,2)
dq60a<-dqr1(60,1)
dq60b<-dqr1(60,2)
dq61a<-dqr1(61,1)
dq61b<-dqr1(61,2)
dq62a<-dqr1(62,1)
dq62b<-dqr1(62,2)
dq63a<-dqr1(63,1)
dq63b<-dqr1(63,2)
dq64a<-dqr1(64,1)
dq64b<-dqr1(64,2)
dq65i<-dqr1(65,1)
dq65ii<-dqr1(65,2)
dq66a<-dqr1(66,1)
dq66b<-dqr1(66,2)
dq67a<-dqr1(67,1)
dq67b<-dqr1(67,2)
dq68a<-dqr1(68,1)
dq68b<-dqr1(68,2)
dq69a<-dqr1(69,1)
dq69b<-dqr1(69,2)
dq70a<-dqr1(70,1)
dq70b<-dqr1(70,2)
dq71a<-dqr1(71,1)
dq71b<-dqr1(71,2)
dq72a<-dqr1(72,1)
dq72b<-dqr1(72,2)
dq73a<-dqr1(73,1)
dq73b<-dqr1(73,2)
dq74i<-dqr1(74,1)
dq74ii<-dqr1(74,2)
dq75<-dqr1(75)
dq76<-dqr1(76)
dq77<-dqr1(77)
dq78<-dqr1(78)
dq79<-dqr1(79)
dq80<-dqr1(80)
dq81<-dqr1(81)
dq82<-dqr1(82)
dq83<-dqr1(83)
dq84<-dqr1(84)
dq85<-dqr1(85)
dq86<-dqr1(86)
dq87<-dqr1(87)
dq88<-dqr1(88)
dq89<-dqr1(89)
dq90<-dqr1(90)
dq91<-dqr1(91)
dq92<-dqr1(92)
dq93<-dqr1(93)
dq94<-dqr1(94)

# extract the various subsets, U & R subsets

dqr2<-function(x,wch,wch2)
{
if(missing(wch))
	stop("\nYou must either give U (rban), R (ural) or L (ondon)")
if(missing(wch2))
	stop("\nYou must either give 65 or 74")
if(wch2==65)
	{
	x2<-x[x$URL65==wch,]
	x3<-x2[order(x2$P4465),]
	if(names(x3)[1]=="Place")
		x4<-x3[,c(8:ncol(x3))]
	else
		x4<-x3[,c(7:ncol(x3))]
        if(wch=="U")
            if(nrow(x4)!=954)
                {
                cat("\n",nrow(x4))
                stop("wrong number of places for URL65='U', should be 954")
                }
        if(wch=="L")
            if(nrow(x4)!=29)
                {
                cat("\n",nrow(x4))
                stop("wrong number of places for URL65='L', should be 29")
                }
        if(wch=="R")
            if(nrow(x4)!=468)
                {
                cat("\n",nrow(x4))
                stop("wrong number of places for URL65='R', should be 468")
                }
        }
if(wch2==74)
	{
	x2<-x[x$URL74==wch,]
	x3<-x2[order(x2$P4474),]
        if(as.numeric(substring(dimnames(x3)[[2]][10],2,3))<65)
            {
            if(names(x3)[1]=="Place")
		x4<-x3[,c(8:ncol(x3))]
	   else
		x4<-x3[,c(7:ncol(x3))]
            }
        if(as.numeric(substring(dimnames(x3)[[2]][10],2,3))>65)
            {
            if(names(x3)[1]=="Place")
		x4<-x3[,c(6:ncol(x3))]
	   else
		x4<-x3[,c(5:ncol(x3))]
            }
        if(as.numeric(substring(dimnames(x3)[[2]][10],2,3))==65)
            {
            if(names(x3)[1]=="Place")
                    {
                    if(names(x3)[3]=="URL65")
                		x4<-x3[,c(8:ncol(x3))]
                    else
                		x4<-x3[,c(6:ncol(x3))]
                    }
	   else
                    {
                    if(names(x3)[3]=="URL65")
                		x4<-x3[,c(7:ncol(x3))]
                    else
                		x4<-x3[,c(5:ncol(x3))]
                    }
            }
        if(wch=="U")
            if(nrow(x4)!=845)
                {
                cat("\n",nrow(x4))
                stop("wrong number of places for URL65='U', should be 845")
                }
        if(wch=="R")
            if(nrow(x4)!=457)
                {
                cat("\n",nrow(x4))
                stop("wrong number of places for URL74='R', should be 457")
                }
	}
x4<-t(x4)
dimnames(x4)[[1]][1:nrow(x4)]<-substring(dimnames(x4)[[1]][1:nrow(x4)],2)
x4
}


dqu644a<-dqr2(dq44a,"U",65)
dqu644b<-dqr2(dq44b,"U",65)
dqu645a<-dqr2(dq45a,"U",65)
dqu645b<-dqr2(dq45b,"U",65)
dqu646a<-dqr2(dq46a,"U",65)
dqu646b<-dqr2(dq46b,"U",65)
dqu647a<-dqr2(dq47a,"U",65)
dqu647b<-dqr2(dq47b,"U",65)
dqu648a<-dqr2(dq48a,"U",65)
dqu648b<-dqr2(dq48b,"U",65)
dqu649a<-dqr2(dq49a,"U",65)
dqu649b<-dqr2(dq49b,"U",65)
dqu650a<-dqr2(dq50a,"U",65)
dqu650b<-dqr2(dq50b,"U",65)
dqu651a<-dqr2(dq51a,"U",65)
dqu651b<-dqr2(dq51b,"U",65)
dqu652a<-dqr2(dq52a,"U",65)
dqu652b<-dqr2(dq52b,"U",65)
dqu653a<-dqr2(dq53a,"U",65)
dqu653b<-dqr2(dq53b,"U",65)
dqu654a<-dqr2(dq54a,"U",65)
dqu654b<-dqr2(dq54b,"U",65)
dqu655a<-dqr2(dq55a,"U",65)
dqu655b<-dqr2(dq55b,"U",65)
dqu656a<-dqr2(dq56a,"U",65)
dqu656b<-dqr2(dq56b,"U",65)
dqu657a<-dqr2(dq57a,"U",65)
dqu657b<-dqr2(dq57b,"U",65)
dqu658a<-dqr2(dq58a,"U",65)
dqu658b<-dqr2(dq58b,"U",65)
dqu659a<-dqr2(dq59a,"U",65)
dqu659b<-dqr2(dq59b,"U",65)
dqu660a<-dqr2(dq60a,"U",65)
dqu660b<-dqr2(dq60b,"U",65)
dqu661a<-dqr2(dq61a,"U",65)
dqu661b<-dqr2(dq61b,"U",65)
dqu662a<-dqr2(dq62a,"U",65)
dqu662b<-dqr2(dq62b,"U",65)
dqu663a<-dqr2(dq63a,"U",65)
dqu663b<-dqr2(dq63b,"U",65)
dqu664a<-dqr2(dq64a,"U",65)
dqu664b<-dqr2(dq64b,"U",65)
dqu665i<-dqr2(dq65i,"U",65)

dqr644a<-dqr2(dq44a,"R",65)
dqr644b<-dqr2(dq44b,"R",65)
dqr645a<-dqr2(dq45a,"R",65)
dqr645b<-dqr2(dq45b,"R",65)
dqr646a<-dqr2(dq46a,"R",65)
dqr646b<-dqr2(dq46b,"R",65)
dqr647a<-dqr2(dq47a,"R",65)
dqr647b<-dqr2(dq47b,"R",65)
dqr648a<-dqr2(dq48a,"R",65)
dqr648b<-dqr2(dq48b,"R",65)
dqr649a<-dqr2(dq49a,"R",65)
dqr649b<-dqr2(dq49b,"R",65)
dqr650a<-dqr2(dq50a,"R",65)
dqr650b<-dqr2(dq50b,"R",65)
dqr651a<-dqr2(dq51a,"R",65)
dqr651b<-dqr2(dq51b,"R",65)
dqr652a<-dqr2(dq52a,"R",65)
dqr652b<-dqr2(dq52b,"R",65)
dqr653a<-dqr2(dq53a,"R",65)
dqr653b<-dqr2(dq53b,"R",65)
dqr654a<-dqr2(dq54a,"R",65)
dqr654b<-dqr2(dq54b,"R",65)
dqr655a<-dqr2(dq55a,"R",65)
dqr655b<-dqr2(dq55b,"R",65)
dqr656a<-dqr2(dq56a,"R",65)
dqr656b<-dqr2(dq56b,"R",65)
dqr657a<-dqr2(dq57a,"R",65)
dqr657b<-dqr2(dq57b,"R",65)
dqr658a<-dqr2(dq58a,"R",65)
dqr658b<-dqr2(dq58b,"R",65)
dqr659a<-dqr2(dq59a,"R",65)
dqr659b<-dqr2(dq59b,"R",65)
dqr660a<-dqr2(dq60a,"R",65)
dqr660b<-dqr2(dq60b,"R",65)
dqr661a<-dqr2(dq61a,"R",65)
dqr661b<-dqr2(dq61b,"R",65)
dqr662a<-dqr2(dq62a,"R",65)
dqr662b<-dqr2(dq62b,"R",65)
dqr663a<-dqr2(dq63a,"R",65)
dqr663b<-dqr2(dq63b,"R",65)
dqr664a<-dqr2(dq64a,"R",65)
dqr664b<-dqr2(dq64b,"R",65)
dqr665i<-dqr2(dq65i,"R",65)

dql644a<-dqr2(dq44a,"L",65)
dql644b<-dqr2(dq44b,"L",65)
dql645a<-dqr2(dq45a,"L",65)
dql645b<-dqr2(dq45b,"L",65)
dql646a<-dqr2(dq46a,"L",65)
dql646b<-dqr2(dq46b,"L",65)
dql647a<-dqr2(dq47a,"L",65)
dql647b<-dqr2(dq47b,"L",65)
dql648a<-dqr2(dq48a,"L",65)
dql648b<-dqr2(dq48b,"L",65)
dql649a<-dqr2(dq49a,"L",65)
dql649b<-dqr2(dq49b,"L",65)
dql650a<-dqr2(dq50a,"L",65)
dql650b<-dqr2(dq50b,"L",65)
dql651a<-dqr2(dq51a,"L",65)
dql651b<-dqr2(dq51b,"L",65)
dql652a<-dqr2(dq52a,"L",65)
dql652b<-dqr2(dq52b,"L",65)
dql653a<-dqr2(dq53a,"L",65)
dql653b<-dqr2(dq53b,"L",65)
dql654a<-dqr2(dq54a,"L",65)
dql654b<-dqr2(dq54b,"L",65)
dql655a<-dqr2(dq55a,"L",65)
dql655b<-dqr2(dq55b,"L",65)
dql656a<-dqr2(dq56a,"L",65)
dql656b<-dqr2(dq56b,"L",65)
dql657a<-dqr2(dq57a,"L",65)
dql657b<-dqr2(dq57b,"L",65)
dql658a<-dqr2(dq58a,"L",65)
dql658b<-dqr2(dq58b,"L",65)
dql659a<-dqr2(dq59a,"L",65)
dql659b<-dqr2(dq59b,"L",65)
dql660a<-dqr2(dq60a,"L",65)
dql660b<-dqr2(dq60b,"L",65)
dql661a<-dqr2(dq61a,"L",65)
dql661b<-dqr2(dq61b,"L",65)
dql662a<-dqr2(dq62a,"L",65)
dql662b<-dqr2(dq62b,"L",65)
dql663a<-dqr2(dq63a,"L",65)
dql663b<-dqr2(dq63b,"L",65)
dql664a<-dqr2(dq64a,"L",65)
dql664b<-dqr2(dq64b,"L",65)
dql665i<-dqr2(dq65i,"L",65)

dqu744a<-dqr2(dq44a,"U",74)
dqu744b<-dqr2(dq44b,"U",74)
dqu745a<-dqr2(dq45a,"U",74)
dqu745b<-dqr2(dq45b,"U",74)
dqu746a<-dqr2(dq46a,"U",74)
dqu746b<-dqr2(dq46b,"U",74)
dqu747a<-dqr2(dq47a,"U",74)
dqu747b<-dqr2(dq47b,"U",74)
dqu748a<-dqr2(dq48a,"U",74)
dqu748b<-dqr2(dq48b,"U",74)
dqu749a<-dqr2(dq49a,"U",74)
dqu749b<-dqr2(dq49b,"U",74)
dqu750a<-dqr2(dq50a,"U",74)
dqu750b<-dqr2(dq50b,"U",74)
dqu751a<-dqr2(dq51a,"U",74)
dqu751b<-dqr2(dq51b,"U",74)
dqu752a<-dqr2(dq52a,"U",74)
dqu752b<-dqr2(dq52b,"U",74)
dqu753a<-dqr2(dq53a,"U",74)
dqu753b<-dqr2(dq53b,"U",74)
dqu754a<-dqr2(dq54a,"U",74)
dqu754b<-dqr2(dq54b,"U",74)
dqu755a<-dqr2(dq55a,"U",74)
dqu755b<-dqr2(dq55b,"U",74)
dqu756a<-dqr2(dq56a,"U",74)
dqu756b<-dqr2(dq56b,"U",74)
dqu757a<-dqr2(dq57a,"U",74)
dqu757b<-dqr2(dq57b,"U",74)
dqu758a<-dqr2(dq58a,"U",74)
dqu758b<-dqr2(dq58b,"U",74)
dqu759a<-dqr2(dq59a,"U",74)
dqu759b<-dqr2(dq59b,"U",74)
dqu760a<-dqr2(dq60a,"U",74)
dqu760b<-dqr2(dq60b,"U",74)
dqu761a<-dqr2(dq61a,"U",74)
dqu761b<-dqr2(dq61b,"U",74)
dqu762a<-dqr2(dq62a,"U",74)
dqu762b<-dqr2(dq62b,"U",74)
dqu763a<-dqr2(dq63a,"U",74)
dqu763b<-dqr2(dq63b,"U",74)
dqu764a<-dqr2(dq64a,"U",74)
dqu764b<-dqr2(dq64b,"U",74)
dqu765i<-dqr2(dq65i,"U",74)
dqu765ii<-dqr2(dq65ii,"U",74)
dqu766a<-dqr2(dq66a,"U",74)
dqu766b<-dqr2(dq66b,"U",74)
dqu767a<-dqr2(dq67a,"U",74)
dqu767b<-dqr2(dq67b,"U",74)
dqu768a<-dqr2(dq68a,"U",74)
dqu768b<-dqr2(dq68b,"U",74)
dqu769a<-dqr2(dq69a,"U",74)
dqu769b<-dqr2(dq69b,"U",74)
dqu770a<-dqr2(dq70a,"U",74)
dqu770b<-dqr2(dq70b,"U",74)
dqu771a<-dqr2(dq71a,"U",74)
dqu771b<-dqr2(dq71b,"U",74)
dqu772a<-dqr2(dq72a,"U",74)
dqu772b<-dqr2(dq72b,"U",74)
dqu773a<-dqr2(dq73a,"U",74)
dqu773b<-dqr2(dq73b,"U",74)
dqu774i<-dqr2(dq74i,"U",74)

dqr744a<-dqr2(dq44a,"R",74)
dqr744b<-dqr2(dq44b,"R",74)
dqr745a<-dqr2(dq45a,"R",74)
dqr745b<-dqr2(dq45b,"R",74)
dqr746a<-dqr2(dq46a,"R",74)
dqr746b<-dqr2(dq46b,"R",74)
dqr747a<-dqr2(dq47a,"R",74)
dqr747b<-dqr2(dq47b,"R",74)
dqr748a<-dqr2(dq48a,"R",74)
dqr748b<-dqr2(dq48b,"R",74)
dqr749a<-dqr2(dq49a,"R",74)
dqr749b<-dqr2(dq49b,"R",74)
dqr750a<-dqr2(dq50a,"R",74)
dqr750b<-dqr2(dq50b,"R",74)
dqr751a<-dqr2(dq51a,"R",74)
dqr751b<-dqr2(dq51b,"R",74)
dqr752a<-dqr2(dq52a,"R",74)
dqr752b<-dqr2(dq52b,"R",74)
dqr753a<-dqr2(dq53a,"R",74)
dqr753b<-dqr2(dq53b,"R",74)
dqr754a<-dqr2(dq54a,"R",74)
dqr754b<-dqr2(dq54b,"R",74)
dqr755a<-dqr2(dq55a,"R",74)
dqr755b<-dqr2(dq55b,"R",74)
dqr756a<-dqr2(dq56a,"R",74)
dqr756b<-dqr2(dq56b,"R",74)
dqr757a<-dqr2(dq57a,"R",74)
dqr757b<-dqr2(dq57b,"R",74)
dqr758a<-dqr2(dq58a,"R",74)
dqr758b<-dqr2(dq58b,"R",74)
dqr759a<-dqr2(dq59a,"R",74)
dqr759b<-dqr2(dq59b,"R",74)
dqr760a<-dqr2(dq60a,"R",74)
dqr760b<-dqr2(dq60b,"R",74)
dqr761a<-dqr2(dq61a,"R",74)
dqr761b<-dqr2(dq61b,"R",74)
dqr762a<-dqr2(dq62a,"R",74)
dqr762b<-dqr2(dq62b,"R",74)
dqr763a<-dqr2(dq63a,"R",74)
dqr763b<-dqr2(dq63b,"R",74)
dqr764a<-dqr2(dq64a,"R",74)
dqr764b<-dqr2(dq64b,"R",74)
dqr765i<-dqr2(dq65i,"R",74)
dqr765ii<-dqr2(dq65ii,"R",74)
dqr766a<-dqr2(dq66a,"R",74)
dqr766b<-dqr2(dq66b,"R",74)
dqr767a<-dqr2(dq67a,"R",74)
dqr767b<-dqr2(dq67b,"R",74)
dqr768a<-dqr2(dq68a,"R",74)
dqr768b<-dqr2(dq68b,"R",74)
dqr769a<-dqr2(dq69a,"R",74)
dqr769b<-dqr2(dq69b,"R",74)
dqr770a<-dqr2(dq70a,"R",74)
dqr770b<-dqr2(dq70b,"R",74)
dqr771a<-dqr2(dq71a,"R",74)
dqr771b<-dqr2(dq71b,"R",74)
dqr772a<-dqr2(dq72a,"R",74)
dqr772b<-dqr2(dq72b,"R",74)
dqr773a<-dqr2(dq73a,"R",74)
dqr773b<-dqr2(dq73b,"R",74)
dqr774i<-dqr2(dq74i,"R",74)

#get the names of places

tmp<-as.character(dq44a$Place[dq44a$URL65=="U"])
tmp2<-dq44a$P4465[dq44a$URL65=="U"]
ewu4465nam<-tmp[order(tmp2)]
rm(tmp,tmp2)

tmp<-as.character(dq44a$Place[dq44a$URL65=="L"])
tmp2<-dq44a$P4465[dq44a$URL65=="L"]
ewl4465nam<-tmp[order(tmp2)]
rm(tmp,tmp2)

tmp<-as.character(dq44a$Place[dq44a$URL65=="R"])
tmp2<-dq44a$P4465[dq44a$URL65=="R"]
ewr4465nam<-tmp[order(tmp2)]
rm(tmp,tmp2)

tmp<-as.character(dq44a$Place[dq44a$URL74=="U"])
tmp2<-dq44a$P4474[dq44a$URL74=="U"]
ewu4474nam<-tmp[order(tmp2)]
rm(tmp,tmp2)

tmp<-as.character(dq44a$Place[dq44a$URL74=="R"])
tmp2<-dq44a$P4474[dq44a$URL74=="R"]
ewr4474nam<-tmp[order(tmp2)]
rm(tmp,tmp2)

## corrected
tmp<-as.character(dq75$Place[!is.na(dq75$PP)])
tmp2<-dq75$PP[!is.na(dq75$PP)]
tmp3<-tmp[order(tmp2)]
ew4494nam<-tmp3
ewl4494nam<-tmp3[1:33]
ewt4494nam<-tmp3[34:399]
rm(tmp,tmp2,tmp3)

PPnum<-as.numeric(names(table(dq75$PP)))

# PP for e &w 1944 - 1994

dqr3<-function(x)
{
x2<-x[!is.na(x$PP),]
if(as.numeric(substring(dimnames(x2)[[2]][10],2,6))<74.2)
	{
        if(as.numeric(substring(dimnames(x2)[[2]][10],2,6))<65.2)
        	{
        	if(names(x2)[1]=="Place")
			xn<-8
		else
			xn<-7
		}
        else
		{
		if(names(x2)[1]=="Place")
			xn<-6
		else
			xn<-5
		}
	x4<-matrix(data=0,ncol=403,nrow=length(dimnames(x2)[[2]][xn:ncol(x2)]),dimnames=list(substring(dimnames(x2)[[2]][xn:ncol(x2)],2),NULL))
	for(i in 1:403)
		for(j in 1:nrow(x4))
			x4[j,i]<-sum(x2[,(xn-1+j)][x2$PP==i],na.rm=T)
	}
else
	{
	x3<-x2[order(x2$PP),]
	if(names(x3)[1]=="Place")
		x4<-x3[,c(4:ncol(x3))]
	else
		x4<-x3[,c(3:ncol(x3))]
	x4<-t(x4)
	dimnames(x4)[[1]]<-substring(dimnames(x4)[[1]],2)
	}
x4
}

dqa944a<-dqr3(dq44a)
dqa944a<-dqr3(dq44a)
dqa944b<-dqr3(dq44b)
dqa945a<-dqr3(dq45a)
dqa945b<-dqr3(dq45b)
dqa946a<-dqr3(dq46a)
dqa946b<-dqr3(dq46b)
dqa947a<-dqr3(dq47a)
dqa947b<-dqr3(dq47b)
dqa948a<-dqr3(dq48a)
dqa948b<-dqr3(dq48b)
dqa949a<-dqr3(dq49a)
dqa949b<-dqr3(dq49b)
dqa950a<-dqr3(dq50a)
dqa950b<-dqr3(dq50b)
dqa951a<-dqr3(dq51a)
dqa951b<-dqr3(dq51b)
dqa952a<-dqr3(dq52a)
dqa952b<-dqr3(dq52b)
dqa953a<-dqr3(dq53a)
dqa953b<-dqr3(dq53b)
dqa954a<-dqr3(dq54a)
dqa954b<-dqr3(dq54b)
dqa955a<-dqr3(dq55a)
dqa955b<-dqr3(dq55b)
dqa956a<-dqr3(dq56a)
dqa956b<-dqr3(dq56b)
dqa957a<-dqr3(dq57a)
dqa957b<-dqr3(dq57b)
dqa958a<-dqr3(dq58a)
dqa958b<-dqr3(dq58b)
dqa959a<-dqr3(dq59a)
dqa959b<-dqr3(dq59b)
dqa960a<-dqr3(dq60a)
dqa960b<-dqr3(dq60b)
dqa961a<-dqr3(dq61a)
dqa961b<-dqr3(dq61b)
dqa962a<-dqr3(dq62a)
dqa962b<-dqr3(dq62b)
dqa963a<-dqr3(dq63a)
dqa963b<-dqr3(dq63b)
dqa964a<-dqr3(dq64a)
dqa964b<-dqr3(dq64b)
dqa965i<-dqr3(dq65i)
dqa965ii<-dqr3(dq65ii)
dqa966a<-dqr3(dq66a)
dqa966b<-dqr3(dq66b)
dqa967a<-dqr3(dq67a)
dqa967b<-dqr3(dq67b)
dqa968a<-dqr3(dq68a)
dqa968b<-dqr3(dq68b)
dqa969a<-dqr3(dq69a)
dqa969b<-dqr3(dq69b)
dqa970a<-dqr3(dq70a)
dqa970b<-dqr3(dq70b)
dqa971a<-dqr3(dq71a)
dqa971b<-dqr3(dq71b)
dqa972a<-dqr3(dq72a)
dqa972b<-dqr3(dq72b)
dqa973a<-dqr3(dq73a)
dqa973b<-dqr3(dq73b)
dqa974i<-dqr3(dq74i)
dqa974ii<-dqr3(dq74ii)
dqa975<-dqr3(dq75)
dqa976<-dqr3(dq76)
dqa977<-dqr3(dq77)
dqa978<-dqr3(dq78)
dqa979<-dqr3(dq79)
dqa980<-dqr3(dq80)
dqa981<-dqr3(dq81)
dqa982<-dqr3(dq82)
dqa983<-dqr3(dq83)
dqa984<-dqr3(dq84)
dqa985<-dqr3(dq85)
dqa986<-dqr3(dq86)
dqa987<-dqr3(dq87)
dqa988<-dqr3(dq88)
dqa989<-dqr3(dq89)
dqa990<-dqr3(dq90)
dqa991<-dqr3(dq91)
dqa992<-dqr3(dq92)
dqa993<-dqr3(dq93)
dqa994<-dqr3(dq94)

# sort out places which split into different post 1974 areas

dqr6<-function(x,y)
{
x2<-x[!is.na(x$PP)&x$PP>900,]
if(as.numeric(substring(dimnames(x2)[[2]][10],2,6))<65.2) 
	{
	if(names(x2)[1]=="Place")
		{
		pxn<-2
		xn<-8
		}
	else
		{
		pxn<-1
		xn<-7
		}
	}
else
	{
	if(names(x2)[1]=="Place")
		{
		pxn<-2
		xn<-6
		}
	else
		{
		pxn<-1
		xn<-5
		}
	}
	
# two way splits
ppn2<-c(1000,1001,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1018,1021,1022,1023,1024,1026,1029,1030,1032,1033,1034,1036,1038,1040,1042,1047,1049,1050,1057,1061,1062,1066,1067,1068,1074,1077,1080,1083,1094,1095,1096,1097,1098,1100,1101,1102,1103,1105,1107)
pppn2a<-c(35,39,44,44,56,56,58,65,68,68,72,76,77,80,85,92,172,110,115,119,127,133,196,343,295,178,178,315,231,278,339,325,273,298,50,276,277,290,290,175,318,340,350,350,375,375,379,371,376,385,380,386,399,385,128)
pppn2b<-c(39,94,280,336,241,60,209,105,88,131,147,229,245,182,216,140,102,279,313,308,352,256,146,147,151,154,154,160,167,178,198,215,218,220,353,358,330,350,350,290,322,326,330,340,367,367,369,370,371,388,402,400,401,396,261)
pprop2<-c(0.5,0.37,0.1,0.2,0.42,0.1,0.85,0.15,0.075,0.22,0.6,0.6,0.85,0.015,0.47,0.625,0.625,0.45,0.8,0.575,0.525,0.8,0.625,0.667,0.3,0.82,0.833,0.8,0.167,0.6,0.875,0.8,0.3,0.1,0.9,0.375,0.6,0.2,0.2,0.61,0.25,0.55,0.5,0.7,0.725,0.625,0.1,0.55,0.925,0.075,0.025,0.275,0.55,0.125,0.75)
for(i in 1:length(ppn2))
	{
	tmp<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==ppn2[i],]))*pprop2[i],0)
	tmp[is.na(tmp)]<-0
	tmp2<-as.numeric(t(x2[,xn:ncol(x2)][x2$PP==ppn2[i],]))-tmp
	tmp2[is.na(tmp2)]<-0
	y[,pppn2a[i]]<-as.numeric(t(y[,pppn2a[i]]))+tmp
	y[,pppn2b[i]]<-as.numeric(t(y[,pppn2b[i]]))+tmp2
	}
	
# three way splits
ppn3<-c(1017,1027,1055)
pppn3a<-c(97,138,235)
pppn3b<-c(213,298,241)
pppn3c<-c(351,311,265)
pprop3a<-c(0.05,0.075,0.2)
pprop3b<-c(0.8,0.27,0.1)
for(i in 1:length(ppn3))
	{
	tmp<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==ppn3[i],]))*pprop3a[i],0)
	tmp[is.na(tmp)]<-0
	tmp2<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==ppn3[i],]))*pprop3b[i],0)
	tmp2[is.na(tmp2)]<-0
	tmp3<-as.numeric(t(x2[,xn:ncol(x2)][x2$PP==ppn3[i],]))-tmp-tmp2
	tmp3[is.na(tmp3)]<-0
	y[,pppn3a[i]]<-as.numeric(t(y[,pppn3a[i]]))+tmp
	y[,pppn3b[i]]<-as.numeric(t(y[,pppn3b[i]]))+tmp2
	y[,pppn3c[i]]<-as.numeric(t(y[,pppn3c[i]]))+tmp3
	}
	
# Basford.RD - 4 way
tmp<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1002,]))*0.147,0)
tmp[is.na(tmp)]<-0
tmp2<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1002,]))*0.346,0)
tmp2[is.na(tmp2)]<-0
tmp3<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1002,]))*0.25,0)
tmp3[is.na(tmp3)]<-0
tmp4<-as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1002,]))-tmp-tmp2-tmp3
tmp4[is.na(tmp4)]<-0
y[,40]<-as.numeric(t(y[,40]))+tmp
y[,75]<-as.numeric(t(y[,75]))+tmp2
y[,141]<-as.numeric(t(y[,141]))+tmp3
y[,250]<-as.numeric(t(y[,250]))+tmp4

# Dorking and Horley RD (1 NA)
tmp<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1020,]))*0.4,0)
tmp[is.na(tmp)]<-0
tmp2<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1020,]))*.45,0)
tmp2[is.na(tmp2)]<-0
y[,201]<-as.numeric(t(y[,201]))+tmp
y[,239]<-as.numeric(t(y[,239]))+tmp2

# Meriden RD & Bucklow RD (only part taken)
tmp<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1045,]))*0.2,0)
tmp[is.na(tmp)]<-0
y[,210]<-as.numeric(t(y[,210]))+tmp
tmp<-round(as.numeric(t(x2[,xn:ncol(x2)][x2$PP==1075,]))*0.4,0)
tmp[is.na(tmp)]<-0
y[,321]<-as.numeric(t(y[,321]))+tmp

# luton , m bedford, bedford
tmp<-round(as.numeric(t(y[,185]))*0.213,0)
tmp[is.na(tmp)]<-0
tmp2<-as.numeric(t(y[,185]))-2*tmp
tmp2[is.na(tmp2)]<-0
y[,51]<-as.numeric(t(y[,51]))+tmp
y[,186]<-as.numeric(t(y[,186]))+tmp
y[,185]<-tmp2

# milton keynes, chiltern
tmp<-round(as.numeric(t(y[,95]))*0.22,0)
tmp[is.na(tmp)]<-0
tmp2<-as.numeric(t(y[,95]))-tmp
tmp2[is.na(tmp2)]<-0
y[,200]<-as.numeric(t(y[,200]))+tmp
y[,95]<-tmp2

# darlington sedgefield
tmp<-round(as.numeric(t(y[,109]))*0.175,0)
tmp[is.na(tmp)]<-0
tmp2<-as.numeric(t(y[,109]))-tmp
tmp2[is.na(tmp2)]<-0
y[,275]<-as.numeric(t(y[,275]))+tmp
y[,109]<-tmp2
for(j in xn:ncol(x2))
	{
	if(as.numeric(substring(dimnames(x2)[[2]][j],2,6))<67.25)
		{
	# Amber Valley
		tmp<-round(as.numeric(t(y[1+j-xn,38]))*0.88,0)
		tmp[is.na(tmp)]<-0
		y[1+j-xn,38]<-tmp
		}
	else
		tgh3<-3
	}
	
# dudley, wolverhampton, sandwell, walsall
for(j in xn:ncol(x2))
	{
	if(as.numeric(substring(dimnames(x2)[[2]][j],2,6))<65.25)
		{
		tmp<-round(as.numeric(t(y[1+j-xn,116]))*0.111,0)
		tmp[is.na(tmp)]<-0
		tmp2<-round(as.numeric(t(y[1+j-xn,116]))*0.057,0)
		tmp2[is.na(tmp2)]<-0
		tmp3<-as.numeric(t(y[1+j-xn,116]))-tmp-tmp2
		tmp3[is.na(tmp3)]<-0
		tmp4<-round(as.numeric(t(y[1+j-xn,272]))*0.22,0)
		tmp4[is.na(tmp4)]<-0
		tmp5<-round(as.numeric(t(y[1+j-xn,337]))*0.902,0)
		tmp5[is.na(tmp5)]<-0
		tmp6<-as.numeric(t(y[1+j-xn,337]))-tmp5
		tmp6[is.na(tmp6)]<-0
		y[1+j-xn,116]<-tmp3
		y[1+j-xn,272]<-as.numeric(t(y[1+j-xn,272]))+tmp-tmp4
		y[1+j-xn,357]<-as.numeric(t(y[1+j-xn,357]))+tmp6
		y[1+j-xn,337]<-tmp5+tmp2
		}
	else
		tgh3<-3
	}
for(j in xn:ncol(x2))
	{
	if(as.numeric(substring(dimnames(x2)[[2]][j],2,6))<66.25)
		{
		tgh<-c(0.5,0.167,0.11)
		
	# gloucester, tewkesbury
		tmp<-round(as.numeric(t(y[1+j-xn,311]))*0.14,0)
		tmp[is.na(tmp)]<-0
		tmp2<-as.numeric(t(y[1+j-xn,311]))-tmp
		tmp2[is.na(tmp2)]<-0
		y[1+j-xn,311]<-tmp2
		y[1+j-xn,144]<-as.numeric(t(y[1+j-xn,144]))+tmp
		
	# plymouth, s. hams
		tmp<-round(as.numeric(t(y[1+j-xn,257]))*0.28,0)
		tmp[is.na(tmp)]<-0
		tmp2<-as.numeric(t(y[1+j-xn,257]))-tmp
		tmp2[is.na(tmp2)]<-0
		y[1+j-xn,257]<-tmp2
		y[1+j-xn,232]<-as.numeric(t(y[1+j-xn,232]))+tmp
		}
	else
		tgh<-c(0.167,0.167,0)
		
# cardiff , vale of glamorgan
	tmp<-round(as.numeric(t(x2[,j][x2$PP==1099]))*tgh[1],0)
	tmp[is.na(tmp)]<-0
	tmp2<-round(as.numeric(t(x2[,j][x2$PP==1099]))*tgh[2],0)
	tmp2[is.na(tmp2)]<-0
	tmp3<-as.numeric(t(x2[,j][x2$PP==1099]))-tmp-tmp2
	tmp3[is.na(tmp3)]<-0
	y[1+j-xn,372]<-as.numeric(t(y[1+j-xn,372]))+tmp
	y[1+j-xn,396]<-as.numeric(t(y[1+j-xn,396]))+tmp2
	y[1+j-xn,401]<-as.numeric(t(y[1+j-xn,401]))+tmp3
	
# sheffield, NE derbyshire, chesterfield
	tmp<-round(as.numeric(t(y[1+j-xn,212]))*tgh[3],0)
	tmp[is.na(tmp)]<-0
	tmp2<-round(as.numeric(t(y[1+j-xn,212]))*0.08,0)
	tmp2[is.na(tmp2)]<-0
	tmp3<-as.numeric(t(y[1+j-xn,212]))-tmp-tmp2
	tmp3[is.na(tmp3)]<-0
	y[1+j-xn,280]<-as.numeric(t(y[1+j-xn,280]))+tmp
	y[1+j-xn,93]<-as.numeric(t(y[1+j-xn,93]))+tmp2
	y[1+j-xn,212]<-tmp3
	}
for(j in xn:ncol(x2))
	{
	if(as.numeric(substring(dimnames(x2)[[2]][j],2,6))<64.25)
		{
		
# northampton, n northants
		tmp<-round(as.numeric(t(y[1+j-xn,263]))*0.16,0)
		tmp[is.na(tmp)]<-0
		tmp2<-as.numeric(t(y[1+j-xn,263]))-tmp
		tmp2[is.na(tmp2)]<-0
		y[1+j-xn,263]<-tmp2
		y[1+j-xn,219]<-as.numeric(t(y[1+j-xn,219]))+tmp
		
# london - chislehurst 
		tmp<-round(as.numeric(t(x2[,j][x2$PP==1109]))*0.5,0)
		tmp[is.na(tmp)]<-0
		tmp2<-as.numeric(t(x2[,j][x2$PP==1109]))-tmp
		tmp2[is.na(tmp2)]<-0
		y[1+j-xn,6]<-as.numeric(t(y[1+j-xn,6]))+tmp2
		y[1+j-xn,4]<-as.numeric(t(y[1+j-xn,4]))+tmp
		
# wandsworth , lambeth
		tmp<-round(as.numeric(t(y[1+j-xn,22]))*0.315,0)
		tmp[is.na(tmp)]<-0
		tmp2<-round(as.numeric(t(y[1+j-xn,32]))*0.339,0)
		tmp2[is.na(tmp2)]<-0
		y[1+j-xn,22]<-as.numeric(t(y[1+j-xn,22]))+tmp2-tmp
		y[1+j-xn,32]<-as.numeric(t(y[1+j-xn,32]))-tmp2+tmp
		}
	else
		tgh3<-3
	}

# islwyn, rhymney valley
tmp<-round(as.numeric(t(y[,381]))*0.125,0)
tmp[is.na(tmp)]<-0
tmp2<-as.numeric(t(y[,381]))-tmp
tmp2[is.na(tmp2)]<-0
y[,396]<-as.numeric(t(y[,396]))+tmp
y[,381]<-tmp2
y[,c(1:45,47:105,107:152,154:283,285:403)]
}



dqa944a<-dqr6(dq44a,dqa944a)
dqa944b<-dqr6(dq44b,dqa944b)
dqa945a<-dqr6(dq45a,dqa945a)
dqa945b<-dqr6(dq45b,dqa945b)
dqa946a<-dqr6(dq46a,dqa946a)
dqa946b<-dqr6(dq46b,dqa946b)
dqa947a<-dqr6(dq47a,dqa947a)
dqa947b<-dqr6(dq47b,dqa947b)
dqa948a<-dqr6(dq48a,dqa948a)
dqa948b<-dqr6(dq48b,dqa948b)
dqa949a<-dqr6(dq49a,dqa949a)
dqa949b<-dqr6(dq49b,dqa949b)
dqa950a<-dqr6(dq50a,dqa950a)
dqa950b<-dqr6(dq50b,dqa950b)
dqa951a<-dqr6(dq51a,dqa951a)
dqa951b<-dqr6(dq51b,dqa951b)
dqa952a<-dqr6(dq52a,dqa952a)
dqa952b<-dqr6(dq52b,dqa952b)
dqa953a<-dqr6(dq53a,dqa953a)
dqa953b<-dqr6(dq53b,dqa953b)
dqa954a<-dqr6(dq54a,dqa954a)
dqa954b<-dqr6(dq54b,dqa954b)
dqa955a<-dqr6(dq55a,dqa955a)
dqa955b<-dqr6(dq55b,dqa955b)
dqa956a<-dqr6(dq56a,dqa956a)
dqa956b<-dqr6(dq56b,dqa956b)
dqa957a<-dqr6(dq57a,dqa957a)
dqa957b<-dqr6(dq57b,dqa957b)
dqa958a<-dqr6(dq58a,dqa958a)
dqa958b<-dqr6(dq58b,dqa958b)
dqa959a<-dqr6(dq59a,dqa959a)
dqa959b<-dqr6(dq59b,dqa959b)
dqa960a<-dqr6(dq60a,dqa960a)
dqa960b<-dqr6(dq60b,dqa960b)
dqa961a<-dqr6(dq61a,dqa961a)
dqa961b<-dqr6(dq61b,dqa961b)
dqa962a<-dqr6(dq62a,dqa962a)
dqa962b<-dqr6(dq62b,dqa962b)
dqa963a<-dqr6(dq63a,dqa963a)
dqa963b<-dqr6(dq63b,dqa963b)
dqa964a<-dqr6(dq64a,dqa964a)
dqa964b<-dqr6(dq64b,dqa964b)
dqa965i<-dqr6(dq65i,dqa965i)
dqa965ii<-dqr6(dq65ii,dqa965ii)
dqa966a<-dqr6(dq66a,dqa966a)
dqa966b<-dqr6(dq66b,dqa966b)
dqa967a<-dqr6(dq67a,dqa967a)
dqa967b<-dqr6(dq67b,dqa967b)
dqa968a<-dqr6(dq68a,dqa968a)
dqa968b<-dqr6(dq68b,dqa968b)
dqa969a<-dqr6(dq69a,dqa969a)
dqa969b<-dqr6(dq69b,dqa969b)
dqa970a<-dqr6(dq70a,dqa970a)
dqa970b<-dqr6(dq70b,dqa970b)
dqa971a<-dqr6(dq71a,dqa971a)
dqa971b<-dqr6(dq71b,dqa971b)
dqa972a<-dqr6(dq72a,dqa972a)
dqa972b<-dqr6(dq72b,dqa972b)
dqa973a<-dqr6(dq73a,dqa973a)
dqa973b<-dqr6(dq73b,dqa973b)
dqa974i<-dqr6(dq74i,dqa974i)

# e&w L 44-94

dql944a<-dqa944a[,1:33]
dql944b<-dqa944b[,1:33]
dql945a<-dqa945a[,1:33]
dql945b<-dqa945b[,1:33]
dql946a<-dqa946a[,1:33]
dql946b<-dqa946b[,1:33]
dql947a<-dqa947a[,1:33]
dql947b<-dqa947b[,1:33]
dql948a<-dqa948a[,1:33]
dql948b<-dqa948b[,1:33]
dql949a<-dqa949a[,1:33]
dql949b<-dqa949b[,1:33]
dql950a<-dqa950a[,1:33]
dql950b<-dqa950b[,1:33]
dql951a<-dqa951a[,1:33]
dql951b<-dqa951b[,1:33]
dql952a<-dqa952a[,1:33]
dql952b<-dqa952b[,1:33]
dql953a<-dqa953a[,1:33]
dql953b<-dqa953b[,1:33]
dql954a<-dqa954a[,1:33]
dql954b<-dqa954b[,1:33]
dql955a<-dqa955a[,1:33]
dql955b<-dqa955b[,1:33]
dql956a<-dqa956a[,1:33]
dql956b<-dqa956b[,1:33]
dql957a<-dqa957a[,1:33]
dql957b<-dqa957b[,1:33]
dql958a<-dqa958a[,1:33]
dql958b<-dqa958b[,1:33]
dql959a<-dqa959a[,1:33]
dql959b<-dqa959b[,1:33]
dql960a<-dqa960a[,1:33]
dql960b<-dqa960b[,1:33]
dql961a<-dqa961a[,1:33]
dql961b<-dqa961b[,1:33]
dql962a<-dqa962a[,1:33]
dql962b<-dqa962b[,1:33]
dql963a<-dqa963a[,1:33]
dql963b<-dqa963b[,1:33]
dql964a<-dqa964a[,1:33]
dql964b<-dqa964b[,1:33]
dql965i<-dqa965i[,1:33]
dql965ii<-dqa965ii[,1:33]
dql966a<-dqa966a[,1:33]
dql966b<-dqa966b[,1:33]
dql967a<-dqa967a[,1:33]
dql967b<-dqa967b[,1:33]
dql968a<-dqa968a[,1:33]
dql968b<-dqa968b[,1:33]
dql969a<-dqa969a[,1:33]
dql969b<-dqa969b[,1:33]
dql970a<-dqa970a[,1:33]
dql970b<-dqa970b[,1:33]
dql971a<-dqa971a[,1:33]
dql971b<-dqa971b[,1:33]
dql972a<-dqa972a[,1:33]
dql972b<-dqa972b[,1:33]
dql973a<-dqa973a[,1:33]
dql973b<-dqa973b[,1:33]
dql974i<-dqa974i[,1:33]
dql974ii<-dqa974ii[,1:33]
dql975<-dqa975[,1:33]
dql976<-dqa976[,1:33]
dql977<-dqa977[,1:33]
dql978<-dqa978[,1:33]
dql979<-dqa979[,1:33]
dql980<-dqa980[,1:33]
dql981<-dqa981[,1:33]
dql982<-dqa982[,1:33]
dql983<-dqa983[,1:33]
dql984<-dqa984[,1:33]
dql985<-dqa985[,1:33]
dql986<-dqa986[,1:33]
dql987<-dqa987[,1:33]
dql988<-dqa988[,1:33]
dql989<-dqa989[,1:33]
dql990<-dqa990[,1:33]
dql991<-dqa991[,1:33]
dql992<-dqa992[,1:33]
dql993<-dqa993[,1:33]
dql994<-dqa994[,1:33]

dqt944a<-dqa944a[,34:399]
dqt944b<-dqa944b[,34:399]
dqt945a<-dqa945a[,34:399]
dqt945b<-dqa945b[,34:399]
dqt946a<-dqa946a[,34:399]
dqt946b<-dqa946b[,34:399]
dqt947a<-dqa947a[,34:399]
dqt947b<-dqa947b[,34:399]
dqt948a<-dqa948a[,34:399]
dqt948b<-dqa948b[,34:399]
dqt949a<-dqa949a[,34:399]
dqt949b<-dqa949b[,34:399]
dqt950a<-dqa950a[,34:399]
dqt950b<-dqa950b[,34:399]
dqt951a<-dqa951a[,34:399]
dqt951b<-dqa951b[,34:399]
dqt952a<-dqa952a[,34:399]
dqt952b<-dqa952b[,34:399]
dqt953a<-dqa953a[,34:399]
dqt953b<-dqa953b[,34:399]
dqt954a<-dqa954a[,34:399]
dqt954b<-dqa954b[,34:399]
dqt955a<-dqa955a[,34:399]
dqt955b<-dqa955b[,34:399]
dqt956a<-dqa956a[,34:399]
dqt956b<-dqa956b[,34:399]
dqt957a<-dqa957a[,34:399]
dqt957b<-dqa957b[,34:399]
dqt958a<-dqa958a[,34:399]
dqt958b<-dqa958b[,34:399]
dqt959a<-dqa959a[,34:399]
dqt959b<-dqa959b[,34:399]
dqt960a<-dqa960a[,34:399]
dqt960b<-dqa960b[,34:399]
dqt961a<-dqa961a[,34:399]
dqt961b<-dqa961b[,34:399]
dqt962a<-dqa962a[,34:399]
dqt962b<-dqa962b[,34:399]
dqt963a<-dqa963a[,34:399]
dqt963b<-dqa963b[,34:399]
dqt964a<-dqa964a[,34:399]
dqt964b<-dqa964b[,34:399]
dqt965i<-dqa965i[,34:399]
dqt965ii<-dqa965ii[,34:399]
dqt966a<-dqa966a[,34:399]
dqt966b<-dqa966b[,34:399]
dqt967a<-dqa967a[,34:399]
dqt967b<-dqa967b[,34:399]
dqt968a<-dqa968a[,34:399]
dqt968b<-dqa968b[,34:399]
dqt969a<-dqa969a[,34:399]
dqt969b<-dqa969b[,34:399]
dqt970a<-dqa970a[,34:399]
dqt970b<-dqa970b[,34:399]
dqt971a<-dqa971a[,34:399]
dqt971b<-dqa971b[,34:399]
dqt972a<-dqa972a[,34:399]
dqt972b<-dqa972b[,34:399]
dqt973a<-dqa973a[,34:399]
dqt973b<-dqa973b[,34:399]
dqt974i<-dqa974i[,34:399]
dqt974ii<-dqa974ii[,34:399]
dqt975<-dqa975[,34:399]
dqt976<-dqa976[,34:399]
dqt977<-dqa977[,34:399]
dqt978<-dqa978[,34:399]
dqt979<-dqa979[,34:399]
dqt980<-dqa980[,34:399]
dqt981<-dqa981[,34:399]
dqt982<-dqa982[,34:399]
dqt983<-dqa983[,34:399]
dqt984<-dqa984[,34:399]
dqt985<-dqa985[,34:399]
dqt986<-dqa986[,34:399]
dqt987<-dqa987[,34:399]
dqt988<-dqa988[,34:399]
dqt989<-dqa989[,34:399]
dqt990<-dqa990[,34:399]
dqt991<-dqa991[,34:399]
dqt992<-dqa992[,34:399]
dqt993<-dqa993[,34:399]
dqt994<-dqa994[,34:399]

# create vector of weeks 1944-1994

ewtime<-as.numeric(c(dimnames(dql944a)[[1]],dimnames(dql944b)[[1]],dimnames(dql945a)[[1]],dimnames(dql945b)[[1]],dimnames(dql946a)[[1]],dimnames(dql946b)[[1]],dimnames(dql947a)[[1]],dimnames(dql947b)[[1]],dimnames(dql948a)[[1]],dimnames(dql948b)[[1]],dimnames(dql949a)[[1]],dimnames(dql949b)[[1]],dimnames(dql950a)[[1]],dimnames(dql950b)[[1]],dimnames(dql951a)[[1]],dimnames(dql951b)[[1]],dimnames(dql952a)[[1]],dimnames(dql952b)[[1]],dimnames(dql953a)[[1]],dimnames(dql953b)[[1]],dimnames(dql954a)[[1]],dimnames(dql954b)[[1]],dimnames(dql955a)[[1]],dimnames(dql955b)[[1]],dimnames(dql956a)[[1]],dimnames(dql956b)[[1]],dimnames(dql957a)[[1]],dimnames(dql957b)[[1]],dimnames(dql958a)[[1]],dimnames(dql958b)[[1]],dimnames(dql959a)[[1]],dimnames(dql959b)[[1]],dimnames(dql960a)[[1]],dimnames(dql960b)[[1]],dimnames(dql961a)[[1]],dimnames(dql961b)[[1]],dimnames(dql962a)[[1]],dimnames(dql962b)[[1]],dimnames(dql963a)[[1]],dimnames(dql963b)[[1]],dimnames(dql964a)[[1]],dimnames(dql964b)[[1]],dimnames(dql965i)[[1]],dimnames(dql965ii)[[1]],dimnames(dql966a)[[1]],dimnames(dql966b)[[1]],dimnames(dql967a)[[1]],dimnames(dql967b)[[1]],dimnames(dql968a)[[1]],dimnames(dql968b)[[1]],dimnames(dql969a)[[1]],dimnames(dql969b)[[1]],dimnames(dql970a)[[1]],dimnames(dql970b)[[1]],dimnames(dql971a)[[1]],dimnames(dql971b)[[1]],dimnames(dql972a)[[1]],dimnames(dql972b)[[1]],dimnames(dql973a)[[1]],dimnames(dql973b)[[1]],dimnames(dql974i)[[1]],dimnames(dql974ii)[[1]],dimnames(dql975)[[1]],dimnames(dql976)[[1]],dimnames(dql977)[[1]],dimnames(dql978)[[1]],dimnames(dql979)[[1]],dimnames(dql980)[[1]],dimnames(dql981)[[1]],dimnames(dql982)[[1]],dimnames(dql983)[[1]],dimnames(dql984)[[1]],dimnames(dql985)[[1]],dimnames(dql986)[[1]],dimnames(dql987)[[1]],dimnames(dql988)[[1]],dimnames(dql989)[[1]],dimnames(dql990)[[1]],dimnames(dql991)[[1]],dimnames(dql992)[[1]],dimnames(dql993)[[1]],dimnames(dql994)[[1]]))

# create where spreadsheets fit into to overall database

ewtime2<-c(0,26,52,78,104,130,156,182,209,235,261,287,313,339,365,391,417,443,470,496,522,548,574,600,626,652,678,704,730,756,783,809,835,861,887,913,939,965,991,1017,1043,1069,1096,1108,1148,1174,1200,1226,1252,1278,1305,1331,1357,1383,1409,1435,1461,1487,1513,1539,1565,1578,1618,1670,1722,1774,1826,1878,1931,1983,2035,2087,2139,2191,2244,2296,2348,2400,2452,2504,2557,2609,2661)

# create matrices

dqu4465<-matrix(data=NA,ncol=954,nrow=1108,dimnames=list(ewtime[1:1108],ewu4465nam))
dql4465<-matrix(data=NA,ncol=29,nrow=1108,dimnames=list(ewtime[1:1108],ewl4465nam))
dqr4465<-matrix(data=NA,ncol=468,nrow=1108,dimnames=list(ewtime[1:1108],ewr4465nam))
dqt4494<-matrix(data=NA,ncol=366,nrow=2661,dimnames=list(ewtime,ewt4494nam))
dql4494<-matrix(data=NA,ncol=33,nrow=2661,dimnames=list(ewtime,ewl4494nam))
dqu4474<-matrix(data=NA,ncol=845,nrow=1578,dimnames=list(ewtime[1:1578],ewu4474nam))
dqr4474<-matrix(data=NA,ncol=457,nrow=1578,dimnames=list(ewtime[1:1578],ewr4474nam))

# fill matrices

dqr4465[(ewtime2[1]+1):ewtime2[2],1:468]<-dqr644a[1:nrow(dqr644a),1:468]
dqr4465[(ewtime2[2]+1):ewtime2[3],1:468]<-dqr644b[1:nrow(dqr644b),1:468]
dqr4465[(ewtime2[3]+1):ewtime2[4],1:468]<-dqr645a[1:nrow(dqr645a),1:468]
dqr4465[(ewtime2[4]+1):ewtime2[5],1:468]<-dqr645b[1:nrow(dqr645b),1:468]
dqr4465[(ewtime2[5]+1):ewtime2[6],1:468]<-dqr646a[1:nrow(dqr646a),1:468]
dqr4465[(ewtime2[6]+1):ewtime2[7],1:468]<-dqr646b[1:nrow(dqr646b),1:468]
dqr4465[(ewtime2[7]+1):ewtime2[8],1:468]<-dqr647a[1:nrow(dqr647a),1:468]
dqr4465[(ewtime2[8]+1):ewtime2[9],1:468]<-dqr647b[1:nrow(dqr647b),1:468]
dqr4465[(ewtime2[9]+1):ewtime2[10],1:468]<-dqr648a[1:nrow(dqr648a),1:468]
dqr4465[(ewtime2[10]+1):ewtime2[11],1:468]<-dqr648b[1:nrow(dqr648b),1:468]
dqr4465[(ewtime2[11]+1):ewtime2[12],1:468]<-dqr649a[1:nrow(dqr649a),1:468]
dqr4465[(ewtime2[12]+1):ewtime2[13],1:468]<-dqr649b[1:nrow(dqr649b),1:468]
dqr4465[(ewtime2[13]+1):ewtime2[14],1:468]<-dqr650a[1:nrow(dqr650a),1:468]
dqr4465[(ewtime2[14]+1):ewtime2[15],1:468]<-dqr650b[1:nrow(dqr650b),1:468]
dqr4465[(ewtime2[15]+1):ewtime2[16],1:468]<-dqr651a[1:nrow(dqr651a),1:468]
dqr4465[(ewtime2[16]+1):ewtime2[17],1:468]<-dqr651b[1:nrow(dqr651b),1:468]
dqr4465[(ewtime2[17]+1):ewtime2[18],1:468]<-dqr652a[1:nrow(dqr652a),1:468]
dqr4465[(ewtime2[18]+1):ewtime2[19],1:468]<-dqr652b[1:nrow(dqr652b),1:468]
dqr4465[(ewtime2[19]+1):ewtime2[20],1:468]<-dqr653a[1:nrow(dqr653a),1:468]
dqr4465[(ewtime2[20]+1):ewtime2[21],1:468]<-dqr653b[1:nrow(dqr653b),1:468]
dqr4465[(ewtime2[21]+1):ewtime2[22],1:468]<-dqr654a[1:nrow(dqr654a),1:468]
dqr4465[(ewtime2[22]+1):ewtime2[23],1:468]<-dqr654b[1:nrow(dqr654b),1:468]
dqr4465[(ewtime2[23]+1):ewtime2[24],1:468]<-dqr655a[1:nrow(dqr655a),1:468]
dqr4465[(ewtime2[24]+1):ewtime2[25],1:468]<-dqr655b[1:nrow(dqr655b),1:468]
dqr4465[(ewtime2[25]+1):ewtime2[26],1:468]<-dqr656a[1:nrow(dqr656a),1:468]
dqr4465[(ewtime2[26]+1):ewtime2[27],1:468]<-dqr656b[1:nrow(dqr656b),1:468]
dqr4465[(ewtime2[27]+1):ewtime2[28],1:468]<-dqr657a[1:nrow(dqr657a),1:468]
dqr4465[(ewtime2[28]+1):ewtime2[29],1:468]<-dqr657b[1:nrow(dqr657b),1:468]
dqr4465[(ewtime2[29]+1):ewtime2[30],1:468]<-dqr658a[1:nrow(dqr658a),1:468]
dqr4465[(ewtime2[30]+1):ewtime2[31],1:468]<-dqr658b[1:nrow(dqr658b),1:468]
dqr4465[(ewtime2[31]+1):ewtime2[32],1:468]<-dqr659a[1:nrow(dqr659a),1:468]
dqr4465[(ewtime2[32]+1):ewtime2[33],1:468]<-dqr659b[1:nrow(dqr659b),1:468]
dqr4465[(ewtime2[33]+1):ewtime2[34],1:468]<-dqr660a[1:nrow(dqr660a),1:468]
dqr4465[(ewtime2[34]+1):ewtime2[35],1:468]<-dqr660b[1:nrow(dqr660b),1:468]
dqr4465[(ewtime2[35]+1):ewtime2[36],1:468]<-dqr661a[1:nrow(dqr661a),1:468]
dqr4465[(ewtime2[36]+1):ewtime2[37],1:468]<-dqr661b[1:nrow(dqr661b),1:468]
dqr4465[(ewtime2[37]+1):ewtime2[38],1:468]<-dqr662a[1:nrow(dqr662a),1:468]
dqr4465[(ewtime2[38]+1):ewtime2[39],1:468]<-dqr662b[1:nrow(dqr662b),1:468]
dqr4465[(ewtime2[39]+1):ewtime2[40],1:468]<-dqr663a[1:nrow(dqr663a),1:468]
dqr4465[(ewtime2[40]+1):ewtime2[41],1:468]<-dqr663b[1:nrow(dqr663b),1:468]
dqr4465[(ewtime2[41]+1):ewtime2[42],1:468]<-dqr664a[1:nrow(dqr664a),1:468]
dqr4465[(ewtime2[42]+1):ewtime2[43],1:468]<-dqr664b[1:nrow(dqr664b),1:468]
dqr4465[(ewtime2[43]+1):ewtime2[44],1:468]<-dqr665i[1:nrow(dqr665i),1:468]

# check on NA's
table(is.na(t(dqr4465)))
468*1108

dql4465[(ewtime2[1]+1):ewtime2[2],1:29]<-dql644a[1:nrow(dql644a),1:29]
dql4465[(ewtime2[2]+1):ewtime2[3],1:29]<-dql644b[1:nrow(dql644b),1:29]
dql4465[(ewtime2[3]+1):ewtime2[4],1:29]<-dql645a[1:nrow(dql645a),1:29]
dql4465[(ewtime2[4]+1):ewtime2[5],1:29]<-dql645b[1:nrow(dql645b),1:29]
dql4465[(ewtime2[5]+1):ewtime2[6],1:29]<-dql646a[1:nrow(dql646a),1:29]
dql4465[(ewtime2[6]+1):ewtime2[7],1:29]<-dql646b[1:nrow(dql646b),1:29]
dql4465[(ewtime2[7]+1):ewtime2[8],1:29]<-dql647a[1:nrow(dql647a),1:29]
dql4465[(ewtime2[8]+1):ewtime2[9],1:29]<-dql647b[1:nrow(dql647b),1:29]
dql4465[(ewtime2[9]+1):ewtime2[10],1:29]<-dql648a[1:nrow(dql648a),1:29]
dql4465[(ewtime2[10]+1):ewtime2[11],1:29]<-dql648b[1:nrow(dql648b),1:29]
dql4465[(ewtime2[11]+1):ewtime2[12],1:29]<-dql649a[1:nrow(dql649a),1:29]
dql4465[(ewtime2[12]+1):ewtime2[13],1:29]<-dql649b[1:nrow(dql649b),1:29]
dql4465[(ewtime2[13]+1):ewtime2[14],1:29]<-dql650a[1:nrow(dql650a),1:29]
dql4465[(ewtime2[14]+1):ewtime2[15],1:29]<-dql650b[1:nrow(dql650b),1:29]
dql4465[(ewtime2[15]+1):ewtime2[16],1:29]<-dql651a[1:nrow(dql651a),1:29]
dql4465[(ewtime2[16]+1):ewtime2[17],1:29]<-dql651b[1:nrow(dql651b),1:29]
dql4465[(ewtime2[17]+1):ewtime2[18],1:29]<-dql652a[1:nrow(dql652a),1:29]
dql4465[(ewtime2[18]+1):ewtime2[19],1:29]<-dql652b[1:nrow(dql652b),1:29]
dql4465[(ewtime2[19]+1):ewtime2[20],1:29]<-dql653a[1:nrow(dql653a),1:29]
dql4465[(ewtime2[20]+1):ewtime2[21],1:29]<-dql653b[1:nrow(dql653b),1:29]
dql4465[(ewtime2[21]+1):ewtime2[22],1:29]<-dql654a[1:nrow(dql654a),1:29]
dql4465[(ewtime2[22]+1):ewtime2[23],1:29]<-dql654b[1:nrow(dql654b),1:29]
dql4465[(ewtime2[23]+1):ewtime2[24],1:29]<-dql655a[1:nrow(dql655a),1:29]
dql4465[(ewtime2[24]+1):ewtime2[25],1:29]<-dql655b[1:nrow(dql655b),1:29]
dql4465[(ewtime2[25]+1):ewtime2[26],1:29]<-dql656a[1:nrow(dql656a),1:29]
dql4465[(ewtime2[26]+1):ewtime2[27],1:29]<-dql656b[1:nrow(dql656b),1:29]
dql4465[(ewtime2[27]+1):ewtime2[28],1:29]<-dql657a[1:nrow(dql657a),1:29]
dql4465[(ewtime2[28]+1):ewtime2[29],1:29]<-dql657b[1:nrow(dql657b),1:29]
dql4465[(ewtime2[29]+1):ewtime2[30],1:29]<-dql658a[1:nrow(dql658a),1:29]
dql4465[(ewtime2[30]+1):ewtime2[31],1:29]<-dql658b[1:nrow(dql658b),1:29]
dql4465[(ewtime2[31]+1):ewtime2[32],1:29]<-dql659a[1:nrow(dql659a),1:29]
dql4465[(ewtime2[32]+1):ewtime2[33],1:29]<-dql659b[1:nrow(dql659b),1:29]
dql4465[(ewtime2[33]+1):ewtime2[34],1:29]<-dql660a[1:nrow(dql660a),1:29]
dql4465[(ewtime2[34]+1):ewtime2[35],1:29]<-dql660b[1:nrow(dql660b),1:29]
dql4465[(ewtime2[35]+1):ewtime2[36],1:29]<-dql661a[1:nrow(dql661a),1:29]
dql4465[(ewtime2[36]+1):ewtime2[37],1:29]<-dql661b[1:nrow(dql661b),1:29]
dql4465[(ewtime2[37]+1):ewtime2[38],1:29]<-dql662a[1:nrow(dql662a),1:29]
dql4465[(ewtime2[38]+1):ewtime2[39],1:29]<-dql662b[1:nrow(dql662b),1:29]
dql4465[(ewtime2[39]+1):ewtime2[40],1:29]<-dql663a[1:nrow(dql663a),1:29]
dql4465[(ewtime2[40]+1):ewtime2[41],1:29]<-dql663b[1:nrow(dql663b),1:29]
dql4465[(ewtime2[41]+1):ewtime2[42],1:29]<-dql664a[1:nrow(dql664a),1:29]
dql4465[(ewtime2[42]+1):ewtime2[43],1:29]<-dql664b[1:nrow(dql664b),1:29]
dql4465[(ewtime2[43]+1):ewtime2[44],1:29]<-dql665i[1:nrow(dql665i),1:29]

table(is.na(t(dql4465)))
29*1108

dqu4465[(ewtime2[1]+1):ewtime2[2],1:954]<-dqu644a[1:nrow(dqu644a),1:954]
dqu4465[(ewtime2[2]+1):ewtime2[3],1:954]<-dqu644b[1:nrow(dqu644b),1:954]
dqu4465[(ewtime2[3]+1):ewtime2[4],1:954]<-dqu645a[1:nrow(dqu645a),1:954]
dqu4465[(ewtime2[4]+1):ewtime2[5],1:954]<-dqu645b[1:nrow(dqu645b),1:954]
dqu4465[(ewtime2[5]+1):ewtime2[6],1:954]<-dqu646a[1:nrow(dqu646a),1:954]
dqu4465[(ewtime2[6]+1):ewtime2[7],1:954]<-dqu646b[1:nrow(dqu646b),1:954]
dqu4465[(ewtime2[7]+1):ewtime2[8],1:954]<-dqu647a[1:nrow(dqu647a),1:954]
dqu4465[(ewtime2[8]+1):ewtime2[9],1:954]<-dqu647b[1:nrow(dqu647b),1:954]
dqu4465[(ewtime2[9]+1):ewtime2[10],1:954]<-dqu648a[1:nrow(dqu648a),1:954]
dqu4465[(ewtime2[10]+1):ewtime2[11],1:954]<-dqu648b[1:nrow(dqu648b),1:954]
dqu4465[(ewtime2[11]+1):ewtime2[12],1:954]<-dqu649a[1:nrow(dqu649a),1:954]
dqu4465[(ewtime2[12]+1):ewtime2[13],1:954]<-dqu649b[1:nrow(dqu649b),1:954]
dqu4465[(ewtime2[13]+1):ewtime2[14],1:954]<-dqu650a[1:nrow(dqu650a),1:954]
dqu4465[(ewtime2[14]+1):ewtime2[15],1:954]<-dqu650b[1:nrow(dqu650b),1:954]
dqu4465[(ewtime2[15]+1):ewtime2[16],1:954]<-dqu651a[1:nrow(dqu651a),1:954]
dqu4465[(ewtime2[16]+1):ewtime2[17],1:954]<-dqu651b[1:nrow(dqu651b),1:954]
dqu4465[(ewtime2[17]+1):ewtime2[18],1:954]<-dqu652a[1:nrow(dqu652a),1:954]
dqu4465[(ewtime2[18]+1):ewtime2[19],1:954]<-dqu652b[1:nrow(dqu652b),1:954]
dqu4465[(ewtime2[19]+1):ewtime2[20],1:954]<-dqu653a[1:nrow(dqu653a),1:954]
dqu4465[(ewtime2[20]+1):ewtime2[21],1:954]<-dqu653b[1:nrow(dqu653b),1:954]
dqu4465[(ewtime2[21]+1):ewtime2[22],1:954]<-dqu654a[1:nrow(dqu654a),1:954]
dqu4465[(ewtime2[22]+1):ewtime2[23],1:954]<-dqu654b[1:nrow(dqu654b),1:954]
dqu4465[(ewtime2[23]+1):ewtime2[24],1:954]<-dqu655a[1:nrow(dqu655a),1:954]
dqu4465[(ewtime2[24]+1):ewtime2[25],1:954]<-dqu655b[1:nrow(dqu655b),1:954]
dqu4465[(ewtime2[25]+1):ewtime2[26],1:954]<-dqu656a[1:nrow(dqu656a),1:954]
dqu4465[(ewtime2[26]+1):ewtime2[27],1:954]<-dqu656b[1:nrow(dqu656b),1:954]
dqu4465[(ewtime2[27]+1):ewtime2[28],1:954]<-dqu657a[1:nrow(dqu657a),1:954]
dqu4465[(ewtime2[28]+1):ewtime2[29],1:954]<-dqu657b[1:nrow(dqu657b),1:954]
dqu4465[(ewtime2[29]+1):ewtime2[30],1:954]<-dqu658a[1:nrow(dqu658a),1:954]
dqu4465[(ewtime2[30]+1):ewtime2[31],1:954]<-dqu658b[1:nrow(dqu658b),1:954]
dqu4465[(ewtime2[31]+1):ewtime2[32],1:954]<-dqu659a[1:nrow(dqu659a),1:954]
dqu4465[(ewtime2[32]+1):ewtime2[33],1:954]<-dqu659b[1:nrow(dqu659b),1:954]
dqu4465[(ewtime2[33]+1):ewtime2[34],1:954]<-dqu660a[1:nrow(dqu660a),1:954]
dqu4465[(ewtime2[34]+1):ewtime2[35],1:954]<-dqu660b[1:nrow(dqu660b),1:954]
dqu4465[(ewtime2[35]+1):ewtime2[36],1:954]<-dqu661a[1:nrow(dqu661a),1:954]
dqu4465[(ewtime2[36]+1):ewtime2[37],1:954]<-dqu661b[1:nrow(dqu661b),1:954]
dqu4465[(ewtime2[37]+1):ewtime2[38],1:954]<-dqu662a[1:nrow(dqu662a),1:954]
dqu4465[(ewtime2[38]+1):ewtime2[39],1:954]<-dqu662b[1:nrow(dqu662b),1:954]
dqu4465[(ewtime2[39]+1):ewtime2[40],1:954]<-dqu663a[1:nrow(dqu663a),1:954]
dqu4465[(ewtime2[40]+1):ewtime2[41],1:954]<-dqu663b[1:nrow(dqu663b),1:954]
dqu4465[(ewtime2[41]+1):ewtime2[42],1:954]<-dqu664a[1:nrow(dqu664a),1:954]
dqu4465[(ewtime2[42]+1):ewtime2[43],1:954]<-dqu664b[1:nrow(dqu664b),1:954]
dqu4465[(ewtime2[43]+1):ewtime2[44],1:954]<-dqu665i[1:nrow(dqu665i),1:954]

table(is.na(t(dqu4465)))
954*1108

dqr4474[(ewtime2[1]+1):ewtime2[2],1:457]<-dqr744a[1:nrow(dqr744a),1:457]
dqr4474[(ewtime2[2]+1):ewtime2[3],1:457]<-dqr744b[1:nrow(dqr744b),1:457]
dqr4474[(ewtime2[3]+1):ewtime2[4],1:457]<-dqr745a[1:nrow(dqr745a),1:457]
dqr4474[(ewtime2[4]+1):ewtime2[5],1:457]<-dqr745b[1:nrow(dqr745b),1:457]
dqr4474[(ewtime2[5]+1):ewtime2[6],1:457]<-dqr746a[1:nrow(dqr746a),1:457]
dqr4474[(ewtime2[6]+1):ewtime2[7],1:457]<-dqr746b[1:nrow(dqr746b),1:457]
dqr4474[(ewtime2[7]+1):ewtime2[8],1:457]<-dqr747a[1:nrow(dqr747a),1:457]
dqr4474[(ewtime2[8]+1):ewtime2[9],1:457]<-dqr747b[1:nrow(dqr747b),1:457]
dqr4474[(ewtime2[9]+1):ewtime2[10],1:457]<-dqr748a[1:nrow(dqr748a),1:457]
dqr4474[(ewtime2[10]+1):ewtime2[11],1:457]<-dqr748b[1:nrow(dqr748b),1:457]
dqr4474[(ewtime2[11]+1):ewtime2[12],1:457]<-dqr749a[1:nrow(dqr749a),1:457]
dqr4474[(ewtime2[12]+1):ewtime2[13],1:457]<-dqr749b[1:nrow(dqr749b),1:457]
dqr4474[(ewtime2[13]+1):ewtime2[14],1:457]<-dqr750a[1:nrow(dqr750a),1:457]
dqr4474[(ewtime2[14]+1):ewtime2[15],1:457]<-dqr750b[1:nrow(dqr750b),1:457]
dqr4474[(ewtime2[15]+1):ewtime2[16],1:457]<-dqr751a[1:nrow(dqr751a),1:457]
dqr4474[(ewtime2[16]+1):ewtime2[17],1:457]<-dqr751b[1:nrow(dqr751b),1:457]
dqr4474[(ewtime2[17]+1):ewtime2[18],1:457]<-dqr752a[1:nrow(dqr752a),1:457]
dqr4474[(ewtime2[18]+1):ewtime2[19],1:457]<-dqr752b[1:nrow(dqr752b),1:457]
dqr4474[(ewtime2[19]+1):ewtime2[20],1:457]<-dqr753a[1:nrow(dqr753a),1:457]
dqr4474[(ewtime2[20]+1):ewtime2[21],1:457]<-dqr753b[1:nrow(dqr753b),1:457]
dqr4474[(ewtime2[21]+1):ewtime2[22],1:457]<-dqr754a[1:nrow(dqr754a),1:457]
dqr4474[(ewtime2[22]+1):ewtime2[23],1:457]<-dqr754b[1:nrow(dqr754b),1:457]
dqr4474[(ewtime2[23]+1):ewtime2[24],1:457]<-dqr755a[1:nrow(dqr755a),1:457]
dqr4474[(ewtime2[24]+1):ewtime2[25],1:457]<-dqr755b[1:nrow(dqr755b),1:457]
dqr4474[(ewtime2[25]+1):ewtime2[26],1:457]<-dqr756a[1:nrow(dqr756a),1:457]
dqr4474[(ewtime2[26]+1):ewtime2[27],1:457]<-dqr756b[1:nrow(dqr756b),1:457]
dqr4474[(ewtime2[27]+1):ewtime2[28],1:457]<-dqr757a[1:nrow(dqr757a),1:457]
dqr4474[(ewtime2[28]+1):ewtime2[29],1:457]<-dqr757b[1:nrow(dqr757b),1:457]
dqr4474[(ewtime2[29]+1):ewtime2[30],1:457]<-dqr758a[1:nrow(dqr758a),1:457]
dqr4474[(ewtime2[30]+1):ewtime2[31],1:457]<-dqr758b[1:nrow(dqr758b),1:457]
dqr4474[(ewtime2[31]+1):ewtime2[32],1:457]<-dqr759a[1:nrow(dqr759a),1:457]
dqr4474[(ewtime2[32]+1):ewtime2[33],1:457]<-dqr759b[1:nrow(dqr759b),1:457]
dqr4474[(ewtime2[33]+1):ewtime2[34],1:457]<-dqr760a[1:nrow(dqr760a),1:457]
dqr4474[(ewtime2[34]+1):ewtime2[35],1:457]<-dqr760b[1:nrow(dqr760b),1:457]
dqr4474[(ewtime2[35]+1):ewtime2[36],1:457]<-dqr761a[1:nrow(dqr761a),1:457]
dqr4474[(ewtime2[36]+1):ewtime2[37],1:457]<-dqr761b[1:nrow(dqr761b),1:457]
dqr4474[(ewtime2[37]+1):ewtime2[38],1:457]<-dqr762a[1:nrow(dqr762a),1:457]
dqr4474[(ewtime2[38]+1):ewtime2[39],1:457]<-dqr762b[1:nrow(dqr762b),1:457]
dqr4474[(ewtime2[39]+1):ewtime2[40],1:457]<-dqr763a[1:nrow(dqr763a),1:457]
dqr4474[(ewtime2[40]+1):ewtime2[41],1:457]<-dqr763b[1:nrow(dqr763b),1:457]
dqr4474[(ewtime2[41]+1):ewtime2[42],1:457]<-dqr764a[1:nrow(dqr764a),1:457]
dqr4474[(ewtime2[42]+1):ewtime2[43],1:457]<-dqr764b[1:nrow(dqr764b),1:457]
dqr4474[(ewtime2[43]+1):ewtime2[44],1:457]<-dqr765i[1:nrow(dqr765i),1:457]
dqr4474[(ewtime2[44]+1):ewtime2[45],1:457]<-dqr765ii[1:nrow(dqr765ii),1:457]
dqr4474[(ewtime2[45]+1):ewtime2[46],1:457]<-dqr766a[1:nrow(dqr766a),1:457]
dqr4474[(ewtime2[46]+1):ewtime2[47],1:457]<-dqr766b[1:nrow(dqr766b),1:457]
dqr4474[(ewtime2[47]+1):ewtime2[48],1:457]<-dqr767a[1:nrow(dqr767a),1:457]
dqr4474[(ewtime2[48]+1):ewtime2[49],1:457]<-dqr767b[1:nrow(dqr767b),1:457]
dqr4474[(ewtime2[49]+1):ewtime2[50],1:457]<-dqr768a[1:nrow(dqr768a),1:457]
dqr4474[(ewtime2[50]+1):ewtime2[51],1:457]<-dqr768b[1:nrow(dqr768b),1:457]
dqr4474[(ewtime2[51]+1):ewtime2[52],1:457]<-dqr769a[1:nrow(dqr769a),1:457]
dqr4474[(ewtime2[52]+1):ewtime2[53],1:457]<-dqr769b[1:nrow(dqr769b),1:457]
dqr4474[(ewtime2[53]+1):ewtime2[54],1:457]<-dqr770a[1:nrow(dqr770a),1:457]
dqr4474[(ewtime2[54]+1):ewtime2[55],1:457]<-dqr770b[1:nrow(dqr770b),1:457]
dqr4474[(ewtime2[55]+1):ewtime2[56],1:457]<-dqr771a[1:nrow(dqr771a),1:457]
dqr4474[(ewtime2[56]+1):ewtime2[57],1:457]<-dqr771b[1:nrow(dqr771b),1:457]
dqr4474[(ewtime2[57]+1):ewtime2[58],1:457]<-dqr772a[1:nrow(dqr772a),1:457]
dqr4474[(ewtime2[58]+1):ewtime2[59],1:457]<-dqr772b[1:nrow(dqr772b),1:457]
dqr4474[(ewtime2[59]+1):ewtime2[60],1:457]<-dqr773a[1:nrow(dqr773a),1:457]
dqr4474[(ewtime2[60]+1):ewtime2[61],1:457]<-dqr773b[1:nrow(dqr773b),1:457]
dqr4474[(ewtime2[61]+1):ewtime2[62],1:457]<-dqr774i[1:nrow(dqr774i),1:457]

table(is.na(t(dqr4474)))
457*1578

dqu4474[(ewtime2[1]+1):ewtime2[2],1:845]<-dqu744a[1:nrow(dqu744a),1:845]
dqu4474[(ewtime2[2]+1):ewtime2[3],1:845]<-dqu744b[1:nrow(dqu744b),1:845]
dqu4474[(ewtime2[3]+1):ewtime2[4],1:845]<-dqu745a[1:nrow(dqu745a),1:845]
dqu4474[(ewtime2[4]+1):ewtime2[5],1:845]<-dqu745b[1:nrow(dqu745b),1:845]
dqu4474[(ewtime2[5]+1):ewtime2[6],1:845]<-dqu746a[1:nrow(dqu746a),1:845]
dqu4474[(ewtime2[6]+1):ewtime2[7],1:845]<-dqu746b[1:nrow(dqu746b),1:845]
dqu4474[(ewtime2[7]+1):ewtime2[8],1:845]<-dqu747a[1:nrow(dqu747a),1:845]
dqu4474[(ewtime2[8]+1):ewtime2[9],1:845]<-dqu747b[1:nrow(dqu747b),1:845]
dqu4474[(ewtime2[9]+1):ewtime2[10],1:845]<-dqu748a[1:nrow(dqu748a),1:845]
dqu4474[(ewtime2[10]+1):ewtime2[11],1:845]<-dqu748b[1:nrow(dqu748b),1:845]
dqu4474[(ewtime2[11]+1):ewtime2[12],1:845]<-dqu749a[1:nrow(dqu749a),1:845]
dqu4474[(ewtime2[12]+1):ewtime2[13],1:845]<-dqu749b[1:nrow(dqu749b),1:845]
dqu4474[(ewtime2[13]+1):ewtime2[14],1:845]<-dqu750a[1:nrow(dqu750a),1:845]
dqu4474[(ewtime2[14]+1):ewtime2[15],1:845]<-dqu750b[1:nrow(dqu750b),1:845]
dqu4474[(ewtime2[15]+1):ewtime2[16],1:845]<-dqu751a[1:nrow(dqu751a),1:845]
dqu4474[(ewtime2[16]+1):ewtime2[17],1:845]<-dqu751b[1:nrow(dqu751b),1:845]
dqu4474[(ewtime2[17]+1):ewtime2[18],1:845]<-dqu752a[1:nrow(dqu752a),1:845]
dqu4474[(ewtime2[18]+1):ewtime2[19],1:845]<-dqu752b[1:nrow(dqu752b),1:845]
dqu4474[(ewtime2[19]+1):ewtime2[20],1:845]<-dqu753a[1:nrow(dqu753a),1:845]
dqu4474[(ewtime2[20]+1):ewtime2[21],1:845]<-dqu753b[1:nrow(dqu753b),1:845]
dqu4474[(ewtime2[21]+1):ewtime2[22],1:845]<-dqu754a[1:nrow(dqu754a),1:845]
dqu4474[(ewtime2[22]+1):ewtime2[23],1:845]<-dqu754b[1:nrow(dqu754b),1:845]
dqu4474[(ewtime2[23]+1):ewtime2[24],1:845]<-dqu755a[1:nrow(dqu755a),1:845]
dqu4474[(ewtime2[24]+1):ewtime2[25],1:845]<-dqu755b[1:nrow(dqu755b),1:845]
dqu4474[(ewtime2[25]+1):ewtime2[26],1:845]<-dqu756a[1:nrow(dqu756a),1:845]
dqu4474[(ewtime2[26]+1):ewtime2[27],1:845]<-dqu756b[1:nrow(dqu756b),1:845]
dqu4474[(ewtime2[27]+1):ewtime2[28],1:845]<-dqu757a[1:nrow(dqu757a),1:845]
dqu4474[(ewtime2[28]+1):ewtime2[29],1:845]<-dqu757b[1:nrow(dqu757b),1:845]
dqu4474[(ewtime2[29]+1):ewtime2[30],1:845]<-dqu758a[1:nrow(dqu758a),1:845]
dqu4474[(ewtime2[30]+1):ewtime2[31],1:845]<-dqu758b[1:nrow(dqu758b),1:845]
dqu4474[(ewtime2[31]+1):ewtime2[32],1:845]<-dqu759a[1:nrow(dqu759a),1:845]
dqu4474[(ewtime2[32]+1):ewtime2[33],1:845]<-dqu759b[1:nrow(dqu759b),1:845]
dqu4474[(ewtime2[33]+1):ewtime2[34],1:845]<-dqu760a[1:nrow(dqu760a),1:845]
dqu4474[(ewtime2[34]+1):ewtime2[35],1:845]<-dqu760b[1:nrow(dqu760b),1:845]
dqu4474[(ewtime2[35]+1):ewtime2[36],1:845]<-dqu761a[1:nrow(dqu761a),1:845]
dqu4474[(ewtime2[36]+1):ewtime2[37],1:845]<-dqu761b[1:nrow(dqu761b),1:845]
dqu4474[(ewtime2[37]+1):ewtime2[38],1:845]<-dqu762a[1:nrow(dqu762a),1:845]
dqu4474[(ewtime2[38]+1):ewtime2[39],1:845]<-dqu762b[1:nrow(dqu762b),1:845]
dqu4474[(ewtime2[39]+1):ewtime2[40],1:845]<-dqu763a[1:nrow(dqu763a),1:845]
dqu4474[(ewtime2[40]+1):ewtime2[41],1:845]<-dqu763b[1:nrow(dqu763b),1:845]
dqu4474[(ewtime2[41]+1):ewtime2[42],1:845]<-dqu764a[1:nrow(dqu764a),1:845]
dqu4474[(ewtime2[42]+1):ewtime2[43],1:845]<-dqu764b[1:nrow(dqu764b),1:845]
dqu4474[(ewtime2[43]+1):ewtime2[44],1:845]<-dqu765i[1:nrow(dqu765i),1:845]
dqu4474[(ewtime2[44]+1):ewtime2[45],1:845]<-dqu765ii[1:nrow(dqu765ii),1:845]
dqu4474[(ewtime2[45]+1):ewtime2[46],1:845]<-dqu766a[1:nrow(dqu766a),1:845]
dqu4474[(ewtime2[46]+1):ewtime2[47],1:845]<-dqu766b[1:nrow(dqu766b),1:845]
dqu4474[(ewtime2[47]+1):ewtime2[48],1:845]<-dqu767a[1:nrow(dqu767a),1:845]
dqu4474[(ewtime2[48]+1):ewtime2[49],1:845]<-dqu767b[1:nrow(dqu767b),1:845]
dqu4474[(ewtime2[49]+1):ewtime2[50],1:845]<-dqu768a[1:nrow(dqu768a),1:845]
dqu4474[(ewtime2[50]+1):ewtime2[51],1:845]<-dqu768b[1:nrow(dqu768b),1:845]
dqu4474[(ewtime2[51]+1):ewtime2[52],1:845]<-dqu769a[1:nrow(dqu769a),1:845]
dqu4474[(ewtime2[52]+1):ewtime2[53],1:845]<-dqu769b[1:nrow(dqu769b),1:845]
dqu4474[(ewtime2[53]+1):ewtime2[54],1:845]<-dqu770a[1:nrow(dqu770a),1:845]
dqu4474[(ewtime2[54]+1):ewtime2[55],1:845]<-dqu770b[1:nrow(dqu770b),1:845]
dqu4474[(ewtime2[55]+1):ewtime2[56],1:845]<-dqu771a[1:nrow(dqu771a),1:845]
dqu4474[(ewtime2[56]+1):ewtime2[57],1:845]<-dqu771b[1:nrow(dqu771b),1:845]
dqu4474[(ewtime2[57]+1):ewtime2[58],1:845]<-dqu772a[1:nrow(dqu772a),1:845]
dqu4474[(ewtime2[58]+1):ewtime2[59],1:845]<-dqu772b[1:nrow(dqu772b),1:845]
dqu4474[(ewtime2[59]+1):ewtime2[60],1:845]<-dqu773a[1:nrow(dqu773a),1:845]
dqu4474[(ewtime2[60]+1):ewtime2[61],1:845]<-dqu773b[1:nrow(dqu773b),1:845]
dqu4474[(ewtime2[61]+1):ewtime2[62],1:845]<-dqu774i[1:nrow(dqu774i),1:845]

table(is.na(t(dqu4474)))
1578*845

dqt4494[(ewtime2[1]+1):ewtime2[2],1:366]<-dqt944a[1:nrow(dqt944a),1:366]
dqt4494[(ewtime2[2]+1):ewtime2[3],1:366]<-dqt944b[1:nrow(dqt944b),1:366]
dqt4494[(ewtime2[3]+1):ewtime2[4],1:366]<-dqt945a[1:nrow(dqt945a),1:366]
dqt4494[(ewtime2[4]+1):ewtime2[5],1:366]<-dqt945b[1:nrow(dqt945b),1:366]
dqt4494[(ewtime2[5]+1):ewtime2[6],1:366]<-dqt946a[1:nrow(dqt946a),1:366]
dqt4494[(ewtime2[6]+1):ewtime2[7],1:366]<-dqt946b[1:nrow(dqt946b),1:366]
dqt4494[(ewtime2[7]+1):ewtime2[8],1:366]<-dqt947a[1:nrow(dqt947a),1:366]
dqt4494[(ewtime2[8]+1):ewtime2[9],1:366]<-dqt947b[1:nrow(dqt947b),1:366]
dqt4494[(ewtime2[9]+1):ewtime2[10],1:366]<-dqt948a[1:nrow(dqt948a),1:366]
dqt4494[(ewtime2[10]+1):ewtime2[11],1:366]<-dqt948b[1:nrow(dqt948b),1:366]
dqt4494[(ewtime2[11]+1):ewtime2[12],1:366]<-dqt949a[1:nrow(dqt949a),1:366]
dqt4494[(ewtime2[12]+1):ewtime2[13],1:366]<-dqt949b[1:nrow(dqt949b),1:366]
dqt4494[(ewtime2[13]+1):ewtime2[14],1:366]<-dqt950a[1:nrow(dqt950a),1:366]
dqt4494[(ewtime2[14]+1):ewtime2[15],1:366]<-dqt950b[1:nrow(dqt950b),1:366]
dqt4494[(ewtime2[15]+1):ewtime2[16],1:366]<-dqt951a[1:nrow(dqt951a),1:366]
dqt4494[(ewtime2[16]+1):ewtime2[17],1:366]<-dqt951b[1:nrow(dqt951b),1:366]
dqt4494[(ewtime2[17]+1):ewtime2[18],1:366]<-dqt952a[1:nrow(dqt952a),1:366]
dqt4494[(ewtime2[18]+1):ewtime2[19],1:366]<-dqt952b[1:nrow(dqt952b),1:366]
dqt4494[(ewtime2[19]+1):ewtime2[20],1:366]<-dqt953a[1:nrow(dqt953a),1:366]
dqt4494[(ewtime2[20]+1):ewtime2[21],1:366]<-dqt953b[1:nrow(dqt953b),1:366]
dqt4494[(ewtime2[21]+1):ewtime2[22],1:366]<-dqt954a[1:nrow(dqt954a),1:366]
dqt4494[(ewtime2[22]+1):ewtime2[23],1:366]<-dqt954b[1:nrow(dqt954b),1:366]
dqt4494[(ewtime2[23]+1):ewtime2[24],1:366]<-dqt955a[1:nrow(dqt955a),1:366]
dqt4494[(ewtime2[24]+1):ewtime2[25],1:366]<-dqt955b[1:nrow(dqt955b),1:366]
dqt4494[(ewtime2[25]+1):ewtime2[26],1:366]<-dqt956a[1:nrow(dqt956a),1:366]
dqt4494[(ewtime2[26]+1):ewtime2[27],1:366]<-dqt956b[1:nrow(dqt956b),1:366]
dqt4494[(ewtime2[27]+1):ewtime2[28],1:366]<-dqt957a[1:nrow(dqt957a),1:366]
dqt4494[(ewtime2[28]+1):ewtime2[29],1:366]<-dqt957b[1:nrow(dqt957b),1:366]
dqt4494[(ewtime2[29]+1):ewtime2[30],1:366]<-dqt958a[1:nrow(dqt958a),1:366]
dqt4494[(ewtime2[30]+1):ewtime2[31],1:366]<-dqt958b[1:nrow(dqt958b),1:366]
dqt4494[(ewtime2[31]+1):ewtime2[32],1:366]<-dqt959a[1:nrow(dqt959a),1:366]
dqt4494[(ewtime2[32]+1):ewtime2[33],1:366]<-dqt959b[1:nrow(dqt959b),1:366]
dqt4494[(ewtime2[33]+1):ewtime2[34],1:366]<-dqt960a[1:nrow(dqt960a),1:366]
dqt4494[(ewtime2[34]+1):ewtime2[35],1:366]<-dqt960b[1:nrow(dqt960b),1:366]
dqt4494[(ewtime2[35]+1):ewtime2[36],1:366]<-dqt961a[1:nrow(dqt961a),1:366]
dqt4494[(ewtime2[36]+1):ewtime2[37],1:366]<-dqt961b[1:nrow(dqt961b),1:366]
dqt4494[(ewtime2[37]+1):ewtime2[38],1:366]<-dqt962a[1:nrow(dqt962a),1:366]
dqt4494[(ewtime2[38]+1):ewtime2[39],1:366]<-dqt962b[1:nrow(dqt962b),1:366]
dqt4494[(ewtime2[39]+1):ewtime2[40],1:366]<-dqt963a[1:nrow(dqt963a),1:366]
dqt4494[(ewtime2[40]+1):ewtime2[41],1:366]<-dqt963b[1:nrow(dqt963b),1:366]
dqt4494[(ewtime2[41]+1):ewtime2[42],1:366]<-dqt964a[1:nrow(dqt964a),1:366]
dqt4494[(ewtime2[42]+1):ewtime2[43],1:366]<-dqt964b[1:nrow(dqt964b),1:366]
dqt4494[(ewtime2[43]+1):ewtime2[44],1:366]<-dqt965i[1:nrow(dqt965i),1:366]
dqt4494[(ewtime2[44]+1):ewtime2[45],1:366]<-dqt965ii[1:nrow(dqt965ii),1:366]
dqt4494[(ewtime2[45]+1):ewtime2[46],1:366]<-dqt966a[1:nrow(dqt966a),1:366]
dqt4494[(ewtime2[46]+1):ewtime2[47],1:366]<-dqt966b[1:nrow(dqt966b),1:366]
dqt4494[(ewtime2[47]+1):ewtime2[48],1:366]<-dqt967a[1:nrow(dqt967a),1:366]
dqt4494[(ewtime2[48]+1):ewtime2[49],1:366]<-dqt967b[1:nrow(dqt967b),1:366]
dqt4494[(ewtime2[49]+1):ewtime2[50],1:366]<-dqt968a[1:nrow(dqt968a),1:366]
dqt4494[(ewtime2[50]+1):ewtime2[51],1:366]<-dqt968b[1:nrow(dqt968b),1:366]
dqt4494[(ewtime2[51]+1):ewtime2[52],1:366]<-dqt969a[1:nrow(dqt969a),1:366]
dqt4494[(ewtime2[52]+1):ewtime2[53],1:366]<-dqt969b[1:nrow(dqt969b),1:366]
dqt4494[(ewtime2[53]+1):ewtime2[54],1:366]<-dqt970a[1:nrow(dqt970a),1:366]
dqt4494[(ewtime2[54]+1):ewtime2[55],1:366]<-dqt970b[1:nrow(dqt970b),1:366]
dqt4494[(ewtime2[55]+1):ewtime2[56],1:366]<-dqt971a[1:nrow(dqt971a),1:366]
dqt4494[(ewtime2[56]+1):ewtime2[57],1:366]<-dqt971b[1:nrow(dqt971b),1:366]
dqt4494[(ewtime2[57]+1):ewtime2[58],1:366]<-dqt972a[1:nrow(dqt972a),1:366]
dqt4494[(ewtime2[58]+1):ewtime2[59],1:366]<-dqt972b[1:nrow(dqt972b),1:366]
dqt4494[(ewtime2[59]+1):ewtime2[60],1:366]<-dqt973a[1:nrow(dqt973a),1:366]
dqt4494[(ewtime2[60]+1):ewtime2[61],1:366]<-dqt973b[1:nrow(dqt973b),1:366]
dqt4494[(ewtime2[61]+1):ewtime2[62],1:366]<-dqt974i[1:nrow(dqt974i),1:366]
dqt4494[(ewtime2[62]+1):ewtime2[63],1:366]<-dqt974ii[1:nrow(dqt974ii),1:366]
dqt4494[(ewtime2[63]+1):ewtime2[64],1:366]<-dqt975[1:nrow(dqt975),1:366]
dqt4494[(ewtime2[64]+1):ewtime2[65],1:366]<-dqt976[1:nrow(dqt976),1:366]
dqt4494[(ewtime2[65]+1):ewtime2[66],1:366]<-dqt977[1:nrow(dqt977),1:366]
dqt4494[(ewtime2[66]+1):ewtime2[67],1:366]<-dqt978[1:nrow(dqt978),1:366]
dqt4494[(ewtime2[67]+1):ewtime2[68],1:366]<-dqt979[1:nrow(dqt979),1:366]
dqt4494[(ewtime2[68]+1):ewtime2[69],1:366]<-dqt980[1:nrow(dqt980),1:366]
dqt4494[(ewtime2[69]+1):ewtime2[70],1:366]<-dqt981[1:nrow(dqt981),1:366]
dqt4494[(ewtime2[70]+1):ewtime2[71],1:366]<-dqt982[1:nrow(dqt982),1:366]
dqt4494[(ewtime2[71]+1):ewtime2[72],1:366]<-dqt983[1:nrow(dqt983),1:366]
dqt4494[(ewtime2[72]+1):ewtime2[73],1:366]<-dqt984[1:nrow(dqt984),1:366]
dqt4494[(ewtime2[73]+1):ewtime2[74],1:366]<-dqt985[1:nrow(dqt985),1:366]
dqt4494[(ewtime2[74]+1):ewtime2[75],1:366]<-dqt986[1:nrow(dqt986),1:366]
dqt4494[(ewtime2[75]+1):ewtime2[76],1:366]<-dqt987[1:nrow(dqt987),1:366]
dqt4494[(ewtime2[76]+1):ewtime2[77],1:366]<-dqt988[1:nrow(dqt988),1:366]
dqt4494[(ewtime2[77]+1):ewtime2[78],1:366]<-dqt989[1:nrow(dqt989),1:366]
dqt4494[(ewtime2[78]+1):ewtime2[79],1:366]<-dqt990[1:nrow(dqt990),1:366]
dqt4494[(ewtime2[79]+1):ewtime2[80],1:366]<-dqt991[1:nrow(dqt991),1:366]
dqt4494[(ewtime2[80]+1):ewtime2[81],1:366]<-dqt992[1:nrow(dqt992),1:366]
dqt4494[(ewtime2[81]+1):ewtime2[82],1:366]<-dqt993[1:nrow(dqt993),1:366]
dqt4494[(ewtime2[82]+1):ewtime2[83],1:366]<-dqt994[1:nrow(dqt994),1:366]

table(is.na(t(dqt4494)))
366*2661

dql4494[(ewtime2[1]+1):ewtime2[2],1:33]<-dql944a[1:nrow(dql944a),1:33]
dql4494[(ewtime2[2]+1):ewtime2[3],1:33]<-dql944b[1:nrow(dql944b),1:33]
dql4494[(ewtime2[3]+1):ewtime2[4],1:33]<-dql945a[1:nrow(dql945a),1:33]
dql4494[(ewtime2[4]+1):ewtime2[5],1:33]<-dql945b[1:nrow(dql945b),1:33]
dql4494[(ewtime2[5]+1):ewtime2[6],1:33]<-dql946a[1:nrow(dql946a),1:33]
dql4494[(ewtime2[6]+1):ewtime2[7],1:33]<-dql946b[1:nrow(dql946b),1:33]
dql4494[(ewtime2[7]+1):ewtime2[8],1:33]<-dql947a[1:nrow(dql947a),1:33]
dql4494[(ewtime2[8]+1):ewtime2[9],1:33]<-dql947b[1:nrow(dql947b),1:33]
dql4494[(ewtime2[9]+1):ewtime2[10],1:33]<-dql948a[1:nrow(dql948a),1:33]
dql4494[(ewtime2[10]+1):ewtime2[11],1:33]<-dql948b[1:nrow(dql948b),1:33]
dql4494[(ewtime2[11]+1):ewtime2[12],1:33]<-dql949a[1:nrow(dql949a),1:33]
dql4494[(ewtime2[12]+1):ewtime2[13],1:33]<-dql949b[1:nrow(dql949b),1:33]
dql4494[(ewtime2[13]+1):ewtime2[14],1:33]<-dql950a[1:nrow(dql950a),1:33]
dql4494[(ewtime2[14]+1):ewtime2[15],1:33]<-dql950b[1:nrow(dql950b),1:33]
dql4494[(ewtime2[15]+1):ewtime2[16],1:33]<-dql951a[1:nrow(dql951a),1:33]
dql4494[(ewtime2[16]+1):ewtime2[17],1:33]<-dql951b[1:nrow(dql951b),1:33]
dql4494[(ewtime2[17]+1):ewtime2[18],1:33]<-dql952a[1:nrow(dql952a),1:33]
dql4494[(ewtime2[18]+1):ewtime2[19],1:33]<-dql952b[1:nrow(dql952b),1:33]
dql4494[(ewtime2[19]+1):ewtime2[20],1:33]<-dql953a[1:nrow(dql953a),1:33]
dql4494[(ewtime2[20]+1):ewtime2[21],1:33]<-dql953b[1:nrow(dql953b),1:33]
dql4494[(ewtime2[21]+1):ewtime2[22],1:33]<-dql954a[1:nrow(dql954a),1:33]
dql4494[(ewtime2[22]+1):ewtime2[23],1:33]<-dql954b[1:nrow(dql954b),1:33]
dql4494[(ewtime2[23]+1):ewtime2[24],1:33]<-dql955a[1:nrow(dql955a),1:33]
dql4494[(ewtime2[24]+1):ewtime2[25],1:33]<-dql955b[1:nrow(dql955b),1:33]
dql4494[(ewtime2[25]+1):ewtime2[26],1:33]<-dql956a[1:nrow(dql956a),1:33]
dql4494[(ewtime2[26]+1):ewtime2[27],1:33]<-dql956b[1:nrow(dql956b),1:33]
dql4494[(ewtime2[27]+1):ewtime2[28],1:33]<-dql957a[1:nrow(dql957a),1:33]
dql4494[(ewtime2[28]+1):ewtime2[29],1:33]<-dql957b[1:nrow(dql957b),1:33]
dql4494[(ewtime2[29]+1):ewtime2[30],1:33]<-dql958a[1:nrow(dql958a),1:33]
dql4494[(ewtime2[30]+1):ewtime2[31],1:33]<-dql958b[1:nrow(dql958b),1:33]
dql4494[(ewtime2[31]+1):ewtime2[32],1:33]<-dql959a[1:nrow(dql959a),1:33]
dql4494[(ewtime2[32]+1):ewtime2[33],1:33]<-dql959b[1:nrow(dql959b),1:33]
dql4494[(ewtime2[33]+1):ewtime2[34],1:33]<-dql960a[1:nrow(dql960a),1:33]
dql4494[(ewtime2[34]+1):ewtime2[35],1:33]<-dql960b[1:nrow(dql960b),1:33]
dql4494[(ewtime2[35]+1):ewtime2[36],1:33]<-dql961a[1:nrow(dql961a),1:33]
dql4494[(ewtime2[36]+1):ewtime2[37],1:33]<-dql961b[1:nrow(dql961b),1:33]
dql4494[(ewtime2[37]+1):ewtime2[38],1:33]<-dql962a[1:nrow(dql962a),1:33]
dql4494[(ewtime2[38]+1):ewtime2[39],1:33]<-dql962b[1:nrow(dql962b),1:33]
dql4494[(ewtime2[39]+1):ewtime2[40],1:33]<-dql963a[1:nrow(dql963a),1:33]
dql4494[(ewtime2[40]+1):ewtime2[41],1:33]<-dql963b[1:nrow(dql963b),1:33]
dql4494[(ewtime2[41]+1):ewtime2[42],1:33]<-dql964a[1:nrow(dql964a),1:33]
dql4494[(ewtime2[42]+1):ewtime2[43],1:33]<-dql964b[1:nrow(dql964b),1:33]
dql4494[(ewtime2[43]+1):ewtime2[44],1:33]<-dql965i[1:nrow(dql965i),1:33]
dql4494[(ewtime2[44]+1):ewtime2[45],1:33]<-dql965ii[1:nrow(dql965ii),1:33]
dql4494[(ewtime2[45]+1):ewtime2[46],1:33]<-dql966a[1:nrow(dql966a),1:33]
dql4494[(ewtime2[46]+1):ewtime2[47],1:33]<-dql966b[1:nrow(dql966b),1:33]
dql4494[(ewtime2[47]+1):ewtime2[48],1:33]<-dql967a[1:nrow(dql967a),1:33]
dql4494[(ewtime2[48]+1):ewtime2[49],1:33]<-dql967b[1:nrow(dql967b),1:33]
dql4494[(ewtime2[49]+1):ewtime2[50],1:33]<-dql968a[1:nrow(dql968a),1:33]
dql4494[(ewtime2[50]+1):ewtime2[51],1:33]<-dql968b[1:nrow(dql968b),1:33]
dql4494[(ewtime2[51]+1):ewtime2[52],1:33]<-dql969a[1:nrow(dql969a),1:33]
dql4494[(ewtime2[52]+1):ewtime2[53],1:33]<-dql969b[1:nrow(dql969b),1:33]
dql4494[(ewtime2[53]+1):ewtime2[54],1:33]<-dql970a[1:nrow(dql970a),1:33]
dql4494[(ewtime2[54]+1):ewtime2[55],1:33]<-dql970b[1:nrow(dql970b),1:33]
dql4494[(ewtime2[55]+1):ewtime2[56],1:33]<-dql971a[1:nrow(dql971a),1:33]
dql4494[(ewtime2[56]+1):ewtime2[57],1:33]<-dql971b[1:nrow(dql971b),1:33]
dql4494[(ewtime2[57]+1):ewtime2[58],1:33]<-dql972a[1:nrow(dql972a),1:33]
dql4494[(ewtime2[58]+1):ewtime2[59],1:33]<-dql972b[1:nrow(dql972b),1:33]
dql4494[(ewtime2[59]+1):ewtime2[60],1:33]<-dql973a[1:nrow(dql973a),1:33]
dql4494[(ewtime2[60]+1):ewtime2[61],1:33]<-dql973b[1:nrow(dql973b),1:33]
dql4494[(ewtime2[61]+1):ewtime2[62],1:33]<-dql974i[1:nrow(dql974i),1:33]
dql4494[(ewtime2[62]+1):ewtime2[63],1:33]<-dql974ii[1:nrow(dql974ii),1:33]
dql4494[(ewtime2[63]+1):ewtime2[64],1:33]<-dql975[1:nrow(dql975),1:33]
dql4494[(ewtime2[64]+1):ewtime2[65],1:33]<-dql976[1:nrow(dql976),1:33]
dql4494[(ewtime2[65]+1):ewtime2[66],1:33]<-dql977[1:nrow(dql977),1:33]
dql4494[(ewtime2[66]+1):ewtime2[67],1:33]<-dql978[1:nrow(dql978),1:33]
dql4494[(ewtime2[67]+1):ewtime2[68],1:33]<-dql979[1:nrow(dql979),1:33]
dql4494[(ewtime2[68]+1):ewtime2[69],1:33]<-dql980[1:nrow(dql980),1:33]
dql4494[(ewtime2[69]+1):ewtime2[70],1:33]<-dql981[1:nrow(dql981),1:33]
dql4494[(ewtime2[70]+1):ewtime2[71],1:33]<-dql982[1:nrow(dql982),1:33]
dql4494[(ewtime2[71]+1):ewtime2[72],1:33]<-dql983[1:nrow(dql983),1:33]
dql4494[(ewtime2[72]+1):ewtime2[73],1:33]<-dql984[1:nrow(dql984),1:33]
dql4494[(ewtime2[73]+1):ewtime2[74],1:33]<-dql985[1:nrow(dql985),1:33]
dql4494[(ewtime2[74]+1):ewtime2[75],1:33]<-dql986[1:nrow(dql986),1:33]
dql4494[(ewtime2[75]+1):ewtime2[76],1:33]<-dql987[1:nrow(dql987),1:33]
dql4494[(ewtime2[76]+1):ewtime2[77],1:33]<-dql988[1:nrow(dql988),1:33]
dql4494[(ewtime2[77]+1):ewtime2[78],1:33]<-dql989[1:nrow(dql989),1:33]
dql4494[(ewtime2[78]+1):ewtime2[79],1:33]<-dql990[1:nrow(dql990),1:33]
dql4494[(ewtime2[79]+1):ewtime2[80],1:33]<-dql991[1:nrow(dql991),1:33]
dql4494[(ewtime2[80]+1):ewtime2[81],1:33]<-dql992[1:nrow(dql992),1:33]
dql4494[(ewtime2[81]+1):ewtime2[82],1:33]<-dql993[1:nrow(dql993),1:33]
dql4494[(ewtime2[82]+1):ewtime2[83],1:33]<-dql994[1:nrow(dql994),1:33]

table(is.na(t(dql4494)))
33*2661

# post 1965 london boundaries different than pre, therefore have to "recalculate what london is 1944-1965, based on post 1965 boundaries

tmp<-rep(NA,1108)
for(i in 1:1108)
	tmp[i]<-sum(t(dql4494[i,]),na.rm=T)
dqt4494[1:1108,1]<-dqu4474[1:1108,387]<-tmp



# latitude and longitude

latlong<-read.csv("latlong1.csv",sep=";",header=T)

dqr4<-function(x,URL,wch)
{
if(missing(URL))
	stop("\nYou must either give U, R or L")
if(missing(wch))
	stop("\nYou must either give 65 or 74")
if(wch==65)
	{
	x2<-x[x$URL65==URL,]
	x3<-x2[order(x2$P4465),]
	}
if(wch==74)
	{
	x2<-x[x$URL74==URL,]
	x3<-x2[order(x2$P4474),]
	}
if(names(x3)[1]=="Place")
	x4<-x3[,2:3]
else
	x4<-x3[,1:2]
t(x4)
}

dqXYl4465<-dqr4(latlong,"L",65)
dimnames(dqXYl4465)[[2]]<-ewl4465nam
dqXYr4465<-dqr4(latlong,"R",65)
dimnames(dqXYr4465)[[2]]<-ewr4465nam
dqXYu4465<-dqr4(latlong,"U",65)
dimnames(dqXYu4465)[[2]]<-ewu4465nam
dqXYr4474<-dqr4(latlong,"R",74)
dimnames(dqXYr4474)[[2]]<-ewr4474nam
dqXYu4474<-dqr4(latlong,"U",74)
dimnames(dqXYu4474)[[2]]<-ewu4474nam

dqr5<-function(x)
{
x2<-x[!is.na(x$PP),]
if(names(x2)[1]=="Place")
	xn<-2
else
	xn<-1
x4<-matrix(data=0,ncol=399,nrow=2,dimnames= list(c("Long","Lat"),ew4494nam))
for(i in 1:399)
	for(j in 1:2)
		x4[j,i]<-round(mean(x2[,(xn-1+j)][x2$PP==PPnum[i]],na.rm=T),3)
x4
}

dqXa4494<-dqr5(latlong)
dqXYl4494<-dqXa4494[,1:33]
dqXYt4494<-dqXa4494[,34:399]

# annual cases

ewtimeA<-floor(ewtime)

dqAr4465<-matrix(data=NA,ncol=ncol(dqr4465),nrow=length(seq(44,65,1)),dimnames=list(seq(44,65,1),ewr4465nam))
dqAu4465<-matrix(data=NA,ncol=ncol(dqu4465),nrow=length(seq(44,65,1)),dimnames=list(seq(44,65,1),ewu4465nam))
dqAl4465<-matrix(data=NA,ncol=ncol(dql4465),nrow=length(seq(44,65,1)),dimnames=list(seq(44,65,1),ewl4465nam))
dqAr4474<-matrix(data=NA,ncol=ncol(dqr4474),nrow=length(seq(44,74,1)),dimnames=list(seq(44,74,1),ewr4474nam))
dqAu4474<-matrix(data=NA,ncol=ncol(dqu4474),nrow=length(seq(44,74,1)),dimnames=list(seq(44,74,1),ewu4474nam))
dqAl4494<-matrix(data=NA,ncol=ncol(dql4494),nrow=length(seq(44,94,1)),dimnames=list(seq(44,94,1),ewl4494nam))
dqAt4494<-matrix(data=NA,ncol=ncol(dqt4494),nrow=length(seq(44,94,1)),dimnames=list(seq(44,94,1),ewt4494nam))

for(j in 1:nrow(dqAl4494))
	{
	for(i in 1:ncol(dql4494))
		dqAl4494[j,i]<-sum(dql4494[,i][ewtimeA==j+43],na.rm=T)
	for(k in 1:ncol(dqt4494))
		dqAt4494[j,k]<-sum(dqt4494[,k][ewtimeA==j+43],na.rm=T)
	}

for(j in 1:nrow(dqAr4474))
	{
	for(i in 1:ncol(dqr4474))
		dqAr4474[j,i]<-sum(dqr4474[,i][ewtimeA==j+43],na.rm=T)
	for(k in 1:ncol(dqu4474))
		dqAu4474[j,k]<-sum(dqu4474[,k][ewtimeA==j+43],na.rm=T)
	}

for(j in 1:nrow(dqAr4465))
	{
	for(i in 1:ncol(dqr4465))
		dqAr4465[j,i]<-sum(dqr4465[,i][ewtimeA==j+43],na.rm=T)
	for(k in 1:ncol(dqu4465))
		dqAu4465[j,k]<-sum(dqu4465[,k][ewtimeA==j+43],na.rm=T)
	for(w in 1:ncol(dql4465))
		dqAl4465[j,w]<-sum(dql4465[,w][ewtimeA==j+43],na.rm=T)
	}


# population

dqpop4464<-read.csv("pop4464.csv",header=T)
dqpop6573<-read.csv("pop6573.csv",header=T)
dqpop7494<-read.csv("pop7494.csv",header=T)

dqPu64464<-dqr2(dqpop4464,"U",65)
dqPl64464<-dqr2(dqpop4464,"L",65)
dqPr64464<-dqr2(dqpop4464,"R",65)
dqPu4464<-dqPu64464[seq(1,61,3),]
dqPl4464<-dqPl64464[seq(1,61,3),]
dqPr4464<-dqPr64464[seq(1,61,3),]
dqBu4464<-dqPu64464[seq(2,62,3),]
dqBl4464<-dqPl64464[seq(2,62,3),]
dqBr4464<-dqPr64464[seq(2,62,3),]
dqDu4464<-dqPu64464[seq(3,63,3),]
dqDl4464<-dqPl64464[seq(3,63,3),]
dqDr4464<-dqPr64464[seq(3,63,3),]

table(is.na(dqPu4464))
table(is.na(dqPr4464))
table(is.na(dqPl4464))

dimnames(dqPu4464)[[1]]<-dimnames(dqPl4464)[[1]]<-dimnames(dqPr4464)[[1]]<-dimnames(dqBu4464)[[1]]<-dimnames(dqDu4464)[[1]]<-dimnames(dqDl4464)[[1]]<-dimnames(dqDr4464)[[1]]<-dimnames(dqBl4464)[[1]]<-dimnames(dqBr4464)[[1]]<-seq(44,64,1)

dimnames(dqPu4464)[[2]]<-dimnames(dqBu4464)[[2]]<-dimnames(dqDu4464)[[2]]<-ewu4465nam
dimnames(dqPl4464)[[2]]<-dimnames(dqBl4464)[[2]]<-dimnames(dqDl4464)[[2]]<-ewl4465nam
dimnames(dqPr4464)[[2]]<-dimnames(dqBr4464)[[2]]<-dimnames(dqDr4464)[[2]]<-ewr4465nam

# 1944 - 1973 r, u

dqPu74464<-dqr2(dqpop4464,"U",74)
dqPr74464<-dqr2(dqpop4464,"R",74)

dqPu74464a<-dqPu74464[seq(1,61,3),]
dqPr74464a<-dqPr74464[seq(1,61,3),]
dqDu74464a<-dqPu74464[seq(3,63,3),]
dqDr74464a<-dqPr74464[seq(3,63,3),]
dqBu74464a<-dqPu74464[seq(2,62,3),]
dqBr74464a<-dqPr74464[seq(2,62,3),]

table(is.na(dqPu74464a))
table(is.na(dqPr74464a))

dqPu76573<-dqr2(dqpop6573,"U",74)
dqPu76573a<-dqPu76573[seq(1,25,3),]
dqBu76573a<-dqPu76573[seq(2,26,3),]
dqDu76573a<-dqPu76573[seq(3,27,3),]

dqPr76573<-dqr2(dqpop6573,"R",74)
dqPr76573a<-dqPr76573[seq(1,25,3),]
dqBr76573a<-dqPr76573[seq(2,26,3),]
dqDr76573a<-dqPr76573[seq(3,27,3),]

table(is.na(dqPu76573a))
table(is.na(dqPr76573a))

dqPu4473<-dqBu4473<-dqDu4473<-matrix(data=NA,ncol=845,nrow=30,dimnames=list(seq(44,73,1),ewu4474nam))
dqPr4473<-dqBr4473<-dqDr4473<-matrix(data=NA,ncol=457,nrow=30,dimnames=list(seq(44,73,1),ewr4474nam))
dqPl4473<-dqBl4473<-dqDl4473<-matrix(data=NA,ncol=33,nrow=30,dimnames=list(seq(44,73,1),ewl4494nam))

dqPu4473[1:21,1:845]<-dqPu74464a[1:21,1:845]
dqBu4473[1:21,1:845]<-dqBu74464a[1:21,1:845]
dqDu4473[1:21,1:845]<-dqDu74464a[1:21,1:845]

dqPu4473[22:30,1:845]<-dqPu76573a[1:9,1:845]
dqBu4473[22:30,1:845]<-dqBu76573a[1:9,1:845]
dqDu4473[22:30,1:845]<-dqDu76573a[1:9,1:845]

table(is.na(dqPu4473))

dqPr4473[1:21,1:457]<-dqPr74464a[1:21,1:457]
dqBr4473[1:21,1:457]<-dqBr74464a[1:21,1:457]
dqDr4473[1:21,1:457]<-dqDr74464a[1:21,1:457]

dqPr4473[22:30,1:457]<-dqPr76573a[1:9,1:457]
dqBr4473[22:30,1:457]<-dqBr76573a[1:9,1:457]
dqDr4473[22:30,1:457]<-dqDr76573a[1:9,1:457]

table(is.na(dqPr4473))

# 44 94

dqPl4494<-dqBl4494<-dqDl4494<-matrix(data=NA,ncol=33,nrow=51,dimnames=list(seq(44,94,1),ewl4494nam))
dqPt4494<-dqBt4494<-dqDt4494<-matrix(data=NA,ncol=366,nrow=51,dimnames=list(seq(44,94,1),ewt4494nam))

dqpop9a<-dqpop4464[,c(1:7,8:28)]
dqpop9b<-dqpop4464[,c(1:7,29:49)]
dqpop9c<-dqpop4464[,c(1:7,50:70)]
dqP9a<-dqr3(dqpop9a)
dqP9b<-dqr3(dqpop9b)
dqP9c<-dqr3(dqpop9c)

dqP9a<-dqr6(dqpop9a,dqP9a)
dqP9b<-dqr6(dqpop9b,dqP9b)
dqP9c<-dqr6(dqpop9c,dqP9c)

table(is.na(dqP9a))
table(is.na(dqP9b))
table(is.na(dqP9c))

dqPl94494a<-dqP9a[seq(1,19,3),1:33]
dqPl94494b<-dqP9b[seq(1,19,3),1:33]
dqPl94494c<-dqP9c[seq(1,19,3),1:33]
dqBl94494a<-dqP9a[seq(2,20,3),1:33]
dqBl94494b<-dqP9b[seq(2,20,3),1:33]
dqBl94494c<-dqP9c[seq(2,20,3),1:33]
dqDl94494a<-dqP9a[seq(3,21,3),1:33]
dqDl94494b<-dqP9b[seq(3,21,3),1:33]
dqDl94494c<-dqP9c[seq(3,21,3),1:33]

  dqPl4494[1:7,1:33]<-dqPl94494a[1:7,1:33]
 dqPl4494[8:14,1:33]<-dqPl94494b[1:7,1:33]
dqPl4494[15:21,1:33]<-dqPl94494c[1:7,1:33]
  dqBl4494[1:7,1:33]<-dqBl94494a[1:7,1:33]
 dqBl4494[8:14,1:33]<-dqBl94494b[1:7,1:33]
dqBl4494[15:21,1:33]<-dqBl94494c[1:7,1:33]
  dqDl4494[1:7,1:33]<-dqDl94494a[1:7,1:33]
 dqDl4494[8:14,1:33]<-dqDl94494b[1:7,1:33]
dqDl4494[15:21,1:33]<-dqDl94494c[1:7,1:33]

dqPt94494a<-dqP9a[seq(1,19,3),34:399]
dqPt94494b<-dqP9b[seq(1,19,3),34:399]
dqPt94494c<-dqP9c[seq(1,19,3),34:399]
dqBt94494a<-dqP9a[seq(2,20,3),34:399]
dqBt94494b<-dqP9b[seq(2,20,3),34:399]
dqBt94494c<-dqP9c[seq(2,20,3),34:399]
dqDt94494a<-dqP9a[seq(3,21,3),34:399]
dqDt94494b<-dqP9b[seq(3,21,3),34:399]
dqDt94494c<-dqP9c[seq(3,21,3),34:399]

  dqPt4494[1:7,1:366]<-dqPt94494a[1:7,1:366]
 dqPt4494[8:14,1:366]<-dqPt94494b[1:7,1:366]
dqPt4494[15:21,1:366]<-dqPt94494c[1:7,1:366]
  dqBt4494[1:7,1:366]<-dqBt94494a[1:7,1:366]
 dqBt4494[8:14,1:366]<-dqBt94494b[1:7,1:366]
dqBt4494[15:21,1:366]<-dqBt94494c[1:7,1:366]
  dqDt4494[1:7,1:366]<-dqDt94494a[1:7,1:366]
 dqDt4494[8:14,1:366]<-dqDt94494b[1:7,1:366]
dqDt4494[15:21,1:366]<-dqDt94494c[1:7,1:366]

tmp<-tmp2<-tmp3<-rep(NA,21)
for(i in 1:21)
	{
	 tmp[i]<-sum(t(dqPl4494[i,]),na.rm=T)
	tmp2[i]<-sum(t(dqBl4494[i,]),na.rm=T)
	tmp3[i]<-sum(t(dqDl4494[i,]),na.rm=T)
	}
dqPt4494[1:21,1]<-tmp
dqBt4494[1:21,1]<-tmp2
dqDt4494[1:21,1]<-tmp3
dqpop9d<-dqpop6573
dqP9d<-dqr3(dqpop9d)
dqP9d<-dqr6(dqpop9d,dqP9d)

table(is.na(dqP9d))

dqPl94494d<-dqP9d[seq(1,25,3),1:33]
dqBl94494d<-dqP9d[seq(2,26,3),1:33]
dqDl94494d<-dqP9d[seq(3,27,3),1:33]

dqPt94494d<-dqP9d[seq(1,25,3),34:399]
dqBt94494d<-dqP9d[seq(2,26,3),34:399]
dqDt94494d<-dqP9d[seq(3,27,3),34:399]

dqPl4494[22:30,1:33]<-dqPl94494d[1:9,1:33]
dqBl4494[22:30,1:33]<-dqBl94494d[1:9,1:33]
dqDl4494[22:30,1:33]<-dqDl94494d[1:9,1:33]

dqPt4494[22:30,1:366]<-dqPt94494d[1:9,1:366]
dqBt4494[22:30,1:366]<-dqBt94494d[1:9,1:366]
dqDt4494[22:30,1:366]<-dqDt94494d[1:9,1:366]

tmp<-tmp2<-tmp3<-rep(NA,9)
for(i in 22:30)
	{
	 tmp[i-21]<-sum(t(dqPl4494[i,]),na.rm=T)
	tmp2[i-21]<-sum(t(dqBl4494[i,]),na.rm=T)
	tmp3[i-21]<-sum(t(dqDl4494[i,]),na.rm=T)
	}
dqPt4494[22:30,1]<-tmp
dqBt4494[22:30,1]<-tmp2
dqDt4494[22:30,1]<-tmp3

dqP9e<-dqr3(dqpop7494)
dqPl94494e<-dqP9e[seq(1,61,3),1:33]
dqBl94494e<-dqP9e[seq(2,62,3),1:33]
dqDl94494e<-dqP9e[seq(3,63,3),1:33]

dqPt94494e<-dqP9e[seq(1,61,3),34:399]
dqBt94494e<-dqP9e[seq(2,62,3),34:399]
dqDt94494e<-dqP9e[seq(3,63,3),34:399]

dqPl4494[31:51,1:33]<-dqPl94494e[1:21,1:33]
dqBl4494[31:51,1:33]<-dqBl94494e[1:21,1:33]
dqDl4494[31:51,1:33]<-dqDl94494e[1:21,1:33]

dqPt4494[31:51,1:366]<-dqPt94494e[1:21,1:366]
dqBt4494[31:51,1:366]<-dqBt94494e[1:21,1:366]
dqDt4494[31:51,1:366]<-dqDt94494e[1:21,1:366]


# exclude the places that don't go 44-94

rm4494<-c(34:109,111:125,127:130,132:152,154:173,175:195,197:204,206:252,254:268,270:282,284:290,292:314,316:399)-33

dqt4494<-dqt4494[,rm4494]
dqXYt4494<-dqXYt4494[,rm4494]
dqPt4494<-dqPt4494[,rm4494]
dqBt4494<-dqBt4494[,rm4494]
dqDt4494<-dqDt4494[,rm4494]
dqAt4494<-dqAt4494[,rm4494]

# the final databases

ewXYl4465<-dqXYl4465
ewXYr4465<-dqXYr4465
ewXYu4465<-dqXYu4465
ewXYr4474<-dqXYr4474
ewXYu4474<-dqXYu4474
ewXYl4494<-dqXYl4494
ewXY4494<-dqXYt4494

ewM4494<-dqt4494
ewMl4494<-dql4494
ewMr4474<-dqr4474
ewMu4474<-dqu4474
ewMu4465<-dqu4465
ewMr4465<-dqr4465
ewMl4465<-dql4465

ewAr4465<-dqAr4465
ewAu4465<-dqAu4465
ewAl4465<-dqAl4465
ewAr4474<-dqAr4474
ewAu4474<-dqAu4474
ewAl4494<-dqAl4494
ewA4494<-dqAt4494

ewPu4473<-dqPu4473
ewPr4473<-dqPr4473
ewPu4464<-dqPu4464
ewPr4464<-dqPr4464
ewPl4464<-dqPl4464
ewPl4494<-dqPl4494
ewP4494<-dqPt4494

ewBu4473<-dqBu4473
ewBr4473<-dqBr4473
ewBu4464<-dqBu4464
ewBr4464<-dqBr4464
ewBl4464<-dqBl4464
ewBl4494<-dqBl4494
ewB4494<-dqBt4494

ewDu4473<-dqDu4473
ewDr4473<-dqDr4473
ewDu4464<-dqDu4464
ewDr4464<-dqDr4464
ewDl4464<-dqDl4464
ewDl4494<-dqDl4494
ewD4494<-dqDt4494

ewXYl4465<-as.data.frame(ewXYl4465)
ewXYr4465<-as.data.frame(ewXYr4465)
ewXYu4465<-as.data.frame(ewXYu4465)
ewXYr4474<-as.data.frame(ewXYr4474)
ewXYu4474<-as.data.frame(ewXYu4474)
ewXYl4494<-as.data.frame(ewXYl4494)
ewXY4494<-as.data.frame(ewXY4494)
ewM4494<-as.data.frame(ewM4494)
ewMl4494<-as.data.frame(ewMl4494)
ewMr4474<-as.data.frame(ewMr4474)
ewMu4474<-as.data.frame(ewMu4474)
ewMu4465<-as.data.frame(ewMu4465)
ewMr4465<-as.data.frame(ewMr4465)
ewMl4465<-as.data.frame(ewMl4465)
ewAr4465<-as.data.frame(ewAr4465)
ewAu4465<-as.data.frame(ewAu4465)
ewAl4465<-as.data.frame(ewAl4465)
ewAr4474<-as.data.frame(ewAr4474)
ewAu4474<-as.data.frame(ewAu4474)
ewAl4494<-as.data.frame(ewAl4494)
ewA4494<-as.data.frame(ewA4494)
ewPu4473<-as.data.frame(ewPu4473)
ewPr4473<-as.data.frame(ewPr4473)
ewPu4464<-as.data.frame(ewPu4464)
ewPr4464<-as.data.frame(ewPr4464)
ewPl4464<-as.data.frame(ewPl4464)
ewPl4494<-as.data.frame(ewPl4494)
ewP4494<-as.data.frame(ewP4494)
ewBu4473<-as.data.frame(ewBu4473)
ewBr4473<-as.data.frame(ewBr4473)
ewBu4464<-as.data.frame(ewBu4464)
ewBr4464<-as.data.frame(ewBr4464)
ewBl4464<-as.data.frame(ewBl4464)
ewBl4494<-as.data.frame(ewBl4494)
ewB4494<-as.data.frame(ewB4494)
ewDu4473<-as.data.frame(ewDu4473)
ewDr4473<-as.data.frame(ewDr4473)
ewDu4464<-as.data.frame(ewDu4464)
ewDr4464<-as.data.frame(ewDr4464)
ewDl4464<-as.data.frame(ewDl4464)
ewDl4494<-as.data.frame(ewDl4494)
ewD4494<-as.data.frame(ewD4494)




rm(dq44a,dq44b,dq45a,dq45b,dq46a,dq46b,dq47a,dq47b,dq48a,dq48b,dq49a,dq49b,dq50a,dq50b,dq51a,dq51b,dq52a,dq52b,dq53a,dq53b,dq54a,dq54b,dq55a,dq55b,dq56a,dq56b,dq57a,dq57b,dq58a,dq58b,dq59a,dq59b,dq60a,dq60b,dq61a,dq61b,dq62a,dq62b,dq63a,dq63b,dq64a,dq64b,dq65i,dq65ii,dq66a,dq66b,dq67a,dq67b,dq68a,dq68b,dq69a,dq69b,dq70a,dq70b,dq71a,dq71b,dq72a,dq72b,dq73a,dq73b,dq74i,dq74ii,dq75,dq76,dq77,dq78,dq79,dq80,dq81,dq82,dq83,dq84,dq85,dq86,dq87,dq88,dq89,dq90,dq91,dq92,dq93,dq94,dqu644a,dqu644b,dqu645a,dqu645b,dqu646a,dqu646b,dqu647a,dqu647b,dqu648a,dqu648b,dqu649a,dqu649b,dqu650a,dqu650b,dqu651a,dqu651b,dqu652a,dqu652b,dqu653a,dqu653b,dqu654a,dqu654b,dqu655a,dqu655b,dqu656a,dqu656b,dqu657a,dqu657b,dqu658a,dqu658b,dqu659a,dqu659b,dqu660a,dqu660b,dqu661a,dqu661b,dqu662a,dqu662b,dqu663a,dqu663b,dqu664a,dqu664b,dqu665i,dqr644a,dqr644b,dqr645a,dqr645b,dqr646a,dqr646b,dqr647a,dqr647b,dqr648a,dqr648b,dqr649a,dqr649b,dqr650a,dqr650b,dqr651a,dqr651b,dqr652a,dqr652b,dqr653a,dqr653b,dqr654a,dqr654b,dqr655a,dqr655b,dqr656a,dqr656b,dqr657a,dqr657b,dqr658a,dqr658b,dqr659a,dqr659b,dqr660a,dqr660b,dqr661a,dqr661b,dqr662a,dqr662b,dqr663a,dqr663b,dqr664a,dqr664b,dqr665i,dql644a,dql644b,dql645a,dql645b,dql646a,dql646b,dql647a,dql647b,dql648a,dql648b,dql649a,dql649b,dql650a,dql650b,dql651a,dql651b,dql652a,dql652b,dql653a,dql653b,dql654a,dql654b,dql655a,dql655b,dql656a,dql656b,dql657a,dql657b,dql658a,dql658b,dql659a,dql659b,dql660a,dql660b,dql661a,dql661b,dql662a,dql662b,dql663a,dql663b,dql664a,dql664b,dql665i,dqu744a,dqu744b,dqu745a,dqu745b,dqu746a,dqu746b,dqu747a,dqu747b,dqu748a,dqu748b,dqu749a,dqu749b,dqu750a,dqu750b,dqu751a,dqu751b,dqu752a,dqu752b,dqu753a,dqu753b,dqu754a,dqu754b,dqu755a,dqu755b,dqu756a,dqu756b,dqu757a,dqu757b,dqu758a,dqu758b,dqu759a,dqu759b,dqu760a,dqu760b,dqu761a,dqu761b,dqu762a,dqu762b,dqu763a,dqu763b,dqu764a,dqu764b,dqu765i)
rm(dqu765ii,dqu766a,dqu766b,dqu767a,dqu767b,dqu768a,dqu768b,dqu769a,dqu769b,dqu770a,dqu770b,dqu771a,dqu771b,dqu772a,dqu772b,dqu773a,dqu773b,dqu774i,dqr744a,dqr744b,dqr745a,dqr745b,dqr746a,dqr746b,dqr747a,dqr747b,dqr748a,dqr748b,dqr749a,dqr749b,dqr750a,dqr750b,dqr751a,dqr751b,dqr752a,dqr752b,dqr753a,dqr753b,dqr754a,dqr754b,dqr755a,dqr755b,dqr756a,dqr756b,dqr757a,dqr757b,dqr758a,dqr758b,dqr759a,dqr759b,dqr760a,dqr760b,dqr761a,dqr761b,dqr762a,dqr762b,dqr763a,dqr763b,dqr764a,dqr764b,dqr765i,dqr765ii,dqr766a,dqr766b,dqr767a,dqr767b,dqr768a,dqr768b,dqr769a,dqr769b,dqr770a,dqr770b,dqr771a,dqr771b,dqr772a,dqr772b,dqr773a,dqr773b,dqr774i,dqa944a,dqa944b,dqa945a,dqa945b,dqa946a,dqa946b,dqa947a,dqa947b,dqa948a,dqa948b,dqa949a,dqa949b,dqa950a,dqa950b,dqa951a,dqa951b,dqa952a,dqa952b,dqa953a,dqa953b,dqa954a,dqa954b,dqa955a,dqa955b,dqa956a,dqa956b,dqa957a,dqa957b,dqa958a,dqa958b,dqa959a,dqa959b,dqa960a,dqa960b,dqa961a,dqa961b,dqa962a,dqa962b,dqa963a,dqa963b,dqa964a,dqa964b,dqa965i,dqa965ii,dqa966a,dqa966b,dqa967a,dqa967b,dqa968a,dqa968b,dqa969a,dqa969b,dqa970a,dqa970b,dqa971a,dqa971b,dqa972a,dqa972b,dqa973a,dqa973b,dqa974i,dqa974ii,dqa975,dqa976,dqa977,dqa978,dqa979,dqa980,dqa981,dqa982,dqa983,dqa984,dqa985,dqa986,dqa987,dqa988,dqa989,dqa990,dqa991,dqa992,dqa993,dqa994,dqt944a,dqt944b,dqt945a,dqt945b,dqt946a,dqt946b,dqt947a,dqt947b,dqt948a,dqt948b,dqt949a,dqt949b,dqt950a,dqt950b,dqt951a,dqt951b,dqt952a,dqt952b,dqt953a,dqt953b,dqt954a,dqt954b,dqt955a,dqt955b,dqt956a,dqt956b,dqt957a,dqt957b,dqt958a,dqt958b,dqt959a,dqt959b,dqt960a,dqt960b,dqt961a,dqt961b,dqt962a,dqt962b,dqt963a,dqt963b,dqt964a,dqt964b,dqt965i,dqt965ii,dqt966a,dqt966b,dqt967a,dqt967b,dqt968a,dqt968b,dqt969a,dqt969b,dqt970a,dqt970b,dqt971a,dqt971b,dqt972a,dqt972b,dqt973a,dqt973b,dqt974i,dqt974ii,dqt975,dqt976,dqt977,dqt978,dqt979,dqt980,dqt981,dqt982,dqt983,dqt984,dqt985,dqt986)
rm(dqt987,dqt988,dqt989,dqt990,dqt991,dqt992,dqt993,dqt994,dql944a,dql944b,dql945a,dql945b,dql946a,dql946b,dql947a,dql947b,dql948a,dql948b,dql949a,dql949b,dql950a,dql950b,dql951a,dql951b,dql952a,dql952b,dql953a,dql953b,dql954a,dql954b,dql955a,dql955b,dql956a,dql956b,dql957a,dql957b,dql958a,dql958b,dql959a,dql959b,dql960a,dql960b,dql961a,dql961b,dql962a,dql962b,dql963a,dql963b,dql964a,dql964b,dql965i,dql965ii,dql966a,dql966b,dql967a,dql967b,dql968a,dql968b,dql969a,dql969b,dql970a,dql970b,dql971a,dql971b,dql972a,dql972b,dql973a,dql973b,dql974i,dql974ii,dql975,dql976,dql977,dql978,dql979,dql980,dql981,dql982,dql983,dql984,dql985,dql986,dql987,dql988,dql989,dql990,dql991,dql992,dql993,dql994,dqu4465,dql4465,dqr4465,dqt4494,dql4494,dqu4474,dqr4474,latlong,dqXYl4465,dqXYr4465,dqXYu4465,dqXYr4474,dqXYu4474,dqXa4494,dqXYl4494,dqXYt4494,dqAr4465,dqAu4465,dqAl4465,dqAr4474,dqAu4474,dqAl4494,dqAt4494,dqPu4464,dqPl4464,dqPr4464,dqBu4464,dqBl4464,dqBr4464,dqDu4464,dqDl4464,dqDr4464,dqPu4473,dqBu4473,dqDu4473,dqPr4473,dqBr4473,dqDr4473,dqPl4473,dqBl4473,dqDl4473,dqPl4494,dqBl4494,dqDl4494,dqPt4494,dqBt4494,dqDt4494,dqP9a,dqP9b,dqP9c,dqP9d,dqP9e,dqr1,dqr2,dqr3,dqr4,dqr5,dqr6,dqpop4464,dqpop6573,dqpop7494,dqPu64464,dqPl64464,dqPr64464,dqPu74464,dqPr74464,dqPu74464a,dqPr74464a,dqDu74464a,dqDr74464a,dqBu74464a,dqBr74464a,dqPu76573,dqPu76573a,dqBu76573a,dqDu76573a,dqPr76573,dqPr76573a,dqBr76573a,dqDr76573a,dqpop9a,dqpop9b,dqpop9c,dqPl94494a,dqPl94494b,dqPl94494c,dqBl94494a,dqBl94494b,dqBl94494c,dqDl94494a,dqDl94494b,dqDl94494c,dqPt94494a,dqPt94494b,dqPt94494c,dqBt94494a,dqBt94494b,dqBt94494c,dqDt94494a,dqDt94494b,dqDt94494c,dqpop9d,dqPl94494d,dqBl94494d,dqDl94494d,dqPt94494d,dqBt94494d,dqDt94494d,dqPl94494e,dqBl94494e,dqDl94494e,dqPt94494e,dqBt94494e,dqDt94494e,rm4494,i,j,k,PPnum,tmp,tmp2,tmp3,w,ewtime2,ewtimeA)
