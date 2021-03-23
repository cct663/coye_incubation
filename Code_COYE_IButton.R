####################################################################################################################
## Code for female common yellowthroat incubation paper using ibutton data. Written by Conor Taff.				  ##
## Last updated on 3/5/2017 in R version 3.3.1																	  ##
####################################################################################################################

####################################################################################################################
## This section is all setting up data objects and functions, etc. No actual analysis or plotting.                ##
####################################################################################################################
	
	## Load packages that might be used used in this analysis.
		pkg<-c("plyr","lme4","AICcmodavg","bbmle","nlme","reshape2","ggplot2","nadiv","broom","standardize",
				"rethinking","MuMIn","lmerTest","coefplot","MASS","gridExtra","sjPlot","rptR","plotrix","lmtest")
			for(i in 1:length(pkg)){library(pkg[i],character.only=TRUE)}
			
	## Set na action
		options(na.action=na.omit)
	
	## Read in the main data files stored in the working directory.
		a1<-read.csv("Input_DataByNest.csv")
		d1<-read.csv("Input_DataByBout.csv")
		f1<-read.csv("Input_DataByFemale.csv")
		s1<-read.delim("Input_Sunrise.txt")
		n1<-read.delim("Input_DatabyNestling.txt")
		
	## Possibly exclude these nests that had less than 10 bouts scored because the ibuttons did not work well.
		# I don't think it really matters in any substantial way whether these are included or not
			#d1<-subset(d1,d1$NestID!="2012-23"&d1$NestID!="2012-39"&d1$NestID!="2011-17"&
			#	d1$NestID!="2011-16"&d1$NestID!="2011-22"&d1$NestID!="2011-14")
			#a1<-subset(a1,a1$NestID!="2012-23"&a1$NestID!="2012-39"&a1$NestID!="2011-17"&
			#	a1$NestID!="2011-16"&a1$NestID!="2011-22"&a1$NestID!="2011-14")
			d1<-subset(d1,d1$NestID!="2012-50"&d1$NestID!="2011-25"&d1$NestID!="2012-23"&d1$NestID!="2012-26")
			a1<-subset(a1,a1$NestID!="2012-50"&a1$NestID!="2011-25"&a1$NestID!="2012-23"&a1$NestID!="2012-26")
	
	## Exclude on and off bouts that occur during the night
		d1<-subset(d1,d1$StartTime>16500&d1$StartTime<76000)	
	
	##Rescale & Center continuous variables in f1 to be in SD units.
		f1$FCCar.s<-scale(resid(lm(FCCar~as.factor(Year),data=f1)))
		f1$Fbib.s<-scale(resid(lm(Fbib~as.factor(Year),data=f1)))
		f1$Ftotal.s<-scale(resid(lm(Ftotal~as.factor(Year),data=f1)))
		f1$FUVSat.s<-scale(resid(lm(FUVSat~as.factor(Year),data=f1)))
		f1$Fyel.s<-scale(resid(lm(Fyel~as.factor(Year),data=f1)))
		f1$Year<-as.factor(f1$Year)
		f1$FUVB.s<-scale(resid(lm(FUV~as.factor(Year),data=f1)))
		
	## Join metadata on female measurements to the incubation by bout and incubation by nest data objects.
		d1<-join(d1,f1,"Female",match="first")
		a1<-join(a1,f1,"Female",match="first")
		
	## Add a condition column as residual of mass on tarsus regression
		c<-na.omit(a1[,c("NestID","FWt","Ftars")])
		cond<-as.data.frame(resid(lm(c$FWt~c$Ftars)))
		colnames(cond)<-"Fcond"
		cond$NestID<-c$NestID
		a1<-join(a1,cond,"NestID")
	
	## Add sunrise time to bout level data
		s1$sunlook<-paste(s1$jdate,s1$year,sep="_")
		s2<-s1[,c("sunlook","sunrise")]
		d1$sunlook<-paste(d1$Jdate,d1$Year,sep="_")
		d1<-join(d1,s2,"sunlook")
		d1$Sunrise.Seconds<-d1$sunrise*60
		
	## Make a column that change the start time of the bout to be relative to sunrise for the day it was measured
		d1$StartVsSun<-d1$StartTime-d1$Sunrise.Seconds
		
	## Center some predictors in the by bout dataset
		d1$IncubateDay.s<-scale(d1$IncubateDay)
		d1$Jdate.s<-scale(d1$Jdate)
		d1$StartTime.s<-scale(d1$StartTime)
		d1$Ambient.s<-scale(d1$Ambient)
		d1$Sunrise.Seconds.s<-scale(d1$Sunrise.Seconds)
	
	## Create subsets of the bout level data for on bouts and off bouts to analyze separately
		d2.on<-subset(d1,d1$OnOrOff=="On")
		d2.off<-subset(d1,d1$OnOrOff=="Off")
		d2.off.1st<-subset(d2.off,d2.off$FirstOff!="No")		# further subset of just first off bouts
				
	# Create an object that averages multiple nests from the same year
			a.y<-as.data.frame(unique(a1$Female))
			colnames(a.y)<-"Female"
				for(i in 1:nrow(a.y)){
					sub<-subset(a1,a1$Female== a.y$Female[i])
					a.y$OnBout[i]<-mean(na.omit(sub$OnBout))
					a.y$OffBout[i]<-mean(na.omit(sub$OffBout))
					a.y$CycleLength[i]<-mean(na.omit(sub$CycleLength))
					a.y$PercentOn[i]<-mean(na.omit(sub$PercentOn))
					a.y$FirstBoutTime[i]<-mean(na.omit(sub$FirstBoutTime))
					a.y$IncStartDate[i]<-mean(na.omit(sub$IncStartDate))
					a.y$nobs[i]<-nrow(sub)
					a.y$Color[i]<-as.character(sub$Color[1])
				}
				a.y$Color<-as.factor(a.y$Color)

####################################################################################################################
## Summary plots and summary statistics that don't really fall into the main analysis sections below.	          ##
####################################################################################################################

	## Plot bout on vs. off length at a nest level with error bars within nests
		pdf("Output_OnOff.pdf",width=5.2,height=5.6)
			plot(a1$OnBout,a1$OffBout,col="slateblue",pch=16,xlab="On bout length (mins)",
				ylab="Off bout length (mins)",ylim=c(9,26),xlim=c(23,72.5))		
				
			for(i in 1:nrow(a1)){
				lines(c(a1$OnBout[i]+a1$OnSE[i],a1$OnBout[i]-a1$OnSE[i]),rep(a1$OffBout[i],2),
					col=col.alpha("black",0.3))
				lines(rep(a1$OnBout[i],2),c(a1$OffBout[i]+a1$OffSE[i],a1$OffBout[i]-a1$OffSE[i]),
					col=col.alpha("black",0.3))
			}
			abline(lm(a1$OffBout~a1$OnBout))
			mod<-lmer(OffBout~OnBout+(1|Color),data=a1)
			#text(66,9,"P < 0.0001")
		dev.off()

####################################################################################################################
## This section deals with external predictors of incubation (temperature, date, time, etc) and plots them.       ##
####################################################################################################################

	## At the level of individual bouts, what environmental factors predict incubation length
		# set na action to fail so dredge function will work
			options(na.action=na.fail)
		
		## Run model selection for on bout length	
			## Set up data
				dat<-d2.on[,c("Length.Minute","FemaleID","NestID","StartTime.s","Jdate.s","Ambient.s","Year")]
				dat.on.bout<-na.omit(dat)
			# Run the model selection
				mod<-lmer(Length.Minute~StartTime.s+Jdate.s+Ambient.s+as.factor(Year)+
					I(StartTime.s^2)+(1|FemaleID)+(1|NestID),data=dat.on.bout)
				dr<-dredge(mod,trace=2)		# Identifies ambient temp, year, start time, and start time^2 as best model
				best.cov.on<-lmer(Length.Minute~Ambient.s+as.factor(Year)+StartTime.s+I(StartTime.s^2)+(1|NestID)+(1|FemaleID),data=dat)
				int.cov.on<-lmer(Length.Minute~1+(1|NestID)+(1|FemaleID),data=dat)
				#sjt.lmer(best.cov.on,int.cov.on)
				write.csv(dr,"Output_AIC_On_Covar.csv")
			
		## Run model selection for off bout length
			# Set up data
				dat<-d2.off[,c("Length.Minute","FemaleID","NestID","StartTime.s","Jdate.s","Ambient.s","Year")]
				dat.off.bout<-na.omit(dat)
			## Run the model selection
				mod<-lmer(Length.Minute~StartTime.s+Jdate.s+Ambient.s+as.factor(Year)+
					I(StartTime.s^2)+(1|FemaleID)+(1|NestID),data=dat.off.bout)
				dr<-dredge(mod,trace=2)		# Identifies ambient temp, jdate, and quadratic for start time as best model
				best.cov.off<-lmer(Length.Minute~Ambient.s+Jdate.s+I(StartTime.s^2)+(1|NestID)+(1|FemaleID),data=dat.off.bout)
				int.cov.off<-lmer(Length.Minute~1+(1|NestID)+(1|FemaleID),data=dat.off.bout)
				#sjt.lmer(best.cov.off,int.cov.off)
				write.csv(dr,"Output_AIC_Off_Covar.csv")
				
		## Run model selection for onset of activity
			# Set up data
				dat<-d2.off.1st[,c("StartTime","Sunrise.Seconds.s","FemaleID","NestID","Jdate.s","Ambient.s","Year")]
				dat.onset<-na.omit(dat)
			## Run the model selection
				mod<-lmer(StartTime~Sunrise.Seconds.s+Jdate.s+Ambient.s+as.factor(Year)+
					(1|FemaleID)+(1|NestID),data=dat.onset)
				dr<-dredge(mod,trace=2)		# Identifies ambient temp, jdate, and quadratic for start time as best model
				best.cov.onset<-lmer(StartTime~Ambient.s+as.factor(Year)+Jdate.s+Sunrise.Seconds.s+(1|NestID)+(1|FemaleID),data=dat.onset)
				best.cov.onset2<-lmer(StartTime/60/60~Ambient.s+as.factor(Year)+Jdate.s+Sunrise.Seconds.s+(1|NestID)+(1|FemaleID),data=dat.onset)
				int.cov.onset<-lmer(StartTime~1+(1|NestID)+(1|FemaleID),data=dat.onset)
				#sjt.lmer(best.cov.onset,int.cov.onset)
				write.csv(dr,"Output_AIC_Onset_Covar.csv")
				
		## Extract random effects from on and off bout models and add to nest level table
			## On Bouts
				ON.f<-as.data.frame(ranef(best.cov.on)$FemaleID)
				ON.f[,2]<-rownames(ON.f)
				colnames(ON.f)<-c("On.f.blup","Color")
				ON.n<-as.data.frame(ranef(best.cov.on)$NestID)
				ON.n[,2]<-rownames(ON.n)
				colnames(ON.n)<-c("On.n.blup","NestID")
			## Off Bouts
				OFF.f<-as.data.frame(ranef(best.cov.off)$FemaleID)
				OFF.f[,2]<-rownames(OFF.f)
				colnames(OFF.f)<-c("Off.f.blup","Color")
				OFF.n<-as.data.frame(ranef(best.cov.off)$NestID)
				OFF.n[,2]<-rownames(OFF.n)
				colnames(OFF.n)<-c("Off.n.blup","NestID")
			## Onset
				ONSET.f<-as.data.frame(ranef(best.cov.onset)$FemaleID)
				ONSET.f[,2]<-rownames(ONSET.f)
				colnames(ONSET.f)<-c("Onset.f.blup","Color")
				ONSET.n<-as.data.frame(ranef(best.cov.onset)$NestID)
				ONSET.n[,2]<-rownames(ONSET.n)
				colnames(ONSET.n)<-c("Onset.n.blup","NestID")
			
			## add these BLUP estimates to the nest level data set	
				a2<-a1
				a2<-join(a2,ON.f,"Color")
				a2<-join(a2,ON.n,"NestID")
				a2<-join(a2,OFF.f,"Color")
				a2<-join(a2,OFF.n,"NestID")
				a2<-join(a2,ONSET.f,"Color")
				a2<-join(a2,ONSET.n,"NestID")
				a2$OnInter<-rep(fixef(best.cov.on)[1],nrow(a2))
				a2$OffInter<-rep(fixef(best.cov.off)[1],nrow(a2))
				a2$OnsetInter<-rep(fixef(best.cov.onset)[1],nrow(a2))
				a2$ON<-a2$OnInter+a2$On.f.blup+a2$On.n.blup
				a2$OFF<-a2$OffInter+a2$Off.f.blup+a2$Off.n.blup
				a2$PER<-a2$ON/(a2$ON+a2$OFF)
				a2$LEN<-a2$ON+a2$OFF
				a2$ONSET<-a2$OnsetInter+a2$Onset.f.blup+a2$Onset.n.blup
						
			## join nest data to nestling data frame
				an<-a2[,c("Female","NestID","Color","ClutchSize","PredorHatch","Year","ON","OFF","PER","LEN","ONSET")]	
				n1<-join(n1,an,"NestID")
				n1$ONSET2<-n1$ONSET/60
					
			## Set na actions back to normal	
				options(na.action=na.omit)

	## Make some plots of incubation vs. ambient/date conditions
		
		## Plot of first off bout time vs. date (not a strong relationship)
			pdf("Output_Onset_Fig.pdf",width=4.8,height=5.4)
				plot(d2.off.1st$Jdate,d2.off.1st$StartTime/60/60,ylim=c(16000/60/60,23000/60/60),pch=16,col=col.alpha("slateblue",0.6),
					ylab="Time (hours after midnight)",xlab="Day of year")
				lines(lowess(s1$jdate,s1$sunrise/60),lwd=3,col="orange")
				abline(lm(d2.off.1st$StartTime/60/60~d2.off.1st$Jdate),lwd=2)
				legend("bottomright",c("1st off bout","civil sunrise"),pch=c(16,NA),lwd=c(0,3),col=c("slateblue","orange"),bty="n",)
			dev.off()
			summary(lm(StartTime~Jdate,data=d2.off.1st))
			
	## Plot relationships with bout length and environment (ambient temp, time of day)
	
		pdf("Output_Env_Covars.pdf",height=6.5,width=7)
			par(mfrow=c(2,2))
			par(mar=c(3.8,4.1,4.1,2.1))
			plot(d2.on$Ambient,d2.on$Length.Minute+runif(nrow(d2.on),-1,1),col=col.alpha("slateblue",0.2),
				pch=16,ylab="On bout length (min)",
				xlab="Ambient temperature (°C)",xlim=c(8,35),ylim=c(0,90), yaxt = "n")
			axis(2, seq(-10, 100, 10))
			abline(lm(Length.Minute~Ambient,data=d2.on),lwd=2)
			corner.label("A")
			plot(d2.off$Ambient,d2.off$Length.Minute+runif(nrow(d2.off),-1,1),col=col.alpha("slateblue",0.2),pch=16,
				ylab="Off bout length (min)",xlab="Ambient temperature (°C)",xlim=c(8,35),ylim=c(0,35), yaxt = "n")
			axis(2, seq(-10, 100, 10))
			abline(lm(Length.Minute~Ambient,data=d2.off),lwd=2)
			corner.label("B")
			par(mar=c(5.1,4.1,2.8,2.1))
			plot(d2.on$StartTime/60/60,d2.on$Length.Minute+runif(nrow(d2.on),-1,1),col=col.alpha("slateblue",0.2),
				pch=16,ylab="On bout length (min)",xlab="Start time (hour)",ylim=c(0,90), yaxt = "n")
			axis(2, seq(-10, 100, 10))
				m<-lm(Length.Minute~StartTime+I(StartTime^2),data=d2.on)
				r<-as.data.frame(seq(0,24,0.1)*60*60)
				colnames(r)<-"StartTime"
				ys<-predict(m,r)
				lines(r[,1]/60/60,ys,lwd=2,col="black")
				corner.label("C")
			plot(d2.off$StartTime/60/60,d2.off$Length.Minute+runif(nrow(d2.off),-1,1),col=col.alpha("slateblue",0.2),
				pch=16,ylab="Off bout length (min)",xlab="Start time (hour)",ylim=c(0,35), yaxt = "n")
			axis(2, seq(-10, 100, 10))
				m<-lm(Length.Minute~StartTime+I(StartTime^2),data=d2.off)
				r<-as.data.frame(seq(0,24,0.1)*60*60)
				colnames(r)<-"StartTime"
				ys<-predict(m,r)
				lines(r[,1]/60/60,ys,lwd=2,col="black")
				corner.label("D")
		dev.off()

####################################################################################################################
## This section calculates within-individual repeatability of incubation on various time scales.			      ##
####################################################################################################################
	
	## Analyze repeatability of female incubation behavior across bouts, nests, and years
		## Need to fix these after excluding night time bouts and final nest inclusion data set
	
		runner<-0			## This takes a long time to run and is set not to run. To run, change this to '0'.
		if(runner==1){
	
		## Make a table
			rep.table<-as.data.frame(matrix(nrow=20,ncol=7))
			colnames(rep.table)<-c("Behavior","Comp","R","CI.low","CI.high","Permut_P","Covar")
			rep.table[,1]<-rep(c("On","Off","Cycle","Constancy","Onset"),4)
			rep.table[,2]<-rep(c(rep("Bouts",5),rep("Nests",5)),2)
	
		# Across bouts within a single nesting attempt [these take a long time to run so I've added stats here and hashed out]
			# Repeatability of on bout length
				on.nest<-rpt(Length~1+(1|NestID),grname="NestID",datatype="Gaussian",data=d2.on,npermut=1000)	
				rep.table[1,3:7]<-c(on.nest$R,on.nest$CI_emp[1],on.nest$CI_emp[2],on.nest$P[2],"NA")
			# Repeatability of off bout lenth
				off.nest<-rpt(Length~1+(1|NestID),grname="NestID",datatype="Gaussian",data=d2.off,npermut=1000)
				rep.table[2,3:7]<-c(off.nest$R,off.nest$CI_emp[1],off.nest$CI_emp[2],off.nest$P[2],"NA")	
			# Repeatability of onset of daily activity
				onset.nest<-rpt(StartTime~1+(1|NestID),grname="NestID",datatype="Gaussian",data=d2.off.1st,npermut=1000)	
				rep.table[5,3:7]<-c(onset.nest$R,onset.nest$CI_emp[1],onset.nest$CI_emp[2],onset.nest$P[2],"NA")
				
		# Across multiple nests in the same year
			# Repeatability of on bout length
				on.year<-rpt(OnBout~1+(1|Female),grname="Female",datatype="Gaussian",data=a1,npermut=1000)	
				rep.table[6,3:7]<-c(on.year$R,on.year$CI_emp[1],on.year$CI_emp[2],on.year$P[2],"NA")
				off.year<-rpt(OffBout~1+(1|Female),grname="Female",datatype="Gaussian",data=a1,npermut=1000)	
				rep.table[7,3:7]<-c(off.year$R,off.year$CI_emp[1],off.year$CI_emp[2],off.year$P[2],"NA")
				len.year<-rpt(CycleLength~1+(1|Female),grname="Female",datatype="Gaussian",data=a1,npermut=1000)	
				rep.table[8,3:7]<-c(len.year$R,len.year$CI_emp[1],len.year$CI_emp[2],len.year$P[2],"NA")
				cons.year<-rpt(PercentOn~1+(1|Female),grname="Female",datatype="Gaussian",data=a1,npermut=1000)	
				rep.table[9,3:7]<-c(cons.year$R,cons.year$CI_emp[1],cons.year$CI_emp[2],cons.year$P[2],"NA")
				onset.year<-rpt(FirstBoutTime~IncStartDate+(1|Female),grname="Female",datatype="Gaussian",data=a1,npermut=1000)
				rep.table[10,3:7]<-c(onset.year$R,onset.year$CI_emp[1],onset.year$CI_emp[2],onset.year$P[2],"NA")
				
		# Across bouts within a single nesting attempt [these take a long time to run so I've added stats here and hashed out]
			# Repeatability of on bout length
				on.nest<-rpt(Length~Ambient.s+as.factor(Year)+StartTime.s+I(StartTime.s^2)+
					(1|NestID),grname="NestID",datatype="Gaussian",data=d2.on,npermut=1000)	
				rep.table[11,3:7]<-c(on.nest$R,on.nest$CI_emp[1],on.nest$CI_emp[2],on.nest$P[2],"Yes")
			# Repeatability of off bout lenth
				off.nest<-rpt(Length~Ambient.s+Jdate.s+I(StartTime.s^2)+(1|NestID),grname="NestID",datatype="Gaussian",data=d2.off,npermut=1000)
				rep.table[12,3:7]<-c(off.nest$R,off.nest$CI_emp[1],off.nest$CI_emp[2],off.nest$P[2],"Yes")	
			# Repeatability of onset of daily activity
				onset.nest<-rpt(StartTime~Ambient.s+as.factor(Year)+Jdate.s+Sunrise.Seconds.s+
					(1|NestID),grname="NestID",datatype="Gaussian",data=d2.off.1st,npermut=1000)	
				rep.table[15,3:7]<-c(onset.nest$R,onset.nest$CI_emp[1],onset.nest$CI_emp[2],onset.nest$P[2],"Yes")
				
		# Across multiple nests in the same year
			# Repeatability of on bout length
				on.year<-rpt(ON~1+(1|Female),grname="Female",datatype="Gaussian",data=a2,npermut=1000)	
				rep.table[16,3:7]<-c(on.year$R,on.year$CI_emp[1],on.year$CI_emp[2],on.year$P[2],"Yes")
				off.year<-rpt(OFF~1+(1|Female),grname="Female",datatype="Gaussian",data=a2,npermut=1000)	
				rep.table[17,3:7]<-c(off.year$R,off.year$CI_emp[1],off.year$CI_emp[2],off.year$P[2],"Yes")
				len.year<-rpt(LEN~1+(1|Female),grname="Female",datatype="Gaussian",data=a2,npermut=1000)	
				rep.table[18,3:7]<-c(len.year$R,len.year$CI_emp[1],len.year$CI_emp[2],len.year$P[2],"Yes")
				cons.year<-rpt(PER~1+(1|Female),grname="Female",datatype="Gaussian",data=a2,npermut=1000)	
				rep.table[19,3:7]<-c(cons.year$R,cons.year$CI_emp[1],cons.year$CI_emp[2],cons.year$P[2],"Yes")
				onset.year<-rpt(ONSET~IncStartDate+(1|Female),grname="Female",datatype="Gaussian",data=a2,npermut=1000)
				rep.table[20,3:7]<-c(onset.year$R,onset.year$CI_emp[1],onset.year$CI_emp[2],onset.year$P[2],"Yes")
			
			write.csv(rep.table,"Output_Repeatability_Table.csv")
			
		}
	
####################################################################################################################
## This section tests whether any aspects of female condition/quality are related to incubation.			      ##
####################################################################################################################	
	
	## Is there evidence that incubation is costly: measures of inc as predictors of condition or hematocrit
		## Subset of females with banding measures during current nest. Note that while all females were captured at least
			# once for sampling, they were not all captured during each nest attempt so that the measurements for things
			# like physiology or mass might be from different dates or even a different year in some cases. This subset is
			# only birds measured during the current nesting attempt.
				a2$DateOffset<-a2$CapDate-a2$IncStartDate
				a2.cap<-subset(a2,a2$DateOffset>0 & a2$DateOffset<13)
				a2.cap$ONSET2<-a2.cap$ONSET/60		# this is scaled to minutes to fit better
				a2.cap$PER.s<-scale(a2.cap$PER)
				a2.cap$LEN.s<-scale(a2.cap$LEN)
				a2.cap$ONSET2.s<-scale(a2.cap$ONSET2)
				a2.cap$Fwing.s <- scale(a2.cap$Fwing)
				a2.cap$Fcond.s <- scale(a2.cap$Fcond)
				
				m.cond<-lmer(Fcond~PER.s+LEN.s+ONSET2.s+(1|Color),data=a2.cap)
				m.cond.PER<-lmer(Fcond~LEN.s+ONSET2.s+(1|Color),data=a2.cap)
				m.cond.LEN<-lmer(Fcond~PER.s+ONSET2.s+(1|Color),data=a2.cap)
				m.cond.ONSET<-lmer(Fcond~PER.s+LEN.s+(1|Color),data=a2.cap)
				anova(m.cond,m.cond.PER,test="LRT")
				anova(m.cond,m.cond.LEN,test="LRT")
				anova(m.cond,m.cond.ONSET,test="LRT")
				confint(m.cond)
				
			
				m.fwing<-lmer(Fwing~PER.s+LEN.s+ONSET2.s+(1|Color),data=a2.cap)
				m.fwing.PER<-lmer(Fwing~LEN.s+ONSET2.s+(1|Color),data=a2.cap)
				m.fwing.LEN<-lmer(Fwing~PER.s+ONSET2.s+(1|Color),data=a2.cap)
				m.fwing.ONSET<-lmer(Fwing~PER.s+LEN.s+(1|Color),data=a2.cap)
				anova(m.fwing, m.fwing.PER,test="LRT")
				anova(m.fwing, m.fwing.LEN,test="LRT")
				anova(m.fwing, m.fwing.ONSET,test="LRT")
				confint(m.fwing)
				
				#Modified for revision with predictor and response variables swapped
				
				mc1 <- lmer(PER ~ Fcond.s + (1|Color), data = a2.cap)
				mc1n <- lmer(PER ~ 1 + (1|Color), data = a2.cap)
				anova(mc1, mc1n, test = "LRT")
				confint(mc1)
				summary(mc1)
				mc2 <- lmer(LEN ~ Fcond.s + (1|Color), data = a2.cap)
				mc2n <- lmer(LEN ~ 1 + (1|Color), data = a2.cap)
				anova(mc2, mc2n, test = "LRT")
				confint(mc2)
				mc3 <- lmer(ONSET2/60 ~ Fcond.s + (1|Color), data = a2.cap)
				mc3n <- lmer(ONSET2/60 ~ 1 + (1|Color), data = a2.cap)
				anova(mc3, mc3n, test = "LRT")
				confint(mc3)
				
				
				mw1 <- lmer(PER ~ Fwing.s + (1|Color), data = a2.cap)
				mw1n <- lmer(PER ~ 1 + (1|Color), data = a2.cap)
				anova(mw1, mw1n, test = "LRT")
				confint(mw1)
				summary(mw1)
				mw2 <- lmer(LEN ~ Fwing.s + (1|Color), data = a2.cap)
				mw2n <- lmer(LEN ~ 1 + (1|Color), data = a2.cap)
				anova(mw2, mw2n, test = "LRT")
				confint(mw2)
				summary(mw2)
				mw3 <- lmer(ONSET2/60 ~ Fwing.s + (1|Color), data = a2.cap)
				mw3n <- lmer(ONSET2/60 ~ 1 + (1|Color), data = a2.cap)
				anova(mw3, mw3n, test = "LRT")
				confint(mw3)
				summary(mw3)
				
					
####################################################################################################################
## This section tests whether incubation has any relationship with nestling phenotype/survival.				      ##
####################################################################################################################	
				
			
	## Test for effect of incubation characteristics on nestling phenotpyes
			n2<-subset(n1,n1$Weight>0.1&n1$Wing>0.1)			
			cond.n<-as.data.frame(resid(lm(n2$Weight~n2$Wing)))
			colnames(cond.n)<-"Ncond"
			cond.n$NestID<-n2$NestID
			n2<-join(n2,cond.n,"NestID")
			n2$PER.s<-scale(n2$PER)
			n2$LEN.s<-scale(n2$LEN)
			n2$ONSET2.s<-scale(n2$ONSET2)
			
			n.con<-lmer(Ncond~PER+LEN+ONSET2+(1|NestID)+(1|Color),data=n2)
			n.con.PER<-lmer(Ncond ~LEN+ONSET2+(1|NestID)+(1|Color),data=n2)
			n.con.LEN<-lmer(Ncond ~PER+ONSET2+(1|NestID)+(1|Color),data=n2)
			n.con.ONSET2<-lmer(Ncond ~PER+LEN+(1|NestID)+(1|Color),data=n2)
			anova(n.con,n.con.PER,test="LRT")
			anova(n.con,n.con.LEN,test="LRT")
			anova(n.con,n.con.ONSET2,test="LRT")
			confint(n.con)
			
			n.wt<-lmer(Weight~PER.s+LEN.s+ONSET2.s+(1|NestID)+(1|Color),data=n2)
			n.wt.PER<-lmer(Weight ~LEN.s+ONSET2.s+(1|NestID)+(1|Color),data=n2)
			n.wt.LEN<-lmer(Weight ~PER.s+ONSET2.s+(1|NestID)+(1|Color),data=n2)
			n.wt.ONSET2<-lmer(Weight ~PER.s+LEN.s+(1|NestID)+(1|Color),data=n2)
			anova(n.wt,n.wt.PER,test="LRT")
			anova(n.wt,n.wt.LEN,test="LRT")
			anova(n.wt,n.wt.ONSET2,test="LRT")
			confint(n.wt)
			
			n.wg<-lmer(Wing~PER.s+LEN.s+ONSET2.s+(1|NestID)+(1|Color),data=n2)
			n.wg.PER<-lmer(Wing ~LEN.s+ONSET2.s+(1|NestID)+(1|Color),data=n2)
			n.wg.LEN<-lmer(Wing ~PER.s+ONSET2.s+(1|NestID)+(1|Color),data=n2)
			n.wg.ONSET2<-lmer(Wing ~PER.s+LEN.s+(1|NestID)+(1|Color),data=n2)
			anova(n.wg,n.wg.PER,test="LRT")
			anova(n.wg,n.wg.LEN,test="LRT")
			anova(n.wg,n.wg.ONSET2,test="LRT")
			confint(n.wg)
			
	## Make plot of nestling hatching success with respect to cycle length and onset
		pdf("Output_Egg_Hatch.pdf",width=7,height=5)
			par(mfrow=c(1,2))
			boxplot(n1$LEN~n1$Fate.at.Hatch,pars=list(outcol="white"),names=c("Hatched","Did not hatch"),
				ylab="Incubation cycle length (minutes)", xlab = "")
			for(i in 1:nrow(n1)){
				ifelse(n1$Fate.at.Hatch[i]=="H",n1$xx[i]<-1,n1$xx[i]<-2)
				}
			points(n1$xx+runif(nrow(n1),-.2,.2),n1$LEN,col=col.alpha("slateblue",0.35),pch=16)
			corner.label("(A)")
			n1$ONSET_hr <- n1$ONSET2 / 60
			boxplot(n1$ONSET_hr~n1$Fate.at.Hatch,pars=list(outcol="white"),names=c("Hatched","Did not hatch"),
				ylab="Onset of activity (hours after midnight)", xlab = "")
			points(n1$xx+runif(nrow(n1),-.2,.2),n1$ONSET_hr,col=col.alpha("slateblue",0.35),pch=16)
			corner.label("(B)")
		dev.off()
		
		
		# Revised version of above plot for BEAS revision
		
		  colnames(n1)[3] <- "Year2"
		  library(ggplot2)
		  pa <- ggplot(data = n1, mapping = aes(x = LEN, y = Fate.at.Hatch)) +
		    geom_boxplot(outlier.shape = NA) + theme_classic() +
		    geom_jitter(height = 0.2, width = 0, color = "slateblue", alpha = 0.4, size = 2) +
		    xlab("Incubation cycle length (minutes)") +
		    ylab("") +
		    scale_y_discrete(labels = c("Hatched", "Did not hatch")) +
		    theme(axis.text.y = element_text(size = 11)) +
		    annotate("text", x = 31, y = 2.5, label = "(A)")
		    #theme(axis.text.y = element_text(angle = 90))
		  
		  pb <- ggplot(data = n1, mapping = aes(x = ONSET2/60, y = Fate.at.Hatch)) +
		    geom_boxplot(outlier.shape = NA) + theme_classic() +
		    geom_jitter(height = 0.2, width = 0, color = "slateblue", alpha = 0.4, size = 2) +
		    xlab("Onset of activity (hours after midnight)") +
		    ylab("") +
		    scale_y_discrete(labels = c("Hatched", "Did not hatch")) +
		    theme(axis.text.y = element_text(size = 11)) +
		    xlim(5.1, 6.1) +
		    annotate("text", x = 5.1, y = 2.5, label = "(B)")
		  #theme(axis.text.y = element_text(angle = 90))
		  
		  ggsave("hatch_vs_inc.png", ggpubr::ggarrange(pa, pb, nrow = 2), width = 5.8, height = 5, device = "png")
			
	## Test for effect of incubation on hatching at nestling level
		n3<-subset(n1,n1$PredorHatch=="Hatch")
		n3$PER.s<-scale(n3$PER)
		n3$LEN.s<-scale(n3$LEN)
		n3$ONSET2.s<-scale(n3$ONSET2)
		m.hatch<-glmer(Fate.at.Hatch~PER.s+LEN.s+ONSET2.s+(1|NestID)+(1|Color),data=n3,family="binomial")
		m.hatch.LEN<-glmer(Fate.at.Hatch~PER.s+ONSET2.s+(1|NestID)+(1|Color),data=n3,family="binomial")
		m.hatch.PER<-glmer(Fate.at.Hatch~LEN.s+ONSET2.s+(1|NestID)+(1|Color),data=n3,family="binomial")
		m.hatch.ONSET<-glmer(Fate.at.Hatch~PER.s+LEN.s+(1|NestID)+(1|Color),data=n3,family="binomial")
		anova(m.hatch,m.hatch.LEN,test="LRT")
		anova(m.hatch,m.hatch.PER,test="LRT")
		anova(m.hatch,m.hatch.ONSET,test="LRT")
		confint(m.hatch)
		summary(m.hatch)
				
	## Test for effect of incubation on hatching/predation likelihood [NOT IN CURRENT VERSION - NO EFFECT]
		a.surv<-a2[,c("Female","Color","IncStartDate","MidFateDay","PredorHatch","SureOfTiming","ON","OFF","PER","LEN","ONSET")]
		a.surv<-na.omit(a.surv)
		for(i in 1:nrow(a.surv)){
			ifelse(a.surv$PredorHatch[i]=="Hatch",a.surv$censored[i]<-1,a.surv$censored[i]<-2)
		}
		a.surv$Time<-a.surv$MidFateDay-a.surv$IncStartDate
		a.surv$ONSET2<-a.surv$ONSET/60
		a.surv$PER.s<-scale(a.surv$PER)
		a.surv$LEN.s<-scale(a.surv$LEN)
		a.surv$ONSET2.s<-scale(a.surv$ONSET2)
		m.surv<-glmer(PredorHatch~PER.s+LEN.s+ONSET2.s+(1|Color),data=a.surv,family="binomial")
		m.surv.PER<-glmer(PredorHatch~LEN.s+ONSET2.s+(1|Color),data=a.surv,family="binomial")
		m.surv.LEN<-glmer(PredorHatch~PER.s+ONSET2.s+(1|Color),data=a.surv,family="binomial")
		m.surv.ONSET<-glmer(PredorHatch~PER.s+LEN.s+(1|Color),data=a.surv,family="binomial")
		anova(m.surv,m.surv.PER,test="LRT")
		anova(m.surv,m.surv.LEN,test="LRT")
		anova(m.surv,m.surv.ONSET,test="LRT")
		summary(m.surv)
		confint(m.surv)

	## Test for effect of incubation constancy on incubation period
		pdf("Output_IncPeriod.pdf",height=6,width=4.4)
		
			dx<-na.omit(a2[,c("IncPeriod","PER","LEN","ONSET","Female","Color")])
			for(i in 1:nrow(dx)){
				ifelse(dx$IncPeriod[i]==11,dx$x[i]<-1,
					ifelse(dx$IncPeriod[i]==12,dx$x[i]<-2,
					ifelse(dx$IncPeriod[i]==13,dx$x[i]<-3,
					dx$x[i]<-5)))
			}
			dx$Female<-as.character(dx$Female)
			dx$Color<-as.character(dx$Color)
			dx[nrow(dx)+1,]<-c(14,0.2,15,500,"F20xx.xx","xx",4)
			dx$PER<-as.numeric(dx$PER)
			dx$LEN<-as.numeric(dx$LEN)
			dx$ONSET<-as.numeric(dx$ONSET)
			dx$IncPeriod<-as.factor(dx$IncPeriod)
			dx$x<-as.numeric(dx$x)
			boxplot(dx$PER~dx$IncPeriod,xlab="Incubation period (days)",
				ylab="Incubation constancy (% on nest)",ylim=c(0.6,0.8), yaxt = "n")
			points(dx$x+runif(nrow(dx),-0.2,0.2),dx$PER,pch=16,col=col.alpha("slateblue",0.7))
			axis(2, seq(0.5, 0.9, 0.05), labels = seq(50, 90, 5), las = 2)
		
		dev.off()
		
		## Figure for revision
		
		  library(ggplot2)
		  p1 <- ggplot(data = dx, mapping = aes(x = PER, y = IncPeriod)) +
		    geom_boxplot() + geom_jitter(height = 0.2, width = 0, color = "slateblue", alpha = 0.6, size = 2) + 
		    theme_classic() +
		    xlim(0.6, 0.8) +
		    xlab("Incubation constancy (% on nest)") +
		    ylab("Incubation period (days)")
		  ggsave("period_by_constancy.png", p1, device = "png", width = 4.5, height = 2.8)

			dx2<-subset(dx,dx$PER>0.4)
			dx2$PER.s<-scale(dx2$PER)
			dx2$LEN.s<-scale(dx2$LEN)
			dx2$ONSET2<-dx2$ONSET/60
			dx2$ONSET2.s<-scale(dx2$ONSET2)
			dx2$IncPeriod2<-as.numeric(as.character(dx2$IncPeriod))
			m.period<-lmer(IncPeriod2~PER.s+LEN.s+ONSET2.s+(1|Color),data=dx2)
			m.period.PER<-lmer(IncPeriod2~LEN.s+ONSET2.s+(1|Color),data=dx2)
			m.period.LEN<-lmer(IncPeriod2~PER.s+ONSET2.s+(1|Color),data=dx2)
			m.period.ONSET<-lmer(IncPeriod2~PER.s+LEN.s+(1|Color),data=dx2)
			
	
			anova(m.period,m.period.PER,test="LRT")
			anova(m.period,m.period.LEN,test="LRT")
			anova(m.period,m.period.ONSET,test="LRT")
			summary(m.period)
			confint(m.period)
		
		
####################################################################################################################
## This section determines whether incubation charactistics are related to female ornamentation.			      ##
####################################################################################################################	
	

		m.PER<-lmer(PER*100~FUVSat.s+Fyel.s+FCCar.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.PER.uv<-lmer(PER*100~Fyel.s+FCCar.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.PER.yel<-lmer(PER*100~FUVSat.s+FCCar.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.PER.ccar<-lmer(PER*100~FUVSat.s+Fyel.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.PER.bib<-lmer(PER*100~FUVSat.s+Fyel.s+FCCar.s+Age+(1|Female)+(1|Color),data=a2)
		m.PER.age<-lmer(PER*100~FUVSat.s+Fyel.s+FCCar.s+Fbib.s+(1|Female)+(1|Color),data=a2)
		anova(m.PER,m.PER.uv,test="LRT")
		anova(m.PER,m.PER.yel,test="LRT")
		anova(m.PER,m.PER.ccar,test="LRT")
		anova(m.PER,m.PER.bib,test="LRT")
		anova(m.PER,m.PER.age,test="LRT")
		summary(m.PER)
		confint(m.PER)

		m.ONSET<-lmer(ONSET/60~FUVSat.s+Fyel.s+FCCar.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.ONSET.uv<-lmer(ONSET/60~Fyel.s+FCCar.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.ONSET.yel<-lmer(ONSET/60~FUVSat.s+FCCar.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.ONSET.ccar<-lmer(ONSET/60~FUVSat.s+Fyel.s+Fbib.s+Age+(1|Female)+(1|Color),data=a2)
		m.ONSET.bib<-lmer(ONSET/60~FUVSat.s+Fyel.s+FCCar.s+Age+(1|Female)+(1|Color),data=a2)
		m.ONSET.age<-lmer(ONSET/60~FUVSat.s+Fyel.s+FCCar.s+Fbib.s+(1|Female)+(1|Color),data=a2)
		anova(m.ONSET,m.ONSET.uv,test="LRT")
		anova(m.ONSET,m.ONSET.yel,test="LRT")
		anova(m.ONSET,m.ONSET.ccar,test="LRT")
		anova(m.ONSET,m.ONSET.bib,test="LRT")
		anova(m.ONSET,m.ONSET.age,test="LRT")
		summary(m.ONSET)
		confint(m.ONSET)
		
		m.LEN<-lmer(LEN~FUVSat.s+Fyel.s+FCCar.s+Fbib.s+Age+(1|Color),data=a2)
		m.LEN.uv<-lmer(LEN~Fyel.s+FCCar.s+Fbib.s+Age+(1|Color),data=a2)
		m.LEN.yel<-lmer(LEN~FUVSat.s+FCCar.s+Fbib.s+Age+(1|Color),data=a2)
		m.LEN.ccar<-lmer(LEN~FUVSat.s+Fyel.s+Fbib.s+Age+(1|Color),data=a2)
		m.LEN.bib<-lmer(LEN~FUVSat.s+Fyel.s+FCCar.s+Age+(1|Color),data=a2)
		m.LEN.age<-lmer(LEN~FUVSat.s+Fyel.s+FCCar.s+Fbib.s+(1|Color),data=a2)
		anova(m.LEN,m.LEN.uv,test="LRT")
		anova(m.LEN,m.LEN.yel,test="LRT")
		anova(m.LEN,m.LEN.ccar,test="LRT")
		anova(m.LEN,m.LEN.bib,test="LRT")
		anova(m.LEN,m.LEN.age,test="LRT")
		summary(m.LEN)
		confint(m.LEN)
		
		
	## Make a figure
		pdf("Output_Female_Inc.pdf",width=8,height=4.5)
		par(mfrow=c(1,2))
		plot(a2$Fwing,a2$PER,col=col.alpha("slateblue",0.6),pch=16,ylab="Incubation constancy",
			xlab="Wing length (mm)", yaxt = "n", ylim = c(0.62, 0.79))
		axis(2, seq(0, 1, 0.05))
		corner.label("(A)")
		abline(lm(a2$PER~a2$Fwing))
		plot(a2$FCCar,a2$PER,col=col.alpha("slateblue",0.6),pch=16,ylab="Incubation constancy",
			xlab="Bib carotenoid chroma",xlim=c(0.7,1), xaxt = "n", ylim = c(0.62, 0.79))
		axis(1, seq(0, 1, 0.1))
		corner.label("(B)")
		abline(lm(a2$PER~a2$FCCar))
		dev.off()
		
		
		
		
	