# ------------------------------------------------
# ------------------------------------------------
# MANUAL CONTENT ANALYSIS DATA--------------------
# ------------------------------------------------
# ------------------------------------------------

p2009  <- read.spss("./ZA5307_v1-0-0.sav",to.data.frame=T,use.value.labels=F)
tv2009 <- read.spss("./ZA5306_v1-2-0.sav",to.data.frame=T,use.value.labels=F)
p2013  <- read.spss("./ZA5706_v1-0-0.sav",to.data.frame=T,use.value.labels=F)
tv2013 <- read.spss("./ZA5705_v1-0-0.sav",to.data.frame=T,use.value.labels=F)

p2009F  <- read.spss("./ZA5307_v1-0-0.sav",to.data.frame=T,use.value.labels=T)
tv2009F <- read.spss("./ZA5306_v1-2-0.sav",to.data.frame=T,use.value.labels=T)
p2013F  <- read.spss("./ZA5706_v1-0-0.sav",to.data.frame=T,use.value.labels=T)
tv2013F <- read.spss("./ZA5705_v1-0-0.sav",to.data.frame=T,use.value.labels=T)

p2009$B12 #politics
p2009$B14 #polity
p2009$B16 #policy

p2009$B12 <- replace(p2009$B12,is.na(p2009$B12),0)
p2009$B14 <- replace(p2009$B14,is.na(p2009$B14),0)
p2009$B16 <- replace(p2009$B16,is.na(p2009$B16),0)

p2009$date <- as.Date(paste0(p2009$V03+2000,"-",p2009$V04,"-",p2009$V05),format="%Y-%m-%d")

p2009$bildung		<- (p2009$B16>4099 & p2009$B16<4200)*1 
p2009$energie		<- (p2009$B16>3519 & p2009$B16<3523)*1 + (p2009$B16>3526 & p2009$B16<3530)*1
p2009$EU			<- (p2009$B16>3109 & p2009$B16<3120)*1 
p2009$foederalismus	<- (p2009$B12>2200 & p2009$B12<2399)*1 
p2009$gesundheit		<- (p2009$B16>3759 & p2009$B16<3800)*1
p2009$haushalt		<- (p2009$B16>4299 & p2009$B16<4320)*1 
p2009$einnahmen		<- (p2009$B16>4319 & p2009$B16<4400)*1 
p2009$infrastruktur	<- (p2009$B16>3499 & p2009$B16<3520)*1 + (p2009$B16>3539 & p2009$B16<3600)*1
p2009$inneres		<- (p2009$B16>3419 & p2009$B16<3499)*1
p2009$extremismus		<- (p2009$B16>3411 & p2009$B16<3416)*1
p2009$geheimdienste	<- (p2009$B16==3423)*1 + (p2009$B16==3424)*1
p2009$kriminalitaet	<- (p2009$B16>3409 & p2009$B16<3412)*1 + (p2009$B16==3419)*1
p2009$internationales	<- (p2009$B16>3119 & p2009$B16<3179)*1 + (p2009$B16>3199 & p2009$B16<3300)*1
p2009$int.konfl		<- (p2009$B16>3179 & p2009$B16<3200)*1
p2009$int.terror		<- (p2009$B16==3181)*1			   
p2009$migration		<- (p2009$B16>3749 & p2009$B16<3760)*1
p2009$verdrossenheit	<- (p2009$B12>1099 & p2009$B12<1199)*1 + (p2009$B14>2009 & p2009$B12<2199)*1 + (p2009$B12>2399 & p2009$B12<2499)*1 + (p2009$B16>3415 & p2009$B16<3419)*1
p2009$sonstiges		<- (p2009$B12>1600 & p2009$B12<1999)*1 + (p2009$B12>2499 & p2009$B12<2999)*1 + (p2009$B16>3729 & p2009$B16<3740)*1 + (p2009$B16>4000 & p2009$B16<4099)*1 + (p2009$B16>4199 & p2009$B16<4300)*1 + (p2009$B16>4399 & p2009$B16<4999)*1  
p2009$arbeit		<- (p2009$B16>3799 & p2009$B16<3840)*1 + (p2009$B16>3869 & p2009$B16<3899)*1
p2009$einkommen		<- (p2009$B16>3719 & p2009$B16<3730)*1 + (p2009$B16>3839 & p2009$B16<3870)*1
p2009$familie		<- (p2009$B16>3709 & p2009$B16<3720)*1
p2009$rente			<- (p2009$B16>3739 & p2009$B16<3750)*1
p2009$wohnen		<- (p2009$B16>3529 & p2009$B16<3540)*1
p2009$umwelt		<- (p2009$B16>3599 & p2009$B16<3120)*1 + (p2009$B16>3629 & p2009$B16<3700)*1
p2009$klima			<- (p2009$B16>3619 & p2009$B16<3630)*1 
p2009$verteidigung	<- (p2009$B16>3300 & p2009$B16<3400)*1
p2009$wirtschaft		<- (p2009$B16==3910)*1 + (p2009$B16==3912)*1 + (p2009$B16==3913)*1 + (p2009$B16==3920)*1 + (p2009$B16==3921)*1 + (p2009$B16==3924)*1 + (p2009$B16==3930)*1 + (p2009$B16>3949 & p2009$B16<3999)*1 
p2009$krise			<- (p2009$B16==3911)*1 + (p2009$B16>3914 & p2009$B16<3919)*1 + (p2009$B16==3922)*1 + (p2009$B16==3923)*1 + (p2009$B16>3939 & p2009$B16<3950)*1     
p2009$wahl			<- (p2009$B12>1199 & p2009$B12<1599)*1 

tv2009$B14 <- replace(tv2009$B14,is.na(tv2009$B14),0)
tv2009$B16 <- replace(tv2009$B16,is.na(tv2009$B16),0)
tv2009$B18 <- replace(tv2009$B18,is.na(tv2009$B18),0)

tv2009$date <- as.Date(paste0(tv2009$V03,"-",tv2009$V04,"-",tv2009$V05),format="%Y-%m-%d")

tv2009$bildung		<- (tv2009$B18>4099 & tv2009$B18<4200)*1 
tv2009$energie		<- (tv2009$B18>3519 & tv2009$B18<3523)*1 + (tv2009$B18>3526 & tv2009$B18<3530)*1
tv2009$EU			<- (tv2009$B18>3109 & tv2009$B18<3120)*1 
tv2009$foederalismus	<- (tv2009$B14>2200 & tv2009$B14<2399)*1 
tv2009$gesundheit		<- (tv2009$B18>3759 & tv2009$B18<3800)*1
tv2009$haushalt		<- (tv2009$B18>4299 & tv2009$B18<4320)*1 
tv2009$einnahmen		<- (tv2009$B18>4319 & tv2009$B18<4400)*1 
tv2009$infrastruktur	<- (tv2009$B18>3499 & tv2009$B18<3520)*1 + (tv2009$B18>3539 & tv2009$B18<3600)*1
tv2009$inneres		<- (tv2009$B18>3419 & tv2009$B18<3499)*1
tv2009$extremismus		<- (tv2009$B18>3411 & tv2009$B18<3416)*1
tv2009$geheimdienste	<- (tv2009$B18==3423)*1 + (tv2009$B18==3424)*1
tv2009$kriminalitaet	<- (tv2009$B18>3409 & tv2009$B18<3412)*1 + (tv2009$B18==3419)*1
tv2009$internationales	<- (tv2009$B18>3119 & tv2009$B18<3179)*1 + (tv2009$B18>3199 & tv2009$B18<3300)*1
tv2009$int.konfl		<- (tv2009$B18>3179 & tv2009$B18<3200)*1
tv2009$int.terror		<- (tv2009$B18==3181)*1			   
tv2009$migration		<- (tv2009$B18>3749 & tv2009$B18<3760)*1
tv2009$verdrossenheit	<- (tv2009$B14>1099 & tv2009$B14<1199)*1 + (tv2009$B16>2009 & tv2009$B14<2199)*1 + (tv2009$B14>2399 & tv2009$B14<2499)*1 + (tv2009$B18>3415 & tv2009$B18<3419)*1
tv2009$sonstiges		<- (tv2009$B14>1600 & tv2009$B14<1999)*1 + (tv2009$B14>2499 & tv2009$B14<2999)*1 + (tv2009$B18>3729 & tv2009$B18<3740)*1 + (tv2009$B18>4000 & tv2009$B18<4099)*1 + (tv2009$B18>4199 & tv2009$B18<4300)*1 + (tv2009$B18>4399 & tv2009$B18<4999)*1  
tv2009$arbeit		<- (tv2009$B18>3799 & tv2009$B18<3840)*1 + (tv2009$B18>3869 & tv2009$B18<3899)*1
tv2009$einkommen		<- (tv2009$B18>3719 & tv2009$B18<3730)*1 + (tv2009$B18>3839 & tv2009$B18<3870)*1
tv2009$familie		<- (tv2009$B18>3709 & tv2009$B18<3720)*1
tv2009$rente			<- (tv2009$B18>3739 & tv2009$B18<3750)*1
tv2009$wohnen		<- (tv2009$B18>3529 & tv2009$B18<3540)*1
tv2009$umwelt		<- (tv2009$B18>3599 & tv2009$B18<3120)*1 + (tv2009$B18>3629 & tv2009$B18<3700)*1
tv2009$klima			<- (tv2009$B18>3619 & tv2009$B18<3630)*1 
tv2009$verteidigung	<- (tv2009$B18>3300 & tv2009$B18<3400)*1
tv2009$wirtschaft		<- (tv2009$B18==3910)*1 + (tv2009$B18==3912)*1 + (tv2009$B18==3913)*1 + (tv2009$B18==3920)*1 + (tv2009$B18==3921)*1 + (tv2009$B18==3924)*1 + (tv2009$B18==3930)*1 + (tv2009$B18>3949 & tv2009$B18<3999)*1 
tv2009$krise			<- (tv2009$B18==3911)*1 + (tv2009$B18>3914 & tv2009$B18<3919)*1 + (tv2009$B18==3922)*1 + (tv2009$B18==3923)*1 + (tv2009$B18>3939 & tv2009$B18<3950)*1     
tv2009$wahl			<- (tv2009$B14>1199 & tv2009$B14<1599)*1 


tv2009$B14 # politics
tv2009$B16 # polity
tv2009$B18 # policy


p2013$b12 <- replace(p2013$b12,is.na(p2013$b12),0)
p2013$b14 <- replace(p2013$b14,is.na(p2013$b14),0)
p2013$b16 <- replace(p2013$b16,is.na(p2013$b16),0)

p2013$date <- as.Date(paste0(p2013$v03+2000,"-",p2013$v04,"-",p2013$v05),format="%Y-%m-%d")

p2013$bildung		<- (p2013$b16>4099 & p2013$b16<4200)*1 
p2013$energie		<- (p2013$b16>3519 & p2013$b16<3523)*1 + (p2013$b16>3526 & p2013$b16<3530)*1
p2013$EU			<- (p2013$b16>3109 & p2013$b16<3120)*1 
p2013$foederalismus	<- (p2013$b12>2200 & p2013$b12<2399)*1 
p2013$gesundheit		<- (p2013$b16>3759 & p2013$b16<3800)*1
p2013$haushalt		<- (p2013$b16>4299 & p2013$b16<4320)*1 
p2013$einnahmen		<- (p2013$b16>4319 & p2013$b16<4400)*1 
p2013$infrastruktur	<- (p2013$b16>3499 & p2013$b16<3520)*1 + (p2013$b16>3539 & p2013$b16<3600)*1
p2013$inneres		<- (p2013$b16>3419 & p2013$b16<3433)*1 + 1*(p2013$b16==3437)
p2013$extremismus		<- (p2013$b16>3411 & p2013$b16<3416)*1
p2013$geheimdienste	<- (p2013$b16==3423)*1 + (p2013$b16==3424)*1 + 1*(p2013$b16>3432 & p2013$b16<3437)
p2013$kriminalitaet	<- (p2013$b16>3409 & p2013$b16<3412)*1 + (p2013$b16==3419)*1
p2013$internationales	<- (p2013$b16>3119 & p2013$b16<3179)*1 + (p2013$b16>3199 & p2013$b16<3300)*1
p2013$int.konfl		<- (p2013$b16>3179 & p2013$b16<3200)*1
p2013$int.terror		<- (p2013$b16==3181)*1			   
p2013$migration		<- (p2013$b16>3749 & p2013$b16<3760)*1
p2013$verdrossenheit	<- (p2013$b12>1099 & p2013$b12<1199)*1 + (p2013$b14>2009 & p2013$b12<2199)*1 + (p2013$b12>2399 & p2013$b12<2499)*1 + (p2013$b16>3415 & p2013$b16<3419)*1
p2013$sonstiges		<- (p2013$b12>1600 & p2013$b12<1999)*1 + (p2013$b12>2499 & p2013$b12<2999)*1 + (p2013$b16>3729 & p2013$b16<3740)*1 + (p2013$b16>4000 & p2013$b16<4099)*1 + (p2013$b16>4199 & p2013$b16<4300)*1 + (p2013$b16>4399 & p2013$b16<4999)*1  
p2013$arbeit		<- (p2013$b16>3799 & p2013$b16<3840)*1 + (p2013$b16>3869 & p2013$b16<3899)*1
p2013$einkommen		<- (p2013$b16>3719 & p2013$b16<3730)*1 + (p2013$b16>3839 & p2013$b16<3870)*1
p2013$familie		<- (p2013$b16>3709 & p2013$b16<3720)*1
p2013$rente			<- (p2013$b16>3739 & p2013$b16<3750)*1
p2013$wohnen		<- (p2013$b16>3529 & p2013$b16<3540)*1
p2013$umwelt		<- (p2013$b16>3599 & p2013$b16<3120)*1 + (p2013$b16>3629 & p2013$b16<3700)*1
p2013$klima			<- (p2013$b16>3619 & p2013$b16<3630)*1 
p2013$verteidigung	<- (p2013$b16>3300 & p2013$b16<3400)*1
p2013$wirtschaft		<- (p2013$b16==3910)*1 + (p2013$b16==3912)*1 + (p2013$b16==3913)*1 + (p2013$b16==3920)*1 + (p2013$b16==3921)*1 + (p2013$b16==3924)*1 + (p2013$b16==3930)*1 + (p2013$b16>3949 & p2013$b16<3999)*1 
p2013$krise			<- (p2013$b16==3911)*1 + (p2013$b16>3914 & p2013$b16<3919)*1 + (p2013$b16==3922)*1 + (p2013$b16==3923)*1 + (p2013$b16>3939 & p2013$b16<3950)*1     
p2013$wahl			<- (p2013$b12>1199 & p2013$b12<1599)*1 

length(table(c(tv2013$B16,tv2009$B18,p2009$B16,p2013$b16)))
length(table(c(tv2013$B14,tv2009$B16,p2009$B14,p2013$b14)))
length(table(c(tv2013$B12,tv2009$B14,p2009$B12,p2013$b12)))

tv2013$B12 <- replace(tv2013$B12,is.na(tv2013$B12),0)
tv2013$B14 <- replace(tv2013$B14,is.na(tv2013$B14),0)
tv2013$B16 <- replace(tv2013$B16,is.na(tv2013$B16),0)

tv2013$date <- as.Date(paste0(tv2013$V03+2000,"-",tv2013$V04,"-",tv2013$V05),format="%Y-%m-%d")

tv2013$bildung		<- (tv2013$B16>4099 & tv2013$B16<4200)*1 
tv2013$energie		<- (tv2013$B16>3519 & tv2013$B16<3523)*1 + (tv2013$B16>3526 & tv2013$B16<3530)*1
tv2013$EU			<- (tv2013$B16>3109 & tv2013$B16<3120)*1 
tv2013$foederalismus	<- (tv2013$B12>2200 & tv2013$B12<2399)*1 
tv2013$gesundheit		<- (tv2013$B16>3759 & tv2013$B16<3800)*1
tv2013$haushalt		<- (tv2013$B16>4299 & tv2013$B16<4320)*1 
tv2013$einnahmen		<- (tv2013$B16>4319 & tv2013$B16<4400)*1 
tv2013$infrastruktur	<- (tv2013$B16>3499 & tv2013$B16<3520)*1 + (tv2013$B16>3539 & tv2013$B16<3600)*1
tv2013$inneres		<- (tv2013$B16>3419 & tv2013$B16<3433)*1 + 1*(tv2013$B16==3437)
tv2013$extremismus	<- (tv2013$B16>3411 & tv2013$B16<3416)*1
tv2013$geheimdienste	<- (tv2013$B16==3423)*1 + (tv2013$B16==3424)*1 + 1*(tv2013$B16>3432 & tv2013$B16<3437)
tv2013$kriminalitaet	<- (tv2013$B16>3409 & tv2013$B16<3412)*1 + (tv2013$B16==3419)*1
tv2013$internationales	<- (tv2013$B16>3119 & tv2013$B16<3179)*1 + (tv2013$B16>3199 & tv2013$B16<3300)*1
tv2013$int.konfl		<- (tv2013$B16>3179 & tv2013$B16<3200)*1
tv2013$int.terror		<- (tv2013$B16==3181)*1			   
tv2013$migration		<- (tv2013$B16>3749 & tv2013$B16<3760)*1
tv2013$verdrossenheit	<- (tv2013$B12>1099 & tv2013$B12<1199)*1 + (tv2013$B14>2009 & tv2013$B12<2199)*1 + (tv2013$B12>2399 & tv2013$B12<2499)*1 + (tv2013$B16>3416 & tv2013$B16<3420)*1
tv2013$sonstiges		<- (tv2013$B12>1600 & tv2013$B12<1999)*1 + (tv2013$B12>2499 & tv2013$B12<2999)*1 + (tv2013$B16>3729 & tv2013$B16<3740)*1 + (tv2013$B16>4000 & tv2013$B16<4099)*1 + (tv2013$B16>4199 & tv2013$B16<4300)*1 + (tv2013$B16>4399 & tv2013$B16<4999)*1  
tv2013$arbeit		<- (tv2013$B16>3799 & tv2013$B16<3840)*1 + (tv2013$B16>3869 & tv2013$B16<3899)*1
tv2013$einkommen		<- (tv2013$B16>3719 & tv2013$B16<3730)*1 + (tv2013$B16>3839 & tv2013$B16<3870)*1
tv2013$familie		<- (tv2013$B16>3709 & tv2013$B16<3720)*1
tv2013$rente			<- (tv2013$B16>3739 & tv2013$B16<3750)*1
tv2013$wohnen		<- (tv2013$B16>3529 & tv2013$B16<3540)*1
tv2013$umwelt		<- (tv2013$B16>3599 & tv2013$B16<3120)*1 + (tv2013$B16>3629 & tv2013$B16<3700)*1
tv2013$klima			<- (tv2013$B16>3619 & tv2013$B16<3630)*1 
tv2013$verteidigung	<- (tv2013$B16>3300 & tv2013$B16<3400)*1
tv2013$wirtschaft		<- (tv2013$B16==3910)*1 + (tv2013$B16==3912)*1 + (tv2013$B16==3913)*1 + (tv2013$B16==3920)*1 + (tv2013$B16==3921)*1 + (tv2013$B16==3924)*1 + (tv2013$B16==3930)*1 + (tv2013$B16>3949 & tv2013$B16<3999)*1 
tv2013$krise			<- (tv2013$B16==3911)*1 + (tv2013$B16>3914 & tv2013$B16<3919)*1 + (tv2013$B16==3922)*1 + (tv2013$B16==3923)*1 + (tv2013$B16>3939 & tv2013$B16<3950)*1     
tv2013$wahl			<- (tv2013$B12>1199 & tv2013$B12<1599)*1 

issue <- c("bildung","energie","EU","foederalismus","gesundheit",
		"haushalt","einnahmen","infrastruktur","inneres","extremismus",
		"geheimdienste","kriminalitaet","internationales","int.konfl","int.terror",
		"migration","verdrossenheit","sonstiges","arbeit","einkommen",
		"familie","rente","wohnen","umwelt","klima",
		"verteidigung","wirtschaft","krise","wahl")

p2013x <- rbind(p2013,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
p2013x$date[2404:2416] <- c("2013-06-23","2013-06-30","2013-07-07","2013-07-14",
					"2013-07-21","2013-07-28","2013-08-04","2013-08-11",
					"2013-08-18","2013-08-25","2013-09-01","2013-09-08",
					"2013-09-15")

p2009x <- rbind(p2009,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
p2009x$date[2324:2336] <- c("2009-06-28","2009-07-05","2009-07-12","2009-07-19",
					"2009-07-26","2009-08-02","2009-08-09","2009-08-16",
					"2009-08-23","2009-08-30","2009-09-06","2009-09-13",
					"2009-09-20")

ma.2013 <- data.frame(datum=NA,tv=NA,print=NA,thema=NA)

for (i in 1:length(issue))
{
tv <- tapply(tv2013[,issue[i]],tv2013$date,sum)
print <- tapply(p2013x[,issue[i]],p2013x$date,sum)
datum <- rownames(tapply(tv2013[,issue[i]],tv2013$date,sum))
thema <- rep(issue[i],times=length(datum))
ma.2013 <- rbind(ma.2013,cbind(datum,tv,print,thema))
}

ma.2009 <- data.frame(datum=NA,tv=NA,print=NA,thema=NA)

for (i in 1:length(issue))
{
tv <- tapply(tv2009[,issue[i]],tv2009$date,sum)
print <- tapply(p2009x[,issue[i]],p2009x$date,sum)
datum <- rownames(tapply(tv2009[,issue[i]],tv2009$date,sum))
thema <- rep(issue[i],times=length(datum))
ma.2009 <- rbind(ma.2009,cbind(datum,tv,print,thema))
}

ma.2009$tv <- as.numeric(replace(ma.2009$tv,is.na(ma.2009$tv),0))
ma.2009$print <- as.numeric(replace(ma.2009$print,is.na(ma.2009$print),0))

ma.2013$tv <- as.numeric(replace(ma.2013$tv,is.na(ma.2013$tv),0))
ma.2013$print <- as.numeric(replace(ma.2013$print,is.na(ma.2013$print),0))

ma.2009$doy <- strptime(ma.2009$datum,format="%Y-%m-%d")$yday
ma.2013$doy <- strptime(ma.2013$datum,format="%Y-%m-%d")$yday

for (i in 1:length(issue))
{
if (sum(ma.2009[(2+91*(i-1)):(1+91*(i)),"print"])>0) {
	ma.2009[(2+91*(i-1)):(1+91*(i)),"prs"] <- StructTS(ma.2009[(2+91*(i-1)):(1+91*(i)),"print"])$fitted[,1]}
if (sum(ma.2009[(2+91*(i-1)):(1+91*(i)),"tv"])>0) {
	ma.2009[(2+91*(i-1)):(1+91*(i)),"tvs"] <- StructTS(ma.2009[(2+91*(i-1)):(1+91*(i)),"tv"])$fitted[,1]}
if (sum(ma.2013[(2+91*(i-1)):(1+91*(i)),"print"])>0) {
	ma.2013[(2+91*(i-1)):(1+91*(i)),"prs"] <- StructTS(ma.2013[(2+91*(i-1)):(1+91*(i)),"print"])$fitted[,1]}
if (sum(ma.2013[(2+91*(i-1)):(1+91*(i)),"tv"])>0) {
	ma.2013[(2+91*(i-1)):(1+91*(i)),"tvs"] <- StructTS(ma.2013[(2+91*(i-1)):(1+91*(i)),"tv"])$fitted[,1]}
}

ma.2009$prs <- replace(ma.2009$prs,ma.2009$prs<0,0)
ma.2009$tvs <- replace(ma.2009$tvs,ma.2009$tvs<0,0)
ma.2013$prs <- replace(ma.2013$prs,ma.2013$prs<0,0)
ma.2013$tvs <- replace(ma.2013$tvs,ma.2013$tvs<0,0)

ma.2009$tvs <- replace(ma.2009$tvs,is.na(ma.2009$tvs),0)
ma.2009$prs <- replace(ma.2009$prs,is.na(ma.2009$prs),0)
ma.2013$tvs <- replace(ma.2013$tvs,is.na(ma.2013$tvs),0)
ma.2013$prs <- replace(ma.2013$prs,is.na(ma.2013$prs),0)

agenda <- cbind(tapply(ma.2009$tv,ma.2009$thema,sum),
tapply(ma.2009$print,ma.2009$thema,sum),
tapply(ma.2013$tv,ma.2013$thema,sum),
tapply(ma.2013$print,ma.2013$thema,sum))

# ------------------------------------------------
# ------------------------------------------------
# SURVEY -----------------------------------------
# ------------------------------------------------
# ------------------------------------------------

rcs2013 <- read.spss("./ZA5703_v2-0-0.sav",to.data.frame=TRUE,use.value.labels=FALSE)
rcs2009 <- read.spss("./ZA5303_v6-0-0_ohneOffeneNennungen.sav",to.data.frame=TRUE,use.value.labels=FALSE)

rcs2013F <- read.spss("./ZA5703_v2-0-0.sav",to.data.frame=TRUE,use.value.labels=TRUE)
rcs2009F <- read.spss("./ZA5303_v6-0-0_ohneOffeneNennungen.sav",to.data.frame=TRUE,use.value.labels=TRUE)

allissues <- c(levels(rcs2009F$pre014c1),levels(rcs2013F$pre014c1)[!(levels(rcs2013F$pre014c1) %in% levels(rcs2009F$pre014c1))])

levels(rcs2009F$pre014c1) %in% allissues
levels(rcs2013F$pre014c1) %in% allissues

iss2009 <- matrix(0,nrow=dim(rcs2009)[1],ncol=length(allissues))
iss2013 <- matrix(0,nrow=dim(rcs2013)[1],ncol=length(allissues))

colnames(iss2009) <- allissues
colnames(iss2013) <- allissues

for (j in 1:dim(iss2009)[2])
{
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre014c1),ifelse(rcs2009F$pre014c1==allissues[j],iss2009[,j]+1,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre014c2),ifelse(rcs2009F$pre014c2==allissues[j],iss2009[,j]+1,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre014c3),ifelse(rcs2009F$pre014c3==allissues[j],iss2009[,j]+1,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre014c4),ifelse(rcs2009F$pre014c4==allissues[j],iss2009[,j]+1,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre014c5),ifelse(rcs2009F$pre014c5==allissues[j],iss2009[,j]+1,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre016c1),ifelse(rcs2009F$pre016c1==allissues[j],iss2009[,j]+0.5,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre016c2),ifelse(rcs2009F$pre016c2==allissues[j],iss2009[,j]+0.5,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre016c3),ifelse(rcs2009F$pre016c3==allissues[j],iss2009[,j]+0.5,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre016c4),ifelse(rcs2009F$pre016c4==allissues[j],iss2009[,j]+0.5,iss2009[,j]),iss2009[,j])
	iss2009[,j] <- ifelse(!is.na(rcs2009F$pre016c5),ifelse(rcs2009F$pre016c5==allissues[j],iss2009[,j]+0.5,iss2009[,j]),iss2009[,j])

	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre014c1),ifelse(rcs2013F$pre014c1==allissues[j],iss2013[,j]+1,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre014c2),ifelse(rcs2013F$pre014c2==allissues[j],iss2013[,j]+1,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre014c3),ifelse(rcs2013F$pre014c3==allissues[j],iss2013[,j]+1,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre014c4),ifelse(rcs2013F$pre014c4==allissues[j],iss2013[,j]+1,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre014c5),ifelse(rcs2013F$pre014c5==allissues[j],iss2013[,j]+1,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre016c1),ifelse(rcs2013F$pre016c1==allissues[j],iss2013[,j]+0.5,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre016c2),ifelse(rcs2013F$pre016c2==allissues[j],iss2013[,j]+0.5,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre016c3),ifelse(rcs2013F$pre016c3==allissues[j],iss2013[,j]+0.5,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre016c4),ifelse(rcs2013F$pre016c4==allissues[j],iss2013[,j]+0.5,iss2013[,j]),iss2013[,j])
	iss2013[,j] <- ifelse(!is.na(rcs2013F$pre016c5),ifelse(rcs2013F$pre016c5==allissues[j],iss2013[,j]+0.5,iss2013[,j]),iss2013[,j])

}

iss2009.1 <- matrix(0,nrow=dim(rcs2009)[1],ncol=length(allissues))
iss2013.1 <- matrix(0,nrow=dim(rcs2013)[1],ncol=length(allissues))

colnames(iss2009.1) <- allissues
colnames(iss2013.1) <- allissues

iss2009 <- iss2009/rowSums(iss2009)
iss2013 <- iss2013/rowSums(iss2013)

for (j in 1:dim(iss2009.1)[2])
{
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos012c1),ifelse(rcs2009F$pos012c1==allissues[j],iss2009.1[,j]+1,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos012c2),ifelse(rcs2009F$pos012c2==allissues[j],iss2009.1[,j]+1,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos012c3),ifelse(rcs2009F$pos012c3==allissues[j],iss2009.1[,j]+1,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos012c4),ifelse(rcs2009F$pos012c4==allissues[j],iss2009.1[,j]+1,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos012c5),ifelse(rcs2009F$pos012c5==allissues[j],iss2009.1[,j]+1,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos014c1),ifelse(rcs2009F$pos014c1==allissues[j],iss2009.1[,j]+0.5,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos014c2),ifelse(rcs2009F$pos014c2==allissues[j],iss2009.1[,j]+0.5,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos014c3),ifelse(rcs2009F$pos014c3==allissues[j],iss2009.1[,j]+0.5,iss2009.1[,j]),iss2009.1[,j])
	iss2009.1[,j] <- ifelse(!is.na(rcs2009F$pos014c4),ifelse(rcs2009F$pos014c4==allissues[j],iss2009.1[,j]+0.5,iss2009.1[,j]),iss2009.1[,j])

	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos011c1),ifelse(rcs2013F$pos011c1==allissues[j],iss2013.1[,j]+1,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos011c2),ifelse(rcs2013F$pos011c2==allissues[j],iss2013.1[,j]+1,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos011c3),ifelse(rcs2013F$pos011c3==allissues[j],iss2013.1[,j]+1,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos011c4),ifelse(rcs2013F$pos011c4==allissues[j],iss2013.1[,j]+1,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos011c5),ifelse(rcs2013F$pos011c5==allissues[j],iss2013.1[,j]+1,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos013c1),ifelse(rcs2013F$pos013c1==allissues[j],iss2013.1[,j]+0.5,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos013c2),ifelse(rcs2013F$pos013c2==allissues[j],iss2013.1[,j]+0.5,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos013c3),ifelse(rcs2013F$pos013c3==allissues[j],iss2013.1[,j]+0.5,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos013c4),ifelse(rcs2013F$pos013c4==allissues[j],iss2013.1[,j]+0.5,iss2013.1[,j]),iss2013.1[,j])
	iss2013.1[,j] <- ifelse(!is.na(rcs2013F$pos013c5),ifelse(rcs2013F$pos013c5==allissues[j],iss2013.1[,j]+0.5,iss2013.1[,j]),iss2013.1[,j])

}

iss2013.1 <- iss2013.1/rowSums(iss2013.1)

Xiss2009 <- data.frame(iss2009)
Xiss2009.1 <- data.frame(iss2009.1)
Xiss2013 <- data.frame(iss2013)
Xiss2013.1 <- data.frame(iss2013.1)

bildung <- c(207,208,209,210,212,214,215,324,325,326,327)
energie <- c(104,105,106,110,111,295,296,297,298)
atom <- c(197,108,109)
EU <- c(267,268,31,62:65)
foederalismus <- c(32,33)
gesundheit <- c(150:161,312:314)
haushalt <- c(217:222,229,336)
ausgaben <- c(226,228,330)
einnahmen <- c(223,224,225,227,233,331)
infrastruktur <- c(101,102,103,115,231,232,291:294,301:302)
inneres <- c(100,281,290,82,92,93,94,95,98)
extremismus <- c(283,85,86,87,88)
geheimdienste <- c(288,289,96,97)
kriminalitaet <- c(282,83,84)
internationales <- c(269:272,277,61,66:71)
int.konfl <- c(273,275,276,72,77,279,74,79,80)
int.terror <- c(274,73)
internet <- c(113,114)
migration <- c(145:149,309:311)
verdrossenheit <- c(10:16,194,2,201,21:29,3,34:39,4,40:49,5,50:55,57:59,6,7,8,89,9,90,91,238:249,253:264,284,322)
sonstiges <- c(1,20,203,211,213,216,230,235:237,252,265,285:287,30,328,329,337:339,60,99)
sozial <- c(124,135:139,162,316,317)
arbeit <- c(163:167,171,176,178:183,319,320)
demographie <- c(234,315)
einkommen <- c(130:132,134,168,169,172:175,177,307,318,56)
familie <- c(125:129,306)
rente <- c(133,140:144,308)
wohnen <- c(112,299,300)
umwelt <- c(116:118,121:123,303:305)
klima <- c(119:120)
verteidigung <- c(278,280,75,76,78,81)
wahl <- c(17,18,19,250,251)
w䨲ung <- c(204,205,332,333)
wirtschaft <- c(170,184,193,197:200,202,206,321,323)
banken <- c(189:192)
krise <- c(185:187,195,196,334,335,188)

x <- colSums(t((!is.na(rcs2009[,c("pre014c1","pre014c2","pre014c3","pre014c4","pre014c5","pre016c1","pre016c2","pre016c3","pre016c4","pre016c5","pre016c6","pre016c7")])))*c(1,1,1,1,1,.5,.5,.5,.5,.5,.5,.5))
mean(x)
sd(x)

colSums(t((!is.na(rcs2009[,c("pre014c1","pre014c2","pre014c3","pre014c4","pre014c5","pre016c1","pre016c2","pre016c3","pre016c4","pre016c5","pre016c6","pre016c7")])))*c(1,1,1,1,1,.5,.5,.5,.5,.5,.5,.5))

Xiss2009$bildung 		<- rowSums(Xiss2009[,bildung])
Xiss2009$energie 		<- rowSums(Xiss2009[,energie])
Xiss2009$atom 		<- rowSums(Xiss2009[,atom])
Xiss2009$EU 			<- rowSums(Xiss2009[,EU])
Xiss2009$foederalismus 	<- rowSums(Xiss2009[,foederalismus])
Xiss2009$gesundheit 	<- rowSums(Xiss2009[,gesundheit])
Xiss2009$haushalt 		<- rowSums(Xiss2009[,haushalt])
Xiss2009$ausgaben 		<- rowSums(Xiss2009[,ausgaben])
Xiss2009$einnahmen 	<- rowSums(Xiss2009[,einnahmen])
Xiss2009$infrastruktur 	<- rowSums(Xiss2009[,infrastruktur])
Xiss2009$inneres 		<- rowSums(Xiss2009[,inneres])
Xiss2009$extremismus 	<- rowSums(Xiss2009[,extremismus])
Xiss2009$geheimdienste 	<- rowSums(Xiss2009[,geheimdienste])
Xiss2009$kriminalitaet 	<- rowSums(Xiss2009[,kriminalitaet])
Xiss2009$internationales <- rowSums(Xiss2009[,internationales])
Xiss2009$int.konfl 	<- rowSums(Xiss2009[,int.konfl])
Xiss2009$int.terror 	<- rowSums(Xiss2009[,int.terror])
Xiss2009$internet 		<- rowSums(Xiss2009[,internet])
Xiss2009$migration 	<- rowSums(Xiss2009[,migration])
Xiss2009$verdrossenheit 	<- rowSums(Xiss2009[,verdrossenheit])
Xiss2009$sonstiges 	<- rowSums(Xiss2009[,sonstiges])
Xiss2009$sozial 		<- rowSums(Xiss2009[,sozial])
Xiss2009$arbeit 		<- rowSums(Xiss2009[,arbeit])
Xiss2009$demographie 	<- rowSums(Xiss2009[,demographie])
Xiss2009$einkommen 	<- rowSums(Xiss2009[,einkommen])
Xiss2009$familie 		<- rowSums(Xiss2009[,familie])
Xiss2009$rente 		<- rowSums(Xiss2009[,rente])
Xiss2009$wohnen 		<- rowSums(Xiss2009[,wohnen])
Xiss2009$umwelt 		<- rowSums(Xiss2009[,umwelt])
Xiss2009$klima 		<- rowSums(Xiss2009[,klima])
Xiss2009$verteidigung 	<- rowSums(Xiss2009[,verteidigung])
Xiss2009$wahl 		<- rowSums(Xiss2009[,wahl])
Xiss2009$w䨲ung 		<- rowSums(Xiss2009[,w䨲ung])
Xiss2009$wirtschaft 	<- rowSums(Xiss2009[,wirtschaft])
Xiss2009$banken 		<- rowSums(Xiss2009[,banken])
Xiss2009$krise 		<- rowSums(Xiss2009[,krise])

Xiss2009.1$bildung 		<- rowSums(Xiss2009.1[,bildung])
Xiss2009.1$energie 		<- rowSums(Xiss2009.1[,energie])
Xiss2009.1$atom 		<- rowSums(Xiss2009.1[,atom])
Xiss2009.1$EU 			<- rowSums(Xiss2009.1[,EU])
Xiss2009.1$foederalismus 	<- rowSums(Xiss2009.1[,foederalismus])
Xiss2009.1$gesundheit 	<- rowSums(Xiss2009.1[,gesundheit])
Xiss2009.1$haushalt 		<- rowSums(Xiss2009.1[,haushalt])
Xiss2009.1$ausgaben 		<- rowSums(Xiss2009.1[,ausgaben])
Xiss2009.1$einnahmen 	<- rowSums(Xiss2009.1[,einnahmen])
Xiss2009.1$infrastruktur 	<- rowSums(Xiss2009.1[,infrastruktur])
Xiss2009.1$inneres 		<- rowSums(Xiss2009.1[,inneres])
Xiss2009.1$extremismus 	<- rowSums(Xiss2009.1[,extremismus])
Xiss2009.1$geheimdienste 	<- rowSums(Xiss2009.1[,geheimdienste])
Xiss2009.1$kriminalitaet 	<- rowSums(Xiss2009.1[,kriminalitaet])
Xiss2009.1$internationales <- rowSums(Xiss2009.1[,internationales])
Xiss2009.1$int.konfl 	<- rowSums(Xiss2009.1[,int.konfl])
Xiss2009.1$int.terror 	<- rowSums(Xiss2009.1[,int.terror])
Xiss2009.1$internet 		<- rowSums(Xiss2009.1[,internet])
Xiss2009.1$migration 	<- rowSums(Xiss2009.1[,migration])
Xiss2009.1$verdrossenheit 	<- rowSums(Xiss2009.1[,verdrossenheit])
Xiss2009.1$sonstiges 	<- rowSums(Xiss2009.1[,sonstiges])
Xiss2009.1$sozial 		<- rowSums(Xiss2009.1[,sozial])
Xiss2009.1$arbeit 		<- rowSums(Xiss2009.1[,arbeit])
Xiss2009.1$demographie 	<- rowSums(Xiss2009.1[,demographie])
Xiss2009.1$einkommen 	<- rowSums(Xiss2009.1[,einkommen])
Xiss2009.1$familie 		<- rowSums(Xiss2009.1[,familie])
Xiss2009.1$rente 		<- rowSums(Xiss2009.1[,rente])
Xiss2009.1$wohnen 		<- rowSums(Xiss2009.1[,wohnen])
Xiss2009.1$umwelt 		<- rowSums(Xiss2009.1[,umwelt])
Xiss2009.1$klima 		<- rowSums(Xiss2009.1[,klima])
Xiss2009.1$verteidigung 	<- rowSums(Xiss2009.1[,verteidigung])
Xiss2009.1$wahl 		<- rowSums(Xiss2009.1[,wahl])
Xiss2009.1$w䨲ung 		<- rowSums(Xiss2009.1[,w䨲ung])
Xiss2009.1$wirtschaft 	<- rowSums(Xiss2009.1[,wirtschaft])
Xiss2009.1$banken 		<- rowSums(Xiss2009.1[,banken])
Xiss2009.1$krise 		<- rowSums(Xiss2009.1[,krise])

Xiss2013$bildung 		<- rowSums(Xiss2013[,bildung])
Xiss2013$energie 		<- rowSums(Xiss2013[,energie])
Xiss2013$atom 		<- rowSums(Xiss2013[,atom])
Xiss2013$EU 			<- rowSums(Xiss2013[,EU])
Xiss2013$foederalismus 	<- rowSums(Xiss2013[,foederalismus])
Xiss2013$gesundheit 	<- rowSums(Xiss2013[,gesundheit])
Xiss2013$haushalt 		<- rowSums(Xiss2013[,haushalt])
Xiss2013$ausgaben 		<- rowSums(Xiss2013[,ausgaben])
Xiss2013$einnahmen 	<- rowSums(Xiss2013[,einnahmen])
Xiss2013$infrastruktur 	<- rowSums(Xiss2013[,infrastruktur])
Xiss2013$inneres 		<- rowSums(Xiss2013[,inneres])
Xiss2013$extremismus 	<- rowSums(Xiss2013[,extremismus])
Xiss2013$geheimdienste 	<- rowSums(Xiss2013[,geheimdienste])
Xiss2013$kriminalitaet 	<- rowSums(Xiss2013[,kriminalitaet])
Xiss2013$internationales <- rowSums(Xiss2013[,internationales])
Xiss2013$int.konfl 	<- rowSums(Xiss2013[,int.konfl])
Xiss2013$int.terror 	<- rowSums(Xiss2013[,int.terror])
Xiss2013$internet 		<- rowSums(Xiss2013[,internet])
Xiss2013$migration 	<- rowSums(Xiss2013[,migration])
Xiss2013$verdrossenheit 	<- rowSums(Xiss2013[,verdrossenheit])
Xiss2013$sonstiges 	<- rowSums(Xiss2013[,sonstiges])
Xiss2013$sozial 		<- rowSums(Xiss2013[,sozial])
Xiss2013$arbeit 		<- rowSums(Xiss2013[,arbeit])
Xiss2013$demographie 	<- rowSums(Xiss2013[,demographie])
Xiss2013$einkommen 	<- rowSums(Xiss2013[,einkommen])
Xiss2013$familie 		<- rowSums(Xiss2013[,familie])
Xiss2013$rente 		<- rowSums(Xiss2013[,rente])
Xiss2013$wohnen 		<- rowSums(Xiss2013[,wohnen])
Xiss2013$umwelt 		<- rowSums(Xiss2013[,umwelt])
Xiss2013$klima 		<- rowSums(Xiss2013[,klima])
Xiss2013$verteidigung 	<- rowSums(Xiss2013[,verteidigung])
Xiss2013$wahl 		<- rowSums(Xiss2013[,wahl])
Xiss2013$w䨲ung 		<- rowSums(Xiss2013[,w䨲ung])
Xiss2013$wirtschaft 	<- rowSums(Xiss2013[,wirtschaft])
Xiss2013$banken 		<- rowSums(Xiss2013[,banken])
Xiss2013$krise 		<- rowSums(Xiss2013[,krise])

Xiss2013.1$bildung 		<- rowSums(Xiss2013.1[,bildung])
Xiss2013.1$energie 		<- rowSums(Xiss2013.1[,energie])
Xiss2013.1$atom 		<- rowSums(Xiss2013.1[,atom])
Xiss2013.1$EU 			<- rowSums(Xiss2013.1[,EU])
Xiss2013.1$foederalismus 	<- rowSums(Xiss2013.1[,foederalismus])
Xiss2013.1$gesundheit 	<- rowSums(Xiss2013.1[,gesundheit])
Xiss2013.1$haushalt 		<- rowSums(Xiss2013.1[,haushalt])
Xiss2013.1$ausgaben 		<- rowSums(Xiss2013.1[,ausgaben])
Xiss2013.1$einnahmen 	<- rowSums(Xiss2013.1[,einnahmen])
Xiss2013.1$infrastruktur 	<- rowSums(Xiss2013.1[,infrastruktur])
Xiss2013.1$inneres 		<- rowSums(Xiss2013.1[,inneres])
Xiss2013.1$extremismus 	<- rowSums(Xiss2013.1[,extremismus])
Xiss2013.1$geheimdienste 	<- rowSums(Xiss2013.1[,geheimdienste])
Xiss2013.1$kriminalitaet 	<- rowSums(Xiss2013.1[,kriminalitaet])
Xiss2013.1$internationales <- rowSums(Xiss2013.1[,internationales])
Xiss2013.1$int.konfl 	<- rowSums(Xiss2013.1[,int.konfl])
Xiss2013.1$int.terror 	<- rowSums(Xiss2013.1[,int.terror])
Xiss2013.1$internet 		<- rowSums(Xiss2013.1[,internet])
Xiss2013.1$migration 	<- rowSums(Xiss2013.1[,migration])
Xiss2013.1$verdrossenheit 	<- rowSums(Xiss2013.1[,verdrossenheit])
Xiss2013.1$sonstiges 	<- rowSums(Xiss2013.1[,sonstiges])
Xiss2013.1$sozial 		<- rowSums(Xiss2013.1[,sozial])
Xiss2013.1$arbeit 		<- rowSums(Xiss2013.1[,arbeit])
Xiss2013.1$demographie 	<- rowSums(Xiss2013.1[,demographie])
Xiss2013.1$einkommen 	<- rowSums(Xiss2013.1[,einkommen])
Xiss2013.1$familie 		<- rowSums(Xiss2013.1[,familie])
Xiss2013.1$rente 		<- rowSums(Xiss2013.1[,rente])
Xiss2013.1$wohnen 		<- rowSums(Xiss2013.1[,wohnen])
Xiss2013.1$umwelt 		<- rowSums(Xiss2013.1[,umwelt])
Xiss2013.1$klima 		<- rowSums(Xiss2013.1[,klima])
Xiss2013.1$verteidigung 	<- rowSums(Xiss2013.1[,verteidigung])
Xiss2013.1$wahl 		<- rowSums(Xiss2013.1[,wahl])
Xiss2013.1$w䨲ung 		<- rowSums(Xiss2013.1[,w䨲ung])
Xiss2013.1$wirtschaft 	<- rowSums(Xiss2013.1[,wirtschaft])
Xiss2013.1$banken 		<- rowSums(Xiss2013.1[,banken])
Xiss2013.1$krise 		<- rowSums(Xiss2013.1[,krise])

year <- trunc(rcs2009$pre_datum/10000)+2000
mon <- trunc((rcs2009$pre_datum-90000)/100)
day <- rcs2009$pre_datum-90000-mon*100
datum <- as.Date(paste0(day,".",mon,".",year),format="%d.%m.%Y")
rcstag <- rcs2009$pre_tag
df.iss2009 <- data.frame(ID=rcs2009$lfdn,date=datum,rcsday=rcstag,Xiss2009[,340:374],Xiss2009.1[,340:374])

datum <- as.Date(rcs2013$pre_datum/86400,origin="1582-10-15")
rcstag <- rcs2013$pre_tag
df.iss2013 <- data.frame(ID=rcs2013$lfdn,date=datum,rcsday=rcstag,Xiss2013[,340:374],Xiss2013.1[,340:374])

df.iss2009 <- data.frame(iss2009,ID=rcs2009$lfdn)
df.iss2013 <- data.frame(iss2013,ID=rcs2013$lfdn)

gg.iss <- data.frame(pub=c(colSums(cc.iss2009[,4:38]),
	colSums(cc.iss2009[39:73]),
	colSums(cc.iss2013[,4:38]),
	colSums(cc.iss2013[39:73])),
		issue=rep(names(cc.iss2009)[4:38],times=4),
		year=c(rep(2009,times=70),rep(2013,times=70)),
		wave=c(rep(1,times=35),rep(2,times=35),
			rep(1,times=35),rep(2,times=35)) )

gg.iss$YEAR <- factor(gg.iss$year)
gg.iss$WAVE <- factor(gg.iss$wave)


ggi.rcs <- data.frame(doy=rep(NA,times=2100),day=rep(NA,times=2100),issue=rep(NA,times=2100),
			attention=rep(NA,times=2100))

ggi.rcs.13 <- data.frame(doy=rep(NA,times=2660),day=rep(NA,times=2660),issue=rep(NA,times=2660),
			attention=rep(NA,times=2660))

cc.iss2009$doy <- strptime(cc.iss2009$date,"%Y-%m-%d")$yday
cc.iss2013$doy <- strptime(cc.iss2013$date,"%Y-%m-%d")$yday


wt <- cc.iss2009$wt

x <- aggregate(cc.iss2009[,4:38],by=list(cc.iss2009$rcsday),FUN=wtd.mean,weights=cc.iss2009$wt,normwt=T)
y <- x[,2:36]/rowSums(x[,2:36])

xx <- aggregate(cc.iss2013[,4:38],by=list(cc.iss2013$rcsday),FUN="mean")
yy <- xx[,2:36]/rowSums(xx[,2:36])


for (i in 4:38)
{
i.rcs <- data.frame(doy=rep(NA,times=60),day=rep(NA,times=60),issue=rep(NA,times=60),attention=rep(NA,times=60))
i.rcs[,"day"] <- rownames(tapply(cc.iss2009[,i],cc.iss2009$rcsday,mean))
i.rcs[,"attention"] <- tapply(cc.iss2009[,i],cc.iss2009$rcsday,mean)
i.rcs[,"issue"] <- colnames(cc.iss2009)[i]
i.rcs[,"doy"] <- as.numeric(rownames(tapply(cc.iss2009[,i],cc.iss2009$doy,mean)))
ggi.rcs <- rbind(ggi.rcs,i.rcs)
}

for (i in 4:38)
{
i.rcs <- data.frame(doy=rep(NA,times=76),day=rep(NA,times=76),issue=rep(NA,times=76),attention=rep(NA,times=76))
i.rcs[,"day"] <- rownames(tapply(cc.iss2013[,i],cc.iss2013$rcsday,mean))
i.rcs[,"attention"] <- tapply(cc.iss2013[,i],cc.iss2013$rcsday,mean)
i.rcs[,"issue"] <- colnames(cc.iss2013)[i]
i.rcs[,"doy"] <- as.numeric(rownames(tapply(cc.iss2013[,i],cc.iss2013$doy,mean)))
ggi.rcs.13 <- rbind(ggi.rcs.13,i.rcs)
}

for (i in 4:38)
{
i.rcs <- data.frame(doy=rep(NA,times=62),day=rep(NA,times=62),issue=rep(NA,times=62),attention=rep(NA,times=62))
i.rcs[,"day"] <- rownames(tapply(cc.iss2017[,i],cc.iss2017$rcsday,mean))
i.rcs[,"attention"] <- tapply(cc.iss2017[,i],cc.iss2017$rcsday,mean)
i.rcs[,"issue"] <- colnames(cc.iss2017)[i]
i.rcs[,"doy"] <- as.numeric(rownames(tapply(cc.iss2017[,i],cc.iss2017$rcsday,mean)))
ggi.rcs.17 <- rbind(ggi.rcs.17,i.rcs)
}

ggi.rcs <- ggi.rcs[2101:4200,]

ggi.rcs.13 <- ggi.rcs.13[2661:5320,]

ggi.rcs.17 <- ggi.rcs.17[2171:4340,]
ggi.rcs.17$day <- ggi.rcs.17$doy-204

### Create SELECTOR for only those day--issue combinations for which there is both content analysis and public opinion data

ma.2009.index <- which(paste(ma.2009$thema,ma.2009$doy)%in%paste(ggi.rcs$issue,ggi.rcs$doy))
gg.2009.index <- which(paste(ggi.rcs$issue,ggi.rcs$doy)%in%paste(ma.2009$thema,ma.2009$doy))

ma.2013.index <- which(paste(ma.2013$thema,ma.2013$doy)%in%paste(ggi.rcs.13$issue,ggi.rcs.13$doy))
gg.2013.index <- which(paste(ggi.rcs.13$issue,ggi.rcs.13$doy)%in%paste(ma.2013$thema,ma.2013$doy))

ma.2017.index <- which(paste(ma.2017$thema,ma.2017$doy)%in%paste(ggi.rcs.17$issue,ggi.rcs.17$doy))
gg.2017.index <- which(paste(ggi.rcs.17$issue,ggi.rcs.17$doy)%in%paste(ma.2017$thema,ma.2017$doy))

dat.2009 <- data.frame(	doy=ggi.rcs$doy[gg.2009.index],
				public=ggi.rcs$attention[gg.2009.index],
				thema=ggi.rcs$issue[gg.2009.index],
				tvr=ma.2009$tv[ma.2009.index],
				prr=ma.2009$print[ma.2009.index])

dat.2013 <- data.frame(	doy=ggi.rcs.13$doy[gg.2013.index],
				public=ggi.rcs.13$attention[gg.2013.index],
				thema=ggi.rcs.13$issue[gg.2013.index],
				tvr=as.numeric(ma.2013$tv[ma.2013.index]),
				prr=as.numeric(ma.2013$print[ma.2013.index]))

dat.2017 <- data.frame(	doy=ggi.rcs.17$doy[gg.2017.index],
				public=ggi.rcs.17$attention[gg.2017.index],
				thema=ggi.rcs.17$issue[gg.2017.index],
				tvr=ma.2017$tv[ma.2017.index],
				prr=ma.2017$print[ma.2017.index])

##### PREPARE TIME SERIES FOR ANALYSIS


#
#
#
#
# Kalman Filterung

for (i in 1:29)
{
x <- subset(dat.2009,thema==issue[i])$public
y <- StructTS(x,type="BSM")$fit[,1]
y <- rollmean(x,k=7,align=center)
}

dat.2009$thema <- relevel(dat.2009$thema,ref="kriminalitaet")

#
#
#
#
# Auto-ARIMA Cleaning

data.2009 <- dat.2009

data.2009 <- subset(dat.2009,!is.na(public))

data.2009$tv <- replace(data.2009$tv,is.na(data.2009$tv),0)

data.2013 <- dat.2013

data.2013 <- subset(dat.2013,!is.na(public))

data.2013$tv <- replace(data.2013$tv,is.na(data.2013$tv),0)

for (i in 1:length(issue))
{
if (issue[i] %in% dat.2009$thema){
	data.2009[data.2009$thema==issue[i],"public"] <- na.approx(data.2009[data.2009$thema==issue[i],"public"])
	data.2009[data.2009$thema==issue[i],"public"] <- na.approx(Arima(data.2009[data.2009$thema==issue[i],"public"],order=c(0,1,5))$res,rule=2)
	}
}


for (i in 1:length(issue))
{
if (issue[i] %in% dat.2009$thema){
	data.2009[data.2009$thema==issue[i],"public"] <- na.approx(data.2009[data.2009$thema==issue[i],"public"])
	data.2009[data.2009$thema==issue[i],"public"] <- na.approx(auto.arima(data.2009[data.2009$thema==issue[i],"public"],start.q=5,stepwise=F,approximation=F)$res,rule=2)
	}
}

for (i in 1:length(issue))
{
if (issue[i] %in% dat.2009$thema){
	data.2009[data.2009$thema==issue[i],"pr"] <- na.approx(data.2009[data.2009$thema==issue[i],"pr"])
	data.2009[data.2009$thema==issue[i],"pr"] <- na.approx(auto.arima(data.2009[data.2009$thema==issue[i],"pr"])$res,rule=2)
	}
}

for (i in 1:length(issue))
{
if (issue[i] %in% dat.2009$thema){
	data.2009[data.2009$thema==issue[i],"tv"] <- na.approx(data.2009[data.2009$thema==issue[i],"tv"])
	data.2009[data.2009$thema==issue[i],"tv"] <- na.approx(auto.arima(data.2009[data.2009$thema==issue[i],"tv"])$res,rule=2)
	}
}


for (i in 1:length(issue))
{
if (issue[i] %in% dat.2013$thema){
	data.2013[data.2013$thema==issue[i],"public"] <- na.approx(data.2013[data.2013$thema==issue[i],"public"])
	data.2013[data.2013$thema==issue[i],"public"] <- na.approx(auto.arima(data.2013[data.2013$thema==issue[i],"public"])$res,rule=2)
	}
}

for (i in 1:length(issue))
{
if (issue[i] %in% dat.2013$thema){
	data.2013[data.2013$thema==issue[i],"pr"] <- na.approx(data.2013[data.2013$thema==issue[i],"pr"])
	data.2013[data.2013$thema==issue[i],"pr"] <- na.approx(auto.arima(data.2013[data.2013$thema==issue[i],"pr"])$res,rule=2)
	}
}

for (i in 1:length(issue))
{
if (issue[i] %in% dat.2013$thema){
	data.2013[data.2013$thema==issue[i],"tv"] <- na.approx(data.2013[data.2013$thema==issue[i],"tv"])
	data.2013[data.2013$thema==issue[i],"tv"] <- na.approx(auto.arima(data.2013[data.2013$thema==issue[i],"tv"])$res,rule=2)
	}
}






#########################################

#########################################

##### ANALYZE THE AGENDA STRUCTURE

tvag <- tapply(dat.2009$tv,dat.2009$thema,sum,na.rm=T)

prag <- tapply(dat.2009$pr,dat.2009$thema,sum,na.rm=T)







#########################################

#########################################

##### DEFINES THE FUNCTION FOR EXTRACTING THE ATTENTION DYNAMICS DESCRIPTORS


eventextract <- function(ts,threshold){
	# threshold is the absolute change rate required for identifying a substantial change
	changerate <- mean(abs(diff(ts)))					#day-by-day volatility
	baseline <- mean(ts)							#articles per day
	rel.change <- abs(diff(ts))/ifelse(ts[1:(length(ts)-1)]>0,ts[1:(length(ts)-1)],baseline)
	rel.changerate <- mean(rel.change,na.rm=T)			#relative volatility
	changerate.min <- ifelse(changerate<threshold,threshold,changerate)
	unbase <- ts-baseline
	unbase <- replace(unbase,unbase<0,0)
	pes <- rep(NA,times=length(ts)) #potential events' current size
	pfes <- rep(NA,times=length(ts)) #potential events' total size
	pmeh <- rep(NA,times=length(ts)) #potential events' maximum height
	eventlog <- rep(NA,times=length(ts))
	for (i in 1:length(ts))
		{
		pes[i] <- ifelse(unbase[i]==0,0,unbase[i]+ifelse(i>1,pes[i-1],0))
		}
	for (i in length(ts):1)
		{
		pfes[i] <- ifelse(pes[i]==0 | i==length(ts),pes[i],max(pfes[i+1],pes[i]))
		}
	peakts 	<- rep(NA,times=length(ts))
	volumets 	<- rep(NA,times=length(ts))
	peakts	<- pes>	changerate.min
	volumets	<- pfes>	3*changerate.min
	volts <- as.numeric(volumets)
	eventlog[1] <- as.numeric(volts[1]>0)
	for (i in 2:length(ts))
		{	
		eventlog[i] <- ifelse(volts[i]==volts[i-1] | volts[i]==0,eventlog[i-1],eventlog[i-1]+1)
		}
	x <- NULL
	wavecount <- max(eventlog,na.rm=T)					#event frequency
	wavecount <- ifelse(is.finite(wavecount),wavecount,0)
	if (wavecount>0) {
	for (i in 1:wavecount)
		{
		x <- append(x,sum(rel.change[eventlog==i],na.rm=T)/sum(eventlog==1,na.rm=T))
		}
	}
	eventlog <- replace(eventlog,volts==0,NA)	
	eventspan <- max(which(eventlog==max(eventlog,na.rm=T)))-min(which(eventlog==min(eventlog,na.rm=T)))	 # eventspan
	wavevolume <- sum(unbase*volts,na.rm=T)/sum(ts,na.rm=T)	#event-centeredness
	waveduration <- sum(volts)/wavecount				#duration
	waveheight.max <- ifelse(wavecount>0,rep(NA,times=wavecount),NA)				#maximum intensity
	wavevolume.mean <- sum(unbase*volts,na.rm=T)/wavecount	#event volume
	waveheight.mean <- sum(unbase*volts,na.rm=T)/sum(volts)	#mean event intensity
	relvol.events <- mean(x)
	baseline.adj <- ifelse(wavecount>0,(sum(ts)-(wavevolume.mean*wavecount))/length(ts),baseline)
	if (wavecount>0 & is.finite(wavecount)) {
	for (i in 1:wavecount)
		{
		waveheight.max[i] <- max(unbase[eventlog==i],na.rm=T)
		}
	}
	return(list(timeseries=round(ts,3),waveheight=round(pes,3),wavesize=round(pfes,3),wave=volts,number=eventlog,
			baseline=baseline,changerate=changerate,relative.volatility=rel.changerate,
			eventspan=eventspan,event.frequency=wavecount,event.centeredness=wavevolume,
			duration=waveduration,maximum.waveheight=waveheight.max,mean.wavevolume=wavevolume.mean,
			mean.event.intensity=waveheight.mean,relative.volatility.in.events=relvol.events,
			adjusted.baseline=baseline.adj))
}


eventextract(issplot[,1],0.05)[c("mean.wavevolume","baseline","event.frequency","relative.volatility.in.events")]

issplot.pr <- replace(issplot.pr,is.na(issplot.pr),0)
issplot.tv <- replace(issplot.tv,is.na(issplot.tv),0)
issplot <- replace(issplot,is.na(issplot),0)

issue.dimensions <- rbind(as.numeric(eventextract(issplot.tv[,1]+issplot.pr[,1],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,2]+issplot.pr[,2],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,3]+issplot.pr[,3],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,4]+issplot.pr[,4],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,5]+issplot.pr[,5],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,6]+issplot.pr[,6],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,7]+issplot.pr[,7],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,8]+issplot.pr[,8],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,9]+issplot.pr[,9],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,10]+issplot.pr[,10],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,11]+issplot.pr[,11],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,12]+issplot.pr[,12],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,13]+issplot.pr[,13],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,14]+issplot.pr[,14],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,15]+issplot.pr[,15],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,16]+issplot.pr[,16],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,17]+issplot.pr[,17],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,18]+issplot.pr[,18],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,19]+issplot.pr[,19],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,20]+issplot.pr[,20],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,21]+issplot.pr[,21],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,22]+issplot.pr[,22],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,23]+issplot.pr[,23],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,24]+issplot.pr[,24],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,25]+issplot.pr[,25],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,26]+issplot.pr[,26],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,27]+issplot.pr[,27],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv[,28]+issplot.pr[,28],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]))

colnames(issue.dimensions) <- c("spike.momentum","issue.baseline","spike.frequency","spike.oscillation","ib")
rownames(issue.dimensions) <- names(table(dat.2009$thema))
issue.dimensions$issue <- names(table(dat.2009$thema))
issue.dimensions[,"spike.momentum"] <- replace(issue.dimensions[,"spike.momentum"],!is.finite(issue.dimensions[,"spike.momentum"]),0)
issue.dimensions[,"issue.baseline"] <- replace(issue.dimensions[,"issue.baseline"],!is.finite(issue.dimensions[,"issue.baseline"]),0)

issue.dimensions <- data.frame(issue.dimensions)

issue.dimensions$ase <- c(4,0,2,0,0,1,3,4,NA,0,1.5,1.5,
				2,5,13,NA,0,0,12,2,3,1,0,1,6.5,
				NA,3,0)*3/2


matrix(tapply(dat.2013$public,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.13
matrix(tapply(dat.2013$tvr,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.tv.13
matrix(tapply(dat.2013$prr,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.pr.13
matrix(tapply(dat.2013$tv,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.tv.13
matrix(tapply(dat.2013$pr,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.pr.13

issplot.pr.13 <- replace(issplot.pr.13,is.na(issplot.pr.13),0)
issplot.tv.13 <- replace(issplot.tv.13,is.na(issplot.tv.13),0)
issplot.13 <- replace(issplot.13,is.na(issplot.13),0)

issue.dimensions.2013 <- rbind(as.numeric(eventextract(issplot.tv.13[,1]+issplot.pr.13[,1],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,2]+issplot.pr.13[,2],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,3]+issplot.pr.13[,3],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,4]+issplot.pr.13[,4],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,5]+issplot.pr.13[,5],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,6]+issplot.pr.13[,6],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,7]+issplot.pr.13[,7],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,8]+issplot.pr.13[,8],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,9]+issplot.pr.13[,9],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,10]+issplot.pr.13[,10],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,11]+issplot.pr.13[,11],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,12]+issplot.pr.13[,12],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,13]+issplot.pr.13[,13],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,14]+issplot.pr.13[,14],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,15]+issplot.pr.13[,15],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,16]+issplot.pr.13[,16],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,17]+issplot.pr.13[,17],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,18]+issplot.pr.13[,18],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,19]+issplot.pr.13[,19],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,20]+issplot.pr.13[,20],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,21]+issplot.pr.13[,21],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,22]+issplot.pr.13[,22],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,23]+issplot.pr.13[,23],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,24]+issplot.pr.13[,24],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,25]+issplot.pr.13[,25],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,26]+issplot.pr.13[,26],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,27]+issplot.pr.13[,27],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]),
as.numeric(eventextract(issplot.tv.13[,28]+issplot.pr.13[,28],1)[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]))

issue.dimensions.2013 <- data.frame(issue.dimensions.2013)

colnames(issue.dimensions.2013) <- c("spike.momentum","issue.baseline","spike.frequency","spike.oscillation","ib")
rownames(issue.dimensions.2013) <- names(table(dat.2013$thema))
issue.dimensions.2013$issue <- names(table(dat.2013$thema))
issue.dimensions.2013[,"spike.frequency"] <- replace(issue.dimensions.2013[,"spike.frequency"],!is.finite(issue.dimensions.2013[,"spike.frequency"]),0)
issue.dimensions.2013[,"spike.momentum"] <- replace(issue.dimensions.2013[,"spike.momentum"],!is.finite(issue.dimensions.2013[,"spike.momentum"]),0)
issue.dimensions.2013[,"issue.baseline"] <- replace(issue.dimensions.2013[,"issue.baseline"],!is.finite(issue.dimensions.2013[,"issue.baseline"]),0)

issue.dimensions.2013[,"ase"] <- 
				c(0,3,1,2,4.5,1,10,3,NA,10.5,
				1,5.5,2.5,1,4,NA,0.5,0,3,0,
				7.5,0.5,2,4,NA,9.5,1,0)

issdim <- rbind(issue.dimensions,issue.dimensions.2013)
issdim$year <- c(rep("2009",times=28),rep("2013",times=28))
issdim$iyear <- interaction(issdim$year,issdim$issue)

issdim$spike.to.baseline <- issdim$spike.momentum/issdim$issue.baseline

issdim$spike.to.baseline <- replace(issdim$spike.to.baseline,is.nan(issdim$spike.to.baseline),0)


#########################################


#########################################

#########################################

##### GET THE PUBLIC ATTENTION DYNAMICS DESCRIPTORS


matrix(tapply(dat.2009$public,interaction(dat.2009$doy,dat.2009$thema),sum),nrow=60) -> issplot
matrix(tapply(dat.2009$tvr,interaction(dat.2009$doy,dat.2009$thema),sum),nrow=60) -> issplot.tv
matrix(tapply(dat.2009$prr,interaction(dat.2009$doy,dat.2009$thema),sum),nrow=60) -> issplot.pr
matrix(tapply(dat.2009$tv,interaction(dat.2009$doy,dat.2009$thema),sum),nrow=60) -> issplot.tv
matrix(tapply(dat.2009$pr,interaction(dat.2009$doy,dat.2009$thema),sum),nrow=60) -> issplot.pr

publicdyn <- list(NULL)

for (i in 1:28)
{
publicdyn[[i]] <- eventextract(issplot[,i],.05)
}

names(publicdyn) <- levels(dat.2009$thema)

matrix(tapply(dat.2013$public,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot13
matrix(tapply(dat.2013$tvr,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.tv13
matrix(tapply(dat.2013$prr,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.pr13
matrix(tapply(dat.2013$tv,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.tv13
matrix(tapply(dat.2013$pr,interaction(dat.2013$doy,dat.2013$thema),sum),nrow=75) -> issplot.pr13

publicdyn13 <- list(NULL)

for (i in 1:28)
{
publicdyn13[[i]] <- eventextract(issplot13[,i],.05)
}

names(publicdyn13) <- levels(dat.2013$thema)




#########################################

#########################################

##### VAR, IRG, Granger Causality = Time Series Analysis

l.thema <- list.thema[c(-9,-16,-18,-23,-26,-28)]

var.np.09 <- list(arbeit==NULL)
var.np.13 <- list(arbeit==NULL)
var.tv.09 <- list(arbeit==NULL)
var.tv.13 <- list(arbeit==NULL)

irf.np.09 <- list(arbeit==NULL)
irf.np.13 <- list(arbeit==NULL)
irf.tv.09 <- list(arbeit==NULL)
irf.tv.13 <- list(arbeit==NULL)

irf.rnp.09 <- list(arbeit==NULL)
irf.rnp.13 <- list(arbeit==NULL)
irf.rtv.09 <- list(arbeit==NULL)
irf.rtv.13 <- list(arbeit==NULL)

grc.np.09 <- list(arbeit==NULL)
grc.np.13 <- list(arbeit==NULL)
grc.rnp.09 <- list(arbeit==NULL)
grc.rnp.13 <- list(arbeit==NULL)
grc.tv.09 <- list(arbeit==NULL)
grc.tv.13 <- list(arbeit==NULL)
grc.rtv.09 <- list(arbeit==NULL)
grc.rtv.13 <- list(arbeit==NULL)

for (i in 1:length(l.thema))
{
var.np.09[[i]] <- VAR(y=subset(data.2009,thema==l.thema[i])[,c("public","pr")],p=14)
var.np.13[[i]] <- VAR(y=subset(data.2013,thema==l.thema[i])[,c("public","pr")],p=14)
var.tv.09[[i]] <- VAR(y=subset(data.2009,thema==l.thema[i])[,c("public","tv")],p=14)
var.tv.13[[i]] <- VAR(y=subset(data.2013,thema==l.thema[i])[,c("public","tv")],p=14)
}

for (i in 1:length(l.thema))
{
irf.np.09[[i]] <- irf(var.np.09[[i]],n.ahead=14,impulse="pr",response="public",cumulative=T,boot=T,runs=10,ortho=T)
irf.np.13[[i]] <- irf(var.np.13[[i]],n.ahead=14,impulse="pr",response="public",cumulative=T,boot=T,runs=10,ortho=T)
irf.tv.09[[i]] <- irf(var.tv.09[[i]],n.ahead=14,impulse="tv",response="public",cumulative=T,boot=T,runs=10,ortho=T)
irf.tv.13[[i]] <- irf(var.tv.13[[i]],n.ahead=14,impulse="tv",response="public",cumulative=T,boot=T,runs=10,ortho=T)
irf.rnp.09[[i]] <- irf(var.np.09[[i]],n.ahead=14,impulse="public",response="pr",cumulative=T,boot=T,runs=10,ortho=T)
irf.rnp.13[[i]] <- irf(var.np.13[[i]],n.ahead=14,impulse="public",response="pr",cumulative=T,boot=T,runs=10,ortho=T)
irf.rtv.09[[i]] <- irf(var.tv.09[[i]],n.ahead=14,impulse="public",response="tv",cumulative=T,boot=T,runs=10,ortho=T)
irf.rtv.13[[i]] <- irf(var.tv.13[[i]],n.ahead=14,impulse="public",response="tv",cumulative=T,boot=T,runs=10,ortho=T)
}

for (i in 1:length(l.thema))
{
grc.np.09[[i]] <- causality(var.np.09[[i]],cause="pr",boot=F)
grc.np.13[[i]] <- causality(var.np.13[[i]],cause="pr",boot=F)
grc.tv.09[[i]] <- causality(var.tv.09[[i]],cause="tv",boot=F)
grc.tv.13[[i]] <- causality(var.tv.13[[i]],cause="tv",boot=F)
grc.rnp.09[[i]] <- causality(var.np.09[[i]],cause="public",boot=F)
grc.rnp.13[[i]] <- causality(var.np.13[[i]],cause="public",boot=F)
grc.rtv.09[[i]] <- causality(var.tv.09[[i]],cause="public",boot=F)
grc.rtv.13[[i]] <- causality(var.tv.13[[i]],cause="public",boot=F)
}

en.thema <- c(	"Labor","Education","Income","Tax","Energy",
			"EU","Extremism","Family","Intelligence Service","Health",
			"Budget","Infrastructure","Domestic Security","Intl. Conflict","International Affairs",
			"Crime","Economic Crisis","Migration","Pension","Disenchantment with Politics",
			"Defense","Economy")

df.irf <- data.frame(est=rep(NA,times=88*15),lo=rep(NA,times=88*15),hi=rep(NA,times=88*15),
				lag=rep(NA,times=88*15),
				thema=rep(en.thema,each=60),
				year=rep(rep(c("2009","2013"),each=30),times=22),
				medium=rep(rep(c("Newspaper","TV","Newspaper","TV"),each=15),times=22))

for (i in 1:22)
{
df.irf$est[((i-1)*60+1):(60*(i-1)+15)] <- irf.np.09[[i]][1]$irf$pr
df.irf$est[((i-1)*60+16):(60*(i-1)+30)] <- irf.tv.09[[i]][1]$irf$tv
df.irf$est[((i-1)*60+31):(60*(i-1)+45)] <- irf.np.13[[i]][1]$irf$pr
df.irf$est[((i-1)*60+46):(60*(i-1)+60)] <- irf.tv.13[[i]][1]$irf$tv
df.irf$lo[((i-1)*60+1):(60*(i-1)+15)] <- irf.np.09[[i]][2]$Lower$pr
df.irf$lo[((i-1)*60+16):(60*(i-1)+30)] <- irf.tv.09[[i]][2]$Lower$tv
df.irf$lo[((i-1)*60+31):(60*(i-1)+45)] <- irf.np.13[[i]][2]$Lower$pr
df.irf$lo[((i-1)*60+46):(60*(i-1)+60)] <- irf.tv.13[[i]][2]$Lower$tv
df.irf$hi[((i-1)*60+1):(60*(i-1)+15)] <- irf.np.09[[i]][3]$Upper$pr
df.irf$hi[((i-1)*60+16):(60*(i-1)+30)] <- irf.tv.09[[i]][3]$Upper$tv
df.irf$hi[((i-1)*60+31):(60*(i-1)+45)] <- irf.np.13[[i]][3]$Upper$pr
df.irf$hi[((i-1)*60+46):(60*(i-1)+60)] <- irf.tv.13[[i]][3]$Upper$tv
df.irf$lag[((i-1)*60+1):(60*(i-1)+60)] <- c(1:15,1:15,1:15,1:15)
}

df.irf$lo <- ifelse(df.irf$est<df.irf$lo,df.irf$est,df.irf$lo)
df.irf$hi <- ifelse(df.irf$est>df.irf$hi,df.irf$est,df.irf$hi)

df.irfr <- data.frame(est=rep(NA,times=88*15),lo=rep(NA,times=88*15),hi=rep(NA,times=88*15),
				lag=rep(NA,times=88*15),
				thema=rep(en.thema,each=60),
				year=rep(rep(c("2009","2013"),each=30),times=22),
				medium=rep(rep(c("Newspaper","TV","Newspaper","TV"),each=15),times=22))

for (i in 1:22)
{
df.irfr$est[((i-1)*60+1):(60*(i-1)+15)] <- irf.rnp.09[[i]][1]$irf$public
df.irfr$est[((i-1)*60+16):(60*(i-1)+30)] <- irf.rtv.09[[i]][1]$irf$public
df.irfr$est[((i-1)*60+31):(60*(i-1)+45)] <- irf.rnp.13[[i]][1]$irf$public
df.irfr$est[((i-1)*60+46):(60*(i-1)+60)] <- irf.rtv.13[[i]][1]$irf$public
df.irfr$lo[((i-1)*60+1):(60*(i-1)+15)] <- irf.rnp.09[[i]][2]$Lower$public
df.irfr$lo[((i-1)*60+16):(60*(i-1)+30)] <- irf.rtv.09[[i]][2]$Lower$public
df.irfr$lo[((i-1)*60+31):(60*(i-1)+45)] <- irf.rnp.13[[i]][2]$Lower$public
df.irfr$lo[((i-1)*60+46):(60*(i-1)+60)] <- irf.rtv.13[[i]][2]$Lower$public
df.irfr$hi[((i-1)*60+1):(60*(i-1)+15)] <- irf.rnp.09[[i]][3]$Upper$public
df.irfr$hi[((i-1)*60+16):(60*(i-1)+30)] <- irf.rtv.09[[i]][3]$Upper$public
df.irfr$hi[((i-1)*60+31):(60*(i-1)+45)] <- irf.rnp.13[[i]][3]$Upper$public
df.irfr$hi[((i-1)*60+46):(60*(i-1)+60)] <- irf.rtv.13[[i]][3]$Upper$public
df.irfr$lag[((i-1)*60+1):(60*(i-1)+60)] <- c(1:15,1:15,1:15,1:15)
}

themli.long <-  c("Labor","Education","Income","Tax","Energy",
		"EU","Extremism","Family","Intelligence Service","Health",
	      "Budget","Infrastructure","Domestic Security","International Conflict",
	      "International Affairs","Crime","Economic Crisis","Migration",
		"Pension","Disenchantment with Politics","Defense","Economy")
themli <-  c("EU","Family","Health",
	      "Intl. Conflict","Economic Crisis",
		"Economy")  

themli <-  c("Intl. Conflict")  

gg.irf <- ggplot(subset(df.irf,thema%in%themli),aes(linetype=medium,x=lag,y=est,ymin=lo,ymax=hi,color=medium))+geom_smooth()+
		geom_hline(yintercept=0)+facet_grid(thema~year)+theme_bw()+theme(legend.position="bottom")+
		xlab("Days after News Story (=Impulse)")+ylab("Public Agenda Change (=Response)")+
		guides(color=guide_legend("News stories' impulse of public agenda"),
			linetype=guide_legend("News stories' impulse of public agenda"))+
		ggtitle("Cumulative impulse-response functions")

df.kf09 <- data.frame(
		attention=c( subset(data.2009,thema==l.thema[14])[,c("public")]/max(subset(data.2009,thema==l.thema[14])[,c("public")]),
			subset(data.2009,thema==l.thema[14])[,c("pr")]/max(subset(data.2009,thema==l.thema[14])[,c("pr")]),
			subset(data.2009,thema==l.thema[14])[,c("tv")]/max(subset(data.2009,thema==l.thema[14])[,c("tv")])),
		doy= rep(subset(data.2009,thema==l.thema[14])[,c("doy")],times=3),
		agenda=rep(c("(1) Public","(3) Newspaper","(2) TV"),each=60)
		)

df.kf13 <- data.frame(
		attention=c( subset(data.2013,thema==l.thema[14])[,c("public")]/max(subset(data.2013,thema==l.thema[14])[,c("public")]),
			subset(data.2013,thema==l.thema[14])[,c("pr")]/max(subset(data.2013,thema==l.thema[14])[,c("pr")]),
			subset(data.2013,thema==l.thema[14])[,c("tv")]/max(subset(data.2013,thema==l.thema[14])[,c("tv")])),
		doy= rep(subset(data.2013,thema==l.thema[14])[,c("doy")],times=3),
		agenda=rep(c("(1) Public","(3) Newspaper","(2) TV"),each=75)
		)

df.kf09 <- data.frame(
		raw=c( subset(dat.2009,thema==l.thema[14])[,c("public")]/max(subset(dat.2009,thema==l.thema[14])[,c("public")]),
			subset(dat.2009,thema==l.thema[14])[,c("prr")]/max(subset(dat.2009,thema==l.thema[14])[,c("prr")]),
			subset(dat.2009,thema==l.thema[14])[,c("tvr")]/max(subset(dat.2009,thema==l.thema[14])[,c("tvr")])),
		attention=c( StructTS(subset(dat.2009,thema==l.thema[14])[,c("public")]/max(subset(dat.2009,thema==l.thema[14])[,c("public")]))$fit[,1],
			subset(dat.2009,thema==l.thema[14])[,c("pr")]/max(subset(dat.2009,thema==l.thema[14])[,c("pr")]),
			subset(dat.2009,thema==l.thema[14])[,c("tv")]/max(subset(dat.2009,thema==l.thema[14])[,c("tv")])),
		doy= rep(subset(dat.2009,thema==l.thema[14])[,c("doy")],times=3),
		agenda=rep(c("(1) Public","(3) Newspaper","(2) TV"),each=60)
		)

df.kf13 <- data.frame(
		raw=c( subset(dat.2013,thema==l.thema[14])[,c("public")]/max(subset(dat.2013,thema==l.thema[14])[,c("public")]),
			subset(dat.2013,thema==l.thema[14])[,c("prr")]/max(subset(dat.2013,thema==l.thema[14])[,c("prr")]),
			subset(dat.2013,thema==l.thema[14])[,c("tvr")]/max(subset(dat.2013,thema==l.thema[14])[,c("tvr")])),
		attention=c( StructTS(subset(dat.2013,thema==l.thema[14])[,c("public")]/max(subset(dat.2013,thema==l.thema[14])[,c("public")]))$fit[,1],
			subset(dat.2013,thema==l.thema[14])[,c("pr")]/max(subset(dat.2013,thema==l.thema[14])[,c("pr")]),
			subset(dat.2013,thema==l.thema[14])[,c("tv")]/max(subset(dat.2013,thema==l.thema[14])[,c("tv")])),
		doy= rep(subset(dat.2013,thema==l.thema[14])[,c("doy")],times=3),
		agenda=rep(c("(1) Public","(3) Newspaper","(2) TV"),each=75)
		)

df.kf13$year <- "2013"
df.kf09$year <- "2009"

df.kf <- rbind(df.kf09,df.kf13)
df.kf$thema <- "International Conflict"

gg.kf <- ggplot()+
	geom_line(data=df.kf,aes(y=attention,x=doy,color=agenda,shape=agenda))+
	geom_point(data=df.kf,aes(y=raw,x=doy,color=agenda,shape=agenda),alpha=.25,size=1.0)+
	facet_grid(thema~year)+
	theme_bw()+theme(legend.position="bottom")+
		xlab("Day of year")+ylab("% of maximum attention")+
		guides(color=guide_legend("Issue Salience"),
			shape=guide_legend("Issue Salience"))+
	ggtitle("Kalman-filtered time series")

	geom_smooth(data=df.kf,linetype="dotted",aes(y=attention,x=doy,color=agenda,shape=agenda),method="loess",span=.35)+

gg.timeseries <- grid.arrange(gg.kf,gg.irf,ncol=1,nrow=2)

ggsave(gg.timeseries,file="FIGURE2.png",device="png",units="cm",width=16,height=20,dpi=1000)



ggsave(gg.irf,file="irf6.svg",device="svg",unit="cm",width=16,height=20)

ggplot(df.int.konfl,aes(y=est,ymin=lo,ymax=hi,x=lag))+geom_ribbon(alpha=.5)+facet_grid(.~year)


dat.2009$year <- "2009"
dat.2013$year <- "2013"

df.ts <- rbind(dat.2009,dat.2013)
df.ts1 <- subset(df.ts,thema%in%l.thema)
df.ts1$en.thema <- c(rep(themli.long,each=60),rep(themli.long,each=75))

df.intkonfl <- data.frame(x=)

gg.ts <- ggplot(subset(df.ts1,en.thema=="International Conflict"),aes(x=doy,y=tv))+geom_point()

names(grc.np.09) <- l.thema

grc.np.09[["energie"]]


np09lead <- rep(NA,times=length(l.thema))
np13lead <- rep(NA,times=length(l.thema))
tv09lead <- rep(NA,times=length(l.thema))
tv13lead <- rep(NA,times=length(l.thema))

rnp09lead <- rep(NA,times=length(l.thema))
rnp13lead <- rep(NA,times=length(l.thema))
rtv09lead <- rep(NA,times=length(l.thema))
rtv13lead <- rep(NA,times=length(l.thema))

inp09lead <- rep(NA,times=length(l.thema))
inp13lead <- rep(NA,times=length(l.thema))
itv09lead <- rep(NA,times=length(l.thema))
itv13lead <- rep(NA,times=length(l.thema))

names(irf.np.09) <- l.thema
names(irf.np.13) <- l.thema
names(irf.tv.09) <- l.thema
names(irf.tv.13) <- l.thema

for(i in 1:length(l.thema))
{
np09lead[i] <- grc.np.09[[i]]$Granger$p.value
np13lead[i] <- grc.np.13[[i]]$Granger$p.value
tv09lead[i] <- grc.tv.09[[i]]$Granger$p.value
tv13lead[i] <- grc.tv.13[[i]]$Granger$p.value
}

for(i in 1:length(l.thema))
{
rnp09lead[i] <- grc.rnp.09[[i]]$Granger$p.value
rnp13lead[i] <- grc.rnp.13[[i]]$Granger$p.value
rtv09lead[i] <- grc.rtv.09[[i]]$Granger$p.value
rtv13lead[i] <- grc.rtv.13[[i]]$Granger$p.value
}

for(i in 1:length(l.thema))
{
inp09lead[i] <- grc.rnp.09[[i]]$Instant$p.value
inp13lead[i] <- grc.rnp.13[[i]]$Instant$p.value
itv09lead[i] <- grc.rtv.09[[i]]$Instant$p.value
itv13lead[i] <- grc.rtv.13[[i]]$Instant$p.value
}

np09.mediaplus <- rep(NA,times=length(l.thema))
np13.mediaplus <- rep(NA,times=length(l.thema))
tv09.mediaplus <- rep(NA,times=length(l.thema))
tv13.mediaplus <- rep(NA,times=length(l.thema))

np09.mediaminus <- rep(NA,times=length(l.thema))
np13.mediaminus <- rep(NA,times=length(l.thema))
tv09.mediaminus <- rep(NA,times=length(l.thema))
tv13.mediaminus <- rep(NA,times=length(l.thema))

np09.publicplus <- rep(NA,times=length(l.thema))
np13.publicplus <- rep(NA,times=length(l.thema))
tv09.publicplus <- rep(NA,times=length(l.thema))
tv13.publicplus <- rep(NA,times=length(l.thema))

np09.publicminus <- rep(NA,times=length(l.thema))
np13.publicminus <- rep(NA,times=length(l.thema))
tv09.publicminus <- rep(NA,times=length(l.thema))
tv13.publicminus <- rep(NA,times=length(l.thema))

for (i in 1:length(l.thema))
{
np09.mediaplus[[i]] <- max(unlist(irf.np.09[[i]]$irf))
np09.mediaminus[[i]] <- min(unlist(irf.np.09[[i]]$irf))
np09.publicplus[[i]] <- max(unlist(irf.rnp.09[[i]]$irf))
np09.publicminus[[i]] <- min(unlist(irf.rnp.09[[i]]$irf))

np13.mediaplus[[i]] <- max(unlist(irf.np.13[[i]]$irf))
np13.mediaminus[[i]] <- min(unlist(irf.np.13[[i]]$irf))
np13.publicplus[[i]] <- max(unlist(irf.rnp.13[[i]]$irf))
np13.publicminus[[i]] <- min(unlist(irf.rnp.13[[i]]$irf))

tv09.mediaplus[[i]] <- max(unlist(irf.tv.09[[i]]$irf))
tv09.mediaminus[[i]] <- min(unlist(irf.tv.09[[i]]$irf))
tv09.publicplus[[i]] <- max(unlist(irf.rtv.09[[i]]$irf))
tv09.publicminus[[i]] <- min(unlist(irf.rtv.09[[i]]$irf))

tv13.mediaplus[[i]] <- max(unlist(irf.tv.13[[i]]$irf))
tv13.mediaminus[[i]] <- min(unlist(irf.tv.13[[i]]$irf))
tv13.publicplus[[i]] <- max(unlist(irf.rtv.13[[i]]$irf))
tv13.publicminus[[i]] <- min(unlist(irf.rtv.13[[i]]$irf))
}

mediaplus <- c(np09.mediaplus,np13.mediaplus,tv09.mediaplus,tv13.mediaplus)
mediaminus <- c(np09.mediaminus,np13.mediaminus,tv09.mediaminus,tv13.mediaminus)
publicplus <- c(np09.publicplus ,np13.publicplus ,tv09.publicplus ,tv13.publicplus )
publicminus <- c(np09.publicminus ,np13.publicminus ,tv09.publicminus ,tv13.publicminus )

medialead <- c(np09lead,np13lead,tv09lead,tv13lead)
publiclead <- c(rnp09lead,rnp13lead,rtv09lead,rtv13lead)
instant <- c(inp09lead,inp13lead,itv09lead,itv13lead)

overview <- data.frame(	
		topic=rep(l.thema,times=4),
		year=rep(c("2009","2013","2009","2013"),each=22),
		medium=rep(c("Newspaper","Television"),each=44),
		media.lead=medialead,
		public.lead=publiclead,
		instant=instant,
		mplus=mediaplus,
		mminus=mediaminus,
		pplus=publicplus,
		pminus=publicminus)
overview$m <- rowMaxs(cbind(overview$mplus,-overview$mminus))
overview$m.dir <- ifelse(overview$mplus>(-overview$mminus),1,-1)
overview$p.dir <- ifelse(overview$pplus>(-overview$pminus),1,-1)

overview$msig05 <- overview$media.lead<.05
overview$msig01 <- overview$media.lead<.01
overview$msig001 <- overview$media.lead<.001

overview$psig05 <- overview$public.lead<.05
overview$psig01 <- overview$public.lead<.01
overview$psig001 <- overview$public.lead<.001

overview$isig05 <- overview$instant<.05
overview$isig01 <- overview$instant<.01
overview$isig001 <- overview$instant<.001

overview$sig05 <- rowSums(overview[,c("msig05","psig05","isig05")])
overview$sig01 <- rowSums(overview[,c("msig01","psig01","isig01")])
overview$sig001 <- rowSums(overview[,c("msig001","psig001","isig001")])

TABLE3.2 <- cbind(TABLE3,overview$msig05,overview$isig05,overview$psig05)


overview$m.dir
overview$msig05
overview$isig05
overview$psig05

value <- c(18,5,41,24,13,5,46,24,6,4,53,25)
who <- factor(rep(c("media","instantaneous","public"),each=4),levels=c("media","instantaneous","public"),ordered=T)
dir <- factor(rep(c("positive, p<.05","negative, p<.05","positive, not sign.", "negative, not sign."),times=3),levels=c("positive, p<.05","negative, p<.05","positive, not sign.", "negative, not sign."),ordered=T)

df.FIGURE3 <- data.frame(value=value,who=who,dir=dir)

ggplot(df.FIGURE3)+geom_col(aes(y=value,group=who,x=who,fill=dir),position="stack")+
	geom_text(color="white",aes(label=value,x=who,y=c(10,20,40,75,10,15,40,75,4,8,40,75)))+
	scale_fill_grey(start=0.2,end=0.8)+xlab("Who leads (Granger causality)")+
	ylab("Number of cases")+theme_bw()+theme(legend.position="bottom")+
	guides(fill = guide_legend(title = "Direction/Significance of Effect"))

#########################################

#########################################

##### PREPARE DISSDIM22 DATA SET FOR PREDICTING STRENGTH OF AGENDA SETTING EFFECTS 

for(i in 1:28)
{
issdim$p.spike[i] <- publicdyn[[i]]$mean.wavevolume*100
issdim$p.base[i]  <- publicdyn[[i]]$baseline*100
issdim$p.spike[i+28] <- publicdyn13[[i]]$mean.wavevolume*100
issdim$p.base[i+28]  <- publicdyn13[[i]]$baseline*100
}

issdim$m.spike <- issdim$spike.momentum
issdim$m.base <- issdim$issue.baseline

issdim$p.spike <- replace(issdim$p.spike,!is.finite(issdim$p.spike),0)

issdim$log.m.spike <- log(issdim$m.spike+.01)
issdim$log.p.spike <- log(issdim$p.spike+.01)
issdim$log.m.base <- log(issdim$m.base+.01)
issdim$log.p.base <- log(issdim$p.base+.01)

cor(issdim[,c("m.spike","p.spike","m.base","p.base")])

dissdim <- rbind(issdim,issdim)

dissdim$ase3[1:28] <- replace(ase3.09.pr,is.na(ase3.09.pr),0)
dissdim$ase3[29:56] <- replace(ase3.13.pr,is.na(ase3.13.pr),0)
dissdim$ase3[57:84] <- replace(ase3.09.tv,is.na(ase3.09.tv),0)
dissdim$ase3[85:112] <- replace(ase3.13.tv,is.na(ase3.13.tv),0)
dissdim$medium[1:56] <- "newspaper"
dissdim$medium[57:112] <- "TV"

dissdim22 <- subset(dissdim,issue%in%l.thema)

dissdim22$mlpf <- ifelse(overview$m.dir==1,overview$msig05,FALSE)
dissdim22$mlpfinst <- ifelse(overview$m.dir==1,(overview$msig05+overview$isig05)>0,FALSE)

TABLE2 <- dissdim22[,c("medium","issue","year","m.spike","m.base","p.spike","p.base")]
TABLE3 <- dissdim22[,c("medium","issue","year","ase4")]

dissdim22$ase4 <- 100*dissdim22$ase3/max(dissdim22$ase3)

df.irf$issue <- rep(dissdim22$issue[1:22],each=60)

ase <- aggregate(df.irf$est,by=list(df.irf$thema,df.irf$year,df.irf$medium),FUN=max)

dissdim22$ase <- ase[,4]

dissdim22$ase4 <- 100*dissdim22$ase/max(dissdim22$ase)

dissdim22$jahr <- factor(dissdim22$year)

dissdim22$ase5 <- dissdim22$ase4*dissdim22$mlpf

dissdim22$lase <- dissdim22$ase4>10



