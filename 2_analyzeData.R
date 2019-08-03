
tspublic2009 <- list()
tspublic2013 <- list()

for (i in 4:38)
{
tspublic2009[[names(cc.iss2009)[i]]] <- tapply(cc.iss2009[,i]/rowSums(cc.iss2009[,4:38])*cc.iss2009$wt,cc.iss2009$rcsday,mean,na.rm=T)
tspublic2013[[names(cc.iss2013)[i]]] <- tapply(cc.iss2013[,i]/rowSums(cc.iss2013[,4:38])*cc.iss2013$wt,cc.iss2013$rcsday,mean,na.rm=T)
}

dat.2013$tvr <- as.numeric(dat.2013$tvr)
dat.2013$prr <- ifelse(is.na(dat.2013$prr),0,dat.2013$prr)

dat <- list()

themen <- levels(dat.2013$thema)

themenfull <- c(1:28)[c(-9,-16,-18,-19,-23,-28)]

for (i in 1:length(themen))
{
dat[["2009"]][["public"]][[themen[i]]] <- data.frame(doy=subset(dat.2009,thema==themen[i])$doy,raw=tspublic2009[[themen[i]]])
dat[["2009"]][["tv"]][[themen[i]]] <- subset(dat.2009,thema==themen[i])[,c("doy","tvr")]
dat[["2009"]][["newspaper"]][[themen[i]]] <- subset(dat.2009,thema==themen[i])[,c("doy","prr")]
}

for (i in 1:length(themen))
{
dat[["2013"]][["public"]][[themen[i]]] <- data.frame(doy=subset(dat.2013,thema==themen[i])$doy,raw=tspublic2013[[themen[i]]][1:75])
dat[["2013"]][["tv"]][[themen[i]]] <- subset(dat.2013,thema==themen[i])[,c("doy","tvr")]
dat[["2013"]][["newspaper"]][[themen[i]]] <- subset(dat.2013,thema==themen[i])[,c("doy","prr")]
}

for (i in 1:length(themen))
{
names(dat[["2009"]][["public"]][[themen[i]]])[2] <- "raw"
names(dat[["2009"]][["tv"]][[themen[i]]])[2] <- "raw"
names(dat[["2009"]][["newspaper"]][[themen[i]]])[2] <- "raw"
names(dat[["2013"]][["public"]][[themen[i]]])[2] <- "raw"
names(dat[["2013"]][["tv"]][[themen[i]]])[2] <- "raw"
names(dat[["2013"]][["newspaper"]][[themen[i]]])[2] <- "raw"
}

years <- c("2009","2013")
agenda <- c("public","tv","newspaper")
themen <- levels(dat.2013$thema)

for (y in 1:length(years))
{
	for (a in 1:length(agenda))
	{
		for (t in 1:length(themen))
		{
		if (sum(dat[[years[y]]][[agenda[a]]][[themen[t]]]$raw)==0){
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$kf <- 0	
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$aarima <- 0	
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$arima110 <- 0
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$arima010 <- 0
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$rollmean5 <- 0			
		}
		if (sum(dat[[years[y]]][[agenda[a]]][[themen[t]]]$raw)>0){
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$kf <- StructTS(dat[[years[y]]][[agenda[a]]][[themen[t]]]$raw)$fit[,1]
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$aarima <- auto.arima(dat[[years[y]]][[agenda[a]]][[themen[t]]]$kf)$fit
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$arima110 <- Arima(dat[[years[y]]][[agenda[a]]][[themen[t]]]$kf,order=c(1,1,0))$resid 
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$arima010 <- Arima(dat[[years[y]]][[agenda[a]]][[themen[t]]]$kf,order=c(0,1,0))$resid 
			dat[[years[y]]][[agenda[a]]][[themen[t]]]$rollmean5 <- c(NA,NA,rollmean(dat[[years[y]]][[agenda[a]]][[themen[t]]]$kf,align="center",k=5),NA,NA)
		}
		}
	}
}

tvsum09 <- c(NA)
tvsum13 <- c(NA)
npsum09 <- c(NA)
npsum13 <- c(NA)
pmean09 <- c(NA)
pmean13 <- c(NA)
pmax09 <- c(NA)
pmax13 <- c(NA)

for (i in 1:length(themenfull))
{
 tvsum09 <- rbind(tvsum09,sum(dat[["2009"]][["tv"]][[themenfull[i]]]$raw,na.rm=T))
 tvsum13 <- rbind(tvsum13,sum(dat[["2013"]][["tv"]][[themenfull[i]]]$raw,na.rm=T))
 npsum09 <- rbind(npsum09,sum(dat[["2009"]][["newspaper"]][[themenfull[i]]]$raw,na.rm=T))
 npsum13 <- rbind(npsum13,sum(dat[["2013"]][["newspaper"]][[themenfull[i]]]$raw,na.rm=T))
 pmean09 <- rbind(pmean09,mean(dat[["2009"]][["public"]][[themenfull[i]]]$raw,na.rm=T))
 pmean13 <- rbind(pmean13,mean(dat[["2013"]][["public"]][[themenfull[i]]]$raw,na.rm=T))
 pmax09 <- rbind(pmax09,max(dat[["2009"]][["public"]][[themenfull[i]]]$raw,na.rm=T))
 pmax13 <- rbind(pmax13,max(dat[["2013"]][["public"]][[themenfull[i]]]$raw,na.rm=T))
}

themenag <- c("verdrossenheit","geheimdienste","verteidigung","int.konfl",
	"krise","einnahmen","gesundheit","infrastruktur","internationales",
	"extremismus","inneres","EU","arbeit","einkommen","familie","wirtschaft",
	"rente","energie","migration","haushalt","bildung")

rownames(tvsum09)[2:23] <- themen[themenfull]
rownames(tvsum13)[2:23] <- themen[themenfull]
rownames(npsum09)[2:23] <- themen[themenfull]
rownames(npsum13)[2:23] <- themen[themenfull]
rownames(pmean09)[2:23] <- themen[themenfull]
rownames(pmean13)[2:23] <- themen[themenfull]
rownames(pmax09)[2:23] <- themen[themenfull]
rownames(pmax13)[2:23] <- themen[themenfull]

x <- cbind(tvsum09[themenag,],npsum09[themenag,],round(pmean09[themenag,],2),round(pmax09[themenag,],2),
	tvsum13[themenag,],npsum13[themenag,],round(pmean13[themenag,],2),round(pmax13[themenag,],2))

print(xtable(x),type="html")

issues <- themen[themenfull]

agenda.media <- c("tv-public","np-public")
agenda.public <- c("public-tv","public-np")

varresults <- list()
grangerresults <- data.frame(year=rep(years,each=length(themen)*6),issue=rep(themen,times=12),cause=rep(rep(c("public","tv","publictv","public","np","publicnp"),each=28),times=3),mediaagenda=rep(rep(c("tv","tv","tv","np","np","np"),each=28),times=3),pvalue=NA,maxfit=NA,minfit=NA)
irfresults <- list()

for (y in 1:length(years))
{
	for (t in themenfull)
	{
	tryCatch({
		pu <- na.approx(dat[[y]][["public"]][[t]]$rollmean5)
		tv <- na.approx(dat[[y]][["tv"]][[t]]$rollmean5)
		np <- na.approx(dat[[y]][["newspaper"]][[t]]$rollmean5)
	var.pu.tv <- VAR(data.frame(public=pu/max(pu),tv=tv/max(tv)),p=14)
	var.pu.np <- VAR(data.frame(public=pu/max(pu),np=np/max(np)),p=14)

	varresults[[years[y]]][[themen[t]]][["public-tv"]] <- var.pu.tv
	varresults[[years[y]]][[themen[t]]][["public-np"]] <- var.pu.np

	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="public"&grangerresults$mediaagenda=="tv",]$pvalue <- as.numeric(causality(var.pu.tv,cause="public")$Granger$p.value)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="tv"&grangerresults$mediaagenda=="tv",]$pvalue <- as.numeric(causality(var.pu.tv,cause="tv")$Granger$p.value)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="publictv"&grangerresults$mediaagenda=="tv",]$pvalue <- as.numeric(causality(var.pu.tv,cause="public")$Instant$p.value)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="public"&grangerresults$mediaagenda=="np",]$pvalue <- as.numeric(causality(var.pu.np,cause="public")$Granger$p.value)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="np"&grangerresults$mediaagenda=="np",]$pvalue <- as.numeric(causality(var.pu.np,cause="np")$Granger$p.value)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="publicnp"&grangerresults$mediaagenda=="np",]$pvalue <- as.numeric(causality(var.pu.np,cause="public")$Instant$p.value)

		pu <- na.approx(dat[[y]][["public"]][[t]]$arima110)
		tv <- na.approx(dat[[y]][["tv"]][[t]]$arima110)
		np <- na.approx(dat[[y]][["newspaper"]][[t]]$arima110)
	var.pu.tv <- VAR(data.frame(public=pu/max(pu),tv=tv/max(tv)),p=14)
	var.pu.np <- VAR(data.frame(public=pu/max(pu),np=np/max(np)),p=14)

	varresults[[years[y]]][[themen[t]]][["public-tv"]] <- var.pu.tv
	varresults[[years[y]]][[themen[t]]][["public-np"]] <- var.pu.np

	irf.ptv <- irf(var.pu.tv,impulse="public",response="tv",cumulative=T,boot=T,runs=100,ortho=T,seed=9349,n.ahead=14)
	irf.tvp <- irf(var.pu.tv,impulse="tv",response="public",cumulative=T,boot=T,runs=100,ortho=T,seed=9349,n.ahead=14)
	irf.pnp <- irf(var.pu.np,impulse="public",response="np",cumulative=T,boot=T,runs=100,ortho=T,seed=9349,n.ahead=14)
	irf.npp <- irf(var.pu.np,impulse="np",response="public",cumulative=T,boot=T,runs=100,ortho=T,seed=9349,n.ahead=14)

	irfresults[[years[y]]][[themen[t]]][["public-tv"]]$fit	<- irf.ptv[[1]]$public
	irfresults[[years[y]]][[themen[t]]][["public-tv"]]$lower	<- irf.ptv[[2]]$public
	irfresults[[years[y]]][[themen[t]]][["public-tv"]]$upper	<- irf.ptv[[3]]$public
	irfresults[[years[y]]][[themen[t]]][["tv-public"]]$fit	<- irf.tvp[[1]]$tv
	irfresults[[years[y]]][[themen[t]]][["tv-public"]]$lower	<- irf.tvp[[2]]$tv
	irfresults[[years[y]]][[themen[t]]][["tv-public"]]$upper	<- irf.tvp[[3]]$tv
	irfresults[[years[y]]][[themen[t]]][["public-np"]]$fit	<- irf.pnp[[1]]$public
	irfresults[[years[y]]][[themen[t]]][["public-np"]]$lower	<- irf.pnp[[2]]$public
	irfresults[[years[y]]][[themen[t]]][["public-np"]]$upper	<- irf.pnp[[3]]$public
	irfresults[[years[y]]][[themen[t]]][["np-public"]]$fit	<- irf.npp[[1]]$np
	irfresults[[years[y]]][[themen[t]]][["np-public"]]$lower	<- irf.npp[[2]]$np
	irfresults[[years[y]]][[themen[t]]][["np-public"]]$upper	<- irf.npp[[3]]$np

	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="public"&grangerresults$mediaagenda=="tv","maxfit"] <- max(irfresults[[years[y]]][[themen[t]]][["public-tv"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="tv"&grangerresults$mediaagenda=="tv","maxfit"] <- max(irfresults[[years[y]]][[themen[t]]][["tv-public"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="public"&grangerresults$mediaagenda=="np","maxfit"] <- max(irfresults[[years[y]]][[themen[t]]][["public-np"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="np"&grangerresults$mediaagenda=="np","maxfit"] <- max(irfresults[[years[y]]][[themen[t]]][["np-public"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="publicnp"&grangerresults$mediaagenda=="np","maxfit"] <- max(irfresults[[years[y]]][[themen[t]]][["np-public"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="publictv"&grangerresults$mediaagenda=="tv","maxfit"] <- max(irfresults[[years[y]]][[themen[t]]][["tv-public"]]$fit)

	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="public"&grangerresults$mediaagenda=="tv","minfit"] <- min(irfresults[[years[y]]][[themen[t]]][["public-tv"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="tv"&grangerresults$mediaagenda=="tv","minfit"] <- min(irfresults[[years[y]]][[themen[t]]][["tv-public"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="public"&grangerresults$mediaagenda=="np","minfit"] <- min(irfresults[[years[y]]][[themen[t]]][["public-np"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="np"&grangerresults$mediaagenda=="np","minfit"] <- min(irfresults[[years[y]]][[themen[t]]][["np-public"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="publicnp"&grangerresults$mediaagenda=="np","minfit"] <- min(irfresults[[years[y]]][[themen[t]]][["np-public"]]$fit)
	grangerresults[grangerresults$year==years[y]&grangerresults$issue==themen[t]&grangerresults$cause=="publictv"&grangerresults$mediaagenda=="tv","minfit"] <- min(irfresults[[years[y]]][[themen[t]]][["tv-public"]]$fit)
	print(c(years[y],themen[t]))
	Sys.sleep(0.01)
	flush.console()
	})
	}
}


irf.int.konfl <- data.frame(irfresults[["2009"]][["int.konfl"]][["tv-public"]])
irf.int.konflr <- data.frame(irfresults[["2009"]][["int.konfl"]][["public-tv"]])

irf.int.konfl$lag <- 0:14
names(irf.int.konfl) <- c("est","lo","hi","lag")
irf.int.konflr$lag <- 0:14
names(irf.int.konflr) <- c("est","lo","hi","lag")

irfdemo <- ggplot(irf.int.konfl,aes(x=lag,y=est/3,ymin=lo/3,ymax=(hi+0.05)/3))+
		geom_ribbon(alpha=.25)+geom_line(size=1.25)+ylim(-0.1,0.3)+
		geom_hline(yintercept=0)+theme_bw()+theme(legend.position="bottom")+
		xlab("Days after news story (=impulse)")+ylab("Public salience change \n (=response)")+
		guides(color=guide_legend("TV news stories' impulse on public salience"),
			linetype=guide_legend("TV news stories' impulse on public salience"))+
		ggtitle("Cumulative impulse-response functions")+
		scale_color_manual(values=c("#222222","#888888"))

irfrdemo <- ggplot(irf.int.konflr,aes(x=lag,y=est*3,ymin=lo*3,ymax=(hi+0.05)*3))+
		geom_ribbon(alpha=.25)+geom_line(size=1.25)+ylim(-2,2)+
		geom_hline(yintercept=0)+theme_bw()+theme(legend.position="bottom")+
		xlab("Days after public salience change (=Impulse)")+ylab("Additional TV news stories \n (=Response)")+
		guides(color=guide_legend("Public salience's impulse on TV news stories"),
			linetype=guide_legend("Public salience's impulse on TV news stories"))+
		ggtitle("Cumulative impulse-response functions")+
		scale_color_manual(values=c("#222222","#888888"))

tvdemo <- ggplot(dat[[1]][[2]][[15]])+
	geom_point(aes(y=raw,x=doy),shape=20,size=1.5)+
	geom_line(aes(y=kf,x=doy),color="#666666",linetype="solid",size=0.75)+
	geom_line(aes(y=arima110,x=doy),color="#aaaaaa",linetype="solid",size=1.5)+
	geom_hline(aes(yintercept=2.7),linetype="longdash")+
	geom_hline(aes(yintercept=0),linetype="solid")+
	annotate("rect",xmin=247,xmax=255,ymin=2.7,ymax=7.5,alpha=.25,color="black")+
	theme_bw()+ylab("TV news stories")+ggtitle("International Conflict, 2009, TV news salience")
npdemo <- ggplot(dat[[1]][[3]][[15]])+
	geom_point(aes(y=raw,x=doy),shape=20,size=1.5)+
	geom_line(aes(y=kf,x=doy),color="#666666",linetype="solid",size=0.75)+
	geom_line(aes(y=arima110,x=doy),color="#aaaaaa",linetype="solid",size=1.5)+
	geom_hline(aes(yintercept=2.7),linetype="longdash")+
	geom_hline(aes(yintercept=0),linetype="solid")+
	annotate("rect",xmin=247,xmax=255,ymin=2.7,ymax=7.5,alpha=.25,color="black")+
	theme_bw()+ylab("News stories")+ggtitle("International Conflict, 2009, newspaper salience")
mediademo <- ggplot(dat[[1]][[2]][[15]]+dat[[1]][[3]][[15]])+
	geom_point(aes(y=raw,x=doy/2),shape=20,size=1.5)+
	geom_line(aes(y=kf,x=doy/2),color="#666666",linetype="solid",size=0.75)+
	geom_line(aes(y=arima110,x=doy/2),color="#aaaaaa",linetype="solid",size=1.5)+
	geom_hline(aes(yintercept=2.7),linetype="longdash")+
	geom_hline(aes(yintercept=0),linetype="solid")+
	annotate("rect",xmin=246,xmax=257,ymin=2.7,ymax=12.0,alpha=.25,color="black")+
	theme_bw()+ylab("Newspaper stories")+ggtitle("International Conflict, 2009, media salience (TV+newspaper)")+
	xlab("Day of year")
pubdemo <- ggplot(dat[[1]][[1]][[15]])+
	geom_point(aes(y=raw,x=doy),shape=20,size=1.5)+
	geom_line(aes(y=kf,x=doy),color="#666666",linetype="solid",size=0.75)+
	geom_line(aes(y=arima110,x=doy),color="#aaaaaa",linetype="solid",size=1.5)+
	geom_hline(aes(yintercept=0.02),linetype="longdash")+
	geom_hline(aes(yintercept=0),linetype="solid")+
	annotate("rect",xmin=248,xmax=257,ymin=0.02,ymax=0.0525,alpha=.25,color="black")+
	annotate("rect",xmin=258,xmax=268,ymin=0.02,ymax=0.0525,alpha=.25,color="black")+
	theme_bw()+ylab("Issue mentions per respondent")+ggtitle("International Conflict, 2009, public salience")+
	xlab("Day of year")

tsdemo <- grid.arrange(mediademo,irfdemo,pubdemo,irfrdemo,ncol=2)
ggsave(tsdemo,file="tsdemo.svg",dpi=1200,unit="cm",width=16,height=16,scale=1.25)
ggsave(tsdemo,file="tsdemo.jpg",dpi=1200,unit="cm",width=16,height=16,scale=1.25)
ggsave(tsdemo,file="tsdemo.pdf",dpi=1200,unit="cm",width=16,height=16,scale=1.25)
ggsave(tsdemo,file="tsdemo.png",dpi=1200,unit="cm",width=16,height=16,scale=1.25)





events <- data.frame(issue=rep(themen,times=9),year=rep(years,each=84),agenda=rep(rep(agenda,each=28),times=length(years)))

events$meanwavevolume <- NA
events$adjustedbaseline <- NA
events$eventfrequency <- NA
events$relativevolatilityinevents <- NA
events$baseline <- NA

#for (y in 1:length(years))
#{
#for (a in 1:length(agenda))
#{
#for (t in 1:length(themen))
#{
#	events[events$issue==themen[t]&events$year==years[y]&events$agenda==agenda[a]	,c("meanwavevolume","adjustedbaseline","eventfrequency","relativevolatilityinevents","baseline")] <-	eventextract(dat[[years[y]]][[agenda[a]]][[themen[t]]]$raw,	ifelse(agenda[a]=="public",.05,1))[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]
#}
#}
#}

for (y in 1:length(years))
{
for (t in 1:length(themen))
{
	events[events$issue==themen[t]&events$year==years[y]&events$agenda=="tv"		,c("meanwavevolume","adjustedbaseline","eventfrequency","relativevolatilityinevents","baseline")] <-	eventextract((dat[[years[y]]][["tv"]][[themen[t]]]$raw+dat[[years[y]]][["newspaper"]][[themen[t]]]$raw),	1.0)		[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]
	events[events$issue==themen[t]&events$year==years[y]&events$agenda=="newspaper"	,c("meanwavevolume","adjustedbaseline","eventfrequency","relativevolatilityinevents","baseline")] <-	eventextract((dat[[years[y]]][["newspaper"]][[themen[t]]]$raw+dat[[years[y]]][["tv"]][[themen[t]]]$raw),	1.0)		[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]
	events[events$issue==themen[t]&events$year==years[y]&events$agenda=="public"		,c("meanwavevolume","adjustedbaseline","eventfrequency","relativevolatilityinevents","baseline")] <-	eventextract( dat[[years[y]]][["public"]][[themen[t]]]$raw,	.05)		[c("mean.wavevolume","adjusted.baseline","event.frequency","relative.volatility.in.events","baseline")]
}
}

eventextract((dat[[years[y]]][["tv"]][[themen[t]]]$raw+dat[[years[y]]][["newspaper"]][[themen[t]]]$raw),	1.0)
eventextract((dat[[years[y]]][["newspaper"]][[themen[t]]]$raw+dat[[years[y]]][["tv"]][[themen[t]]]$raw),	1.0)
eventextract((dat[[years[y]]][["public"]][[themen[t]]]$raw)								,	0.025)


grangerresults$mpi <- Recode(grangerresults$cause,"'public'='public';'np'='media';'tv'='media';'publictv'='instantaneous';'publicnp'='instantaneous'")
grangerresults$positive <- abs(grangerresults$maxfit)>abs(grangerresults$minfit)
grangerresults$significant <- grangerresults$pvalue<.05
grangerresults$eff <- ifelse(grangerresults$positive,grangerresults$maxfit,0)
grangerresults$result <- ifelse(grangerresults$positive==TRUE & grangerresults$pvalue<.10,"positive, p<.10",
					ifelse(grangerresults$positive==FALSE & grangerresults$pvalue<.10,"negative, p<.10",
						ifelse(grangerresults$positive==TRUE & !grangerresults$pvalue<.10,"positive, n. s.","negative, n. s.")))
grangerresults$result <- factor(grangerresults$result,levels=c("positive, p<.10","negative, p<.10","positive, n. s.","negative, n. s."),ordered=T)
grangerresults$result05 <- ifelse(grangerresults$positive==TRUE & grangerresults$pvalue<.05,"positive, p<.05",
					ifelse(grangerresults$positive==FALSE & grangerresults$pvalue<.05,"negative, p<.05",
						ifelse(grangerresults$positive==TRUE & !grangerresults$pvalue<.05,"positive, n. s.","negative, n. s.")))
grangerresults$result05 <- factor(grangerresults$result05,levels=c("positive, p<.05","negative, p<.05","positive, n. s.","negative, n. s."),ordered=T)
grangerresults$mpi <- factor(grangerresults$mpi,labels=c("media","instantaneous","public"),levels=c("media","instantaneous","public"),ordered=T)


table(subset(grangerresults,year=="2009" | year=="2013")$pvalue<.10,subset(grangerresults,year=="2009" | year=="2013")$positive,subset(grangerresults,year=="2009" | year=="2013")$mpi)

grangertv <- subset(grangerresults,mediaagenda=="tv")
grangernp <- subset(grangerresults,mediaagenda=="np")
grangermedia <- subset(grangerresults,mpi=="media")
grangerinstant <- subset(grangerresults,mpi=="instantaneous")
grangerpublic <- subset(grangerresults,mpi=="public")

granger0913 <- subset(grangerresults,year!="2017")

 table(tapply(grangermedia$pvalue<.05&grangermedia$positive==T,interaction(grangermedia$issue,grangermedia$year),sum)>0)
 table(tapply(grangerinstant$pvalue<.05&grangerinstant$positive==T,interaction(grangerinstant$issue,grangerinstant$year),sum)>0)
 table(tapply(grangerpublic$pvalue<.05&grangerpublic$positive==T,interaction(grangerpublic$issue,grangerpublic$year),sum)>0)

table(grangertv$pvalue<.05,grangertv$positive,grangertv$mpi)
table(grangernp$pvalue<.05,grangernp$positive,grangernp$mpi)

table(tapply(grangermedia$pvalue<.05&grangermedia$positive==T,interaction(grangermedia$issue,grangermedia$year),sum)>0)

table(grangerresults$pvalue<.05,grangerresults$positive,grangerresults$mpi,grangerresults$year)


table(grangerresults$pvalue<.10,grangerresults$positive,grangerresults$mpi)
table(grangerresults$pvalue<.05,grangerresults$positive,grangerresults$mpi)
table(grangerresults$pvalue<.01,grangerresults$positive,grangerresults$mpi)

aggregate(granger0913$pvalue,by=list(granger0913$year,granger0913$issue,granger0913$mediaagenda),FUN="min")

table(granger0913$pvalue<.10,granger0913$positive,granger0913$mpi)
table(granger0913$pvalue<.05,granger0913$positive,granger0913$mpi)
table(granger0913$pvalue<.01,granger0913$positive,granger0913$mpi)

table(table(granger0913$pvalue<.05,interaction(granger0913$issue,granger0913$year,granger0913$mediaagenda))[2,])
table(table(granger0913$pvalue<.05 & granger0913$mpi!="public" & granger0913$positive==T,interaction(granger0913$issue,granger0913$year,granger0913$mediaagenda))[2,])

table(tapply(subset(grangerresults,!is.na(pvalue))$pvalue<.10,interaction(subset(grangerresults,!is.na(pvalue))$issue,subset(grangerresults,!is.na(pvalue))$year,subset(grangerresults,!is.na(pvalue))$mediaagenda),sum,na.rm=T))
table(tapply(subset(grangerresults,!is.na(pvalue))$pvalue<.05,interaction(subset(grangerresults,!is.na(pvalue))$issue,subset(grangerresults,!is.na(pvalue))$year,subset(grangerresults,!is.na(pvalue))$mediaagenda),sum,na.rm=T))


TABLE.A6 <- grangerresults[,c("year","issue","mediaagenda","cause","pvalue","maxfit","minfit")]

TABLE.A6$direction <- TABLE.A6$maxfit+TABLE.A6$minfit
TABLE.A6$significant <- TABLE.A6$pvalue<.05

distribution <- ggplot(subset(grangerresults,!is.na(pvalue)))+ylim(0,44)+
	geom_bar(aes(x=mpi,fill=result),position="stack")+
	xlab("Who leads (Granger causality)")+ylab("Number of cases")+
	scale_fill_manual(values=c("#222222","#666666","#CCCCCC","#EEEEEE"))+
	guides(fill=guide_legend("Direction/significance of effect"))+theme_bw()+theme(legend.position="bottom")+
	annotate("text",x=1,y=12,label="25")+annotate("text",x=1,y=45,label="38")+annotate("text",x=1,y=66,color="#eeeeee",label="5")+annotate("text",x=1,y=80,color="#eeeeee",label="20")+
	annotate("text",x=2,y=12,label="23")+annotate("text",x=2,y=45,label="39")+annotate("text",x=2,y=66,color="#eeeeee",label="7")+annotate("text",x=2,y=80,color="#eeeeee",label="19")+
	annotate("text",x=3,y=12,label="31")+annotate("text",x=3,y=50,label="43")+annotate("text",x=3,y=76,color="#eeeeee",label="4")+annotate("text",x=3,y=82,color="#eeeeee",label="10")+
	facet_grid(.~year)

distribution <- ggplot(subset(grangerresults,!is.na(pvalue)&year!="2017"))+
	geom_bar(aes(x=mpi,fill=result05),position="stack")+
	xlab("Who leads (Granger causality)")+ylab("Number of cases")+
	facet_grid(.~year)+ylim(0,44)+
	scale_fill_manual(values=c("#222222","#666666","#CCCCCC","#EEEEEE"))+
	guides(fill=guide_legend("Direction/significance of effect"))+theme_bw()+theme(legend.position="bottom")+
	annotate("text",x=1,y=12,label="23")+annotate("text",x=1,y=40,label="33")+annotate("text",x=1,y=62,color="#eeeeee",label="10")+annotate("text",x=1,y=80,color="#eeeeee",label="22")+
	annotate("text",x=2,y=12,label="17")+annotate("text",x=2,y=40,label="39")+annotate("text",x=2,y=68,color="#eeeeee",label="11")+annotate("text",x=2,y=80,color="#eeeeee",label="21")+
	annotate("text",x=3,y=12,label="16")+annotate("text",x=3,y=40,label="40")+annotate("text",x=3,y=67,color="#eeeeee",label="13")+annotate("text",x=3,y=80,color="#eeeeee",label="19")

distribution10 <- ggplot(subset(grangerresults,!is.na(pvalue)&year!="2017"))+
	geom_bar(aes(x=mpi,fill=result),position="stack")+
	xlab("Who leads (Granger causality)")+ylab("Number of cases")+
	scale_fill_manual(values=c("#222222","#666666","#CCCCCC","#EEEEEE"))+
	guides(fill=guide_legend("Direction/significance of effect"))+theme_bw()+theme(legend.position="bottom")+
	annotate("text",x=1,y=7,label="20")+annotate("text",x=1,y=30,label="25")+annotate("text",x=1,y=51,color="#eeeeee",label="13")+annotate("text",x=1,y=80,color="#eeeeee",label="30")+
	annotate("text",x=2,y=7,label="18")+annotate("text",x=2,y=30,label="32")+annotate("text",x=2,y=56,color="#eeeeee",label="15")+annotate("text",x=2,y=80,color="#eeeeee",label="23")+
	annotate("text",x=3,y=7,label="19")+annotate("text",x=3,y=30,label="35")+annotate("text",x=3,y=60,color="#eeeeee",label="15")+annotate("text",x=3,y=80,color="#eeeeee",label="19")


distribution05 <- ggplot(subset(grangerresults,!is.na(pvalue)&year!="2017"))+
	geom_bar(aes(x=mpi,fill=result05),position="stack")+
	xlab("Who leads (Granger causality)")+ylab("Number of cases")+
	scale_fill_manual(values=c("#222222","#666666","#CCCCCC","#EEEEEE"))+
	guides(fill=guide_legend("Direction/significance of effect"))+theme_bw()+theme(legend.position="bottom")+
	annotate("text",x=1,y=7,label="22")+annotate("text",x=1,y=42,label="33")+annotate("text",x=1,y=61,color="#eeeeee",label="10")+annotate("text",x=1,y=80,color="#eeeeee",label="23")+
	annotate("text",x=2,y=7,label="21")+annotate("text",x=2,y=42,label="39")+annotate("text",x=2,y=65,color="#eeeeee",label="11")+annotate("text",x=2,y=80,color="#eeeeee",label="17")+
	annotate("text",x=3,y=7,label="19")+annotate("text",x=3,y=42,label="40")+annotate("text",x=3,y=65,color="#eeeeee",label="13")+annotate("text",x=3,y=80,color="#eeeeee",label="16")

ggsave(distribution05 ,file="dist05.svg",unit="cm",width=16,height=10,dpi=1200,scale=1.25)
ggsave(distribution05 ,file="dist05.jpg",unit="cm",width=16,height=10,dpi=1200,scale=1.25)
ggsave(distribution05 ,file="dist05.pdf",unit="cm",width=16,height=10,dpi=1200,scale=1.25)
ggsave(distribution05 ,file="dist05.png",unit="cm",width=16,height=10,dpi=1200,scale=1.25)

ggsave(distribution10 ,file="dist10.svg",unit="cm",width=16,height=10,dpi=1200,scale=1.25)
ggsave(distribution10 ,file="dist10.jpg",unit="cm",width=16,height=10,dpi=1200,scale=1.25)
ggsave(distribution10 ,file="dist10.pdf",unit="cm",width=16,height=10,dpi=1200,scale=1.25)
ggsave(distribution10 ,file="dist10.png",unit="cm",width=16,height=10,dpi=1200,scale=1.25)

# The share of issues exhibiting classical agenda setting effects seems to vary from campaign to campaign

ineq(colMeans(matrix(unlist(tspublic2009),ncol=35)))
ineq(colMeans(matrix(unlist(tspublic2013),ncol=35)))
ineq(colMeans(matrix(unlist(tspublic2017),ncol=35)))

entropy.2009 <- entropy.empirical(colMeans(matrix(unlist(tspublic2009),ncol=35)))/entropy.empirical(rep(sum(colMeans(matrix(unlist(tspublic2009),ncol=35))),times=35))
entropy.2013 <- entropy.empirical(colMeans(matrix(unlist(tspublic2013),ncol=35)))/entropy.empirical(rep(sum(colMeans(matrix(unlist(tspublic2013),ncol=35))),times=35))
entropy.2017 <- entropy.empirical(colMeans(matrix(unlist(tspublic2017),ncol=35)))/entropy.empirical(rep(sum(colMeans(matrix(unlist(tspublic2017),ncol=35))),times=35))

ggsave(distribution,file="dist.png",units="cm",dpi=1000,width=16,height=16,scale=1.15)
ggsave(distribution,file="dist.svg",units="cm",dpi=1000,width=16,height=16,scale=1.15)
ggsave(distribution,file="dist.jpg",units="cm",dpi=1000,width=16,height=16,scale=1.15)
ggsave(distribution,file="dist.pdf",units="cm",dpi=1000,width=16,height=16,scale=1.15)

tvclass <- events[events$agenda=="tv",]
npclass <- events[events$agenda=="newspaper",]
publicclass <- events[events$agenda=="public",]
tvpositive <- grangerresults[grangerresults$cause=="tv",c("pvalue","maxfit","minfit","positive","significant","eff")]
nppositive <- grangerresults[grangerresults$cause=="np",c("pvalue","maxfit","minfit","positive","significant","eff")]

tveff <- data.frame(tvclass[,1:3],media.spike=tvclass[,4],media.base=tvclass[,5],pub.spike=publicclass[,4],pub.base=publicclass[,5],tvpositive)
npeff <- data.frame(npclass[,1:3],media.spike=tvclass[,4],media.base=tvclass[,5],pub.spike=publicclass[,4],pub.base=publicclass[,5],nppositive)

fulleff <- rbind(tveff,npeff)

fulleff$media.spike <- ifelse(is.nan(fulleff$media.spike),0,fulleff$media.spike)
fulleff$pub.spike <- ifelse(is.nan(fulleff$pub.spike),0,fulleff$pub.spike)

q95.mspike <- quantile(fulleff$media.spike,seq(0,1,.01))[96]
q95.pspike <- quantile(fulleff$pub.spike,seq(0,1,.01))[96]
q95.mbase <- quantile(fulleff$media.base,seq(0,1,.01))[96]
q95.pbase <- quantile(fulleff$pub.base,seq(0,1,.01))[96]

fulleff$m.spike2 <- replace(fulleff$media.spike,fulleff$media.spike>q95.mspike,q95.mspike)
fulleff$m.base2 <- replace(fulleff$media.base,fulleff$media.base>q95.mbase,q95.mbase)
fulleff$p.spike2 <- replace(fulleff$pub.spike,fulleff$pub.spike>q95.pspike,q95.pspike)
fulleff$p.base2 <- replace(fulleff$pub.base,fulleff$pub.base>q95.pbase,q95.pbase)

fulleff$m.spike <- 99*fulleff$media.spike/max(fulleff$media.spike)+1
fulleff$m.base <- 99*fulleff$media.base/max(fulleff$media.base)+1
fulleff$p.spike <- 99*fulleff$pub.spike/max(fulleff$pub.spike)+1
fulleff$p.base <- 99*fulleff$pub.base/max(fulleff$pub.base)+1

fulleff$log2.m.spike <- log(99*fulleff$media.spike/max(fulleff$media.spike)+1)
fulleff$log2.m.base <- log(99*fulleff$media.base/max(fulleff$media.base)+1)
fulleff$log2.p.spike <- log(99*fulleff$pub.spike/max(fulleff$pub.spike)+1)
fulleff$log2.p.base <- log(99*fulleff$pub.base/max(fulleff$pub.base)+1)


fulleff$effx <- ifelse(fulleff$eff>(quantile(fulleff$eff,na.rm=T,seq(0,1,.01))[100]),(quantile(fulleff$eff,na.rm=T,seq(0,1,.01))[100]),fulleff$eff)
fulleff$maxfitx <- ifelse(fulleff$maxfit>(quantile(fulleff$maxfit,na.rm=T,seq(0,1,.01))[100]),(quantile(fulleff$maxfit,na.rm=T,seq(0,1,.01))[100]),fulleff$maxfit)

# fulleff$effx <- ifelse(fulleff$eff>(quantile(fulleff$eff,na.rm=T,seq(0,1,.01))[100]),0.539,fulleff$eff)
# fulleff$maxfitx <- ifelse(fulleff$maxfit>(quantile(fulleff$maxfit,na.rm=T,seq(0,1,.01))[100]),0.539,fulleff$maxfit)

fulleff$eff2 <- (99*fulleff$effx/max(fulleff$effx,na.rm=T))+1
fulleff$eff3 <- (99*fulleff$maxfitx/max(fulleff$maxfitx,na.rm=T))+1

fulleff$logeff2 <- log(fulleff$eff2)
fulleff$logeff3 <- log(fulleff$eff3)

fulleff$log.m.spike 	<- log(fulleff$m.spike)
fulleff$log.m.base 	<- log(fulleff$m.base)
fulleff$log.p.spike 	<- log(fulleff$p.spike)
fulleff$log.p.base 	<- log(fulleff$p.base)

fulleff$log2.m.spike <- replace(fulleff$log.m.spike,fulleff$log.m.spike>3.4,3)

fulleff$spike <- ((fulleff$p.spike)+(fulleff$m.spike))/2
fulleff$base <- ((fulleff$p.base)+(fulleff$m.base))/2

fulleff$log.spike <- log(fulleff$spike)
fulleff$log.base <- log(fulleff$base)

fulleff$possig <- fulleff$positive*fulleff$significant
fulleff$possig10 <- fulleff$positive*(fulleff$pvalue<.10)
fulleff$possig01 <- fulleff$positive*(fulleff$pvalue<.01)

fulleff$iyear <- interaction(fulleff$year,fulleff$issue)

fulleff2 <- subset(fulleff,year=="2009"|year=="2013")

fulleff2$constantvariable <- c(1)


for (y in 1:length(years[1:2]))
{
for (a in 1:length(agenda[2:3]))
{
for (i in 1:length(issues))
{
selector <- fulleff$issue==issues[i]&fulleff$agenda==agenda[a+1]&fulleff$year==years[y]
fulleff[selector,"lag.media.max"] <- which.max(irfresults[[years[y]]][[issues[i]]][[agenda.media[a]]]$fit)-1
fulleff[selector,"lag.public.max"] <- which.max(irfresults[[years[y]]][[issues[i]]][[agenda.public[a]]]$fit)-1
fulleff[selector,"lag.media.min"] <- which.min(irfresults[[years[y]]][[issues[i]]][[agenda.media[a]]]$fit)-1
fulleff[selector,"lag.public.min"] <- which.min(irfresults[[years[y]]][[issues[i]]][[agenda.public[a]]]$fit)-1
}
}
}


possig.X <- glmer(possig01~constantvariable+(1|issue)+(1|agenda)+(1|year),family=binomial,data=fulleff2,seed=9349)
possig.0 <- glmer(possig01~1+(1|issue)+(1|agenda)+(1|year),family=binomial,data=fulleff2,seed=9349)
possig.1 <- glmer(possig01~1+log.m.spike+log.m.base+(1|issue)+(1|agenda)+(1|year),family=binomial,data=fulleff2,seed=9349)
possig.11 <- glmer(possig01~1+log.p.spike+log.p.base+(1|issue)+(1|agenda)+(1|year),family=binomial,data=fulleff2,seed=9349)
possig.2 <- glmer(possig01~1+log.p.spike+log.p.base+log.m.spike+log.m.base+(1|issue)+(1|agenda)+(1|year),family=binomial,data=fulleff2,seed=9349,method="boot",nsim=200)
possig.2Y <- glmer(possig01~1+year*(log.p.spike+log.p.base+log.m.spike+log.m.base)+(1|issue)+(1|agenda)+(1|year),family=binomial,data=fulleff2,seed=9349)

str.0 <- lmer(logeff3~1+(1|issue)+(1|agenda)+(1|year),data=fulleff2,seed=9349)
str.1 <- lmer(logeff3~1+log.m.spike+log.m.base+(1|issue)+(1|agenda)+(1|year),data=fulleff2,seed=9349)
str.11 <- lmer(logeff3~1+log.p.spike+log.p.base+(1|issue)+(1|agenda)+(1|year),data=fulleff2,seed=9349)
str.2 <- lmer(logeff3~1+log.p.spike+log.p.base+log.m.spike+log.m.base+(1|issue)+(1|agenda)+(1|year),data=fulleff2,seed=9349)
str.2Y <- lmer(logeff3~1+year*(log.p.spike+log.p.base+log.m.spike+log.m.base)+(1|issue)+(1|agenda)+(1|year),data=fulleff2,seed=9349)
	
lm.2 <- lm(logeff3~1+log.p.spike+log.p.base+log.m.spike+log.m.base,data=fulleff2)

ci.possig.0 <- confint(possig.0,method="boot",nsim=1000)
ci.possig.1 <- confint(possig.1,method="boot",nsim=1000)
ci.possig.2 <- confint(possig.2,method="boot",nsim=1000)
ci.str.0 <- confint(str.0,method="boot",nsim=1000)
ci.str.1 <- confint(str.1,method="boot",nsim=1000)
ci.str.2 <- confint(str.2,method="boot",nsim=1000)

df.possig.spikeC <- data.frame(Effect(mod=possig.2Y,focal.predictors=c("log.m.spike","year"),xlevels=list(log.m.spike=seq(0,5,0.1))))
df.possig.pspikeC <- data.frame(Effect(mod=possig.2Y,focal.predictors=c("log.p.spike","year"),xlevels=list(log.p.spike=seq(0,5,0.1))))
df.possig.baseC <- data.frame(Effect(mod=possig.2Y,focal.predictors=c("log.m.base","year"),xlevels=list(log.m.base=seq(0,5,0.1))))
df.possig.pbaseC <- data.frame(Effect(mod=possig.2Y,focal.predictors=c("log.p.base","year"),xlevels=list(log.p.base=seq(0,5,0.1))))

df.possig.spikeC$m.spike <- exp(df.possig.spikeC$log.m.spike)
df.possig.pspikeC$p.spike <- exp(df.possig.pspikeC$log.p.spike)
df.possig.baseC$m.base <- exp(df.possig.baseC$log.m.base)
df.possig.pbaseC$p.base <- exp(df.possig.pbaseC$log.p.base)

df.str.spikeC <- data.frame(Effect(mod=str.2Y,focal.predictors=c("log.m.spike","year"),xlevels=list(log.m.spike=seq(0,5,0.1))))
df.str.pspikeC <- data.frame(Effect(mod=str.2Y,focal.predictors=c("log.p.spike","year"),xlevels=list(log.p.spike=seq(0,5,0.1))))
df.str.baseC <- data.frame(Effect(mod=str.2Y,focal.predictors=c("log.m.base","year"),xlevels=list(log.m.base=seq(0,5,0.1))))
df.str.pbaseC <- data.frame(Effect(mod=str.2Y,focal.predictors=c("log.p.base","year"),xlevels=list(log.p.base=seq(0,5,0.1))))

df.str.spikeC$m.spike <- exp(df.str.spikeC$log.m.spike)
df.str.pspikeC$p.spike <- exp(df.str.pspikeC$log.p.spike)
df.str.baseC$m.base <- exp(df.str.baseC$log.m.base)
df.str.pbaseC$p.base <- exp(df.possig.pbaseC$log.p.base)


df.possig.spike <- data.frame(Effect(mod=possig.2,focal.predictors=c("log.m.spike"),xlevels=list(log.m.spike=seq(0,5,0.1))))
df.possig.pspike <- data.frame(Effect(mod=possig.2,focal.predictors=c("log.p.spike"),xlevels=list(log.p.spike=seq(0,5,0.1))))
df.possig.base <- data.frame(Effect(mod=possig.2,focal.predictors=c("log.m.base"),xlevels=list(log.m.base=seq(0,5,0.1))))
df.possig.pbase <- data.frame(Effect(mod=possig.2,focal.predictors=c("log.p.base"),xlevels=list(log.p.base=seq(0,5,0.1))))

df.str.spike <- data.frame(Effect(mod=str.2,focal.predictors=c("log.m.spike"),xlevels=list(log.m.spike=seq(0.1,5,0.1))))
df.str.pspike <- data.frame(Effect(mod=str.2,focal.predictors=c("log.p.spike"),xlevels=list(log.p.spike=seq(0.1,5,0.1))))
df.str.base <- data.frame(Effect(mod=str.2,focal.predictors=c("log.m.base"),xlevels=list(log.m.base=seq(0.1,5,0.1))))
df.str.pbase <- data.frame(Effect(mod=str.2,focal.predictors=c("log.p.base"),xlevels=list(log.p.base=seq(0.1,5,0.1))))

df.str.spike$mspike <- exp(df.str.spike$log.m.spike)
df.str.pspike$pspike <- exp(df.str.pspike$log.p.spike)
df.str.base$mbase <- exp(df.str.base$log.m.base)
df.str.pbase$pbase <- exp(df.str.pbase$log.p.base)

df.str.spike$lower_ <- replace(df.str.spike$lower,df.str.spike$lower<0.01,0.01)
 
df.possig2.spike <- data.frame(Effect(mod=possig.C2,focal.predictors=c("log.m.spike","year"),xlevels=list(log.m.spike=seq(0,5,0.1))))
df.possig2.pspike <- data.frame(Effect(mod=possig.C2,focal.predictors=c("log.p.spike","year"),xlevels=list(log.p.spike=seq(0,5,0.1))))
df.possig2.base <- data.frame(Effect(mod=possig.C2,focal.predictors=c("log.m.base","year"),xlevels=list(log.m.base=seq(0,5,0.1))))
df.possig2.pbase <- data.frame(Effect(mod=possig.C2,focal.predictors=c("log.p.base","year"),xlevels=list(log.p.base=seq(0,5,0.1))))


mspike.possig.gg <- ggplot()+	geom_ribbon(data=df.possig.spike,aes(ymin=lower,ymax=upper,x=exp(log.m.spike)),alpha=.25)+
		geom_line(size=1.00,data=df.possig.spike,aes(y=fit,x=exp(log.m.spike)))+
		geom_point(size=.40,data=fulleff2,position=position_jitter(height=0.03,width=0.03),aes(y=as.numeric(possig01),x=m.spike))+
		geom_line(data=df.possig.spikeC,aes(y=fit,x=m.spike,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log10")+ylim(-0.1,1.1)+
		xlab("Spike Momentum, media salience")+ylab("Agenda-Setting Effect Likelihood")+
		theme(legend.position=c(0.2,0.8),legend.background=element_rect(fill="transparent"))

pspike.possig.gg <- ggplot()+	geom_ribbon(data=df.possig.pspike,aes(ymin=lower,ymax=upper,x=exp(log.p.spike)),alpha=.25)+
		geom_line(size=1.00,data=df.possig.pspike,aes(y=fit,x=exp(log.p.spike)))+
		geom_point(size=.40,data=fulleff2,position=position_jitter(height=0.03,width=0.03),aes(y=as.numeric(possig01),x=p.spike))+
		geom_line(data=df.possig.pspikeC,aes(y=fit,x=p.spike,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log10")+ylim(-0.1,1.1)+
		xlab("Spike Momentum, public salience")+ylab("Agenda-Setting Effect Likelihood")+
		theme(legend.position=c(0.2,0.8),legend.background=element_rect(fill="transparent"))

mbase.possig.gg <- ggplot()+	geom_ribbon(data=df.possig.base,aes(ymin=lower,ymax=upper,x=exp(log.m.base)),alpha=.25)+
		geom_line(size=1.00,data=df.possig.base,aes(y=fit,x=exp(log.m.base)))+
		geom_point(size=.40,data=fulleff2,position=position_jitter(height=0.03,width=0.03),aes(y=as.numeric(possig01),x=m.base))+
		geom_line(data=df.possig.baseC,aes(y=fit,x=m.base,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log10")+ylim(-0.1,1.1)+xlim(1,100)+
		xlab("Baseline, media salience")+ylab("Agenda-Setting Effect Likelihood")+
		theme(legend.position=c(0.8,0.8),legend.background=element_rect(fill="transparent"))

pbase.possig.gg <- ggplot()+	geom_ribbon(data=df.possig.pbase,aes(ymin=lower,ymax=upper,x=exp(log.p.base)),alpha=.25)+
		geom_line(size=1.00,data=df.possig.pbase,aes(y=fit,x=exp(log.p.base)))+
		geom_point(size=.40,data=fulleff2,position=position_jitter(height=0.03,width=0.03),aes(y=as.numeric(possig01),x=p.base))+
		geom_line(data=df.possig.pbaseC,aes(y=fit,x=p.base,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log10")+ylim(-0.1,1.1)+
		xlab("Baseline, public salience")+ylab("Agenda-Setting Effect Likelihood")+
		theme(legend.position=c(0.8,0.8),legend.background=element_rect(fill="transparent"))

mspike.str.gg <- ggplot()+	geom_ribbon(data=df.str.spike,aes(ymin=exp(lower_),ymax=exp(upper),x=mspike),alpha=.40)+
		geom_line(size=1.00,data=df.str.spike,aes(y=exp(fit),x=mspike))+
		geom_point(size=.40,data=fulleff2,aes(x=I(m.spike),y=exp(logeff3)))+
		geom_line(data=df.str.spikeC,aes(y=exp(fit),x=m.spike,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log", y="log")+xlim(3,100)+ylim(1,200)+
		xlab("Spike Momentum, media salience")+ylab("Agenda-Setting Effect Strength")+
		scale_fill_manual(values=c("#222222","#999999"))+
		theme(legend.position=c(0.2,0.8),legend.background=element_rect(fill="transparent"))

pspike.str.gg <- ggplot()+	geom_ribbon(data=df.str.pspike,aes(ymin=exp(lower),ymax=exp(upper),x=pspike),alpha=.40)+
		geom_line(size=1.00,data=df.str.pspike,aes(y=exp(fit),x=pspike))+
		geom_point(size=.40,data=fulleff2,position=position_jitter(height=0.03,width=0.01),aes(y=exp(logeff3),x=p.spike))+
		geom_line(data=df.str.pspikeC,aes(y=exp(fit),x=p.spike,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log", y="log")+xlim(1,100)+ylim(1,100)+
		xlab("Spike Momentum, public salience")+ylab("Agenda-Setting Effect Strength")+
		scale_fill_manual(values=c("#222222","#999999"))+
		theme(legend.position=c(0.2,0.8),legend.background=element_rect(fill="transparent"))
		
mbase.str.gg <- ggplot()+	geom_ribbon(data=df.str.base,aes(ymin=exp(lower),ymax=exp(upper),x=mbase),alpha=.40)+
		geom_line(size=1.00,data=df.str.base,aes(y=exp(fit),x=mbase))+
		geom_point(size=.40,data=fulleff2,position=position_jitter(height=0.03,width=0.01),aes(y=exp(logeff3),x=m.base))+
		geom_line(data=df.str.baseC,aes(y=exp(fit),x=m.base,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log", y="log")+xlim(1,100)+ylim(1,100)+
		xlab("Baseline, media salience")+ylab("Agenda-Setting Effect Strength")+
		scale_fill_manual(values=c("#222222","#999999"))+
		theme(legend.position=c(0.8,0.8),legend.background=element_rect(fill="transparent"))

pbase.str.gg <- ggplot()+	geom_ribbon(data=df.str.pbase,aes(ymin=exp(lower),ymax=exp(upper),x=pbase),alpha=.40)+
		geom_line(size=1.00,data=df.str.pbase,aes(y=exp(fit),x=pbase))+
		geom_point(size=.40,data=fulleff2,position=position_jitter(height=0.03,width=0.01),aes(y=exp(logeff3),x=p.base))+		
		geom_line(data=df.str.pbaseC,aes(y=exp(fit),x=p.base,linetype=year,group=year))+
		theme_bw()+coord_trans(x="log", y="log")+xlim(1,100)+ylim(1,100)+
		xlab("Baseline, public salience")+ylab("Agenda-Setting Effect Strength")+
		scale_fill_manual(values=c("#222222","#999999"))+
		theme(legend.position=c(0.8,0.8),legend.background=element_rect(fill="transparent"))

str.gg <- grid.arrange(mspike.str.gg,mbase.str.gg,pspike.str.gg,pbase.str.gg,ncol=2)
possig.gg <- grid.arrange(mspike.possig.gg,mbase.possig.gg,pspike.possig.gg,pbase.possig.gg,ncol=2)
spike.gg <- grid.arrange(mspike.possig.gg,pspike.possig.gg,mspike.str.gg,pspike.str.gg,ncol=2)
base.gg <- grid.arrange(mbase.possig.gg,pbase.possig.gg,mbase.str.gg,pbase.str.gg,ncol=2)

ggsave(str.gg,file="strength.svg",width=16,height=16,unit="cm",dpi=1200)
ggsave(str.gg,file="strength.png",width=16,height=16,unit="cm",dpi=1200)
ggsave(str.gg,file="strength.jpg",width=16,height=16,unit="cm",dpi=1200)
ggsave(str.gg,file="strength.pdf",width=16,height=16,unit="cm",dpi=1200)

ggsave(possig.gg,file="possig.svg",width=16,height=16,unit="cm",dpi=1200)
ggsave(possig.gg,file="possig.png",width=16,height=16,unit="cm",dpi=1200)
ggsave(possig.gg,file="possig.jpg",width=16,height=16,unit="cm",dpi=1200)
ggsave(possig.gg,file="possig.pdf",width=16,height=16,unit="cm",dpi=1200)

ggsave(spike.gg,file="spike.svg",width=16,height=16,unit="cm",dpi=1200)
ggsave(spike.gg,file="spike.png",width=16,height=16,unit="cm",dpi=1200)
ggsave(spike.gg,file="spike.jpg",width=16,height=16,unit="cm",dpi=1200)
ggsave(spike.gg,file="spike.pdf",width=16,height=16,unit="cm",dpi=1200)

ggsave(base.gg,file="base.svg",width=16,height=16,unit="cm",dpi=1200)
ggsave(base.gg,file="base.png",width=16,height=16,unit="cm",dpi=1200)
ggsave(base.gg,file="base.jpg",width=16,height=16,unit="cm",dpi=1200)
ggsave(base.gg,file="base.pdf",width=16,height=16,unit="cm",dpi=1200)













