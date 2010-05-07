#################
# Preprocessing #
#################

load <- function(){
	
	# neccesary packages
	require(lattice)
	require(agsemisc)
	require(plotrix)
	require(Hmisc)
	
	# read data in
	# works for call 
	#       Rscript --vannilla /..../script.R file.txt
	data  <- read.delim(commandArgs()[7], comment.char="", quote="")
	
	# transform the booleans to get R to reckon them
	data$user.is.host <- as.character(data$user.is.host)
	data$user.is.host[data$user.is.host == "true"] <- "TRUE"
	data$user.is.host[data$user.is.host == "false"] <- "FALSE"
	data$user.is.host[data$user.is.host == ""] <- NA
	data$user.is.host <- as.logical(data$user.is.host)
	
	data$feedback.disabled <- as.character(data$feedback.disabled)
	data$feedback.disabled[data$feedback.disabled == "true"] <- "TRUE"
	data$feedback.disabled[data$feedback.disabled == "false"] <- "FALSE"
	data$feedback.disabled[data$feedback.disabled == ""] <- NA
	data$feedback.disabled <- as.logical(data$feedback.disabled)
	
	# set the pseudonym to NA if none is set
	data$user.pseudonym <- as.character(data$user.pseudonym)
	data$user.pseudonym[data$user.pseudonym == ""] <- NA
	
	# Remove all internal data 
	# e.g. data statistic data submitted by members of the AG SE, including development or testing versions
	data <- data[!(grepl("(internal|sarosTeam)", data$filename) | grepl("TESTING", data$saros.version)),]
	
	# Remove all data before we know whether somebody is host
	data <- data[!is.na(data$user.is.host),]
	
	# Fix a version glitch 
	data$saros.version[data$saros.version == "9.12.04.r1878"] <- "9.12.4.r1878" 
	
	# Fix missing session.local.start values via filename
	# 2007-01-04T13:24:56.000+01:00
	# it does not make sense to extract the time from the filename as well as this time reflects the
	# time of submission and NOT the starting time, therefore no session length can be calculated for those
	dates <- sapply(data$filename, function(x){
				x <- sub('.*(\\d\\d\\d\\d-\\d\\d-\\d\\d).*', '\\1T00:00:00.000Z', x, perl=T) 
			})
	
	data$session.local.start = as.character(data$session.local.start)   
	data$session.local.start[data$session.local.start == ""] <- dates[data$session.local.start == ""]
	data$session.local.start <- as.POSIXct(strptime(as.character(data$session.local.start), "%Y-%m-%dT%H:%M:%S"))
	
	# get rid of columns where all values are NA
	data <- data[,colSums(is.na(data))<nrow(data)]
	
	print("Data read in...")
	return(data)
}

#################
# Export As CSV #
#################

exportCsv <- function(dataset) {
	# the filename where to write the csv
	filename = "statistics.txt"
	source <- dataset
	# write a tab separated tsv, use tab as seperator
	write.table(source,filename,sep=",",row.names=FALSE,col.names=TRUE)
}

#####################
# PNG Plot Function #
#####################

pngPlot <- function(file, width, height, fun, png=FALSE){
	# plots are saved to a PNG, parameters are listed
	# output file location, width, height, function to be plotted
	# e.g. pngPlot("C:/..", 6, 5, function(){ ... })
	png(paste(file, ".png", sep=""), width=width*100, height=height*100)
	fun()
	dev.off()

}

#################
# Aux Functions #
#################

buildWeekLabels <- function(dateFactor){	
	# return a list of labels for all weeks found in the given factor
	sapply(levels(dateFactor), function(x){ 
				y = as.POSIXct(x)
				week = format(y, "%W")
			})
}

buildMonthLabels <- function(dateFactor, trimMonth=3){
	# return a list of labels for all months found in the given factor
	sapply(levels(dateFactor), function(x){ 
				y = as.POSIXct(x)
				month = substr(format(y, "%b"), 1, trimMonth)
				if (as.POSIXlt(x)$mon == 5) 
					paste(month, format(y, "%Y"), sep="\n")
				else 
					month
			})
}

#######################
# General Information #
#######################

plot_DownloadsPerMonth <- function(){
	# hardcoded vector containing the number of downloads according to month vector below
	download <- c( 0, 87, 129, 182, 548, 726, 1247, 1696, 3042, 4131, 8311, 6655, 6359, 9625, 16768)
	
	# periods as enumerated type
	months <- factor(c(
					"2009-01-01", 
					"2009-02-01", 
					"2009-03-01", 
					"2009-04-01", 
					"2009-05-01", 
					"2009-06-01", 
					"2009-07-01", 
					"2009-08-01", 
					"2009-09-01", 
					"2009-10-01",
					"2009-11-01", 
					"2009-12-01",
					"2010-01-01",
					"2010-02-01",
					"2010-03-01")) 
	
	
	labels = buildMonthLabels(months)
	
	monthNumeric <- as.numeric(months)
	
	print(
			barchart(
					download ~ monthNumeric,
					horizontal=F,
					xlab="",
					ylab="Downloads per month",
					xlim=c(0, max(monthNumeric) + 1), 
					ylim=c(0, max(download)*1.1),
					stack=T,
					col = hcl(h = seq(0, 240, by = 20)),
					panel=function(...){
						panel.grid(h=-1,v=max(monthNumeric) + 1 - min(monthNumeric))
						panel.barchart(...)
					},
					scales = list(
							axs="i", alternating=1, 
							x = list(
									at=min(monthNumeric):max(monthNumeric), 
									labels=labels)),
			))
}

plot_SessionsPerWeek <- function(dataset){
	week = cut(dataset$session.local.start, "week")
	year = cut(dataset$session.local.start, "year")
	
	sessions = aggregate(dataset$filename, list(week=week), length)
	
	sessions$week <- factor(as.character(sessions$week))
	labels = buildWeekLabels(sessions$week)
	
	weekNumeric <- as.numeric(sessions$week)
	
	print(
			barchart(
					x ~ weekNumeric,
					horizontal=F,
					xlab="Month",
					ylab="Sessions per week",
					xlim=c(0, max(weekNumeric) + 1), 
					ylim=c(0, max(sessions$x)*1.1),
					data=sessions,
					stack=T,
					col = hcl(h = seq(0, 240, by = 20)),
					panel=function(...){
						panel.grid(h=-1,v=max(weekNumeric) + 1 - min(weekNumeric))
						panel.barchart(...)
					},
					scales = list(
							axs="i", alternating=1, 
							x = list(
									at=min(weekNumeric):max(weekNumeric), 
									labels=labels)),
			))
}

plot_SessionsPerWeekAndVersion <- function(dataset){
	# Only look at the each session once
	dataset = dataset[dataset$user.is.host,]
	
	# Only look at sessions where a user was added
	dataset = dataset[dataset$session.users.total > 1,]
	
	week = cut(dataset$session.local.start, "week")
	year = cut(dataset$session.local.start, "year")
	
	sessions = aggregate(dataset$filename, list(week=week, version=dataset$saros.version), length)
	
	sessions$version <- factor(sub('(\\d+\\.\\d+.\\d+)\\.(r\\d+)', '\\2-\\1', as.character(sessions$version), perl=T))
	
	print(levels(sessions$version))
	
	sessions$week <- factor(as.character(sessions$week))
	labels = buildWeekLabels(sessions$week)
	
	weekNumeric <- as.numeric(sessions$week)
	
	print(
			barchart(
					x ~ weekNumeric,
					horizontal=F,
					groups=version,
					par.settings = simpleTheme(col=rainbow(length(levels(as.factor(sessions$version))))),
					xlab="Week in 2009",
					ylab="Sessions per week",
					xlim=c(0, max(weekNumeric) + 1), 
					ylim=c(0, max(sessions$x)*1.1),
					data=sessions,
					stack=T,
					auto.key = list(
							points = FALSE, 
							rectangles = TRUE, 
							corner = c(0.025,0.95)
					# space = "right"
					),
					# col=c("green"),
					panel=function(...){
						panel.grid(h=-1,v=max(weekNumeric) + 1 - min(weekNumeric))
						panel.barchart(...)
					},
					scales = list(
							axs="i", alternating=1, 
							x = list(
									at=min(weekNumeric):max(weekNumeric), 
									labels=labels)),
			)
	)
}

plot_SessionsPerWeekAndEclipseVersion <- function(dataset){
	
	# Only look at the each session once
	dataset = dataset[dataset$user.is.host,]
	
	# Only look at sessions where a user was added
	dataset = dataset[dataset$session.users.total > 1,]
	
	week = cut(dataset$session.local.start, "week")
	year = cut(dataset$session.local.start, "year")
	
	sessions = aggregate(dataset$filename, list(week=week, version=dataset$eclipse.version), length)
	
	sessions$version <- factor(sub('(\\d+\\.\\d+.\\d+)\\.(r\\d+)', '\\2-\\1', as.character(sessions$version), perl=T))
	
	print(levels(sessions$version))
	
	sessions$week <- factor(as.character(sessions$week))
	labels = buildWeekLabels(sessions$week)
	
	weekNumeric <- as.numeric(sessions$week)
	
	print(
			barchart(
					x ~ weekNumeric,
					horizontal=F,
					groups=version,
					par.settings = simpleTheme(col=rainbow(length(levels(as.factor(sessions$version))))),
					xlab="Week in 2009/2010",
					ylab="Sessions per week",
					xlim=c(0, max(weekNumeric) + 1), 
					ylim=c(0, max(sessions$x)*1.1),
					data=sessions,
					stack=T,
					auto.key = list(
							points = FALSE, 
							rectangles = TRUE, 
							corner = c(0.025,0.95)
					),
					panel=function(...){
						panel.grid(h=-1,v=max(weekNumeric) + 1 - min(weekNumeric))
						panel.barchart(...)
					},
					scales = list(
							axs="i", alternating=1, 
							x = list(
									at=min(weekNumeric):max(weekNumeric), 
									labels=labels)),
			)
	)
}

plot_OS <- function(dataset){
	# only include one single session for each unique user ID 
	dataset <- subset(dataset,!duplicated(dataset$random.user.id))
	sessions <- aggregate(dataset$filename, list(os.name=dataset$os.name), length)
	
	print(
			barchart(
					os.name ~ x,
					par.settings = simpleTheme(col=rainbow(length(levels(as.factor(sessions$os.name))))),
					xlab="Number of Sessions",
					ylab="Operating System",
					xlim=c(0, max(sessions$x) + 1), 
					data=sessions,
					col = hcl(h = seq(60, 480, by = 40)),
					panel.text("suu"),
					auto.key = list(
							points = FALSE, 
							rectangles = TRUE, 
							corner = c(0.025,0.95)
					)
			)
	)
}

plot_OSgroupWindows <- function(dataset){
	# only include one single session for each unique user ID 
	dataset <- subset(dataset,!duplicated(dataset$random.user.id))
	
	# group all systems including "Windows"
	levels(dataset$os.name)[grepl("Windows", levels(dataset$os.name))] <- "Windows"
	sessions = aggregate(dataset$filename, list(os.name=dataset$os.name), length)
	
	print(
			barchart(
					os.name ~ x,
					xlab = "Number of Sessions",
					ylab = "Operating System",
					xlim = c(0, max(sessions$x) *1.1), 
					col = hcl((h = seq(0,120, by = 40))),
					data = sessions,
					auto.key = list(
							points = FALSE, 
							rectangles = TRUE, 
							corner = c(0.025,0.95)
					)
			)
	)
}

plot_FeedbackDisabled <- function(dataset){
	dataset = dataset[dataset$session.count == 1,]
	
	mytable <- table(dataset$feedback.disabled)
	lbls <- paste(names(mytable), "\n", mytable, sep="")
	print(
			pie3D(	mytable, 
					labels = lbls,
					explode = 0.3,
					main="Feedback disabled at first session started?")
	)
}

plot_averageCommunicationUsage <- function(dataset) {
	
	g <- dataset$data_transfer.IBB.total_size_kb
	# remove all NA values
	g <- g[!is.na(g)]
	# remove NULL values
	g <- g[!sapply(g,is.null)]
	# remove all values that are "0"
	g <- g[!g == 0]  
	
	h <- dataset$data_transfer.Jingle.TCP.total_size_kb
	# remove all NA values
	h <- h[!is.na(h)]
	# remove NULL values
	h <- h[!sapply(h,is.null)]
	# remove all values that are "0"
	h <- h[!h == 0] 
	
	i <- dataset$data_transfer.Jingle.UDP.total_size_kb
	# remove all NA values
	i <- i[!is.na(i)]
	# remove NULL values
	i <- i[!sapply(i,is.null)]
	# remove all values that are "0"
	i <- i[!i == 0] 
	
	
	# get mean of data_transfer.IBB.total_size_kb
	gm <- mean(g)
	hm <- mean(h)
	im <- mean(i)
	
	avgThr <- matrix(c(gm,hm,im),ncol=3,byrow=FALSE)
	colnames(avgThr) <- c("IBB","Jingle-TCP","Jingle-UDP")
	rownames(avgThr) <- c("average throughput")
	avgThr <- as.table(avgThr)
	
	barplot(avgThr,
			col = hcl(h = seq(0, 120, by = 20)),
			las = 1,
			ylab = "Total KB")	
}

plot_IbbEventsPerSession <- function(dataset) {
	# Only look at sessions where a user was added
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	
	mytable <- table(dataset$data_transfer.IBB.number_of_events)
	print(mytable)
	lbls <- paste(names(mytable), "\n", mytable, sep="")
	
	print(
			barplot(
					mytable,
					col = hcl(h = seq(0, 240, by = 40)),
					las = 1,
					xlab = "Number of IBB events",
					ylab = "Sessions"
			)
	)	
}

################
# Role Changes #
################

plot_RoleChanges <- function(dataset){
	# include only sessions where more than 1 user participated and which lasted not longer than 4 hours
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 320,]
	
	# This would give the number of session with just one user
	# sessionsOneUser <- NROW(dataset[dataset$session.users.total == 1,])
	
	sessions = aggregate(dataset$filename, list(role.changes=dataset$role.changes), length)
	sessions$role.changes <- as.character(sessions$role.changes)
	
	print(
			barchart(
					role.changes ~ x,
					data=sessions,
					col = hcl((h = seq(0,120, by = 20))),
					xlab="Number of Sessions",
					ylab="Role Changes",
					main="Total Role Changes" 
			)
	)
}

plot_RoleChangesBoxes <- function(dataset) {
	# include sessions of at least 3 minutes, less than 5 hours and with at least one user in
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time > 3,]
	dataset = dataset[dataset$session.time < 300,]
	
	print(dataset$role.changes)
	print(
			bwplot(
					role.changes ~ session.time, 
					data = dataset,
					ylab = "Role Changes", 
					xlab = "Session Duration",
					main = "Role Changes Vs. Duration",
					panel=panel.bwstrip,
					varwidth = TRUE,
					col = "red", 
					cex = 0.4,
					pch = 1,
					type="strip,N,grid,mean",
					strip.limit = 125,
					box.ratio=20,
					levels.fos = NULL	
			)
	)
}

plot_UsersPerRoleChanges <- function(dataset) {
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time > 3,]
	dataset = dataset[dataset$session.time < 300,]
	print(
			mosaicplot(~session.users.total+role.changes, 
					data = dataset,
					color = hcl((h = seq(0,500, by = 100))), 
					shade = FALSE,
					cex.axis = 1.2, 
					las = par("las"),
					ylab = "Role Changes",
					xlab = "Users",
					main = "Users Per Role Change",
					na.action = stats::na.omit
			)
	)
}

plot_RoleChangesVsTime <- function(dataset) 
{
	
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time > 3,]
	dataset = dataset[dataset$session.time < 300,]
	
	print(
			xyplot(
					role.changes ~ session.time,
					data=dataset,
					cex = .4,
					pch = 1,
					xlab="Length of session in minutes",
					ylab="Number of role change"
			)
	)
}

plot_RoleChangesPie <- function(dataset) {
	# only look at sessions where user was host and had more than one folk in
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 320,]
	
	mytable <- table(dataset$role.changes)
	print(mytable)
	lbls <- paste(names(mytable), "\n", mytable, sep="")
	print(
			pie3D(	mytable, 
					labels = lbls,
					main="Role changes",
					explode = 0.1)	
	)
}

###################
# Users / Session #
###################

plot_UsersPerSession <- function(dataset) {
	dataset <- dataset[dataset$user.is.host,]
	sessions <- aggregate(dataset$filename, list(session.users.total=dataset$session.users.total), length)
	
	print(
			barchart(
					session.users.total ~ x,
					xlab="Number of Sessions",
					ylab="Number of Users",
					log = "x",
					xlim=c(0, max(sessions$x) *1.05),
					col = hcl((h = seq(0,240, by = 60))),
					data=sessions,
					auto.key = list(
							points = FALSE, 
							rectangles = TRUE, 
							corner = c(0.025,0.95)
					)
			)
	)
}

plot_UsersPerSessionPie <- function(dataset) {
	dataset = dataset[dataset$user.is.host == TRUE,]	
	mytable <- table(dataset$session.users.total)
	lbls <- paste(names(mytable), "\n", mytable, sep="")
	print(
			pie3D(	mytable, 
					labels = lbls,
					main="Users per Session",
					explode = 0.1)	
	)
}

plot_SessionCount <- function(dataset) {
	#countAggregated = aggregate(dataset$filename, list(session.count=dataset$session.count), length)
	#print(countAggregated)
	#dataset <- dataset[dataset$user.is.host,]
	sessions = aggregate(dataset$filename, list(session.count=dataset$session.count), length)
	
	mytable <- table(dataset$session.count)
	print(mytable)
	lbls <- paste(names(mytable), "\n", mytable, sep="")
	
	print(
			barplot(
					mytable,
					col = hcl(h = seq(0, 240, by = 5)),
					las = 1,
					ylab = "Number of Sessions",
					xlab = "Session Count",
			)
	)
}

####################
# Session Duration #
####################

plot_SessionDuration <- function(dataset){
	
	dataset = dataset[dataset$user.is.host,]
	#dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	
	sessions <- dataset$session.time
	
	print(
			dotchart2(
					sessions,
					main="Session Duration", 
					cex=0.8,
					lines = TRUE,
					dotsize = 0.5
			)
	
	)
	
}

plot_SessionDurationBox <- function(dataset){
	
	#dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 60,]
	dataset = dataset[dataset$session.time > 3,]
	
	sessions <- dataset$session.time	
	isHost = dataset$user.is.host == TRUE
	a.resetplotparams()
	
	
	print(
			bwplot(
					isHost ~ dataset$session.time,
					data = dataset,
					scattered = FALSE,
					main="Session Duration",
					xlab = "Duration in minutes",
					ylab = "User is Host",
					panel=panel.bwstrip,
					varwidth = TRUE,
					col = "red", 
					pch = 1,
					type="mean,strip,density,N,grid",
					strip.limit = 311,
					box.ratio=20,
					levels.fos = NULL
			
			)
	
	
	)
	
}

plot_SessionDurationPie <- function(dataset) {
	
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 300,]
	
	
	sessionsLess5m <- dataset[dataset$session.time <= 5,]
	
	sessionsLess15m <- dataset[dataset$session.time <= 15,]
	sessionsLess15m <- sessionsLess15m[sessionsLess15m$session.time > 5,]
	
	sessionsLess30m <- dataset[dataset$session.time > 15,]
	sessionsLess30m <- sessionsLess30m[sessionsLess30m$session.time <= 30,]
	
	sessionsLess45m <- dataset[dataset$session.time > 30,]
	sessionsLess45m <- sessionsLess45m[sessionsLess45m$session.time <= 45,]
	
	sessionsLess60m <- dataset[dataset$session.time > 45,]
	sessionsLess60m <- sessionsLess60m[sessionsLess60m$session.time <= 60,]	
	
	sessionsGreater60m <- dataset[dataset$session.time > 60,]
	
	m <-c(NROW(sessionsLess5m),NROW(sessionsLess15m),NROW(sessionsLess30m),NROW(sessionsLess45m),NROW(sessionsLess60m),NROW(sessionsGreater60m))
	
	lbls <- c("<5", "<15", "<30", "<45", "<60", ">60")
	
	print(
			pie3D(
					m,
					labels = lbls,
					main="Session duration in minutes",
					col=rainbow(length(lbls)),
					explode=0.1
			)	
	)			
}

plot_SessionDurationBar <- function(dataset) {
	
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 300,]
	
	
	sessionsLess5m <- dataset[dataset$session.time <= 5,]
	
	sessionsLess15m <- dataset[dataset$session.time <= 15,]
	sessionsLess15m <- sessionsLess15m[sessionsLess15m$session.time > 5,]
	
	sessionsLess30m <- dataset[dataset$session.time > 15,]
	sessionsLess30m <- sessionsLess30m[sessionsLess30m$session.time <= 30,]
	
	sessionsLess45m <- dataset[dataset$session.time > 30,]
	sessionsLess45m <- sessionsLess45m[sessionsLess45m$session.time <= 45,]
	
	sessionsLess60m <- dataset[dataset$session.time > 45,]
	sessionsLess60m <- sessionsLess60m[sessionsLess60m$session.time <= 60,]	
	
	sessionsLess75m <- dataset[dataset$session.time > 60,]
	sessionsLess75m <- sessionsLess75m[sessionsLess75m$session.time <= 75,]	
	
	sessionsLess90m <- dataset[dataset$session.time > 75,]
	sessionsLess90m <- sessionsLess90m[sessionsLess90m$session.time <= 90,]	
	
	sessionsLess105m <- dataset[dataset$session.time > 90,]
	sessionsLess105m <- sessionsLess105m[sessionsLess105m$session.time <= 105,]	
	
	sessionsLess120m <- dataset[dataset$session.time > 105,]
	sessionsLess120m <- sessionsLess120m[sessionsLess120m$session.time <= 120,]	
	
	sessionsGreater <- dataset[dataset$session.time > 120,]
	
	duration <- matrix(c(NROW(sessionsLess5m),
					NROW(sessionsLess15m),
					NROW(sessionsLess30m),
					NROW(sessionsLess45m),
					NROW(sessionsLess60m),
					NROW(sessionsLess90m),
					NROW(sessionsLess105m),
					NROW(sessionsLess120m),
					NROW(sessionsGreater)),ncol=9,byrow=FALSE)
	colnames(duration) <- c("<5", "<=15", "<=30", "<=45", "<=60", "<=90","<=105","<=120",">120")
	rownames(duration) <- c("Number of Sessions")
	duration <- as.table(duration)
	
	
	print(
			barplot(
					duration,
					col = hcl(h = seq(0, 220, by = 20)),
					las = 1,
					ylab = "Length",
					main="Session duration in minutes"
			)	
	)			
}

plot_SessionDurationBW <- function(dataset){
	
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 300,]
	
	moreThan50Edits = dataset$textedits.count > 50
	
	print(
			bwplot(
					moreThan50Edits ~ session.time,
					data=dataset,
					xlab="Length of session",
					ylab="More than 50 edits in session",
					panel=panel.bwstrip,
					type="mean,strip,density,N"
			)
	)
}

######################
# Edits / Char Edits #
######################

plot_LocalEdits <- function(dataset){
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	dataset = dataset[dataset$textedits.chars < 100000]
	
	print(
			bwplot(
					~ textedits.count,
					data=dataset,
					xlab="Text Edits", panel=panel.bwstrip,
					type="mean,strip,density,N"
			)
	)
}

plot_LocalChars <- function(dataset){	
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	
	print(
			bwplot(
					~ textedits.chars,
					data=dataset,
					xlab="Chars", 
					panel=panel.bwstrip,
					type="mean,strip,density,N"
			)
	)
}

plot_SessionDurationVsLocalEdits <- function(dataset){
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	
	print(
			xyplot(
					textedits.count ~ session.time,
					data=dataset,
					xlab="Length of session in minutes",
					ylab="Number of text edits"
			)
	)
}

plot_NonParallelEditsVsEditsCharBased <- function(dataset){
	
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$textedits.count > 20 ,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,] 
	dataset = dataset[dataset$textedits.nonparallel.chars < 5000]
	
	print(
			xyplot(
					main="Non Parallel Edits Vs Edits",
					textedits.chars ~ textedits.nonparallel.chars,
					data=dataset,
					abs(1),
					xlab="Number of non parallel edits(chars)",
					ylab="Number of char text edits(chars)"
			)
	)
}

plot_NonParallelEditsPerSession <- function(dataset){
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 300,]
	
	Over50PercentNonParallelEdits = dataset$textedits.nonparallel.percent > 50
	
	print(
			bwplot(
					Over50PercentNonParallelEdits ~ textedits.count,
					data=dataset,
					xlab="Textedits per session",
					ylab="> 50Percent non parallel edits in session",
					panel=panel.bwstrip,
					type="mean,strip,density,N"
			)
	)	
}

plot_DegreeOfParallelism <- function(dataset) {
	# Only look at sessions where a user was added
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	dataset <- dataset[dataset$user.is.host,]
	
	# aggregate data in respect to percentage of concurrent edits
	sessions = aggregate(dataset$filename, list(textedits.nonparallel.percent=dataset$textedits.nonparallel.percent), length)
	sessions$textedits.nonparallel.percent <- as.character(sessions$textedits.nonparallel.percent)
	
	print(
			barchart(
					textedits.nonparallel.percent ~ x,
					data=sessions,
					xlab="Number of Sessions",
					ylab="% of non parallel changes"
			)
	)		
}

plot_IntervalsOfConcurrentEdits <- function(dataset) {
	
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 300,]
	
	interval1 = aggregate(dataset$filename, list(textedits.parallel.interval.1.percent=dataset$textedits.parallel.interval.1.percent), length)
	interval1$textedits.parallel.interval.1.percent <- as.character(interval1$textedits.parallel.interval.1.percent)
	
	interval2 = aggregate(dataset$filename, list(textedits.parallel.interval.2.percent=dataset$textedits.parallel.interval.2.percent), length)
	interval2$textedits.parallel.interval.2.percent <- as.character(interval2$textedits.parallel.interval.2.percent)
	
	interval5 = aggregate(dataset$filename, list(textedits.parallel.interval.5.percent=dataset$textedits.parallel.interval.5.percent), length)
	interval5$textedits.parallel.interval.5.percent <- as.character(interval5$textedits.parallel.interval.5.percent)
	
	interval10 = aggregate(dataset$filename, list(textedits.parallel.interval.10.percent=dataset$textedits.parallel.interval.10.percent), length)
	interval10$textedits.parallel.interval.10.percent <- as.character(interval10$textedits.parallel.interval.10.percent)
	
	interval15 = aggregate(dataset$filename, list(textedits.parallel.interval.15.percent=dataset$textedits.parallel.interval.15.percent), length)
	interval15$textedits.parallel.interval.15.percent <- as.character(interval15$textedits.parallel.interval.15.percent)
	
	customdata <- c(interval1, interval2, interval5, interval10, interval15)
	
	#print(customdata)
	
}

plot_NonParallelCharsPerSession <- function(dataset) {
	# only look at sessions once and where there were at least 2 participators
	dataset = dataset[dataset$user.is.host,]	
	dataset = dataset[dataset$session.users.total > 1,]
	# non parallel char edits
	nonParaChars <- dataset$textedits.nonparallel.chars
	# remove all NA values
	nonParaChars <- nonParaChars[!is.na(nonParaChars)]
	
	# plot number of non parallel chars per session
	
	plot(nonParaChars, 
			xlab="sessions", 
			ylab="non parallel chars", 
			main="non parallel chars per session", 
			pch=15, 
			col="blue")
}

plot_NonParallelPercentageVsChars <- function(dataset) {
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	
	print(
			xyplot(
					textedits.nonparallel.percent ~ textedits.nonparallel.chars,
					data=dataset,
					xlab="Non Parallel Chars",
					ylab="Non Parallel Percentage",
					auto.key = FALSE,
					aspect = "fill"
			
			)
	)
}

plot_LocalEditsVsChars <- function(dataset) {
	dataset = dataset[dataset$user.is.host,]
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 240,]
	
	print(
			xyplot(
					textedits.chars ~ textedits.count,
					data=dataset,
					xlab="Number of Edits",
					ylab="Number of Chars",
					auto.key = FALSE,
					aspect = "fill",
					cex=1			
			)
	)
}

#####################
# Role Distribution #
#####################

plot_DriverRatioBW <- function(dataset) {
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 300,]
	
	host = dataset$user.is.host == TRUE
	
	print(
			bwplot(
					host ~ role.driver.percent,
					data=dataset,
					xlab="Time in driver role",
					ylab="User is host",
					panel=panel.bwstrip,
					type="mean,strip,density,N"
			)
	)
}

plot_ObserverRatioBW <- function(dataset) {
	dataset = dataset[dataset$session.users.total > 1,]
	dataset = dataset[dataset$session.time < 300,]
	
	host = dataset$user.is.host == TRUE
	
	print(
			bwplot(
					host ~ role.observer.percent,
					data=dataset,
					xlab="Time in driver role",
					ylab="User is host",
					panel=panel.bwstrip,
					type="mean,strip,density,N"
			)
	)
}

###########################
# Call Plotting Functions #
###########################

makePlots <- function() {
	print("Plotting...")
	
	#################
	# General Stuff #
	#################
	
	pngPlot((file=paste(getwd(), "plots/downloads", sep="/")), 6, 3, plot_DownloadsPerMonth)
	
	pngPlot((file=paste(getwd(), "plots/sessionsPerWeekAndVersion", sep="/")), 8, 6, function(){
				plot_SessionsPerWeekAndVersion(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/sessionsPerWeek", sep="/")), 8, 6, function(){
				plot_SessionsPerWeek(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/OS", sep="/")), 8, 3, function(){
				plot_OS(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/OS(WindowsGrouped)", sep="/")), 8, 4, function(){
				plot_OSgroupWindows(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/sessionsPerWeekAndEclipseVersion", sep="/")), 8, 6, function(){
				plot_SessionsPerWeekAndEclipseVersion(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/iBBEventsPerSession", sep="/")), 8, 4, function(){
				plot_IbbEventsPerSession(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/feedbackPie", sep="/")), 8, 3, function(){
				plot_FeedbackDisabled(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/averageThroughput", sep="/")), 8, 3, function(){
				plot_averageCommunicationUsage(data)
			})
	
	################
	# Role Changes #
	################
	
	pngPlot((file=paste(getwd(), "plots/roleChangesPerUser", sep="/")), 8, 8, function(){
				plot_UsersPerRoleChanges(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/roleChanges", sep="/")), 8, 4, function(){
				plot_RoleChanges(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/roleChangesVsTime", sep="/")), 8, 4, function(){
				plot_RoleChangesVsTime(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/roleChangesPie", sep="/")), 8, 3, function(){
				plot_RoleChangesPie(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/roleChangesBoxes", sep="/")), 9,4 , function(){
				plot_RoleChangesBoxes(data)
			})
	
	#####################
	# Role Distribution #
	#####################
	
	pngPlot((file=paste(getwd(), "plots/driverRatioBW", sep="/")), 8, 3, function(){
				plot_DriverRatioBW(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/observerRatioBW", sep="/")), 8, 3, function(){
				plot_ObserverRatioBW(data)
			})
	
	###################
	# Users / Session #
	###################
	
	pngPlot((file=paste(getwd(), "plots/usersPerSession", sep="/")), 8, 4, function(){
				plot_UsersPerSession(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/usersPerSessionPie", sep="/")), 8, 3, function(){
				plot_UsersPerSessionPie(data)
			})
	
	pngPlot( (file=paste(getwd(), "plots/sessionCount", sep="/")), 10, 8, function(){
				plot_SessionCount(data)
			})
	
	####################
	# Session Duration #
	####################
	
	pngPlot((file=paste(getwd(), "plots/SessionDuration(over50edits)", sep="/")), 8, 5, function(){
				plot_SessionDurationBW(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/sessionDuration2", sep="/")), 8, 3, function(){
				plot_SessionDuration(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/sessionDurationPie", sep="/")), 8, 3, function(){
				plot_SessionDurationPie(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/sessionDurationBar", sep="/")), 8, 3, function(){
				plot_SessionDurationBar(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/sessionDurationBox", sep="/")), 20,15, function(){
				plot_SessionDurationBox(data)
			})
	
	
	#################
	# Edits / Chars #
	#################
	
	pngPlot((file=paste(getwd(), "plots/sessionDurationVsLocalEdits", sep="/")), 8, 6, function(){
				plot_SessionDurationVsLocalEdits(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/localEdits", sep="/")), 8, 3, function(){
				plot_LocalEdits(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/localChars", sep="/")), 8, 3, function(){
				plot_LocalChars(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/degreeOfParallelism", sep="/")), 8, 8, function(){
				plot_DegreeOfParallelism(data)
			})	
	
	pngPlot((file=paste(getwd(), "plots/localEditsVsChars", sep="/")), 8, 3, function(){
				plot_LocalEditsVsChars(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/nonParallelEditsVsCharEdits", sep="/")), 8, 6, function(){
				plot_NonParallelEditsVsEditsCharBased(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/nonParallelEditsPerSession", sep="/")), 8, 3, function(){
				plot_NonParallelEditsPerSession(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/nonParallelCharsPerSession", sep="/")), 8, 8, function(){
				plot_NonParallelCharsPerSession(data)
			})
	
	pngPlot((file=paste(getwd(), "plots/nonParallelPercentageVsChars", sep="/")), 8, 8, function(){
				plot_NonParallelPercentageVsChars(data)
			})	

}

################# 
# Main programm #
#################

# call processing of data
data = load()

# exports the data once processed to a *.csv file
# exportCsv(data)

# do the plotting
makePlots()
