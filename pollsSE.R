library(pollofpolls)

today <- as.character(Sys.Date())

# Read in Swedish Riksdag (Parliment) polling data
wholeSE <- popRead('SE-parliament')
#wholeSE <- read.csv("./polls_SE-parliament_2018-08-10.csv")

# Trim polling data of web based polls (systemic bias with wild variance)
trimmedSE <- wholeSE
trimmedSE$polls <- trimmedSE$polls[trimmedSE$polls$firm != 'YouGov' & 
                                   trimmedSE$polls$firm != 'Sentio' &
                                   trimmedSE$polls$firm != 'United Minds']

# Plot the polls
plot(wholeSE)
plot(wholeSE, xlim=as.Date(c('2017-08-06', '2018-08-02')))

# Add trends to polls
wholeSE <- popAddTrend(wholeSE, name='Kalman 0.003 - Interpolated', type='kalman', args=list(sd = 0.003))
wholeSE <- popAddTrend(wholeSE, name='Kalman 0.003 - Raw', type='kalman', args=list(sd = 0.003), 
                       interpolations=list(lastInterpolation = list()))
wholeSE <- popAddTrend(wholeSE, name='30 Day Weighted Mean', type='weightedMeanLastDays', 
                       args=list(days = 30, maxObs = Inf))

plot(wholeSE, main="Test", xlim=as.Date(c('2017-07-31', today)))

# Add trends to trimmed polls
trimmedSE <- popAddTrend(trimmedSE, name='Kalman 0.003 - Interpolated', type='kalman', args=list(sd = 0.003))
trimmedSE <- popAddTrend(trimmedSE, name='Kalman 0.003 - Raw', type='kalman', args=list(sd = 0.003), 
                         interpolations=list(lastInterpolation = list()))
trimmedSE <- popAddTrend(trimmedSE, name='30 Day Weighted Mean', type='weightedMeanLastDays', 
                            args=list(days = 30, maxObs = Inf))

plot(trimmedSE, xlim=as.Date(c('2017-07-31', today)))


# Examine individual web-based polls for 12 months preceeding 2014 election
YouGov <- wholeSE
YouGov$polls <- YouGov$polls[YouGov$polls$firm == 'YouGov']
Sentio <- wholeSE
Sentio$polls <- wholeSE$polls[wholeSE$polls$firm == 'Sentio']
UnitedMinds <- wholeSE
UnitedMinds$polls <- UnitedMinds$polls[UnitedMinds$polls$firm == 'United Minds']

YouGov <- popAddTrend(YouGov, name='Kalman 0.003 - Interpolated', type='kalman', args=list(sd = 0.003))
YouGov <- popAddTrend(YouGov, name='Kalman 0.003 - Raw', type='kalman', args=list(sd = 0.003), 
                       interpolations=list(lastInterpolation = list()))
YouGov <- popAddTrend(YouGov, name='30 Day Weighted Mean', type='weightedMeanLastDays', 
                         args=list(days = 30, maxObs = Inf))

plot(YouGov, xlim=as.Date(c('2013-09-14', '2014-09-14')))

Sentio <- popAddTrend(Sentio, name='Kalman 0.003 - Interpolated', type='kalman', args=list(sd = 0.003))
Sentio <- popAddTrend(Sentio, name='Kalman 0.003 - Raw', type='kalman', args=list(sd = 0.003), 
                       interpolations=list(lastInterpolation = list()))
Sentio <- popAddTrend(Sentio, name='30 Day Weighted Mean', type='weightedMeanLastDays', 
                         args=list(days = 30, maxObs = Inf))

plot(Sentio, xlim=as.Date(c('2013-09-14', '2014-09-14')))

UnitedMinds <- popAddTrend(UnitedMinds, name='Kalman 0.003 - Interpolated', type='kalman', args=list(sd = 0.003))
UnitedMinds <- popAddTrend(UnitedMinds, name='Kalman 0.003 - Raw', type='kalman', args=list(sd = 0.003), 
                      interpolations=list(lastInterpolation = list()))
UnitedMinds <- popAddTrend(UnitedMinds, name='30 Day Weighted Mean', type='weightedMeanLastDays', 
                      args=list(days = 30, maxObs = Inf))

plot(UnitedMinds, xlim=as.Date(c('2013-09-14', '2014-09-14')))



# Examine aggregate of web-based polls for 12 months preceeding 2014 election
webOnlySE <- wholeSE
webOnlySE$polls <- webOnlySE$polls[webOnlySE$polls$firm == 'YouGov' | 
                                       webOnlySE$polls$firm == 'Sentio' |
                                       webOnlySE$polls$firm == 'United Minds']

webOnlySE <- popAddTrend(webOnlySE, name='Kalman 0.003 - Interpolated', type='kalman', args=list(sd = 0.003))
webOnlySE <- popAddTrend(webOnlySE, name='Kalman 0.003 - Raw', type='kalman', args=list(sd = 0.003), 
                         interpolations=list(lastInterpolation = list()))
webOnlySE <- popAddTrend(webOnlySE, name='30 Day Weighted Mean', type='weightedMeanLastDays', 
                         args=list(days = 30, maxObs = Inf))

plot(webOnlySE, xlim=as.Date(c('2013-09-14', '2014-09-14')))


# Compare to aggregate of non-web based polls for 12 months preceeding 2014 election
plot(trimmedSE, xlim=as.Date(c('2013-09-14', '2014-09-14')))

