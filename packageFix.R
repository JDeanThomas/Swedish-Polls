toLong = function(data, what='polls') {
    value = NULL # WARNINGS
    melt(data[[what]], variable.name = "party", measure.vars=intersect(data$parties$code, colnames(data[[what]])))[!is.na(value)]
}


weightedMeanLastDays = function(data, days = 30, maxObs = Inf) {
    pollsLong = toLong(data)
    trendData = NULL

    setorderv(pollsLong, 'date', order=-1)
    for (i in (min(pollsLong$date):(max(pollsLong$date) + 1))) {
        t = pollsLong[date <= i & date > (i - days) & !is.na(value)]

        if (nrow(t) == 0)
            next

        t[, id := .GRP, by=.(date, firm)]
        t = t[id < maxObs]
        t[, age := -as.integer(date - i)]
        t[, weight := days - age]

        trendData = rbind(trendData,
                          t[, .(value = sum(value*weight)/sum(weight), date = i), by=party])
    }

    return (trendData)
}
