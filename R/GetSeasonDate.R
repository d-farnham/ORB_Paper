GetSeasonDate <- function(date, method = 'DJF'){
	require(lubridate)
	month <- lubridate::month(date)
	GetSeasonMonth(month, method)
}


GetSeasonMonth <- function(month, method = 'DJF'){
	require(lubridate)
	if(method == 'DJF'){
		seasons <- c(rep('DJF', 2), rep('MAM', 3), rep('JJA', 3), rep('SON', 3), 'DJF')
	} else if (method == 'JFM') {
		seasons <- c(rep('JFM', 3), rep('AMJ', 3), rep('JAS', 3), rep('OND', 3))
	}
	seasons[month]
}

