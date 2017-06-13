GetCondProbbySeason = function(RIP_vec, RIP_lag_vec, season_vec, season){
	keep = season_vec == season
	RIP_vec_new = ifelse(keep == TRUE, RIP_vec, NA)
	RIP_lag_vec = ifelse(keep == TRUE, RIP_lag_vec, NA)
	con_ting_table = table(RIP_vec_new, 
												 RIP_lag_vec, deparse.level = 2)
	cond_prob_out = round(con_ting_table[4]/(con_ting_table[3] + con_ting_table[4]),3)
	return(cond_prob_out)
}
