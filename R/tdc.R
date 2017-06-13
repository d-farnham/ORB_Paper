## Tail Dependence Coefficient ##
tdc = function(x1, x2, p, upper = FALSE)
{
	x = cbind(x1,x2)
	N = nrow(x)
	
	r <- x                                # initiating the r matrix
	for (i in 1:ncol(x))
	{
		r[,i] <- ecdf(x[,i])(x[,i])          # stores the rank of the data for each column
	}
	lambda.u = sum(rowSums(r>=(p))==2)/sum(r[,2]>=p)
	lambda.l = sum(rowSums(r<=(p))==2)/sum(r[,2]>=p)
	if(upper == FALSE){
		return(lambda.l)}
	if(upper == TRUE){
		return(lambda.u)}
}