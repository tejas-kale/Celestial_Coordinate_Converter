val_separator <- function(val) {
	if(any(grep("[a-zA-Z]", val))) 
		val_wo_chars <- as.numeric(unlist(strsplit(val, "[a-zA-Z]")))
	else if(any(grep(" ", val, fixed=TRUE)))
		val_wo_chars <- as.numeric(unlist(strsplit(val, " ", fixed=TRUE)))
	else if(any(grep(":", val, fixed=TRUE)))
		val_wo_chars <- as.numeric(unlist(strsplit(val, ":", fixed=TRUE)))

	if(length(val_wo_chars != 3))
                        val_wo_chars <- c(val_wo_chars, rep(0, 3-length(val_wo_chars)))
        return(val_wo_chars)
}



format_num <- function(col, sig_digs) {
	if (is.numeric(col))
		sprintf(paste("%1.", sig_digs, "f", sep=""), col)
	else
		col
}



read_data <- function(in_file, is_header, disp) {
	# Guess delimiter 
	delims_guess_data <- readLines(in_file, n=5)
	delims_set <- c(",", ";", "\t", "|")
	poss_delims <- lapply(delims_guess_data, FUN=function(line_str) sapply(delims_set, FUN=function(delim) grepl(delim, line_str, fixed=TRUE)))
	poss_delims <- as.data.frame(do.call("rbind", poss_delims))
	poss_delims <- apply(poss_delims, 2, FUN=function(col) all(col))

	# Check for more than 1 delimiter
	if(sum(poss_delims)>1)
		stop("More than 1 delimiter found in the data set. Please check your file.") else
	if(sum(poss_delims)==0)
		stop("Input data can only have the following delimiters: , ; \t |") else
	delim_guess <- delims_set[which(poss_delims, arr.ind=TRUE)]
	
	# Read data
	ra_dec_data <- read.table(in_file, sep=delim_guess, comment.char="#", header=is_header, stringsAsFactors=FALSE)
	
	if(disp)
		as.data.frame(lapply(ra_dec_data, FUN=function(col) format_num(col, 4)))
	else
		ra_dec_data
}



ra_dec_format <- function(data_) {
	data_class <- sapply(data_, class)
	if(all(data_class=="numeric"))
		return("Degree")
	else if(all(data_class=="character"))
		return("Time")
	else
		return("Unrecognisable format")
}



deg_to_time <- function(ra_dec_deg_df, col_names, out_file, out_sep, out_form) {
	to_time <- function(val, val_type, out_form="hms") {
		if(val_type=="DEC") {
			hour <- as.integer(val)
			mins <- round(((abs(val)-abs(as.integer(val)))*60), 2)
			secs <- round(((mins-as.integer(mins)))*60, 2)
		} else
		if(val_type=="RA") {
			hour <- as.integer(val/15)
			mins <- as.integer(((val/15)-hour)*60)
			secs <- round(((((val/15)-hour)*60)-mins)*60, 2)
		}

		val_time <- switch(out_form,
					hms=paste(hour, ifelse(val_type=="RA", "h", "d"), mins, "m", secs, "s", sep=""),
					colon=paste(c(hour, mins, secs), collapse=":", sep=""),
					space=paste(c(hour, mins, secs)))
		return(val_time)
	}

	ra_time <- lapply(ra_dec_deg_df$RA, FUN=function(ra_val) to_time(ra_val, "RA", out_form))
	ra_time <- do.call("rbind", ra_time)
	if(ncol(ra_time)>1)
		ra_time <- apply(ra_time, 1, FUN=function(r) paste(r, collapse=" "))
	dec_time <- lapply(ra_dec_deg_df$DEC, FUN=function(dec_val) to_time(dec_val, "DEC", out_form))
	dec_time <- do.call("rbind", dec_time)
	if(ncol(dec_time)>1)
		dec_time <- apply(dec_time, 1, FUN=function(r) paste(r, collapse=" "))
	ra_dec_time_df <- as.data.frame(cbind(ra_time, dec_time))
	names(ra_dec_time_df) <- c("RA", "DEC")
	row.names(ra_dec_time_df) <- 1:nrow(ra_dec_time_df)

#	write.table(ra_dec_time_df, file=out_file, sep=out_sep, row.names=FALSE, col.names=col_names, quote=FALSE)

	ra_dec_time_df
}



time_to_deg <- function(ra_dec_time_df, col_names, out_file, out_sep, num_sig_dig) {
	ra_dec_split_bychar_list <- lapply(ra_dec_time_df, FUN=function(data_col) sapply(data_col, FUN=function(col_val) val_separator(col_val)))
	ra_dec_split_bychar_list <- lapply(ra_dec_split_bychar_list, FUN=function(var_mat) t(as.data.frame(var_mat)))
	names(ra_dec_split_bychar_list) <- c("RA", "DEC")

	ra_deg <- apply(ra_dec_split_bychar_list$RA, 1, FUN=function(ra_val) abs(ra_val[1]*15) + (ra_val[2]/4) + (ra_val[3]/240))
	dec_deg <- apply(ra_dec_split_bychar_list$DEC, 1, FUN=function(dec_val) sign(dec_val[1]) * ((abs(dec_val[1]) + (dec_val[2]/60) + (dec_val[3]/3600))))
	ra_dec_deg_df <- as.data.frame(cbind(ra_deg, dec_deg))
	names(ra_dec_deg_df) <- c("RA", "DEC")
	row.names(ra_dec_deg_df) <- 1:nrow(ra_dec_deg_df)

#	write.table(round(ra_dec_deg_df, num_sig_dig), file=out_file, sep=out_sep, row.names=FALSE, col.names=col_names, quote=FALSE)

	ra_dec_deg_df <- as.data.frame(lapply(ra_dec_deg_df, FUN=function(col) format_num(col, num_sig_dig)))
	ra_dec_deg_df
}



ra_dec_conv_func <- function(in_file, is_header=TRUE, col_sel="1,2", out_file, out_sep=",", num_sig_dig=4, col_names=FALSE, out_form="hms") {
	# Read data
	ra_dec_data <- read_data(in_file, is_header, FALSE)
	
	# Select requisite columns
	col_sel <- as.numeric(unlist(strsplit(col_sel, ",", fixed=TRUE)))
	ra_dec_data <- ra_dec_data[,col_sel]
	names(ra_dec_data) <- c("RA", "DEC")

	# Convert inputs to appropriate format
	num_sig_dig <- as.numeric(num_sig_dig)
	col_names <- as.logical(col_names)

	# Guess RA-DEC type and convert it to the other format
	data_class <- sapply(ra_dec_data, class)
	if(all(data_class=="numeric"))
		deg_to_time(ra_dec_data, col_names, out_file, out_sep, out_form) else 
	if(all(data_class=="character"))
		time_to_deg(ra_dec_data, col_names, out_file, out_sep, num_sig_dig) else
	stop("Cannot identify supplied co-ordinates")
}

