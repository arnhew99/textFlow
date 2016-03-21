textFlow <- function(x, y, labels, adj=0.5, cex=1, available_width=NULL, ignore.linebreaks=TRUE) {

	## Matt Arnold March 2016
	## 
	## 
	## 
	## Function arguments
	## 
	## x, y				Coordinates of top left corner of "text box" in the current
	##					underlying coordinate system (each numeric, length 1)
	## labels			String to be formatted and printed (character, length 1)
	## adj				Ignored...
	## cex				Character expansion factor (numeric, length 1)
	## available_width	Override the detection of the amount of space available and 
	##					force the "text box" to be a certain width (left from x)
	
	## This function probably works best with par(xpd=NA) if you want to place a 
	## label/title around a plot
	

	## NB adj is currently ignored and this function will always produced left
	## aligned text. 

	lims <- list(xlim=par("usr")[1:2], ylim=par("usr")[3:4])
	
	## allow for available_width calculation to be overridden 
	if (is.null(available_width)) {
		## work out how much space we've got
		available_width <- diff(lims$xlim)
		available_width <- available_width - (x - lims$xlim[1])
	} 
	
	
	## remove any line breaks
	if (ignore.linebreaks) {
		labels <- paste(strsplit(labels, "\n", fixed=TRUE)[[1]], collapse="")
	}
	
	## split up the string into words
	labels_split <- strsplit(labels, " ", fixed=TRUE)[[1]]
	
	
	## work out how wide the string is... if it's too long, break the line
	current_stringwidth <- 0
	current_line <- 0
	bold <- FALSE
	italic <- FALSE
	
	# store the Xs and Ys in a list
	positions <- rep(list(list(x=c(), y=c(), label=c())), 4)
	
	for (i in 1:length(labels_split)) {
	
		if (findMark(labels_split[i], "#b#")$mark) {
			bold <- !bold
			labels_split[i] <- findMark(labels_split[i], "#b#")$string
		}
		if (findMark(labels_split[i], "#i#")$mark) {
			italic <- !italic
			labels_split[i] <- findMark(labels_split[i], "#i#")$string
		}
		
		fontstyle <- 1
		if (all(bold, italic)) fontstyle <- 4
		else if (bold) fontstyle <- 2
		else if (italic) fontstyle <- 3
		
		## find out if there is a forced line break ahead of the 
		## current word, as long as we're detecting linebreaks
		forced_linebreak <- FALSE
		nbreaks <- 0
		if (!ignore.linebreaks) {
			if (findMark(labels_split[i], "\n")$mark) {
				forced_linebreak <- TRUE
				foundMark <- findMark(labels_split[i], "\n")
				labels_split[i] <- foundMark$string
				nbreaks <- foundMark$mark.count
			}
		}
		
		## annoyingly it looks like we're going to have to place each individual word
		test_stringwidth <- current_stringwidth + strwidth(paste0(" ", labels_split[i]), cex=cex, font=fontstyle)
		if (test_stringwidth > available_width) {
			forced_linebreak <- TRUE
			nbreaks <- 1
		}
		linebreak <- FALSE
		if (any(forced_linebreak)) {
			current_line <- current_line + nbreaks*(strheight("A\nA", cex=cex) - strheight("A", cex=cex))
			current_stringwidth <- 0
			linebreak <- TRUE
		} 
		
		
		
		# text(x=x + current_stringwidth, y=y-current_line, labels=labels_split[i], cex=cex, adj=c(0,0), font=fontstyle)
		positions[[fontstyle]]$x <- c(positions[[fontstyle]]$x, x+current_stringwidth)
		positions[[fontstyle]]$y <- c(positions[[fontstyle]]$y, y-current_line)
		positions[[fontstyle]]$label <- c(positions[[fontstyle]]$label, labels_split[i])
		
		
		if (linebreak) {
			current_stringwidth <- 0 + strwidth(paste0(labels_split[i], " "), cex=cex, font=fontstyle)
		} else {
			current_stringwidth <- test_stringwidth
		}
	}
	
	
	for (i in 1:4) {
		if (is.null(positions[[i]]$x)) next
		text(x=positions[[i]]$x, y=positions[[i]]$y, labels=positions[[i]]$label, cex=cex, adj=c(0,0), font=i)
	}
	
}


findMark <- function(string, mark) {
	# only works on individual words...
	if (string == "") return(list(mark=FALSE, string=""))
	split_string <- strsplit(string, mark, fixed=TRUE)[[1]]
	if (length(split_string) > 1) {
		return(list(mark=TRUE, string=split_string[min(which(split_string != ""))], mark.count=sum(split_string=="")))
	}
	else if (split_string[1] == "") return(list(mark=TRUE, string=split_string[1], mark.count=1)) 
	else return(list(mark=FALSE, string=string[1], mark.count=0))
}
