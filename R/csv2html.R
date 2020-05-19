#' Convert csv from \code{getFlickrImages} to html Format
#'
#' @param csvFile file from  \code{getFlickrImages}. This csv file is usually stored in the \code{py} foldere and called \code{output.csv}.
#' @param htmlFile output htm file 
#' @export
csv2html = function(csvFile = "output.csv", htmlFile = "mypage.html"){

ocean = read.csv(csvFile)

outline = with(dat,paste("<tr><td>",lon,
				"</td><td>",lat,
				"</td><td>",datetaken,
				"</td><td>",dateupload,
				"</td><td><img src=",url," width = 300></td></tr>",sep=""))

# ctreates html page

sink(htmlFile)
cat("<html>
		<body>
			<code>
				<table border=1>
					<tr>
						<th>Lat</th>
						<th>Lon</th>
						<th>Date</th>
						<th>Date Upload</th>
						<th>Img</th>
					</tr>")
cat(paste(outline,"\n",sep=""))
cat("				</table>
			</code>
		</body>
</html>")
sink()
}

#' Export Pre-validated Pictures to html
#' 
#' This function exports the pre-validated pictures in an html file for further screening 
#' @param dat data.frame of pre-validated pictures. There three columns: Species, ID, Img.
#' @export

prevalidated2html = function(dat = dat, file = "validate.html"){

outline = with(dat,paste("<tr><td>",species_name,
				"</td><td>",id,
				"</td><td><img src=",img_name," width = 300></td></tr>",sep=""))

# creates html page

sink(file)
cat("<html>
		<body>
			<code>
				<table border=1>
					<tr>
						<th>Species</th>
						<th>ID</th>
						<th>Img</th>
					</tr>")
cat(paste(outline,"\n",sep=""))
cat("				</table>
			</code>
		</body>
</html>")
sink()

datIds = data.frame(id = dat$id, species= "")
write.csv(datIds, "../data/toConfirm.csv", row.names = F)
}