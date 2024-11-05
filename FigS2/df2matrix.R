Args = commandArgs(T)

data.frame2matrix = function(data, rowtitle, coltitle, datatitle, rowdecreasing = FALSE, coldecreasing = FALSE, default_value = NA) {
  # check, whether titles exist as columns names in the data.frame data
  if ( (!(rowtitle%in%names(data))) 
       || (!(coltitle%in%names(data))) 
       || (!(datatitle%in%names(data))) ) {
    stop('data.frame2matrix: bad row-, col-, or datatitle.')
  }

  # get number of rows in data
  ndata = dim(data)[1]

  # extract rownames and colnames for the matrix from the data.frame
  rownames = sort(unique(data[[rowtitle]]), decreasing = rowdecreasing)
  nrows = length(rownames)
  colnames = sort(unique(data[[coltitle]]), decreasing = coldecreasing)
  ncols = length(colnames)

  # initialize the matrix
  out_matrix = matrix(NA, 
                      nrow = nrows, ncol = ncols,
                      dimnames=list(rownames, colnames))

  # iterate rows of data
  for (i1 in 1:ndata) {
    # get matrix-row and matrix-column indices for the current data-row
    iR = which(rownames==data[[rowtitle]][i1])
    iC = which(colnames==data[[coltitle]][i1])

    # throw an error if the matrix entry (iR,iC) is already filled.
    if (!is.na(out_matrix[iR, iC])) {
        print(data[[rowtitle]][i1])
        print(data[[coltitle]][i1])
        stop('data.frame2matrix: double entry in data.frame')
        }

    out_matrix[iR, iC] = data[[datatitle]][i1]
  }

  # set empty matrix entries to the default value
  out_matrix[is.na(out_matrix)] = default_value

  # return matrix
  return(out_matrix)

}

myData <- read.table(Args[1], header=F, sep="\t")

colnames(myData) = c('dim1','dim2','values')

myMatrix = data.frame2matrix(myData,'dim1','dim2','values')

write.table(myMatrix, file=Args[2], sep="\t",quote=F)
