BYROW=2
BYCOL=1
#' Reads binary matrix
#' 
#' The function checks if the target binary file has the expected number of rows and columns.
#' @param fileStub file name for binary file. Will create fileStub_BY_COL.bin and fileStub_BY_ROW.
#' @param nCol number of column
#' @param nRow number of rows
#' @return status.  0 =ok
#' @useDynLib rBinaryMatrixIO readBinaryMatrix
#' @export
#' @examples 
#' 
readBinaryIntMatrix=function(fileStub,nCol,nRow){
  fileC=sprintf("%s_BY_COL.bin",fileStub)
  if(!file.exists(fileC)){
    stop("target doesn't exist")
  }
  res=.C("readBinaryMatrix",fileC,as.integer(nCol),as.integer(nRow),result=integer(nRow*nCol),status=integer(1))
  if(res$status>0){
    stop("Exception in readBinaryMatrix")
  }
  matrix(as.integer(res[["result"]]),ncol = nCol);
}
#' Writes binary matrix
#'
#' @param fileStub file name for binary file. Will create fileStub_BY_COL.bin and fileStub_BY_ROW.
#' @param inmatrix matrix if integers.  Should have at least one element.
#' @return status.  0 =ok
#' @useDynLib rBinaryMatrixIO writeBinaryMatrix
#' @export
#' @examples 
#' 
writeBinaryIntMatrix=function(fileStub,inmatrix){
  
  fileC=sprintf("%s_BY_COL.bin",fileStub)
  fileR=sprintf("%s_BY_ROW.bin",fileStub)
  if(file.exists(fileC) || file.exists(fileR)){
    stop(paste0("target files ",fileC," or ",fileR," already exists - please delete"))
  }
  nRow=nrow(inmatrix)
  nCol=ncol(inmatrix)
  if(class(inmatrix[1])!="integer"){
    stop("Attempt to write non integer matrix")
  }
  resc=.C("writeBinaryMatrix",
          as.character(fileC),
          as.integer(nCol),
          as.integer(nRow),
          as.integer(inmatrix),
          as.integer(BYCOL),
          status=integer(1)
          )
  if(resc$status>0){
    stop("Exception in writeBinaryMatrix")
  }
  ##  Here we transpose the matrix so that the data is written a row at a time.
  resr=.C("writeBinaryMatrix",
          as.character(fileR),
          as.integer(nCol),
          as.integer(nRow),
          as.integer(t(inmatrix)),
          as.integer(BYROW),
          status=integer(1)
  )
  if(resr$status>0){
    stop("Exception in writeBinaryMatrix")
  }
}
#' Reads binary matrix restricting to the specified rows
#'
#' @param fileStub file name for binary file. Will create fileStub_BY_COL.bin and fileStub_BY_ROW.
#' @param nCol number of column
#' @param nRow number of rows
#' @param idxRow 1 based row indices.
#' @return status.  0 =ok
#' @useDynLib rBinaryMatrixIO readBinaryMatrixByRowIndex
#' @export
#' @examples 
#' 
readBinaryIntMatrixByRow=function(fileStub,nCol,nRow,idxRow){
  fileR=sprintf("%s_BY_ROW.bin",fileStub)
  if(!file.exists(fileR)){
    stop("target doesn't exist")
  }
  nIndexedRows=length(idxRow)
  if(max(idxRow)>=nRow){
    stop("error")
  }
  res=.C("readBinaryMatrixByRowIndex",fileR,as.integer(nCol),as.integer(nRow),as.integer(idxRow-1),as.integer(nIndexedRows),result=integer(nCol*nIndexedRows),status=integer(1));
  if(res$status>0){
    stop("Exception in readBinaryMatrixByRowIndex")
  }
  t(matrix(res$result,ncol=nIndexedRows))
}

#' Reads binary matrix restricting to the specified columns
#'
#' @param fileStub file name for binary file. Will create fileStub_BY_COL.bin and fileStub_BY_ROW.
#' @param nCol number of column
#' @param nRow number of rows
#' @param idxColumn 1 based row indices.
#' @return status.  0 =ok
#' @useDynLib rBinaryMatrixIO readBinaryMatrixByRowIndex
#' @export
#' @examples 
#' 
readBinaryIntMatrixByColumn=function(fileStub,nCol,nRow,idxColumn){
  fileC=sprintf("%s_BY_COL.bin",fileStub)
  if(!file.exists(fileC)){
    stop("target doesn't exist")
  }
  nIndexedColumns=length(idxColumn)
  if(max(idxColumn)>nCol){
    cat("nCol=",nCol," max idxColumn=",max(idxColumn),"\n")
    stop("bounds error")
  }
  res=.C("readBinaryMatrixByColumnIndex",fileC,as.integer(nCol),as.integer(nRow),as.integer(idxColumn-1),as.integer(nIndexedColumns),result=integer(nIndexedColumns*nRow),status=integer(1));
  if(res$status>0){
    stop("Exception in readBinaryMatrixByRowIndex")
  }
  matrix(as.integer(res[["result"]]),ncol = nIndexedColumns)
}
