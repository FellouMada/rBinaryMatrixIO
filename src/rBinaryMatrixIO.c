#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <Rmath.h>
#include <float.h>
int writeBinaryMatrix(char ** filespec,int * nCol,int * nRow,int * in,int * bycol,int * status);
static const int FORMAT_INT=123;
static const int BYCOL=1;
static const int BYROW=2;

int checkHeader(FILE * fp,int nCol,int nRow, int byWhat){
  size_t res;
  int header[4];
  res = fread(header,sizeof(int),4,fp);
  
  /* check header  matches request */
  if( header[0]!=FORMAT_INT){
    printf("Not a binary matrix file!");
    return 1;
  }
  if( header[2]!=nCol || header[3]!=nRow){
    printf("Stored binary matrix has ncol=%d and nrow=%d not [%d,%d]",header[2],header[3],nCol,nRow);
    return 1;
  }
  if(header[1] !=byWhat){
    printf("Binary matrix not stored by column");
    return 1;
  }
  return 0;
}

int readBinaryMatrix(char ** filespec,int * nCol,int * nRow,int * out, int * status){
  *status=0;
  FILE *fp;
  fp=fopen(*filespec,"rb");
  if (!fp)
  {
    *status=1; 
    printf("Unable to open file!");
    return 1;
  }
  size_t res;
  *status=checkHeader(fp,*nCol,*nRow,BYCOL);
  if(*status>0){
    fclose(fp);
    return 1;
  }
  fseek(fp, 4*(sizeof(int)), SEEK_SET);
  res = fread( out, sizeof(int), (*nCol)*(*nRow), fp);
  fclose(fp);
  return 0;
}


int readBinaryMatrixByRowIndex(char ** filespec,int * nCol,int * nRow,int * indexes, int * num_indexes,int * out, int * status){
  *status=0;
  FILE *fp;
  fp=fopen(*filespec,"rb");
  if (!fp)
  {
    *status=1;
    printf("Unable to open file!");
    return 1;
  }
  *status=checkHeader(fp,*nCol,*nRow,BYROW);
  if(*status>0){
    fclose(fp);
    return 1;
  }
  size_t res;
  int i, j=0, k=0;
  int tmp[*nRow];
  for (i=0; i<(*num_indexes); i++) {
    /* add 4 bytes for the header */
    fseek(fp, (4+indexes[i]*(*nCol))*(sizeof(int)), SEEK_SET);
    res = fread(out+i*(*nCol), sizeof(int), *nCol, fp);//We can read directly into out without tmp variable
    /*fseek(fp, 0, SEEK_SET);
    for(j=0;j<(*nRow);j++){
      out[k++] = tmp[j];
    }*/
  }
  fclose(fp);
  return 0;
}

int readBinaryMatrixByColumnIndex(char ** filespec,int * nCol,int * nRow,int * indexes, int * num_indexes,int * out, int * status){
  *status=0;
  FILE *fp;
  fp=fopen(*filespec,"rb");
  if (!fp)
  {
    *status=1;
    printf("Unable to open file!");
    return 1;
  }
  *status=checkHeader(fp,*nCol,*nRow,BYCOL);
  if(*status>0){
    fclose(fp);
    return 1;
  }
  size_t res;
  int i, j=0, k=0;
  for (i=0; i<(*num_indexes); i++) {
    /* add 4 bytes for the header */
    fseek(fp, (4+indexes[i]*(*nRow))*(sizeof(int)), SEEK_SET);
    res = fread(out+i*(*nRow), sizeof(int), *nRow, fp);
  }
  fclose(fp);
  return 0;
}

int writeBinaryMatrix(char ** filespec,int * nCol,int * nRow,int * in,int * bycol,int *status){
  *status=0;
  FILE *fp;
  fp=fopen(*filespec,"wb");
  if (!fp)
  {
    printf("Unable to open file!");
    *status=1;
    return 1;
  }
  int FORMAT_INT=123;
  int COL_OR_ROW=*bycol;
  //write the first 4 indices..
  int header[4];
  header[0]=123;
  header[1]=*bycol;
  header[2]=*nCol;
  header[3]=*nRow;
  size_t n;
  /* TODO: actually check the number of written elemets (n) */
  n=fwrite( (const void*) header, sizeof(int), 4, fp);
  n=fwrite( (const void*) in, sizeof(int), (*nCol)*(*nRow), fp);
  fclose(fp);
  return 1;
}

