#include "haralick.h"
#include <stdio.h>
#include <stdlib.h>

#define FGET(img,x,y) (((float *)((img)->imageData + (y)*(img)->widthStep))[(x)])

#define NCOLORS 8

void add_balanced_occurrence(double addition, double *sd_matrices, int angle, int colr1, int colr2)
{
  *(sd_matrices + (angle*NCOLORS*NCOLORS) + (colr1*NCOLORS) + colr2 ) += addition;
  *(sd_matrices + (angle*NCOLORS*NCOLORS) + (colr2*NCOLORS) + colr1 ) += addition;
}

/*
 * Calculates gray-tone co-occurrence matrices for neighboring
 * cells at directions 0', 45', 90', 135'
 * 
 * Takes pointers to IplImage and allocated space for matrices
 */
void calculate_matrices(IplImage *im, double *sd_matrices)
{
  CvSize imSize = cvGetSize(im);
  int w = imSize.width;
  int h = imSize.height;

  // Numbers of neighboring resolutions in different angles for balancing
  int neighbours_0   = 2 * (w-1) * h;
  int neighbours_45  = 2 * (w-1) * (h-1);
  int neighbours_90  = 2 *   w   * (w-1);
  int neighbours_135 = neighbours_45; 

  // All cells could also be divided by neighbour resolution counts afterwards
  double addition_0   = 1.0 / neighbours_0;
  double addition_45  = 1.0 / neighbours_45;
  double addition_90  = 1.0 / neighbours_90;
  double addition_135 = 1.0 / neighbours_135;

  int x,y;
  for (y=0; y<h; y++) {
    for (x=0; x<w; x++) {
      // 0 degrees: horizontal co-occurrence
      if (x+1 < w)
	add_balanced_occurrence(addition_0, sd_matrices, 0, get_color(im, x, y), get_color(im, x+1, y));

      // 45 degress: diagonal right-up co-occurrence
      if ( (x+1<w) && (y>0) )
	add_balanced_occurrence(addition_45, sd_matrices, 1, get_color(im, x, y), get_color(im, x+1, y-1));

      // 90 degrees: vertical co-occurrence
      if (y>0)
	add_balanced_occurrence(addition_90, sd_matrices, 2, get_color(im, x, y), get_color(im, x, y-1));

      // 135 degress: diagonal left-up co-occurrence
      if ( (x>0) && (y>0) )
	add_balanced_occurrence(addition_135, sd_matrices, 3, get_color(im, x, y), get_color(im, x-1, y-1));
    }
  }
}

int get_color(IplImage* im, int x, int y)
{
  float color = FGET(im,x,y);
  if (color < 0 || color > 1) {
    printf("Error! (x,y)=(%d,%d)'s value is out of [0,1]: %f\n", x, y, color);
    return 0;
  }
  int col = (int)(NCOLORS*color);
  return col;
}

double* prepare_matrix()
{
  // 4*NCOLORS*NCOLORS should be enough but leads to memory corruption -> bug
  double* sd_matrices = malloc(5*NCOLORS*NCOLORS*sizeof(double));
  int angle,i,j;
  for (angle=0; angle<4; angle++) {
    for (j=0; j<NCOLORS; j++) {
      for (i=0; i<NCOLORS; i++) {
        *(sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i ) = 0.0;
      }
    } 
  }
  return sd_matrices;
}


double calculate_asm_average(IplImage *im)
{
  // Gray-tone spatial-dependence matrices for degrees 0, 45, 90, 135
  // Four 2-dimensional arrays containing color-to-color occurrences.
  double* sd_matrices = prepare_matrix();

  calculate_matrices(im, sd_matrices);
  double asm_sum = 0.0;

  int angle, i, j;
  for (angle=0; angle<4; angle++) {
    double asm_val = 0.0;
    for (j=0; j<NCOLORS; j++) {
      for (i=0; i<NCOLORS; i++) {
	double val = *(sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
	asm_val += val*val;
      }
    }
    asm_sum += asm_val / 4;
  }
  printf("ASM sum: %f\n", asm_sum);
  free(sd_matrices);
  return asm_sum;
}

/*
haralick_values calculate_values(IplImage *im)
{
  double* sd_matrices = prepare_matrix();

  calculate_matrices(im, sd_matrices);
  double asm_sum = 0.0;

  int angle, i, j;
  for (angle=0; angle<4; angle++) {
    double asm_val = 0.0;
    for (j=0; j<NCOLORS; j++) {
      for (i=0; i<NCOLORS; i++) {
	double val = *(sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
	asm_val += val*val;
      }
    }
    asm_sum += asm_val / 4;
  }
  printf("ASM sum: %f\n", asm_sum);
  free(sd_matrices);
  struct Haralicks h = { asm_sum };
  return h;
}*/

//FIXME global
struct haralick_values t;

struct haralick_values *calculate_values(IplImage *im)
{
  t.asm_average = 5.0;
  t.asm_0       = 0.0;
  t.asm_45      = 45.0;
  t.asm_90      = 90.0;
  t.asm_135     = 135.0;
  return &t;
}
