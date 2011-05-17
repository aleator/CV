#include "haralick.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define FGET(img,x,y) (((float *)((img)->imageData + (y)*(img)->widthStep))[(x)])

#define NCOLORS 8

#define ANGLE_0 0
#define ANGLE_45 1
#define ANGLE_90 2
#define ANGLE_135 3 

#define EPSILON 0.00000000000001

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

/*
 * Calculates angular second moment.
 *
 * @param sd_matrices  set of co-occurrence matrices at four angles
 * @param angle        angle (matrix) of interest
 * @return             angular second moment for specified co-occurrence matrix
 */
double calculate_asm(double *sd_matrices, int angle)
{
  double sum = 0.0;
  int j, i;
  for ( j=0; j<NCOLORS; j++ ) {
    for ( i=0; i<NCOLORS; i++ ) {
      double cell = *( sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
      sum += cell*cell;
    }
  }
  return sum;
}

/*
 * Calculates contrast.
 *
 * @param sd_matrices  set of co-occurrence matrices at four angles
 * @param angle        angle (matrix) of interest
 * @return             contrast or specified co-occurrence matrix
 */
double calculate_contrast(double *sd_matrices, int angle)
{
  double sum = 0.0;
  int i, j, n;
  for ( n=0; n<NCOLORS; n++ ) {
    double partial_sum = 0.0;
    for ( j=0; j<NCOLORS; j++ ) {
      for ( i=0; i<NCOLORS; i++ ) {
        if ( abs((i+1)-(j+1)) == n ) {
          double cell = *( sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
          partial_sum += cell;
        }
      }
    }
    sum += (n*n) * partial_sum;
  }
  return sum;
}

/*
 * Calculates correlation.
 *
 * @param sd_matrices  set of co-occurrence matrices at four angles
 * @param angle        angle (matrix) of interest
 * @return             correlation or specified co-occurrence matrix
 */
double calculate_correlation(double *sd_matrices, int angle)
{/*
  double u_x[NCOLORS] = 0.0;
  double sig_x[NCOLORS] = 0.0;
  for ( j=1; j<=NCOLORS; j++ ) {
    for ( i=1; i<=NCOLORS; i++ ) {
    }
  }

  double u_y[NCOLORS] = 0.0;
  double sig_y[NCOLORS] = 0.0;

  double sum = 0.0;
  int i, j, n;
  for ( n=0; n<NCOLORS; n++ ) {
    double partial_sum = 0.0;
    for ( j=1; j<=NCOLORS; j++ ) {
      for ( i=1; i<=NCOLORS; i++ ) {
        if ( abs(i-j) == n ) {
          double cell = *( sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
          partial_sum += cell;
        }
      }
    }
    sum += (n*n) * partial_sum;
  }
*/
  return -1.0;
}

//FIXME global
struct haralick_values t;

struct haralick_values *calculate_values(IplImage *image)
{
  // Gray-tone spatial-dependence matrices for degrees 0, 45, 90, 135
  // Four 2-dimensional arrays containing color-to-color occurrences.
  double* sd_matrices = prepare_matrix();

  calculate_matrices(image, sd_matrices);
  double asm_sum = 0.0;

  t.asm_0   = calculate_asm(sd_matrices, ANGLE_0);
  t.asm_45  = calculate_asm(sd_matrices, ANGLE_45);
  t.asm_90  = calculate_asm(sd_matrices, ANGLE_90);
  t.asm_135 = calculate_asm(sd_matrices, ANGLE_135);
  t.contrast_0   = calculate_contrast(sd_matrices, ANGLE_0);
  t.contrast_45  = calculate_contrast(sd_matrices, ANGLE_45);
  t.contrast_90  = calculate_contrast(sd_matrices, ANGLE_90);
  t.contrast_135 = calculate_contrast(sd_matrices, ANGLE_135);

  free(sd_matrices);
  return &t;
}

