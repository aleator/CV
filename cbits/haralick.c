#include "haralick.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define FGET(img,x,y) (((float *)((img)->imageData + (y)*(img)->widthStep))[(x)])

#define NCOLORS 256

#define ANGLE_0 0
#define ANGLE_45 1
#define ANGLE_90 2
#define ANGLE_135 3 

#define EPSILON 0.00000000000001


struct array_properties {
  int n;
  double sum;
  double average;
  double std_dev;
  double var;
};

void calculate_array_properties(double *array, int n, struct array_properties *properties)
{
  properties->n = n;
  int i;
  for ( i=0; i<n; i++ ) {
    properties->sum += *(array+i);
  }
  properties->average = properties->sum/properties->n;
  double partial_sum = 0.0;
  for ( i=0; i<n; i++ ) {
    partial_sum += pow(*(array+i) - properties->average, 2);
  }
  partial_sum /= properties->n - 1;
  properties->var = partial_sum;
  properties->std_dev = sqrt(partial_sum);
}


void add_balanced_occurrence(double addition, double *sd_matrices, int angle, int colr1, int colr2)
{
  *(sd_matrices + (angle*NCOLORS*NCOLORS) + (colr1*NCOLORS) + colr2 ) += addition;
  *(sd_matrices + (angle*NCOLORS*NCOLORS) + (colr2*NCOLORS) + colr1 ) += addition;
}

int get_color(IplImage* im, int x, int y, float minimum, float maximum)
{
  float color = FGET(im,x,y);
  if (color < 0 || color > 1) {
    printf("Error! (x,y)=(%d,%d)'s value is out of [0,1]: %f\n", x, y, color);
    return 0;
  }
  // Preventing unnecessary divide by zero at scaling with images that have only 1 color.
  if (maximum-minimum < EPSILON) {
    return 0;
  }
  float ratio = (color-minimum)/(maximum-minimum);
  int col = (int)(NCOLORS*ratio);
  return col;
}



/*
 * Calculates gray-tone co-occurrence matrices for neighboring
 * cells at directions 0', 45', 90', 135'
 * 
 * Image's color values are between [0,1]. To normalize values, colors are quantized to
 * NCOLOR areas not between [0,1], but [min,max] in image.
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
  float minimum = 1.0;
  float maximum = 0.0;

  // Find min and max values 
  for (y=0; y<h; y++) {
    for (x=0; x<w; x++) {
      float cur_color = FGET(im,x,y);
      if (cur_color > maximum)
        maximum = cur_color;
      if (cur_color < minimum)
        minimum = cur_color;
    }
  }

  for (y=0; y<h; y++) {
    for (x=0; x<w; x++) {
      int current_color = get_color(im, x, y, minimum, maximum) 
      // 0 degrees: horizontal co-occurrence
      if (x+1 < w)
	add_balanced_occurrence(addition_0, sd_matrices, 0, current_color , get_color(im, x+1, y, minimum, maximum));

      // 45 degress: diagonal right-up co-occurrence
      if ( (x+1<w) && (y>0) )
	add_balanced_occurrence(addition_45, sd_matrices, 1, current_color, get_color(im, x+1, y-1, minimum, maximum));

      // 90 degrees: vertical co-occurrence
      if (y>0)
	add_balanced_occurrence(addition_90, sd_matrices, 2, current_color, get_color(im, x, y-1, minimum, maximum));

      // 135 degress: diagonal left-up co-occurrence
      if ( (x>0) && (y>0) )
	add_balanced_occurrence(addition_135, sd_matrices, 3, current_color, get_color(im, x-1, y-1, minimum, maximum));
    }
  }
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
      sum += pow(cell, 2);;
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
{
  struct array_properties *row_props    = malloc(NCOLORS*sizeof(*row_props));
  struct array_properties *column_props = malloc(NCOLORS*sizeof(*row_props));
  int i, j;

  // prepare means and standard deviations for co-occurrence matrix rows
  for ( j=0; j<NCOLORS; j++ ) {
    double *array = sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS);
    calculate_array_properties(array, NCOLORS, row_props+j);
  }

  // prepare means and standard deviations for co-occurrence matrix columns
  for ( i=0; i<NCOLORS; i++ ) {
    double *array = malloc(NCOLORS*sizeof(double));
    for ( j=0; j<NCOLORS; j++ ) {
      *(array+j) = *( sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
    }
    calculate_array_properties(array, NCOLORS, column_props+i);
    free(array);
  }

  double sum = 0.0;
  for ( i=0; i<NCOLORS; i++ ) {
    for ( j=0; j<NCOLORS; j++ ) {
      double cell     = *( sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
      double avgs     = ((*(row_props+j)).average) * ((*(column_props+i)).average);
      double std_devs = ((*(row_props+j)).std_dev) * ((*(column_props+i)).std_dev);
      sum += ( (i*j) * cell - avgs ) / std_devs;
    }
  }

  free(row_props);
  free(column_props);
  return sum;
}

/*
 * Calculates entropy.
 *
 * @param sd_matrices  set of co-occurrence matrices at four angles
 * @param angle        angle (matrix) of interest
 * @return             entropy for specified co-occurrence matrix
 */
double calculate_entropy(double *sd_matrices, int angle)
{
  double sum = 0.0;
  int j, i;
  for ( j=0; j<NCOLORS; j++ ) {
    for ( i=0; i<NCOLORS; i++ ) {
      double cell = *( sd_matrices + (angle*NCOLORS*NCOLORS) + (j*NCOLORS) + i );
      // Since log(0) is not defined, Haralick et al. 1973 recommends adding an
      // arbitarily small positive constant.
      sum = cell * log(cell+EPSILON);
    }
  }
  return -sum;
}


//FIXME global
struct haralick_values t;

/* 
 * Calculates texture features for given image.
 *
 * @param  image IplImage to be analyzed 
 * @return haralick_values struct containing calculated texture features
 */
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
  t.correlation_0   = calculate_correlation(sd_matrices, ANGLE_0);
  t.correlation_45  = calculate_correlation(sd_matrices, ANGLE_45);
  t.correlation_90  = calculate_correlation(sd_matrices, ANGLE_90);
  t.correlation_135 = calculate_correlation(sd_matrices, ANGLE_135);
  t.entropy_0   = calculate_entropy(sd_matrices, ANGLE_0);
  t.entropy_45  = calculate_entropy(sd_matrices, ANGLE_45);
  t.entropy_90  = calculate_entropy(sd_matrices, ANGLE_90);
  t.entropy_135 = calculate_entropy(sd_matrices, ANGLE_135);

  free(sd_matrices);
  return &t;
}

