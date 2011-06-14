#include "haralick.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

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



#define IGET(img,x,y) (((uchar *)((img)->imageData + (y)*(img)->widthStep))[(x)])

/*
 * Calculates the (normalized) grayscale-co-occurence matrix for an 8 bit image.
 * Takes pointers to 8U image and zero initialized space for the matrix (255 x 255 x sizeof(Int))
 */
void calculate_co_occurence_matrix(IplImage *im, int dx, int dy,  double *sd_matrix)
{
  CvSize imSize = cvGetSize(im);
  int w = imSize.width;
  int h = imSize.height;
  int matrix_size = 256;

  float minimum = 1.0;
  float maximum = 0.0;
  
  int usable_height = (dy > 0 ? h-dy : h) ;
  int usable_width  = (dx > 0 ? w-dx : w) ;

  int start_y = (dy < 0 ? -dy : 0) ;
  int start_x = (dx < 0 ? -dx : 0) ;

  int comparisons=0;

  for (int y=start_y;   y < usable_height ; y++) {
    for (int x=start_x; x < usable_width  ; x++) {
      int current_val   = IGET(im, x,    y   ); 
      int neighbour_val = IGET(im, x+dx, y+dy); 
      assert(current_val < 256);
      assert(neighbour_val < 256);
	  sd_matrix[current_val+neighbour_val*(matrix_size-1)] += 1;
      comparisons++;
    }
  }

  for (int j=0;   j < matrix_size*matrix_size ; j++) {
	  sd_matrix[j] /= comparisons;
    }
}

/*
 * Calculates angular second moment.
 *
 * @param matrix       co-occurrence matrix
 * @param size         number of elements in the matrix
 * @return             angular second moment for specified co-occurrence matrix
 */
double calculate_asm(const double *matrix, const int size)
{
  double sum = 0.0;
  for (int j=0; j<size; j++ ) {
    for (int i=0; i<size; i++ ) {
      double cell = matrix[i+j*size];
      sum += pow(cell, 2);
    }
  }
  return sum;
}

/*
 * Calculates contrast.
 *
 * @param matrix       co-occurrence matrix
 * @param size         number of elements in the matrix
 * @return             non-normalized contrast measure. (Divide by size of original image or by the
 *                     sum of co-occurence matrix for actual contrast).
 */
double calculate_contrast(const double *matrix, const int size)
{
  double sum = 0.0;
    for   (int j=0; j<size; j++ ) {
      for (int i=0; i<size; i++ ) {
        sum += (pow((i-j),2) * matrix[i+j*size]);  // Should i and j be normalized to [0,1] range?
      }
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
/*struct haralick_values *calculate_values(IplImage *image)
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
} */

