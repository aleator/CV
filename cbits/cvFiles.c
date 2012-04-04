#include "cvFiles.h"
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/highgui/highgui_c.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

typedef unsigned int uint_t;

const float epsilon = 0.001;

int *mask_idx;
int mask_size;
int r;
int d;

float calc_sum(float *pos, float *count)
{
  int *p, *e;
  float c, s, v;
  
  c = 0;
  s = 0;
  e = &mask_idx[mask_size];
  for (p = mask_idx; p < e; p++) {
    v = *(pos + *p);
    if (v > epsilon) {
      s += v;
      c += 1;
    }
  }
  *count = c;
  return s;
}

float calc_sum2(float *pos, int *count)
{
  int i, c, *p, *e;
  float s, v;
  
  c = 0;
  s = 0;
  e = &mask_idx[mask_size];
  for (p = mask_idx; p < e; p++) {
    v = *(pos + *p);
    if (v > epsilon) {
      s += v*v;
      c += 1;
    }
  }
  *count = c;
  return s;
}

IplImage *read_from_tcr(const char *path, uint_t **timestamp_array)
{
  IplImage *dst;
  float *dst_data, *dst_pos;
  uint_t dst_stride, *timestamps, timestamp;
  size_t width, height, file_size, read_size, line_size, timestamp_size;
  size_t line_count, timestamp_count;
  CvSize size;
  FILE *input_file;
  
  width = 2592;
  line_size = width * sizeof(float);
  timestamp_size = sizeof(uint_t);
  dst = NULL;
  
  input_file = fopen(path, "rb");
  if (input_file == NULL) {
    printf("Error: unable to open file %s\n", path);
    return NULL;
  }
  
  /* seek file end to determine image size */
  fseek(input_file , 0 , SEEK_END);
  file_size = ftell(input_file);
  rewind(input_file);
  height = (size_t)(file_size / (line_size + timestamp_size));
  
  size.width = width;
  size.height = height;
  dst = cvCreateImage(size, IPL_DEPTH_32F, 1);
  if (dst == NULL) {
    printf("Error: unable to create image\n");
    return NULL;
  }
  
  dst_data = (float*)dst->imageData;
  dst_stride = (uint_t)(dst->widthStep / sizeof(float));
  
  if (timestamp_array != NULL) {
    timestamps = (uint_t *)malloc(height * timestamp_size);
    if (timestamps == NULL) {
      printf("Error: unable to allocate timestamp array\n");
      return NULL;
    }
    *timestamp_array = timestamps;
  }
  else {
    timestamps = NULL;
  }
  
  line_count = 0;
  timestamp_count = 0;
  
  dst_pos = dst_data;
  while (1) {
    /* read line timestamp to use for rectification */
    read_size = fread((void*)&timestamp, timestamp_size, 1, input_file);
    if (read_size < 1) {
      if (line_count < height) {
        printf("Error: unexpected end of file after line %d\n", line_count);
        return dst;
      }
      break;
    }
    else {
      if (timestamps != NULL) {
        timestamps[timestamp_count++] = timestamp;
      }
    }
    
    /* read data line */
    read_size = fread((void*)dst_pos, sizeof(float), width, input_file);
    if (read_size < width) {
      printf("Error unexpected end of file after line %d\n", line_count);
      return dst;
    }
    else {
      line_count += 1;
      dst_pos += dst_stride;
    }
  }
  fclose(input_file);
  
  return dst;
}

IplImage *rectify_tcr(IplImage *src, uint_t *timestamps, uint_t startx, uint_t endx, uint_t starty, uint_t endy)
{
  IplImage *dst;
  float *src_data, *dst_data, *src_pos, *dst_pos, tratio;
  uint_t src_stride, dst_stride, tstart, tend, tdiff, *timestamps_accum;
  size_t height, new_width, new_height, row_size;
  int i, j, result;
  CvSize size;
  
  new_width = endx - startx;
  new_height = new_width;
  
  tstart = timestamps[starty];
  tend = timestamps[endy];
  tdiff = tend - tstart;
  tratio = (float)tdiff / (float)new_height;
  
  /* create a lookup table for rectified image time stamps */
  timestamps_accum = (uint_t *)malloc(new_height * sizeof(uint_t));
  if (timestamps_accum == NULL) {
    printf("Error: unable to allocate cumulative timestamp array\n");
    return NULL;
  }
  
  timestamps_accum[0] = tstart;
  for (i = 1; i < new_height; i++) {
    timestamps_accum[i] = tstart + (uint_t)(tratio * i);
  }
  
  size.width = new_width;
  size.height = new_height;        
  dst = cvCreateImage(size, IPL_DEPTH_32F, 1);
  if (dst == NULL) {
    printf("Error: rectify_tcr: Unable to create image\n");
    return NULL;
  }
  height = src->height;
  src_data = (float*)src->imageData;
  src_stride = (uint_t)(src->widthStep / sizeof(float));
  dst_data = (float*)dst->imageData;
  dst_stride = (uint_t)(dst->widthStep / sizeof(float));
  
  src_pos = src_data;
  j = starty;
  row_size = new_width * sizeof(float);
  dst_pos = dst_data + starty * dst_stride + startx;
  for (int i = 0; i < new_height; i++, dst_pos += dst_stride) {
    if (timestamps[j] < timestamps_accum[i]) {
      j++;
      if (j == height) {
        printf("Error: rectify_tcr: row overflow\n");
        break;
      }
      src_pos += src_stride;
    }
    memcpy(dst_pos, src_pos, row_size);
  }
  
  free(timestamps_accum);
  return dst;
}

int box_mean_abs_diff(IplImage *src, IplImage *dst, int r, float ignore_val, float ignore_eps)
{
  float *src_data, *src_pos, *dst_data, *dst_pos, value, sum, sum_count, min_count;
  uint_t src_stride, dst_stride, width, height;
  int x, y, p, d, mask_size, *mask_idx;
  
  cvSet(dst, cvScalarAll(0), NULL);
  src_data = (float*)src->imageData;
  src_stride = (uint_t)(src->widthStep / sizeof(float));
  dst_data = (float*)dst->imageData;
  dst_stride = (uint_t)(dst->widthStep / sizeof(float));
  width = src->width;
  height = src->height;
  
  r = 2;
  d = (2*r+1);
  /* create the mask offset table for the averaging filter mask */
  mask_size = d*d;
  mask_idx = malloc(mask_size * sizeof(int));
  for (y = -r, p = 0; y <= r; y++) {
    for (x = -r; x <= r; x++) {
      mask_idx[p++] = y * src_stride + x;
    }
  }
  
  min_count = mask_size / 2.0;
  for (y = r; y < height - r; y++) {
    src_pos = src_data + y * src_stride + r;
    dst_pos = dst_data + y * dst_stride + r;
    for (x = r; x < width - r; x++, src_pos++, dst_pos++) {
      value = *src_pos;
      sum = calc_sum(src_pos, &sum_count);
      if (sum_count > min_count) {
        if (value < ignore_val - ignore_eps || ignore_val + ignore_eps < value) {
          value = (sum/sum_count) - value;
          if (value < 0) value = -value;
        }
        else {
          value = 0;
        }
        *dst_pos = value;
      }
      else {
        *dst_pos = 0;
      }
    }
  }
  
  free(mask_idx);
}

IplImage *read_from_tcr_rectified(const char *path)
{
  IplImage *src, *tmp, *dst;
  float *tmp_data, *tmp_pos;
  CvSize size;
  uint_t *timestamps, tmp_stride, width, height, x, y, r;
  uint_t minx, maxx, startx, endx, miny, maxy, starty, endy;
  float value;
  
  src = read_from_tcr(path, &timestamps);
  if (src == NULL || timestamps == NULL) {
    return NULL;
  }
  width = src->width;
  height = src->height;
  size.width = width;
  size.height = height;
  tmp = cvCreateImage(size, IPL_DEPTH_32F, 1);
  if (tmp == NULL) {
    printf("Error: read_from_tcr_rectified: unable to create image\n");
    return NULL;
  }
  
  r = 2;
  
  box_mean_abs_diff(src, tmp, r, 0, 0.001);
  
  tmp_data = (float*)tmp->imageData;
  tmp_stride = (uint_t)(tmp->widthStep / sizeof(float));
  for (y = 0; y < height; y++) {
    tmp_pos = tmp_data + y * tmp_stride;
    for (x = 0; x < width; x++) {
      value = *tmp_pos;
      if (x < minx) minx = x; else if (x > maxx) maxx = x;
      if (y < miny) miny = y; else if (y > maxy) maxy = y;
    }
  }
  
  startx = 0;
  if (minx > 2) startx = minx - 2;
  endx = width - 1;
  if (maxx < endx - 2) endx = maxx + 2;
  starty = 0;
  if (miny >  2) starty = miny - 2;
  endy = height - 1;
  if (maxy < endy - 2) endy = maxy + 2;
  
  dst = rectify_tcr(tmp, timestamps, startx, endx, starty, endy);
  return dst;
}

IplImage *read_from_tcr_rectified_old(const char *path)
{
  IplImage *src, *tmp, *tmp2, *tmp3, *tmp4, *dst;
  float *src_data, *tmp_data, *dst_data, *src_pos, *tmp_pos, *dst_pos;
  int src_stride, tmp_stride, dst_stride;
  unsigned int *timestamps, *timestamps_accum;
  unsigned int timestamp, timestamp_count, tstart, tend, tdiff;
  float tratio;
  CvSize size;
  size_t width, height, read_size;
  int line_width, timestamp_width, line_count, file_size;
  FILE *input_file;
  /* histogram for grayscale equalization */
  float hist[256];

  width = 2592;
  line_width = width * sizeof(float);
  timestamp_width = sizeof(unsigned int);
  src = NULL;
  tmp = NULL;
  dst = NULL;
  
  input_file = fopen(path, "rb");
  if (input_file != NULL) {
    /* seek file end to determine image size */
    fseek (input_file , 0 , SEEK_END);
    file_size = ftell(input_file);
    rewind (input_file);
    height = (size_t)(file_size / (line_width + timestamp_width));
    /*printf("File size: %d, rows: %d\n", file_size, height);*/
    
    size.width = width;
    size.height = height;
    src = cvCreateImage(size, IPL_DEPTH_32F, 1);
    tmp = cvCreateImage(size, IPL_DEPTH_32F, 1);
    tmp2 = cvCreateImage(size, IPL_DEPTH_32F, 1);
    tmp3 = cvCreateImage(size, IPL_DEPTH_32F, 1);
    tmp4 = cvCreateImage(size, IPL_DEPTH_32F, 1);
    src_data = (float*)src->imageData;
    tmp_data = (float*)tmp->imageData;
    src_stride = (int)(src->widthStep / sizeof(float));
    tmp_stride = (int)(tmp->widthStep / sizeof(float));
    
    cvSet(tmp, cvScalarAll(0), NULL);
    
    r = 2;
    d = (2*r+1);
    /* create the mask offset table for the averaging filter mask */
    mask_size = d*d;
    mask_idx = malloc(mask_size * sizeof(int));
    for (int i = -r, pos = 0; i <= r; i++) {
      for (int j = -r; j <= r; j++) {
        mask_idx[pos++] = i * src_stride + j;
        /*printf("%d ", mask_idx[pos-1]);*/
      }
    }
    /*printf("\n");*/
    
    timestamps = (unsigned int *)malloc(height * timestamp_width);
    if (src_data != NULL) {
      line_count = 0;
      timestamp_count = 0;
      
      src_pos = src_data;
      while (1) {
        /* read line timestamp to use for rectification */
        read_size = fread((void*)&timestamp, timestamp_width, 1, input_file);
        if (read_size < 1) {
          break;
        }
        else {
          timestamps[timestamp_count++] = timestamp;
        }
        
        /* read data line */
        read_size = fread((void*)src_pos, sizeof(float), width, input_file);
        if (read_size < width) {
          break;
        }
        else {
          line_count += 1;
          src_pos += src_stride;
        }
      }      
      fclose(input_file);
      
      /*cvSaveImage("tcr_raw.png", src, 0);*/
      /*
      cvSmooth(src,tmp,CV_GAUSSIAN,5,5,0,0);
      cvLaplace(tmp,tmp2,5);
      */
      /*
      cvSmooth(src,tmp,CV_BLUR,5,5,0,0);
      cvSmooth(src,tmp2,CV_GAUSSIAN,3,3,0,0);
      cvAbsDiff(tmp,src,tmp3);
      cvSaveImage("tcr_diff.png", tmp3, 0);
      cvSaveImage("tcr_smooth2.png", tmp2, 0);
      */
      /*
      cvSaveImage("tcr_laplacian5.png", tmp2, 0);
      */
      {
        unsigned int x, y, minx, maxx, miny, maxy, count;
        unsigned int new_width, new_height, startx, endx, starty, endy;
        float value, sum, sum1, sum2, min, max, ratio, stretch, avg, sdv;
        float sum_count, min_count, stretch_1, stretch_2;
        
        min = 2000000000;
        max = 0;
        minx = 2000000000;
        maxx = 0;
        miny = 2000000000;
        maxy = 0;
        count = 0;
        sum1 = 0;
        sum2 = 0;
        min_count = mask_size / 2.0;
        for (y = r; y < height - r; y++) {
          src_pos = src_data + y * src_stride + r;
          tmp_pos = tmp_data + y * tmp_stride + r;
          for (x = r; x < width - r; x++, src_pos++, tmp_pos++) {
            value = *src_pos;
            sum = calc_sum(src_pos, &sum_count);
            if (sum_count > min_count) {
              if (value > epsilon) {
                value = (sum/sum_count) - value;
                if (value < min) min = value; else if (value > max) max = value;
                if (x < minx) minx = x; else if (x > maxx) maxx = x;
                if (y < miny) miny = y; else if (y > maxy) maxy = y;
                sum1 += value;
                sum2 += value*value;
                count += 1;
              }
              else {
                value = 0;//sum/count;
              }
              *tmp_pos = value;
            }
            else {
              *tmp_pos = 0;
            }
          }
        }
        printf("\n");
        avg = sum1 / count;
        sdv = sqrt(sum2 / count - avg*avg);
        printf("min=%f max=%f avg=%f sdv=%f minx=%d maxx=%d miny=%d maxy=%d\n", min, max, avg, sdv, minx, maxx, miny, maxy);
        /*cvSaveImage("tcr_filtered.png", tmp, 0);*/
        
        startx = r;
        if (minx > startx + 2) startx = minx - 2;
        endx = width - r - 1;
        if (maxx < endx - 2) endx = maxx + 2;
        starty = r;
        if (miny > starty + 2) starty = miny - 2;
        endy = height - r - 1;
        if (maxy < endy - 2) endy = maxy + 2;
        
        new_width = endx - startx;
        /*new_height = endy - starty;*/
        new_height = new_width;
        
        tstart = timestamps[starty];
        tend = timestamps[endy];
        tdiff = tend - tstart;
        ratio = (float)tdiff / (float)new_height;
        printf("start=%d end=%d diff=%d ratio=%f\n", tstart, tend, tdiff, ratio);
        
        /* create a lookup table for rectified image time stamps */
        timestamps_accum = (unsigned int *)malloc(new_height * timestamp_width);
        timestamps_accum[0] = tstart;
        for (int i = 1; i < new_height; i++) {
          timestamps_accum[i] = tstart + (unsigned int)(ratio * i);
        }
        
        size.width = new_width;
        size.height = new_height;        
        dst = cvCreateImage(size, IPL_DEPTH_32F, 1);
        dst_data = (float*)dst->imageData;
        dst_stride = (int)(dst->widthStep / sizeof(float));
        
        dst_pos = dst_data;
        int j = starty;
        int row_size = new_width * sizeof(float);
        tmp_pos = tmp_data + starty * width + startx;
        for (int i = 0; i < new_height; i++, dst_pos += dst_stride) {
          if (timestamps[j] < timestamps_accum[i]) {
            j += 1;
            if (j == height) {
              printf("row overflow\n");
              break;
            }
            tmp_pos += tmp_stride;
          }
          memcpy(dst_pos, tmp_pos, row_size);
        }
        
        free(timestamps_accum);
        /*cvSaveImage("tcr_rectified.png", dst, 0);*/
        
        int hidx;
        /* create a cumulative gaussian distribution table for normalizing the histogram */
        float norm[256];
        float nsum = 0;
        for (int i = 0; i < 256; i++) {
          norm[i] = exp(-(pow(((float)i/255.0) - 0.5, 2.0) / (2*0.25*0.25)));
          nsum += norm[i];
        }
        norm[0] /= nsum;
        for (int i = 1; i < 256; i++) {
          norm[i] = (norm[i] / nsum) + norm[i-1];
          /*printf("%f ", norm[i]);*/
        }
        /*printf("\n");*/
        for (int i = 0; i < 256; i++) {
          hist[i] = 0;
        }
        stretch = 1.0 / ((avg+2*sdv) - 0);
        stretch_1 = 0.5 / (avg - 0);
        stretch_2 = 0.5 / (max - avg);
        
        /* normalize values and create the histogram */
        printf("stretch_1=%f stretch_2=%f\n", stretch_1, stretch_2);
        count = 0;
        for (y = 0; y < new_height; y++) {
          dst_pos = dst_data + y * dst_stride;
          for (x = 0; x < new_width; x++, dst_pos++) {
            value = *dst_pos;
            if (value > epsilon) {
              if (value < (avg+epsilon)) {
                value = stretch_1 * (value - 0);
              }
              else {
                value = 0.5 + stretch_2 * (value - avg);
              }
              /*value = stretch * (value - 0);*/
              hidx = (int)(255.0 * value);
              if (hidx < 1) hidx = 1;
              else if (hidx > 255) hidx = 255;
              hist[hidx] += 1;
              count += 1;
              *dst_pos = value;
            }
            else {
              *dst_pos = 0;
            }
          }
        }
        /*cvSaveImage("tcr_normalized.png", dst, 0);*/
        
        for (int i = 1; i < 256; i++) {
          printf("%.3f ", hist[i]);
        }
        printf("\n");
        /* transform the histogram into a cumulative distribution */
        hist[0] = 0;
        for (int i = 1; i < 256; i++) {
          hist[i] = (hist[i] / count) + hist[i-1];
          printf("%.3f ", hist[i]);
        }
        printf("\n");
        
        for (y = 0; y < new_height; y++) {
          dst_pos = dst_data + y * dst_stride;
          for (x = 0; x < new_width; x++, dst_pos++) {
            value = *dst_pos;
            if (value < epsilon) {
              value = 0;
            }
            else {
              hidx = (int)(255.0 * value);
              if (hidx < 1) hidx = 1;
              else if (hidx > 255) hidx = 255;
              value = (hist[hidx] * value); /*norm[hidx] * */
            }
            *dst_pos = value;
          }
        }
        /*cvSaveImage("tcr_stretched.png", dst, 0);*/
      }
    }
    else {
      printf("Failed to allocate memory for image");
    }
    free(mask_idx);
    free(timestamps);
    cvReleaseImage(&tmp);
    cvReleaseImage(&tmp2);
    cvReleaseImage(&tmp3);
    cvReleaseImage(&tmp4);
    cvReleaseImage(&src);
  }
  else {
    printf("Failed to open file %s\n", path);
  }
  
  return dst;
}

IplImage *read_from_tcr_projected(const char *path)
{
  
}

IplImage *read_from_tcr_old(const char *path)
{
  IplImage *tmp, *dst;
  CvSize size;
  float *tmp_data, *dst_data, *tmp_pos, *dst_pos;
  /*unsigned char *b_data, *b_pos;*/
  float hist[256];
  size_t width = 2592;
  size_t height;
  int tmp_stride, dst_stride;/*, b_stride;*/
  int line_width = width * sizeof(float);
  int timestamp_width = sizeof(unsigned int);
  
  tmp = NULL;
  dst = NULL;
  FILE *input_file = fopen(path, "rb");
  size_t readsize;
  int line_count;
  int file_size;
  if (input_file != NULL) {
    unsigned int *timestamps, *timestamps_accum;
    unsigned int timestamp, prev_timestamp, timestamp_diff;
    unsigned int total_timestamp_diff;
    unsigned int total_timestamp_diff_2;
    unsigned int timestamp_count;
    float timestamp_avg, timestamp_sdv;
    
    fseek (input_file , 0 , SEEK_END);
    file_size = ftell (input_file);
    rewind (input_file);
    height = (size_t)(file_size / (line_width + timestamp_width));
    printf("File size: %d, rows: %d\n", file_size, height);
    size.width = width;
    size.height = height;
    tmp = cvCreateImage(size, IPL_DEPTH_32F, 1);
    tmp_data = (float*)tmp->imageData;
    tmp_stride = (int)(tmp->widthStep / sizeof(float));
    timestamps = (unsigned int *)malloc(height * timestamp_width);
    timestamps_accum = (unsigned int *)malloc(height * timestamp_width);
    if (tmp_data != NULL) {
      line_count = 0;
      prev_timestamp = 0;
      total_timestamp_diff = 0;
      total_timestamp_diff_2 = 0;
      timestamp_count = 1;
      timestamps[0] = 0;
      tmp_pos = tmp_data;
      
      while (1) {
        readsize = fread((void*)&timestamp, timestamp_width, 1, input_file);
        /*printf("Read %d elements for timestamp\n", readsize);*/
        if (readsize < 1) {
          break;
        }
        else {
          if (prev_timestamp > 0) {
            timestamp_diff = (timestamp - prev_timestamp);
            /*printf("%d ", timestamp_diff);*/
            total_timestamp_diff += timestamp_diff;
            total_timestamp_diff_2 += (timestamp_diff * timestamp_diff);
            timestamps[timestamp_count] = timestamp_diff;
            timestamp_count += 1;
          }
          prev_timestamp = timestamp;
        }
        readsize = fread((void*)tmp_pos, sizeof(float), width, input_file);
        /*printf("Read %d elements for line\n", readsize);*/
        if (readsize < width) {
          break;
        }
        else {
          line_count += 1;
          tmp_pos += tmp_stride;
        }
      }
      
      fclose(input_file);
      timestamp_avg = total_timestamp_diff / timestamp_count;
      timestamp_sdv = sqrt(total_timestamp_diff_2 / timestamp_count - timestamp_avg*timestamp_avg);
      printf("Read %d lines, avg diff %f, diff sdv %f\n", line_count, timestamp_avg, timestamp_sdv);
      
      timestamps_accum[0] = 0;
      for (int i = i; i < height; i++) {
        timestamps_accum[i] = timestamps[i] / total_timestamp_diff + timestamps_accum[i-1];
      }
      
      {
        unsigned int pos, x, y, px, py, minx, maxx, miny, maxy, sumx, sumy, count;
        unsigned int new_width, new_height;
        float value, min, max, sum, sum2, avg, sdv, ratio, stretch, stretch_1, stretch_2;
        float a, r, rmin, rmax, cx, cy;
        unsigned char bval;
        
        min = 2000000000;
        max = 0;
        sum = 0;
        sum2 = 0;
        minx = 2000000000;
        maxx = 0;
        miny = 2000000000;
        maxy = 0;
        sumx = 0;
        sumy = 0;
        count = 0;
        
        for (y = 0; y < height; y++) {
          tmp_pos = tmp_data + y * tmp_stride;
          for (x = 0; x < width; x++, tmp_pos++) {
            value = *tmp_pos;
            if (value > 0.0001) {
              if (value < min) min = value;
              if (value > max) max = value;
              if (x < minx) minx = x;
              if (x > maxx) maxx = x;
              if (y < miny) miny = y;
              if (y > maxy) maxy = y;
              sum += value;
              sum2 += (value * value);
              sumx += x;
              sumy += y;
              count += 1;
            }
          }
        }
        
        for (int i = 0; i < 256; i++) {
          hist[i] = 0;
        }
        int hidx;
        
        avg = sum / count;
        sdv = sqrt((sum2 / count) - (avg * avg));
        cx = (float)sumx / count;
        cy = (float)sumy / count;
        ratio = (float)(maxy - miny) / (float)(maxx - minx);
        
        printf("amin=%f amax=%f aavg=%f asdv=%f\n", min, max, avg, sdv);
        printf("cx=%f cy=%f ratio=%f\n", cx, cy, ratio);

        stretch = 1.0 / (max - min);
        stretch_1 = 0.2 / (avg - min);
        stretch_2 = 0.8 / (max - avg);
        
        rmin = 2000000000;
        rmax = 0;
        count = 0;
        for (y = 0; y < height; y++) {
          tmp_pos = tmp_data + y * tmp_stride;
          for (x = 0; x < width; x++, tmp_pos++) {
            value = *tmp_pos;
            if (value > 0.0001) {
              px = floor(x - cx);
              py = floor((y / ratio) - (cy / ratio));
              r = sqrt(py*py + px*px);
              if (r < rmin) rmin = r;
              if (r > rmax) rmax = r;
              hidx = (int)(255 * stretch * (value - min));
              if (hidx < 1) hidx = 1;
              else if (hidx > 255) hidx = 255;
              hist[hidx] += 1;
              count += 1;
            }
          }
        }
        
        hist[0] = 0;
        for (int i = 1; i < 256; i++) {
          hist[i] = (hist[i] / count) + hist[i-1];
          printf("%f ", hist[i]);
        }
        
        new_height = ceil(rmax - rmin);
        new_width = ceil(2 * M_PI * rmax);
        printf("w=%d h=%d rmin=%f rmax=%f\n", new_width, new_height, rmin, rmax);
        size.width = new_width;
        size.height = new_height;
        
        dst = cvCreateImage(size, IPL_DEPTH_32F, 1);
        dst_data = (float*)dst->imageData;
        dst_stride = (int)(dst->widthStep / sizeof(float));
        
        for (y = 0; y < new_height; y++) {
          dst_pos = dst_data + y * dst_stride;
          r = (float)(rmax - y);
          for (x = 0; x < new_width; x++, dst_pos++) {
            a = ((float)x / (float)new_width) * 2 * M_PI;
            px = floor(cx + (r * cos(a)));
            py = floor(((cy / ratio) + (r * sin(a))) * ratio);
            if (px < 0 || py < 0 || px >= width || py >= height) {
              /*printf("x=%d y=%d\n", px, py);*/
            }
            else {
              value = *(tmp_data + py * tmp_stride + px);
              if (value < 0.0001) {
                value = 0;
              }
              if (value < avg) {
                value = stretch_1 * (value - min);
                hidx = (int)(255 * value);
                if (hidx < 1) hidx = 1;
                else if (hidx > 255) hidx = 255;
                value = (hist[hidx] * value);
              }
              else {
                value = 0.2 + stretch_2 * (value - avg);
                hidx = (int)(255 * value);
                if (hidx < 1) hidx = 1;
                else if (hidx > 255) hidx = 255;
                value = (hist[hidx] * value);
              }
              *dst_pos = value;
            }
          }
        }
      }
    }
    else {
      printf("Failed to allocate memory for image");
    }
    cvReleaseImage(&tmp);
  }
  else {
    printf("Failed to open file %s\n", path);
  }
  return dst;
}


IplImage *read_from_tcr_sdv(const char *path)
{
  IplImage *dst;
  CvSize size;
  float *data, *data2, *data_pos, *data2_pos;
  size_t width = 2592;
  size_t height;
  int line_width = width * sizeof(float);
  int timestamp_width = sizeof(unsigned int);
  
  dst = NULL;
  FILE *input_file = fopen(path, "rb");
  size_t readsize;
  int line_count;
  int file_size;
  if (input_file != NULL) {
    unsigned int timestamp, prev_timestamp, timestamp_diff;
    unsigned int total_timestamp_diff;
    unsigned int total_timestamp_diff_2;
    unsigned int timestamp_count;
    
    fseek (input_file , 0 , SEEK_END);
    file_size = ftell (input_file);
    rewind (input_file);
    height = (size_t)(file_size / (line_width + timestamp_width));
    printf("File size: %d, rows: %d\n", file_size, height);
      
    data = (float *)malloc(height * width * sizeof(float));
    data2 = (float *)malloc(height * width * sizeof(float));
    memset(data2, 0, height * width * sizeof(float));
    if (data != NULL) {
      line_count = 0;
      prev_timestamp = 0;
      total_timestamp_diff = 0;
      total_timestamp_diff_2 = 0;
      timestamp_count = 0;
      data_pos = data;
      
      while (1) {
        readsize = fread((void*)&timestamp, timestamp_width, 1, input_file);
        /*printf("Read %d elements for timestamp\n", readsize);*/
        if (readsize < 1) {
          break;
        }
        else {
          if (prev_timestamp > 0) {
            timestamp_diff = (timestamp - prev_timestamp);
            total_timestamp_diff += timestamp_diff;
            total_timestamp_diff_2 += (timestamp_diff * timestamp_diff);
            timestamp_count += 1;
          }
          prev_timestamp = timestamp;
        }
        readsize = fread((void*)data_pos, sizeof(float), width, input_file);
        /*printf("Read %d elements for line\n", readsize);*/
        if (readsize < width) {
          break;
        }
        else {
          line_count += 1;
          data_pos += width;
        }
      }
      size.width = width;
      size.height = line_count;
      dst = cvCreateImageHeader(size, IPL_DEPTH_32F, 1);
      cvSetData(dst, (void*)data2, line_width);
      fclose(input_file);
      printf("Read %d lines, average diff %d\n", line_count, (int)(total_timestamp_diff / timestamp_count));
      
      {
        int size, pos, x, y, count, c, c2;
        float amin, amax, smin, smax, asum, asum2, ssum, ssum2, aavg, asdv, savg, ssdv;
        float value,v1,v2,v3,v4,v5,v6,v7,v8,v9,vsum,vsum2,vavg,vavg2,vsdv,vsdv2;
        float stretch_1, stretch_2;
        amin = 2000000000;
        smin = 2000000000;
        amax = 0;
        smax = 0;
        asum = 0;
        asum2 = 0;
        ssum = 0;
        ssum2 = 0;
        count = 0;
        vsum = 0;
        vsum2 = 0;
        vavg = 0;
        vavg2 = 0;
        vsdv = 0;
        vsdv2 = 0;
        for (y = 2; y < height - 2; y++) {
          for (x = 2; x < width - 2; x++) {
            data_pos = &data[y * width + x];
            if (*data_pos > 0.0001) {
              /*
              v1 = *(data_pos - width - 1);
              v2 = *(data_pos - width);
              v3 = *(data_pos - width + 1);
              v4 = *(data_pos - 1);
              v5 = *data_pos;
              v6 = *(data_pos + 1);
              v7 = *(data_pos + width - 1);
              v8 = *(data_pos + width);
              v9 = *(data_pos + width + 1);
              */
              vsum = calc_sum(data_pos, &c);
              /*v1+v2+v3+v4+v5+v6+v7+v8+v9;*/
              vsum2 = calc_sum2(data_pos, &c2);
              /*(v1*v1)+(v2*v2)+(v3*v3)+(v4*v4)+(v5*v5)+(v6*v6)+(v7*v7)+(v8*v8)+(v9*v9);*/
              vavg = vsum / c;
              vavg2 = vavg * vavg;
              if (vavg < amin) amin = vavg;
              else if (vavg > amax) amax = vavg;
              asum += vavg;
              asum2 += vavg2;
              
              vsum2 /= c2;
              if (vsum2 < vavg2) {
                vsdv = 0;
              }
              else {
                vsdv = sqrt(vsum2 - vavg2);
              }
              if (vsdv < smin) smin = vsdv;
              else if (vsdv > smax) smax = vsdv;
              ssum += vsdv;
              ssum2 += (vsdv*vsdv);
              count += 1;
            }
          }
        }
        /*
        for (pos = 0; pos < size; pos++) {
          value = data[pos];
          v1 = data[pos-
          if (value < 0.0001) {
            count_0 += 1;
          }
          else {
            if (value < min) min = value;
            if (value > max) max = value;
            sum_b += value;
            sum2_b += (value * value);
            count_b += 1;
          }
        }
        */
        aavg = asum / count;
        asdv = sqrt((asum2 / count) - (aavg * aavg));
        savg = ssum / count;
        ssdv = sqrt((ssum2 / count) - (savg * savg));
        /*
        if (min < (avg - 2 * sdv)) min = (avg - 2 * sdv);
        if (max > (avg + 2 * sdv)) max = (avg + 2 * sdv);
        */
        /*
        stretch_1 = 0.1 / (avg - min);
        stretch_2 = 0.9 / (max - avg);
        */
        printf("amin=%f amax=%f aavg=%f asdv=%f\n", amin, amax, aavg, asdv);
        printf("smin=%f smax=%f savg=%f ssdv=%f\n", smin, smax, savg, ssdv);
        
        smin = 2000000000;
        smax = 0;
        ssum = 0;
        ssum2 = 0;
        count = 0;
        vsum = 0;
        vsum2 = 0;
        for (y = 2; y < height-2; y++) {
          for (x = 2; x < width-2; x++) {
            data_pos = &data[y * width + x];
            data2_pos = &data2[y * width + x];
            if (*data_pos > 0.0001) {
              /*
              v1 = *(data_pos - width - 1);
              v2 = *(data_pos - width);
              v3 = *(data_pos - width + 1);
              v4 = *(data_pos - 1);
              v5 = *data_pos;
              v6 = *(data_pos + 1);
              v7 = *(data_pos + width - 1);
              v8 = *(data_pos + width);
              v9 = *(data_pos + width + 1);
              */
              vsum = calc_sum(data_pos, &c);
              /*v1+v2+v3+v4+v5+v6+v7+v8+v9;*/
              vsum2 = calc_sum2(data_pos, &c2);
              /*(v1*v1)+(v2*v2)+(v3*v3)+(v4*v4)+(v5*v5)+(v6*v6)+(v7*v7)+(v8*v8)+(v9*v9);*/
              vavg = vsum/c;
              vavg2 = vavg*vavg;
              vsum2 /= c2;
              if (vsum2 < vavg2) {
                vsdv = 0;
              }
              else {
                vsdv = sqrt(vsum2 - vavg2);
              }
              /*if (vsdv > savg) {*/
                value = vsdv / ssdv; /* - savg */
                *data2_pos = value;
                if (value > 1) value = 1;
                if (value < smin) smin = value;
                if (value > smax) smax = value;
                ssum += value;
                ssum2 += value*value;
                count += 1;
              /*}*/
            }
          }
        }
        savg = ssum / count;
        ssdv = sqrt((ssum2 / count) - (savg*savg));
        printf("vmin=%f vmax=%f vavg=%f vsdv=%f\n", smin, smax, savg, ssdv);
        
        size = width * height;
        stretch_1 = 1.0 / (smax - smin);
        for (pos = 0; pos < size; pos++) {
          value = data2[pos];
          if (value < 0.0001) {
            value = 0.5;
          }
          else {
            value = stretch_1 * (value - smin);
          }
          data2[pos] = value;
        }
      }
    }
    else {
      printf("Failed to allocate memory for image");
    }
    free(data);
  }
  else {
    printf("Failed to open file %s\n", path);
  }
  return dst;
}
