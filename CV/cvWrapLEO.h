//@+leo-ver=4-thin
//@+node:aleator.20050908101148.2:@thin cvWrapLEO.h
//@@language c
#ifndef __CVWRAP__
#define __CVWRAP__

#include <cv.h>
#include <cxtypes.h>
#include <highgui.h>
#include <complex.h>

IplImage* wrapCreateImage32F(const int width, const int height, const int channels);
IplImage* wrapCreateImage64F(const int width, const int height, const int channels);

IplImage* wrapCreateImage8U(const int width, const int height, const int channels);

void wrapSubRS(const CvArr *src, double s,CvArr *dst);
void wrapSubS(const CvArr *src, double s,CvArr *dst);
void wrapAddS(const CvArr *src, double s, CvArr *dst);

double wrapAvg(const CvArr *src);
double wrapStdDev(const CvArr *src);
double wrapStdDevMask(const CvArr *src,const CvArr *mask);
double wrapSum(const CvArr *src);
void wrapMinMax(const CvArr *src,const CvArr *mask
               ,double *minVal, double *maxVal);
void wrapAbsDiffS(const CvArr *src, double s, CvArr *dst);

void wrapSetImageROI(IplImage *i,int x, int y, int w, int h);

IplImage* wrapSobel(IplImage *src,int dx
                   ,int dy,int size);

IplImage* wrapLaplace(IplImage *src,int size);

IplImage* ensure8U(const IplImage *src);
IplImage* ensure32F(const IplImage *src);

void wrapSet32F2D(CvArr *arr, int x, int y, double value);
double wrapGet32F2D(CvArr *arr, int x, int y);

void wrapDrawCircle(CvArr *img, int x, int y, int radius, double color, int thickness);

void wrapDrawLine(CvArr *img, int x, int y, int x1, int y1, double color, int thickness);

void wrapFillPolygon(IplImage *img, int pc, int *xs, int *ys, double color);

void wrapMatMul(int w, int h, double *mat
               , double *vec, double *t);

// Utils. Place them in another file
IplImage* rotateImage(IplImage* src,double scale,double angle);
CvHistogram* calculateHistogram(IplImage *img,int bins);
void wrapReleaseHist(CvHistogram *hist);
double getHistValue(CvHistogram *h,int bin);
void get_histogram(IplImage *img,IplImage *mask
                 ,float a, float b,int isCumulative
                 ,int binCount
                 ,double *values);

IplImage* getSubImage(IplImage *img, int sx,int sy,int w,int h);
int getImageHeight(IplImage *img);
int getImageWidth(IplImage *img);


IplImage* susanSmooth(IplImage *src, int w, int h
                     ,double t, double sigma);

IplImage* susanEdge(IplImage *src,int w,int h,double t);
IplImage* getNthCentralMoment(IplImage *src, int n, int w, int h);
IplImage* getNthAbsCentralMoment(IplImage *src, int n, int w, int h);
IplImage* getNthMoment(IplImage *src, int n, int w, int h);

double calcGabor(double x, double y
                ,double stdX, double stdY
                ,double theta, double phase
                ,double cycles);

void gaborFilter(const CvArr *src, CvArr *dst
                ,int maskWidth, int maskHeight
                ,double stdX, double stdY
                ,double theta,double phase
                ,double cycles);

void radialGaborFilter(const CvArr *src, CvArr *dst
                ,int maskWidth, int maskHeight
                ,double sigma
                ,double phase,double center
                ,double cycles);

void renderRadialGabor(CvArr *dst,int width, int height
                ,double sigma
                ,double phase, double center
                ,double cycles);

void render_gaussian(IplImage *dst
                   ,double stdX, double stdY);

void renderGabor(CvArr *dst,int width, int height
                ,double dx, double dy
                ,double stdX, double stdY
                ,double theta, double phase
                ,double cycles);

void smb(IplImage *image,double t);
void smab(IplImage *image,int w, int h,double t);

IplImage* selectiveAvgFilter(IplImage *src,double t
                            ,int wwidth, int wheight);

IplImage* wrapFilter2D(IplImage *src, int ax,int ay, 
                    int w, int h, double *kernel);
IplImage* wrapFilter2DImg(IplImage *src
                         ,IplImage *mask
                         ,int ax,int ay);

void wrapFloodFill(IplImage *i, int x, int y, double c
                  ,double low, double high,int fixed);

void sqrtImage(IplImage *src,IplImage *dst);

void weighted_localBinaryPattern(IplImage *src,int offsetX,int offsetXY
                                , IplImage* weights, double *LBP);

void localBinaryPattern(IplImage *src, int *LBP);
void localBinaryPattern3(IplImage *src, int *LBP);
void localBinaryPattern5(IplImage *src, int *LBP);
void localHorizontalBinaryPattern(IplImage *src, int *LBP);
void localVerticalBinaryPattern(IplImage *src, int *LBP);

void get_weighted_histogram(IplImage *src, IplImage *weights, 
                       double start, double end, 
                       int bins, double *histo);


void eigenValsViaSVD(double *A, int size, double *eVals
                    ,double *eVects);

IplImage* sizeFilter(IplImage *src, double minSize, double maxSize);
int blobCount(IplImage *src);


IplImage *acquireImage(int w, int h, double *d);

void wrapProbHoughLines(IplImage *img, double rho, double theta
                       , int threshold, double minLength
                       , double gapLength
                       , int *maxLines
                       , int *xs, int *ys
                       , int *xs1, int *ys1);


double average_of_line(int x0, int y0
                     ,int x1, int y1
                     ,IplImage *src);
                     
IplImage* adaUpdateDistrImage(IplImage *target
                          ,IplImage *weigths
                          ,IplImage *test
                          ,double at);

double adaFitness1(IplImage *target
                 ,IplImage *weigths
                 ,IplImage *test);
           
CvMoments* getMoments(IplImage *src, int isBinary);

void freeCvMoments(CvMoments *x);

void getHuMoments(CvMoments *src,double *hu);

void freeCvHuMoments(CvHuMoments *x);

void haarFilter(IplImage *intImg, 
                int a, int b, int c, int d,
                IplImage *target);

double haar_at(IplImage *intImg, 
                int x1, int y1, int w, int h);

void wrapDrawRectangle(CvArr *img, int x1, int y1, 
                       int x2, int y2, double color,
                       int thickness);
                       
void calculateAtan(IplImage *src, IplImage *dst);


// Contours
typedef struct {
  CvMemStorage *storage;
  CvSeq *contour;
  CvSeq *start;
  
} FoundContours;

CvMoments* contour_moments(FoundContours *f);
void contour_points(FoundContours *f, int *xs, int *ys);
CvMoments* contour_Moments(FoundContours *f);
int cur_contour_size(FoundContours *f);
double contour_area(FoundContours *f);
double contour_perimeter(FoundContours *f);
int more_contours(FoundContours *f);
int next_contour(FoundContours *f);
int reset_contour(FoundContours *f);
void free_found_contours(FoundContours *f);
void get_next_contour(FoundContours *fc);
void print_contour(FoundContours *fc);
FoundContours* get_contours(IplImage *src);

double juliaF(double a, double b,double x, double y);
void simpleMatchTemplate(const IplImage* target, const IplImage* template, int* x, int* y, double *val, int type);
IplImage* templateImage(const IplImage* target, const IplImage* template);
IplImage* simpleMergeImages(IplImage *a, IplImage *b,int offset_x, int offset_y);

void alphaBlit(IplImage *a, IplImage *aAlpha, IplImage *b, IplImage *bAlpha, int offset_x, int offset_y);
void blitImg(IplImage *a, IplImage *b,int offset_x, int offset_y);
IplImage* fadedEdges(int w, int h, int edgeW);
IplImage* rectangularDistance(int w, int h);
void radialRemap(IplImage *source, IplImage *dest, double k);
void plainBlit(IplImage *a, IplImage *b, int offset_y, int offset_x);
void wrapMinMaxLoc(const IplImage* target, int* minx, int* miny, int* maxx, int* maxy, double *minval, double *maxval);
void incrImageC(void);
IplImage* vignettingModelCos4(int w, int h) ;
IplImage* vignettingModelCos4XCyl(int w, int h) ;
IplImage* vignettingModelX2Cyl(int w, int h,double m, double s, double c);
void wrapDrawText(CvArr *img, char *text, float s, int x, int y);

IplImage* vignettingModelB3(int w, int h,double b1, double b2, double b3);
inline CvPoint2D64f toNormalizedCoords(CvSize area, CvPoint from);
inline CvPoint fromNormalizedCoords(CvSize area, CvPoint2D64f from);
inline double eucNorm(CvPoint2D64f p);
IplImage* vignettingModelP(int w, int h,double scalex, double scaley, double max);
IplImage* wrapPerspective(IplImage* src, double a1, double a2, double a3
                                       , double a4, double a5, double a6
                                       , double a7, double a8, double a9);
IplImage* simplePerspective(double k,IplImage *src);
double bilinearInterp(IplImage *tex, double u, double v);
inline CvPoint2D64f fromNormalizedCoords64f(CvSize area, CvPoint2D64f from);
void findHomography(double* srcPts, double *dstPts, int noPts, double *homography);
void masked_merge(IplImage *src1, IplImage *mask, IplImage *src2, IplImage *dst);
IplImage* makeEvenUp(IplImage *src);
IplImage* padUp(IplImage *src,int right, int bottom);
IplImage* makeEvenDown(IplImage *src);
void vertical_average(IplImage *src1, IplImage *dst);

IplImage* composeMultiChannel(IplImage* img0
                             ,IplImage* img1
                             ,IplImage* img2
                             ,IplImage* img3
                             ,const int channels);

IplImage *acquireImageSlow(int w, int h, double *d);
void exportImageSlow(IplImage *img, double *d);

IplImage *acquireImageSlowComplex(int w, int h, complex double *d);
void exportImageSlowComplex(IplImage *img, complex double *d);
void subpixel_blit(IplImage *a, IplImage *b, double offset_y, double offset_x);
double bicubicInterp(IplImage *tex, double u, double v);
#endif
//@-node:aleator.20050908101148.2:@thin cvWrapLEO.h
//@-leo
