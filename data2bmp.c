#include "stdio.h"
#include "stdlib.h"
#include "fcntl.h"

#define PIXELSIZE  3

/* color types */

#define RED  0
#define GREEN  1
#define BLUE  2

/* file layout variables */

char letterB;
char letterM;
int fileSize;
short reserved1;
short reserved2;
unsigned short pixelArrayOffset;
short reserved3;
int structSize;
int imageWidth;
int imageHeight;
short nPlanes;
short bitCount;
int compression;
int imageSize;
int XpixelsPerMeter;
int YpixelsPerMeter;
int colorsUsed;
int colorsImportant;

/* image storage */

unsigned char *sourceImage;

/* other variables */

int savefd;

int scanLinePadSize;

void printHelp() {
  printf("dataToBmp\n");
  printf("  <destination .bmp file>\n");
  printf("\n");
}

void getArgs(int argc, char *argv []) {
  int argIdx;
  char *arg;

  if (argc == 1) { /* no args */
    printHelp();
    exit(0);
  }

  if (argc != 2) {
    printf("need destination .bmp file\n");
    exit(0);
  }

  #ifdef UNIX
  savefd = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC, 0600);
  #else
  savefd = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0600);
  #endif
  if (savefd == -1) {
    printf("failed to create file %s\n", argv[1]);
    exit(0);
  } 
}

void saveByte(int fd, char arg) {
  write(fd, &arg, 1); 
}

void saveShort(int fd, short arg) {
  #ifdef SWAP
  char *bytes;

  bytes = (char *)&arg;
  write(fd, bytes + 1, 1); 
  write(fd, bytes, 1); 
  #else
  write(fd, &arg, 2); 
  #endif
}

void saveInteger(int fd, int arg) {
  #ifdef SWAP
  char *bytes;

  bytes = (char *)&arg;
  write(fd, bytes + 3, 1); 
  write(fd, bytes + 2, 1); 
  write(fd, bytes + 1, 1); 
  write(fd, bytes, 1); 
  #else
  write(fd, &arg, 4); 
  #endif
}

void initStructValues(int inputHeight, int inputWidth) {
  scanLinePadSize = inputWidth % 4;
  letterB = 'B';
  letterM = 'M';
  fileSize = 54 + (inputWidth * PIXELSIZE + scanLinePadSize) * inputHeight;
  reserved1 = 0;
  reserved2 = 0;
  pixelArrayOffset = 54;
  reserved3 = 0;
  structSize = 40;
  imageWidth = inputWidth;
  imageHeight = inputHeight;
  nPlanes = 1;
  bitCount = 24;
  compression = 0;
  imageSize = 0;
  XpixelsPerMeter = 0;
  YpixelsPerMeter = 0;
  colorsUsed = 0;
  colorsImportant = 0;
}

unsigned char getPixel(
  int i, int j, int color) {

  char *pixelPosition;
  pixelPosition = sourceImage + (i * imageWidth + j) * PIXELSIZE;

  if (color == RED)
    return (*(pixelPosition + 2));
  if (color == GREEN)
    return (*(pixelPosition + 1));
  /* BLUE */
  return (*pixelPosition);
}

void readData() {
  int widthIdx;
  int heightIdx;
  int imageIdx;
  int pixel;

  scanf("%d", &imageWidth);
  scanf("%d", &imageHeight);

  sourceImage = malloc(imageHeight * imageWidth * PIXELSIZE);
  if (sourceImage == NULL) {
    printf("not enough memory\n");
    exit(0);
  }

  imageIdx = 0;
  for (heightIdx = 0; heightIdx < imageHeight; heightIdx++)
    for (widthIdx = 0; widthIdx < imageWidth; widthIdx++) {
      scanf("%d", &pixel);
      sourceImage[imageIdx] = (pixel & 0x00ff0000) >> 16;
      sourceImage[imageIdx + 1] = (pixel & 0x0000ff00) >> 8;
      sourceImage[imageIdx + 2] = pixel & 0x000000ff;
      imageIdx += 3;
    }
}

void saveBinary(int fd) {
  int heightIdx;
  int scanLinePadIdx;
  int scanLineLength;
  char *scanLinePosition;
  int padValue;

  saveByte(fd, letterB);
  saveByte(fd, letterM);
  saveInteger(fd, fileSize);
  saveShort(fd, reserved1);
  saveShort(fd, reserved2);
  saveShort(fd, pixelArrayOffset);
  saveShort(fd, reserved3);
  saveInteger(fd, structSize);
  saveInteger(fd, imageWidth);
  saveInteger(fd, imageHeight);
  saveShort(fd, nPlanes);
  saveShort(fd, bitCount);
  saveInteger(fd, compression);
  saveInteger(fd, imageSize);
  saveInteger(fd, XpixelsPerMeter);
  saveInteger(fd, YpixelsPerMeter);
  saveInteger(fd, colorsUsed);
  saveInteger(fd, colorsImportant);

  padValue = 0;
  scanLineLength = imageWidth * PIXELSIZE;
  scanLinePosition = sourceImage + (imageHeight - 1) * scanLineLength;
  for (heightIdx = imageHeight - 1; heightIdx >= 0; heightIdx--) {
    write(fd, scanLinePosition, scanLineLength); 
    scanLinePosition -= scanLineLength;
    if (scanLinePadSize != 0)
      write(fd, &padValue, scanLinePadSize); 
  }
}

main(int argc, char *argv []) {
  getArgs(argc, argv);

  readData();

  initStructValues(imageHeight, imageWidth);
  saveBinary(savefd);
  close(savefd);

  free(sourceImage);
}
