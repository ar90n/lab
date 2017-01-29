using System;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Ink;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;
using System.Windows.Media.Imaging;

namespace CartoonEffect
{
    public class PrimitiveFilters
    {
        public static bool EdgeFilter(WriteableBitmap outputBitmap, WriteableBitmap inputBitmap, int threshold )
        {
            for (int srcY = 1; srcY < inputBitmap.PixelHeight - 1; srcY++)
            {
                for (int srcX = 1; srcX < inputBitmap.PixelWidth - 1; srcX++)
                {
                    var pixel0 = inputBitmap.Pixels[ srcY * inputBitmap.PixelWidth + srcX - 1];
                    var redPixel0 = (pixel0 >> 16) & 0xff;
                    var greenPixel0 = (pixel0 >> 8) & 0xff;
                    var bluePixel0 = (pixel0 & 0xff);
                    var pixel1 = inputBitmap.Pixels[srcY * inputBitmap.PixelWidth + srcX + 1];
                    var redPixel1 = (pixel1 >> 16) & 0xff;
                    var greenPixel1 = (pixel1 >> 8) & 0xff;
                    var bluePixel1 = (pixel1 & 0xff);
                    var pixel2 = inputBitmap.Pixels[(srcY - 1 ) * inputBitmap.PixelWidth + srcX];
                    var redPixel2 = (pixel2 >> 16) & 0xff;
                    var greenPixel2 = (pixel2 >> 8) & 0xff;
                    var bluePixel2 = (pixel2 & 0xff);
                    var pixel3 = inputBitmap.Pixels[(srcY + 1 ) * inputBitmap.PixelWidth + srcX];
                    var redPixel3 = (pixel3 >> 16) & 0xff;
                    var greenPixel3 = (pixel3 >> 8) & 0xff;
                    var bluePixel3 = (pixel3 & 0xff);

                    var diffRed = Math.Abs(redPixel1 - redPixel0) + Math.Abs(redPixel3 - redPixel2);
                    var diffGreen = Math.Abs(greenPixel1 - greenPixel0) + Math.Abs(greenPixel3 - greenPixel2);
                    var diffBlue = Math.Abs(bluePixel1 - bluePixel0) + Math.Abs(bluePixel3 - bluePixel2);
                    var sumDiff = diffRed + diffGreen + diffBlue;

                    outputBitmap.Pixels[srcY * outputBitmap.PixelWidth + srcX] = (int)( ( threshold < sumDiff ) ? 0xff000000 : 0xffffffff );
                }
            }

            return true;
        }

        public static bool DomainTransformFilter(WriteableBitmap outputBitmap, WriteableBitmap inputBitmap, double sigI, double sigSpa, int N)
        {
            var numOfPixels = inputBitmap.PixelWidth * inputBitmap.PixelHeight;
            var domainXDistance = new double[numOfPixels];
            var domainYDistance = new double[numOfPixels];

            double ave = 0.0;
            double sd = 0.0;
            for (int offset = 0; offset < inputBitmap.Pixels.Length; offset++)
            {
                var pixel = inputBitmap.Pixels[offset];
                var redPixel = (pixel >> 16) & 0xff;
                var greenPixel = (pixel >> 8) & 0xff;
                var bluePixel = (pixel & 0xff);
                var gray = (redPixel + greenPixel + bluePixel) / 3.0;

                ave += gray;
                sd += gray * gray;
            }

            ave /= numOfPixels;
            sd /= numOfPixels;
            sd -= ave * ave;
            sd = Math.Sqrt(sd);


            var spaceDistL1 = sigSpa / (sigI * sd);
            for (int srcY = 0; srcY < inputBitmap.PixelHeight; srcY++)
            {
                var offsetToHeadOfLine = srcY * inputBitmap.PixelWidth;
                domainXDistance[offsetToHeadOfLine] = 0.0;

                var prevPixel = inputBitmap.Pixels[offsetToHeadOfLine];
                var prevRed = (prevPixel >> 16) & 0xff;
                var prevGreen = (prevPixel >> 8) & 0xff;
                var prevBlue = (prevPixel & 0xff);

                for (int srcX = 1; srcX < inputBitmap.PixelWidth; srcX++)
                {
                    var currentPixel = inputBitmap.Pixels[offsetToHeadOfLine + srcX];
                    var currentRed = (currentPixel >> 16) & 0xff;
                    var currentGreen = (currentPixel >> 8) & 0xff;
                    var currentBlue = (currentPixel & 0xff);

                    var absDiffRed = Math.Abs(currentRed - prevRed);
                    var absDiffGreen = Math.Abs(currentGreen - prevGreen);
                    var absDiffBlue = Math.Abs(currentBlue - prevBlue);
                    var distL1 = spaceDistL1 * (absDiffRed + absDiffGreen + absDiffBlue);

                    domainXDistance[offsetToHeadOfLine + srcX] = domainXDistance[offsetToHeadOfLine + srcX - 1] + 1.0 + distL1;

                    prevRed = currentRed;
                    prevGreen = currentGreen;
                    prevBlue = currentBlue;
                }
            }

            for (int srcX = 0; srcX < inputBitmap.PixelWidth; srcX++)
            {
                var offsetToHeadOfLine = srcX;
                domainYDistance[offsetToHeadOfLine] = 0.0;

                var prevPixel = inputBitmap.Pixels[offsetToHeadOfLine];
                var prevRed = (prevPixel >> 16) & 0xff;
                var prevGreen = (prevPixel >> 8) & 0xff;
                var prevBlue = (prevPixel & 0xff);
                for (int srcY = 1; srcY < inputBitmap.PixelHeight; srcY++)
                {
                    var currentPixel = inputBitmap.Pixels[offsetToHeadOfLine + srcY * inputBitmap.PixelWidth];
                    var currentRed = (currentPixel >> 16) & 0xff;
                    var currentGreen = (currentPixel >> 8) & 0xff;
                    var currentBlue = (currentPixel & 0xff);

                    var absDiffRed = Math.Abs(currentRed - prevRed);
                    var absDiffGreen = Math.Abs(currentGreen - prevGreen);
                    var absDiffBlue = Math.Abs(currentBlue - prevBlue);
                    var distL1 = spaceDistL1 * (absDiffRed + absDiffGreen + absDiffBlue);

                    domainYDistance[offsetToHeadOfLine + srcY * inputBitmap.PixelWidth] = domainYDistance[offsetToHeadOfLine + (srcY - 1) * inputBitmap.PixelWidth] + 1.0 + distL1;

                    prevRed = currentRed;
                    prevGreen = currentGreen;
                    prevBlue = currentBlue;
                }
            }

            double SQRT3 = Math.Sqrt(3.0);
            double SQRT4N1 = Math.Sqrt(Math.Pow(4.0, N) - 1.0);
            var tmpBitmap = new WriteableBitmap(inputBitmap.PixelWidth, inputBitmap.PixelHeight);

            /* 分離実装の繰り返し処理 */
            for (int ite = 0; ite < N; ite++)
            {
                /* フィルタの標準偏差パラメータ */
                double sigmaH = sigSpa * SQRT3 * (Math.Pow(2.0, N - (ite + 1.0))) / SQRT4N1;
                /* フィルタ半径 */
                double radius = sigmaH * SQRT3;

                if (ite == 0)
                {
                    for (int i = 0; i < inputBitmap.PixelHeight; i++)
                    {
                        RunNC_X(inputBitmap, tmpBitmap, i, radius, domainXDistance);
                    }
                    for (int j = 0; j < inputBitmap.PixelWidth; j++)
                    {
                        RunNC_Y(tmpBitmap, outputBitmap, j, radius, domainYDistance);
                    }
                }
                else
                {
                    for (int i = 0; i < inputBitmap.PixelHeight; i++)
                    {
                        RunNC_X(outputBitmap, tmpBitmap, i, radius, domainXDistance);
                    }
                    for (int j = 0; j < inputBitmap.PixelWidth; j++)
                    {
                        RunNC_Y(tmpBitmap, outputBitmap, j, radius, domainYDistance);
                    }
                }
            }

            return true;
        }

        /* 移動平均によるBox関数の高速畳み込み x方向*/
        private static void RunNC_X(WriteableBitmap inputBitmap, WriteableBitmap outputBitmap, int i, double radius, double[] domainX)
        {
            int offsetToHeadOfLine = i * inputBitmap.PixelWidth;
            int left = 0;
            int right = 0;
            int tmpPixel = inputBitmap.Pixels[offsetToHeadOfLine];
            int sumRed = (tmpPixel >> 16) & 0xff;
            int sumGreen = (tmpPixel >> 8) & 0xff;
            int sumBlue = (tmpPixel & 0xff);
            int sumN = 1;

            for (int kk = 0; kk < inputBitmap.PixelWidth; kk++)
            {
                int TMPright = right;
                /* 範囲内を加算 */
                for (int j = TMPright + 1; j < inputBitmap.PixelWidth; j++)
                {
                    double dis = domainX[offsetToHeadOfLine + j] - domainX[offsetToHeadOfLine + kk];
                    if (Math.Abs(dis) <= radius)
                    {
                        tmpPixel = inputBitmap.Pixels[offsetToHeadOfLine + j];
                        sumRed += (tmpPixel >> 16) & 0xff;
                        sumGreen += (tmpPixel >> 8) & 0xff;
                        sumBlue += (tmpPixel & 0xff);

                        sumN++;
                        right++;
                    }
                    else
                    {
                        break;
                    }
                }
                int TMPleft = left;
                /* 範囲外を減算 */
                for (int j = TMPleft; j < kk; j++)
                {
                    double dis = domainX[offsetToHeadOfLine + j] - domainX[offsetToHeadOfLine + kk];
                    if (Math.Abs(dis) > radius)
                    {
                        tmpPixel = inputBitmap.Pixels[offsetToHeadOfLine + j];
                        sumRed -= (tmpPixel >> 16) & 0xff;
                        sumGreen -= (tmpPixel >> 8) & 0xff;
                        sumBlue -= (tmpPixel & 0xff);

                        sumN--;
                        left++;
                    }
                    else
                    {
                        break;
                    }
                }

                var pixelRed = (uint)((double)sumRed / sumN);
                var pixelGreen = (uint)((double)sumGreen / sumN);
                var pixelBlue = (uint)((double)sumBlue / sumN);
                outputBitmap.Pixels[offsetToHeadOfLine + kk] = (int)(0xff000000 | (pixelRed << 16) | (pixelGreen << 8) | pixelBlue);
            }
        }

        /* 移動平均によるBox関数の高速畳み込み y方向*/
        private static void RunNC_Y(WriteableBitmap inputBitmap, WriteableBitmap outputBitmap, int j, double radius, double[] domainY)
        {
            int offsetToHeadOfLine = j;
            int left = 0;
            int right = 0;
            int tmpPixel = inputBitmap.Pixels[offsetToHeadOfLine];
            int sumRed = (tmpPixel >> 16) & 0xff;
            int sumGreen = (tmpPixel >> 8) & 0xff;
            int sumBlue = (tmpPixel & 0xff);
            int sumN = 1;

            for (int kk = 0; kk < inputBitmap.PixelHeight; kk++)
            {
                int TMPright = right;
                /* 範囲内を加算 */
                for (int i = TMPright + 1; i < inputBitmap.PixelHeight; i++)
                {
                    double dis = domainY[offsetToHeadOfLine + i * inputBitmap.PixelWidth]
                               - domainY[offsetToHeadOfLine + kk * inputBitmap.PixelWidth];
                    if (Math.Abs(dis) <= radius)
                    {
                        tmpPixel = inputBitmap.Pixels[offsetToHeadOfLine + i * inputBitmap.PixelWidth];
                        sumRed += (tmpPixel >> 16) & 0xff;
                        sumGreen += (tmpPixel >> 8) & 0xff;
                        sumBlue += (tmpPixel & 0xff);

                        sumN++;
                        right++;
                    }
                    else
                    {
                        break;
                    }
                }
                int TMPleft = left;
                /* 範囲外を減算 */
                for (int i = TMPleft; i < kk; i++)
                {
                    double dis = domainY[offsetToHeadOfLine + i * inputBitmap.PixelWidth]
                               - domainY[offsetToHeadOfLine + kk * inputBitmap.PixelWidth];
                    if (Math.Abs(dis) > radius)
                    {
                        tmpPixel = inputBitmap.Pixels[offsetToHeadOfLine + i * inputBitmap.PixelWidth];
                        sumRed -= (tmpPixel >> 16) & 0xff;
                        sumGreen -= (tmpPixel >> 8) & 0xff;
                        sumBlue -= (tmpPixel & 0xff);

                        sumN--;
                        left++;
                    }
                    else
                    {
                        break;
                    }
                }

                var pixelRed = (uint)((double)sumRed / sumN);
                var pixelGreen = (uint)((double)sumGreen / sumN);
                var pixelBlue = (uint)((double)sumBlue / sumN);
                outputBitmap.Pixels[offsetToHeadOfLine + kk * inputBitmap.PixelWidth] = (int)(0xff000000 | (pixelRed << 16) | (pixelGreen << 8) | pixelBlue);
            }
        }

        internal static void AndImage(WriteableBitmap dstBitmap, WriteableBitmap dstBitmap_2, WriteableBitmap tmpBitmap)
        {
            for (int offset = 0; offset < dstBitmap.Pixels.Length; offset++)
            {
                var pixelValue = dstBitmap_2.Pixels[offset] & tmpBitmap.Pixels[offset];
                dstBitmap.Pixels[offset] = pixelValue;
            }
        }
    }
}
