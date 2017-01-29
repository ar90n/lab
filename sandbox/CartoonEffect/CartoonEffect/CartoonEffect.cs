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
    public class CartoonEffect : Effect
    {
        public CartoonEffect(WriteableBitmap bitmap)
            : base(bitmap)
        {
        }

        public override bool preProcess()
        {
            return true;
        }

        public override bool process(WriteableBitmap dstBitmap)
        {
            WriteableBitmap tmpBitmap = new WriteableBitmap(dstBitmap.PixelWidth, dstBitmap.PixelHeight);
            PrimitiveFilters.DomainTransformFilter(dstBitmap, srcBitmap, 2.0, 12.0, 1);
            PrimitiveFilters.EdgeFilter( tmpBitmap, dstBitmap, 32 );
            PrimitiveFilters.AndImage(dstBitmap, dstBitmap, tmpBitmap);

            return true;   
        }
    }
}
