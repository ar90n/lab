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
    public class EffectFactory
    {
        public  static Effect build( string algorithm, WriteableBitmap srcBitmap )
        {
            Effect tmpEffect = null;

            if (algorithm == "Cartoon")
            {
                tmpEffect = new CartoonEffect(srcBitmap);
            }
            else
            {
                throw new NotImplementedException();
            }

            return tmpEffect;
        }
    }
}
