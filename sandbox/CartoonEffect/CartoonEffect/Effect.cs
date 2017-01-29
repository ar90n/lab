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
using System.Diagnostics;

namespace CartoonEffect
{
    public abstract class Effect
    {
        protected WriteableBitmap srcBitmap;

        public Effect() { }
        public Effect(WriteableBitmap bitmap) { srcBitmap = bitmap; Debug.WriteLine("constructor of super class called"); }
        public abstract bool preProcess();
        public abstract bool process(WriteableBitmap dstBitmap);
    }
}
