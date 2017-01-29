using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;
using Microsoft.Phone.Controls;
using System.Windows.Media.Imaging;
using System.IO;
using Microsoft.Xna.Framework.Media;
using System.Diagnostics;


namespace CartoonEffect
{
    public partial class Page1 : PhoneApplicationPage
    {
        WriteableBitmap srcBitmap = null;
        WriteableBitmap dstBitmap = null;
        Effect effect = null;

        public Page1()
        {
            InitializeComponent();
        }

        private void previewImg_Tap(object sender, GestureEventArgs e)
        {
            if (previewImg.Source == srcBitmap)
            {
                previewImg.Source = dstBitmap;
            }
            else
            {
                previewImg.Source = srcBitmap;
            }

        }

        protected override void OnNavigatedTo(System.Windows.Navigation.NavigationEventArgs e)
        {
            base.OnNavigatedTo(e);

            string algorithm = "";
            if (NavigationContext.QueryString.TryGetValue("algorithm", out algorithm))
            {
                PageTitle.Text = algorithm + " Effect";

                srcBitmap = new WriteableBitmap( (Application.Current as App).selectedBitmap );
                dstBitmap = new WriteableBitmap( srcBitmap.PixelWidth, srcBitmap.PixelHeight );


                effect = EffectFactory.build(algorithm, srcBitmap);
                effect.preProcess();
                effect.process(dstBitmap);
            }
            else
            {
                throw new NotImplementedException();
            }

            previewImg.Source = dstBitmap;
        }

        private void saveButton_Click(object sender, EventArgs e)
        {
            Debug.WriteLine("saveButton_Click");

            if (dstBitmap != null)
            {
                MemoryStream stream = new MemoryStream();
                dstBitmap.SaveJpeg(stream, dstBitmap.PixelWidth, dstBitmap.PixelHeight, 0, 100);

                using (MediaLibrary lib = new MediaLibrary())
                {
                    lib.SavePicture("Paintize-" + DateTime.Now.ToString("yyyyMMddhhmmss"), stream.ToArray());
                    MessageBox.Show("保存しました");
                }

            }
        }
    }
}