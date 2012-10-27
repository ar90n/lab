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
using Microsoft.Phone.Tasks;
using System.Windows.Media.Imaging;
using System.Diagnostics;

namespace CartoonEffect
{
    public partial class MainPage : PhoneApplicationPage
    {
        PhotoChooserTask photoChooserTask = new PhotoChooserTask();
        CameraCaptureTask cameraCaptureTask = new CameraCaptureTask();
        bool isImageSelected;

        // コンストラクター
        public MainPage()
        {
            InitializeComponent();

            isImageSelected = false;

            // ListBox コントロールのデータ コンテキストをサンプル データに設定します
            DataContext = App.ViewModel;
            this.Loaded += new RoutedEventHandler(MainPage_Loaded);

            photoChooserTask.Completed += new EventHandler<PhotoResult>(photoChooserTask_Completed);
            cameraCaptureTask.Completed += new EventHandler<PhotoResult>(cameraCaptureTask_Completed);
        }

        void cameraCaptureTask_Completed(object sender, PhotoResult e)
        {
            if (e.TaskResult != TaskResult.OK)
            {
                return;
            }

            isImageSelected = true;

            (Application.Current as App ).selectedBitmap.SetSource( e.ChosenPhoto );
        }

        void photoChooserTask_Completed(object sender, PhotoResult e)
        {
            if (e.TaskResult != TaskResult.OK)
            {
                return;
            }

            isImageSelected = true;

            (Application.Current as App).selectedBitmap.SetSource(e.ChosenPhoto);
        }

        // ViewModel Items のデータを読み込みます
        private void MainPage_Loaded(object sender, RoutedEventArgs e)
        {
            if (!App.ViewModel.IsDataLoaded)
            {
                App.ViewModel.LoadData();
            }

        }

        protected override void OnNavigatedTo( System.Windows.Navigation.NavigationEventArgs e )
        {
            base.OnNavigatedTo(e);

            if (isImageSelected)
            {
                isImageSelected = false;
                string algorithmName = ((PivotItem)rootPivot.SelectedItem).Header.ToString().Split(' ')[0];
                NavigationService.Navigate(new Uri("/ProcessPage.xaml?algorithm=" + algorithmName, UriKind.Relative));
            }
        }

        private void menuItemVersion_Click(object sender, EventArgs e)
        {
            MessageBox.Show( (Application.Current as App ).currentVersion );
        }

        private void menuItemAbout_Click(object sender, EventArgs e)
        {

        }

        private void iconButtonCamera_Click(object sender, EventArgs e)
        {
            cameraCaptureTask.Show();
        }

        private void iconButtonLoad_Click(object sender, EventArgs e)
        {
            photoChooserTask.Show();
        }

        private void iconButtonSetting_Click(object sender, EventArgs e)
        {
        }

        private void rootPivot_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            Debug.WriteLine("SeletectedIndex:" + rootPivot.SelectedIndex);
            Debug.WriteLine("Title:" + rootPivot.Title);
            Debug.WriteLine(((PivotItem)rootPivot.SelectedItem).Header);
        }
    }
}