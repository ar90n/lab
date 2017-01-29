using System;

namespace Adapter
{
    public class PrintBanner : Print
    {
        public PrintBanner( string str )
        {
            Banner tmpBanner = new Banner( str );
            PrintWeak = tmpBanner.ShowWithParen;
            PrintStrong = tmpBanner.ShowWithAster;
        }
    }
}
