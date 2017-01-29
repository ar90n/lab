using System;

namespace Adapter
{
    public class Adapter
    {
        public static void Main()
        {
            Print p = new PrintBanner( "Hello" );
            p.PrintWeak();
            p.PrintStrong();
        }
    }
}
