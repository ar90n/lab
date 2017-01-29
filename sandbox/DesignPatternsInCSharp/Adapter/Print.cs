using System;

namespace Adapter
{
    public class Print
    {
        public delegate void PrintDecorate();

        public PrintDecorate PrintWeak;
        public PrintDecorate PrintStrong;
    }
}
