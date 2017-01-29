using System;

namespace Adapter
{
    public class Banner
    {
        private string m_String;

        public Banner( string str )
        {
            m_String = str;
        }

        public void ShowWithParen()
        {
            Console.Write( "(" + m_String + ")\n" );
        }

        public void ShowWithAster()
        {
            Console.Write( "*" + m_String + "*\n" );
        }
    }
}

