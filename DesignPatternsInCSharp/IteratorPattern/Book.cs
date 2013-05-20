using System;

namespace Section1
{
    public class Book
    {
        private string m_Name = "";
        public string name
        {
            get { return m_Name; }
        }

        public Book( string name )
        {
            this.m_Name = name;
        }
    }
}
