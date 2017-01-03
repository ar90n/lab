using System;
using System.Collections.Generic;

namespace Section1
{
    public class BookShelf : Aggregate
    {
        private List< Book > m_Books;

        public BookShelf( )
        {
            this.m_Books = new List< Book >();
        }

        public Book GetBookAt( int index )
        {
            return m_Books[ index ];
        }

        public void AppendBook( Book book )
        {
            m_Books.Add( book );
        }

        public int GetLength()
        {
            return m_Books.Count;
        }

        public Iterator Iterator()
        {
            return new BookShelfIterator( this );
        }
    }
}
