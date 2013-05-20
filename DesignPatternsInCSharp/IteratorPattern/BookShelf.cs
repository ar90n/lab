using System;

namespace Section1
{
    public class BookShelf : Aggregate
    {
        private Book[] m_Books;
        private int m_Last = 0;

        public BookShelf( int maxsize )
        {
            this.m_Books = new Book[ maxsize ];
        }

        public Book GetBookAt( int index )
        {
            return m_Books[ index ];
        }

        public void AppendBook( Book book )
        {
            m_Books[ m_Last ] = book;
            m_Last++;
        }

        public int getLength()
        {
            return m_Last;
        }

        public Iterator iterator()
        {
            return new BookShelfIterator( this );
        }
    }
}
