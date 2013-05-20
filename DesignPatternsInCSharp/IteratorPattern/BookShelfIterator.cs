using System;

namespace Section1
{
    public class BookShelfIterator : Iterator
    {
        private BookShelf m_BookShelf;
        private int m_Index;

        public BookShelfIterator( BookShelf bookShelf )
        {
            m_BookShelf = bookShelf;
            m_Index = 0;
        }

        public bool HasNext()
        {
            if( m_Index < m_BookShelf.GetLength() )
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        public Object Next()
        {
            Book book = m_BookShelf.GetBookAt( m_Index );
            m_Index++;
            
            return book;
        }
    }
}
