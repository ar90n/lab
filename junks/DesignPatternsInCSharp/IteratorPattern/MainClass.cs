using System;

namespace Section1
{
    class MainClass
    {
        static void Main()
        {
            BookShelf bookShelf = new BookShelf();
            bookShelf.AppendBook( new Book( "Around the World in 80 Days" ) );
            bookShelf.AppendBook( new Book( "Bible" ) );
            bookShelf.AppendBook( new Book( "Cinderella" ) );
            bookShelf.AppendBook( new Book( "Daddy-Long-Legs" ) );

            Iterator it = bookShelf.Iterator();
            while( it.HasNext() )
            {
                Book book = ( Book )it.Next();
                System.Console.WriteLine( book.name );
            }

        }
    }
}
