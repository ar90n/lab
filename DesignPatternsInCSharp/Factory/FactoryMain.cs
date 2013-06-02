namespace Factory
{
    public class FactoryMain {
        public static void Main( )
        {
            var factory = new IDCardFactory();
            var card1 = factory.CreateProduct( "test" );
            card1.Use();
        }
    }
}
