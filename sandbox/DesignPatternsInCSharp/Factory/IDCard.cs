using System;

namespace Factory
{
    public class IDCard : Product {
        private string m_Owner;
        public string owner
        {
            get { return m_Owner; }
        }

        public IDCard( string owner ) {
            Console.Write( "create a " + owner + "'s card\n" );
            this.m_Owner = owner;
        }

        public void Use() {
            Console.Write( "use a " + owner + "'s card\n" );
        }
    }
}
