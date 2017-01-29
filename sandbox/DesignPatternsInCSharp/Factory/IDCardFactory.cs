using System;
using System.Collections.Generic;

namespace Factory
{
    public class IDCardFactory : Factory {
        private List< string > m_Owners = new List< string >();
        public List< string > owners
        {
            get { return m_Owners; }
        }

        public override Product CreateProduct( string owner ) {
            return new IDCard( owner );
        }

        public override void RegisterProduct( Product product ) {
            m_Owners.Add( ((IDCard)product).owner );
        }
    }
}
