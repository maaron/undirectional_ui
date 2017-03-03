using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace UnidirectionalViewModel
{
    public abstract class ViewModel<TEvent>
    {
        public abstract Command<TEvent> Update(TEvent e);
    }
}
