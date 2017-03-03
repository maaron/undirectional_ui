using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace UnidirectionalViewModel
{
    public interface UserControl1Event
    {
        R Match<R>(Func<ButtonClicked, R> f1);
    }

    public class ButtonClicked : UserControl1Event
    {
        public R Match<R>(Func<ButtonClicked, R> f1)
        {
            return f1(this);
        }
    }

    public class UserControl1ViewModel
    {
        public string Name { get; set; }

        public string Value { get; set; }

        public Command<UserControl1Event> Update(UserControl1Event e)
        {
            return Command.None<UserControl1Event>();
        }
    }
}
