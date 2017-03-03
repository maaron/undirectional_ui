using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace UnidirectionalViewModel
{
    public abstract class AppEvent
    {
        public abstract R Match<R>(Func<UserControl1Event, R> user1);

        public static AppEvent User1(UserControl1Event e)
        {
            return new User1Case(e);
        }
    }

    public class User1Case : AppEvent
    {
        public UserControl1Event Value { get; private set; }

        public User1Case(UserControl1Event value)
        {
            Value = value;
        }

        public override R Match<R>(Func<UserControl1Event, R> user1)
        {
            return user1(Value);
        }
    }

    public class AppViewModel : ViewModel<AppEvent>
    {
        public UserControl1ViewModel User1 { get; set; }

        public string Name { get; set; }

        public override Command<AppEvent> Update(AppEvent e)
        {
            return e.Match(user1 => User1.Update(user1).Select(AppEvent.User1));
        }
    }
}
