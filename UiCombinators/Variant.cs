using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace UiCombinators
{
    public interface IVariant<T1, T2>
    {
        R Map<R>(Func<T1, R> f1, Func<T2, R> f2);
    }

    public class Variant1<T1, T2> : IVariant<T1, T2>
    {
        public T1 Value { get; private set; }

        public R Map<R>(Func<T1, R> f1, Func<T2, R> f2)
        {
            return f1(Value);
        }
    }

    public class Variant2<T1, T2> : IVariant<T1, T2>
    {
        public T2 Value { get; private set; }

        public R Map<R>(Func<T1, R> f1, Func<T2, R> f2)
        {
            return f2(Value);
        }
    }
}
