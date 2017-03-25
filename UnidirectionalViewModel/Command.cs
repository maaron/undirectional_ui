using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace UnidirectionalViewModel
{
    public delegate void Command<E>(Action<E> action);

    public static class Command
    {
        public static Command<E> None<E>()
        {
            return action => { };
        }

        public static Command<R> Select<E, R>(this Command<E> source, Func<E, R> f)
        {
            return action => source(e => action(f(e)));
        }

        public static Command<E> Batch<E>(IEnumerable<Command<E>> source)
        {
            return action =>
            {
                foreach (var cmd in source)
                    cmd(action);
            };
        }

        public static Command<E> And<E>(this Command<E> first, Command<E> second)
        {
            return action =>
            {
                first(action);
                second(action);
            };
        }
    }

    public interface ICmd<E> : IEnumerable<ICmd<E>>
    {
        void Run(Action<E> action);
    }

    public abstract class DataCmd<D, E> : ICmd<E>
    {
        public D Data { get; private set; }
        public abstract void Run(Action<E> action);

        public override string ToString()
        {
            return Data == null ? "null" : Data.ToString();
        }

        public override bool Equals(object obj)
        {
            return 
                obj != null 
                && obj is DataCmd<D, E> 
                && Data.Equals((DataCmd<D, E>)obj);
        }

        public override int GetHashCode()
        {
            return Data == null ? 0 : Data.GetHashCode();
        }

        public IEnumerator<ICmd<E>> GetEnumerator()
        {
            yield return this;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            yield return this;
        }
    }
}
