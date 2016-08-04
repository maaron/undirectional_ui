using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TodoElmStyle
{
    public interface IComponent<TModel, TMessage, TView>
    {
        TModel InitialModel { get; }
        TModel Update(TMessage message, TModel model);
        TView View(TModel model);
        Action<TMessage> Message { get; set; }
    }

    public class StatelessComponent<TModel, TMessage, TView> : IComponent<TModel, TMessage, TView>
    {
        private Func<TMessage, TModel, TModel> updateImpl;
        private Func<TModel, TView> viewImpl;

        public StatelessComponent(
            TModel initialModel, 
            Func<TMessage, TModel, TModel> update, 
            Func<TModel, TView> view)
        {
            InitialModel = initialModel;
            updateImpl = update;
            viewImpl = view;
            Message = delegate { };
        }

        public TModel InitialModel { get; private set; }

        public TModel Update(TMessage message, TModel model)
        {
            return updateImpl(message, model);
        }

        public TView View(TModel model)
        {
            return viewImpl(model);
        }

        public Action<TMessage> Message { get; set; }
    }

    public static class Component
    {
        public static StatelessComponent<TModel, TMessage, TView> Create<TModel, TMessage, TView>(
            TModel model,
            Func<TMessage, TModel, TModel> update,
            Func<TModel, TView> view)
        {
            return new StatelessComponent<TModel, TMessage, TView>(model, update, view);
        }
    }
}
