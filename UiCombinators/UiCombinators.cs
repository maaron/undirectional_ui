using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;

namespace UiCombinators
{
    public interface IComponent<TMsg, TModel>
    {
        TModel Init();
        TModel Update(TMsg message, TModel model);
        DrawingVisual View(TModel model);
    }

    public static class Component
    {
        public static Component<TMsg, TModel> Create<TMsg, TModel>(
            Func<TModel> init,
            Func<TMsg, TModel, TModel> update,
            Func<TModel, DrawingVisual> view)
        {
            return new Component<TMsg, TModel>(init, update, view);
        }
    }

    public class Component<TMsg, TModel> : IComponent<TMsg, TModel>
    {
        public Func<TModel> Init { get; private set; }
        public Func<TMsg, TModel, TModel> Update { get; private set; }
        public Func<TModel, DrawingVisual> View { get; private set; }

        public Component(Func<TModel> init,
            Func<TMsg, TModel, TModel> update,
            Func<TModel, DrawingVisual> view)
        {
            Init = init;
            Update = update;
            View = view;
        }

        TModel IComponent<TMsg, TModel>.Init()
        {
            return Init();
        }

        TModel IComponent<TMsg, TModel>.Update(TMsg message, TModel model)
        {
            return Update(message, model);
        }

        DrawingVisual IComponent<TMsg, TModel>.View(TModel model)
        {
            return View(model);
        }
    }

    public static class Combinators
    {
        public static IComponent<IVariant<TMsg1, TMsg2>, Tuple<TModel1, TModel2>> TopDown<TMsg1, TModel1, TMsg2, TModel2>(
            this IComponent<TMsg1, TModel1> top,
            IComponent<TMsg2, TModel2> bottom)
        {
            return Component.Create<IVariant<TMsg1, TMsg2>, Tuple<TModel1, TModel2>>(
                () => Tuple.Create(top.Init(), bottom.Init()),
                
                (msg, model) => msg.Map(
                    topMsg => Tuple.Create(top.Update(topMsg, model.Item1), model.Item2),
                    bottomMsg => Tuple.Create(model.Item1, bottom.Update(bottomMsg, model.Item2))),
                
                model =>
                {
                    var topView = top.View(model.Item1);
                    var bottomView = bottom.View(model.Item2);
                    bottomView.Transform = new TranslateTransform(0.0, topView.ContentBounds.Height);
                    var visual = new DrawingVisual();
                    visual.Children.Add(topView);
                    visual.Children.Add(bottomView);
                    return visual;
                });
        }
    }
}
