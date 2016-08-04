using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;

namespace TodoElmStyle
{
    public static class Extensions
    {
        public static T[] Add<T>(this T[] t, T e)
        {
            var copy = new T[t.Length + 1];
            Array.Copy(t, copy, t.Length);
            copy[copy.Length - 1] = e;
            return copy;
        }

        public static T Containing<T>(this T panel, IEnumerable<UIElement> children) where T : Panel
        {
            foreach (var child in children)
                panel.Children.Add(child);

            return panel;
        }

        public static T Containing<T>(this T panel, params UIElement[] children) where T : Panel
        {
            return panel.Containing((IEnumerable<UIElement>)children);
        }

        public static T DoWith<T>(this T t, Action<T> action)
        {
            action(t);
            return t;
        }

        public static Grid WithRow(this Grid grid, 
            GridLength? Height = default(GridLength?), 
            double? MinHeight = null, 
            double? MaxHeight = null, 
            double? Offset = null, 
            string SharedSizeGroup = null)
        {
            var row = new RowDefinition();
            if (Height.HasValue) row.Height = Height.Value;
            if (MinHeight.HasValue) row.MinHeight = MinHeight.Value;
            if (MaxHeight.HasValue) row.MaxHeight = MaxHeight.Value;
            if (SharedSizeGroup != null) row.SharedSizeGroup = SharedSizeGroup;

            grid.RowDefinitions.Add(row);

            return grid;
        }
    }
}
