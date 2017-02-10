using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Media.Animation;
using System.Collections.Immutable;

namespace TodoElmStyle
{
    public class FooTemplate<T> : ContentControl
    {
    }

    public class Todos
    {
        public readonly ImmutableList<TodoItem> TodoItems;
        public readonly bool Filter;

        public Todos()
        {
            TodoItems = ImmutableList<TodoItem>.Empty;
            Filter = false;
        }

        public Todos(ImmutableList<TodoItem> todos, bool filter)
        {
            TodoItems = todos;
            Filter = filter;
        }
    }

    public class TodoItem
    {
        public string Text { get; private set; }
        public bool Completed { get; private set; }

        public TodoItem(string text, bool completed)
        {
            Text = text;
            Completed = completed;
        }
    }

    public enum MessageType { ToggleFilter, Add, Remove, ToggleCompleted };

    public class TodoMessage
    {
        public readonly MessageType Type;

        private object messageObj;

        private TodoMessage(MessageType type, object message)
        {
            Type = type;
            messageObj = message;
        }

        public static TodoMessage Add(TodoItem item)
        {
            return new TodoMessage(MessageType.Add, item);
        }

        public static TodoMessage ToggleFilter(bool filter)
        {
            return new TodoMessage(MessageType.ToggleFilter, filter);
        }

        public static TodoMessage Remove(TodoItem item)
        {
            return new TodoMessage(MessageType.Remove, item);
        }

        public static TodoMessage ToggleCompleted(TodoItem item)
        {
            return new TodoMessage(MessageType.ToggleCompleted, item);
        }

        public R Map<R>(
            Func<bool, R> toggleFilter,
            Func<TodoItem, R> add,
            Func<TodoItem, R> remove,
            Func<TodoItem, R> toggleCompleted)
        {
            return Type == MessageType.Add ? add((TodoItem)messageObj)
                : Type == MessageType.Remove ? remove((TodoItem)messageObj)
                : Type == MessageType.ToggleFilter ? toggleFilter((bool)messageObj)
                : /* Type == MessageType.ToggleCompleted */ toggleCompleted((TodoItem)messageObj);
        }
    }

    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private StatelessComponent<Todos, TodoMessage, UIElement> component;
        private Todos model;

        private void Dispatch(TodoMessage msg)
        {
            model = component.Update(msg, model);
            this.Content = component.View(model);
        }

        public MainWindow()
        {
            InitializeComponent();

            component = Component.Create<Todos, TodoMessage, UIElement>(
                new Todos(ImmutableList<TodoItem>.Empty, false),

                (msg, todos) => msg.Map(
                    toggleFilter: filter => new Todos(todos.TodoItems, filter),
                    
                    add: todo => new Todos(todos.TodoItems.Add(todo), todos.Filter),
                    
                    remove: todo => new Todos(todos.TodoItems.Remove(todo), todos.Filter),
                    
                    toggleCompleted: todo => new Todos(
                        todos.TodoItems.Replace(todo, new TodoItem(todo.Text, !todo.Completed)),
                        todos.Filter)),

                (todos) =>
                    new Grid()
                    {
                        Margin = new Thickness(5, 5, 5, 5)
                    }
                    .WithRow(Height: GridLength.Auto)
                    .WithRow()
                    .WithRow(Height: GridLength.Auto)
                    .Containing(
                        CreateHeader(todos).DoWith(t => t.SetValue(Grid.RowProperty, 0)),
                        CreateBody(todos).DoWith(s => s.SetValue(Grid.RowProperty, 1)),
                        new TextBlock()
                        {
                            Text = "Show Completed"
                        }.DoWith(t => t.SetValue(Grid.RowProperty, 2)))
                );

            this.model = component.InitialModel;
            this.Content = component.View(model);
        }

        private UIElement CreateHeader(Todos todos)
        {
            return new Grid()
            {
            }
            .DoWith(g =>
            {
                g.ColumnDefinitions.Add(new ColumnDefinition() { Width = GridLength.Auto });
                g.ColumnDefinitions.Add(new ColumnDefinition());
            })
            .Containing(
                new TextBlock()
                {
                    Text = "Add a todo",
                    HorizontalAlignment = HorizontalAlignment.Stretch
                }
                .DoWith(t => t.SetValue(Grid.ColumnProperty, 0)),
                
                new TextBox()
                {
                }.DoWith(t =>
                {
                    t.Loaded += (sender, args) => t.Focus();

                    t.KeyDown += (sender, args) =>
                    {
                        if (args.Key == Key.Enter)
                        {
                            args.Handled = true;
                            Dispatch(TodoMessage.Add(new TodoItem(t.Text, false)));
                        }
                    };
                })
                .DoWith(t => t.SetValue(Grid.ColumnProperty, 1))
            );
        }

        // This little patterns allows a sub-component within a component tree to maintain a 
        // persistent WPF control.  In this case, it is a scrollviewer which should retain it's 
        // scroll offsets as the component is re-rendered.  The ScrollViewer could also be
        // specified as part of the component's model, and this it is somewhat reasonable- it 
        // contains state information related to the current scroll offset.
        private ContentControl bodyContainer = null;
        private ScrollViewer bodyScroller = new ScrollViewer();
        private UIElement CreateBody(Todos todos)
        {
            // Disconnect the ScrollViewer from it's previous ContentControl parent
            if (bodyContainer != null)
                bodyContainer.Content = null;

            bodyScroller.Content =
                new StackPanel()
                {
                    Orientation = Orientation.Vertical
                }
                .Containing(todos.TodoItems.Select(CreateTodoItem));

            return bodyContainer = new ContentControl() { Content = bodyScroller };
        }

        private UIElement CreateTodoItem(TodoItem todo)
        {
            Panel view = null;
            view =
                new StackPanel()
                {
                    Orientation = Orientation.Horizontal
                }
                .Containing(
                    new CheckBox()
                    {
                        Margin = new Thickness(5, 5, 5, 5),
                        IsChecked = todo.Completed
                    }
                    .DoWith(c =>
                        c.Click += (sender, args) => Dispatch(TodoMessage.ToggleCompleted(todo))),

                    new TextBlock()
                    {
                        Margin = new Thickness(5, 5, 5, 5),
                        Text = todo.Text,
                        TextDecorations = todo.Completed ? TextDecorations.Strikethrough
                            : null
                    },

                    new Button()
                    {
                        Margin = new Thickness(5, 5, 5, 5),
                        HorizontalContentAlignment = HorizontalAlignment.Center,
                        VerticalContentAlignment = VerticalAlignment.Center,
                        Content = new Grid()
                        {
                        }
                        .Containing(
                            new Line()
                            {
                                StrokeThickness = 2,
                                Stroke = Brushes.Red,
                                X1 = 0, Y1 = 0,
                                X2 = 10, Y2 = 10
                            },
                            new Line()
                            {
                                StrokeThickness = 2,
                                Stroke = Brushes.Red,
                                X1 = 0, Y1 = 10,
                                X2 = 10, Y2 = 0
                            })
                    }
                    .DoWith(b =>
                        b.Click += (s, a) =>
                        {
                            var fade = new DoubleAnimation(0.0, TimeSpan.FromSeconds(0.25));
                            Storyboard.SetTarget(fade, view);
                            Storyboard.SetTargetProperty(fade, new PropertyPath(UIElement.OpacityProperty));
                            var story = new Storyboard();
                            story.Children.Add(fade);
                            story.Completed += (s2, a2) => Dispatch(TodoMessage.Remove(todo));
                            story.Begin();
                        }
                    )
                );

            return view;
        }
    }
}
