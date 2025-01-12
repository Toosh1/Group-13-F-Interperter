using System.Windows;

namespace Learning.Views;

public partial class InputWindow : Window
{
    public string InputValue { get; private set; }
    public string InputValue2 { get; private set; }
    
    public InputWindow(string Text, bool twoTextBoxes = false)
    {
        InitializeComponent();
        Label.Content = Text;
        if (twoTextBoxes)
        {
            InputTextBox2.Visibility = Visibility.Visible;
        }
    }
    private void OnOkClick(object sender, RoutedEventArgs e)
    {
        InputValue = InputTextBox.Text;
        InputValue2 = InputTextBox2.Text;
        DialogResult = true; // Closes the window and sets the dialog result
    }
}