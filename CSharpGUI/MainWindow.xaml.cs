using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using FSharpLibrary;
using Learning.Views;
using LiveChart;
using LiveCharts;
using LiveCharts.Wpf;

namespace Learning;

public partial class MainWindow : Window
{
    private string previousPolynomial;

    public MainWindow()
    {
        InitializeComponent();

        ChartSeries = new SeriesCollection
        {
            new LineSeries
            {
                Title = "Data Series",
                Values = new ChartValues<double>() // Start with an empty series
            }
        };

        // Set data context for binding
        viewModel = new ViewModel();
        DataContext = viewModel;
        //DataContext = this; ADD BACK LATER
    }

    public SeriesCollection ChartSeries { get; set; }
    private ViewModel viewModel { get; }


    private void BtnAnswer_OnClick(object sender, RoutedEventArgs e)
    {
        var input = tbInput.Text;
        try
        {
            var answer = interpreter.solve(input);
            answer = Math.Round(answer, 10);
            if (is_int(input)) answer = Convert.ToInt32(Math.Floor(answer));
            tbAnswer.Text += string.Format("Answer:\n{0}\n", answer);
            previousPolynomial = answer.ToString();
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }

    private bool is_int(string input)
    {
        return !input.Contains(".");
    }

    private void BtnPlot_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            foreach (TextBox formulaBox in FormulaStackPanel.Children)
                if (!string.IsNullOrEmpty(formulaBox.Text))
                    viewModel.plotGraph(formulaBox, 99);
        }
        catch (Exception ex)
        {
            tbAnswer.Text = "Error: " + ex.Message;
        }
    }

    private void BtnDifferntiate_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var input = tbInput.Text;
            var rule = Calculus.identifyDifferentiationRule(input);
            var derivative = "";
            switch (rule)
            {
                case "Product Rule":
                    var inputDialog = new InputWindow(rule+"\nEnter First function (u): \nEnter Second function (v)",true);
                    if (inputDialog.ShowDialog() == true)
                    {
                        derivative = Calculus.productRule(inputDialog.InputValue, inputDialog.InputValue2);
                    }
                    break;
                case "Quotient Rule":
                    var inputDialog2 = new InputWindow(rule+"\nEnter numerator function (u): \n Enter denominator function (v)",true);
                    if (inputDialog2.ShowDialog() == true)
                    {
                        derivative = Calculus.quotientRule(inputDialog2.InputValue, inputDialog2.InputValue2);
                    }
                    break;
                case "Chain Rule":
                    var inputDialog3 = new InputWindow(rule+"\nEnter Outer function (u): \n Enter Inner function (v)",true);
                    if (inputDialog3.ShowDialog() == true)
                    {
                        derivative = Calculus.chainRule(inputDialog3.InputValue, inputDialog3.InputValue2);
                    }
                    break;
                default:
                    derivative = Calculus.differentiateExpression(input);
                    break;
            }
          
            Console.WriteLine(derivative);
            tbAnswer.Text += string.Format("Derivative: {0}:\n {1}\n", input, derivative);
            previousPolynomial = derivative;
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }


    private void formulaChangedEvent(object sender, TextChangedEventArgs args)
    {
        var formulaBox = (TextBox)sender;

        if (formulaBox == FormulaStackPanel.Children[FormulaStackPanel.Children.Count - 1])
            if (formulaBox.Text.Length > 0)
            {
                var newBox = new TextBox
                {
                    FontSize = 20,
                    Margin = new Thickness(0, 5, 0, 0)
                };
                newBox.TextChanged += formulaChangedEvent;
                FormulaStackPanel.Children.Add(newBox);
            }

        if (string.IsNullOrEmpty(formulaBox.Text))
        {
            viewModel.removeSeries(formulaBox);
            if (formulaBox != FormulaStackPanel.Children[0]) FormulaStackPanel.Children.Remove(formulaBox);
        }
    }

    private void tbInput_TextChanged(object sender, TextChangedEventArgs e)
    {
    }

    // Help Menu Item Click Handlers
    private void HelpInputBox_OnClick(object sender, RoutedEventArgs e)
    {
        MessageBox.Show("Using the Input Box:\nType a mathematical formula to solve or plot.",
            "Help - Input Box", MessageBoxButton.OK, MessageBoxImage.Information);
    }

    private void HelpSolving_OnClick(object sender, RoutedEventArgs e)
    {
        MessageBox.Show("Solving Formulas:\nClick 'Solve' to calculate the result of the entered formula.",
            "Help - Solving Formulas", MessageBoxButton.OK, MessageBoxImage.Information);
    }

    private void HelpPlotting_OnClick(object sender, RoutedEventArgs e)
    {
        MessageBox.Show("Plotting Graphs:\nClick 'Plot' to visualize the entered formula on a graph.",
            "Help - Plotting Graphs", MessageBoxButton.OK, MessageBoxImage.Information);
    }

    private void HelpDifferentiation_OnClick(object sender, RoutedEventArgs e)
    {
        MessageBox.Show("Differentiation:\nClick 'Differentiate' to compute the derivative of the entered formula. If the expression required complicated formulas, you will be prompted to help the calculator by entering values from the function.",
            "Help - Differentiation", MessageBoxButton.OK, MessageBoxImage.Information);
    }
    private void HelpIntegration_OnClick(object sender, RoutedEventArgs e)
    {
        MessageBox.Show("Integration:\nClick 'Integration' to compute the integral of the entered formula. You will then be prompted to two values for the definite integration, if left empty indefinite integration will be done.",
            "Help - Integration", MessageBoxButton.OK, MessageBoxImage.Information);
    }
    
    private void HelpVectors_OnClick(object sender, RoutedEventArgs e)
    {
        MessageBox.Show("Vectors:\nClick 'Switch To Matrix View' then click 'Grid Size' and select the size of your vector (Note: it is always a square for matricies but you can use how many boxes you wish). Then you can click on the dot, cross or norm buttons to perform your calculations.",
            "Help - Vectors", MessageBoxButton.OK, MessageBoxImage.Information);
    }

    private void HelpMatricies_OnClick(object sender, RoutedEventArgs e)
    {
        MessageBox.Show("Matricies:\nClick 'Switch To Matrix View' then click 'Grid Size' and select the size of your matrix (Note: you do not have to stick to the dimension of the grid you can leave gaps). Then you can click on the determinant or multiply buttons to perform your calculations.",
            "Help - Matricies", MessageBoxButton.OK, MessageBoxImage.Information);
    }


    private void BtnRoot_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var input = tbInput.Text;
            var roots = new List<double>();

            for (double guess = -50; guess <= 50; guess += 0.314)
            {
                var root = Calculus.newtonRaphson(input, guess, 0.00001);
                if (!double.IsNaN(root) && !roots.Contains(root)) roots.Add(root);
            }

            var uniqueRoots = roots.Select(r => Math.Round(r, 5)).Distinct().ToList();
            tbAnswer.Text += string.Format("Roots for {0} :\n{1}\n", input, string.Join(", ", uniqueRoots));
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }

    private void BtnIntegrate_OnClick(object sender, RoutedEventArgs e)
    {
        var inputDialog = new InputWindow("Enter upper bound and lower bound \n(Leave blank for indefinite)",true);
        if (inputDialog.ShowDialog() == true)
        {
            try
            {
                if (inputDialog.InputValue == "")
                {
                    var input = tbInput.Text;
                    var integral = Calculus.integratePolynomial(input);

                    Console.WriteLine(integral);
                    tbAnswer.Text += string.Format("Integral: {0} :\n{1}\n", input, integral);
                    previousPolynomial = integral;
                }
                else
                {
                    var lowerBound = double.Parse(inputDialog.InputValue);
                    var upperBound = double.Parse(inputDialog.InputValue2);
                    var lineBound1 = string.Format("x = {0})", lowerBound);
                    var lineBound2 = string.Format("x = {0})", upperBound);
                    
                    var input = tbInput.Text;
                    var answer = Calculus.definiteIntegral(input,upperBound,lowerBound);
                    tbAnswer.Text += string.Format("Definite integration [{2},{3}]: {0} :\n{1}\n", input, answer,lowerBound,upperBound);
                }
            }
            catch (Exception ex)
            {
                tbAnswer.Text = String.Format("Error: {0}\n", ex.Message);
            }
        }
    }

    [STAThread]
    private void pi_OnClick(object sender, RoutedEventArgs e)
    {
        var textToCopy = "π";
        Clipboard.SetText(textToCopy);
        MessageBox.Show("Copied \"π\" to clipboard.");
    }

    private void e_OnClick(object sender, RoutedEventArgs e)
    {
        var textToCopy = "e";
        Clipboard.SetText(textToCopy);
        MessageBox.Show("Copied \"e\" to clipboard.");
    }

    private void CopyLast_OnClick(object sender, RoutedEventArgs e)
    {
        Clipboard.SetText(previousPolynomial);
        MessageBox.Show(string.Format("Copied \"{0}\" to clipboard.", previousPolynomial));
    }

    private void CartesianChart_MouseWheel(object sender, MouseWheelEventArgs args)
    {
        var vm = DataContext as ViewModel;
        if (vm?.MouseWheelCommand?.CanExecute(args) == true) vm.MouseWheelCommand.Execute(args);
    }

    private void ButtonDot_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var vector1 = GetGridValues(Matrix5Grid1);
            var vector2 = GetGridValues(Matrix5Grid2);
            var result = LinearAlgebra.callDotProduct(vector1, vector2);
            tbAnswer.Text += string.Format("Dot product of {0} and {1}: {2}\n", vector1, vector2, result);
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }

    private void ButtonCross_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var vector1 = GetGridValues(Matrix5Grid1);
            var vector2 = GetGridValues(Matrix5Grid2);
            var result = LinearAlgebra.callCrossProduct(vector1, vector2);
            tbAnswer.Text += string.Format("Cross of {0} and {1}: {2}\n", vector1, vector2, result);
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }

    private void ButtonNorm_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var vector1 = GetGridValues(Matrix5Grid1);
            var result = LinearAlgebra.callNorm(vector1);
            tbAnswer.Text += string.Format("Normal of {0}: {1}\n", vector1, result);
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }

    private void ButtonDeterminant_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var matrixLeft = GetGridValues(Matrix5Grid1);
            var result = LinearAlgebra.callDeterminant(matrixLeft);
            tbAnswer.Text += string.Format("Determinant of {0}: {1}\n", matrixLeft, result);
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }

    private void ButonMult_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var matrixLeft = GetGridValues(Matrix5Grid1);
            var matrixRight = GetGridValues(Matrix5Grid2);
            var result = LinearAlgebra.callMult(matrixLeft, matrixRight);
            tbAnswer.Text += string.Format("Mult of {0} and {1}: {2}\n", matrixLeft, matrixRight, result);
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }

    private void SwitchView_OnClick(object sender, RoutedEventArgs e)
    {
        // Toggle visibility of the matrix input grid
        if (MatrixButtonGrid.Visibility == Visibility.Collapsed)
        {
            Matrix5Grid1.Visibility = Visibility.Visible;
            Matrix5Grid2.Visibility = Visibility.Visible;
            MatrixButtonGrid.Visibility = Visibility.Visible;
            GridSizeHeader.Visibility = Visibility.Visible;
            MatrixView.Visibility = Visibility.Collapsed;
            StandardView.Visibility = Visibility.Visible;
            
            ClearGrid.Visibility = Visibility.Visible;
            ButtonGrid.Visibility = Visibility.Collapsed;
            EquationGrid.Visibility = Visibility.Collapsed;
            
        }
        else
        {
            ButtonGrid.Visibility = Visibility.Visible;
            EquationGrid.Visibility = Visibility.Visible;
            GridSizeHeader.Visibility = Visibility.Collapsed;
            ClearGrid.Visibility = Visibility.Collapsed;
            MatrixView.Visibility = Visibility.Visible;
            StandardView.Visibility = Visibility.Collapsed;
            Matrix5Grid1.Visibility = Visibility.Collapsed;
            Matrix5Grid2.Visibility = Visibility.Collapsed;
            MatrixButtonGrid.Visibility = Visibility.Collapsed;
        }
    }
    public void ConfigureMatrixGrid(object sender, RoutedEventArgs e)
    {
        
        if (sender is MenuItem menuItem && menuItem.Tag is string gridSize)
        {
            ClearMatrix();
            int size = Convert.ToInt32(gridSize);
            for (int i = 0; i < size; i++)
            {
                Matrix5Grid1.RowDefinitions.Add(new RowDefinition());
                Matrix5Grid1.ColumnDefinitions.Add(new ColumnDefinition());
                Matrix5Grid2.RowDefinitions.Add(new RowDefinition());
                Matrix5Grid2.ColumnDefinitions.Add(new ColumnDefinition());
            }

            // Populate grid with TextBoxes
            for (int row = 0; row < size; row++)
            {
                for (int col = 0; col < size; col++)
                {
                    var textBox = new TextBox
                    {
                        Margin = new Thickness(2),
                        Width = 20,
                        Height = 20
                    };
                    Grid.SetRow(textBox, row);
                    Grid.SetColumn(textBox, col);
                    var textBox2 = new TextBox
                    {
                        Margin = new Thickness(2),
                        Width = 20,
                        Height = 20
                    };
                    Grid.SetRow(textBox2, row);
                    Grid.SetColumn(textBox2, col);
                    Matrix5Grid1.Children.Add(textBox);
                    Matrix5Grid2.Children.Add(textBox2);
                }
            }
        }
    }
    public void ClearMatrixGridButton(object sender, RoutedEventArgs e)
    {
        ClearMatrix();
    }

    public void ClearMatrix()
    {
        // Clear children (TextBoxes)
        Matrix5Grid1.Children.Clear();
        Matrix5Grid2.Children.Clear();

        // Clear row and column definitions
        Matrix5Grid1.RowDefinitions.Clear();
        Matrix5Grid1.ColumnDefinitions.Clear();
        Matrix5Grid2.RowDefinitions.Clear();
        Matrix5Grid2.ColumnDefinitions.Clear();
    }

    private string GetGridValues(Grid grid)
    {
        var rows = new List<string>();

        // Loop through all rows
        foreach (var rowDefinition in grid.RowDefinitions)
        {
            var rowValues = new List<string>();

            // Loop through each child in the grid
            foreach (var child in grid.Children)
            {
                if (child is TextBox textBox)
                {
                    // Check if the TextBox is in the current row and is not empty
                    if (Grid.GetRow(textBox) == grid.RowDefinitions.IndexOf(rowDefinition) && !string.IsNullOrWhiteSpace(textBox.Text))
                    {
                        rowValues.Add(textBox.Text);
                    }
                }
            }

            // If there are any values in the row, join them with commas and add to the rows list
            if (rowValues.Count > 0)
            {
                rows.Add(string.Join(",", rowValues));
            }
        }

        // Join all rows with semicolons and return the final string
        return string.Join(";", rows);
    }

    private void BtnTangent_OnClick(object sender, RoutedEventArgs e)
    {
        try
        {
            var inputDialog = new InputWindow("Enter X value for tangent: ");
            if (inputDialog.ShowDialog() == true)
            {
                var xValue = Convert.ToInt32(inputDialog.InputValue);
                var input = tbInput.Text;
                var derivative = Calculus.differentiateExpression(input);
                interpreter.overrideX(xValue);
                var yValue = interpreter.solve(input);
                var mValue = interpreter.solve(derivative);
                var tangentEquation = string.Format("{0}*(x - {1}) + {2}", mValue, xValue, yValue);
                
                Console.WriteLine(tangentEquation);
                var fakeBox = new TextBox();
                fakeBox.Text = tangentEquation; //very hacky fix
                viewModel.plotGraph(fakeBox, 99);
            }
        }
        catch (Exception ex) 
        {
            tbAnswer.Text = String.Format("Error: {0}\n",ex.Message); 
        }
    }
    
}