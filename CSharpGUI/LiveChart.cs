using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Threading;
using CommunityToolkit.Mvvm.Input;
using FSharpLibrary;
using LiveChartsCore;
using LiveChartsCore.Defaults;
using LiveChartsCore.Drawing;
using LiveChartsCore.Kernel;
using LiveChartsCore.Kernel.Events;
using LiveChartsCore.Kernel.Sketches;
using LiveChartsCore.Measure;
using LiveChartsCore.SkiaSharpView;
using LiveChartsCore.SkiaSharpView.Drawing;
using LiveChartsCore.SkiaSharpView.Drawing.Geometries;
using LiveChartsCore.SkiaSharpView.Painting;
using LiveChartsCore.SkiaSharpView.Painting.Effects;
using LiveChartsCore.SkiaSharpView.VisualElements;
using LiveChartsCore.VisualElements;
using SkiaSharp;
// Plotting and Live Charts
// Author: Samuel Chaney
namespace LiveChart
{
    public partial class ViewModel
    {
        // Colours
        private static readonly SKColor col_hl = SKColors.LightSlateGray;
        private static readonly SKColor col_fg = new(160, 160, 160);
        private static readonly SKColor col_bg = new(60, 60, 60);

        // Graph Values
        Dictionary<TextBox, LineSeries<ObservablePoint>> lineSeriesDict = new Dictionary<TextBox, LineSeries<ObservablePoint>>();
        public ObservableCollection<ISeries> Series { get; set; }
        public ObservableCollection<ObservablePoint> _observablePoints { get; set; }
        public IEnumerable<ChartElement<SkiaSharpDrawingContext>> VisualElements { get; set; }
        private List<ChartElement<SkiaSharpDrawingContext>> rootPoints = new List<ChartElement<SkiaSharpDrawingContext>>();
        private bool _pointerDown = false;
        private LvcPointD panStartPos = new(0, 0);
        private DispatcherTimer scrollTimer = new DispatcherTimer();
        private static readonly int limitScale = 10;
        private static readonly double mouseSensitivity = 0.001f;
        private static readonly int RESOLUTION = 99;
        

        public ViewModel()
        {
            /// Viewmodel constructor 
            _observablePoints = new ObservableCollection<ObservablePoint>
            {
                new ObservablePoint(0, 4),
                new ObservablePoint(1, 3),
                new ObservablePoint(3, 2)
            };

            Series = new ObservableCollection<ISeries>
            {
                new LineSeries<ObservablePoint>
                {
                    Values = new ObservableCollection<ObservablePoint>(),
                    Fill = null,
                    GeometrySize = 0,
                    LineSmoothness = 0
                }
            };

            scrollTimer.Interval = TimeSpan.FromMilliseconds(500);
            scrollTimer.Tick += scrollTimerTick;

            VisualElements = rootPoints;
        }

        [RelayCommand]
        public void ChartUpdated(ChartCommandArgs args)
        {

        }

        [RelayCommand]
        public void PointerDown(PointerCommandArgs args)
        {
            _pointerDown = true;
            var chart = (ICartesianChartView<SkiaSharpDrawingContext>)args.Chart;
            panStartPos = chart.ScalePixelsToData(args.PointerPosition);
        }

        [RelayCommand]
        public void PointerMove(PointerCommandArgs args)
        {
            // Pan viewport
            if (!_pointerDown) return;

            // Calculate delta
            var chart = (ICartesianChartView<SkiaSharpDrawingContext>)args.Chart;
            var positionInData = chart.ScalePixelsToData(args.PointerPosition);

            var deltaX = (panStartPos.X - positionInData.X);
            var deltaY = (panStartPos.Y - positionInData.Y);

            // Apply movement
            XAxes[0].MinLimit += deltaX;
            XAxes[0].MaxLimit += deltaX;
            YAxes[0].MinLimit += deltaY;
            YAxes[0].MaxLimit += deltaY;
        }

        [RelayCommand]
        public void PointerUp(PointerCommandArgs args)
        {
            _pointerDown = false;
            replot(RESOLUTION);
        }

        [RelayCommand]
        public void MouseWheel(MouseWheelEventArgs args)
        {
            // Zoom
            double scrollDelta = args.Delta * mouseSensitivity * (XAxes[0].MaxLimit??0 - XAxes[0].MinLimit??0);
            XAxes[0].MinLimit += scrollDelta;
            XAxes[0].MaxLimit -= scrollDelta;
            YAxes[0].MinLimit += scrollDelta;
            YAxes[0].MaxLimit -= scrollDelta;


            // Replot after X milliseconds, prevents constant replotting
            scrollTimer.Stop();
            scrollTimer.Start();
        }

        private void scrollTimerTick(object sender, EventArgs e)
        {
            scrollTimer.Stop();
            replot(RESOLUTION);
        }

        


        // Axes setup
        public ICartesianAxis[] XAxes { get; set; } =
        {
            new Axis
            {
                Name = "X axis",
                NamePaint = new SolidColorPaint(col_fg),
                TextSize = 18,
                Padding = new Padding(5, 15, 5, 5),
                LabelsPaint = new SolidColorPaint(col_bg),
                SeparatorsPaint = new SolidColorPaint
                {
                    Color = col_fg,
                    StrokeThickness = 1,
                    PathEffect = new DashEffect(new float[] {3, 3})
                }, 
                ZeroPaint = new SolidColorPaint
                {
                    Color = col_hl,
                    StrokeThickness = 2
                },
                MinLimit = -limitScale,
                MaxLimit = limitScale,
                MinStep = 2
            }
        };

        public ICartesianAxis[] YAxes { get; set; } =
        {
            new Axis
            {
                Name = "Y axis",
                NamePaint = new SolidColorPaint(col_fg),
                TextSize = 18,
                Padding = new Padding(5, 15, 5, 5),
                LabelsPaint = new SolidColorPaint(col_bg),
                SeparatorsPaint = new SolidColorPaint
                {
                    Color = col_fg,
                    StrokeThickness = 1,
                    PathEffect = new DashEffect(new float[] {3, 3})
                },
                ZeroPaint = new SolidColorPaint
                {
                    Color = col_hl,
                    StrokeThickness = 2
                },
                MinLimit = -limitScale,
                MaxLimit = limitScale,
                MinStep = 2
            }
        };

        public DrawMarginFrame Frame { get; set; } =
        new()
        {
            Fill = new SolidColorPaint(col_bg),
            Stroke = new SolidColorPaint
            {
                Color = col_fg,
                StrokeThickness = 1
            }
        };

        public LineSeries<ObservablePoint> getUniqueSeries(TextBox key)
        {
            /// Returns / Creates line series
            /// Parameters: 'TextBox' used as dictionary key
            /// Return: 'LineSeries<ObservablePoint>'

            if (!lineSeriesDict.ContainsKey(key))
            {
                LineSeries<ObservablePoint> lineSeries = new LineSeries<ObservablePoint>
                {
                    Values = new ObservableCollection<ObservablePoint>(),
                    Fill = null,
                    GeometrySize = 0,
                    LineSmoothness = 1,
                    YToolTipLabelFormatter = point => $"X: {point.Model.X.Value.ToString("0.####")} Y: {point.Model.Y.Value.ToString("0.####")}"
                };

                lineSeriesDict.Add(key, lineSeries);
                lineSeriesDict.TryGetValue(key, out var lSeries);
                Series.Add(lSeries);
                return lSeries;
            }
            else
            {
                lineSeriesDict.TryGetValue(key, out var lSeries);
                return lSeries;
            }
        }

        public void removeSeries(TextBox key)
        {
            if (lineSeriesDict.ContainsKey(key))
            {
                lineSeriesDict.TryGetValue(key, out var lSeries);
                Series.Remove(lSeries);
                lineSeriesDict.Remove(key);
            }
        }

        public void ResetSeries(LineSeries<ObservablePoint> lineSeries)
        {
            lineSeries.Values.Clear();
        }

        public void AddToSeries(LineSeries<ObservablePoint> lineSeries, ObservablePoint point)
        {
            lineSeries.Values.Add(point);
        }

        public void drawPoints()
        {
            VisualElements = rootPoints;
            XAxes[0].MinLimit += 0.01f;
            XAxes[0].MinLimit -= 0.01f;
        }
        
        public void addPoints(double x, double y)
        {
            GeometryVisual<CircleGeometry> rootCircle = new GeometryVisual<CircleGeometry>
            {
                X = x-.1f,
                Y = y+.1f,
                LocationUnit = MeasureUnit.ChartValues,
                Width = .2f,
                Height = .2f,
                SizeUnit = MeasureUnit.ChartValues,
                Fill = new SolidColorPaint(new SKColor(225, 225, 245, 255)) { ZIndex = 10 },
                Stroke = new SolidColorPaint(new SKColor(225, 225, 245)) { ZIndex = 10, StrokeThickness = 1.5f },
            };
            rootPoints.Add(rootCircle);
        }

        public void plotGraph(TextBox formulaBox, int res)
        {
            /// Plots graph from provided equation.
            /// Parameters: 'TextBox' containing formula | 'Int' denoting graph resolution
            /// Return: 'Void'

            LineSeries<ObservablePoint> lineSeries = getUniqueSeries(formulaBox);
            ResetSeries(lineSeries);
           
            double xMin = XAxes[0].MinLimit??0 - 1.0f;
            double xMax = XAxes[0].MaxLimit??0 + 1.0f;

            // Plot points
            double stepSize = (xMax - xMin) / (res - 1);
            for (double i = xMin; i < xMax+1; i+=stepSize)
            {
                interpreter.overrideX(i);
                double answer = interpreter.solve(formulaBox.Text);
                Console.WriteLine(i + ", " + answer);

                var point = new ObservablePoint(i, answer);
                AddToSeries(lineSeries, point);
            }

            // Resolution pass
            int passClear = resPassGC(formulaBox);
            int maxPasses = 3;
            while (passClear != 0 && maxPasses >= 0)
            {
                passClear = resPassGC(formulaBox);
                maxPasses -= 1;
            }

            // Asymptote pass
            checkAsymptote(formulaBox);
        }

        public void replot(int res)
        {
            /// Replot, redraws graph
            /// Parameters: 'Int' indicating resolution
            /// Return: 'Void'
            foreach (TextBox plotKey in lineSeriesDict.Keys)
            {
                plotGraph(plotKey, res);
            }
        }

        private void checkAsymptote(TextBox formulaBox)
        {
            /// Deletes lines located on asymptote
            /// Parameters: 'TextBox' containing formula
            /// Return: 'Void'
            LineSeries<ObservablePoint> lineSeries = getUniqueSeries(formulaBox);
            ICollection<ObservablePoint> updatedPoints = new List<ObservablePoint>();
            ObservablePoint previousPoint = null;
            double previousGradient = 0;
            double gradient = 0;

            foreach (ObservablePoint point in lineSeries.Values)
            {
                
                if (previousPoint != null)
                {
                    gradient = (previousPoint.Y - point.Y ?? 0) / (previousPoint.X - point.X ?? 0);
                    if ((previousPoint.Y * point.Y) < 0 && (previousGradient * gradient) < 0)
                    {
                        updatedPoints.Add(null);
                    }

                }
                previousPoint = point;
                previousGradient = gradient;
                updatedPoints.Add(point);
            }
            // Assign points
            lineSeries.Values = updatedPoints;
        }

        private int resPassGC(TextBox formulaBox)
        {
            /// Resolution pass, iterates over current values in graph, inserting new points at midpoint.
            /// Parameters: 'TextBox' containing formula | 'Bool' denoting if pass is final 
            /// Return: 'Int' indicating completion
            double threshold = 0.02f;
            int pass = 0;
            LineSeries<ObservablePoint> lineSeries = getUniqueSeries(formulaBox);

            string derivative = "y = " + Calculus.differentiateExpression(formulaBox.Text);

            // Fix formatting
            derivative = Regex.Replace(derivative, @"(\d)(x)", "$1*$2");
            derivative = Regex.Replace(derivative, @"(\b(?:sin|cos|tan|sec|csc|cot)\b)\^(\d+)\(([^)]+)\)", "$1($3)^$2");
            
            ObservablePoint previousPoint = null;
            double previousGradient = 0;
            ICollection<ObservablePoint> updatedPoints = new List<ObservablePoint>();

            // Insert points
            double gradient = 0.5;
            bool tryLock = false;
            foreach (ObservablePoint point in lineSeries.Values)
            {
                if (tryLock == false)
                {
                    try
                    {
                        interpreter.overrideX(point.X ?? 0);
                        gradient = interpreter.solve(derivative);
                    }
                    catch (Exception)
                    {
                        gradient = 0.5;
                        tryLock = true;
                    }
                }

                if (previousPoint != null)
                {
                    double gradChange = gradient - previousGradient;

                    // Insert new point
                    if (Math.Abs(gradChange) < threshold)
                    {
                        pass = 1;
                        double midpoint = (double)((previousPoint.X + point.X) / 2.0);

                        interpreter.overrideX(midpoint);
                        double answer = interpreter.solve(formulaBox.Text);
                        ObservablePoint newPoint = new ObservablePoint(midpoint, answer);

                        updatedPoints.Add(newPoint);
                    }
                }
                updatedPoints.Add(point);
                previousPoint = point;
                previousGradient = gradient;
            }
            // Assign points
            lineSeries.Values = updatedPoints;
            return pass;
        }
    }
}