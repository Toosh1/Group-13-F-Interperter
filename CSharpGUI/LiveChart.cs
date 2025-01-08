using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media.Converters;
using System.Windows.Threading;
using System.Xml.Schema;
using CommunityToolkit.Mvvm.Input;
using FSharpLibrary;
using LiveChartsCore;
using LiveChartsCore.Defaults;
using LiveChartsCore.Drawing;
using LiveChartsCore.Kernel.Events;
using LiveChartsCore.Kernel.Sketches;
using LiveChartsCore.SkiaSharpView;
using LiveChartsCore.SkiaSharpView.Drawing;
using LiveChartsCore.SkiaSharpView.Painting;
using LiveChartsCore.SkiaSharpView.Painting.Effects;
using SkiaSharp;
using static Microsoft.FSharp.Core.ByRefKinds;

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

        private bool _pointerDown = false;
        private LvcPointD panStartPos = new(0, 0);
        private DispatcherTimer scrollTimer = new DispatcherTimer();
        private static readonly int limitScale = 10;
        private static readonly double mouseSensitivity = 0.001f;
        private static readonly int RESOLUTION = 49;
        
        // Sections (for lines at x = 0 and y = 0)
        //public ObservableCollection<RectangularSection> Sections { get; set; }

        // Thread-safety mechanisms
        private readonly object _syncLock = new object();
        private bool _isReading = true;

        // Constructor
        public ViewModel()
        {
            // Initialize observable points
            _observablePoints = new ObservableCollection<ObservablePoint>
            {
                new ObservablePoint(0, 4),
                new ObservablePoint(1, 3),
                new ObservablePoint(3, 2)
            };

            // Series setup
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

            // Sections setup for x=0 and y=0
            /*
            Sections = new ObservableCollection<RectangularSection>
            {
                new RectangularSection
                {
                    Xi = 0, // Vertical line at x = 0
                    Xj = 0,
                    Stroke = new SolidColorPaint(SKColors.LightSlateGray) { StrokeThickness = 2 }
                },
                new RectangularSection
                {
                    Yi = 0, // Horizontal line at y = 0
                    Yj = 0,
                    Stroke = new SolidColorPaint(SKColors.LightSlateGray) { StrokeThickness = 2 }
                }
            };*/

            // Setup scroll zoom timer
            scrollTimer.Interval = TimeSpan.FromMilliseconds(500);
            scrollTimer.Tick += scrollTimerTick;
 
            // Start multi-threaded data updates
            StartReadingData();
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
            // Panning
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

            // Recalculate points
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


        // Multi-threaded data addition
        private void StartReadingData()
        {
            // Create multiple tasks to simulate concurrent data updates
            int readTasks = 5;  // Number of tasks to run concurrently

            for (int i = 0; i < readTasks; i++)
            {
                Task.Run(ReadData);
            }
        }

        // Simulate reading data in parallel
        private async Task ReadData()
        {
            Random random = new Random();
            int currentValue = _observablePoints.Count > 0 ? (int)_observablePoints[^1].Y : 0;

            while (_isReading)
            {
                await Task.Delay(100);  // Simulate delay

                // Randomly modify the Y value for the new point
                double newYValue = currentValue + random.Next(-10, 10);
                ObservablePoint newPoint = new ObservablePoint(_observablePoints.Count, newYValue);

                // Add the new point to the ObservableCollection in a thread-safe manner
                lock (_syncLock)
                {
                    // Keep only a fixed number of points for simplicity (e.g., 100 points)
                    if (_observablePoints.Count > 100)
                    {
                        _observablePoints.RemoveAt(0);  // Remove the oldest point
                    }

                    _observablePoints.Add(newPoint);
                }

                currentValue = (int)newYValue;  // Update the current value for next iteration
            }
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

        // Returns / Creates line series
        public LineSeries<ObservablePoint> getUniqueSeries(TextBox key)
        {
            // Create series if non existent
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
                lineSeriesDict.TryGetValue(key, out var lSeries); // Error checking should be done
                return lSeries;
            }
        }

        public void removeSeries(TextBox key)
        {
            // Check if series exists
            if (lineSeriesDict.ContainsKey(key))
            {
                lineSeriesDict.TryGetValue(key, out var lSeries);
                Series.Remove(lSeries);     // Remove from Seires
                lineSeriesDict.Remove(key); // Delete dictionary link
            }
        }

        // Reset points
        public void Reset()
        {
            _observablePoints.Clear();
        }

        public void ResetSeries(LineSeries<ObservablePoint> lineSeries)
        {
            lineSeries.Values.Clear();
        }

        // Add point to series
        public void AddItem(ObservablePoint point)
        {
            _observablePoints.Add(point);
        }

        // Add points to line series
        public void AddToSeries(LineSeries<ObservablePoint> lineSeries, ObservablePoint point)
        {
            lineSeries.Values.Add(point);
        }

        // Plot points
        public void plotGraph(TextBox formulaBox, int res)
        {
            // Create line series if none associated
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
            int maxPasses = 2;
            while (passClear != 0 && maxPasses >= 0)
            {
                passClear = resPassGC(formulaBox);
                maxPasses -= 1;
            }
        }

        // Replot
        private void replot(int res)
        {
            foreach (TextBox plotKey in lineSeriesDict.Keys)
            {
                plotGraph(plotKey, res);
            }
        }

        // Resolution pass (gradient change)
        private int resPassGC(TextBox formulaBox)
        {
            double threshold = 1.0f;
            int pass = 0;

            // Get line series
            LineSeries<ObservablePoint> lineSeries = getUniqueSeries(formulaBox);

            // Integrate to find gradient
            string derivative = "y = " + Calculus.differentiateExpression(formulaBox.Text);

            // Temp formatting fix
            derivative = Regex.Replace(derivative, @"(\d)(x)", "$1*$2");
            Debug.Print(derivative);
            // Iterate over points, insert new when gradient change below/above threshold
            ObservablePoint previousPoint = null;
            double previousGradient = 0;
            ICollection<ObservablePoint> updatedPoints = new List<ObservablePoint>();
            foreach (ObservablePoint point in lineSeries.Values)
            {
                // Find gradient associated with point
                interpreter.overrideX(point.X ?? 0);
                double gradient = interpreter.solve(derivative);

                // Calculate gradient change
                if (previousPoint != null)
                {
                    double gradChange = gradient - previousGradient;
                    if (Math.Abs(gradChange) < threshold)
                    {
                        // Indicate pass was completed
                        pass = 1;

                        //Calculate midpoint
                        double midpoint = (double)((previousPoint.X + point.X) / 2.0);

                        // Calculate new point
                        interpreter.overrideX(midpoint);
                        double answer = interpreter.solve(formulaBox.Text);
                        ObservablePoint newPoint = new ObservablePoint(midpoint, answer);

                        // Insert point
                        updatedPoints.Add(newPoint);
                    }
                }
                updatedPoints.Add(point);
                previousPoint = point;
            }
            // Assign points
            lineSeries.Values = updatedPoints;
            return pass;
        }
        // Resolution Pass
        private int resPass(TextBox formulaBox) // Returns 0 when no more passes are made
        {
            double THRESHOLD = 5.0;
            int complete = 0;
            // Get old series
            LineSeries<ObservablePoint> lineSeries = getUniqueSeries(formulaBox);

            // Loop over points, insert when gradient above threshold
            ICollection<ObservablePoint> updatedPoints = new List<ObservablePoint>();
            ObservablePoint previousPoint = null;
            foreach (ObservablePoint point in lineSeries.Values)
            {
                // Calculate gradient
                if (previousPoint != null)
                {
                    double gradient = (double)((point.Y - previousPoint.Y) / (point.X - previousPoint.X));
                    if (Math.Abs(gradient) > THRESHOLD)
                    {
                        // Indicate a pass was completed
                        complete = 1;

                        // Calculate X midpoint
                        double midpoint = (double)((previousPoint.X + point.X) / 2.0);

                        // Calculate new point
                        interpreter.overrideX(midpoint);
                        double answer = interpreter.solve(formulaBox.Text);
                        ObservablePoint newPoint = new ObservablePoint(midpoint, answer);

                        // Insert point
                        updatedPoints.Add(newPoint);
                    }
                }
                updatedPoints.Add(point);
                previousPoint = point;
            }

            // Assign points
            lineSeries.Values = updatedPoints;
            return complete;
        }
    }
}
