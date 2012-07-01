#if INTERACTIVE
#load @"__solmerged.fsx"
#endif

#r "System.Xaml.dll"
#r "UIAutomationTypes.dll"
#r "WindowsBase.dll"
#r "PresentationFramework.dll"
#r "PresentationCore.dll"

open System.Windows
open System.Threading
open System.Windows.Threading

let ui =
  let mk() =
    let wh = new ManualResetEvent(false)
    let application = ref null
    let start() =
      let app = Application()
      application := app
      ignore(wh.Set())
      app.Run() |> ignore
    let thread = Thread start
    thread.IsBackground <- true
    thread.SetApartmentState ApartmentState.STA
    thread.Start()
    ignore(wh.WaitOne())
    !application, thread
  lazy(mk())

let spawn : ('a -> 'b) -> 'a -> 'b =
  fun f x ->
    let app, thread = ui.Force()
    let f _ =
      try
        let f_x = f x
        fun () -> f_x
      with e ->
        fun () -> raise e
    let t = app.Dispatcher.Invoke(DispatcherPriority.Send, System.Func<_, _>(f), null)
    (t :?> unit -> 'b)()


module Display2D =
  open System.Threading
  open System.Windows
  open System.Windows.Threading
  open System.Windows.Media

  type point = { mutable position: Vector}
  type edge  = { mutable end1: int; mutable end2: int; mutable length: float }

  type View(points:point array, directions) =
    inherit FrameworkElement()
    
    override this.OnRender dc =
      let s = min this.ActualWidth this.ActualHeight
//      dc.PushTransform(TranslateTransform((this.ActualWidth  - s) / 2.,
//                                          (this.ActualHeight - s) / 2.))
      let f op state =  points |> Seq.fold (fun (sx, sy) it -> op (it.position.X) sx, op (it.position.Y) sy) (state,state)
      let (xmax, ymax) = f max System.Double.MinValue
      let (xmin, ymin) = f min System.Double.MaxValue
      let xscale, yscale = let f x = if x = 0. then 1. else x
                           f(xmax - xmin), f(ymax - ymin)
      //printfn "%A %A %A %A " xmin xmax ymin ymax
      dc.PushTransform(ScaleTransform(s/(max xscale yscale)/1.1, s/(max xscale yscale)/1.1))
      dc.PushTransform(TranslateTransform(-xmin-0.0*xscale/2.,-(ymin-0.0*yscale/2.)))
      let pen = Pen(Brushes.Red, 100000000.)
      let point_of (r : Vector) = Point(r.X, 1. - r.Y)
      for s in directions do
        let e1 = points.[s.end1].position
        let e2 = points.[s.end2].position
        dc.DrawLine(pen, point_of e1, point_of e2)

      for p in points do
        let point_of (r : Vector) = Point(r.X, 1. - r.Y)
        dc.DrawEllipse(Brushes.Blue, null, point_of p.position, 0.003, 0.003)
      dc.Pop()
      dc.Pop()

  let make_window(points, directions) = 
    let view = View(points, directions)
    let window = Window(Content=view, Title="Display", Width=800., Height=800.)
    CompositionTarget.Rendering.Add(fun _ -> view.InvalidateVisual())
    window.Show()
    window