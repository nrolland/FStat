
#nowarn "40"
#load @"__solmerged.fsx"

open MSDN.FSharp.Charting
open System.Windows.Forms

#if INTERACTIVE
module InstallFsiAutoDisplay =
  fsi.AddPrinter(fun (ch:ChartTypes.GenericChart) -> 
    let frm = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    let ctl = new ChartControl(ch, Dock = DockStyle.Fill)
    frm.Text <- ChartFormUtilities.ProvideTitle ch
    frm.Controls.Add(ctl)
    frm.Show()
    ctl.Focus() |> ignore
    "(Chart)")
#endif